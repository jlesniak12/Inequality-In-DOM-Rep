#===============================================================================
# Script: 09A_estimation_helpers.R  [REVISED]
#
# Purpose: Reusable estimation, bootstrap, table, and figure helpers for the
#          Parente-style regressions on the region x quarter panel from 08.
#
# Sourced by 09B. Guarded so the driver can source once and reuse.
#
# CHANGES vs previous version:
#   1. Four near-identical run_*() functions collapsed into ONE run_did() that
#      takes the RHS as a string. Adding a design is a one-line spec entry in
#      09B rather than a new function here.
#   2. spec_path() builds the <income>/<baseline>/<design>/ tree.
#   3. bootstrap_pvals() gained a `terms` argument. Restricting to reported
#      terms cuts runtime 2-3x on the control-heavy specs.
#   4. save_table_boot() warns LOUDLY when bootstrap p-values come back NA.
#      Silent NAs previously produced starless tables that read as null results.
#   5. gof_map() built from GEO rather than hardcoded to Region10, so province
#      arms don't lose the FE row silently.
#   6. Sample helpers (restrict_sample, add_post, add_event_windows) for the
#      2021Q2 designs that need a locally-built Post.
#   7. manifest_rows() emits one row per (spec x arm x outcome x term) so the
#      driver can aggregate results across the full grid.
#
# BOOTSTRAP NOTE: pinned to fwildclusterboot 0.12.0. 0.13+ breaks with fixest
# factor levels containing spaces. If upgrading, retest.
#===============================================================================

library(fixest)
library(modelsummary)
library(glue)
library(fwildclusterboot)


#===============================================================================
# Time / sample helpers
#===============================================================================

# Fractional-year index from a "2017Q2" character column. Used by
# restrict_sample() and add_event_windows() for tidy inequality comparisons.
add_ti <- function(data) {
  data %>% mutate(
    .y  = as.integer(substr(time, 1, 4)),
    .q  = as.integer(substr(time, 6, 6)),
    ti  = .y + (.q - 1) / 4
  ) %>% select(-.y, -.q)
}

# Scalar version: "2017Q2" -> 2017.25. Used to convert config$events$event_qtrs
# for plot vlines etc.
qtr_to_ti <- function(qtrs) {
  yr <- as.integer(substr(qtrs, 1, 4))
  q  <- as.integer(substr(qtrs, 6, 6))
  yr + (q - 1) / 4
}

# Restrict to [from, to] in fractional-year units, inclusive.
restrict_sample <- function(data, from = -Inf, to = Inf) {
  d <- if ("ti" %in% names(data)) data else add_ti(data)
  d %>% filter(ti >= from, ti <= to)
}

# Single Post dummy at a given event. Built locally rather than reusing 08's
# post_<event> columns, which are event-WINDOW flags (flip back to 0 at the
# next event) and unsafe as full-panel Post dummies.
add_post <- function(data, event_ti, post_name = "post") {
  d <- if ("ti" %in% names(data)) data else add_ti(data)
  d[[post_name]] <- as.integer(d$ti >= event_ti)
  d
}


#===============================================================================
# Event windows (base2016 per-event design)
#
#   pre       : t < 2017Q2                            reference
#   post_2017 : 2017Q3 - 2019Q2  (post-2017 event, pre-2019 event)
#   post_2021 : 2021Q4 - 2023Q1  (post-2021 event, pre-2023 event)
#   post_2023 : 2023Q3 - 2025Q1  (post-2023 event, pre-2025 event)
#
# Excluded: 2019Q3-2021Q3 (COVID + ENCFT methodology change), 2025Q2+ (one
# quarter of post-data), event quarters themselves.
#
# CAUTION: "pre" is open-ended backwards. Restrict the sample to 2016Q1+
# upstream or the pre period straddles the 2015Q2 event.
#===============================================================================

add_event_windows <- function(data) {
  d <- if ("ti" %in% names(data)) data else add_ti(data)
  d %>%
    mutate(window = case_when(
      ti <  2017.25                 ~ "pre",
      ti >= 2017.50 & ti <= 2019.25 ~ "post_2017",
      ti >= 2021.75 & ti <= 2023.00 ~ "post_2021",
      ti >= 2023.50 & ti <= 2025.00 ~ "post_2023",
      TRUE                          ~ NA_character_
    )) %>%
    filter(!is.na(window)) %>%
    mutate(window = factor(
      window, levels = c("pre", "post_2017", "post_2021", "post_2023")))
}


#===============================================================================
# Output paths
#===============================================================================

# Tree:  <out_dir>/<income>/<baseline>/<design>/
spec_path <- function(out_dir, income, baseline, geo, design, create = TRUE) {
  p <- file.path(out_dir, income, baseline, geo, design)
  if (create) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

# Fits subfolder inside a design directory.
fits_path <- function(design_dir, create = TRUE) {
  p <- file.path(design_dir, "fits")
  if (create) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}


#===============================================================================
# Single generalized regression runner
#
# rhs : character, the treatment term(s). Examples:
#         "exposure_geo_val:post_any"
#         "i(exposure_group, post_any, ref = 'Low exposure')"
#         "i(window, exposure_geo_val, ref = 'pre')"
#         "i(window, i.exposure_group, ref = 'pre', ref2 = 'Low exposure')"
#         "i(year, exposure_geo_val, ref = 2016)"
# weights  : NULL (unweighted) or a column name in `data`.
# controls : character vector or NULL.
#===============================================================================

wts_arg <- function(weights, data) {
  if (is.null(weights)) return(NULL)
  if (!weights %in% names(data)) stop("weight column '", weights, "' missing")
  as.formula(paste0("~", weights))
}

run_did <- function(outcome, data, rhs, weights = NULL, controls = NULL,
                    fe = "Region10 + time", cluster_var = "region_int",
                    min_n = 20) {
  if (!outcome %in% names(data)) return(NULL)
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < min_n) return(NULL)
  ctrl <- if (is.null(controls)) "" else paste("+", paste(controls, collapse = " + "))
  fml  <- as.formula(glue("{outcome} ~ {rhs} {ctrl} | {fe}"))
  feols(fml, data = d, weights = wts_arg(weights, d),
        cluster = as.formula(paste0("~", cluster_var)))
}


#===============================================================================
# Coefficient-name builders
#
# fixest name conventions, so coef_maps can be generated per spec rather than
# hardcoded. Names must match fixest's internal naming character-for-character
# or modelsummary silently drops the row.
#===============================================================================

# "exposure_geo_val:post_any"
nm_cont_post <- function(treat, post) glue("{treat}:{post}")

# "exposure_group::High exposure:post_any"
nm_grp_post <- function(grpvar, level, post) glue("{grpvar}::{level}:{post}")

# "window::post_2017:exposure_geo_val"
nm_win_cont <- function(winvar, level, treat) glue("{winvar}::{level}:{treat}")

# "window::post_2017:exposure_group::High exposure"
nm_win_grp <- function(winvar, wlevel, grpvar, glevel)
  glue("{winvar}::{wlevel}:{grpvar}::{glevel}")

# Non-reference levels of a grouping column, in factor order.
other_levels <- function(data, var, ref) {
  lv <- levels(factor(data[[var]]))
  setdiff(lv, ref)
}


#===============================================================================
# Wild cluster bootstrap
#
# 10 clusters (Region10) is far below the ~40+ needed for reliable
# cluster-robust SEs. Webb-weighted wild bootstrap is the standard mitigation.
#===============================================================================

bootstrap_ci <- function(fit, B = 9999, seed = 42, conf_level = 0.95,
                         terms = NULL, clustid = "region_int") {
  cns <- names(coef(fit))
  if (!is.null(terms)) cns <- intersect(cns, terms)
  
  set.seed(seed)
  purrr::map_dfr(cns, function(cn) {
    bt <- tryCatch(
      boottest(object = fit, param = cn, B = B, clustid = clustid,
               type = "webb", sign_level = 1 - conf_level),
      error = function(e) { warning("Boot failed: ", cn, " - ", e$message); NULL }
    )
    if (is.null(bt)) return(tibble(term = cn, estimate = coef(fit)[cn],
                                   conf.low = NA_real_, conf.high = NA_real_,
                                   p.value = NA_real_))
    tb <- generics::tidy(bt)
    tibble(term = cn, estimate = coef(fit)[cn],
           conf.low = tb$conf.low, conf.high = tb$conf.high, p.value = tb$p.value)
  })
}

# `terms` restricts bootstrap to the coefficients actually reported. Pass
# names(coef_map). Bootstrapping controls you never print is pure waste.
bootstrap_pvals <- function(models, B = 9999, seed = 42, terms = NULL,
                            clustid = "region_int") {
  purrr::imap(models, function(fit, nm) {
    if (is.null(fit)) return(tibble(term = character(), p.boot = numeric()))
    cns <- names(coef(fit))
    if (!is.null(terms)) cns <- intersect(cns, terms)
    if (length(cns) == 0) return(tibble(term = character(), p.boot = numeric()))
    set.seed(seed)
    purrr::map_dfr(cns, function(cn) {
      bt <- tryCatch(
        boottest(object = fit, param = cn, B = B, clustid = clustid,
                 type = "webb", sign_level = 0.10),
        error = function(e) { warning(nm, "/", cn, ": ", e$message); NULL }
      )
      if (is.null(bt)) return(tibble(term = cn, p.boot = NA_real_))
      tibble(term = cn, p.boot = generics::tidy(bt)$p.value)
    })
  })
}


#===============================================================================
# Table helper
#
# modelsummary takes stars from vcov-implied SEs, so we back-solve SEs that
# reproduce the wild-bootstrap p-values. Parenthetical values are thus
# SYNTHETIC SEs, not cluster-robust SEs and not bootstrap SEs - they have no
# standalone interpretation. Table note calls this out.
#===============================================================================

inject_boot_pvals <- function(fit, boot_pvec) {
  cf <- coef(fit)
  shared <- intersect(names(cf), names(boot_pvec))
  if (length(shared) == 0) return(NULL)
  
  # Build a FULL-DIMENSIONAL vcov matching the fit's coefficient names.
  # Controls (and any other non-reported coefs) get a huge SE (1e12 variance)
  # so they never render stars; reported coefs get the back-solved fake SE.
  # This is critical: modelsummary needs vcov dim == fit dim; a partial vcov
  # (shared × shared) fails "cannot extract" for any spec with controls.
  V <- diag(1e12, nrow = length(cf))
  dimnames(V) <- list(names(cf), names(cf))
  
  fake_se <- abs(cf[shared]) /
    qnorm(1 - pmin(pmax(boot_pvec[shared], 1e-6), 1 - 1e-6) / 2)
  fake_se[!is.finite(fake_se) | fake_se == 0] <- 1e6
  diag(V)[shared] <- fake_se^2
  
  function(model) V
}

# GOF map built from GEO. Hardcoding "FE: Region10" (as the old 09A did) means
# the FE row silently vanishes from the table under any other geography.
gof_map <- function(geo) tibble::tribble(
  ~raw,                     ~clean,               ~fmt,
  paste0("FE: ", geo),      paste(geo, "FE"),      0,
  "FE: time",               "Quarter FE",          0,
  "nobs",                   "Observations",        0,
  "r.squared",              "R2",                  3
)

# save_table_boot() writes tbl_<file_base>.{html,tex} to `path`, plus optionally
# saves the fitted-model list under fits/<file_base>.rds for re-rendering
# without re-bootstrapping. Returns the bootstrap p-value tibbles.
#
# `outcome_labels` is a named character vector (fixest outcome column -> nice
# label for table column headers). Passed explicitly rather than global.
#
# `boot_terms` restricts bootstrap to reported coefficients (usually
# names(coef_map)).
save_table_boot <- function(models, coef_map, title, notes, file_base, path,
                            outcome_labels, geo, has_controls = NA,
                            B = 9999, seed = 42, boot_terms = names(coef_map),
                            save_fits = TRUE) {
  
  models <- purrr::compact(models)
  if (length(models) == 0) {
    warning("No fitted models for ", file_base, " - skipping table.")
    return(invisible(NULL))
  }
  
  pval_list <- bootstrap_pvals(models, B = B, seed = seed, terms = boot_terms)
  
  # Loud failure check. Silent NA p-values render starless tables that look
  # like null results rather than broken bootstraps. Most common cause:
  # fwildclusterboot refusing a weighted feols fit.
  all_p <- unlist(lapply(pval_list, function(x) x$p.boot))
  n_na  <- sum(is.na(all_p)); n_tot <- length(all_p)
  if (n_tot > 0 && n_na > 0) {
    warning(sprintf("[%s] %d/%d bootstrap p-values are NA. Stars are NOT trustworthy. Check whether boottest accepted the weighted fit.",
                    file_base, n_na, n_tot), call. = FALSE)
  }
  
  vcov_override <- purrr::imap(models, function(fit, nm) {
    pv <- pval_list[[nm]]
    if (is.null(pv) || nrow(pv) == 0) return(NULL)
    inject_boot_pvals(fit, setNames(pv$p.boot, pv$term))
  }) %>% purrr::compact()
  
  models_r <- setNames(models, unname(outcome_labels[names(models)]))
  vcov_r   <- setNames(vcov_override, names(models_r)[match(names(vcov_override),
                                                            names(models))])
  
  boot_note <- glue("Stars from wild cluster bootstrap (Webb, B={B}, ",
                    "cluster = {geo}). Parenthetical values are synthetic SEs ",
                    "back-solved from bootstrap p-values, not cluster-robust ",
                    "SEs. * p<0.10, ** p<0.05, *** p<0.01.")
  
  # Controls row: Yes/No indicator across all outcome columns. Position it
  # inside the GOF block (below coefficients, above the Observations row).
  add_rows_df <- NULL
  if (!is.na(has_controls)) {
    ctrl_val <- if (isTRUE(has_controls)) "Yes" else "No"
    add_rows_df <- data.frame(
      term = "Controls",
      matrix(ctrl_val, nrow = 1, ncol = length(models_r),
             dimnames = list(NULL, names(models_r))),
      check.names = FALSE
    )
    attr(add_rows_df, "position") <- length(coef_map) * 2 + 1  # after coef rows
  }
  
  for (ext in c("html", "tex")) {
    modelsummary(models_r, coef_map = coef_map, gof_map = gof_map(geo),
                 fmt = "%.3f",                                # force decimal
                 stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
                 vcov = vcov_r, title = title,
                 notes = c(list(boot_note), notes),
                 add_rows = add_rows_df,
                 output = file.path(path, paste0("tbl_", file_base, ".", ext)))
  }
  
  if (save_fits) {
    fp <- fits_path(path)
    saveRDS(models, file.path(fp, paste0(file_base, ".rds")))
  }
  
  cat("    saved:", file.path(path, paste0("tbl_", file_base)), "\n")
  invisible(pval_list)
}


#===============================================================================
# Manifest row builder
#
# One row per (outcome x reported coefficient) for a single fitted spec-arm.
# Driver concatenates and writes manifest.csv.
#===============================================================================

manifest_rows <- function(fits, pvals, spec_meta, coef_map, out_dir) {
  purrr::imap_dfr(fits, function(fit, nm) {
    base <- tibble::as_tibble(spec_meta)
    if (is.null(fit)) {
      return(bind_cols(base, tibble(
        outcome = nm, term = NA_character_,
        estimate = NA_real_, p_boot = NA_real_,
        nobs = NA_integer_, r2 = NA_real_,
        formula = NA_character_, out_dir = out_dir,
        error = "fit is NULL"
      )))
    }
    keep <- intersect(names(coef(fit)), names(coef_map))
    if (length(keep) == 0) {
      return(bind_cols(base, tibble(
        outcome = nm, term = NA_character_,
        estimate = NA_real_, p_boot = NA_real_,
        nobs = nobs(fit), r2 = unname(fixest::r2(fit)["r2"]),
        formula = paste(deparse(formula(fit)), collapse = " "),
        out_dir = out_dir, error = "coef_map matched no fit coefficients"
      )))
    }
    pv <- pvals[[nm]]
    bind_cols(base, tibble(
      outcome  = nm,
      term     = keep,
      estimate = unname(coef(fit)[keep]),
      p_boot   = if (is.null(pv)) NA_real_ else
        pv$p.boot[match(keep, pv$term)],
      nobs     = nobs(fit),
      r2       = unname(fixest::r2(fit)["r2"]),
      formula  = paste(deparse(formula(fit)), collapse = " "),
      out_dir  = out_dir,
      error    = NA_character_
    ))
  })
}


#===============================================================================
# Event-study plot (year-by-year continuous)
#
# CI ribbon deliberately omitted (matches 05a style). Significance conveyed by
# point colour/shape.
#===============================================================================

parse_year <- function(term) as.integer(regmatches(term, regexpr("\\d{4}", term)))

# `event_years` are the MW event dates drawn as vertical dotted lines.
# Passed by 09B (base-specific: 2017.25, 2019.5, 2021.5, 2023.25 for base2016;
# 2021.5 only for base2021q2).
plot_event_study <- function(boot_tbl, title = NULL, subtitle = NULL,
                             ref_year = 2016, y_label = NULL,
                             event_years = NULL) {
  if (is.null(boot_tbl) || nrow(boot_tbl) == 0) return(NULL)
  pd <- boot_tbl %>%
    mutate(year = parse_year(term)) %>%
    bind_rows(tibble(year = ref_year, estimate = 0,
                     conf.low = 0, conf.high = 0, p.value = NA_real_)) %>%
    arrange(year) %>%
    mutate(sig = case_when(
      p.value < 0.01 ~ "p < 0.01",
      p.value < 0.05 ~ "p < 0.05",
      p.value < 0.10 ~ "p < 0.10",
      TRUE           ~ "n.s."),
      sig = factor(sig, levels = c("p < 0.01", "p < 0.05", "p < 0.10", "n.s.")))
  ggplot(pd, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50",
               linewidth = 0.4) +
    annotate("rect", xmin = ref_year - 0.5, xmax = ref_year + 0.5,
             ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey50") +
    geom_line(colour = "#2166ac", linewidth = 0.7) +
    geom_point(aes(colour = sig, shape = sig), size = 2.5) +
    { if (!is.null(event_years) && length(event_years) > 0)
      geom_vline(xintercept = event_years, linetype = "dotted",
                 colour = "red", alpha = 0.6, linewidth = 0.5)
      else NULL } +
    scale_colour_manual(values = c("p < 0.01" = "#d73027", "p < 0.05" = "#fc8d59",
                                   "p < 0.10" = "#fee090", "n.s." = "grey60"),
                        drop = FALSE) +
    scale_shape_manual(values = c("p < 0.01" = 16, "p < 0.05" = 16,
                                  "p < 0.10" = 17, "n.s." = 1), drop = FALSE) +
    scale_x_continuous(breaks = seq(2014, 2025, 1)) +
    labs(title = title, subtitle = subtitle, x = NULL,
         y = y_label %||% "Coefficient x Year",
         colour = "Significance", shape = "Significance") +
    theme_surveytools() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

save_plot <- function(p, name, path,
                      w = config$fig_defaults$width,
                      h = config$fig_defaults$height) {
  if (is.null(p)) return(invisible(NULL))
  fp <- file.path(path, paste0(name, ".", config$fig_defaults$format))
  ggsave(fp, p, width = w, height = h, dpi = config$fig_defaults$dpi)
  cat("    saved:", fp, "\n")
}

cat("=== 09A helpers loaded ===\n")