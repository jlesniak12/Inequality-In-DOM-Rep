#===============================================================================
# Script: 09a_estimation helpers.R
#
# Purpose: Reusable estimation, bootstrap, table, and figure helpers for the
#          Parente-style regressions on the region x quarter panel from 08.
#          Adapted from 05a (sector version) - region substitutions throughout.
#
# Sourced by 09b (main estimation).
#
# BOOTSTRAP NOTE: pinned to fwildclusterboot 0.12.0. 0.13+ breaks with fixest
# factor levels containing spaces. If upgrading, retest.
#===============================================================================

library(fixest)
library(modelsummary)
library(glue)
library(fwildclusterboot)


#===============================================================================
# Constants
#===============================================================================

# Outcomes for the target table, in column order.
OUTCOMES <- c(
  log_var_hwage_total    = "log_var_hwage_total",
  log_var_hwage_formal   = "log_var_hwage_formal",
  log_var_hwage_informal = "log_var_hwage_informal",
  log_informal_share     = "log_informal_share",
  log_selfemp_share      = "log_selfemp_share"
)

OUTCOME_LABELS <- c(
  log_var_hwage_total    = "log(VAll)",
  log_var_hwage_formal   = "log(VF)",
  log_var_hwage_informal = "log(VI)",
  log_informal_share     = "log(Inf. Share)",
  log_selfemp_share      = "log(Self-emp.)"
)

# Real MW event quarters as fractional years for the event-study plot vlines.
MW_EVENT_YEARS <- c(2017.25, 2019.50, 2021.50, 2023.25)


#===============================================================================
# Window assignment
#
# Windows for the per-event ("collapsed") specification.
#
#   pre       : t < 2017Q2                            reference
#   post_2017 : 2017Q3 - 2019Q2  (post-2017 event, pre-2019 event)
#   post_2021 : 2021Q4 - 2023Q1  (post-2021 event, pre-2023 event)
#   post_2023 : 2023Q3 - 2025Q1  (post-2023 event, pre-2025 event)
#
# Deliberately EXCLUDED:
#   2019Q3-2021Q3  the 2019 event's post-period is contaminated by COVID and
#                  the ENCFT methodology change; no clean window available.
#   2025Q2+        the 2025 event has one quarter of post-data.
#   Event quarters themselves.
#
# Change here (not 09b) if the event calendar or window design changes.
#===============================================================================

add_window <- function(data) {
  data %>%
    mutate(
      y = as.integer(substr(time, 1, 4)),
      q = as.integer(substr(time, 6, 6)),
      ti = y + (q - 1) / 4,
      window = case_when(
        ti <  2017.25                     ~ "pre",
        ti >= 2017.50 & ti <= 2019.25     ~ "post_2017",
        ti >= 2021.75 & ti <= 2023.00     ~ "post_2021",
        ti >= 2023.50 & ti <= 2025.00     ~ "post_2023",
        TRUE                              ~ NA_character_
      )
    ) %>%
    filter(!is.na(window)) %>%
    mutate(window = factor(window,
                           levels = c("pre", "post_2017", "post_2021", "post_2023"))) %>%
    select(-y, -q, -ti)
}


#===============================================================================
# Regression runners
#
# Four specifications, one function each so a reader can see the formulas.
# All use Region10 + time FE and cluster on region_int (numeric cluster id
# for fwildclusterboot compatibility).
#
# weights: NULL = unweighted; otherwise a variable name (character) present
# in `data`. Pass "baseline_emp" for the Parente-style employment-weighted spec.
#===============================================================================

wts_arg <- function(weights, data) {
  if (is.null(weights)) return(NULL)
  if (!weights %in% names(data)) stop("weight column '", weights, "' missing")
  as.formula(paste0("~", weights))
}
run_pooled_cont <- function(outcome, data, weights = NULL, controls = NULL) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  ctrl <- if (is.null(controls)) "" else paste("+", paste(controls, collapse = " + "))
  feols(as.formula(glue(
    "{outcome} ~ exposure_geo_val:post_any {ctrl} | Region10 + time"
  )), data = d, weights = wts_arg(weights, d), cluster = ~region_int)
}

run_pooled_tier <- function(outcome, data, weights = NULL, controls = NULL) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  ctrl <- if (is.null(controls)) "" else paste("+", paste(controls, collapse = " + "))
  feols(as.formula(glue(
    "{outcome} ~ i(exposure_group, post_any, ref = 'Low exposure') {ctrl} | Region10 + time"
  )), data = d, weights = wts_arg(weights, d), cluster = ~region_int)
}

run_window_cont <- function(outcome, data, weights = NULL, controls = NULL) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  ctrl <- if (is.null(controls)) "" else paste("+", paste(controls, collapse = " + "))
  feols(as.formula(glue(
    "{outcome} ~ i(window, exposure_geo_val, ref = 'pre') {ctrl} | Region10 + time"
  )), data = d, weights = wts_arg(weights, d), cluster = ~region_int)
}

run_window_tier <- function(outcome, data, weights = NULL, controls = NULL) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  ctrl <- if (is.null(controls)) "" else paste("+", paste(controls, collapse = " + "))
  feols(as.formula(glue(
    "{outcome} ~ i(window, i.exposure_group, ref = 'pre', ref2 = 'Low exposure') {ctrl} |
                 Region10 + time"
  )), data = d, weights = wts_arg(weights, d), cluster = ~region_int)
}

run_es_cont <- function(outcome, data, weights = NULL, controls = NULL,
                        ref_year = 2016) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  ctrl <- if (is.null(controls)) "" else paste("+", paste(controls, collapse = " + "))
  feols(as.formula(glue(
    "{outcome} ~ i(year, exposure_geo_val, ref = {ref_year}) {ctrl} | Region10 + time"
  )), data = d, weights = wts_arg(weights, d), cluster = ~region_int)
}

#===============================================================================
# Wild cluster bootstrap
#
# 10 clusters (Region10) is far below the ~40+ needed for reliable
# cluster-robust SEs. Webb-weighted wild bootstrap is standard mitigation.
# Returns tidy tibbles of (term, estimate, conf.low, conf.high, p.value).
#===============================================================================

bootstrap_ci <- function(fit, B = 9999, seed = 42, conf_level = 0.95,
                          terms = NULL) {
  cns <- names(coef(fit))
  if (!is.null(terms)) cns <- intersect(cns, terms)
  
  set.seed(seed)
  purrr::map_dfr(cns, function(cn) {
    bt <- tryCatch(
      boottest(object = fit, param = cn, B = B, clustid = "region_int",
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

bootstrap_pvals <- function(models, B = 9999, seed = 42) {
  purrr::imap(models, function(fit, nm) {
    cns <- names(coef(fit))
    if (length(cns) == 0) return(tibble(term = character(), p.boot = numeric()))
    set.seed(seed)
    purrr::map_dfr(cns, function(cn) {
      bt <- tryCatch(
        boottest(object = fit, param = cn, B = B, clustid = "region_int",
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
# Uses the "fake vcov from bootstrap p-values" trick from 05a: modelsummary
# takes stars from vcov-implied SEs, so we back-calculate SEs that reproduce
# the wild-bootstrap p-values. HTML + .tex output.
#===============================================================================

inject_boot_pvals <- function(fit, boot_pvec) {
  cf <- coef(fit)
  shared <- intersect(names(cf), names(boot_pvec))
  if (length(shared) == 0) return(NULL)
  fake_se <- abs(cf[shared]) /
    qnorm(1 - pmin(pmax(boot_pvec[shared], 1e-6), 1 - 1e-6) / 2)
  fake_se[!is.finite(fake_se) | fake_se == 0] <- 1e6
  V <- diag(fake_se^2, nrow = length(shared))
  dimnames(V) <- list(shared, shared)
  function(model) V
}

GOF_MAP <- tibble::tribble(
  ~raw,              ~clean,           ~fmt,
  "FE: Region10",    "Region10 FE",     0,
  "FE: time",        "Quarter FE",      0,
  "nobs",            "Observations",    0,
  "r.squared",       "R²",              3
)

save_table_boot <- function(models, coef_map, title, notes, file_base, path,
                            B = 9999, seed = 42) {
  pval_list <- bootstrap_pvals(models, B = B, seed = seed)
  
  vcov_override <- purrr::imap(models, function(fit, nm) {
    pv <- pval_list[[nm]]
    if (is.null(pv) || nrow(pv) == 0) return(NULL)
    inject_boot_pvals(fit, setNames(pv$p.boot, pv$term))
  }) %>% purrr::compact()
  
  # Rename columns to the outcome labels (nicer LaTeX headers)
  models_r <- setNames(models, unname(OUTCOME_LABELS[names(models)]))
  vcov_r   <- setNames(vcov_override, names(models_r))
  
  boot_note <- glue("Stars from wild cluster bootstrap (Webb, B={B}, ",
                    "cluster = Region10). * p<0.10, ** p<0.05, *** p<0.01.")
  
  modelsummary(models_r, coef_map = coef_map, gof_map = GOF_MAP,
               stars = c("*"=0.10, "**"=0.05, "***"=0.01),
               vcov = vcov_r, title = title, notes = c(list(boot_note), notes),
               output = file.path(path, paste0(file_base, ".html")))
  
  modelsummary(models_r, coef_map = coef_map, gof_map = GOF_MAP,
               stars = c("*"=0.10, "**"=0.05, "***"=0.01),
               vcov = vcov_r, title = title, notes = c(list(boot_note), notes),
               output = file.path(path, paste0(file_base, ".tex")))
  
  cat("Saved table:", file.path(path, file_base), "\n")
  invisible(pval_list)
}


#===============================================================================
# Event-study plot (year-by-year continuous)
#
# CI ribbon deliberately omitted (see 05a note). Significance conveyed by
# point colour/shape.
#===============================================================================

parse_year <- function(term) as.integer(regmatches(term, regexpr("\\d{4}", term)))

plot_event_study <- function(boot_tbl, title = NULL, subtitle = NULL,
                             ref_year = 2016, y_label = NULL) {
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
      sig = factor(sig, levels = c("p < 0.01","p < 0.05","p < 0.10","n.s.")))
  ggplot(pd, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    annotate("rect", xmin = ref_year - 0.5, xmax = ref_year + 0.5,
             ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey50") +
    geom_line(colour = "#2166ac", linewidth = 0.7) +
    geom_point(aes(colour = sig, shape = sig), size = 2.5) +
    geom_vline(xintercept = MW_EVENT_YEARS, linetype = "dotted",
               colour = "red", alpha = 0.6, linewidth = 0.5) +
    scale_colour_manual(values = c("p < 0.01"="#d73027","p < 0.05"="#fc8d59",
                                   "p < 0.10"="#fee090","n.s."="grey60"), drop = FALSE) +
    scale_shape_manual(values = c("p < 0.01"=16,"p < 0.05"=16,
                                  "p < 0.10"=17,"n.s."=1), drop = FALSE) +
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
  cat("Saved plot:", fp, "\n")
}

cat("=== 09a helpers loaded ===\n")
