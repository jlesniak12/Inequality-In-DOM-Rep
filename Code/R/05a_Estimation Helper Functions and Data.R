#===============================================================================
# Script: 05a_Estimation Helper Functions and Data.R
#
# PURPOSE:
#   Shared setup sourced by 05b and 05c. Loads data, scales units, builds
#   six regression datasets, defines all helper functions, sets output paths.
#
# BOOTSTRAP NOTE:
#   fwildclusterboot >= 0.13 extracts data internally from the fixest object.
#   The `data` argument to boottest() was removed in that version.
#   The `fit_data` attribute pattern is therefore REMOVED from this script.
#   Instead, refit functions use `cluster = ~sector` so feols stores the
#   cluster variable internally where boottest can find it.
#
# KEY DESIGN DECISIONS:
#   - Exposure ×100, below_min ×100, informal ×100 (p.p. scale)
#   - log_var_wage NOT rescaled (already unit-free)
#   - Year×quarter FEs (time_fe = factor(time)) absorb 44 quarter shocks
#   - Interaction uses year_num (annual) → one β per year, no collinearity
#   - Wild bootstrap Webb weights, B=9999, clustered at sector (10 clusters)
#   - CI ribbon REMOVED from event study plots — significance shown via
#     point colour/shape (less visual clutter, avoids NA band gaps)
#   - save_table outputs HTML + .tex only (no PDF — use \input{} in paper)
#===============================================================================


#remotes::install_version("fwildclusterboot", version = "0.12.0")

source("Code/R/00_setup.R")
library(fixest)
library(modelsummary)
library(glue)
library(fwildclusterboot)



#===============================================================================
# STEP 0. Output Paths
#===============================================================================

pd       <- config$paths$processed_data
base_sf  <- file.path(config$paths$outputs, config$output_stage,
                      config$out_subdirs$reg_sector_firmsize)
base_s   <- file.path(config$paths$outputs, config$output_stage,
                      config$out_subdirs$reg_sector)

out_sf_main <- file.path(base_sf, "Main")
out_sf_rob  <- file.path(base_sf, "Robustness")
out_sf_full <- file.path(base_sf, "Full Data")
out_s_main  <- file.path(base_s,  "Main")
out_s_rob   <- file.path(base_s,  "Robustness")
out_s_full  <- file.path(base_s,  "Full Data")

for (p in c(out_sf_main, out_sf_rob, out_sf_full,
            out_s_main,  out_s_rob,  out_s_full)) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}
cat("Output folders ready.\n")

#===============================================================================
# STEP 1. Constants
#===============================================================================

COVID_QTRS <- c("2020Q1","2020Q2","2020Q3","2020Q4","2021Q1","2021Q2")

OUTCOMES <- c(
  log_var_wage = "log_var_wage",
  below_min    = "below_min",
  informal     = "informal"
)

OUTCOME_LABELS <- c(
  log_var_wage = "Log wage variance",
  below_min    = "Non-compliance rate (p.p.)",
  informal     = "Informality share (p.p.)"
)

MW_EVENT_YEARS <- c(2017.5, 2019.75, 2021.75, 2023.5)

#===============================================================================
# STEP 2. Load Data
#===============================================================================

panel_sf <- readRDS(file.path(pd, "panel_sf_clean.rds")) %>%
  mutate(time = as.character(time))

panel_sf_full_path <- file.path(pd, "panel_sf_full.rds")
if (file.exists(panel_sf_full_path)) {
  panel_sf_full <- readRDS(panel_sf_full_path) %>%
    mutate(time = as.character(time))
  cat("Full unvalidated panel loaded (panel_sf_full.rds).\n")
} else {
  panel_sf_full <- panel_sf
  warning("panel_sf_full.rds not found — full-data specs use validated panel as fallback.")
}

exposure_s <- readRDS(file.path(pd, "sector_mw_exposure_baseline.rds"))

#===============================================================================
# STEP 3. Build Sector × Quarter Panels
#===============================================================================

build_panel_s <- function(panel) {
  panel %>%
    filter(!is.na(exposure_sf_val)) %>%
    group_by(Employment_Sector, time, time_index) %>%
    summarise(
      log_var_wage = weighted.mean(log_var_wage, pi, na.rm = TRUE),
      below_min    = weighted.mean(below_min,    pi, na.rm = TRUE),
      informal     = weighted.mean(informal,     pi, na.rm = TRUE),
      pi_sector    = sum(pi, na.rm = TRUE),
      .groups      = "drop"
    ) %>%
    left_join(
      exposure_s %>% select(Employment_Sector, exposure_baseline_val),
      by = "Employment_Sector"
    ) %>%
    mutate(group = Employment_Sector, sector_fe = factor(Employment_Sector))
}

panel_s      <- build_panel_s(panel_sf)
panel_s_full <- build_panel_s(panel_sf_full)

#===============================================================================
# STEP 4. Scale to Percentage Points
#===============================================================================

scale_pp <- function(df, is_sector = FALSE) {
  exp_col <- if (is_sector) "exposure_baseline_val" else "exposure_sf_val"
  df %>% mutate(
    !!exp_col := .data[[exp_col]] * 100,
    below_min  = below_min  * 100,
    informal   = informal   * 100
  )
}

panel_sf      <- scale_pp(panel_sf)
panel_sf_full <- scale_pp(panel_sf_full)
panel_s       <- scale_pp(panel_s,      is_sector = TRUE)
panel_s_full  <- scale_pp(panel_s_full, is_sector = TRUE)

cat("Unit scaling applied (×100).\n")
cat("exposure_sf_val range:     ",
    round(range(panel_sf$exposure_sf_val, na.rm = TRUE), 1), "\n")
cat("exposure_baseline_val range:",
    round(range(panel_s$exposure_baseline_val, na.rm = TRUE), 1), "\n\n")

#===============================================================================
# STEP 5. Build Regression Datasets
#===============================================================================

prep_reg_sf <- function(panel, exclude_covid = TRUE) {
  d <- if (exclude_covid) filter(panel, !time %in% COVID_QTRS) else panel
  d %>%
    filter(!is.na(exposure_sf_val)) %>%
    mutate(
      cell_fe    = factor(cell_id),
      year_num   = as.integer(substr(time, 1, 4)),
      time_fe    = factor(time),
      year_fe    = relevel(factor(year_num), ref = "2016"),
      sector     = factor(Employment_Sector),
      sector_int = as.integer(factor(Employment_Sector))  # add this
    )
}

prep_reg_s <- function(panel, exclude_covid = TRUE) {
  d <- if (exclude_covid) filter(panel, !time %in% COVID_QTRS) else panel
  d %>%
    filter(!is.na(exposure_baseline_val)) %>%
    mutate(
      year_num   = as.integer(substr(time, 1, 4)),
      time_fe    = factor(time),
      year_fe    = relevel(factor(year_num), ref = "2016"),
      sector     = factor(Employment_Sector),
      sector_fe  = factor(Employment_Sector),
      sector_int = as.integer(factor(Employment_Sector))  # add this
    )
}

reg_sf       <- prep_reg_sf(panel_sf,      exclude_covid = TRUE)
reg_s        <- prep_reg_s(panel_s,        exclude_covid = TRUE)
reg_sf_covid <- prep_reg_sf(panel_sf,      exclude_covid = FALSE)
reg_s_covid  <- prep_reg_s(panel_s,        exclude_covid = FALSE)
reg_sf_full  <- prep_reg_sf(panel_sf_full, exclude_covid = TRUE)
reg_s_full   <- prep_reg_s(panel_s_full,   exclude_covid = TRUE)

cat("=== DATASET DIMENSIONS ===\n")
cat("reg_sf (validated, main):      ", nrow(reg_sf),
    "obs,", n_distinct(reg_sf$cell_id), "cells\n")
cat("reg_s (validated, sector):     ", nrow(reg_s),
    "obs,", n_distinct(reg_s$Employment_Sector), "sectors\n")
cat("reg_sf_covid (COVID included): ", nrow(reg_sf_covid), "obs\n")
cat("reg_s_covid  (COVID included): ", nrow(reg_s_covid),  "obs\n")
cat("reg_sf_full (unvalidated):     ", nrow(reg_sf_full),
    "obs,", n_distinct(reg_sf_full$cell_id), "cells\n")
cat("reg_s_full  (unvalidated):     ", nrow(reg_s_full),   "obs\n\n")

#===============================================================================
# STEP 6. Window Assignment Helpers
#===============================================================================

add_window_4 <- function(data) {
  data %>%
    mutate(
      y  = as.integer(substr(time, 1, 4)),
      q  = as.integer(substr(time, 6, 6)),
      ti = y + (q - 1) / 4,
      window = case_when(
        ti <  2017.00                         ~ "pre",
        ti >= 2017.00 & ti <= 2019.25         ~ "mid_2017_2019",
        ti >= 2021.75 & ti <= 2023.00         ~ "post_2021_2022",
        ti >= 2023.50                         ~ "post_2023",
        TRUE                                  ~ NA_character_
      )
    ) %>%
    filter(!is.na(window)) %>%
    mutate(window = factor(window, levels = c("pre","mid_2017_2019",
                                              "post_2021_2022","post_2023"))) %>%
    select(-y, -q, -ti)
}

add_window_covid <- function(data) {
  data %>%
    mutate(
      y            = as.integer(substr(time, 1, 4)),
      q            = as.integer(substr(time, 6, 6)),
      ti           = y + (q - 1) / 4,
      covid_period = as.integer(time %in% COVID_QTRS),
      window = case_when(
        ti <  2017.00                         ~ "pre",
        ti >= 2017.00 & ti <= 2019.25         ~ "mid_2017_2019",
        ti >  2019.25 & ti <= 2021.50         ~ "covid_gap",
        ti >= 2021.75 & ti <= 2023.00         ~ "post_2021_2022",
        ti >= 2023.50                         ~ "post_2023",
        TRUE                                  ~ NA_character_
      )
    ) %>%
    filter(!is.na(window)) %>%
    mutate(window = factor(window, levels = c("pre","mid_2017_2019","covid_gap",
                                              "post_2021_2022","post_2023"))) %>%
    select(-y, -q, -ti)
}

#===============================================================================
# STEP 7. Regression Helpers
#===============================================================================

run_es_sf <- function(outcome, data, ref = 2016) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) { message("Skipping '", outcome, "'"); return(NULL) }
  feols(as.formula(glue(
    "{outcome} ~ i(year_num, exposure_sf_val, ref={ref}) | cell_fe + time_fe"
  )), data=d, weights=~pi, vcov=~sector)
}

run_es_s <- function(outcome, data, ref = 2016) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) { message("Skipping '", outcome, "'"); return(NULL) }
  feols(as.formula(glue(
    "{outcome} ~ i(year_num, exposure_baseline_val, ref={ref}) | sector_fe + time_fe"
  )), data=d, weights=~pi_sector, vcov=~sector)
}

run_collapsed_sf <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(as.formula(glue(
    "{outcome} ~ i(window, exposure_sf_val, ref='pre') | cell_fe + time_fe"
  )), data=d, weights=~pi, vcov=~sector)
}

run_collapsed_s <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(as.formula(glue(
    "{outcome} ~ i(window, exposure_baseline_val, ref='pre') | sector_fe + time_fe"
  )), data=d, weights=~pi_sector, vcov=~sector)
}

run_covid_sf <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(as.formula(glue(
    "{outcome} ~ i(window, exposure_sf_val, ref='pre') +
                 covid_period + covid_period:exposure_sf_val |
                 cell_fe + time_fe"
  )), data=d, weights=~pi, vcov=~sector)
}

run_covid_s <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(as.formula(glue(
    "{outcome} ~ i(window, exposure_baseline_val, ref='pre') +
                 covid_period + covid_period:exposure_baseline_val |
                 sector_fe + time_fe"
  )), data=d, weights=~pi_sector, vcov=~sector)
}

#===============================================================================
# STEP 8. Bootstrap Helpers
#
# REQUIRES: fwildclusterboot 0.12.0 — pinned due to breaking API changes in
# 0.13+ that are incompatible with fixest factor FE models with multi-word
# level names (e.g. "Rest of Service Sector"). Do not upgrade without testing.
# To install: remotes::install_version("fwildclusterboot", version = "0.12.0")
#
# Pattern: unweighted refit stores filtered data via attr(fit, "fit_data"),
# passed explicitly to boottest() via data = fd.
#===============================================================================

refit_unweighted_sf <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  fit <- feols(
    as.formula(glue(
      "{outcome} ~ i(year_num, exposure_sf_val, ref=2016) | cell_fe + time_fe"
    )),
    data = d,
    cluster = ~sector_int
  )
  attr(fit, "fit_data") <- d
  fit
}

refit_unweighted_s <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  fit <- feols(
    as.formula(glue(
      "{outcome} ~ i(year_num, exposure_baseline_val, ref=2016) | sector_fe + time_fe"
    )),
    data = d,
    cluster = ~sector_int
  )
  attr(fit, "fit_data") <- d
  fit
}

bootstrap_ci <- function(fit, B=9999, seed=42, conf_level=0.95) {
  cns <- names(coef(fit))
  es  <- cns[grepl("year_num::", cns, fixed=TRUE)]
  if (length(es) == 0) return(NULL)
  fd  <- attr(fit, "fit_data")
  if (is.null(fd)) stop("fit_data attribute missing.")
  set.seed(seed)
  map_dfr(es, function(cn) {
    bt <- tryCatch(
      boottest(
        object     = fit,
        param      = cn,
        B          = B,
        clustid    = "sector_int",
        type       = "webb",
        sign_level = 1 - conf_level
      ),
      error = function(e) {
        warning("Bootstrap failed: ", cn, " — ", e$message)
        NULL
      }
    )
    if (is.null(bt)) {
      return(tibble(term=cn, estimate=coef(fit)[cn],
                    conf.low=NA_real_, conf.high=NA_real_, p.value=NA_real_))
    }
    tb <- generics::tidy(bt)
    tibble(term=cn, estimate=coef(fit)[cn],
           conf.low=tb$conf.low, conf.high=tb$conf.high, p.value=tb$p.value)
  })
}




# Bootstrap p-values for a list of collapsed/robustness models
bootstrap_pvals <- function(models, B = 999, seed = 42) {
  # Lower B for collapsed models — fewer coefficients, faster
  imap(models, function(fit, nm) {
    cns   <- names(coef(fit))
    if (length(cns) == 0) return(tibble(term = character(), p.boot = numeric()))
    set.seed(seed)
    map_dfr(cns, function(cn) {
      bt <- tryCatch(
        boottest(
          object     = fit,
          param      = cn,
          B          = B,
          clustid    = "sector_int",
          type       = "webb",
          sign_level = 0.10        # sign_level here doesn't affect p.value output
        ),
        error = function(e) { warning(nm, "/", cn, ": ", e$message); NULL }
      )
      if (is.null(bt)) return(tibble(term = cn, p.boot = NA_real_))
      tibble(term = cn, p.boot = generics::tidy(bt)$p.value)
    })
  })
}

save_table_boot <- function(models, coef_map, title, notes, file_base, path,
                            B = 999, seed = 42) {
  
  # Run bootstrap once
  pval_list <- bootstrap_pvals(models, B = B, seed = seed)
  print(pval_list)  # keep until confirmed working, then remove
  
  # Build named numeric vectors — statistic_override is reliable across all
  # modelsummary versions and unambiguous about what it does
  pval_override <- imap(models, function(fit, nm) {
    pv <- pval_list[[nm]]
    v  <- setNames(pv$p.boot, pv$term)
    v[names(coef(fit))]  # align to coef order
  })
  
  models_renamed <- setNames(models,
                             vapply(names(models), function(nm) switch(nm,
                                                                       log_var_wage = "Log Wage Var.",
                                                                       below_min    = "Non-compliance",
                                                                       informal     = "Informality", nm), character(1)))
  
  pval_renamed <- setNames(pval_override, names(models_renamed))
  
  boot_note <- paste0("Stars from wild cluster bootstrap (Webb weights, B=", B,
                      ", clustered at sector). * p<0.10, ** p<0.05, *** p<0.01.")
  
  tex_notes <- list(
    "Exposure and proportion outcomes scaled x100 (p.p.). log\\_var\\_wage in log units.",
    "Cell/sector and year x quarter FE. Weighted by baseline employment share.",
    paste0("Stars from wild cluster bootstrap (Webb weights, B=", B,
           ", clustered at sector). * p$<$0.10, ** p$<$0.05, *** p$<$0.01.")
  )
  
  # HTML
  modelsummary(
    models,
    coef_map           = coef_map,
    gof_map            = gof_map,
    stars              = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    statistic_override = pval_override,
    title              = title,
    notes              = c(list(boot_note), notes),
    output             = file.path(path, paste0(file_base, ".html"))
  )
  
  # tex
  modelsummary(
    models_renamed,
    coef_map           = coef_map,
    gof_map            = gof_map,
    stars              = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    statistic_override = pval_renamed,
    title              = title,
    notes              = tex_notes,
    output             = file.path(path, paste0(file_base, ".tex"))
  )
  
  cat("Saved table (bootstrap stars):", file.path(path, file_base), "\n")
}

#===============================================================================
# STEP 9. Plot Helper
#
# CI ribbon REMOVED — significance shown via point colour/shape only.
# This avoids: (a) visual clutter, (b) NA band gaps for thin outcomes,
# (c) geom_ribbon warnings when bootstrap fails for some coefficients.
# Bootstrap p-values are still computed and drive the significance colours.
#===============================================================================

parse_year <- function(term) as.integer(regmatches(term, regexpr("\\d{4}", term)))

plot_event_study <- function(boot_tbl, title=NULL, subtitle=NULL,
                             ref_year=2016, y_label=NULL) {
  if (is.null(boot_tbl) || nrow(boot_tbl)==0) return(NULL)
  pd2 <- boot_tbl %>%
    mutate(year = parse_year(term)) %>%
    bind_rows(tibble(year=ref_year, estimate=0,
                     conf.low=0, conf.high=0, p.value=NA_real_)) %>%
    arrange(year) %>%
    mutate(
      sig = case_when(
        p.value < 0.01 ~ "p < 0.01",
        p.value < 0.05 ~ "p < 0.05",
        p.value < 0.10 ~ "p < 0.10",
        TRUE           ~ "n.s."
      ),
      sig = factor(sig, levels=c("p < 0.01","p < 0.05","p < 0.10","n.s."))
    )
  ggplot(pd2, aes(x=year, y=estimate)) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey50", linewidth=0.4) +
    annotate("rect", xmin=ref_year-0.5, xmax=ref_year+0.5,
             ymin=-Inf, ymax=Inf, alpha=0.08, fill="grey50") +
    geom_line(colour="#2166ac", linewidth=0.7) +
    geom_point(aes(colour=sig, shape=sig), size=2.5) +
    geom_vline(xintercept=MW_EVENT_YEARS, linetype="dotted",
               colour="red", alpha=0.6, linewidth=0.5) +
    scale_colour_manual(
      values=c("p < 0.01"="#d73027","p < 0.05"="#fc8d59",
               "p < 0.10"="#fee090","n.s."="grey60"), drop=FALSE) +
    scale_shape_manual(
      values=c("p < 0.01"=16,"p < 0.05"=16,"p < 0.10"=17,"n.s."=1),
      drop=FALSE) +
    scale_x_continuous(breaks=seq(2014,2025,1)) +
    labs(title=title, subtitle=subtitle, x=NULL,
         y=y_label %||% "Coefficient on Exposure (p.p.) × Year",
         colour="Significance", shape="Significance") +
    theme_surveytools() +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          axis.ticks.x=element_line())
}

save_plot <- function(p, name, path,
                      w=config$fig_defaults$width,
                      h=config$fig_defaults$height) {
  if (is.null(p)) return(invisible(NULL))
  fp <- file.path(path, paste0(name, ".", config$fig_defaults$format))
  ggsave(fp, p, width=w, height=h, dpi=config$fig_defaults$dpi)
  cat("Saved plot:", fp, "\n")
}

run_and_plot_es <- function(reg_data, refit_fn, es_fn, plot_path,
                            file_prefix, subtitle_extra, B=9999) {
  es_fits <- map(OUTCOMES, es_fn, data=reg_data) %>% compact()
  cat("Event study fits:", paste(names(es_fits), collapse=", "), "\n")
  
  es_unw <- map(names(OUTCOMES), refit_fn, data=reg_data) %>%
    setNames(names(OUTCOMES)) %>% compact()
  
  cat("Running bootstrap (B=", B, ")...\n", sep="")
  set.seed(42)
  boot_ci <- map(es_unw, bootstrap_ci, B=B, seed=42) %>% compact()
  
  walk(names(boot_ci), function(nm) {
    n_na <- sum(is.na(boot_ci[[nm]]$conf.low))
    if (n_na > 0) warning(nm, ": ", n_na, " bootstrap CIs are NA (shown as n.s.).")
  })
  
  iwalk(OUTCOME_LABELS, function(label, nm) {
    if (!nm %in% names(boot_ci)) return(NULL)
    y_label <- if (nm=="log_var_wage") "Coefficient × Year [log units]" else
      "Coefficient × Year [p.p.]"
    subtitle <- paste(
      subtitle_extra,
      "Exposure and proportions in p.p. (×100).",
      "Point colour/shape = significance (wild bootstrap p-values, B=9999, Webb weights).",
      "Grey rectangle = 2016 reference year (baseline, β normalized to 0).",
      "Clustered at sector level (10 clusters). Year×quarter FE.",
      "Red dotted lines = MW events (2017Q2, 2019Q3, 2021Q3, 2023Q2).",
      sep="\n"
    )
    p <- plot_event_study(boot_ci[[nm]], title=paste("MW Exposure:", label),
                          subtitle=subtitle, y_label=y_label)
    save_plot(p, paste0(file_prefix, "_", nm), plot_path)
  })
  
  invisible(boot_ci)
}

#===============================================================================
# STEP 10. Table Helper
#
# Outputs: HTML (full notes, primary working format)
#          .tex fragment (ASCII-safe notes, clean headers, for \input{} in paper)
# No PDF output — compile .tex manually with tinytex::pdflatex() if needed.
#===============================================================================

coef_map_4 <- c(
  "window::mid_2017_2019:exposure_sf_val"        = "Exposure × Post-2017",
  "window::post_2021_2022:exposure_sf_val"        = "Exposure × Post-2021",
  "window::post_2023:exposure_sf_val"             = "Exposure × Post-2023",
  "window::mid_2017_2019:exposure_baseline_val"   = "Exposure × Post-2017",
  "window::post_2021_2022:exposure_baseline_val"  = "Exposure × Post-2021",
  "window::post_2023:exposure_baseline_val"       = "Exposure × Post-2023"
)

coef_map_covid <- c(
  "window::mid_2017_2019:exposure_sf_val"        = "Exposure × Post-2017",
  "window::post_2021_2022:exposure_sf_val"        = "Exposure × Post-2021",
  "window::post_2023:exposure_sf_val"             = "Exposure × Post-2023",
  "covid_period"                                  = "COVID period (level)",
  "covid_period:exposure_sf_val"                  = "COVID × Exposure (p.p.)",
  "window::mid_2017_2019:exposure_baseline_val"   = "Exposure × Post-2017",
  "window::post_2021_2022:exposure_baseline_val"  = "Exposure × Post-2021",
  "window::post_2023:exposure_baseline_val"       = "Exposure × Post-2023",
  "covid_period:exposure_baseline_val"            = "COVID × Exposure (p.p.)"
)

gof_map <- tribble(
  ~raw,            ~clean,         ~fmt,
  "nobs",          "Observations",   0,
  "r.squared",     "R²",             3,
  "adj.r.squared", "Adj. R²",        3
)

save_table <- function(models, coef_map, title, notes, file_base, path) {
  
  # Rename models for clean LaTeX column headers (no underscores)
  models_renamed <- setNames(models,
                             vapply(names(models), function(nm) {
                               switch(nm,
                                      log_var_wage = "Log Wage Var.",
                                      below_min    = "Non-compliance",
                                      informal     = "Informality",
                                      nm
                               )
                             }, character(1))
  )
  
  # ASCII-safe notes for .tex (no ×, –, or other non-ASCII)
  tex_notes <- list(
    "Exposure and proportion outcomes scaled x100 (p.p.). log\\_var\\_wage in log units.",
    "Cell/sector and year x quarter FE. Weighted by baseline employment share.",
    paste0("SEs clustered at sector level (10 clusters). ",
           "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.")
  )
  
  # HTML — primary working output, full notes preserved
  modelsummary(
    models, coef_map=coef_map, gof_map=gof_map,
    stars=c("*"=0.10,"**"=0.05,"***"=0.01),
    vcov=~sector, title=title, notes=notes,
    output=file.path(path, paste0(file_base, ".html"))
  )
  
  # .tex fragment — for \input{} in paper
  modelsummary(
    models_renamed, coef_map=coef_map, gof_map=gof_map,
    stars=c("*"=0.10,"**"=0.05,"***"=0.01),
    vcov=~sector, title=title, notes=tex_notes,
    output=file.path(path, paste0(file_base, ".tex"))
  )
  
  cat("Saved table:", file.path(path, file_base), "\n")
}

cat("=== 05a helper loaded ===\n\n")