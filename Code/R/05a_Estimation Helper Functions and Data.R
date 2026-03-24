#===============================================================================
# Script: 05a_ES_Helpers_and_Data.R
#
# PURPOSE:
#   Shared setup for event study regression scripts 05b and 05c.
#   Sourced at the top of both — do NOT run standalone.
#
# WHAT THIS FILE DOES:
#   1. Loads panel data and constructs sector-level panel
#   2. Scales variables to percentage points
#   3. Builds reg_sf, reg_s (COVID-excluded) and reg_sf_covid, reg_s_covid
#   4. Defines all regression helper functions (run_es_*, collapsed, COVID)
#   5. Defines bootstrap helper
#   6. Defines plot helper
#   7. Defines shared metadata (OUTCOMES, OUTCOME_LABELS, coef maps, gof map)
#   8. Defines output paths (out_path_sf, out_path_s)
#
# CHANGES FROM ORIGINAL 05_Event Study Regressions.R:
#   [C1] Units scaled to p.p.: exposure ×100, below_min ×100, informal ×100
#   [C2] Time FE uses year×quarter (time_fe) not annual year_fe
#   [C3] Four-window collapsed regression (pre/mid17/post21/post23)
#   [C4] COVID robustness spec with covid_period dummy + exposure interaction
#   [C5] Full-data (unvalidated) robustness using panel_sf_full.rds
#   [C6] Separate output paths for SectorxFirmSize and Sector
#   [FIX] sector_fe added explicitly in reg_s mutate (defensive)
#===============================================================================

source("Code/R/00_setup.R")

library(fixest)
library(modelsummary)
library(fwildclusterboot)
library(glue)


#===============================================================================
# PATHS
#===============================================================================

pd <- config$paths$processed_data

# Output paths — confirmed from config  [C6]
out_path_sf <- file.path(config$paths$outputs, config$output_stage,
                         config$out_subdirs$reg_sector_firmsize)
out_path_s  <- file.path(config$paths$outputs, config$output_stage,
                         config$out_subdirs$reg_sector)

dir.create(out_path_sf, recursive = TRUE, showWarnings = FALSE)
dir.create(out_path_s,  recursive = TRUE, showWarnings = FALSE)


#===============================================================================
# CONSTANTS
#===============================================================================

COVID_QTRS <- c("2020Q1", "2020Q2", "2020Q3", "2020Q4", "2021Q1", "2021Q2")

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
# STEP 1. Load Data
#===============================================================================

# Validated panel (Drop/Out-of-scope cells set to NA per outcome)
panel_sf <- readRDS(file.path(pd, "panel_sf_clean.rds")) %>%
  mutate(time = as.character(time))

# Full panel — no cell exclusions applied (for full-data robustness)  [C5]
# Falls back to panel_sf if panel_sf_full.rds does not yet exist.
panel_sf_full_path <- file.path(pd, "panel_sf_full.rds")
if (file.exists(panel_sf_full_path)) {
  panel_sf_full <- readRDS(panel_sf_full_path) %>%
    mutate(time = as.character(time))
  cat("Full unvalidated panel loaded (panel_sf_full.rds).\n")
} else {
  # Fallback: use the validated panel — full-data robustness will be identical
  # to main spec until panel_sf_full.rds is produced by cell validation script.
  panel_sf_full <- panel_sf
  warning("panel_sf_full.rds not found — full-data robustness uses validated panel as fallback.")
}

# Sector-level exposure for robustness spec
exposure_s <- readRDS(file.path(pd, "sector_mw_exposure_baseline.rds"))


#===============================================================================
# STEP 2. Build Sector × Quarter Panels (collapse from cell panel)
#
# Both validated and full-data versions.
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
    mutate(
      group     = Employment_Sector,
      sector_fe = factor(Employment_Sector)
    )
}

panel_s      <- build_panel_s(panel_sf)
panel_s_full <- build_panel_s(panel_sf_full)


#===============================================================================
# STEP 3. Scale Variables to Percentage Points  [C1]
#
# exposure: ×100 → 1 unit = 1 p.p. of workers near min wage
# below_min, informal: ×100 → coefficients in p.p.
# log_var_wage: NOT scaled — already unit-free log
#===============================================================================

scale_pp <- function(df, is_sector = FALSE) {
  exp_col <- if (is_sector) "exposure_baseline_val" else "exposure_sf_val"
  df %>%
    mutate(
      !!exp_col  := .data[[exp_col]] * 100,
      below_min  = below_min  * 100,
      informal   = informal   * 100
    )
}

panel_sf      <- scale_pp(panel_sf)
panel_sf_full <- scale_pp(panel_sf_full)
panel_s       <- scale_pp(panel_s,      is_sector = TRUE)
panel_s_full  <- scale_pp(panel_s_full, is_sector = TRUE)

cat("=== UNIT SCALING APPLIED (×100) ===\n")
cat("exposure_sf_val range:      ",
    round(range(panel_sf$exposure_sf_val, na.rm = TRUE), 1), "\n")
cat("exposure_baseline_val range:",
    round(range(panel_s$exposure_baseline_val, na.rm = TRUE), 1), "\n\n")


#===============================================================================
# STEP 4. Build Regression Datasets  [C2: time_fe = year×quarter]
#
# Four datasets:
#   reg_sf       — validated, COVID excluded  (main spec)
#   reg_s        — sector-level, COVID excluded  (robustness spec)
#   reg_sf_covid — validated, COVID included  (COVID robustness)
#   reg_s_covid  — sector-level, COVID included
#   reg_sf_full  — unvalidated, COVID excluded  (full-data robustness) [C5]
#   reg_s_full   — sector-level unvalidated, COVID excluded
#===============================================================================

prep_reg_sf <- function(panel, exclude_covid = TRUE) {
  d <- panel
  if (exclude_covid) d <- filter(d, !time %in% COVID_QTRS)
  d %>%
    filter(!is.na(exposure_sf_val)) %>%
    mutate(
      cell_fe  = factor(cell_id),
      year_num = as.integer(substr(time, 1, 4)),
      time_fe  = factor(time),                      # year×quarter FE  [C2]
      year_fe  = relevel(factor(year_num), ref = "2016"),
      sector   = factor(Employment_Sector)
    )
}

prep_reg_s <- function(panel, exclude_covid = TRUE) {
  d <- panel
  if (exclude_covid) d <- filter(d, !time %in% COVID_QTRS)
  d %>%
    filter(!is.na(exposure_baseline_val)) %>%
    mutate(
      year_num  = as.integer(substr(time, 1, 4)),
      time_fe   = factor(time),
      year_fe   = relevel(factor(year_num), ref = "2016"),
      sector    = factor(Employment_Sector),
      sector_fe = factor(Employment_Sector)          # explicit [FIX]
    )
}

reg_sf       <- prep_reg_sf(panel_sf,      exclude_covid = TRUE)
reg_s        <- prep_reg_s(panel_s,        exclude_covid = TRUE)
reg_sf_covid <- prep_reg_sf(panel_sf,      exclude_covid = FALSE)
reg_s_covid  <- prep_reg_s(panel_s,        exclude_covid = FALSE)
reg_sf_full  <- prep_reg_sf(panel_sf_full, exclude_covid = TRUE)
reg_s_full   <- prep_reg_s(panel_s_full,   exclude_covid = TRUE)

cat("=== DATASET DIMENSIONS ===\n")
cat("reg_sf (main):         ", nrow(reg_sf),       "obs,",
    n_distinct(reg_sf$cell_id), "cells\n")
cat("reg_s (robustness):    ", nrow(reg_s),        "obs,",
    n_distinct(reg_s$Employment_Sector), "sectors\n")
cat("reg_sf_covid:          ", nrow(reg_sf_covid), "obs\n")
cat("reg_s_covid:           ", nrow(reg_s_covid),  "obs\n")
cat("reg_sf_full:           ", nrow(reg_sf_full),  "obs\n")
cat("reg_s_full:            ", nrow(reg_s_full),   "obs\n\n")


#===============================================================================
# STEP 5. Window Helper Functions  [C3]
#
# Four-window collapsed regression:
#   pre            = 2014Q3–2016Q4   (reference)
#   mid_2017_2019  = 2017Q1–2019Q2   (post-Event A)
#   post_2021_2022 = 2021Q4–2023Q1   (post-Event C)
#   post_2023      = 2023Q3–2025Q2   (post-Event D — cleanest)
#
# COVID window helper adds covid_gap for inclusion robustness  [C4]
#===============================================================================

add_window_4 <- function(data) {
  data %>%
    mutate(
      year_num_w    = as.integer(substr(time, 1, 4)),
      quarter_num_w = as.integer(substr(time, 6, 6)),
      time_idx      = year_num_w + (quarter_num_w - 1) / 4,
      window = case_when(
        time_idx <  2017.00                             ~ "pre",
        time_idx >= 2017.00 & time_idx <= 2019.25       ~ "mid_2017_2019",
        time_idx >= 2021.75 & time_idx <= 2023.00       ~ "post_2021_2022",
        time_idx >= 2023.50                             ~ "post_2023",
        TRUE                                            ~ NA_character_
      )
    ) %>%
    filter(!is.na(window)) %>%
    mutate(window = factor(window,
                           levels = c("pre", "mid_2017_2019",
                                      "post_2021_2022", "post_2023")))
}

add_window_covid <- function(data) {
  data %>%
    mutate(
      year_num_w    = as.integer(substr(time, 1, 4)),
      quarter_num_w = as.integer(substr(time, 6, 6)),
      time_idx      = year_num_w + (quarter_num_w - 1) / 4,
      covid_period  = as.integer(time %in% COVID_QTRS),
      window = case_when(
        time_idx <  2017.00                             ~ "pre",
        time_idx >= 2017.00 & time_idx <= 2019.25       ~ "mid_2017_2019",
        time_idx >  2019.25 & time_idx <= 2021.50       ~ "covid_gap",
        time_idx >= 2021.75 & time_idx <= 2023.00       ~ "post_2021_2022",
        time_idx >= 2023.50                             ~ "post_2023",
        TRUE                                            ~ NA_character_
      )
    ) %>%
    filter(!is.na(window)) %>%
    mutate(window = factor(window,
                           levels = c("pre", "mid_2017_2019", "covid_gap",
                                      "post_2021_2022", "post_2023")))
}


#===============================================================================
# STEP 6. Regression Helper Functions
#
# All functions take `outcome` (string) and `data` as arguments so they
# work for validated, COVID, and full-data datasets without duplication.
#===============================================================================

# ── Event study: sector × firm size ──────────────────────────────────────────
run_es_sf <- function(outcome, data, ref = 2016) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) { message("Skipping '", outcome, "' — insufficient obs."); return(NULL) }
  feols(as.formula(glue(
    "{outcome} ~ i(year_num, exposure_sf_val, ref = {ref}) | cell_fe + time_fe"
  )), data = d, weights = ~pi, vcov = ~sector)
}

# ── Event study: sector ───────────────────────────────────────────────────────
run_es_s <- function(outcome, data, ref = 2016) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) { message("Skipping '", outcome, "' — insufficient obs."); return(NULL) }
  feols(as.formula(glue(
    "{outcome} ~ i(year_num, exposure_baseline_val, ref = {ref}) | sector_fe + time_fe"
  )), data = d, weights = ~pi_sector, vcov = ~sector)
}

# ── Collapsed (4-window): sector × firm size ──────────────────────────────────
run_collapsed_sf <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(as.formula(glue(
    "{outcome} ~ i(window, exposure_sf_val, ref = 'pre') | cell_fe + time_fe"
  )), data = d, weights = ~pi, vcov = ~sector)
}

# ── Collapsed (4-window): sector ──────────────────────────────────────────────
run_collapsed_s <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(as.formula(glue(
    "{outcome} ~ i(window, exposure_baseline_val, ref = 'pre') | sector_fe + time_fe"
  )), data = d, weights = ~pi_sector, vcov = ~sector)
}

# ── COVID robustness: sector × firm size  [C4] ────────────────────────────────
run_covid_sf <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(as.formula(glue(
    "{outcome} ~ i(window, exposure_sf_val, ref = 'pre') +
                 covid_period + covid_period:exposure_sf_val |
                 cell_fe + time_fe"
  )), data = d, weights = ~pi, vcov = ~sector)
}

# ── COVID robustness: sector  [C4] ───────────────────────────────────────────
run_covid_s <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(as.formula(glue(
    "{outcome} ~ i(window, exposure_baseline_val, ref = 'pre') +
                 covid_period + covid_period:exposure_baseline_val |
                 sector_fe + time_fe"
  )), data = d, weights = ~pi_sector, vcov = ~sector)
}


#===============================================================================
# STEP 7. Bootstrap Helpers
#===============================================================================

refit_unweighted_sf <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  fit <- feols(as.formula(glue(
    "{outcome} ~ i(year_num, exposure_sf_val, ref = 2016) | cell_fe + time_fe"
  )), data = d, vcov = ~sector)
  attr(fit, "fit_data") <- d
  fit
}

refit_unweighted_s <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  fit <- feols(as.formula(glue(
    "{outcome} ~ i(year_num, exposure_baseline_val, ref = 2016) | sector_fe + time_fe"
  )), data = d, vcov = ~sector)
  attr(fit, "fit_data") <- d
  fit
}

bootstrap_ci <- function(fit, B = 9999, seed = 42, conf_level = 0.95) {
  coef_names <- names(coef(fit))
  es_coefs   <- coef_names[grepl("year_num::", coef_names, fixed = TRUE)]
  if (length(es_coefs) == 0) return(NULL)
  fit_data <- attr(fit, "fit_data")
  if (is.null(fit_data)) stop("fit_data attribute missing.")
  set.seed(seed)
  map_dfr(es_coefs, function(cn) {
    bt <- tryCatch(
      boottest(object = fit, param = cn, B = B,
               clustid = "sector", data = fit_data,
               type = "webb", sign_level = 1 - conf_level),
      error = function(e) { warning("Bootstrap failed for ", cn, ": ", e$message); NULL }
    )
    if (is.null(bt)) {
      return(tibble(term = cn, estimate = coef(fit)[cn],
                    conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
    }
    tidy_bt <- generics::tidy(bt)
    tibble(term = cn, estimate = coef(fit)[cn],
           conf.low = tidy_bt$conf.low, conf.high = tidy_bt$conf.high,
           p.value = tidy_bt$p.value)
  })
}


#===============================================================================
# STEP 8. Plot Helper
#===============================================================================

parse_year <- function(term) as.integer(regmatches(term, regexpr("\\d{4}", term)))

plot_event_study <- function(boot_tbl, title = NULL, subtitle = NULL,
                             ref_year = 2016, y_label = NULL) {
  if (is.null(boot_tbl) || nrow(boot_tbl) == 0) return(NULL)
  plot_data <- boot_tbl %>%
    mutate(year = parse_year(term)) %>%
    bind_rows(tibble(year = ref_year, estimate = 0,
                     conf.low = 0, conf.high = 0, p.value = NA_real_)) %>%
    arrange(year) %>%
    mutate(
      sig = case_when(
        p.value < 0.01 ~ "p < 0.01", p.value < 0.05 ~ "p < 0.05",
        p.value < 0.10 ~ "p < 0.10", TRUE ~ "n.s."
      ),
      sig = factor(sig, levels = c("p < 0.01", "p < 0.05", "p < 0.10", "n.s."))
    )
  ggplot(plot_data, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    annotate("rect", xmin = ref_year - 0.5, xmax = ref_year + 0.5,
             ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey50") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = "#2166ac") +
    geom_line(colour = "#2166ac", linewidth = 0.7) +
    geom_point(aes(colour = sig, shape = sig), size = 2.5) +
    geom_vline(xintercept = MW_EVENT_YEARS, linetype = "dotted",
               colour = "red", alpha = 0.6, linewidth = 0.5) +
    scale_colour_manual(
      values = c("p < 0.01" = "#d73027", "p < 0.05" = "#fc8d59",
                 "p < 0.10" = "#fee090", "n.s." = "grey60"), drop = FALSE) +
    scale_shape_manual(
      values = c("p < 0.01" = 16, "p < 0.05" = 16, "p < 0.10" = 17, "n.s." = 1),
      drop = FALSE) +
    scale_x_continuous(breaks = seq(2014, 2025, 1)) +
    labs(title = title, subtitle = subtitle, x = NULL,
         y = y_label %||% "Coefficient on Exposure (p.p.) × Year",
         colour = "Significance", shape = "Significance") +
    theme_surveytools() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.ticks.x = element_line())
}

save_plot <- function(p, name, path, w = config$fig_defaults$width,
                      h = config$fig_defaults$height) {
  ggsave(file.path(path, paste0(name, ".", config$fig_defaults$format)),
         p, width = w, height = h, dpi = config$fig_defaults$dpi)
  cat("Saved:", name, "\n")
}


#===============================================================================
# STEP 9. Shared Table Metadata
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
  ~raw,            ~clean,       ~fmt,
  "nobs",          "Observations", 0,
  "r.squared",     "R²",           3,
  "adj.r.squared", "Adj. R²",      3
)

# Helper: run modelsummary with consistent defaults
save_table <- function(models, coef_map, title, notes, file_base, path) {
  modelsummary(models, coef_map = coef_map, gof_map = gof_map,
               stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
               vcov = ~sector, title = title, notes = notes,
               output = file.path(path, paste0(file_base, ".html")))
  modelsummary(models, coef_map = coef_map, gof_map = gof_map,
               stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
               vcov = ~sector, title = title,
               notes = list("* p<0.10, ** p<0.05, *** p<0.01."),
               output = file.path(path, paste0(file_base, ".tex")))
  cat("Saved table:", file_base, "\n")
}

cat("=== 05a_ES_Helpers_and_Data.R loaded ===\n\n")
