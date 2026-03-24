#===============================================================================
# Script: 05b_Estimations Sector x Firm Size.R
#
# PURPOSE:
#   Event study regressions — SECTOR × FIRM SIZE × QUARTER panel.
#
# OUTPUT STRUCTURE:
#   Regression Results/Sector x Firm Size/Main/       ← validated main spec
#   Regression Results/Sector x Firm Size/Robustness/ ← COVID, strict, full tables
#   Regression Results/Sector x Firm Size/Full Data/  ← unvalidated event study
#
# SECTIONS:
#   A. Main event study + bootstrap + plots  (validated → Main/)
#   B. Four-window collapsed table           (validated → Main/)
#   C. COVID robustness table                (validated → Robustness/)
#   D. Full-data event study + table         (unvalidated → Full Data/)
#   E. Drop-Review-cells robustness table    (validated → Robustness/)
#
# REQUIRES: source("Code/R/05a_Estimation Helper Functions and Data.R")
#===============================================================================

source("Code/R/05a_Estimation Helper Functions and Data.R")


#===============================================================================
# A. MAIN EVENT STUDY — VALIDATED SAMPLE → Main/
#===============================================================================

cat(strrep("=", 70), "\n")
cat("A. MAIN EVENT STUDY — SECTOR × FIRM SIZE (Validated)\n")
cat(strrep("=", 70), "\n\n")

boot_ci_sf <- run_and_plot_es(
  reg_data       = reg_sf,
  refit_fn       = refit_unweighted_sf,
  es_fn          = run_es_sf,
  plot_path      = out_sf_main,
  file_prefix    = "es_sf",
  subtitle_extra = "Main spec: sector × firm size × quarter. Validated sample. 2016 base year.",
  B              = 9999
)

saveRDS(boot_ci_sf, file.path(pd, "bootstrap_ci_sf.rds"))
cat("Bootstrap CIs saved.\n\n")


#===============================================================================
# B. FOUR-WINDOW COLLAPSED — VALIDATED → Main/
#===============================================================================

cat(strrep("=", 70), "\n")
cat("B. FOUR-WINDOW COLLAPSED — VALIDATED\n")
cat(strrep("=", 70), "\n\n")

reg_sf_win <- add_window_4(reg_sf)
cat("Observations per window:\n"); reg_sf_win %>% count(window) %>% print(); cat("\n")

collapsed_sf <- map(names(OUTCOMES), run_collapsed_sf, data=reg_sf_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

for (nm in names(collapsed_sf)) {
  cat("---", nm, "---\n"); print(summary(collapsed_sf[[nm]])); cat("\n")
}

save_table(
  models    = collapsed_sf,
  coef_map  = coef_map_4,
  title     = "Minimum Wage Exposure and Labor Market Outcomes — Sector × Firm Size",
  notes     = list(
    "Main spec: sector × firm size × quarter. Validated cells. Four-window collapsed event study.",
    "Exposure = share of formal workers within 10% of min wage, 2016 baseline (×100, p.p.).",
    "below_min and informal ×100 (p.p.). log_var_wage in log units.",
    "Cell and year×quarter FE. Weighted by baseline employment share.",
    "SEs clustered at sector level (10 clusters). * p<0.10, ** p<0.05, *** p<0.01.",
    "COVID (2020Q1–2021Q2) excluded. Treatment quarters excluded.",
    "Windows: pre=2014Q3–2016Q4; mid=2017Q1–2019Q2; post21=2021Q4–2023Q1; post23=2023Q3–2025Q2.",
    "post_2023 is the most cleanly identified event: post-COVID pre-period."
  ),
  file_base = "table_sf_collapsed_4window",
  path      = out_sf_main
)


#===============================================================================
# C. COVID ROBUSTNESS — VALIDATED → Robustness/
#===============================================================================

cat(strrep("=", 70), "\n")
cat("C. COVID ROBUSTNESS — SECTOR × FIRM SIZE\n")
cat(strrep("=", 70), "\n\n")

reg_sf_covid_win <- add_window_covid(reg_sf_covid)
cat("Observations per window (COVID inclusive):\n")
reg_sf_covid_win %>% count(window, covid_period) %>% print(); cat("\n")

collapsed_sf_covid <- map(names(OUTCOMES), run_covid_sf,
                          data=reg_sf_covid_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

for (nm in names(collapsed_sf_covid)) {
  cat("---", nm, "(COVID robustness) ---\n")
  print(summary(collapsed_sf_covid[[nm]])); cat("\n")
}

save_table(
  models    = collapsed_sf_covid,
  coef_map  = coef_map_covid,
  title     = "COVID Robustness: Sector × Firm Size (COVID Quarters Included)",
  notes     = list(
    "COVID robustness: 2020Q1–2021Q2 included with covid_period dummy.",
    "covid_period:exposure allows COVID to differentially affect high-exposure cells.",
    "Compare post-2021 and post-2023 to Main/ table — stable = COVID not driving results.",
    "Cell and year×quarter FE. Clustered by sector. * p<0.10, ** p<0.05, *** p<0.01."
  ),
  file_base = "table_sf_covid_robustness",
  path      = out_sf_rob
)


#===============================================================================
# D. FULL-DATA ROBUSTNESS — UNVALIDATED → Full Data/
#
# Runs full event study (with bootstrap and plots) AND collapsed table.
# Plots saved to Full Data/ with prefix "es_sf_full" to distinguish from Main/.
# Compare plots and table to Main/ — if similar, thin cells not driving results.
#===============================================================================

cat(strrep("=", 70), "\n")
cat("D. FULL-DATA — SECTOR × FIRM SIZE (Unvalidated)\n")
cat(strrep("=", 70), "\n\n")

boot_ci_sf_full <- run_and_plot_es(
  reg_data       = reg_sf_full,
  refit_fn       = refit_unweighted_sf,
  es_fn          = run_es_sf,
  plot_path      = out_sf_full,
  file_prefix    = "es_sf_full",
  subtitle_extra = "Full data spec: all cells incl. thin/unvalidated. Survey weights. 2016 base year.",
  B              = 9999
)

saveRDS(boot_ci_sf_full, file.path(pd, "bootstrap_ci_sf_full.rds"))

# Collapsed table for full data
reg_sf_full_win <- add_window_4(reg_sf_full)
cat("Observations per window (full data):\n")
reg_sf_full_win %>% count(window) %>% print(); cat("\n")

collapsed_sf_full <- map(names(OUTCOMES), run_collapsed_sf,
                         data=reg_sf_full_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

save_table(
  models    = collapsed_sf_full,
  coef_map  = coef_map_4,
  title     = "Full-Data Robustness: All Cells Including Thin — Sector × Firm Size",
  notes     = list(
    "Full-data robustness: cell validation exclusions removed.",
    "All sector × firm size cells included regardless of sample size.",
    "Survey expansion weights (pi) used — represent population, not precision.",
    "Noisier estimates expected in thin cells. Compare to Main/ table.",
    "Cell and year×quarter FE. Clustered by sector. * p<0.10, ** p<0.05, *** p<0.01."
  ),
  file_base = "table_sf_full_collapsed_4window",
  path      = out_sf_full
)


#===============================================================================
# E. DROP-REVIEW-CELLS ROBUSTNESS → Robustness/
#===============================================================================

cat(strrep("=", 70), "\n")
cat("E. DROP-REVIEW-CELLS ROBUSTNESS — SECTOR × FIRM SIZE\n")
cat(strrep("=", 70), "\n\n")

review_cells <- readRDS(file.path(pd, "validation_decision_table.rds")) %>%
  filter(recommendation == "Review")

if (nrow(review_cells) == 0) {
  cat("No Review cells — robustness check not needed.\n\n")
} else {
  cat("Review cells being dropped:\n")
  print(review_cells %>% select(outcome, Employment_Sector, Wage_group)); cat("\n")
  
  reg_sf_strict <- reg_sf_win  # already windowed from Section B
  
  for (out_name in unique(review_cells$outcome)) {
    if (!out_name %in% names(OUTCOMES)) next
    drop_cells <- review_cells %>%
      filter(outcome == out_name) %>%
      mutate(cell_id = paste0(Employment_Sector, "__", Wage_group)) %>%
      pull(cell_id)
    reg_sf_strict <- reg_sf_strict %>%
      mutate(!!out_name := if_else(cell_id %in% drop_cells,
                                   NA_real_, .data[[out_name]]))
  }
  
  collapsed_sf_strict <- map(names(OUTCOMES), run_collapsed_sf,
                             data=reg_sf_strict) %>%
    setNames(names(OUTCOMES)) %>% compact()
  
  save_table(
    models    = collapsed_sf_strict,
    coef_map  = coef_map_4,
    title     = "Robustness: Drop Review Cells (Sector × Firm Size)",
    notes     = list(
      "Review cells (borderline reliability) excluded.",
      "All other settings identical to Main/ validated spec."
    ),
    file_base = "table_sf_strict_robustness",
    path      = out_sf_rob
  )
}

cat("\n=== 05b complete ===\n")
cat("Main results:   ", out_sf_main, "\n")
cat("Robustness:     ", out_sf_rob,  "\n")
cat("Full data:      ", out_sf_full, "\n")

