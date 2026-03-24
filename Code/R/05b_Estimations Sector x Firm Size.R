#===============================================================================
# Script: 05b_ES_SectorxFirmSize.R
#
# PURPOSE:
#   Event study regressions — SECTOR × FIRM SIZE × QUARTER panel.
#   Saves all outputs to: Outputs/Work In Progress/Regression Results/
#                         Sector x Firm Size/
#
# RUNS:
#   A. Main event study + wild bootstrap + event study plots
#   B. Four-window collapsed regression (main table)
#   C. COVID robustness (collapsed, with covid_period dummy)
#   D. Full-data robustness (unvalidated cells, survey weights)
#   E. Drop-Review-cells robustness
#
# REQUIRES: source("Code/R/05a_ES_Helpers_and_Data.R") first
#===============================================================================

source("Code/R/05a_Estimation Helper Functions and Data.R")


#===============================================================================
# A. MAIN EVENT STUDY
#===============================================================================

cat(strrep("=", 70), "\n")
cat("A. EVENT STUDY — SECTOR × FIRM SIZE\n")
cat(strrep("=", 70), "\n\n")

es_sf <- map(OUTCOMES, run_es_sf, data = reg_sf) %>% compact()
cat("Estimated:", paste(names(es_sf), collapse = ", "), "\n\n")

# Diagnostics
cat("Cells and clusters contributing per outcome:\n")
map_dfr(names(OUTCOMES), function(col) {
  reg_sf %>%
    filter(!is.na(.data[[col]])) %>%
    summarise(n_obs = n(), n_cells = n_distinct(cell_id),
              n_sectors = n_distinct(Employment_Sector), .groups = "drop") %>%
    mutate(outcome = col)
}) %>% select(outcome, n_obs, n_cells, n_sectors) %>% print()

cat("\nNOTE: n_sectors < 30 — wild bootstrap required for reliable inference.\n\n")

for (nm in names(es_sf)) {
  cat("---", nm, "---\n"); print(summary(es_sf[[nm]])); cat("\n")
}

# ── Wild cluster bootstrap ────────────────────────────────────────────────────
cat("Refitting without weights for bootstrap...\n")
es_sf_unw <- map(names(OUTCOMES), refit_unweighted_sf, data = reg_sf) %>%
  setNames(names(OUTCOMES)) %>% compact()

cat("Running bootstrap (B=9999)...\n")
set.seed(42)
boot_ci_sf <- map(es_sf_unw, bootstrap_ci, B = 9999, seed = 42) %>% compact()

walk(names(boot_ci_sf), function(nm) {
  n_na <- sum(is.na(boot_ci_sf[[nm]]$conf.low))
  if (n_na > 0) warning(nm, ": ", n_na, " bootstrap CIs are NA.")
})

saveRDS(boot_ci_sf, file.path(pd, "bootstrap_ci_sf.rds"))
cat("Bootstrap CIs saved.\n\n")

# ── Event study plots ─────────────────────────────────────────────────────────
subtitle_sf <- paste(
  "Main spec: sector × firm size × quarter. 2016 base year.",
  "Exposure and proportions in p.p. (×100). Coefficient: outcome change per 1 p.p. exposure.",
  "Shaded band = wild bootstrap 95% CI (B=9999, Webb weights, unweighted refit).",
  "Clustered at sector level (10 clusters). COVID 2020Q1–2021Q2 excluded.",
  "Year×quarter time FE. Red dotted lines = MW events (2017Q2, 2019Q3, 2021Q3, 2023Q2).",
  sep = "\n"
)

iwalk(OUTCOME_LABELS, function(label, nm) {
  if (!nm %in% names(boot_ci_sf)) return(NULL)
  y_label <- if (nm == "log_var_wage") "Coefficient × Year [log units]" else "Coefficient × Year [p.p.]"
  p <- plot_event_study(boot_ci_sf[[nm]],
                        title    = paste("MW Exposure:", label),
                        subtitle = subtitle_sf,
                        y_label  = y_label)
  if (!is.null(p)) save_plot(p, paste0("es_sf_", nm), out_path_sf)
})


#===============================================================================
# B. FOUR-WINDOW COLLAPSED REGRESSION — MAIN TABLE
#===============================================================================

cat(strrep("=", 70), "\n")
cat("B. FOUR-WINDOW COLLAPSED — SECTOR × FIRM SIZE\n")
cat(strrep("=", 70), "\n\n")

reg_sf_win <- add_window_4(reg_sf)

cat("Observations per window:\n")
reg_sf_win %>% count(window) %>% print(); cat("\n")

collapsed_sf <- map(names(OUTCOMES), run_collapsed_sf, data = reg_sf_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

for (nm in names(collapsed_sf)) {
  cat("---", nm, "---\n"); print(summary(collapsed_sf[[nm]])); cat("\n")
}

save_table(
  models    = collapsed_sf,
  coef_map  = coef_map_4,
  title     = "Minimum Wage Exposure and Labor Market Outcomes — Sector × Firm Size",
  notes     = list(
    "Main spec: sector × firm size × quarter. Four-window collapsed event study.",
    "Exposure = share of formal workers within 10% of min wage, 2016 baseline (×100, p.p.).",
    "below_min and informal also ×100 (p.p.). log_var_wage in log units.",
    "Cell and year×quarter FE. Weighted by baseline employment share.",
    "SEs clustered at sector level (10 clusters). * p<0.10, ** p<0.05, *** p<0.01.",
    "COVID (2020Q1–2021Q2) excluded. Treatment quarters excluded.",
    "Windows: pre=2014Q3–2016Q4; mid=2017Q1–2019Q2; post21=2021Q4–2023Q1; post23=2023Q3–2025Q2.",
    "post_2023 is the most cleanly identified event: post-COVID pre-period."
  ),
  file_base = "table_sf_collapsed_4window",
  path      = out_path_sf
)


#===============================================================================
# C. COVID ROBUSTNESS — SECTOR × FIRM SIZE  [C4]
#===============================================================================

cat(strrep("=", 70), "\n")
cat("C. COVID ROBUSTNESS — SECTOR × FIRM SIZE\n")
cat(strrep("=", 70), "\n\n")

reg_sf_covid_win <- add_window_covid(reg_sf_covid)

cat("Observations per window (COVID inclusive):\n")
reg_sf_covid_win %>% count(window, covid_period) %>% print(); cat("\n")

collapsed_sf_covid <- map(names(OUTCOMES), run_covid_sf,
                          data = reg_sf_covid_win) %>%
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
    "Compare post-2021 and post-2023 to main table — stable = COVID not driving results.",
    "Cell and year×quarter FE. Clustered by sector. * p<0.10, ** p<0.05, *** p<0.01."
  ),
  file_base = "table_sf_covid_robustness",
  path      = out_path_sf
)


#===============================================================================
# D. FULL-DATA ROBUSTNESS — SECTOR × FIRM SIZE  [C5]
#
# Uses unvalidated panel (all cells including thin/dropped ones).
# Survey expansion weights (pi) provide population representation.
# Noisier estimates expected — comparison to main spec is the diagnostic.
#===============================================================================

cat(strrep("=", 70), "\n")
cat("D. FULL-DATA ROBUSTNESS — SECTOR × FIRM SIZE\n")
cat(strrep("=", 70), "\n\n")

reg_sf_full_win <- add_window_4(reg_sf_full)

cat("Observations per window (full data):\n")
reg_sf_full_win %>% count(window) %>% print(); cat("\n")

collapsed_sf_full <- map(names(OUTCOMES), run_collapsed_sf,
                         data = reg_sf_full_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

for (nm in names(collapsed_sf_full)) {
  cat("---", nm, "(full data) ---\n")
  print(summary(collapsed_sf_full[[nm]])); cat("\n")
}

save_table(
  models    = collapsed_sf_full,
  coef_map  = coef_map_4,
  title     = "Full-Data Robustness: All Cells Including Thin (Sector × Firm Size)",
  notes     = list(
    "Full-data robustness: cell validation exclusions removed.",
    "All sector × firm size cells included regardless of sample size.",
    "Survey expansion weights (pi) used — represent population but not precision.",
    "Noisier estimates expected in thin cells. Compare to main table for stability.",
    "Cell and year×quarter FE. Clustered by sector. * p<0.10, ** p<0.05, *** p<0.01."
  ),
  file_base = "table_sf_full_data_robustness",
  path      = out_path_sf
)


#===============================================================================
# E. DROP-REVIEW-CELLS ROBUSTNESS
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
  print(review_cells %>% select(outcome, Employment_Sector, Wage_group))
  cat("\n")
  
  reg_sf_strict <- reg_sf_win
  
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
                             data = reg_sf_strict) %>%
    setNames(names(OUTCOMES)) %>% compact()
  
  save_table(
    models    = collapsed_sf_strict,
    coef_map  = coef_map_4,
    title     = "Robustness: Drop Review Cells (Sector × Firm Size)",
    notes     = list("Review cells excluded. All other settings identical to main spec."),
    file_base = "table_sf_strict_robustness",
    path      = out_path_sf
  )
}

cat("\n=== 05b_ES_SectorxFirmSize.R complete ===\n")

