#===============================================================================
# Script: 05c_ES_Sector.R
#
# PURPOSE:
#   Event study regressions — SECTOR × QUARTER panel (robustness spec).
#   Mirrors 05b exactly but uses sector-level exposure and sector FE.
#   Saves all outputs to: Outputs/Work In Progress/Regression Results/Sector/
#
# RUNS:
#   A. Main event study + wild bootstrap + event study plots
#   B. Four-window collapsed regression
#   C. COVID robustness (collapsed, with covid_period dummy)
#   D. Full-data robustness (unvalidated, survey weights)
#
# REQUIRES: source("Code/R/05a_ES_Helpers_and_Data.R") first
#
# NOTE: No drop-Review-cells robustness here — Review cells are a cell-level
#   concept that does not apply to the sector-aggregated panel directly.
#===============================================================================

source("Code/R/05a_ES_Helpers_and_Data.R")


#===============================================================================
# A. MAIN EVENT STUDY — SECTOR
#===============================================================================

cat(strrep("=", 70), "\n")
cat("A. EVENT STUDY — SECTOR × QUARTER\n")
cat(strrep("=", 70), "\n\n")

es_s <- map(OUTCOMES, run_es_s, data = reg_s) %>% compact()
cat("Estimated:", paste(names(es_s), collapse = ", "), "\n\n")

cat("Sectors and observations per outcome:\n")
map_dfr(names(OUTCOMES), function(col) {
  reg_s %>%
    filter(!is.na(.data[[col]])) %>%
    summarise(n_obs = n(), n_sectors = n_distinct(Employment_Sector),
              .groups = "drop") %>%
    mutate(outcome = col)
}) %>% select(outcome, n_obs, n_sectors) %>% print()

cat("\nNOTE: 10 clusters — wild bootstrap required for reliable inference.\n\n")

for (nm in names(es_s)) {
  cat("---", nm, "---\n"); print(summary(es_s[[nm]])); cat("\n")
}

# ── Wild cluster bootstrap ────────────────────────────────────────────────────
cat("Refitting without weights for bootstrap...\n")
es_s_unw <- map(names(OUTCOMES), refit_unweighted_s, data = reg_s) %>%
  setNames(names(OUTCOMES)) %>% compact()

cat("Running bootstrap (B=9999)...\n")
set.seed(42)
boot_ci_s <- map(es_s_unw, bootstrap_ci, B = 9999, seed = 42) %>% compact()

walk(names(boot_ci_s), function(nm) {
  n_na <- sum(is.na(boot_ci_s[[nm]]$conf.low))
  if (n_na > 0) warning(nm, ": ", n_na, " bootstrap CIs are NA.")
})

saveRDS(boot_ci_s, file.path(pd, "bootstrap_ci_s.rds"))
cat("Bootstrap CIs saved.\n\n")

# ── Event study plots ─────────────────────────────────────────────────────────
subtitle_s <- paste(
  "Robustness spec: sector × quarter. Exposure aggregated to sector level, 2016 baseline.",
  "Exposure and proportions in p.p. (×100). Coefficient: outcome change per 1 p.p. exposure.",
  "Shaded band = wild bootstrap 95% CI (B=9999, Webb weights, unweighted refit).",
  "Clustered at sector level (10 clusters). COVID 2020Q1–2021Q2 excluded.",
  "Year×quarter time FE. Red dotted lines = MW events (2017Q2, 2019Q3, 2021Q3, 2023Q2).",
  sep = "\n"
)

iwalk(OUTCOME_LABELS, function(label, nm) {
  if (!nm %in% names(boot_ci_s)) return(NULL)
  y_label <- if (nm == "log_var_wage") "Coefficient × Year [log units]" else "Coefficient × Year [p.p.]"
  p <- plot_event_study(boot_ci_s[[nm]],
                        title    = paste("MW Exposure (Sector):", label),
                        subtitle = subtitle_s,
                        y_label  = y_label)
  if (!is.null(p)) save_plot(p, paste0("es_s_", nm), out_path_s)
})


#===============================================================================
# B. FOUR-WINDOW COLLAPSED REGRESSION — SECTOR
#===============================================================================

cat(strrep("=", 70), "\n")
cat("B. FOUR-WINDOW COLLAPSED — SECTOR × QUARTER\n")
cat(strrep("=", 70), "\n\n")

reg_s_win <- add_window_4(reg_s)

cat("Observations per window:\n")
reg_s_win %>% count(window) %>% print(); cat("\n")

collapsed_s <- map(names(OUTCOMES), run_collapsed_s, data = reg_s_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

for (nm in names(collapsed_s)) {
  cat("---", nm, "---\n"); print(summary(collapsed_s[[nm]])); cat("\n")
}

save_table(
  models    = collapsed_s,
  coef_map  = coef_map_4,
  title     = "Robustness: Minimum Wage Exposure — Sector × Quarter Panel",
  notes     = list(
    "Robustness spec: sector × quarter. Exposure aggregated to sector level (p.p.).",
    "below_min and informal ×100 (p.p.). log_var_wage in log units.",
    "Sector and year×quarter FE. Weighted by baseline sector employment share.",
    "SEs clustered at sector level (10 clusters). * p<0.10, ** p<0.05, *** p<0.01.",
    "COVID (2020Q1–2021Q2) excluded. Treatment quarters excluded.",
    "Windows: pre=2014Q3–2016Q4; mid=2017Q1–2019Q2; post21=2021Q4–2023Q1; post23=2023Q3–2025Q2."
  ),
  file_base = "table_s_collapsed_4window",
  path      = out_path_s
)


#===============================================================================
# C. COVID ROBUSTNESS — SECTOR  [C4]
#===============================================================================

cat(strrep("=", 70), "\n")
cat("C. COVID ROBUSTNESS — SECTOR × QUARTER\n")
cat(strrep("=", 70), "\n\n")

reg_s_covid_win <- add_window_covid(reg_s_covid)

cat("Observations per window (COVID inclusive):\n")
reg_s_covid_win %>% count(window, covid_period) %>% print(); cat("\n")

collapsed_s_covid <- map(names(OUTCOMES), run_covid_s,
                         data = reg_s_covid_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

for (nm in names(collapsed_s_covid)) {
  cat("---", nm, "(COVID robustness, sector) ---\n")
  print(summary(collapsed_s_covid[[nm]])); cat("\n")
}

save_table(
  models    = collapsed_s_covid,
  coef_map  = coef_map_covid,
  title     = "COVID Robustness: Sector × Quarter (COVID Quarters Included)",
  notes     = list(
    "COVID robustness: 2020Q1–2021Q2 included with covid_period dummy.",
    "covid_period:exposure allows COVID to differentially affect high-exposure sectors.",
    "Compare post-2021 and post-2023 to main sector table — stable = COVID not driving results.",
    "Sector and year×quarter FE. Clustered by sector. * p<0.10, ** p<0.05, *** p<0.01."
  ),
  file_base = "table_s_covid_robustness",
  path      = out_path_s
)


#===============================================================================
# D. FULL-DATA ROBUSTNESS — SECTOR  [C5]
#===============================================================================

cat(strrep("=", 70), "\n")
cat("D. FULL-DATA ROBUSTNESS — SECTOR × QUARTER\n")
cat(strrep("=", 70), "\n\n")

reg_s_full_win <- add_window_4(reg_s_full)

cat("Observations per window (full data, sector):\n")
reg_s_full_win %>% count(window) %>% print(); cat("\n")

collapsed_s_full <- map(names(OUTCOMES), run_collapsed_s,
                        data = reg_s_full_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

for (nm in names(collapsed_s_full)) {
  cat("---", nm, "(full data, sector) ---\n")
  print(summary(collapsed_s_full[[nm]])); cat("\n")
}

save_table(
  models    = collapsed_s_full,
  coef_map  = coef_map_4,
  title     = "Full-Data Robustness: All Cells Including Thin — Sector × Quarter",
  notes     = list(
    "Full-data robustness: cell validation exclusions removed before sector aggregation.",
    "Sector-level outcomes weighted by full (unvalidated) employment shares.",
    "Noisier estimates expected. Compare to main sector table for stability.",
    "Sector and year×quarter FE. Clustered by sector. * p<0.10, ** p<0.05, *** p<0.01."
  ),
  file_base = "table_s_full_data_robustness",
  path      = out_path_s
)

cat("\n=== 05c_ES_Sector.R complete ===\n")