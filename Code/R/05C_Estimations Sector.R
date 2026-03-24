#===============================================================================
# Script: 05C_Estimations Sector.R
#
# PURPOSE:
#   Event study regressions — SECTOR × QUARTER panel.
#   Mirrors 05b but uses sector-level exposure and sector FE.
#
# OUTPUT STRUCTURE:
#   Regression Results/Sector/Main/       ← validated main spec
#   Regression Results/Sector/Robustness/ ← COVID, full tables
#   Regression Results/Sector/Full Data/  ← unvalidated event study
#
# SECTIONS:
#   A. Main event study + bootstrap + plots  (validated → Main/)
#   B. Four-window collapsed table           (validated → Main/)
#   C. COVID robustness table                (validated → Robustness/)
#   D. Full-data event study + table         (unvalidated → Full Data/)
#
# NOTE: No drop-Review-cells robustness — Review cells are a cell-level
#   concept; after sector aggregation they are diluted by other cells.
#
# REQUIRES: source("Code/R/05a_Estimation Helper Functions and Data.R")
#===============================================================================

source("Code/R/05a_Estimation Helper Functions and Data.R")


#===============================================================================
# A. MAIN EVENT STUDY — VALIDATED → Main/
#===============================================================================

cat(strrep("=", 70), "\n")
cat("A. MAIN EVENT STUDY — SECTOR × QUARTER (Validated)\n")
cat(strrep("=", 70), "\n\n")

boot_ci_s <- run_and_plot_es(
  reg_data       = reg_s,
  refit_fn       = refit_unweighted_s,
  es_fn          = run_es_s,
  plot_path      = out_s_main,
  file_prefix    = "es_s",
  subtitle_extra = "Robustness spec: sector × quarter. Exposure at sector level. Validated. 2016 base year.",
  B              = 9999
)

saveRDS(boot_ci_s, file.path(pd, "bootstrap_ci_s.rds"))
cat("Bootstrap CIs saved.\n\n")


#===============================================================================
# B. FOUR-WINDOW COLLAPSED — VALIDATED → Main/
#===============================================================================

cat(strrep("=", 70), "\n")
cat("B. FOUR-WINDOW COLLAPSED — SECTOR (Validated)\n")
cat(strrep("=", 70), "\n\n")

reg_s_win <- add_window_4(reg_s)
cat("Observations per window:\n"); reg_s_win %>% count(window) %>% print(); cat("\n")

collapsed_s <- map(names(OUTCOMES), run_collapsed_s, data=reg_s_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

for (nm in names(collapsed_s)) {
  cat("---", nm, "---\n"); print(summary(collapsed_s[[nm]])); cat("\n")
}

save_table(
  models    = collapsed_s,
  coef_map  = coef_map_4,
  title     = "Robustness: Minimum Wage Exposure — Sector × Quarter Panel",
  notes     = list(
    "Robustness spec: sector × quarter. Validated. Exposure aggregated to sector level (p.p.).",
    "below_min and informal ×100 (p.p.). log_var_wage in log units.",
    "Sector and year×quarter FE. Weighted by baseline sector employment share.",
    "SEs clustered at sector level (10 clusters). * p<0.10, ** p<0.05, *** p<0.01.",
    "COVID (2020Q1–2021Q2) excluded. Treatment quarters excluded.",
    "Windows: pre=2014Q3–2016Q4; mid=2017Q1–2019Q2; post21=2021Q4–2023Q1; post23=2023Q3–2025Q2."
  ),
  file_base = "table_s_collapsed_4window",
  path      = out_s_main
)


#===============================================================================
# C. COVID ROBUSTNESS — VALIDATED → Robustness/
#===============================================================================

cat(strrep("=", 70), "\n")
cat("C. COVID ROBUSTNESS — SECTOR × QUARTER\n")
cat(strrep("=", 70), "\n\n")

reg_s_covid_win <- add_window_covid(reg_s_covid)
cat("Observations per window (COVID inclusive):\n")
reg_s_covid_win %>% count(window, covid_period) %>% print(); cat("\n")

collapsed_s_covid <- map(names(OUTCOMES), run_covid_s,
                         data=reg_s_covid_win) %>%
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
    "Compare post-2021 and post-2023 to Main/ table.",
    "Sector and year×quarter FE. Clustered by sector. * p<0.10, ** p<0.05, *** p<0.01."
  ),
  file_base = "table_s_covid_robustness",
  path      = out_s_rob
)


#===============================================================================
# D. FULL-DATA — UNVALIDATED → Full Data/
#===============================================================================

cat(strrep("=", 70), "\n")
cat("D. FULL-DATA — SECTOR × QUARTER (Unvalidated)\n")
cat(strrep("=", 70), "\n\n")

boot_ci_s_full <- run_and_plot_es(
  reg_data       = reg_s_full,
  refit_fn       = refit_unweighted_s,
  es_fn          = run_es_s,
  plot_path      = out_s_full,
  file_prefix    = "es_s_full",
  subtitle_extra = "Full data spec: all cells incl. thin/unvalidated aggregated to sector. 2016 base year.",
  B              = 9999
)

saveRDS(boot_ci_s_full, file.path(pd, "bootstrap_ci_s_full.rds"))

reg_s_full_win <- add_window_4(reg_s_full)
cat("Observations per window (full data, sector):\n")
reg_s_full_win %>% count(window) %>% print(); cat("\n")

collapsed_s_full <- map(names(OUTCOMES), run_collapsed_s,
                        data=reg_s_full_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

save_table(
  models    = collapsed_s_full,
  coef_map  = coef_map_4,
  title     = "Full-Data Robustness: All Cells Including Thin — Sector × Quarter",
  notes     = list(
    "Full-data robustness: cell validation exclusions removed before sector aggregation.",
    "Noisier estimates expected. Compare to Main/ table for stability.",
    "Sector and year×quarter FE. Clustered by sector. * p<0.10, ** p<0.05, *** p<0.01."
  ),
  file_base = "table_s_full_collapsed_4window",
  path      = out_s_full
)

cat("\n=== 05c complete ===\n")
cat("Main results:   ", out_s_main, "\n")
cat("Robustness:     ", out_s_rob,  "\n")
cat("Full data:      ", out_s_full, "\n")

