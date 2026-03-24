#===============================================================================
# Script: 05C_Estimations Sector.R
#
# SECTIONS:
#   A. Main event study + bootstrap + plots  (validated → Main/)
#   B. Four-window collapsed table           (validated → Main/)
#   C. COVID robustness table                (validated → Robustness/)
#   D. Full-data event study + table         (unvalidated → Full Data/)
#   E. Post-COVID 2023 restricted regression (validated → Robustness/)
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

collapsed_s_covid <- map(names(OUTCOMES), run_covid_s, data=reg_s_covid_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

for (nm in names(collapsed_s_covid)) {
  cat("---", nm, "(COVID robustness, sector) ---\n"); print(summary(collapsed_s_covid[[nm]])); cat("\n")
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

collapsed_s_full <- map(names(OUTCOMES), run_collapsed_s, data=reg_s_full_win) %>%
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


#===============================================================================
# E. POST-COVID 2023 RESTRICTED REGRESSION → Robustness/
#
# Mirrors Section F of 05b but for the sector × quarter panel.
# See 05b Section F for full documentation.
#===============================================================================

cat(strrep("=", 70), "\n")
cat("E. POST-COVID 2023 RESTRICTED REGRESSION — SECTOR × QUARTER\n")
cat(strrep("=", 70), "\n\n")

reg_s_postcovid <- reg_s_win %>%
  filter(window %in% c("post_2021_2022", "post_2023")) %>%
  mutate(window = factor(window, levels = c("post_2021_2022", "post_2023")))

cat("Observations per window (post-COVID restricted, sector):\n")
reg_s_postcovid %>% count(window) %>% print(); cat("\n")

run_postcovid_s <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(
    as.formula(glue(
      "{outcome} ~ i(window, exposure_baseline_val, ref='post_2021_2022') | sector_fe + time_fe"
    )),
    data    = d,
    weights = ~pi_sector,
    vcov    = ~sector
  )
}

collapsed_s_postcovid <- map(names(OUTCOMES), run_postcovid_s,
                             data = reg_s_postcovid) %>%
  setNames(names(OUTCOMES)) %>% compact()

cat("Post-COVID restricted results (sector):\n")
for (nm in names(collapsed_s_postcovid)) {
  cat("---", nm, "---\n"); print(summary(collapsed_s_postcovid[[nm]])); cat("\n")
}

coef_map_postcovid_s <- c(
  "window::post_2023:exposure_baseline_val" =
    "Exposure × Post-2023 (ref: Post-2021 baseline)"
)

save_table(
  models    = collapsed_s_postcovid,
  coef_map  = coef_map_postcovid_s,
  title     = "Post-COVID Restricted: 2023 Event vs Post-2021 Baseline — Sector × Quarter",
  notes     = list(
    "Restricted to 2021Q4–2025Q2 only. Both windows fully post-COVID.",
    "Reference = post_2021_2022 (2021Q4–2023Q1); Treatment = post_2023 (2023Q3–2025Q2).",
    "Exposure = 2016 baseline sector-level (×100, p.p.).",
    "Compare β to 'Exposure × Post-2023' in main sector table (table_s_collapsed_4window).",
    "Sector and year×quarter FE. Clustered by sector. * p<0.10, ** p<0.05, *** p<0.01."
  ),
  file_base = "table_s_postcovid_2023",
  path      = out_s_rob
)

cat("\n=== 05c complete ===\n")
cat("Main results:   ", out_s_main, "\n")
cat("Robustness:     ", out_s_rob,  "\n")
cat("Full data:      ", out_s_full, "\n")