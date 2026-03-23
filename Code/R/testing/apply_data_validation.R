# ==============================================================================
# 03_apply_validation_and_prep_regression.R
#
# PURPOSE:
#   Bridge between 02_cell_validation.R and 04_regressions.R.
#   Applies cell-level reliability decisions (from the decision table) to the
#   sector × firmsize × quarter panel, then aggregates to a sector × quarter
#   panel that is ready for the event study regressions.
#
# INPUTS:
#   - panel_sector_firmsize_quarter.rds   (sector × firmsize × quarter panel)
#   - validation_decision_table.rds       (from 02_cell_validation.R)
#   - sector_mw_exposure_baseline.rds     (sector-level exposure, from 01_build_panels.R)
#   - sector_firmsize_mw_exposure_baseline.rds  (sector × firmsize exposure)
#
# OUTPUTS:
#   - sector_time_panel.rds               (sector × quarter, outcome columns
#                                          set to NA where cell was dropped/out-of-scope)
#   - regression_sample_log.txt           (human-readable log of what was dropped per outcome)
#
# NOTES:
#   - "Out of scope" cells are treated the same as "Drop" — outcome set to NA
#     for that cell. The distinction matters for interpretation (structural zero
#     vs. unreliable), but both are excluded from regression.
#   - "Review" cells are KEPT. Robustness checks should re-run dropping them.
#   - Aggregation from sector × firmsize to sector uses pi (employment share)
#     weights from the baseline, consistent with how exposure was aggregated.
#   - COVID quarters (2020Q1–2021Q2) are flagged but NOT dropped here — the
#     regression script handles exclusion so robustness checks can add them back.
# ==============================================================================

source("Code/R/00_setup.R")

library(tidyverse)

library(gt)

# ==============================================================================
# 0. CONFIGURATION
# ==============================================================================

# Outcomes we are running regressions on
OUTCOME_COLS <- c(
  "log_var_wage",     # log of variance of wages (aggregate)
  "log_50_10",        # log(p50/p10)
  "log_90_10",        # log(p90/p10)
  "below_min_formal", # share of formal workers below minimum wage
  "informal"          # share informal (restricted subsample — see notes)
)

# Map from decision_table outcome keys (02_cell_validation.R) to panel_sf columns
OUTCOME_COL_MAP <- c(
  log_wage   = "log_var_wage",      # validation ran on log_wage; apply to log_var_wage
  log_50_10  = "log_50_10",
  below_min  = "below_min_formal",
  informal   = "informal"
)
# log_90_10 not in validation — inherits log_50_10 decisions (conservative)

EXCLUDE_RECOMMENDATIONS <- c(
  "Drop — multiple reliability failures",
  "Drop — majority of quarters below minimum n",
  "Drop — thin + borderline concern",
  "Out of scope — structural near-zero"
)

COVID_QUARTERS <- c("2020Q1", "2020Q2", "2020Q3", "2020Q4", "2021Q1", "2021Q2")

# ==============================================================================
# 1. LOAD DATA
# ==============================================================================

cat("Loading inputs...\n")

panel_sf <- readRDS(
  file.path(config$paths$processed_data, "panel_sector_firmsize_quarter.rds")
)

decision_table <- readRDS(
  file.path(config$paths$processed_data, "validation_decision_table.rds")
)

exposure_s  <- readRDS(
  file.path(config$paths$processed_data, "sector_mw_exposure_baseline.rds")
)

exposure_sf <- readRDS(
  file.path(config$paths$processed_data, "sector_firmsize_mw_exposure_baseline.rds")
)


panel_emp_privsec_wagegrp <- readRDS(file.path(config$paths$processed_data, "panel_emp_privsec_wagegrp.rds"))
design                    <- readRDS(file.path(config$paths$processed_data, "design_restrictive.rds"))

cat("Panel dimensions (sector × firmsize × quarter):", nrow(panel_sf), "rows\n")
cat("Sectors:", n_distinct(panel_sf$Employment_Sector), "\n")
cat("Firm size tiers:", n_distinct(panel_sf$Wage_group), "\n")
cat("Quarters:", n_distinct(panel_sf$time), "\n\n")

# ==============================================================================
# 2. APPLY CELL VALIDATION RULES
#
# For each outcome, set the outcome column to NA for excluded cells.
# Each outcome is masked independently — a cell excluded for informal
# can still contribute its log_50_10 to the aggregation.
# log_90_10 inherits exclusions from log_50_10.
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("APPLYING CELL VALIDATION RULES\n")
cat(strrep("=", 70), "\n\n")

excluded_cells <- decision_table %>%
  filter(recommendation %in% EXCLUDE_RECOMMENDATIONS) %>%
  select(outcome, Employment_Sector, Wage_group, recommendation)

exclusion_summary <- excluded_cells %>%
  group_by(outcome, recommendation) %>%
  summarise(n_cells = n(), .groups = "drop") %>%
  arrange(outcome, recommendation)

cat("Exclusion summary:\n")
print(exclusion_summary, n = Inf)
cat("\n")

apply_exclusions <- function(data, outcome_key, panel_col) {
  excl <- excluded_cells %>%
    filter(outcome == outcome_key) %>%
    select(Employment_Sector, Wage_group) %>%
    mutate(EXCLUDE = TRUE)
  
  if (nrow(excl) == 0) {
    cat(sprintf("  %-12s → %-20s : no cells excluded\n",
                outcome_key, panel_col))
    return(data)
  }
  
  data <- data %>%
    left_join(excl, by = c("Employment_Sector", "Wage_group")) %>%
    mutate(
      !!panel_col := if_else(coalesce(EXCLUDE, FALSE), NA_real_, .data[[panel_col]])
    ) %>%
    select(-EXCLUDE)
  
  n_masked <- sum(is.na(data[[panel_col]]))
  cat(sprintf("  %-12s → %-20s : %d cells excluded → %d rows set to NA\n",
              outcome_key, panel_col, nrow(excl), n_masked))
  data
}

panel_sf_masked <- panel_sf

for (key in names(OUTCOME_COL_MAP)) {
  panel_sf_masked <- apply_exclusions(panel_sf_masked, key, OUTCOME_COL_MAP[key])
}

# log_90_10 inherits log_50_10 validation
panel_sf_masked <- apply_exclusions(panel_sf_masked, "log_50_10", "log_90_10")
cat("  (log_90_10 inherits log_50_10 validation decisions — conservative)\n\n")

panel_sf_masked <- panel_sf_masked %>%
  mutate(covid_flag  = time %in% COVID_QUARTERS)
#==============================================================================
  # 3. AGGREGATE TO SECTOR x QUARTER
  #
  # Weighted mean across firm size tiers using baseline pi weights already in
  # panel_sf. NA cells do not contribute — pi weights renormalised over
  # non-missing tiers only so they always sum to 1.
  # ==============================================================================

cat(strrep("=", 70), "\n")
cat("AGGREGATING TO SECTOR x QUARTER\n")
cat(strrep("=", 70), "\n\n")

missing_pi <- panel_sf_masked %>%
  filter(is.na(pi)) %>%
  distinct(Employment_Sector, Wage_group)

if (nrow(missing_pi) > 0) {
  warning("Cells missing pi weight (excluded from aggregation):\n",
          paste(paste(missing_pi$Employment_Sector,
                      missing_pi$Wage_group, sep = " | "),
                collapse = "\n"))
}

aggregate_outcome <- function(data, outcome_col) {
  data %>%
    group_by(Employment_Sector, time) %>%
    summarise(
      !!outcome_col := {
        y  <- .data[[outcome_col]]
        w  <- pi
        ok <- !is.na(y) & !is.na(w)
        if (sum(ok) == 0) NA_real_
        else sum(y[ok] * w[ok]) / sum(w[ok])
      },
      !!paste0(outcome_col, "_n_tiers") := sum(!is.na(.data[[outcome_col]])),
      .groups = "drop"
    )
}

# Build sector x quarter skeleton then join each outcome
panel_s <- panel_sf_masked %>%
  distinct(Employment_Sector, time) %>%
  arrange(Employment_Sector, time)

for (col in OUTCOME_COLS) {
  agg <- aggregate_outcome(panel_sf_masked, col)
  panel_s <- panel_s %>%
    left_join(agg, by = c("Employment_Sector", "time"))
}

cat("Sector x quarter panel:", nrow(panel_s), "rows\n")
cat("Sectors:", n_distinct(panel_s$Employment_Sector), "\n")
cat("Quarters:", n_distinct(panel_s$time), "\n\n")

cat("Non-missing rates per outcome:\n")
panel_s %>%
  summarise(across(all_of(OUTCOME_COLS),
                   ~ scales::percent(mean(!is.na(.x)), accuracy = 1))) %>%
  print()
cat("\n")


# ==============================================================================
# 4. SECTOR EMPLOYMENT WEIGHTS FROM MICRODATA
#
# Survey-weighted total employment per sector x quarter from the private
# sector microdata. Computed before cell masking so weights reflect true
# sector size, not the reliability-filtered subsample.
# ==============================================================================

cat("Computing sector employment weights...\n")

sector_employment <- panel_emp_privsec_wagegrp %>%
  group_by(Employment_Sector, year_quarter) %>%
  summarise(
    sector_employment = sum(FACTOR_EXPANSION, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(time = year_quarter)

# Join onto BOTH panels
panel_sf_masked <- panel_sf_masked %>%
  left_join(sector_employment, by = c("Employment_Sector", "time"))

panel_s <- panel_s %>%
  left_join(sector_employment, by = c("Employment_Sector", "time"))

n_missing_wt_sf <- sum(is.na(panel_sf_masked$sector_employment))
n_missing_wt_s  <- sum(is.na(panel_s$sector_employment))

if (n_missing_wt_sf > 0) {
  warning(n_missing_wt_sf, " rows in sector x firmsize panel missing employment weight.")
} else {
  cat("Employment weights (sector x firmsize): OK (no missing)\n")
}

if (n_missing_wt_s > 0) {
  warning(n_missing_wt_s, " rows in sector panel missing employment weight.")
} else {
  cat("Employment weights (sector x quarter):  OK (no missing)\n\n")
}



sector_employment <- panel_emp_privsec_wagegrp %>%
  group_by(Employment_Sector, Wage_group, year_quarter) %>%
  summarise(
    sector_employment = sum(FACTOR_EXPANSION, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(time = year_quarter) %>%
  filter(time == "2024Q4")


gt(sector_employment,
   rowname_col = c("Employment_Sector"),
)



?gt

# ==============================================================================
# 5. ATTACH SECTOR-LEVEL EXPOSURE AND TIME VARIABLES
# ==============================================================================

# Sector-level exposure (pi-weighted average of firm size exposures,
# computed in 01_build_panels.R)
panel_s <- panel_s %>%
  left_join(
    exposure_s %>% select(Employment_Sector, exposure_baseline_val, exposure_group),
    by = "Employment_Sector"
  )

n_missing_exp <- sum(is.na(panel_s$exposure_baseline_val))
if (n_missing_exp > 0) {
  warning(n_missing_exp, " rows missing exposure — check sector name consistency.")
} else {
  cat("Sector exposure: OK (no missing)\n\n")
}

# Time variables — time format "2016Q1", "2016Q2", etc.
panel_s <- panel_s %>%
  mutate(
    year_num    = as.integer(str_sub(time, 1, 4)),
    quarter_num = as.integer(str_sub(time, 6, 6)),
    year_fct    = factor(year_num, levels = sort(unique(year_num))),
    year_fct    = relevel(year_fct, ref = "2016"),
    covid_flag  = time %in% COVID_QUARTERS
  )

cat("Reference year (must be 2016):", levels(panel_s$year_fct)[1], "\n\n")

# Rename for regression script compatibility
panel_s <- panel_s %>%
  rename(group = Employment_Sector)



# ==============================================================================
# 6. CAUTION FLAGS
#
# "Review" cells kept in main spec but flagged for robustness checks.
# ==============================================================================

caution_cells <- decision_table %>%
  filter(str_detect(recommendation, "Review|Caution")) %>%
  select(outcome, Employment_Sector, Wage_group, recommendation)

if (nrow(caution_cells) > 0) {
  cat("Cells retained with Review/Caution flag:\n")
  print(caution_cells, n = Inf)
  cat("\n")
}

panel_s <- panel_s %>%
  left_join(
    caution_cells %>%
      distinct(Employment_Sector) %>%
      mutate(any_caution = TRUE),
    by = c("group" = "Employment_Sector")
  ) %>%
  mutate(any_caution = coalesce(any_caution, FALSE))

# ==============================================================================
# 7. FINAL CHECKS
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("FINAL PANEL SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("Rows:", nrow(panel_s), "\n")
cat("Sectors:", n_distinct(panel_s$group), "\n")
cat("Quarters:", n_distinct(panel_s$time), "\n")
cat("Years:", paste(sort(unique(panel_s$year_num)), collapse = ", "), "\n\n")

cat("Exposure distribution across sectors:\n")
panel_s %>%
  distinct(group, exposure_baseline_val) %>%
  summarise(
    min    = round(min(exposure_baseline_val,    na.rm = TRUE), 4),
    median = round(median(exposure_baseline_val, na.rm = TRUE), 4),
    max    = round(max(exposure_baseline_val,    na.rm = TRUE), 4),
    sd     = round(sd(exposure_baseline_val,     na.rm = TRUE), 4)
  ) %>%
  print()

cat("\nOutcome coverage (non-NA rows, non-COVID quarters):\n")
panel_s %>%
  filter(!covid_flag) %>%
  summarise(across(all_of(OUTCOME_COLS), ~ sum(!is.na(.x)))) %>%
  pivot_longer(everything(),
               names_to = "outcome", values_to = "n_nonmissing") %>%
  print()

cat("\nReference year (2016) non-missing obs per outcome:\n")
panel_s %>%
  filter(year_num == 2016) %>%
  summarise(across(all_of(OUTCOME_COLS), ~ sum(!is.na(.x)))) %>%
  print()

# ==============================================================================
# 8. SAVE
# ==============================================================================

saveRDS(panel_s,
        file.path(config$paths$processed_data, "sector_time_panel.rds"))



saveRDS(panel_sf_masked,
        file.path(config$paths$processed_data, "restricted_sector_time_panel.rds"))


log_path <- file.path(config$paths$processed_data, "regression_sample_log.txt")
sink(log_path)
cat("REGRESSION SAMPLE LOG\n")
cat("Generated:", format(Sys.time()), "\n\n")
cat("=== EXCLUSION SUMMARY ===\n")
print(exclusion_summary, n = Inf)
cat("\n=== CAUTION CELLS RETAINED ===\n")
print(caution_cells, n = Inf)
cat("\n=== INFORMAL SUBSAMPLE NOTE ===\n")
cat("Informal restricted to Micro/Small tiers, 6 sectors.\n")
cat("Surviving cells exposure range: 0.055-0.189\n")
cat("Full panel exposure range:      0.000-0.330\n")
cat("Subsample skews mid-exposure — coefficients not directly comparable",
    "to wage outcomes.\n")
cat("\n=== OUTCOME COVERAGE (non-COVID) ===\n")
panel_s %>%
  filter(!covid_flag) %>%
  summarise(across(all_of(OUTCOME_COLS), ~ sum(!is.na(.x)))) %>%
  pivot_longer(everything(),
               names_to = "outcome", values_to = "n_nonmissing") %>%
  print()
sink()

cat("\nSaved:\n")
cat("  sector_time_panel.rds     →",
    file.path(config$paths$processed_data, "sector_time_panel.rds"), "\n")
cat("  regression_sample_log.txt →", log_path, "\n")
