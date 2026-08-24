#===============================================================================
#
# Script: 07_Construction of mw Exposure.R
#
# Purpose: Construct the fixed (2016 baseline) minimum-wage EXPOSURE measure that
#          serves as the continuous treatment-intensity variable for the
#          Parente-style event study, adapted to the Dominican Republic.
#
#          Exposure for a geographic unit g = the (survey-weighted) share of
#          FORMAL private employees whose wage sits AT/NEAR the firm-size-tier
#          minimum wage, aggregated over firm-size tiers using each tier's
#          baseline employment share within g as weights. This generalizes
#          Parente's "share of formal minimum-wage workers" to a setting where
#          the floor differs by firm size.
#
# Method (Parente 2024 analog):
#   - Unit of EXPOSURE CONSTRUCTION: province (config$exposure$construct_geo).
#     Fine cross-sectional treatment variation. Built on a POOLED baseline YEAR
#     (2016) so sampling error is far smaller than the quarterly design-variable
#     estimates the Central Bank's inference domain is calibrated on.
#   - Unit of INFERENCE: Region4 (handled in script 08); Region10/province as
#     robustness. Region10 and Region4 are NATIVE columns (built in 02).
#   - Exposure FIXED IN TIME at the 2016 value (Parente fixes initial exposure).
#
# Wage / floor concept (HOURLY BASE, standard 44h week; no overtime adjustment):
#   income = real_salary_primary_hourly_base
#   floor  = real_minwage_hourly        (4tier, MAIN)  |
#            real_minwage_hourly_3tier  (3tier, ROBUSTNESS)
#   band   = [1 - tolerance, mw_band_upper]   (see 00_config_additions.R)
#
# Tier-bias note: 4tier vs 3tier bracket a TREATMENT-CORRELATED measurement bias
#   in the 100+ ("Large") bin (legal mediums 100-150 are unidentifiable). We
#   construct BOTH and report the 100+ employment share by region up front so we
#   can judge how much the choice actually moves anything before committing.
#
# Reads:
#   - samples$regression_sample$data   (from 03_Sample Definitions.R)
#       Restricted further here to: Employment_Status == "Formal" & baseline year.
#       (MW is not legally binding in the informal sector, so exposure is a
#        formal-worker concept; informal workers are an OUTCOME later.)
#
# Writes (to processed_data):
#   - exposure_cells_<tier>.rds     geo x tier disaggregated (with terciles)
#   - exposure_geo_<tier>.rds       geo-level aggregated weighted exposure
#   - exposure_diag_<tier>.rds      cell counts / firm-size shares / 100+ share
#   - exposure_band_sensitivity.rds geo exposure across the upper-band grid
#
# Conventions:
#   - Sources setup + sample defs; uses near_mw_share / firmsize_pi /
#     weighted_exposure from functions_weighted_exposure.R.
#   - All function args passed EXPLICITLY (income/band/floor) — the function
#     defaults (income = real_salary_income_total, band 0.9-1.1) are NOT the
#     analysis choices and must never be relied on.
#
#===============================================================================

source(here::here("Code","R","clean scripts","00_setup.R"))
source(file.path(config$paths$scripts, "03_sample definitions.R"))


#===============================================================================
# STEP 0. Resolve parameters from config
#===============================================================================

cat("[07] Constructing baseline minimum-wage exposure measure\n")

pd <- config$data_dirs$regression

GEO          <- config$exposure$construct_geo        # "DES_PROVINCIA"
TIER_SCHEME  <- config$exposure$tier_scheme          # "4tier" | "3tier"
BASE_YEAR    <- config$exposure$baseline_year        # 2016
TOL          <- config$exposure$mw_compliance_tolerance
BAND_LOWER   <- 1 - TOL                               # seam-consistent with below_min
BAND_UPPER   <- config$exposure$mw_band_upper
BAND_GRID    <- config$exposure$mw_band_upper_grid

INCOME_VAR   <- config$income$income        # real_salary_primary_hourly_base

# Tier variable and matching hourly floor selected by scheme
if (TIER_SCHEME == "4tier") {
  TIER_VAR  <- "wage_group"
  FLOOR_VAR <- config$income$minwage_4tier_inc  # real_minwage_hourly
  TIER_KEEP <- config$TIER_LEVELS
} else if (TIER_SCHEME == "3tier") {
  TIER_VAR  <- "wage_group_3tier"
  FLOOR_VAR <- config$income$minwage_3tier_inc  # real_minwage_hourly_3tier
  TIER_KEEP <- c("Micro", "Small", "Medium/Large")
} else {
  stop("config$exposure$tier_scheme must be '4tier' or '3tier'")
}

cat(sprintf("  geo=%s | tiers=%s (%s) | floor=%s | income=%s\n",
            GEO, TIER_SCHEME, TIER_VAR, FLOOR_VAR, INCOME_VAR))
cat(sprintf("  band=[%.3f, %.2f] | baseline=%d annual\n",
            BAND_LOWER, BAND_UPPER, BASE_YEAR))

# Helper: split a numeric exposure vector into Low/Medium/High terciles.
# Defined inline (not in functions file) to keep this script self-contained.
exposure_tercile <- function(x) {
  factor(
    c("Low exposure", "Medium exposure", "High exposure")[dplyr::ntile(x, 3)],
    levels = c("Low exposure", "Medium exposure", "High exposure")
  )
}


#===============================================================================
# STEP 1. (Region10 now built natively in 02_Variable_Construction.R)
#
# Region10 (10 Development Regions, Decreto 710-2004) and Region4 are both
# columns in Full_ENCFT_clean.rds, so no crosswalk is built here. This script
# just selects whichever geography config$exposure$construct_geo names
# (DES_PROVINCIA, Region10, or Region4) as the construction unit.
#===============================================================================

stopifnot(GEO %in% c("DES_PROVINCIA", "Region10", "Region4"))


#===============================================================================
# STEP 2. Baseline frame: formal private employees, 2016
#
#===============================================================================

baseline_df <- samples$reg_tier$data %>%
  filter(Employment_Status == "Formal",
         year == BASE_YEAR,
         .data[[TIER_VAR]] %in% TIER_KEEP) %>%
  mutate(baseline_dummy = as.character(BASE_YEAR))

# Guard: Region10 should be present and non-missing for all mapped provinces
if (!"Region10" %in% names(baseline_df)) {
  stop("Region10 not found — add it to 02_Variable_Construction.R and re-run 02.")
}


cat(sprintf("  baseline rows (formal, %d, known tier): %d across %d %s units\n",
            BASE_YEAR, nrow(baseline_df),
            dplyr::n_distinct(baseline_df[[GEO]]), GEO))



#===============================================================================
# STEP 3. Cell counts & support diagnostics (geo x tier)
#
# Parente's design needs each geo x tier cell to be estimable. Report unweighted
# n AND distinct PSUs per cell so we can flag thin cells before trusting exposure.
#===============================================================================

cell_diag <- baseline_df %>%
  group_by(across(all_of(c(GEO, TIER_VAR)))) %>%
  summarise(
    n_obs  = dplyr::n(),
    n_psu  = dplyr::n_distinct(psu_unique),
    .groups = "drop"
  ) %>%
  arrange(n_obs)

saveRDS(cell_diag,        tagged_rds(pd, "exposure_diag"))
cat(sprintf("  geo x tier cells: %d | thin cells (n<30): %d | (n_psu<5): %d\n",
            nrow(cell_diag),
            sum(cell_diag$n_obs < 30),
            sum(cell_diag$n_psu < 5)))


#===============================================================================
# STEP 4. Exposure: share near MW (geo x tier) and firm-size weights
#
# near_mw_share + firmsize_pi + weighted_exposure, ALL ARGS EXPLICIT.
# Income/band/floor are passed; the function defaults are deliberately ignored.
#===============================================================================

cat("[07] Computing near-MW share (geo x tier) and firm-size weights...\n")

# 5a. Share near MW within geo x tier (the exposure primitive)
near_tbl <- near_mw_share(
  df          = baseline_df,
  time_var    = "baseline_dummy",
  by_vars     = c(GEO, TIER_VAR),
  min_wage    = FLOOR_VAR,
  income      = INCOME_VAR,
  out_col     = "near_min",
  mw_lower    = BAND_LOWER,
  mw_upper    = BAND_UPPER,
  formal_only = FALSE          # already filtered to Formal in baseline_df
)

# 5b. Firm-size employment shares within geo (the aggregation weights).
#     Built on the SAME baseline_df so the weight population matches the
#     numerator population (fixes the unfiltered-pi mismatch we flagged).
pi_tbl <- firmsize_pi(
  df          = baseline_df,
  time_var    = "baseline_dummy",
  by_vars     = c(GEO, TIER_VAR),
  size_var    = TIER_VAR,
  formal_only = FALSE
)

# 5c. Sanity: weights should sum to ~1 within each geo unit
pi_check <- pi_tbl %>%
  group_by(across(all_of(c("baseline_dummy", GEO)))) %>%
  summarise(wsum = sum(pi, na.rm = TRUE), .groups = "drop")
if (any(abs(pi_check$wsum - 1) > 1e-6)) {
  warning("firm-size weights do not sum to 1 within all geo units (max dev ",
          signif(max(abs(pi_check$wsum - 1)), 3), ")")
}



#===============================================================================
# STEP 5. Disaggregated exposure (geo x tier) + terciles
#===============================================================================

exposure_cells <- near_tbl %>%
  left_join(pi_tbl %>% select(all_of(c("baseline_dummy", GEO, TIER_VAR, "pi"))),
            by = c("baseline_dummy", GEO, TIER_VAR)) %>%
  rename(exposure_val = near_min) %>%
  # tercile WITHIN tier (ranks geos within each tier)
  group_by(across(all_of(TIER_VAR))) %>%
  mutate(exposure_group_within_tier = exposure_tercile(exposure_val)) %>%
  ungroup() %>%
  # tercile OVERALL (across all geo x tier cells)
  mutate(exposure_group_overall = exposure_tercile(exposure_val))



#===============================================================================
# STEP 6. Aggregated geo-level exposure (weighted over tiers) + terciles
#
# This is the headline treatment variable: one exposure scalar per geo unit.
#===============================================================================

exposure_geo <- weighted_exposure(
  near_tbl     = near_tbl,
  pi_tbl       = pi_tbl,
  time_var     = "baseline_dummy",
  by_vars      = c(GEO),
  weight_dim   = TIER_VAR,
  exposure_col = "near_min",
  pi_col       = "pi",
  out_col      = "exposure_geo_val"
) %>%
  arrange(desc(exposure_geo_val)) %>%
  mutate(exposure_group = exposure_tercile(exposure_geo_val))

# Attach coarser geography labels so script 08 can cluster at Region4 (the
# survey's certified inference domain) as a robustness row. Region10 and Region4
# are native columns from 02; both nest cleanly, so a distinct() crosswalk from
# the baseline frame is exact.
if (GEO == "DES_PROVINCIA") {
  xwalk <- baseline_df %>% distinct(DES_PROVINCIA, Region10, Region4)
  exposure_geo <- exposure_geo %>% left_join(xwalk, by = "DES_PROVINCIA")
} else if (GEO == "Region10") {
  xwalk <- baseline_df %>% distinct(Region10, Region4)
  exposure_geo <- exposure_geo %>% left_join(xwalk, by = "Region10")
}

# Consistency check: aggregated value == manual weighted sum of cell values

near_mw_share(df = baseline_df, time_var = "baseline_dummy", by_vars = GEO,
              min_wage = FLOOR_VAR, income = INCOME_VAR, out_col = "ungrouped",
              mw_lower = BAND_LOWER, mw_upper = BAND_UPPER, formal_only = FALSE)


#===============================================================================
# STEP 7. Variation diagnostics — does exposure vary enough to identify?
#===============================================================================

var_summary <- exposure_geo %>%
  summarise(
    n_units = dplyr::n(),
    min     = min(exposure_geo_val, na.rm = TRUE),
    p25     = quantile(exposure_geo_val, .25, na.rm = TRUE),
    median  = median(exposure_geo_val, na.rm = TRUE),
    p75     = quantile(exposure_geo_val, .75, na.rm = TRUE),
    max     = max(exposure_geo_val, na.rm = TRUE),
    sd      = sd(exposure_geo_val, na.rm = TRUE),
    iqr     = p75 - p25,
    cv      = sd / mean(exposure_geo_val, na.rm = TRUE)
  )
cat("[07] Exposure variation across geo units:\n")
print(var_summary)



#===============================================================================
# STEP 10. Save outputs
#===============================================================================

saveRDS(exposure_cells,   tagged_rds(pd, "exposure_cells"))
saveRDS(exposure_geo,     tagged_rds(pd, "exposure_geo"))

saveRDS(list(var_summary = var_summary,
             pi_check = pi_check, agg_check = agg_check),
        tagged_rds(pd, "exposure_summary"))

cat("[07] Done. Wrote exposure_cells_", TIER_SCHEME,
    ".rds, exposure_geo_", TIER_SCHEME, ".rds, diagnostics.\n", sep = "")
cat("     Re-run with config$exposure$tier_scheme='3tier' for the robustness arm.\n")



