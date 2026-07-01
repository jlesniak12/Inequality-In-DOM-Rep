#===============================================================================
#
# Script: 06_Construct_Exposure.R
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
#   - Unit of INFERENCE: Region4 (handled in script 07); Region10/province as
#     robustness. Region10 is CONSTRUCTED here from province (not a native var).
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

source("Code/R/clean scripts/00_setup.R")
source("Code/R/clean scripts/03_Sample Definitions.R")

library(dplyr)
library(tidyr)
library(purrr)


#===============================================================================
# STEP 0. Resolve parameters from config
#===============================================================================

cat("[06] Constructing baseline minimum-wage exposure measure\n")

pd <- config$paths$processed_data

GEO          <- config$exposure$construct_geo        # "DES_PROVINCIA"
TIER_SCHEME  <- config$exposure$tier_scheme          # "4tier" | "3tier"
BASE_YEAR    <- config$exposure$baseline_year        # 2016
TOL          <- config$exposure$mw_compliance_tolerance
BAND_LOWER   <- 1 - TOL                               # seam-consistent with below_min
BAND_UPPER   <- config$exposure$mw_band_upper
BAND_GRID    <- config$exposure$mw_band_upper_grid
INCOME_VAR   <- config$exposure$income_hourly        # real_salary_primary_hourly_base

# Tier variable and matching hourly floor selected by scheme
if (TIER_SCHEME == "4tier") {
  TIER_VAR  <- "Wage_group"
  FLOOR_VAR <- config$exposure$minwage_hourly_4tier  # real_minwage_hourly
  TIER_KEEP <- c("Micro", "Small", "Medium", "Large")
} else if (TIER_SCHEME == "3tier") {
  TIER_VAR  <- "Wage_group_3tier"
  FLOOR_VAR <- config$exposure$minwage_hourly_3tier  # real_minwage_hourly_3tier
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
# STEP 1. Province -> 10 Development Regions (Decreto 710-2004)
#
# Constructed from province because Region10 is NOT a native variable. Mapping
# taken from Diseno_muestral.pdf p.1. NOTE: the survey carries the OLD province
# name SALCEDO for what is officially Hermanas Mirabal — mapped accordingly so
# the province is not dropped.
#===============================================================================

region10_map <- tribble(
  ~DES_PROVINCIA,            ~Region10,
  "DISTRITO NACIONAL",       "Ozama o Gran Santo Domingo",
  "SANTO DOMINGO",           "Ozama o Gran Santo Domingo",
  "SANTIAGO",                "Cibao Norte",
  "ESPAILLAT",               "Cibao Norte",
  "PUERTO PLATA",            "Cibao Norte",
  "LA VEGA",                 "Cibao Sur",
  "MONSEÑOR NOUEL",          "Cibao Sur",
  "SANCHEZ RAMIREZ",         "Cibao Sur",
  "DUARTE",                  "Cibao Nordeste",
  "SALCEDO",                 "Cibao Nordeste",   # = Hermanas Mirabal
  "MARIA TRINIDAD SANCHEZ",  "Cibao Nordeste",
  "SAMANA",                  "Cibao Nordeste",
  "VALVERDE",                "Cibao Noroeste",
  "MONTE CRISTI",            "Cibao Noroeste",
  "DAJABON",                 "Cibao Noroeste",
  "SANTIAGO RODRIGUEZ",      "Cibao Noroeste",
  "SAN CRISTOBAL",           "Valdesia",
  "PERAVIA",                 "Valdesia",
  "AZUA",                    "Valdesia",
  "SAN JOSE DE OCOA",        "Valdesia",
  "SAN JUAN",                "El Valle",
  "ELIAS PIÑA",              "El Valle",
  "BARAHONA",                "Enriquillo",
  "BAHORUCO",                "Enriquillo",
  "INDEPENDENCIA",           "Enriquillo",
  "PEDERNALES",              "Enriquillo",
  "SAN PEDRO DE MACORIS",    "Higuamo",
  "HATO MAYOR",              "Higuamo",
  "MONTE PLATA",             "Higuamo",
  "LA ROMANA",               "Yuma",
  "LA ALTAGRACIA",           "Yuma",
  "EL SEIBO",                "Yuma"
)


#===============================================================================
# STEP 2. Baseline frame: formal private employees, 2016
#
# Start from the canonical regression_sample (private employees, positive
# compliance income, known firm size, Govt/Electricity excluded, hours>0), then
# restrict to FORMAL and the baseline year. Using regression_sample guarantees
# the exposure baseline population is the SAME population the regressions run on
# (no filter drift).
#===============================================================================

baseline_df <- samples$regression_sample$data %>%
  filter(Employment_Status == "Formal",
         year == BASE_YEAR,
         .data[[TIER_VAR]] %in% TIER_KEEP) %>%
  mutate(baseline_dummy = as.character(BASE_YEAR)) %>%
  left_join(region10_map, by = "DES_PROVINCIA")

# Guard: every province should map to a region10
unmapped <- baseline_df %>% filter(is.na(Region10)) %>% distinct(DES_PROVINCIA)
if (nrow(unmapped)) {
  warning("Provinces without Region10 mapping: ",
          paste(unmapped$DES_PROVINCIA, collapse = ", "))
}

cat(sprintf("  baseline rows (formal, %d, known tier): %d across %d %s units\n",
            BASE_YEAR, nrow(baseline_df),
            dplyr::n_distinct(baseline_df[[GEO]]), GEO))


#===============================================================================
# STEP 3. Tier-bias diagnostic — 100+ ("Large") employment share by region
#
# The whole 4tier-vs-3tier bias works THROUGH the 100+ bin. If 100+ employment
# is a small / regionally-flat share, the tier choice barely matters and we use
# 4tier (legal categories) with a footnote. If it is large / regionally-varying,
# the bias is treatment-correlated and we must report both schemes as bounds.
# Computed on the 4-tier Wage_group regardless of scheme, since that is where
# "Large" (100+) is identifiable.
#===============================================================================

cat("[06] Tier-bias diagnostic: 100+ ('Large') employment share by region...\n")

# The 100+/Large employment share is just a firm-size share, so reuse
# firmsize_pi rather than hand-rolling svyby. Computed on the 4-tier Wage_group
# regardless of scheme, since that is where "Large" (100+) is identifiable.
# Done at BOTH Region4 and the construction geo so we can see whether the
# tier-bias-prone bin is large and regionally varying.
large_diag_df <- samples$regression_sample$data %>%
  filter(Employment_Status == "Formal",
         year == BASE_YEAR,
         Wage_group %in% c("Micro", "Small", "Medium", "Large")) %>%
  mutate(baseline_dummy = as.character(BASE_YEAR)) %>%
  left_join(region10_map, by = "DES_PROVINCIA")

large_share_by_unit <- function(unit) {
  firmsize_pi(
    df          = large_diag_df,
    time_var    = "baseline_dummy",
    by_vars     = c(unit, "Wage_group"),
    size_var    = "Wage_group",
    formal_only = FALSE          # already filtered to Formal above
  ) %>%
    filter(Wage_group == "Large") %>%
    transmute(unit = unit,
              unit_value = as.character(.data[[unit]]),
              large_share = pi)
}

large_share_diag <- bind_rows(
  large_share_by_unit("Region4"),
  large_share_by_unit(GEO)
)

saveRDS(large_share_diag, file.path(pd, "exposure_diag_large_share.rds"))
cat("  Large-bin (100+) employment share across ", GEO, ": ",
    sprintf("%.1f%%–%.1f%%\n",
            100 * min(large_share_diag$large_share[large_share_diag$unit == GEO], na.rm = TRUE),
            100 * max(large_share_diag$large_share[large_share_diag$unit == GEO], na.rm = TRUE)),
    sep = "")


#===============================================================================
# STEP 4. Cell counts & support diagnostics (geo x tier)
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

saveRDS(cell_diag, file.path(pd, paste0("exposure_diag_", TIER_SCHEME, ".rds")))
cat(sprintf("  geo x tier cells: %d | thin cells (n<30): %d | (n_psu<5): %d\n",
            nrow(cell_diag),
            sum(cell_diag$n_obs < 30),
            sum(cell_diag$n_psu < 5)))


#===============================================================================
# STEP 5. Exposure: share near MW (geo x tier) and firm-size weights
#
# near_mw_share + firmsize_pi + weighted_exposure, ALL ARGS EXPLICIT.
# Income/band/floor are passed; the function defaults are deliberately ignored.
#===============================================================================

cat("[06] Computing near-MW share (geo x tier) and firm-size weights...\n")

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
# STEP 6. Disaggregated exposure (geo x tier) + terciles
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
# STEP 7. Aggregated geo-level exposure (weighted over tiers) + terciles
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

# If exposure is constructed at province level, attach the Region10 and Region4
# labels so script 07 can aggregate/cluster at coarser geographies without
# recomputing. (Region4 comes from the survey; join via a province->Region4
# crosswalk built from the baseline frame.)
if (GEO == "DES_PROVINCIA") {
  prov_to_region4 <- baseline_df %>%
    distinct(DES_PROVINCIA, Region4)
  exposure_geo <- exposure_geo %>%
    left_join(region10_map,    by = "DES_PROVINCIA") %>%
    left_join(prov_to_region4, by = "DES_PROVINCIA")
}

# Consistency check: aggregated value == manual weighted sum of cell values
agg_check <- exposure_cells %>%
  group_by(across(all_of(GEO))) %>%
  summarise(exposure_manual = sum(exposure_val * pi, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(exposure_geo %>% select(all_of(c(GEO, "exposure_geo_val"))), by = GEO) %>%
  mutate(diff = abs(exposure_manual - exposure_geo_val))
cat(sprintf("  agg vs manual weighted-sum max discrepancy: %.2e (expect ~0)\n",
            max(agg_check$diff, na.rm = TRUE)))


#===============================================================================
# STEP 8. Variation diagnostics — does exposure vary enough to identify?
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
cat("[06] Exposure variation across geo units:\n")
print(var_summary)


#===============================================================================
# STEP 9. Band-width sensitivity — is the geo ranking stable across upper bands?
#
# If the High/Low exposure RANKING of geo units is stable across upper-band
# choices, the exact cutoff is innocuous and we keep the default. If it flips,
# we must say so. Reports Spearman rank correlation of geo exposure vs the
# default band.
#===============================================================================

cat("[06] Band-width sensitivity (upper-band grid)...\n")

band_sensitivity <- map_dfr(BAND_GRID, function(ub) {
  nt <- near_mw_share(
    df = baseline_df, time_var = "baseline_dummy",
    by_vars = c(GEO, TIER_VAR), min_wage = FLOOR_VAR, income = INCOME_VAR,
    out_col = "near_min", mw_lower = BAND_LOWER, mw_upper = ub, formal_only = FALSE
  )
  weighted_exposure(nt, pi_tbl, "baseline_dummy", c(GEO), TIER_VAR,
                    "near_min", "pi", "exposure_geo_val") %>%
    mutate(band_upper = ub)
})

# rank correlation of each band vs default
ref <- band_sensitivity %>% filter(band_upper == BAND_UPPER) %>%
  select(all_of(GEO), ref_val = exposure_geo_val)
band_rankcor <- band_sensitivity %>%
  left_join(ref, by = GEO) %>%
  group_by(band_upper) %>%
  summarise(spearman_vs_default =
              cor(exposure_geo_val, ref_val, method = "spearman"),
            .groups = "drop")
cat("  Spearman rank corr of geo exposure vs default band (",
    BAND_UPPER, "):\n", sep = "")
print(band_rankcor)


#===============================================================================
# STEP 10. Save outputs
#===============================================================================

saveRDS(exposure_cells, file.path(pd, paste0("exposure_cells_", TIER_SCHEME, ".rds")))
saveRDS(exposure_geo,   file.path(pd, paste0("exposure_geo_",   TIER_SCHEME, ".rds")))
saveRDS(band_sensitivity, file.path(pd, "exposure_band_sensitivity.rds"))
saveRDS(list(var_summary = var_summary, band_rankcor = band_rankcor,
             pi_check = pi_check, agg_check = agg_check),
        file.path(pd, paste0("exposure_summary_", TIER_SCHEME, ".rds")))

cat("[06] Done. Wrote exposure_cells_", TIER_SCHEME,
    ".rds, exposure_geo_", TIER_SCHEME, ".rds, diagnostics.\n", sep = "")
cat("     Re-run with config$exposure$tier_scheme='3tier' for the robustness arm.\n")
