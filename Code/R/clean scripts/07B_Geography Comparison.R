#===============================================================================
#
# Script: 06b_Geography_Comparison_Harness.R
#
# Purpose: Decide the EXPOSURE CONSTRUCTION geography by comparing cell support,
#          exposure variation, and cross-level agreement across
#          {DES_PROVINCIA, Region10, Region4} x {4tier, 3tier}.
#
#          Motivation: at province x 4tier the 2016 baseline had 65% of cells
#          with n<30 and 27 cells with <5 PSUs (script 06 diagnostics). Province
#          is also below the survey's certified inference domain (Region4). This
#          harness quantifies the precision-vs-variation tradeoff so the choice
#          is evidence-based, not assumed.
#
#          NOTHING is written to the main pipeline here — this is a read-only
#          diagnostic that prints a decision table and returns a list.
#
# Reads:   samples$regression_sample  (03_Sample Definitions.R); needs native
#          Region10 + Region4 columns (built in 02_Variable_Construction.R).
# Uses:    near_mw_share, firmsize_pi, weighted_exposure (functions file).
#
#===============================================================================

source("Code/R/clean scripts/00_setup.R")
source("Code/R/clean scripts/03_Sample Definitions.R")

library(dplyr)
library(tidyr)
library(purrr)


#===============================================================================
# STEP 0. Parameters (shared with 06 via config)
#===============================================================================

BASE_YEAR  <- config$exposure$baseline_year
TOL        <- config$exposure$mw_compliance_tolerance
BAND_LOWER <- 1 - TOL
BAND_UPPER <- config$exposure$mw_band_upper
INCOME_VAR <- config$exposure$income_hourly

GEOS  <- c("DES_PROVINCIA", "Region10", "Region4")
TIERS <- c("4tier", "3tier")

tier_spec <- function(scheme) {
  if (scheme == "4tier")
    list(var = "Wage_group",       floor = config$exposure$minwage_hourly_4tier,
         keep = c("Micro","Small","Medium","Large"))
  else
    list(var = "Wage_group_3tier", floor = config$exposure$minwage_hourly_3tier,
         keep = c("Micro","Small","Medium/Large"))
}

# baseline frame (formal, 2016) — geography columns are all native
base_full <- samples$regression_sample$data %>%
  filter(Employment_Status == "Formal", year == BASE_YEAR) %>%
  mutate(baseline_dummy = as.character(BASE_YEAR))

stopifnot(all(c("Region10", "Region4") %in% names(base_full)))


#===============================================================================
# STEP 1. One construction run -> (exposure_geo, diagnostics) for a geo x tier
#===============================================================================

build_one <- function(geo, scheme) {
  ts  <- tier_spec(scheme)
  df  <- base_full %>% filter(.data[[ts$var]] %in% ts$keep)
  
  # cell support
  cells <- df %>%
    group_by(across(all_of(c(geo, ts$var)))) %>%
    summarise(n_obs = dplyr::n(),
              n_psu = dplyr::n_distinct(psu_unique), .groups = "drop")
  
  # exposure primitive + weights (explicit args; never the function defaults)
  near_tbl <- near_mw_share(
    df = df, time_var = "baseline_dummy", by_vars = c(geo, ts$var),
    min_wage = ts$floor, income = INCOME_VAR, out_col = "near_min",
    mw_lower = BAND_LOWER, mw_upper = BAND_UPPER, formal_only = FALSE
  )
  pi_tbl <- firmsize_pi(
    df = df, time_var = "baseline_dummy", by_vars = c(geo, ts$var),
    size_var = ts$var, formal_only = FALSE
  )
  exp_geo <- weighted_exposure(
    near_tbl, pi_tbl, "baseline_dummy", c(geo), ts$var,
    "near_min", "pi", "exposure_geo_val"
  )
  
  list(geo = geo, scheme = scheme, cells = cells, exposure = exp_geo)
}

runs <- cross2(GEOS, TIERS) %>%
  map(~ build_one(.x[[1]], .x[[2]])) %>%
  setNames(map_chr(cross2(GEOS, TIERS), ~ paste(.x[[1]], .x[[2]], sep = "_")))


#===============================================================================
# STEP 2. Decision table: support + variation, one row per geo x tier
#===============================================================================

decision_tbl <- map_dfr(runs, function(r) {
  cells <- r$cells
  exp   <- r$exposure
  tibble(
    geo            = r$geo,
    tier_scheme    = r$scheme,
    n_units        = dplyr::n_distinct(exp[[r$geo]]),
    n_cells        = nrow(cells),
    pct_cell_n_lt30 = round(100 * mean(cells$n_obs < 30), 1),
    pct_cell_psu_lt5 = round(100 * mean(cells$n_psu < 5), 1),
    median_cell_n  = median(cells$n_obs),
    exp_min        = round(min(exp$exposure_geo_val, na.rm = TRUE), 3),
    exp_median     = round(median(exp$exposure_geo_val, na.rm = TRUE), 3),
    exp_max        = round(max(exp$exposure_geo_val, na.rm = TRUE), 3),
    exp_sd         = round(sd(exp$exposure_geo_val, na.rm = TRUE), 3),
    exp_cv         = round(sd(exp$exposure_geo_val, na.rm = TRUE) /
                             mean(exp$exposure_geo_val, na.rm = TRUE), 3),
    n_zero_units   = sum(exp$exposure_geo_val == 0, na.rm = TRUE)
  )
})

cat("\n==================  GEOGRAPHY x TIER DECISION TABLE  ==================\n")
cat("Support (lower thin% is better) vs variation (higher CV = more identifying\n")
cat("variation). n_zero_units > 0 signals thin-cell noise producing exact zeros.\n\n")
print(as.data.frame(decision_tbl), row.names = FALSE)


#===============================================================================
# STEP 2b. Contamination-vs-sparsity test (province level)
#
# Directly tests the hypothesis: is low/zero province exposure driven by the
# 100+ ("Large") tier-misclassification (legal mediums 100-150 judged against
# the higher LARGE floor -> pushed below the band -> near_mw = 0), or by pure
# sample sparsity (tiny provinces with few formal workers of any tier)?
#
# For each province (4-tier scheme, where Large=100+ is identifiable), report:
#   large_share      weighted 100+ employment share
#   large_cell_n     unweighted n in the Large cell
#   large_cell_exp   exposure (near-MW share) within the Large cell
#   prov_exposure    overall province exposure (weighted over tiers)
#   total_n          total formal n in the province
#
# Reading:
#   - If LOW prov_exposure provinces are HIGH large_share with HIGH large_cell_n
#     but near-zero large_cell_exp  -> CONTAMINATION (your mechanism). Fix via
#     tier scheme (3tier collapses at medium floor) / bounding, NOT coarsening.
#   - If LOW prov_exposure provinces are simply LOW total_n across all tiers
#     -> SPARSITY. Fix via coarsening geography (Region10/Region4).
#   - Most likely: BOTH, in different provinces.
#===============================================================================

contam_test <- local({
  ts <- tier_spec("4tier")
  df <- base_full %>% filter(.data[[ts$var]] %in% ts$keep)
  
  # weighted Large share + total formal employment per province
  prov_emp <- df %>%
    group_by(DES_PROVINCIA) %>%
    summarise(emp_total = sum(FACTOR_EXPANSION, na.rm = TRUE),
              emp_large  = sum(FACTOR_EXPANSION[Wage_group == "Large"],
                               na.rm = TRUE),
              total_n    = dplyr::n(),
              large_cell_n = sum(Wage_group == "Large"),
              .groups = "drop") %>%
    mutate(large_share = emp_large / emp_total)
  
  # exposure within the Large cell, by province (the contaminated cell):
  # recompute near-MW share at province x tier and pull the Large-cell value
  near_pt <- near_mw_share(
    df = df, time_var = "baseline_dummy",
    by_vars = c("DES_PROVINCIA", "Wage_group"),
    min_wage = ts$floor, income = INCOME_VAR, out_col = "near_min",
    mw_lower = BAND_LOWER, mw_upper = BAND_UPPER, formal_only = FALSE
  ) %>%
    filter(Wage_group == "Large") %>%
    select(DES_PROVINCIA, large_cell_exp = near_min)
  
  prov_exp <- runs[["DES_PROVINCIA_4tier"]]$exposure %>%
    select(DES_PROVINCIA, prov_exposure = exposure_geo_val)
  
  prov_emp %>%
    left_join(near_pt,  by = "DES_PROVINCIA") %>%
    left_join(prov_exp, by = "DES_PROVINCIA") %>%
    transmute(
      DES_PROVINCIA,
      total_n,
      large_share    = round(large_share, 3),
      large_cell_n,
      large_cell_exp = round(large_cell_exp, 3),
      prov_exposure  = round(prov_exposure, 3)
    ) %>%
    arrange(prov_exposure)   # lowest-exposure provinces first
})

cat("\n=========  CONTAMINATION vs SPARSITY (province, 4tier)  ===========\n")
cat("Sorted by province exposure (lowest first). Ask of the low-exposure rows:\n")
cat("  high large_share + high large_cell_n + ~0 large_cell_exp = CONTAMINATION\n")
cat("  low total_n across the board                              = SPARSITY\n\n")
print(as.data.frame(contam_test), row.names = FALSE)

# Quick correlations to summarise which story dominates
cat("\n  Correlations across provinces:\n")
cat(sprintf("    cor(prov_exposure, large_share) = %.3f  (negative => contamination)\n",
            cor(contam_test$prov_exposure, contam_test$large_share,
                use = "complete.obs")))
cat(sprintf("    cor(prov_exposure, total_n)     = %.3f  (positive => sparsity-linked)\n",
            cor(contam_test$prov_exposure, contam_test$total_n,
                use = "complete.obs")))
saveRDS(contam_test, file.path(config$paths$processed_data,
                               "exposure_diag_contam_vs_sparsity.rds"))


#===============================================================================
# STEP 3. Cross-level agreement: does province exposure, aggregated up to
#         Region10/Region4, agree with exposure computed DIRECTLY at that level?
#
#         If province-built and directly-built coarse exposure agree, the finer
#         construction is not introducing systematic distortion (only noise). If
#         they diverge, the province cells are too thin to trust.
#         Agreement measured per tier scheme, employment-weighted province ->
#         coarse aggregation.
#===============================================================================

cross_level_agreement <- function(scheme) {
  ts <- tier_spec(scheme)
  df <- base_full %>% filter(.data[[ts$var]] %in% ts$keep)
  
  # province exposure + province employment weights (for aggregating province
  # exposure up to a coarser geography). A weighted employment total is just a
  # weighted sum of the expansion factor — no survey design / svyby needed
  # (avoids the svyby-on-logical column-shape trap).
  prov_exp <- runs[[paste0("DES_PROVINCIA_", scheme)]]$exposure
  prov_emp <- df %>%
    group_by(DES_PROVINCIA) %>%
    summarise(emp = sum(FACTOR_EXPANSION, na.rm = TRUE), .groups = "drop")
  
  xwalk <- df %>% distinct(DES_PROVINCIA, Region10, Region4)
  
  agree_at <- function(coarse) {
    # province exposure aggregated up to coarse, weighted by province employment
    up <- prov_exp %>%
      left_join(prov_emp, by = "DES_PROVINCIA") %>%
      left_join(xwalk,    by = "DES_PROVINCIA") %>%
      group_by(across(all_of(coarse))) %>%
      summarise(exp_from_prov =
                  weighted.mean(exposure_geo_val, emp, na.rm = TRUE),
                .groups = "drop")
    # exposure computed directly at coarse level
    direct <- runs[[paste0(coarse, "_", scheme)]]$exposure %>%
      select(all_of(coarse), exp_direct = exposure_geo_val)
    cmp <- up %>% left_join(direct, by = coarse)
    tibble(
      tier_scheme = scheme, coarse = coarse,
      pearson  = round(cor(cmp$exp_from_prov, cmp$exp_direct,
                           use = "complete.obs"), 3),
      spearman = round(cor(cmp$exp_from_prov, cmp$exp_direct,
                           method = "spearman", use = "complete.obs"), 3),
      max_abs_diff = round(max(abs(cmp$exp_from_prov - cmp$exp_direct),
                               na.rm = TRUE), 3)
    )
  }
  bind_rows(agree_at("Region10"), agree_at("Region4"))
}

agreement_tbl <- map_dfr(TIERS, cross_level_agreement)

cat("\n==============  CROSS-LEVEL AGREEMENT (province -> coarse)  ============\n")
cat("province exposure aggregated up (employment-weighted) vs exposure computed\n")
cat("directly at the coarse level. High corr + low max_abs_diff => province is\n")
cat("not systematically distorting; divergence => province cells too thin.\n\n")
print(as.data.frame(agreement_tbl), row.names = FALSE)


#===============================================================================
# STEP 4. Return invisibly for interactive inspection
#===============================================================================

invisible(list(
  runs          = runs,
  decision_tbl  = decision_tbl,
  agreement_tbl = agreement_tbl
))