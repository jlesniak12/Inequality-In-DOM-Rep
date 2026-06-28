#===============================================================================
#
# Script: 04A_Compute_MW_Context_and_Bindingness.R
#
# Purpose: Compute all statistics needed for the minimum wage context and
#          bindingness descriptive figures. Saves results as RDS objects for
#          the companion plot script (04B_Plot_MW_Context_and_Bindingness.R).
#          No ggplot or figure code lives here.
#
# SURVEY CONVENTION:
#   This script uses the `survey` package directly (svydesign objects come
#   from 03_Sample_Definitions.R; estimation via svyby / svymean / svyquantile).
#   All survey calls are funneled through the small set of tidy helpers defined
#   in the HELPERS section below so that every output object shares the same
#   schema: a tibble with year_quarter, (Wage_group), estimate, se, n_obs.
#   Do not call svyby/svymean/svyquantile outside those helpers without good
#   reason — keeping them centralised is what makes the output consistent.
#
# FIGURES THIS SCRIPT FEEDS:
#
#   MW CONTEXT
#     Fig MW-1  Real minimum wage levels by tier over time
#               -> mw_context_levels.rds
#     Fig MW-2  Nominal MW growth decomposed into real gain vs. inflation
#               at each MW event, by tier
#               -> mw_context_growth_decomp.rds
#     Fig MW-3  Employment share by firm size tier over time
#               (among workers with known firm size)
#               -> mw_context_firmsize_shares.rds
#
#   MW BINDINGNESS
#     Fig MW-4  Log Kaitz index by tier over time
#               log(real tier MW / median real salary within tier, formal)
#               -> mw_bind_kaitz.rds
#     Fig MW-5  Non-compliance rate over time (econ-wide + by tier x scope)
#               -> mw_bind_noncompliance_econ.rds
#               -> mw_bind_noncompliance_tier.rds
#     Fig MW-6  Wage distribution relative to MW — microdata extract for
#               kernel density / bunching histogram (formal workers)
#               -> mw_bind_dist_formal.rds
#
# DESIGNS USED (all from 03_Sample_Definitions.R — no ad-hoc svydesign here):
#   samples$employed$design                all employed (firm-size shares)
#   samples$private_employees_inc$design   private employees, +income, hrs>0
#                                          (compliance, Kaitz, distribution)
#   samples$regression_sample$data         microdata extract for distribution
#
# INCOME / COMPLIANCE CONCEPTS:
#   monthly  real_salary_income_wage_primary  vs real_minwage_harmonized
#            (Measure 1 — monthly, no hours adjustment)
#   hourly   real_salary_primary_hourly_base  vs real_minwage_hourly
#            (Measure 2 — hourly rate capped at 44hrs, PRIMARY)
#   The overtime-adjusted measure (Measure 3) is left to a robustness script.
#
# OUTPUTS -> config$paths$processed_data / "MW Context and Bindingness":
#   mw_context_levels.rds
#   mw_context_growth_decomp.rds
#   mw_context_firmsize_shares.rds
#   mw_bind_kaitz.rds
#   mw_bind_noncompliance_econ.rds
#   mw_bind_noncompliance_tier.rds
#   mw_bind_dist_formal.rds, mw_bind_dist_mw_ref.rds, mw_bind_mw_annual_avg.rds
#
# READS:
#   Min_Wage.rds, CPI.rds   (from 01B)
#   samples object          (from 03_Sample_Definitions.R)
#
#===============================================================================

source("Code/R/setup/00_setup.R")
source("Code/R/clean scripts/03_Sample Definitions.R")

library(survey)

cat("=== 04A_Compute_MW_Context_and_Bindingness.R ===\n\n")


#===============================================================================
# SHARED CONSTANTS
#===============================================================================

# Four legal firm-size tiers. The survey "100+" bin straddles the legal
# medium (51-150) / large (150+) boundary, so large-tier non-compliance is
# biased somewhat upward by 100-150 medium-firm workers measured against the
# higher large floor. This is documented as a caveat in the figure captions
# rather than corrected by collapsing tiers — collapsing would (a) discard the
# clean Medium tier and (b) bake the 2021Q3 medium-floor spike into a pooled
# series. A 3-tier grouping (Wage_group_3tier) exists in the data for optional
# robustness checks but is not used in these descriptive figures.
TIER_LEVELS <- c("Micro", "Small", "Medium", "Large")
MW_EVENT_QTR    <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")
STANDARD_WEEK   <- 44
WEEKS_PER_MONTH <- 52 / 12

# Cells with fewer than this many unweighted obs are flagged sparse = TRUE
# (the estimate is still computed; the plot script decides whether to show it).
MIN_CELL_N <- 30

# Output subfolder
out_dir <- file.path(config$paths$processed_data, "MW Context and Bindingness")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

save_rds <- function(obj, name) {
  path <- file.path(out_dir, paste0(name, ".rds"))
  saveRDS(obj, path)
  cat("  Saved:", path, "\n")
}


#===============================================================================
# HELPERS — tidy wrappers around the survey package
#
# These exist so that every survey estimate in this script comes back with the
# SAME column schema, regardless of whether it's a proportion or a quantile,
# grouped or ungrouped. The plot script can then treat every object uniformly.
#
# OUTPUT SCHEMA (all helpers):
#   year_quarter   chr   the time cell (renamed from whatever time_var was)
#   <group_var>    chr   present only when group_var is supplied
#   estimate       dbl   the survey-weighted statistic
#   se             dbl   standard error (NA where survey cannot compute it)
#   n_obs          int   UNWEIGHTED count of observations in the cell
#   sparse         lgl   n_obs < MIN_CELL_N
#
# Why n_obs is computed separately from the survey estimate:
#   Unweighted N is not a survey-weighted quantity — it's just a row count of
#   the design's data. We pull it from design$variables directly rather than
#   asking svyby for it.
#===============================================================================

# Unweighted observation counts per cell, pulled straight from the design data.
.n_by <- function(design, time_var, group_var = NULL) {
  grp <- c(time_var, group_var)
  design$variables %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(n_obs = dplyr::n(), .groups = "drop") %>%
    # group_var may come back as factor; coerce join keys to character so the
    # later left_join matches the svyby output (which yields character keys).
    dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
}

# Standardise the svyby SE column to a clean "se", whatever survey named it.
# svyby labels the SE differently across FUNs and versions: "se", "se.<var>",
# "<var>.se", or (for svyquantile) sometimes only ci_l/ci_u. This finds the SE
# column among the non-key, non-estimate columns and renames it to "se";
# if none is found, it adds se = NA so the output schema stays stable.
.standardise_se <- function(df, keys) {
  known    <- c(keys, "estimate")
  leftover <- setdiff(names(df), known)
  
  # Prefer an exact "se", then any column containing "se" (e.g. "se.inc"),
  # excluding CI bound columns.
  se_col <- NULL
  if ("se" %in% leftover) {
    se_col <- "se"
  } else {
    cand <- leftover[grepl("(^|\\.)se(\\.|$)", leftover, ignore.case = TRUE) |
                       grepl("se$", leftover, ignore.case = TRUE)]
    cand <- setdiff(cand, c("ci_l", "ci_u", "ci.l", "ci.u"))
    if (length(cand)) se_col <- cand[[1]]
  }
  
  if (is.null(se_col)) {
    df$se <- NA_real_
  } else {
    names(df)[names(df) == se_col] <- "se"
  }
  # Keep only keys, estimate, se (drop CI bounds and any other svyby extras)
  df[, c(keys, "estimate", "se"), drop = FALSE]
}

# Survey-weighted MEAN of a numeric/binary variable by time (+ optional group).
# For a 0/1 indicator this is a proportion. Returns estimate + se + n_obs.
svy_mean_by <- function(design, var, time_var, group_var = NULL,
                        na_rm = TRUE) {
  
  # Drop rows where the target variable is missing so the cell n and the
  # estimate denominator agree.
  if (isTRUE(na_rm)) {
    keep   <- !is.na(design$variables[[var]])
    design <- design[keep, ]
  }
  
  grp     <- c(time_var, group_var)
  by_fml  <- stats::as.formula(paste0("~", paste(grp, collapse = "+")))
  var_fml <- stats::as.formula(paste0("~", var))
  
  est <- svyby(var_fml, by_fml, design, svymean,
               na.rm = na_rm, vartype = "se", keep.names = FALSE) %>%
    tibble::as_tibble()
  
  # Rename the estimate column (named after the variable), coerce keys to char,
  # then standardise whatever SE column svyby produced to a clean "se".
  est <- est %>%
    dplyr::rename(estimate = !!var) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
  est <- .standardise_se(est, keys = grp)
  
  n_df <- .n_by(design, time_var, group_var)
  
  est %>%
    dplyr::left_join(n_df, by = grp) %>%
    dplyr::rename(year_quarter = !!time_var) %>%
    dplyr::mutate(sparse = n_obs < MIN_CELL_N)
}

# Survey-weighted single QUANTILE (e.g. median) by time (+ optional group).
# Uses svyby(svyquantile). Returns estimate + se + n_obs.
# NOTE: svyquantile on very thin cells can fail and svyby errors if ANY cell
# fails. The medians here use large cells; if you ever hit a failure, switch to
# a per-cell loop with try() (see SurveyTools::compute_quantiles for the pattern).
svy_quantile_by <- function(design, var, time_var, group_var = NULL,
                            prob = 0.5, na_rm = TRUE) {
  
  if (isTRUE(na_rm)) {
    keep   <- !is.na(design$variables[[var]])
    design <- design[keep, ]
  }
  
  grp     <- c(time_var, group_var)
  by_fml  <- stats::as.formula(paste0("~", paste(grp, collapse = "+")))
  var_fml <- stats::as.formula(paste0("~", var))
  
  est <- svyby(var_fml, by_fml, design,
               FUN = svyquantile, quantiles = prob, ci = TRUE,
               vartype = "se", keep.names = FALSE, na.rm = na_rm) %>%
    tibble::as_tibble()
  
  # svyby(svyquantile) may name the estimate column `var` or `var.0.5`
  # (quantile appended) depending on survey version. Find it robustly:
  # it's the non-key column that is NOT an SE/CI column.
  non_key  <- setdiff(names(est), grp)
  se_ci    <- non_key[grepl("se|ci", non_key, ignore.case = TRUE)]
  est_col  <- setdiff(non_key, se_ci)
  if (length(est_col) != 1) {
    # Fallback: prefer an exact match to var, else the first non-SE/CI column
    est_col <- if (var %in% non_key) var else setdiff(non_key, se_ci)[[1]]
  }
  names(est)[names(est) == est_col] <- "estimate"
  
  est <- est %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
  est <- .standardise_se(est, keys = grp)
  
  n_df <- .n_by(design, time_var, group_var)
  
  est %>%
    dplyr::left_join(n_df, by = grp) %>%
    dplyr::rename(year_quarter = !!time_var) %>%
    dplyr::mutate(sparse = n_obs < MIN_CELL_N)
}


#===============================================================================
# STEP 0. Load minimum wage and CPI reference data
#===============================================================================

cat("[0] Loading MW and CPI reference data...\n")

# FIXED — gets real_minwage_harmonized from 02's output (2025Q2 base, correct)
encft <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))

CPI <- readRDS(file.path(config$paths$processed_data, "CPI.rds")) %>%
  dplyr::mutate(year_quarter = paste0(year, "Q", quarter))

cat("  Min wage rows:", nrow(min_wage),
    "| Quarters:", dplyr::n_distinct(min_wage$year_quarter), "\n")
cat("  CPI rows:", nrow(CPI), "\n\n")


#===============================================================================
# STEP 1. MW-1: Real minimum wage levels by tier (no survey needed)
#===============================================================================

cat("[1] MW levels by tier...\n")

mw_context_levels <- encft %>%
  dplyr::filter(Wage_group %in% TIER_LEVELS) %>%
  dplyr::distinct(year, quarter, year_quarter, Wage_group,
                  nom_minwage_harmonized, real_minwage_harmonized) %>%
  dplyr::mutate(Wage_group = factor(Wage_group, levels = TIER_LEVELS))

save_rds(mw_context_levels, "mw_context_levels")
cat("  Quarters:", dplyr::n_distinct(mw_context_levels$year_quarter), "\n\n")


#===============================================================================
# STEP 2. MW-2: MW growth decomposition at each event (no survey needed)
#
# At each event quarter, decompose the quarter-on-quarter nominal MW change
# into a real purchasing-power gain plus an inflation component, by tier.
# By construction: nominal_pct = real_pct + inflation_component.
#===============================================================================

cat("[2] MW growth decomposition at events...\n")

# For the decomposition we use mw_context_levels (already 3-tier) and compute
# changes relative to the PREVIOUS EVENT QUARTER, not quarter-on-quarter.
# This captures the full policy cycle: how much did the MW change since the
# last announcement, and how much of that was eaten by cumulative inflation?
#
# "Previous event quarter" for each event. Panel 1 (2017Q2) uses 2015Q2 as its
# base because 2015Q2 was itself a MW increase (+14% nominal across all tiers) —
# so measuring 2017Q2 against 2015Q2 captures one clean policy cycle rather than
# reaching back to an arbitrary 2014Q3 baseline. The later panels each use the
# immediately preceding MW event as their base.
#   2017Q2 → base 2015Q2
#   2019Q3 → base 2017Q2
#   2021Q3 → base 2019Q3
#   2023Q2 → base 2021Q3
MW_EVENT_PREV <- c("2015Q2", "2017Q2", "2019Q3", "2021Q3")  # base quarters

# Extract MW at each event and its base quarter for each tier
event_df <- mw_context_levels %>%
  dplyr::filter(year_quarter %in% c(MW_EVENT_QTR, MW_EVENT_PREV))

event_levels <- purrr::map2_dfr(MW_EVENT_QTR, MW_EVENT_PREV, function(evt, base) {
  cur  <- event_df %>% dplyr::filter(year_quarter == evt)
  prev <- event_df %>% dplyr::filter(year_quarter == base) %>%
    dplyr::rename(nom_prev = nom_minwage_harmonized,
                  real_prev = real_minwage_harmonized) %>%
    dplyr::select(Wage_group, nom_prev, real_prev)
  cur %>%
    dplyr::left_join(prev, by = "Wage_group") %>%
    dplyr::mutate(
      base_quarter = base,
      nom_pct_chg  = (nom_minwage_harmonized  / nom_prev  - 1) * 100,
      real_pct_chg = (real_minwage_harmonized / real_prev - 1) * 100,
      inflation_component = nom_pct_chg - real_pct_chg
    )
})

mw_context_growth_decomp <- event_levels %>%
  dplyr::select(year_quarter, base_quarter, Wage_group,
                nom_pct_chg, real_pct_chg, inflation_component,
                nom_minwage_harmonized, real_minwage_harmonized) %>%
  dplyr::mutate(
    Wage_group   = factor(Wage_group, levels = TIER_LEVELS),
    year_quarter = factor(year_quarter, levels = MW_EVENT_QTR)
  )

save_rds(mw_context_growth_decomp, "mw_context_growth_decomp")
cat("  Rows:", nrow(mw_context_growth_decomp),
    "(expect 16 = 4 events x 4 tiers)\n\n")


#===============================================================================
# STEP 3. MW-3: Employment share by firm size tier over time
#
# Denominator: employed workers (samples$employed) with a KNOWN firm size tier.
# Conditional share of each tier sums to ~1 within each quarter.
#===============================================================================

cat("[3] Employment share by firm size tier over time...\n")

design_employed <- update(
  samples$employed$design,
  has_known_size = as.integer(Wage_group %in% TIER_LEVELS)
)

# (a) Share of employed with known firm size — data-quality series
known_size_share <- svy_mean_by(
  design   = design_employed,
  var      = "has_known_size",
  time_var = "year_quarter"
) %>%
  dplyr::select(year_quarter, known_size_share = estimate)

cat(sprintf("  Known firm size share — min: %.1f%%, max: %.1f%%\n",
            min(known_size_share$known_size_share) * 100,
            max(known_size_share$known_size_share) * 100))

# (b) Conditional tier composition among workers with known firm size.
design_known_fs <- update(
  subset(design_employed, has_known_size == 1),
  is_micro  = as.integer(Wage_group == "Micro"),
  is_small  = as.integer(Wage_group == "Small"),
  is_medium = as.integer(Wage_group == "Medium"),
  is_large  = as.integer(Wage_group == "Large")
)

tier_ind <- c(Micro = "is_micro", Small = "is_small",
              Medium = "is_medium", Large = "is_large")

tier_shares <- purrr::imap_dfr(tier_ind, function(ind_col, tier_name) {
  svy_mean_by(design_known_fs, var = ind_col, time_var = "year_quarter") %>%
    dplyr::transmute(
      year_quarter,
      Wage_group     = tier_name,
      share_of_known = estimate,
      se
    )
})

# Tier numerator counts (for sparsity flag) from the subset design data
tier_n <- design_known_fs$variables %>%
  dplyr::count(year_quarter, Wage_group, name = "n_obs") %>%
  dplyr::mutate(
    year_quarter = as.character(year_quarter),
    Wage_group   = as.character(Wage_group)
  )

mw_context_firmsize_shares <- tier_shares %>%
  dplyr::left_join(tier_n, by = c("year_quarter", "Wage_group")) %>%
  dplyr::left_join(known_size_share, by = "year_quarter") %>%
  dplyr::mutate(
    Wage_group = factor(Wage_group, levels = TIER_LEVELS),
    sparse     = dplyr::coalesce(n_obs, 0L) < MIN_CELL_N
  )

# Sanity: conditional shares sum to ~1 within each quarter
share_sums <- mw_context_firmsize_shares %>%
  dplyr::group_by(year_quarter) %>%
  dplyr::summarise(total = sum(share_of_known), .groups = "drop")
cat(sprintf("  Conditional share sum — min: %.4f, max: %.4f (should be ~1)\n",
            min(share_sums$total), max(share_sums$total)))

save_rds(mw_context_firmsize_shares, "mw_context_firmsize_shares")
cat("  Quarters:", dplyr::n_distinct(mw_context_firmsize_shares$year_quarter),
    "| Rows:", nrow(mw_context_firmsize_shares), "\n\n")


#===============================================================================
# STEP 4. MW-4: Log Kaitz index by tier
#
# Kaitz_t,tier = log(real tier MW_t) - log(p50 real formal salary_t,tier)
# Denominator: survey-weighted median real compliance salary among FORMAL
# private employees within tier x quarter.
#===============================================================================

cat("[4] Kaitz index — formal sector median by tier...\n")

design_formal_tier <- subset(
  samples$private_employees_inc$design,
  Employment_Status == "Formal" & Wage_group %in% TIER_LEVELS
)

formal_median_tier <- svy_quantile_by(
  design    = design_formal_tier,
  var       = "real_salary_income_wage_primary",
  time_var  = "year_quarter",
  group_var = "Wage_group",
  prob      = 0.50
) %>%
  dplyr::select(year_quarter, Wage_group,
                p50_formal = estimate, p50_se = se, n_obs, sparse)

cat("  Tier x quarter cells:", nrow(formal_median_tier),
    "| Quarters:", dplyr::n_distinct(formal_median_tier$year_quarter), "\n")

# mw_context_levels has Wage_group as factor; coerce to character for the join
mw_bind_kaitz <- mw_context_levels %>%
  dplyr::mutate(Wage_group = as.character(Wage_group)) %>%
  dplyr::inner_join(formal_median_tier,
                    by = c("year_quarter", "Wage_group")) %>%
  dplyr::mutate(
    log_kaitz  = log(real_minwage_harmonized) - log(p50_formal),
    Wage_group = factor(Wage_group, levels = TIER_LEVELS)
  ) %>%
  dplyr::select(year, quarter, year_quarter, Wage_group,
                real_minwage_harmonized, p50_formal, p50_se,
                log_kaitz, n_obs, sparse)

cat(sprintf("  Log Kaitz range: [%.3f, %.3f]\n",
            min(mw_bind_kaitz$log_kaitz, na.rm = TRUE),
            max(mw_bind_kaitz$log_kaitz, na.rm = TRUE)))

save_rds(mw_bind_kaitz, "mw_bind_kaitz")
cat("\n")


#===============================================================================
# STEP 5. MW-5: Non-compliance rates over time
#
# Binary indicators from 02_Variable_Construction.R:
#   below_min_monthly_salary       (Measure 1)
#   below_min_hourly_base_salary   (Measure 2, PRIMARY)
#
# Two scopes: formal (legally bound) and all_private (spillover robustness).
# Two aggregations: economy-wide and by tier.
# Base design: samples$private_employees_inc (Govt already excluded upstream).
#===============================================================================

cat("[5] Non-compliance rates...\n")

COMPLIANCE_VARS <- tibble::tribble(
  ~concept,   ~col,                           ~label,
  "monthly",  "below_min_monthly_salary",     "Monthly (Measure 1)",
  "hourly",   "below_min_hourly_base_salary", "Hourly (Measure 2 \u2014 Primary)"
)

base_inc <- samples$private_employees_inc$design

# ── 5a. Economy-wide ──────────────────────────────────────────────────────────
# Three scopes:
#   formal           formal private employees, all tiers (headline)
#   formal_ex_large  formal private employees EXCLUDING Large — robustness check
#                    against the "100+" survey bin contaminating the Large tier
#                    (100-150 medium firms measured against the higher large floor).
#                    If the trend survives dropping Large, the headline isn't an
#                    artifact of the binning issue.
#   all_private      all private employees regardless of formality (spillover)
cat("  Economy-wide...\n")

mw_bind_noncompliance_econ <- purrr::map_dfr(
  c("formal", "formal_ex_large", "all_private"),
  function(scope) {
    d_scope <- switch(scope,
                      "formal"          = subset(base_inc, Employment_Status == "Formal"),
                      "formal_ex_large" = subset(base_inc, Employment_Status == "Formal" &
                                                   Wage_group != "Large"),
                      "all_private"     = base_inc
    )
    purrr::map_dfr(seq_len(nrow(COMPLIANCE_VARS)), function(i) {
      cv <- COMPLIANCE_VARS[i, ]
      svy_mean_by(d_scope, var = cv$col, time_var = "year_quarter") %>%
        dplyr::rename(nc_rate = estimate) %>%
        dplyr::mutate(concept = cv$concept, concept_label = cv$label,
                      scope = scope)
    })
  }
)

cat(sprintf("  Econ rows: %d | Quarters: %d\n",
            nrow(mw_bind_noncompliance_econ),
            dplyr::n_distinct(mw_bind_noncompliance_econ$year_quarter)))
save_rds(mw_bind_noncompliance_econ, "mw_bind_noncompliance_econ")

# ── 5b. By tier ───────────────────────────────────────────────────────────────
cat("  By tier...\n")

mw_bind_noncompliance_tier <- purrr::map_dfr(
  c("formal", "all_private"),
  function(scope) {
    d_scope <- if (scope == "formal") {
      subset(base_inc, Employment_Status == "Formal" &
               Wage_group %in% TIER_LEVELS)
    } else {
      subset(base_inc, Wage_group %in% TIER_LEVELS)
    }
    purrr::map_dfr(seq_len(nrow(COMPLIANCE_VARS)), function(i) {
      cv <- COMPLIANCE_VARS[i, ]
      svy_mean_by(d_scope, var = cv$col,
                  time_var = "year_quarter", group_var = "Wage_group") %>%
        dplyr::rename(nc_rate = estimate) %>%
        dplyr::mutate(concept = cv$concept, concept_label = cv$label,
                      scope = scope)
    })
  }
) %>%
  dplyr::mutate(Wage_group = factor(Wage_group, levels = TIER_LEVELS))

cat(sprintf("  Tier rows: %d\n", nrow(mw_bind_noncompliance_tier)))
save_rds(mw_bind_noncompliance_tier, "mw_bind_noncompliance_tier")
cat("\n")


#===============================================================================
# STEP 6. MW-6: Wage distribution microdata extract (no survey estimation)
#
# Individual-level extract for KDE / bunching histograms at plot time.
# Focal years 2016 and 2024 (full years). Formal private employees, known tier,
# Govt & Electricity excluded (regression_sample). log2_ratio normalises income
# to the tier's annual-average real MW so x=0 is the floor, x=1 is double it.
#===============================================================================

cat("[6] Wage distribution microdata extract...\n")

DIST_FOCAL_YEARS <- c(2016L, 2024L)

mw_annual_avg <- mw_context_levels %>%
  dplyr::filter(year %in% DIST_FOCAL_YEARS) %>%
  dplyr::group_by(year, Wage_group) %>%
  dplyr::summarise(real_mw_annual = mean(real_minwage_harmonized, na.rm = TRUE),
                   .groups = "drop")

dist_data_raw <- samples$regression_sample$data %>%
  dplyr::filter(year %in% DIST_FOCAL_YEARS) %>%
  dplyr::select(year, quarter, year_quarter, Wage_group, Employment_Status,
                real_salary_income_wage_primary,
                real_salary_primary_hourly_base,
                hours_worked_primary,
                FACTOR_EXPANSION, psu_unique, strata_unique) %>%
  dplyr::filter(Wage_group %in% TIER_LEVELS)

cat(sprintf("  Raw rows (regression sample, focal years): %d\n", nrow(dist_data_raw)))

mw_bind_dist_formal <- dist_data_raw %>%
  dplyr::filter(Employment_Status == "Formal") %>%
  dplyr::left_join(mw_annual_avg, by = c("year", "Wage_group")) %>%
  dplyr::mutate(
    log2_ratio_monthly = log2(real_salary_income_wage_primary / real_mw_annual),
    log2_ratio_hourly  = log2(real_salary_primary_hourly_base /
                                (real_mw_annual / (WEEKS_PER_MONTH * STANDARD_WEEK))),
    focal_year = factor(year, levels = DIST_FOCAL_YEARS)
  ) %>%
  dplyr::group_by(year, Wage_group) %>%
  dplyr::mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(is.finite(log2_ratio_monthly), is.finite(log2_ratio_hourly))

cell_counts <- mw_bind_dist_formal %>%
  dplyr::count(year, Wage_group, name = "n_obs") %>%
  dplyr::mutate(sparse = n_obs < MIN_CELL_N)

mw_bind_dist_formal <- mw_bind_dist_formal %>%
  dplyr::left_join(cell_counts, by = c("year", "Wage_group")) %>%
  dplyr::mutate(Wage_group = factor(Wage_group, levels = TIER_LEVELS))

cat(sprintf("  Formal rows retained: %d\n", nrow(mw_bind_dist_formal)))
cat("  Cell counts (year x tier):\n"); print(cell_counts)

mw_bind_dist_mw_ref <- mw_annual_avg %>%
  dplyr::mutate(
    mw_hourly  = real_mw_annual / (WEEKS_PER_MONTH * STANDARD_WEEK),
    Wage_group = factor(Wage_group, levels = TIER_LEVELS)
  )

save_rds(mw_bind_dist_formal, "mw_bind_dist_formal")
save_rds(mw_bind_dist_mw_ref, "mw_bind_dist_mw_ref")
save_rds(mw_annual_avg,       "mw_bind_mw_annual_avg")
cat("\n")


#===============================================================================
# STEP 7. Validation checks
#===============================================================================

cat("[7] Validation checks...\n\n")

cat("-- mw_context_growth_decomp --\n")
decomp_check <- mw_context_growth_decomp %>%
  dplyr::mutate(sum_check = round(real_pct_chg + inflation_component - nom_pct_chg, 6))
if (any(decomp_check$sum_check != 0, na.rm = TRUE)) {
  warning("  Decomposition does not sum to nominal for some rows.")
} else {
  cat("  Decomposition sums correctly (real + inflation = nominal).\n")
}

cat("\n-- mw_bind_kaitz --\n")
cat(sprintf("  Rows: %d | Sparse: %d | Log Kaitz range: [%.3f, %.3f]\n",
            nrow(mw_bind_kaitz), sum(mw_bind_kaitz$sparse, na.rm = TRUE),
            min(mw_bind_kaitz$log_kaitz, na.rm = TRUE),
            max(mw_bind_kaitz$log_kaitz, na.rm = TRUE)))

cat("\n-- mw_bind_noncompliance_tier (formal, hourly) --\n")
mw_bind_noncompliance_tier %>%
  dplyr::filter(scope == "formal", concept == "hourly") %>%
  dplyr::group_by(Wage_group) %>%
  dplyr::summarise(min = min(nc_rate) * 100, max = max(nc_rate) * 100,
                   .groups = "drop") %>%
  print()

cat("\n-- mw_bind_dist_formal --\n")
cat(sprintf("  Rows: %d | NA monthly ratio: %d | NA hourly ratio: %d\n",
            nrow(mw_bind_dist_formal),
            sum(is.na(mw_bind_dist_formal$log2_ratio_monthly)),
            sum(is.na(mw_bind_dist_formal$log2_ratio_hourly))))

cat("\n=== 04A_Compute_MW_Context_and_Bindingness.R complete ===\n")
cat("All outputs saved to:", out_dir, "\n\n")