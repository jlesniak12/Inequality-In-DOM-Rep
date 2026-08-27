#===============================================================================
#
# Script: 07A_Construction of mw Exposure.R
#
# Purpose: Construct the fixed baseline minimum-wage EXPOSURE measure that
#          serves as the continuous treatment-intensity variable for the
#          Parente-style event study, adapted to the Dominican Republic.
#
#          Exposure for a geographic unit g = the (survey-weighted) share of
#          FORMAL private employees whose wage sits within a band of the
#          firm-size-tier minimum wage, optionally aggregated over firm-size
#          tiers using each tier's baseline employment share within g as
#          weights.
#
# Parameterized by TWO axes (config-driven):
#   config$active_baseline -- period/tier/floor specification (config$baselines)
#       base2016_all_tiers  : 4-quarter pooled, all tiers, employment-weighted
#                             aggregation, contemporaneous (worker's own) floor
#       base2021q2_micro    : 1-quarter (2021Q2), Micro tier only, no
#                             aggregation, floor overridden to incoming 2021Q3
#                             Micro value
#   config$active_income   -- income concept (config$income_specs)
#       hourly_base : real_salary_primary_hourly_base  vs real_minwage_hourly
#       monthly     : real_salary_income_wage_primary  vs real_minwage_harmonized
#
# Output tag composition: paste0(IS$tag, BL$tag). Headline (hourly + 2016)
# produces empty-tag filenames, preserving existing downstream references.
#
# Reads:
#   - samples$reg_tier$data   (from 03_sample_definitions.R)
#     Restricted here to Employment_Status == "Formal" and the baseline period.
#     Used at two places:
#       (1) baseline_df for exposure calculation
#       (2) incoming-floor lookup for base2021q2_micro (needs the whole sample,
#           not the baseline slice — the incoming quarter is outside baseline)
#
# Writes (to processed_data/Regression, with suffix = paste0(IS$tag, BL$tag)):
#   - exposure_cells<suffix>.rds   geo x tier disaggregated (+ terciles)
#   - exposure_geo<suffix>.rds     geo-level exposure (+ terciles)
#   - exposure_diag<suffix>.rds    cell counts / thin cells
#   - exposure_summary<suffix>.rds variation + sanity checks
#
# Conventions:
#   - Sources setup + sample defs; uses near_mw_share / firmsize_pi /
#     weighted_exposure from functions_weighted_exposure.R.
#   - All function args passed EXPLICITLY (income/band/floor). Function
#     defaults are NOT the analysis choices.
#
#===============================================================================

if (!exists("config", envir = .GlobalEnv, inherits = FALSE)) {
  source(here::here("Code","R","clean scripts","00_setup.R"))
} else {
  cat("[07] Reusing existing `config` (00_setup not re-sourced)\n")
}

# 03 builds `samples`, which is expensive. Skip re-sourcing if already loaded
# (drivers can source 03 once before looping 07A). Same guard pattern as
# 00_setup above: standalone runs still work; loops preserve driver overrides.
if (!exists("samples", envir = .GlobalEnv, inherits = FALSE) ||
    is.null(samples$reg_tier$data)) {
  source(file.path(config$paths$scripts, "03_sample definitions.R"))
} else {
  cat("[07] Reusing existing `samples` (03 not re-sourced)\n")
}

# Lonely-PSU handling. 03 also sets this, but ENCFT's single-quarter design
# means base2021q2 hits single-PSU strata more often; setting defensively here
# guards against source-order refactors breaking variance estimation silently.
options(survey.lonely.psu = "adjust")


#===============================================================================
# STEP 0. Resolve parameters from config
#===============================================================================

cat("[07] Constructing baseline minimum-wage exposure measure\n")

out_dir <- config$data_dirs$exposure

BL <- config$baselines[[config$active_baseline]]
IS <- config$income_specs[[config$active_income]]

GEO          <- config$exposure$construct_geo
TIER_VAR     <- "wage_group"
INCOME_VAR   <- IS$income
FLOOR_VAR    <- IS$minwage          # column name; may be overridden below
TOL          <- config$exposure$mw_compliance_tolerance
BAND_LOWER   <- 1 - TOL
BAND_UPPER   <- config$exposure$mw_band_upper

# mw_file() lives in 00_setup.R. Convention:
#   <item>__<income_tag>__<baseline_tag>__<geo>.rds
# with '__' between concepts and '_' within multi-word tag names.

cat(sprintf("  baseline=%s | income=%s | geo=%s\n",
            config$active_baseline, config$active_income, GEO))
cat(sprintf("  income=%s | floor=%s | band=[%.3f, %.2f]\n",
            INCOME_VAR, FLOOR_VAR, BAND_LOWER, BAND_UPPER))
cat(sprintf("  filename stem: exposure_*__%s__%s__%s.rds\n",
            IS$tag, BL$tag, GEO))

stopifnot(GEO %in% c("DES_PROVINCIA", "Region10", "Region4"))

# Helper: split a numeric exposure vector into Low/Medium/High terciles.
exposure_tercile <- function(x) {
  factor(
    c("Low exposure", "Medium exposure", "High exposure")[dplyr::ntile(x, 3)],
    levels = c("Low exposure", "Medium exposure", "High exposure")
  )
}


#===============================================================================
# STEP 1. Baseline frame: formal private employees, baseline period
#
# Baseline-aware period filter. Tier keep-list handles the "all" sentinel.
#===============================================================================

tiers_keep <- if (identical(BL$tiers, "all")) config$TIER_LEVELS else BL$tiers

period_expr <- if (BL$period$type == "year") {
  rlang::expr(year == !!BL$period$value)
} else {
  rlang::expr(year_quarter == !!BL$period$value)
}

baseline_df <- samples$reg_tier$data %>%
  filter(Employment_Status == "Formal",
         !!period_expr,
         wage_group %in% tiers_keep) %>%
  mutate(baseline_dummy = paste0(config$active_baseline, IS$tag))

if (!"Region10" %in% names(baseline_df)) {
  stop("Region10 not found - add it to 02_variable_construction.R and re-run 02.")
}

cat(sprintf("  baseline rows (formal, %s, tiers=%s): %d across %d %s units\n",
            BL$label, paste(tiers_keep, collapse = "/"),
            nrow(baseline_df),
            dplyr::n_distinct(baseline_df[[GEO]]), GEO))


#===============================================================================
# STEP 2. Floor override for "incoming" baselines
#
# base2021q2_micro compares 2021Q2 Micro-worker wages to the INCOMING 2021Q3
# Micro floor, not the (Small) floor those workers legally faced at 2021Q2.
# Lookup pulls the harmonized real floor from the full sample at the incoming
# quarter and pins it as a constant column on baseline_df.
#
# The lookup uses IS$minwage (the same column that `worker` mode would use),
# so it automatically respects the active income concept (hourly vs monthly).
#===============================================================================

if (BL$floor$source == "incoming") {
  target_yq <- BL$floor$qtr
  incoming_val <- samples$reg_tier$data %>%
    filter(year_quarter == target_yq, wage_group == BL$floor$tier) %>%
    pull(!!sym(FLOOR_VAR)) %>%
    first()
  
  if (is.na(incoming_val) || is.null(incoming_val)) {
    stop("Incoming floor lookup returned NA/NULL for ", target_yq, " / ",
         BL$floor$tier, " on column ", FLOOR_VAR,
         ". Check that the (year_quarter, wage_group) cell exists in the sample.")
  }
  
  baseline_df$floor_incoming <- incoming_val
  FLOOR_VAR_USE <- "floor_incoming"
  
  cat(sprintf("  incoming floor: %s / %s = %.4f (%s)\n",
              target_yq, BL$floor$tier, incoming_val, FLOOR_VAR))
} else {
  FLOOR_VAR_USE <- FLOOR_VAR
}


#===============================================================================
# STEP 3. Cell counts & support diagnostics (geo x tier)
#===============================================================================

cell_diag <- baseline_df %>%
  group_by(across(all_of(c(GEO, TIER_VAR)))) %>%
  summarise(
    n_obs = dplyr::n(),
    n_psu = dplyr::n_distinct(psu_unique),
    .groups = "drop"
  ) %>%
  arrange(n_obs)

saveRDS(cell_diag, mw_file("exposure_diag", dir = out_dir))
cat(sprintf("  geo x tier cells: %d | thin (n<30): %d | (n_psu<5): %d\n",
            nrow(cell_diag),
            sum(cell_diag$n_obs  < 30),
            sum(cell_diag$n_psu <  5)))


#===============================================================================
# STEP 4. Exposure primitive: share near MW (geo x tier), and firm-size weights
#
# For single-tier baselines (BL$weight_tiers == FALSE), pi_tbl is a trivial
# 1-per-cell object built without calling firmsize_pi. Skips wasted svy compute
# and lets STEP 5 use one code path for both baselines.
#===============================================================================

cat("[07] Computing near-MW share (geo x tier)...\n")

near_tbl <- near_mw_share(
  df          = baseline_df,
  time_var    = "baseline_dummy",
  by_vars     = c(GEO, TIER_VAR),
  min_wage    = FLOOR_VAR_USE,       # constant column when floor overridden
  income      = INCOME_VAR,
  out_col     = "near_min",
  mw_lower    = BAND_LOWER,
  mw_upper    = BAND_UPPER,
  formal_only = FALSE                # already filtered to Formal in STEP 1
)

if (BL$weight_tiers) {
  pi_tbl <- firmsize_pi(
    df          = baseline_df,
    time_var    = "baseline_dummy",
    by_vars     = c(GEO, TIER_VAR),
    size_var    = TIER_VAR,
    formal_only = FALSE
  )
  
  # Sanity: within-geo weights sum to 1
  pi_check <- pi_tbl %>%
    group_by(across(all_of(c("baseline_dummy", GEO)))) %>%
    summarise(wsum = sum(pi, na.rm = TRUE), .groups = "drop")
  if (any(abs(pi_check$wsum - 1) > 1e-6)) {
    warning("firm-size weights do not sum to 1 within all geo units (max dev ",
            signif(max(abs(pi_check$wsum - 1)), 3), ")")
  }
} else {
  # Single-tier: pi = 1 by construction. Build a matching-shape frame from
  # near_tbl so STEP 5's join is trivial and STEP 6's degenerate aggregation
  # produces the same numbers whether it runs or not.
  pi_tbl <- near_tbl %>%
    transmute(baseline_dummy,
              !!GEO      := .data[[GEO]],
              !!TIER_VAR := .data[[TIER_VAR]],
              pi = 1)
  pi_check <- pi_tbl %>%
    group_by(across(all_of(c("baseline_dummy", GEO)))) %>%
    summarise(wsum = sum(pi, na.rm = TRUE), .groups = "drop")
}


#===============================================================================
# STEP 5. Disaggregated exposure (geo x tier) + terciles
#
# NOTE: within-tier and overall terciles are algebraically identical in the
# single-tier baseline. Both columns retained for downstream (08B) compatibility.
#===============================================================================

exposure_cells <- near_tbl %>%
  left_join(pi_tbl %>% select(all_of(c("baseline_dummy", GEO, TIER_VAR, "pi"))),
            by = c("baseline_dummy", GEO, TIER_VAR)) %>%
  rename(exposure_val = near_min) %>%
  group_by(across(all_of(TIER_VAR))) %>%
  mutate(exposure_group_within_tier = exposure_tercile(exposure_val)) %>%
  ungroup() %>%
  mutate(exposure_group_overall = exposure_tercile(exposure_val))


#===============================================================================
# STEP 6. Aggregated geo-level exposure (weighted over tiers) + terciles
#
# For BL$weight_tiers == FALSE, aggregation collapses to identity (one row per
# geo, exposure_geo_val = near_min). Explicit branch rather than passing
# pi = 1 through weighted_exposure — the latter works but obscures intent.
#===============================================================================

exposure_geo <- if (BL$weight_tiers) {
  weighted_exposure(
    near_tbl     = near_tbl,
    pi_tbl       = pi_tbl,
    time_var     = "baseline_dummy",
    by_vars      = c(GEO),
    weight_dim   = TIER_VAR,
    exposure_col = "near_min",
    pi_col       = "pi",
    out_col      = "exposure_geo_val"
  )
} else {
  near_tbl %>%
    transmute(baseline_dummy,
              !!GEO := .data[[GEO]],
              exposure_geo_val = near_min)
}

exposure_geo <- exposure_geo %>%
  arrange(desc(exposure_geo_val)) %>%
  mutate(exposure_group = exposure_tercile(exposure_geo_val))

# Attach coarser geography labels for downstream clustering options.
if (GEO == "DES_PROVINCIA") {
  xwalk <- baseline_df %>% distinct(DES_PROVINCIA, Region10, Region4)
  exposure_geo <- exposure_geo %>% left_join(xwalk, by = "DES_PROVINCIA")
} else if (GEO == "Region10") {
  xwalk <- baseline_df %>% distinct(Region10, Region4)
  exposure_geo <- exposure_geo %>% left_join(xwalk, by = "Region10")
}


#===============================================================================
# STEP 7. Aggregation sanity check
#
# The direct geo-level near_mw_share should approximately equal the tier-weighted
# aggregate when weight_tiers = TRUE (equal by construction when FALSE, since
# there is only one tier and the weighted average is the value itself).
#===============================================================================

direct_geo <- near_mw_share(
  df          = baseline_df,
  time_var    = "baseline_dummy",
  by_vars     = c(GEO),
  min_wage    = FLOOR_VAR_USE,
  income      = INCOME_VAR,
  out_col     = "direct_exposure",
  mw_lower    = BAND_LOWER,
  mw_upper    = BAND_UPPER,
  formal_only = FALSE
)

agg_check <- exposure_geo %>%
  select(all_of(GEO), exposure_geo_val) %>%
  left_join(direct_geo %>% select(all_of(GEO), direct_exposure), by = GEO) %>%
  mutate(diff = exposure_geo_val - direct_exposure)

max_diff <- max(abs(agg_check$diff), na.rm = TRUE)
cat(sprintf("  agg check: max |weighted - direct| = %.4g\n", max_diff))
# Weighted aggregate can differ from direct pooled estimate under
# non-proportional sampling; big gaps flag a construction bug.
if (max_diff > 0.05) {
  warning("Large gap between weighted-aggregate and direct-pooled exposure ",
          "(max |diff| = ", signif(max_diff, 3), "). Review pi construction.")
}


#===============================================================================
# STEP 8. Variation diagnostics
#===============================================================================

var_summary <- exposure_geo %>%
  summarise(
    n_units = dplyr::n(),
    min     = min(exposure_geo_val,     na.rm = TRUE),
    p25     = quantile(exposure_geo_val, .25, na.rm = TRUE),
    median  = median(exposure_geo_val,  na.rm = TRUE),
    p75     = quantile(exposure_geo_val, .75, na.rm = TRUE),
    max     = max(exposure_geo_val,     na.rm = TRUE),
    sd      = sd(exposure_geo_val,      na.rm = TRUE),
    iqr     = p75 - p25,
    cv      = sd / mean(exposure_geo_val, na.rm = TRUE)
  )
cat("[07] Exposure variation across geo units:\n")
print(var_summary)


#===============================================================================
# STEP 9. Save outputs
#===============================================================================

saveRDS(exposure_cells, mw_file("exposure_cells", dir = out_dir))
saveRDS(exposure_geo,   mw_file("exposure_geo", dir = out_dir))
saveRDS(list(var_summary = var_summary,
             pi_check    = pi_check,
             agg_check   = agg_check),
        mw_file("exposure_summary", dir = out_dir))

cat(sprintf("[07] Done. baseline=%s income=%s\n",
            config$active_baseline, config$active_income))
cat("     Wrote:\n")
for (item in c("exposure_cells", "exposure_geo", "exposure_diag", "exposure_summary")) {
  cat("       ", basename(mw_file(item)), "\n")
}