#===============================================================================
#
#
# Purpose: Compute all survey-weighted statistics for MW bindingness and
#          compliance analysis. 
#
#
# THREE INCOME CONCEPTS:
#   monthly     real_wage_compliance_primary        vs real_minwage_harmonized
#   hourly      real_wage_compliance_primary_hourly vs real_minwage_hourly
#   hourly_eff  real_wage_compliance_primary_hourly_eff vs real_minwage_hourly
#   (hourly concepts require hours_worked_primary > 0)
#
# TWO SCOPES:
#   formal      Employment_Status == "Formal"    (primary)
#   all_private No formality filter               (spillover robustness)
#
# OBJECTS SAVED:
#   mw_bindingness_dist_data.rds        snapshot KDE data (2016Q2 vs 2025Q2)
#   mw_bindingness_kaitz_econ.rds       quarterly Kaitz, economy-wide
#   mw_bindingness_kaitz_sector.rds     quarterly Kaitz, by sector
#   mw_bindingness_compliance_econ.rds  quarterly non-compliance rate, economy-wide
#   mw_bindingness_compliance_tier.rds  quarterly non-compliance, by tier
#   mw_bindingness_compliance_sector.rds quarterly non-compliance, by sector
#   mw_bindingness_mw_ref.rds           quarterly MW levels by tier
#
#===============================================================================

source("Code/R/setup/00_setup.R")
source("Code/R/import and prep data/02C_Sample Definitions.R")

cat("=== 05a_MW_Bindingness_Compute.R ===\n")


#===============================================================================
# STEP 0.  Constants
#===============================================================================

TIER_LEVELS     <- c("Micro", "Small", "Medium", "Large")
MIN_CELL_N      <- 20          # relaxed from 50 — more inclusive for figures
SNAP_QTRS       <- c("2016Q2", "2025Q2")
CNS_FACTOR      <- 23.83
LEGAL_HOURS     <- 8

# Income concept metadata
# needs_hours: TRUE = filter to hours_worked_primary > 0 before computing
CONCEPTS <- tibble::tribble(
  ~concept,      ~worker_col,                                  ~mw_col,                   ~label,                                 ~needs_hours,
  "monthly",     "real_salary_primary",               "real_minwage_harmonized", "Monthly (Measure 1)",                  FALSE,
  "hourly",      "real_salary_primary_hourly",        "real_minwage_hourly",     "Hourly (Measure 2 \u2014 Primary)",    TRUE,
  "hourly_eff",  "real_salary_primary_hourly_eff",    "real_minwage_hourly",     "Hourly OT-adjusted (Measure 3)",       TRUE
)

# Compliance variable names (already computed in 02_Prepare Survey Data Panels.R)
COMPLIANCE_VARS <- tibble::tribble(
  ~concept,      ~col,                    ~label,
  "monthly",     "below_min_monthly_salary",     "Monthly (Measure 1)",
  "hourly",      "below_min_hourly_salary",      "Hourly (Measure 2 \u2014 Primary)",
  "hourly_eff",  "below_min_hourly_eff_salary",  "Hourly OT-adjusted (Measure 3)"
)

SCOPES <- tibble::tribble(
  ~scope,        ~label,                                                              ~status_filter,
  "formal",      "Formal private-sector employees (primary)",                         "Formal",
  "all_private", "All private-sector employees: formal + informal (spillover check)", NA_character_
)

pd <- config$paths$processed_data


#===============================================================================
# STEP 1.  Extract base data frames
#===============================================================================

cat("[1] Extracting samples...\n")

# Base for distribution/Kaitz: private employees with positive monthly income.
# No Wage_group or sector restriction at this stage — applied per-concept below.
base_inc <- samples$private_employees_inc$data %>%
  mutate(Wage_group = factor(Wage_group, levels = c(TIER_LEVELS, "Dont Know"),
                             exclude = NULL))

# Regression sample (Wage_group known, Govt/Elec excl.) for Kaitz
base_reg <- samples$regression_sample$data %>%
  mutate(Wage_group = factor(Wage_group, levels = TIER_LEVELS))

cat("    private_employees_inc rows:", nrow(base_inc), "\n")
cat("    regression_sample rows:    ", nrow(base_reg), "\n")
cat("    Formal in base_inc:        ",
    nrow(dplyr::filter(base_inc, Employment_Status == "Formal")), "\n")


#===============================================================================
# STEP 2.  Quarterly MW reference table
#===============================================================================

cat("[2] Building quarterly MW reference...\n")

min_wage_raw <- readRDS(file.path(pd, "Min_Wage.rds"))

mw_ref <- min_wage_raw %>%
  filter(Wage_group %in% TIER_LEVELS) %>%
  mutate(
    year_quarter        = paste0(year, "Q", quarter),
    Wage_group          = factor(Wage_group, levels = TIER_LEVELS),
    real_minwage_hourly = real_minwage_harmonized / (CNS_FACTOR * LEGAL_HOURS)
  ) %>%
  select(year_quarter, year, quarter, Wage_group,
         real_minwage_harmonized, real_minwage_hourly)

saveRDS(mw_ref, file.path(pd, "mw_bindingness_mw_ref.rds"))
cat("    Saved: mw_bindingness_mw_ref.rds\n")


#===============================================================================
# STEP 3.  Helpers
#===============================================================================

get_scope_df <- function(base, scope_row) {
  sf <- scope_row$status_filter
  if (!is.na(sf)) dplyr::filter(base, Employment_Status == sf) else base
}

apply_concept_filter <- function(df, concept_row) {
  # For hourly measures, require hours_worked_primary > 0
  if (concept_row$needs_hours) {
    df <- dplyr::filter(df,
                        !is.na(hours_worked_primary),
                        hours_worked_primary > 0)
  }
  # Require the specific income column to be positive and non-missing
  df <- dplyr::filter(df,
                      !is.na(.data[[concept_row$worker_col]]),
                      .data[[concept_row$worker_col]] > 0)
  df
}

wt_median <- function(df, income_col, group_vars) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    arrange(.data[[income_col]], .by_group = TRUE) %>%
    mutate(cumw = cumsum(FACTOR_EXPANSION) / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
    summarise(
      p50   = .data[[income_col]][which.min(abs(cumw - 0.5))],
      n_obs = n(),
      .groups = "drop"
    )
}


#===============================================================================
# STEP 4.  Distribution snapshot data (2016Q2 vs 2025Q2)
#
#  Uses base_inc (all private employees with positive monthly income).
#  Per-concept filter applied inside loop.
#  Wage_group filter: keep TIER_LEVELS only (drop "Dont Know" etc.) for
#  distribution figures, but this is done after the concept filter so the
#  broadest possible sample is used.
#===============================================================================

cat("[3] Building distribution snapshot data...\n")

snap_long_list <- vector("list", nrow(SCOPES) * nrow(CONCEPTS))
idx <- 0L

for (s in seq_len(nrow(SCOPES))) {
  scope_row <- SCOPES[s, ]
  scope_base <- get_scope_df(base_inc, scope_row) %>%
    filter(year_quarter %in% SNAP_QTRS,
           Wage_group %in% TIER_LEVELS) %>%
    mutate(
      snapshot   = factor(year_quarter, levels = SNAP_QTRS),
      Wage_group = factor(Wage_group, levels = TIER_LEVELS)
    )
  
  for (c in seq_len(nrow(CONCEPTS))) {
    cpt <- CONCEPTS[c, ]
    idx <- idx + 1L
    
    chunk <- apply_concept_filter(scope_base, cpt) %>%
      mutate(
        scope         = scope_row$scope,
        scope_label   = scope_row$label,
        concept       = cpt$concept,
        concept_label = cpt$label,
        worker_income = .data[[cpt$worker_col]],
        mw_benchmark  = .data[[cpt$mw_col]],
        log2_ratio    = log2(worker_income / mw_benchmark)
      ) %>%
      filter(is.finite(log2_ratio)) %>%
      select(year_quarter, snapshot, year, quarter,
             Wage_group, Employment_Sector, Employment_Status,
             FACTOR_EXPANSION,
             scope, scope_label, concept, concept_label,
             worker_income, mw_benchmark, log2_ratio)
    
    chunk <- chunk %>%
      group_by(snapshot, Wage_group, concept, scope) %>%
      mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(snapshot, Wage_group, concept, scope, Employment_Sector) %>%
      mutate(w_norm_sec = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
      ungroup()
    
    cell_n <- chunk %>%
      count(snapshot, Wage_group, concept, scope, Employment_Sector, name = "n_obs")
    
    snap_long_list[[idx]] <- chunk %>%
      left_join(cell_n, by = c("snapshot", "Wage_group", "concept",
                               "scope", "Employment_Sector")) %>%
      mutate(sparse = n_obs < MIN_CELL_N)
  }
}

snap_long <- dplyr::bind_rows(snap_long_list)
cat("    Snapshot rows:", nrow(snap_long), "\n")

# Print tier coverage — verify Medium is now present
cat("    Tier coverage in snapshot:\n")
snap_long %>%
  filter(scope == "formal", concept == "monthly") %>%
  count(year_quarter, Wage_group, sparse, name = "n_workers") %>%
  print(n = 20)

saveRDS(snap_long, file.path(pd, "mw_bindingness_dist_data.rds"))
cat("    Saved: mw_bindingness_dist_data.rds\n")


#===============================================================================
# STEP 5.  Log Kaitz Index — economy-wide (uses regression_sample)
#===============================================================================

cat("[4] Computing economy-wide log Kaitz...\n")

kaitz_econ_list <- vector("list", nrow(SCOPES) * nrow(CONCEPTS))
idx <- 0L

for (s in seq_len(nrow(SCOPES))) {
  scope_row <- SCOPES[s, ]
  scope_df  <- get_scope_df(base_reg, scope_row)
  
  for (c in seq_len(nrow(CONCEPTS))) {
    cpt <- CONCEPTS[c, ]
    idx <- idx + 1L
    
    cpt_df <- apply_concept_filter(scope_df, cpt)
    med    <- wt_median(cpt_df, cpt$worker_col, c("year_quarter", "Wage_group"))
    
    kaitz_econ_list[[idx]] <- med %>%
      left_join(
        mw_ref %>% select(year_quarter, Wage_group,
                          mw_value = !!rlang::sym(cpt$mw_col)),
        by = c("year_quarter", "Wage_group")
      ) %>%
      filter(!is.na(mw_value), !is.na(p50), p50 > 0) %>%
      mutate(
        scope         = scope_row$scope,
        scope_label   = scope_row$label,
        concept       = cpt$concept,
        concept_label = cpt$label,
        log_kaitz     = log(mw_value) - log(p50),
        sparse        = n_obs < MIN_CELL_N
      )
  }
}

kaitz_econ <- dplyr::bind_rows(kaitz_econ_list)
cat("    Economy-wide Kaitz rows:", nrow(kaitz_econ), "\n")
saveRDS(kaitz_econ, file.path(pd, "mw_bindingness_kaitz_econ.rds"))
cat("    Saved: mw_bindingness_kaitz_econ.rds\n")


#===============================================================================
# STEP 6.  Log Kaitz Index — by sector
#===============================================================================

cat("[5] Computing sector-level log Kaitz...\n")

kaitz_sec_list <- vector("list", nrow(SCOPES) * nrow(CONCEPTS))
idx <- 0L

for (s in seq_len(nrow(SCOPES))) {
  scope_row <- SCOPES[s, ]
  scope_df  <- get_scope_df(base_reg, scope_row)
  
  for (c in seq_len(nrow(CONCEPTS))) {
    cpt <- CONCEPTS[c, ]
    idx <- idx + 1L
    
    cpt_df <- apply_concept_filter(scope_df, cpt)
    med    <- wt_median(cpt_df, cpt$worker_col,
                        c("year_quarter", "Wage_group", "Employment_Sector"))
    
    kaitz_sec_list[[idx]] <- med %>%
      left_join(
        mw_ref %>% select(year_quarter, Wage_group,
                          mw_value = !!rlang::sym(cpt$mw_col)),
        by = c("year_quarter", "Wage_group")
      ) %>%
      filter(!is.na(mw_value), !is.na(p50), p50 > 0) %>%
      mutate(
        scope         = scope_row$scope,
        scope_label   = scope_row$label,
        concept       = cpt$concept,
        concept_label = cpt$label,
        log_kaitz     = log(mw_value) - log(p50),
        sparse        = n_obs < MIN_CELL_N
      )
  }
}

kaitz_sec <- dplyr::bind_rows(kaitz_sec_list)
cat("    Sector Kaitz rows:", nrow(kaitz_sec), "\n")
saveRDS(kaitz_sec, file.path(pd, "mw_bindingness_kaitz_sector.rds"))
cat("    Saved: mw_bindingness_kaitz_sector.rds\n")


#===============================================================================
# STEP 7.  Compliance time series
#
#  Uses base_inc (private_employees_inc), not regression_sample, so:
#    - All sectors shown (incl. Government as context)
#    - Workers with unknown Wage_group are included in economy-wide series
#    - Formal/informal split available
#
#  For the economy-wide and tier-level series, we use base_inc directly.
#  For the sector series, we filter to known Wage_group so the sector × tier
#  breakdown is meaningful.
#
#  Compliance variables (below_min_*) are binary: 1 = non-compliant.
#  We compute survey-weighted mean = non-compliance RATE (share below floor).
#
#  SCOPE NOTE: compliance is most interpretable for formal workers (the MW
#  is a legal obligation for them). We compute both scopes but the formal
#  scope is primary.
#===============================================================================

cat("[6] Computing compliance time series...\n")

compute_compliance_rates <- function(df, group_vars) {
  # For each compliance variable, compute weighted mean (= non-compliance rate)
  results <- purrr::map_dfr(seq_len(nrow(COMPLIANCE_VARS)), function(i) {
    cv <- COMPLIANCE_VARS[i, ]
    df %>%
      filter(!is.na(.data[[cv$col]])) %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        nc_rate = weighted.mean(.data[[cv$col]], FACTOR_EXPANSION, na.rm = TRUE),
        n_obs   = n(),
        .groups = "drop"
      ) %>%
      mutate(
        concept       = cv$concept,
        concept_label = cv$label
      )
  })
  results
}

# ── 7A: Economy-wide — all private employees (Wage_group may be unknown) ──────
cat("    Economy-wide compliance...\n")

comp_econ_list <- vector("list", nrow(SCOPES))
for (s in seq_len(nrow(SCOPES))) {
  scope_row <- SCOPES[s, ]
  scope_df  <- get_scope_df(base_inc, scope_row)
  
  comp_econ_list[[s]] <- compute_compliance_rates(scope_df, "year_quarter") %>%
    mutate(scope = scope_row$scope, scope_label = scope_row$label)
}
comp_econ <- dplyr::bind_rows(comp_econ_list)

saveRDS(comp_econ, file.path(pd, "mw_bindingness_compliance_econ.rds"))
cat("    Saved: mw_bindingness_compliance_econ.rds\n")


# ── 7B: By firm size tier (known Wage_group only) ─────────────────────────────
cat("    Tier-level compliance...\n")

base_known_tier <- base_inc %>%
  filter(Wage_group %in% TIER_LEVELS) %>%
  mutate(Wage_group = factor(Wage_group, levels = TIER_LEVELS))

comp_tier_list <- vector("list", nrow(SCOPES))
for (s in seq_len(nrow(SCOPES))) {
  scope_row <- SCOPES[s, ]
  scope_df  <- get_scope_df(base_known_tier, scope_row)
  
  comp_tier_list[[s]] <- compute_compliance_rates(
    scope_df, c("year_quarter", "Wage_group")
  ) %>%
    mutate(
      sparse      = n_obs < MIN_CELL_N,
      scope       = scope_row$scope,
      scope_label = scope_row$label
    )
}
comp_tier <- dplyr::bind_rows(comp_tier_list)

saveRDS(comp_tier, file.path(pd, "mw_bindingness_compliance_tier.rds"))
cat("    Saved: mw_bindingness_compliance_tier.rds\n")


# ── 7C: By sector (known Wage_group only) ─────────────────────────────────────
cat("    Sector-level compliance...\n")

comp_sec_list <- vector("list", nrow(SCOPES))
for (s in seq_len(nrow(SCOPES))) {
  scope_row <- SCOPES[s, ]
  scope_df  <- get_scope_df(base_known_tier, scope_row)
  
  comp_sec_list[[s]] <- compute_compliance_rates(
    scope_df, c("year_quarter", "Employment_Sector")
  ) %>%
    mutate(
      sparse      = n_obs < MIN_CELL_N,
      scope       = scope_row$scope,
      scope_label = scope_row$label
    )
}
comp_sec <- dplyr::bind_rows(comp_sec_list)

saveRDS(comp_sec, file.path(pd, "mw_bindingness_compliance_sector.rds"))
cat("    Saved: mw_bindingness_compliance_sector.rds\n")


#===============================================================================
# DONE
#===============================================================================

cat("\n=== 05a complete ===\n")
cat("Saved to:", pd, "\n")
cat("  mw_bindingness_dist_data.rds\n")
cat("  mw_bindingness_kaitz_econ.rds\n")
cat("  mw_bindingness_kaitz_sector.rds\n")
cat("  mw_bindingness_compliance_econ.rds\n")
cat("  mw_bindingness_compliance_tier.rds\n")
cat("  mw_bindingness_compliance_sector.rds\n")
cat("  mw_bindingness_mw_ref.rds\n")
cat("Run 05b_MW_Bindingness_Figures.R\n")
