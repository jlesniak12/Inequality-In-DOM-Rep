


## ------------------------------------------------------------------------------
# STEP 1C: Log-Variance of Wages
# Uses the same restrictive design as all other regression outcomes for
# consistency — same sample, same cells, same denominator.
# design_broad is kept for the descriptive decomposition figure only.
# ------------------------------------------------------------------------------

# Add log_wage to the restrictive design (already has group from Step 1A)
design <- update(design, log_wage = log(real_salary_income_primary))

# svyby computes survey-weighted variance of log wages within each time × group cell
var_raw <- svyby(
  ~log_wage,
  ~year_quarter + group,
  design  = design,
  FUN     = svyvar,
  na.rm   = TRUE,
  vartype = NULL          # drop SE columns — not used in regression
) %>%
  as_tibble() %>%
  rename(time = year_quarter,
         var_log_wage = log_wage) %>%
  mutate(log_var_wage = log(var_log_wage))




















# ==============================================================================
# SECTION 5: BUILD SECTOR × TIME DESCRIPTIVE PANEL
#
# PURPOSE: Descriptive visualisation and motivation figures ONLY.
#          NOT used directly in regression — regression sector panels are
#          built in 05_regression.R from outcome-specific validated samples.
#
# CONSTRUCTION: pi-weighted collapse of panel_sf, excluding structural zeros.
#
# Structural zeros are cells where formal private employment essentially does
# not exist in DR as formal private employees after sample restrictions.
# These are excluded to prevent noise from distorting sector-level pi-weighted
# averages. They are identified by median n < 10 across all quarters AND
# the economic category being structurally absent (e.g. no large agriculture
# firms, no medium-tier finance micro-establishments).
#
# Full outcome-specific cell dropping is done in 04_cell_validation.R.
# The regression panels built in 05_regression.R use those validated samples.
#
# NOTE: Construction drops out entirely — all four firm size cells are
# structural zeros, leaving no rows to collapse. This is correct and expected.
# ==============================================================================

structural_zeros <- tribble(
  ~Employment_Sector,       ~Wage_group,
  "Construction",           "Large",
  "Construction",           "Medium",
  "Construction",           "Micro",
  "Construction",           "Small",
  "Education",              "Medium",
  "Finance",                "Medium",
  "Health",                 "Medium",
  "Transportation",         "Medium"
)

pi <- firmsize_pi(
  df          = panel_emp_privsec,
  time_var    = "year_quarter",
  time_subset = NULL,
  by_vars     = c("Employment_Sector", "Wage_group"),
  size_var    = "Wage_group",
  formal_only = TRUE
)



panel_st_descriptive <- panel_sf %>%
  anti_join(structural_zeros,
            by = c("Employment_Sector", "Wage_group")) %>%
  group_by(Employment_Sector, time, time_index) %>%
  summarise(
    # --- Relative employment weight (sum of remaining pi weights) ---
    # Will be < 1.0 for sectors where some firm size tiers excluded.
    # Outcomes below use sum(x*pi)/sum(pi) which renormalises automatically.
    sector_pi_sum     = sum(pi, na.rm = TRUE),
    
    # --- Share outcomes ---
    informal          = sum(informal         * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    below_min         = sum(below_min        * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    below_min_formal  = sum(below_min_formal * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    
    # --- Inequality outcomes ---
    log_var_wage      = sum(log_var_wage * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    var_log_wage      = sum(var_log_wage * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    log_50_10         = sum(log_50_10    * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    log_90_10         = sum(log_90_10    * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    log_50            = sum(log_50       * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    log_10            = sum(log_10       * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    p90_p10           = sum(p90_p10      * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    
    # --- Treatment variables (time-varying only, same across firm sizes) ---
    post_17           = first(post_17),
    post_19           = first(post_19),
    post_21           = first(post_21),
    post_23           = first(post_23),
    shock_qoq_nom     = sum(shock_qoq_nom  * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    shock_yoy_nom     = sum(shock_yoy_nom  * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    shock_qoq_real    = sum(shock_qoq_real * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    shock_yoy_real    = sum(shock_yoy_real * pi, na.rm = TRUE) / sum(pi, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  left_join(
    exposure_s_baseline %>%
      select(Employment_Sector, exposure_baseline_val, exposure_group),
    by = "Employment_Sector"
  ) %>%
  mutate(
    quarter_num = as.integer(substr(as.character(time), 6, 6)),
    year_num    = as.integer(substr(as.character(time), 1, 4))
  ) %>%
  arrange(Employment_Sector, time)


# --- Diagnostics ---
cat("\n=== Sector × Time Descriptive Panel ===\n")
cat("Rows:              ", nrow(panel_st_descriptive), "\n")
cat("Sectors:           ", n_distinct(panel_st_descriptive$Employment_Sector), "\n")
cat("Quarters:          ", n_distinct(panel_st_descriptive$time), "\n")
cat("Missing informal:  ", mean(is.na(panel_st_descriptive$informal)), "\n")

# Verify Construction is absent and all other sectors present
cat("\nSectors in descriptive panel (Construction should be absent):\n")
print(sort(unique(panel_st_descriptive$Employment_Sector)))

# Check pi coverage per sector — flag sectors where excluded cells
# represented >20% of baseline employment (renormalisation is substantial)
cat("\nPi weight coverage by sector (1.0 = all firm sizes present):\n")
panel_st_descriptive %>%
  distinct(Employment_Sector, sector_pi_sum) %>%
  # sector_pi_sum varies by quarter if cells have missing outcomes —
  # take median across quarters for a stable summary
  group_by(Employment_Sector) %>%
  summarise(median_pi_coverage = round(median(sector_pi_sum, na.rm=TRUE), 3),
            .groups = "drop") %>%
  arrange(median_pi_coverage) %>%
  mutate(flag = if_else(median_pi_coverage < 0.80,
                        "FLAG — >20% of baseline employment excluded",
                        "OK")) %>%
  print()


# ==============================================================================
# SECTION 6: AGGREGATE VARIANCE DECOMPOSITION
# Computed from design_broad at aggregate × quarter level
# DO NOT split by sector — informal cells too small for reliable estimates
# Follows Parente (2024) decomposition structure:
#   Var(log w) = within-formal + within-informal + between (mean gap)
# ==============================================================================

# --- Step 1: Overall variance of log wages by quarter ---
var_agg_overall <- svyby(
  ~log_wage,
  ~year_quarter,
  design  = design_broad,
  FUN     = svyvar,
  na.rm   = TRUE,
  vartype = NULL
) %>%
  as_tibble() %>%
  rename(time = year_quarter, var_overall = log_wage)


# --- Step 2: Variance by formality status × quarter ---
var_agg_byformal <- svyby(
  ~log_wage,
  ~year_quarter + Employment_Status,
  design  = design_broad,
  FUN     = svyvar,
  na.rm   = TRUE,
  vartype = NULL
) %>%
  as_tibble() %>%
  rename(time = year_quarter, var_byformal = log_wage) %>%
  pivot_wider(id_cols = time,
              names_from  = Employment_Status,
              values_from = var_byformal) %>%
  rename(var_formal   = Formal,
         var_informal = Informal)


# --- Step 3: Mean log wages by formality status × quarter ---
# Needed for between-sector component of decomposition
mean_agg_byformal <- svyby(
  ~log_wage,
  ~year_quarter + Employment_Status,
  design  = design_broad,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = NULL
) %>%
  as_tibble() %>%
  rename(time = year_quarter, mean_byformal = log_wage) %>%
  pivot_wider(id_cols = time,
              names_from  = Employment_Status,
              values_from = mean_byformal) %>%
  rename(mean_formal   = Formal,
         mean_informal = Informal)


# --- Step 4: Employment shares by formality status × quarter ---
share_agg_byformal <- svyby(
  ~informal,
  ~year_quarter,
  design  = design_broad,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = NULL
) %>%
  as_tibble() %>%
  rename(time = year_quarter, share_informal = informal) %>%
  mutate(share_formal = 1 - share_informal)


# --- Step 5: Assemble decomposition ---
decomp_agg <- var_agg_overall %>%
  left_join(var_agg_byformal,    by = "time") %>%
  left_join(mean_agg_byformal,   by = "time") %>%
  left_join(share_agg_byformal,  by = "time") %>%
  mutate(
    # Within-formal component
    within_formal   = share_formal   * var_formal,
    # Within-informal component
    within_informal = share_informal * var_informal,
    # Between component (mean gap, weighted by shares)
    between         = share_formal * share_informal *
      (mean_formal - mean_informal)^2,
    # Check: within_formal + within_informal + between ≈ var_overall
    var_recomposed  = within_formal + within_informal + between,
    decomp_check    = abs(var_overall - var_recomposed),
    
    # Shares of total variance
    share_within_formal   = within_formal   / var_overall,
    share_within_informal = within_informal / var_overall,
    share_between         = between         / var_overall
  )

# Verify decomposition adds up
cat("\n=== Variance Decomposition Check ===\n")
cat("Max decomposition residual:", max(decomp_agg$decomp_check, na.rm = TRUE), "\n")
cat("(Should be < 0.001 — rounding only)\n")

# Quick summary of components over time
decomp_agg %>%
  select(time, var_overall, var_formal, var_informal,
         share_within_formal, share_within_informal, share_between) %>%
  filter(!is.na(var_overall)) %>%
  print(n = Inf)




saveRDS(panel_st_descriptive,
        file.path(config$paths$processed_data,
                  "panel_st_descriptive.rds"))

saveRDS(decomp_agg,
        file.path(config$paths$processed_data, "variance_decomp_aggregate.rds"))




