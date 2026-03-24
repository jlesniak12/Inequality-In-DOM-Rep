#===============================================================================
#
# Script: 03_Compute_Descriptive_Stats.R
#
# Scope:  Computes all descriptive statistics for the full-economy context
#         section. Saves results as RDS objects for 04a to plot.
#         No survey computation happens in figure scripts.
#
# TWO DESIGNS — important distinction:
#
#   design_shares:
#     Population : All employed (OCUPADO == 1), no income filter
#     Rationale  : Informality/self-employment status does not require
#                  positive income — conditioning would introduce selection
#     Used for   : informal share, self-employed share
#
#   design_wage_ineq:
#     Population : Salaried workers (private + public employee) with positive
#                  salary income, non-missing formality status
#     Rationale  : Log wages require positive income; self-employed income is
#                  profit not a wage and is not governed by the minimum wage.
#                  Consistent with the causal sample logic.
#     Used for   : log variance, percentile ratios, variance decomposition
#
#   design_selfempl:
#     Population : Self-employed with positive independent income
#     Used for   : self-employed log income variance (shown as separate
#                  comparison series, not mixed with wage inequality)
#
# Outputs → config$paths$processed_data:
#   desc_variance_by_status.rds     var(log wage) by quarter x {Formal, Informal}
#   desc_variance_overall.rds       var(log wage) overall by quarter
#   desc_variance_decomp.rds        within/between decomp by quarter
#   desc_percentile_ratios.rds      p50/p10, p90/p10 log ratios by quarter
#   desc_employment_shares.rds      informal + self-emp shares by quarter
#   desc_selfemployed_variance.rds  var(log income) for self-employed by quarter
#   desc_firmsize_shares.rds        share of employed by firm size tier by quarter
#                                   (among workers with known firm size)
#
# Reads:
#   Full_ENCFT_clean.rds   (Layer 1 — 02_Prepare Survey Data Variables.R)
#
#===============================================================================

source("Code/R/00_setup.R")


#===============================================================================
# STEP 1. Load and Build Designs
#===============================================================================

Full_ENCFT_clean <- readRDS(
  file.path(config$paths$processed_data, "Full_ENCFT_clean.rds")
)


# ── Design 1: All employed (shares denominator) ──────────────────────────────
design_shares <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = Full_ENCFT_clean %>% filter(OCUPADO == 1),
  nest    = TRUE
) %>%
  update(
    is_informal = as.integer(Employment_Status == "Informal"),
    is_selfempl = as.integer(Employment_Type   == "self-employed")
  )

# ── Design 2: Salaried workers with positive wages (wage inequality) ─────────
design_wage_ineq <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = Full_ENCFT_clean %>%
    filter(
      OCUPADO == 1,
      Employment_Type %in% c("private employee", "public employee"),
      real_salary_income_primary > 0,
      !is.na(Employment_Status)
    ),
  nest    = TRUE
) %>%
  update(
    log_wage = log(real_salary_income_primary),
    one      = 1L
  )

# ── Design 3: Self-employed with positive income (comparison series) ──────────
design_selfempl <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = Full_ENCFT_clean %>%
    filter(
      OCUPADO == 1,
      Employment_Type == "self-employed",
      real_independent_income_primary > 0
    ),
  nest    = TRUE
) %>%
  update(log_income = log(real_independent_income_primary))

cat(sprintf(
  "Designs built — shares: %d obs | wage ineq: %d obs | self-empl: %d obs\n",
  nrow(design_shares$variables),
  nrow(design_wage_ineq$variables),
  nrow(design_selfempl$variables)
))

#===============================================================================
# STEP 2. Variance of Log Wages by Formality Status
#===============================================================================

cat("\n[2] Variance of log wages...\n")

calc_var <- function(des, var, time_var = "year_quarter") {
  # Note: svyvar names the SE column "se1" not "se.<var>", which differs from
  # svymean behaviour. To avoid fragile name-based renaming, use vartype = NULL
  # (point estimate only). SE of the variance is not used in figures or decomp.
  svyby(
    formula = as.formula(paste0("~", var)),
    by      = as.formula(paste0("~", time_var)),
    design  = des,
    FUN     = svyvar,
    na.rm   = TRUE,
    vartype = NULL
  ) %>%
    as_tibble() %>%
    rename(
      time     = !!time_var,
      variance = !!var
    )
}

desc_variance_overall  <- calc_var(design_wage_ineq, "log_wage") %>%
  mutate(group = "Overall")

desc_variance_formal   <- calc_var(
  subset(design_wage_ineq, Employment_Status == "Formal"), "log_wage"
) %>% mutate(group = "Formal")

desc_variance_informal <- calc_var(
  subset(design_wage_ineq, Employment_Status == "Informal"), "log_wage"
) %>% mutate(group = "Informal")

desc_selfemployed_variance <- calc_var(design_selfempl, "log_income") %>%
  mutate(group = "Self-employed")

desc_variance_by_status <- bind_rows(
  desc_variance_formal,
  desc_variance_informal
) %>%
  mutate(group = factor(group, levels = c("Formal", "Informal")))

cat("  done\n")


#===============================================================================
# STEP 3. Variance Decomposition (Parente 2024, eq. 1)
#
# V_t = Σ_j s_jt·V_jt  +  Σ_j s_jt·(E_jt − E_t)²
#        ──────────────      ──────────────────────
#           Within                  Between
#
# j ∈ {Formal, Informal}; s_jt = employment-weighted share
#===============================================================================

cat("[3] Variance decomposition...\n")

# Mean log wage by quarter × status
mean_j <- svyby(
  ~log_wage, ~year_quarter + Employment_Status,
  design  = design_wage_ineq,
  FUN     = svymean,
  na.rm   = TRUE, vartype = NULL
) %>%
  as_tibble() %>%
  rename(time = year_quarter, mean_j = log_wage)

# Variance log wage by quarter × status
var_j <- svyby(
  ~log_wage, ~year_quarter + Employment_Status,
  design  = design_wage_ineq,
  FUN     = svyvar,
  na.rm   = TRUE, vartype = NULL
) %>%
  as_tibble() %>%
  rename(time = year_quarter, var_j = log_wage)

# Employment share s_jt: weighted count within each quarter
emp_j <- svyby(
  ~one, ~year_quarter + Employment_Status,
  design  = design_wage_ineq,
  FUN     = svytotal,
  na.rm   = TRUE, vartype = NULL
) %>%
  as_tibble() %>%
  rename(time = year_quarter, emp_count = one) %>%
  group_by(time) %>%
  mutate(s_j = emp_count / sum(emp_count, na.rm = TRUE)) %>%
  ungroup()

# Overall mean log wage by quarter
mean_all <- svyby(
  ~log_wage, ~year_quarter,
  design  = design_wage_ineq,
  FUN     = svymean,
  na.rm   = TRUE, vartype = NULL
) %>%
  as_tibble() %>%
  rename(time = year_quarter, mean_all = log_wage)

desc_variance_decomp <- mean_j %>%
  left_join(var_j,  by = c("time", "Employment_Status")) %>%
  left_join(emp_j %>% select(time, Employment_Status, s_j),
            by = c("time", "Employment_Status")) %>%
  left_join(mean_all, by = "time") %>%
  mutate(
    within_contrib  = s_j * var_j,
    between_contrib = s_j * (mean_j - mean_all)^2
  ) %>%
  group_by(time) %>%
  summarise(
    within  = sum(within_contrib,  na.rm = TRUE),
    between = sum(between_contrib, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    desc_variance_overall %>% select(time, variance) %>% rename(total_var = variance),
    by = "time"
  ) %>%
  mutate(discrepancy = abs(total_var - within - between))

cat(sprintf("  Max decomp discrepancy: %.6f\n",
            max(desc_variance_decomp$discrepancy, na.rm = TRUE)))
cat("  done\n")


#===============================================================================
# STEP 4. Percentile Ratios
#
# p50/p10 and p90/p10 in log terms. Same sample as variance series
# (salaried workers, positive earnings) for consistency.
#===============================================================================

cat("[4] Percentile ratios...\n")

desc_percentile_ratios <- compute_quantiles(
  design   = design_wage_ineq,
  var      = "real_salary_income_primary",
  time_var = "year_quarter",
  probs    = c(0.10, 0.50, 0.90),
  na.rm    = TRUE
) %>%
  select(time, measure, estimate) %>%
  pivot_wider(names_from = measure, values_from = estimate) %>%
  mutate(
    log_p50_p10 = log(p50) - log(p10),
    log_p90_p10 = log(p90) - log(p10),
    log_p90_p50 = log(p90) - log(p50)
  )

cat("  done\n")


#===============================================================================
  # STEP 4B. Formal Sector Median Wage by Firm Size Tier (for Kaitz ratio)
  #
  # The canonical Kaitz index is log(min_wage / median_formal_wage).
  # We compute the median formal salary within each firm size tier over time so
  # 04a can build a proper tier-specific log Kaitz figure.
  #
  # This does NOT require the regression sample restriction — Full_ENCFT_clean
  # already has Wage_group. We just need formal salaried workers with known size.
  # Using formal-only is correct: the Kaitz measures how binding the floor is
  # for formal workers specifically.
  #===============================================================================

cat("[4b] Formal sector median wage by firm size tier...\n")

design_formal_bysize <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = Full_ENCFT_clean %>%
    filter(
      OCUPADO == 1,
      Employment_Type %in% c("private employee", "public employee"),
      Employment_Status == "Formal",
      real_salary_income_primary > 0,
      Wage_group %in% c("Micro", "Small", "Medium", "Large")
    ),
  nest = TRUE
)

desc_formal_median_by_tier <- compute_quantiles(
  design    = design_formal_bysize,
  var       = "real_salary_income_primary",
  time_var  = "year_quarter",
  group_var = "Wage_group",
  probs     = 0.50,
  na.rm     = TRUE
) %>%
  select(time, group, estimate) %>%
  rename(Wage_group = group, p50_formal = estimate)

cat("  done\n")

#===============================================================================
# STEP 5. Employment Shares
#
# Uses design_shares — all employed, no income filter.
# Informal share = share of employed who are informal (GRUPO_EMPLEO based)
# Self-emp share = share of employed who are self-employed (CATEGORIA based)
#===============================================================================

cat("[5] Employment shares...\n")

informal_share <- compute_prop_indicator(
  design   = design_shares,
  var      = "is_informal",
  time_var = "year_quarter"
) %>%
  select(time, estimate) %>%
  rename(informal_share = estimate)

selfempl_share <- compute_prop_indicator(
  design   = design_shares,
  var      = "is_selfempl",
  time_var = "year_quarter"
) %>%
  select(time, estimate) %>%
  rename(selfempl_share = estimate)

desc_employment_shares <- informal_share %>%
  left_join(selfempl_share, by = "time")




cat("  done\n")


#===============================================================================
# STEP 6. Firm Size Employment Shares Over Time
#
# Uses design_shares (all employed, no income filter) — same denominator as
# informal/self-emp shares for consistency.
#
# Tracks the share of employed workers in each firm size tier over time.
# Important for the identification section because:
#   (a) It confirms all four tiers represent meaningful shares of employment,
#       so the cross-tier variation in exposure is empirically relevant.
#   (b) It shows whether composition is shifting over time — a drift toward
#       micro firms would be a potential confounder since the minimum wage
#       bites hardest in that tier.
#
# Note: Wage_group is only defined for workers with a known firm size.
#       Workers with missing or "Dont Know" firm size are excluded from the
#       share denominator here (i.e. shares sum to 1 among those with known
#       firm size). This is flagged in the caption of the figure.
#       Denominators and NAs are checked below.
#===============================================================================

cat("[6] Firm size employment shares over time...\n")

# Add firm size indicators to design_shares
# Wage_group is already present in the underlying data; add dummies for each tier
design_shares_fs <- update(
  design_shares,
  is_micro  = as.integer(!is.na(Wage_group) & Wage_group == "Micro"),
  is_small  = as.integer(!is.na(Wage_group) & Wage_group == "Small"),
  is_medium = as.integer(!is.na(Wage_group) & Wage_group == "Medium"),
  is_large  = as.integer(!is.na(Wage_group) & Wage_group == "Large"),
  has_known_size = as.integer(
    !is.na(Wage_group) & !(Wage_group %in% c("Dont Know", "Unknown"))
  )
)

# Share of employed with *known* firm size (use as data quality check)
known_size_share <- compute_prop_indicator(
  design   = design_shares_fs,
  var      = "has_known_size",
  time_var = "year_quarter"
) %>%
  select(time, estimate) %>%
  rename(known_size_share = estimate)

cat(sprintf("  Share with known firm size — min: %.1f%%, max: %.1f%%\n",
            min(known_size_share$known_size_share, na.rm = TRUE) * 100,
            max(known_size_share$known_size_share, na.rm = TRUE) * 100))

# Restrict to workers with known firm size for the tier shares
# (shares will sum to 1 within this subgroup)
design_known_fs <- subset(
  design_shares_fs,
  has_known_size == 1
)

# Compute share of each tier among workers with known firm size
tier_vars <- c("is_micro", "is_small", "is_medium", "is_large")

tier_shares_list <- lapply(tier_vars, function(v) {
  compute_prop_indicator(
    design   = design_known_fs,
    var      = v,
    time_var = "year_quarter"
  ) %>%
    select(time, estimate) %>%
    rename(share = estimate) %>%
    mutate(Wage_group = sub("is_", "", v) %>%
             tools::toTitleCase())
})

desc_firmsize_shares <- bind_rows(tier_shares_list) %>%
  mutate(
    Wage_group = recode(Wage_group,
                        "Micro"  = "Micro",
                        "Small"  = "Small",
                        "Medium" = "Medium",
                        "Large"  = "Large"),
    Wage_group = factor(Wage_group,
                        levels = c("Micro", "Small", "Medium", "Large"))
  ) %>%
  # Attach the known-size share for context / caption
  left_join(known_size_share %>% select(time, known_size_share), by = "time")

# Sanity check: shares should sum to ~1 within each quarter
share_sums <- desc_firmsize_shares %>%
  group_by(time) %>%
  summarise(total = sum(share, na.rm = TRUE), .groups = "drop")

cat(sprintf("  Share sum check — min: %.4f, max: %.4f (should be ~1)\n",
            min(share_sums$total), max(share_sums$total)))

cat("  done\n")


#===============================================================================
# STEP 7. Save Outputs
#===============================================================================

pd <- config$paths$processed_data

to_save <- list(
  desc_variance_by_status      = desc_variance_by_status,
  desc_variance_overall        = desc_variance_overall,
  desc_variance_decomp         = desc_variance_decomp,
  desc_percentile_ratios       = desc_percentile_ratios,
  desc_employment_shares       = desc_employment_shares,
  desc_selfemployed_variance   = desc_selfemployed_variance,
  desc_firmsize_shares         = desc_firmsize_shares,
  desc_formal_median_by_tier   = desc_formal_median_by_tier
)

for (nm in names(to_save)) {
  saveRDS(to_save[[nm]], file.path(pd, paste0(nm, ".rds")))
  cat("  Saved:", nm, "\n")
}

cat("\n=== 03_Compute_Descriptive_Stats.R complete ===\n")
