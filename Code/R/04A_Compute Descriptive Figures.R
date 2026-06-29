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

#===============================================================================
# STEP 8. Formal / Informal Summary Table
#
# Replicates the structure of Table 1 in Parente (2024) adapted to the
# Dominican Republic ENCFT context.  Computes the following statistics
# separately for Formal and Informal workers, for every quarter in the data:
#
#   - n_workers          : weighted worker count
#   - mean_income        : mean real monthly labor income (primary job, > 0)
#   - median_income      : median real monthly labor income (primary job, > 0)
#   - pct_secondary_plus : share with secondary or university education
#   - pct_male           : share male
#   - mean_age           : mean age
#   - share_of_employed  : share of all employed workers
#   - pct_small_micro    : share in Micro or Small firms
#                          (among workers with known firm size)
#
# INCOME CONCEPT
#   We use real_total_income_primary = real_salary_income_primary +
#   real_independent_income_primary, restricted to workers where this total
#   is positive.  Rationale: ~60% of informal workers have zero salary income
#   but positive independent (self-employment) income.  Using salary-only
#   would drop the majority of the informal sector from income statistics,
#   producing a severely upward-biased informal median and a misleading
#   formal/informal income gap.  Using the total ensures every working
#   informal worker with any earnings is included.
#
# Population:  All employed (OCUPADO == 1) with non-missing Employment_Status.
#
# Income population:  As above but restricted to real_total_income_primary > 0.
#
# Weights:  FACTOR_EXPANSION throughout.
#
# Outputs -> config$paths$processed_data:
#   desc_summary_table.rds        long tibble, one row per quarter x status
#   desc_summary_table_wide.rds   two-period wide comparison table (if focal
#                                 quarters present)
#
# PNG output -> config$paths$outputs / config$output_stage / "Summary Table":
#   summary_table_2016Q2_vs_2025Q2.png   publication-ready table image
#
#===============================================================================

cat("\n[8] Formal / Informal summary table...\n")

# -- packages (gt for table rendering) ----------------------------------------
if (!requireNamespace("gt", quietly = TRUE)) install.packages("gt")
library(gt)

# -- output folder -------------------------------------------------------------
tbl_out_dir <- file.path(config$paths$outputs, config$output_stage,
                         "Summary Table")
dir.create(tbl_out_dir, recursive = TRUE, showWarnings = FALSE)


#===============================================================================
# 8.1  Base design: all employed with known formality
#===============================================================================

design_summary <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = Full_ENCFT_clean %>%
    filter(OCUPADO == 1, !is.na(Employment_Status)),
  nest    = TRUE
) %>%
  update(
    is_secondary_plus = as.integer(education %in% c("Secondary", "University")),
    is_male           = as.integer(Sex == "Male"),
    one               = 1L,
    is_small_micro    = as.integer(
      !is.na(Wage_group) & Wage_group %in% c("Micro", "Small")
    ),
    has_known_size    = as.integer(
      !is.na(Wage_group) & !(Wage_group %in% c("Dont Know", "Unknown"))
    )
  )


#===============================================================================
# 8.2  Income design: total primary labor income > 0
#
#   real_total_income_primary = real_salary_income_primary +
#                               real_independent_income_primary
#   (constructed in 02_Prepare Survey Data Panels.R)
#
#   Using total income ensures informal workers who earn via self-employment
#   (INGRESO_INDEPENDIENTES) are not silently excluded from income statistics.
#   In the sample ~60% of informal workers have zero salary but positive
#   independent income; conditioning on salary > 0 would drop them entirely.
#===============================================================================

design_income <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = Full_ENCFT_clean %>%
    filter(
      OCUPADO == 1,
      !is.na(Employment_Status),
      real_total_income_primary > 0
    ),
  nest    = TRUE
)


#===============================================================================
# 8.3  Helper: extract scalar from svyquantile (survey-package version safe)
#===============================================================================

extract_svyquantile <- function(svyq_result) {
  q <- svyq_result[[1]]
  if (is.matrix(q)) return(as.numeric(q[1, 1]))
  as.numeric(q)
}


#===============================================================================
# 8.4  Main computation loop over quarters x formality status
#===============================================================================

quarters <- sort(unique(
  Full_ENCFT_clean$year_quarter[Full_ENCFT_clean$OCUPADO == 1]
))
statuses <- c("Formal", "Informal")

cat(sprintf("  Computing over %d quarters x 2 statuses...\n", length(quarters)))

summary_rows <- vector("list", length(quarters) * length(statuses))
idx <- 0L

for (qtr in quarters) {
  for (status in statuses) {
    
    idx <- idx + 1L
    
    des_q     <- subset(design_summary,
                        year_quarter == qtr & Employment_Status == status)
    des_q_inc <- subset(design_income,
                        year_quarter == qtr & Employment_Status == status)
    
    # (a) weighted worker count
    n_workers <- as.numeric(svytotal(~one, des_q, na.rm = TRUE))
    
    # (b) share of total employed this quarter
    des_all_q <- subset(design_summary, year_quarter == qtr)
    n_all     <- as.numeric(svytotal(~one, des_all_q, na.rm = TRUE))
    share_emp <- n_workers / n_all
    
    # (c) mean and median total primary labor income (positive earners only)
    n_inc <- nrow(des_q_inc$variables)
    
    if (n_inc > 0) {
      mean_income <- as.numeric(
        svymean(~real_total_income_primary, des_q_inc, na.rm = TRUE)
      )
      median_income <- extract_svyquantile(
        svyquantile(~real_total_income_primary, des_q_inc,
                    quantiles = 0.5, na.rm = TRUE, ci = FALSE)
      )
    } else {
      mean_income   <- NA_real_
      median_income <- NA_real_
    }
    
    # (d) % secondary or university
    pct_secondary_plus <- as.numeric(
      svymean(~is_secondary_plus, des_q, na.rm = TRUE)
    )
    
    # (e) % male
    pct_male <- as.numeric(svymean(~is_male, des_q, na.rm = TRUE))
    
    # (f) mean age
    mean_age <- as.numeric(svymean(~EDAD, des_q, na.rm = TRUE))
    
    # (g) % in small / micro firms (among workers with known firm size)
    des_q_known <- subset(des_q, has_known_size == 1)
    n_known     <- nrow(des_q_known$variables)
    
    pct_small_micro <- if (n_known > 0) {
      as.numeric(svymean(~is_small_micro, des_q_known, na.rm = TRUE))
    } else {
      NA_real_
    }
    
    summary_rows[[idx]] <- tibble(
      time               = qtr,
      Employment_Status  = status,
      n_workers          = n_workers,
      share_of_employed  = share_emp,
      mean_income        = mean_income,
      median_income      = median_income,
      pct_secondary_plus = pct_secondary_plus,
      pct_male           = pct_male,
      mean_age           = mean_age,
      pct_small_micro    = pct_small_micro
    )
  }
  
  if (which(quarters == qtr) %% 8 == 0)
    cat(sprintf("    ... processed through %s\n", qtr))
}

desc_summary_table <- bind_rows(summary_rows) %>%
  mutate(Employment_Status = factor(Employment_Status,
                                    levels = c("Formal", "Informal"))) %>%
  arrange(time, Employment_Status)

cat(sprintf("  Summary table: %d rows (%d quarters x 2 statuses)\n",
            nrow(desc_summary_table), length(quarters)))
cat("  done\n")


#===============================================================================
# 8.5  Save long-form output
#===============================================================================

saveRDS(desc_summary_table,
        file.path(config$paths$processed_data, "desc_summary_table.rds"))
cat("  Saved: desc_summary_table.rds\n")


#===============================================================================
# 8.6  Build two-period wide comparison table
#===============================================================================

FOCAL_QTRS <- c("2016Q2", "2025Q2")
focal_data <- desc_summary_table %>% filter(time %in% FOCAL_QTRS)

if (nrow(focal_data) < 4) {
  
  cat(sprintf(
    "  Note: focal quarters not fully present (found %d/4 rows).\n",
    nrow(focal_data)
  ))
  cat("  Available quarters: ",
      paste(head(quarters, 10), collapse = ", "), "...\n")
  
} else {
  
  # Row display order and labels
  STAT_LEVELS <- c(
    "n_workers", "mean_income", "median_income",
    "pct_secondary_plus", "pct_male", "mean_age",
    "share_of_employed", "pct_small_micro"
  )
  STAT_LABELS <- c(
    n_workers          = "Number of Workers",
    mean_income        = "Mean Labor Income (real DOP/month)",
    median_income      = "Median Labor Income (real DOP/month)",
    pct_secondary_plus = "% with Secondary or College",
    pct_male           = "% Male",
    mean_age           = "Mean Age",
    share_of_employed  = "Share of Total Employment",
    pct_small_micro    = "% in Small & Micro Firms"
  )
  
  # Cell formatter
  fmt_cell <- function(stat, value) {
    dplyr::case_when(
      stat == "n_workers"                          ~
        formatC(value, format = "f", digits = 0, big.mark = ","),
      stat %in% c("mean_income", "median_income")  ~
        formatC(value, format = "f", digits = 0, big.mark = ","),
      stat %in% c("pct_secondary_plus", "pct_male",
                  "share_of_employed",  "pct_small_micro") ~
        paste0(round(value * 100, 1), "%"),
      stat == "mean_age"                           ~
        as.character(round(value, 1)),
      TRUE ~ as.character(round(value, 2))
    )
  }
  
  # Pivot to wide: columns = "YYYYQQ\nFormal" / "YYYYQQ\nInformal"
  desc_summary_table_wide <- focal_data %>%
    tidyr::pivot_longer(
      cols      = all_of(STAT_LEVELS),
      names_to  = "statistic",
      values_to = "value"
    ) %>%
    mutate(
      col_label  = paste0(time, "\n", Employment_Status),
      statistic  = factor(statistic, levels = STAT_LEVELS),
      value_fmt  = mapply(fmt_cell, as.character(statistic), value),
      stat_label = STAT_LABELS[as.character(statistic)]
    ) %>%
    select(stat_label, col_label, value_fmt) %>%
    tidyr::pivot_wider(names_from = col_label, values_from = value_fmt) %>%
    rename(Statistic = stat_label) %>%
    # enforce row order
    mutate(Statistic = factor(Statistic, levels = STAT_LABELS)) %>%
    arrange(Statistic) %>%
    mutate(Statistic = as.character(Statistic))
  
  saveRDS(desc_summary_table_wide,
          file.path(config$paths$processed_data, "desc_summary_table_wide.rds"))
  cat("  Saved: desc_summary_table_wide.rds\n")
  
  cat("\n  Two-period comparison table (console preview):\n")
  print(desc_summary_table_wide, n = Inf, width = Inf)
  
  
  #=============================================================================
  # 8.7  Render publication-ready PNG table with gt
  #
  #   Layout mirrors the brief: rows = statistics, spanners = period,
  #   sub-columns = Formal | Informal.
  #   Colour palette aligned with theme_surveytools (blues).
  #=============================================================================
  
  HEADER_BLUE  <- "#2C5F8A"   # dark header — matches project blues
  SUBHEAD_BLUE <- "#4A86B8"   # spanner row
  FORMAL_BG    <- "#EBF3FA"   # light blue tint  (formal columns)
  INFORMAL_BG  <- "#FDF6EC"   # light warm tint  (informal columns)
  ALT_ROW      <- "#F7F7F7"   # alternating row shade
  BORDER_COL   <- "#BFBFBF"
  WHITE        <- "#FFFFFF"
  
  # Desired column order
  col_order <- c(
    "Statistic",
    "2016Q2\nFormal",  "2016Q2\nInformal",
    "2025Q2\nFormal",  "2025Q2\nInformal"
  )
  col_order_present <- col_order[col_order %in% names(desc_summary_table_wide)]
  
  tbl_gt <- desc_summary_table_wide %>%
    select(all_of(col_order_present)) %>%
    
    gt(rowname_col = "Statistic") %>%
    
    # ── spanners (one per focal quarter) -------------------------------------
  tab_spanner(
    label   = md("**2016 Q2**"),
    columns = starts_with("2016Q2")
  ) %>%
    tab_spanner(
      label   = md("**2025 Q2**"),
      columns = starts_with("2025Q2")
    ) %>%
    
    # ── clean column labels (strip embedded newline) -------------------------
  cols_label(
    `2016Q2\nFormal`   = md("**Formal**"),
    `2016Q2\nInformal` = md("**Informal**"),
    `2025Q2\nFormal`   = md("**Formal**"),
    `2025Q2\nInformal` = md("**Informal**")
  ) %>%
    
    # ── title & subtitle ------------------------------------------------------
  tab_header(
    title    = md("**Formal and Informal Sector: Worker Characteristics**"),
    subtitle = md(paste0(
      "Dominican Republic — ENCFT &nbsp;|&nbsp; 2016 Q2 vs 2025 Q2<br>",
      "<span style='font-size:11px;color:#555;'>",
      "All employed workers. Income = real total primary labor income ",
      "(salary + independent), Real DOP. Weighted estimates.",
      "</span>"
    ))
  ) %>%
    
    # ── source note -----------------------------------------------------------
  tab_source_note(source_note = md(paste0(
    "*Source:* Banco Central de la Rep\u00fablica Dominicana \u2014 ENCFT. &nbsp;",
    "Formality defined by GRUPO\\_EMPLEO. &nbsp;",
    "Small & Micro = 1\u201350 workers. &nbsp;",
    "Income deflated with CPI."
  ))) %>%
    
    # ── stub (row label) style ------------------------------------------------
  tab_style(
    style = list(
      cell_text(size = px(12), color = "#222222"),
      cell_borders(sides = "right", color = BORDER_COL, weight = px(1))
    ),
    locations = cells_stub()
  ) %>%
    
    # ── spanner background ----------------------------------------------------
  tab_style(
    style = list(
      cell_fill(color = SUBHEAD_BLUE),
      cell_text(color = WHITE, weight = "bold", size = px(13))
    ),
    locations = cells_column_spanners()
  ) %>%
    
    # ── column label header ---------------------------------------------------
  tab_style(
    style = list(
      cell_fill(color = HEADER_BLUE),
      cell_text(color = WHITE, weight = "bold", size = px(12))
    ),
    locations = cells_column_labels()
  ) %>%
    
    # ── formal column tint (body + label) ------------------------------------
  tab_style(
    style = cell_fill(color = FORMAL_BG),
    locations = cells_body(columns = ends_with("Formal"))
  ) %>%
    tab_style(
      style = cell_fill(color = FORMAL_BG),
      locations = cells_column_labels(columns = ends_with("Formal"))
    ) %>%
    
    # ── informal column tint (body + label) ----------------------------------
  tab_style(
    style = cell_fill(color = INFORMAL_BG),
    locations = cells_body(columns = ends_with("Informal"))
  ) %>%
    tab_style(
      style = cell_fill(color = INFORMAL_BG),
      locations = cells_column_labels(columns = ends_with("Informal"))
    ) %>%
    
    # ── alternating row shading (even rows) ----------------------------------
  tab_style(
    style = cell_fill(color = ALT_ROW),
    locations = cells_body(rows = seq(2, nrow(desc_summary_table_wide), 2))
  ) %>%
    
    # ── bold key rows ---------------------------------------------------------
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Statistic %in% c(
        "Number of Workers",
        "Median Labor Income (real DOP/month)",
        "Share of Total Employment"
      )
    )
  ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_stub(
        rows = Statistic %in% c(
          "Number of Workers",
          "Median Labor Income (real DOP/month)",
          "Share of Total Employment"
        )
      )
    ) %>%
    
    # ── body text: right-aligned, consistent size ----------------------------
  tab_style(
    style = list(
      cell_text(size = px(12), align = "right"),
      cell_borders(sides = c("top", "bottom"),
                   color = BORDER_COL, weight = px(0.5))
    ),
    locations = cells_body()
  ) %>%
    
    # ── title & subtitle styling ----------------------------------------------
  tab_style(
    style = cell_text(size = px(15), weight = "bold", color = HEADER_BLUE),
    locations = cells_title(groups = "title")
  ) %>%
    tab_style(
      style = cell_text(size = px(11), color = "#444444"),
      locations = cells_title(groups = "subtitle")
    ) %>%
    
    # ── source note -----------------------------------------------------------
  tab_style(
    style = cell_text(size = px(10), color = "#666666"),
    locations = cells_source_notes()
  ) %>%
    
    # ── column widths ---------------------------------------------------------
  cols_width(
    Statistic ~ px(270),
    everything() ~ px(118)
  ) %>%
    
    # ── global table options --------------------------------------------------
  tab_options(
    table.font.names                    = "Arial",
    table.border.top.color              = HEADER_BLUE,
    table.border.top.width              = px(3),
    table.border.bottom.color           = HEADER_BLUE,
    table.border.bottom.width           = px(2),
    heading.border.bottom.color         = HEADER_BLUE,
    column_labels.border.top.color      = HEADER_BLUE,
    column_labels.border.bottom.color   = HEADER_BLUE,
    column_labels.border.bottom.width   = px(2),
    stub.border.color                   = BORDER_COL,
    row.striping.include_stub           = FALSE,
    data_row.padding                    = px(7),
    source_notes.padding                = px(6),
    table.width                         = px(810)
  )
  
  # Save PNG (zoom = 2 for retina / presentation quality)
  png_path <- file.path(
    tbl_out_dir,
    paste0("summary_table_",
           FOCAL_QTRS[1], "_vs_", FOCAL_QTRS[2], ".png")
  )
  gtsave(tbl_gt, filename = png_path, vwidth = 850, vheight = 540, zoom = 2)
  cat("  Saved PNG:", png_path, "\n")
  
}

cat("\n=== Step 8 complete ===\n")



#===============================================================================
# STEP 9. Wage Distribution Data for Bunching / Min Wage Reference Figures
#
# Prepares the individual-level microdata extracts needed to plot:
#
#   Figure A (formal workers):
#     Survey-weighted kernel density of log real total primary income by firm
#     size tier (Micro, Small, Medium, Large), comparing a full baseline year
#     (2016) with a full recent year (2024).  Four overlaid densities per panel,
#     each with its own matched vertical minimum wage reference line.
#
#   Figure B (informal workers):
#     Survey-weighted kernel density of log real total primary income (pooled,
#     no firm size split) for the same two years, with a shaded band showing
#     the range of minimum wages across all four tiers as a reference zone.
#     Firm size is NOT used to split the informal density because:
#       (a) ~30-40% of informal workers have unknown firm size, so conditioning
#           would introduce strong selection bias.
#       (b) The minimum wage is not legally binding for informal workers; the
#           relevant question is spillover relative to the *range* of formal
#           thresholds, not tier-specific compliance.
#
# INCOME CONCEPT
#   real_total_income_primary (= salary + independent), consistent with Step 8.
#   Restricted to positive values.  Log-transformed for the density.
#
# MINIMUM WAGE LINES
#   real_minwage_harmonized from Min_Wage.rds, averaged over all four quarters
#   within each focal year (annual average in real 2015 DOP terms).  This
#   smooths out within-year noise from the rotating panel and gives a single
#   stable reference line per tier per year.
#   Pre-2021 micro uses the small-firm rate (already encoded in
#   real_minwage_harmonized by 01_Import and Clean Min Wage Data.R).
#
# NOTE ON BANDWIDTH
#   A fixed bandwidth of bw = 0.15 on the log scale is used for both figures
#   and both years.  This is intentional: bunching shows up as a sharp local
#   spike and an overly smooth bandwidth (e.g. 0.3-0.5) will wash it out.
#   0.15 is narrow enough to reveal bunching while still producing a readable
#   density for the thinner medium/large tiers.  Adjust DIST_BW below if needed.
#
# Outputs -> config$paths$processed_data:
#   desc_wage_dist_formal.rds    microdata extract for Figure A (formal)
#   desc_wage_dist_informal.rds  microdata extract for Figure B (informal)
#   desc_mw_annual_avg.rds       annual average real min wage by tier x year
#
#===============================================================================

cat("\n[9] Wage distribution data for bunching figures...\n")

# ── constants (edit here to change focal years or bandwidth) ──────────────────
DIST_YEARS   <- c(2016L, 2024L)
DIST_BW      <- 0.15        # kernel bandwidth on log scale
DIST_TIERS   <- c("Micro", "Small", "Medium", "Large")


#===============================================================================
# 9.1  Annual average real minimum wage by tier x focal year
#===============================================================================

min_wage_raw <- readRDS(file.path(config$paths$processed_data, "Min_Wage.rds"))

desc_mw_annual_avg <- min_wage_raw %>%
  filter(
    Wage_group %in% DIST_TIERS,
    year       %in% DIST_YEARS
  ) %>%
  group_by(year, Wage_group) %>%
  summarise(
    log_real_mw = mean(log(real_minwage_harmonized), na.rm = TRUE),
    real_mw     = mean(real_minwage_harmonized,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Wage_group = factor(Wage_group, levels = DIST_TIERS),
    # Flag: in 2016 micro used the small rate — annotate in figure
    mw_note = dplyr::if_else(
      Wage_group == "Micro" & year == 2016,
      "Small rate (pre-2021)",
      NA_character_
    )
  )

cat(sprintf("  Min wage annual averages computed for %d tier x year combinations.\n",
            nrow(desc_mw_annual_avg)))


#===============================================================================
# 9.2  Formal worker microdata extract (Figure A)
#
#   Population: formal private-sector employees, positive total primary income,
#   known firm size tier (Micro/Small/Medium/Large only).
#   Government and Electricity & Water excluded to match regression sample.
#   Both focal years pooled (year column retained for faceting).
#===============================================================================

desc_wage_dist_formal <- Full_ENCFT_clean %>%
  filter(
    OCUPADO           == 1,
    Employment_Status == "Formal",
    Employment_Type   == "private employee",
    real_total_income_primary > 0,
    !is.na(Wage_group),
    Wage_group %in% DIST_TIERS,
    !(Employment_Sector %in% c("Government", "Electricity and Water")),
    year %in% DIST_YEARS
  ) %>%
  mutate(
    log_income = log(real_total_income_primary),
    Wage_group = factor(Wage_group, levels = DIST_TIERS),
    year_label = paste0(year, " (formal, private sector)")
  ) %>%
  select(
    year, year_label, Wage_group,
    log_income, real_total_income_primary,
    FACTOR_EXPANSION, psu_unique, strata_unique
  )

# Normalise weights within each year x tier so densities integrate to ~1
# when plotted with geom_density(aes(weight = w_norm)).
# This is essential: raw expansion factors vary across years due to population
# growth; without normalisation the 2024 density would dwarf 2016.
desc_wage_dist_formal <- desc_wage_dist_formal %>%
  group_by(year, Wage_group) %>%
  mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
  ungroup()

# Sample size diagnostics — print so analyst can assess density reliability
formal_n <- desc_wage_dist_formal %>%
  group_by(year, Wage_group) %>%
  summarise(
    n_obs     = n(),
    n_wtd     = sum(FACTOR_EXPANSION),
    .groups   = "drop"
  )

cat("  Formal worker counts by year x tier:\n")
print(formal_n, n = Inf)

# Flag tiers with very thin samples (< 100 unweighted obs) — may need caution note
thin_cells <- formal_n %>% filter(n_obs < 100)
if (nrow(thin_cells) > 0) {
  cat("  WARNING: thin cells (n < 100 unweighted obs) — density may be unreliable:\n")
  print(thin_cells)
  cat("  Consider adding a note to the figure caption for these tiers.\n")
}


#===============================================================================
# 9.3  Informal worker microdata extract (Figure B)
#
#   Population: informal workers, positive total primary income, both focal
#   years.  NO firm size filter — pooled across all firm sizes (including
#   unknown) to avoid selection bias.  See header note for rationale.
#===============================================================================

desc_wage_dist_informal <- Full_ENCFT_clean %>%
  filter(
    OCUPADO           == 1,
    Employment_Status == "Informal",
    real_total_income_primary > 0,
    year %in% DIST_YEARS
  ) %>%
  mutate(
    log_income = log(real_total_income_primary),
    year_label = paste0(year, " (informal)")
  ) %>%
  select(
    year, year_label,
    log_income, real_total_income_primary,
    FACTOR_EXPANSION, psu_unique, strata_unique
  )

# Normalise weights within each year
desc_wage_dist_informal <- desc_wage_dist_informal %>%
  group_by(year) %>%
  mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
  ungroup()

informal_n <- desc_wage_dist_informal %>%
  group_by(year) %>%
  summarise(n_obs = n(), n_wtd = sum(FACTOR_EXPANSION), .groups = "drop")

cat("  Informal worker counts by year:\n")
print(informal_n)

# Min-wage band for Figure B: range of log min wages across all four tiers
# within each focal year (used for shaded reference band)
desc_mw_band_informal <- desc_mw_annual_avg %>%
  group_by(year) %>%
  summarise(
    log_mw_min = min(log_real_mw, na.rm = TRUE),
    log_mw_max = max(log_real_mw, na.rm = TRUE),
    .groups    = "drop"
  )

cat("  done\n")


#===============================================================================
# 9.4  Save outputs
#===============================================================================

saveRDS(desc_wage_dist_formal,
        file.path(config$paths$processed_data, "desc_wage_dist_formal.rds"))
saveRDS(desc_wage_dist_informal,
        file.path(config$paths$processed_data, "desc_wage_dist_informal.rds"))
saveRDS(desc_mw_annual_avg,
        file.path(config$paths$processed_data, "desc_mw_annual_avg.rds"))
saveRDS(desc_mw_band_informal,
        file.path(config$paths$processed_data, "desc_mw_band_informal.rds"))

cat("  Saved: desc_wage_dist_formal.rds\n")
cat("  Saved: desc_wage_dist_informal.rds\n")
cat("  Saved: desc_mw_annual_avg.rds\n")
cat("  Saved: desc_mw_band_informal.rds\n")
cat("\n=== Step 9 complete ===\n")



#===============================================================================
# STEP 10. Variance of Log Total Labor Income by Formality Status
#
# Companion to Step 2 (variance of log salary income). Computes var(log total
# income) for Formal and Informal workers separately, where total income =
# salary + independent income across all jobs.
#
# WHY THIS IS NEEDED:
#   Step 2 uses design_wage_ineq which restricts to salaried workers with
#   positive salary income. This excludes ~60% of informal workers (those
#   with zero salary but positive independent/self-employment income).
#   Figure 3a (salary variance) therefore represents informal wage earners
#   only — a positively selected minority of the informal sector.
#
#   This step uses a broader design covering ALL employed workers with
#   positive total labor income, regardless of income source. The informal
#   series now includes self-employed and own-account workers, giving a
#   complete picture of earnings inequality in the informal sector.
#
# DESIGN:
#   Population : OCUPADO == 1, real_total_income_primary > 0,
#                Employment_Status in {Formal, Informal}
#   Income var : real_total_income_primary (primary job only, consistent
#                with the salary income design which also uses primary)
#   Log transform applied inside the design update (NAs for non-positive)
#
# NOTE: Using primary job total income keeps the comparison clean. Including
#   secondary job income would mix employment relationships and make the
#   formal/informal distinction ambiguous for multi-job holders.
#
# Outputs -> config$paths$processed_data:
#   desc_variance_total_by_status.rds   var(log total income) by quarter x
#                                       {Formal, Informal}
#   desc_variance_total_overall.rds     var(log total income) overall by quarter
#
#===============================================================================

cat("\n[10] Variance of log total income by formality status...\n")

# ── Design: all employed with positive total primary income -------------------
design_total_ineq <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = Full_ENCFT_clean %>%
    filter(
      OCUPADO == 1,
      !is.na(Employment_Status),
      Employment_Status %in% c("Formal", "Informal"),
      real_total_income_primary > 0
    ),
  nest    = TRUE
) %>%
  update(
    log_total = log(real_total_income_primary),
    one       = 1L
  )

cat(sprintf("  Total income design: %d obs\n",
            nrow(design_total_ineq$variables)))

# ── Variance by status -------------------------------------------------------
desc_variance_total_formal <- calc_var(
  subset(design_total_ineq, Employment_Status == "Formal"),
  "log_total"
) %>% mutate(group = "Formal")

desc_variance_total_informal <- calc_var(
  subset(design_total_ineq, Employment_Status == "Informal"),
  "log_total"
) %>% mutate(group = "Informal")

desc_variance_total_overall <- calc_var(
  design_total_ineq, "log_total"
) %>% mutate(group = "Overall")

desc_variance_total_by_status <- bind_rows(
  desc_variance_total_formal,
  desc_variance_total_informal
) %>%
  mutate(group = factor(group, levels = c("Formal", "Informal")))

# ── Sanity check: print range for both income concepts -----------------------
cat("  Variance range — salary income (Step 2):\n")
cat(sprintf("    Formal:   %.3f to %.3f\n",
            min(desc_variance_by_status$variance[desc_variance_by_status$group == "Formal"],  na.rm = TRUE),
            max(desc_variance_by_status$variance[desc_variance_by_status$group == "Formal"],  na.rm = TRUE)))
cat(sprintf("    Informal: %.3f to %.3f\n",
            min(desc_variance_by_status$variance[desc_variance_by_status$group == "Informal"], na.rm = TRUE),
            max(desc_variance_by_status$variance[desc_variance_by_status$group == "Informal"], na.rm = TRUE)))

cat("  Variance range — total income (Step 10):\n")
cat(sprintf("    Formal:   %.3f to %.3f\n",
            min(desc_variance_total_by_status$variance[desc_variance_total_by_status$group == "Formal"],  na.rm = TRUE),
            max(desc_variance_total_by_status$variance[desc_variance_total_by_status$group == "Formal"],  na.rm = TRUE)))
cat(sprintf("    Informal: %.3f to %.3f\n",
            min(desc_variance_total_by_status$variance[desc_variance_total_by_status$group == "Informal"], na.rm = TRUE),
            max(desc_variance_total_by_status$variance[desc_variance_total_by_status$group == "Informal"], na.rm = TRUE)))

cat("  done\n")

# ── Save --------------------------------------------------------------------
saveRDS(desc_variance_total_by_status,
        file.path(config$paths$processed_data,
                  "desc_variance_total_by_status.rds"))
saveRDS(desc_variance_total_overall,
        file.path(config$paths$processed_data,
                  "desc_variance_total_overall.rds"))

cat("  Saved: desc_variance_total_by_status.rds\n")
cat("  Saved: desc_variance_total_overall.rds\n")
cat("\n=== Step 10 complete ===\n")


#===============================================================================
# STEP 11. Variance Decomposition — Total Labor Income
#
# Companion to Step 3 (decomposition of log salary variance). Applies the
# same within/between decomposition (Parente 2024, eq. 1) to log total
# primary labor income, covering all employed workers with positive total
# income rather than just wage earners.
#
# FORMULA (identical to Step 3, different income concept and sample):
#
#   V_t = Σ_j s_jt * V_jt  +  Σ_j s_jt * (E_jt − E_t)²
#          ──────────────         ──────────────────────
#             Within                      Between
#
#   j ∈ {Formal, Informal}
#   s_jt = employment-weighted share of group j in quarter t
#          (now computed over ALL workers with positive total income,
#           not just salaried workers — informal share is therefore larger)
#
# KEY ANALYTICAL DIFFERENCES vs Step 3 (salary decomposition):
#
#   1. Sample is broader: ~60% of informal workers excluded from the salary
#      decomposition (zero salary) are now included here. Informal employment
#      share s_j therefore increases, giving the informal sector more weight
#      in the within component.
#
#   2. Within_informal will likely be HIGHER here than in Step 3 because
#      total income for informal workers includes both low-earning own-account
#      workers AND higher-earning self-employed entrepreneurs — a wider spread.
#
#   3. Between component may change as the mean earnings gap between formal
#      and informal shifts when self-employment income is included.
#
# Reads:
#   design_total_ineq   (built in Step 10 — must run Step 10 first)
#   desc_variance_total_overall   (from Step 10)
#
# Outputs -> config$paths$processed_data:
#   desc_variance_total_decomp.rds
#
#===============================================================================

cat("\n[11] Variance decomposition — total income...\n")

# ── Mean log total income by quarter x status --------------------------------
mean_j_total <- svyby(
  ~log_total, ~year_quarter + Employment_Status,
  design  = design_total_ineq,
  FUN     = svymean,
  na.rm   = TRUE, vartype = NULL
) %>%
  as_tibble() %>%
  rename(time = year_quarter, mean_j = log_total)

# ── Variance log total income by quarter x status ----------------------------
var_j_total <- svyby(
  ~log_total, ~year_quarter + Employment_Status,
  design  = design_total_ineq,
  FUN     = svyvar,
  na.rm   = TRUE, vartype = NULL
) %>%
  as_tibble() %>%
  rename(time = year_quarter, var_j = log_total)

# ── Employment share s_jt: weighted count within each quarter ----------------
emp_j_total <- svyby(
  ~one, ~year_quarter + Employment_Status,
  design  = update(design_total_ineq, one = 1L),
  FUN     = svytotal,
  na.rm   = TRUE, vartype = NULL
) %>%
  as_tibble() %>%
  rename(time = year_quarter, emp_count = one) %>%
  group_by(time) %>%
  mutate(s_j = emp_count / sum(emp_count, na.rm = TRUE)) %>%
  ungroup()

# ── Overall mean log total income by quarter ---------------------------------
mean_all_total <- svyby(
  ~log_total, ~year_quarter,
  design  = design_total_ineq,
  FUN     = svymean,
  na.rm   = TRUE, vartype = NULL
) %>%
  as_tibble() %>%
  rename(time = year_quarter, mean_all = log_total)

# ── Assemble decomposition ---------------------------------------------------
desc_variance_total_decomp <- mean_j_total %>%
  left_join(var_j_total,  by = c("time", "Employment_Status")) %>%
  left_join(emp_j_total %>% select(time, Employment_Status, s_j),
            by = c("time", "Employment_Status")) %>%
  left_join(mean_all_total, by = "time") %>%
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
    desc_variance_total_overall %>%
      select(time, variance) %>%
      rename(total_var = variance),
    by = "time"
  ) %>%
  mutate(discrepancy = abs(total_var - within - between))

cat(sprintf("  Max decomp discrepancy (total income): %.6f\n",
            max(desc_variance_total_decomp$discrepancy, na.rm = TRUE)))

# ── Sanity comparison: between component salary vs total ---------------------
# A larger between component in the total income decomposition would mean
# the formal/informal earnings gap is wider when self-employment income is
# included — i.e. informal total earnings are further below formal than
# salary alone suggests.
between_salary <- mean(desc_variance_decomp$between,      na.rm = TRUE)
between_total  <- mean(desc_variance_total_decomp$between, na.rm = TRUE)
within_salary  <- mean(desc_variance_decomp$within,       na.rm = TRUE)
within_total   <- mean(desc_variance_total_decomp$within,  na.rm = TRUE)

cat(sprintf(
  "  Average between component — salary: %.4f | total: %.4f\n",
  between_salary, between_total
))
cat(sprintf(
  "  Average within component  — salary: %.4f | total: %.4f\n",
  within_salary, within_total
))
cat("  done\n")

# ── Save --------------------------------------------------------------------
saveRDS(desc_variance_total_decomp,
        file.path(config$paths$processed_data,
                  "desc_variance_total_decomp.rds"))
cat("  Saved: desc_variance_total_decomp.rds\n")
cat("\n=== Step 11 complete ===\n")
