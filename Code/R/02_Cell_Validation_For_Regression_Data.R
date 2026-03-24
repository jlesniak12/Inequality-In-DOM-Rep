# ==============================================================================
# 02_cell_validation.R
# CELL SIZE AND RELIABILITY VALIDATION
#
# Checks reliability of survey-weighted estimates at sector × firmsize × quarter
# for all four regression outcomes:
#   1. log_wage          — log of mean real wage (continuous, mean estimator)
#   2. log_50_10         — log(p50/p10) wage ratio (quantile ratio, most noise-sensitive)
#   3. below_min         — share earning below minimum wage (proportion)
#   4. informal          — share of informal workers (proportion)
#
# Each outcome has different reliability concerns:
#   - Means (log_wage):     n >= 30 adequate; CV straightforward
#   - Quantile ratios:      p10 drives noise; need n >= 50; CV from linearisation
#   - Proportions:          n >= 30 for shares; near-zero shares need separate treatment
#
# Three-part validation per outcome:
#   Part A — Unweighted cell counts (raw n screen)
#   Part B — Coefficient of Variation on the survey estimate (precision screen)
#   Part C — Temporal stability (noise vs. real variation screen)
#
# Final decision table consolidates all checks, flags cells by outcome.
# ==============================================================================

source("Code/R/00_setup.R")


# ==============================================================================
# 0. LOAD DATA
# ==============================================================================


# Load pre-built objects saved by 01_build_panels.R
panel_sf                  <- readRDS(file.path(config$paths$processed_data, "panel_sector_firmsize_quarter.rds"))
panel_emp_privsec_wagegrp <- readRDS(file.path(config$paths$processed_data, "panel_emp_privsec_wagegrp.rds"))
design                    <- readRDS(file.path(config$paths$processed_data, "design_restrictive.rds"))



#==============================================================================
  # 1. DEFINE OUTCOME METADATA
  #
  # Each outcome entry specifies:
  #   var        — column name in panel_sf (used for stability check)
  #   label      — human-readable name for tables
  #   estimator  — "mean", "quantile_ratio", or "proportion"
  #   n_min      — minimum acceptable unweighted n for this estimator type
  #   n_warn     — borderline n (reliable but flagged)
  #   cv_caution — CV threshold for caution flag
  #   cv_drop    — CV threshold for drop flag
  #   near_zero_threshold — for proportions: below this mean level, flag as
  #                structural near-zero rather than noisy (NULL for non-proportions)
  # ==============================================================================

#thresholds for percent of quarters to flag
thin_cut <- .20
cv_cut <- .20

#threshold for ratio of SD/mean to flag
temp_cut <- 1.0

OUTCOMES <- list(
  
  log_wage = list(
    var               = "log_10",          # log(p10) used for CV; log_wage mean for stability
    stability_var     = "log_50",          # more stable than p10 for trend check
    label             = "Log wage (mean)",
    estimator         = "mean",
    n_min             = 30,
    n_warn            = 50,
    cv_caution        = 0.165,
    cv_drop           = 0.33,
    near_zero         = NULL
  ),
  
  log_50_10 = list(
    var               = "log_50_10",       # log(p50/p10) — ratio of two quantiles
    stability_var     = "log_50_10",
    label             = "Log(p50/p10)",
    estimator         = "quantile_ratio",
    n_min             = 50,               # stricter: p10 from <50 obs is unreliable
    n_warn            = 75,
    cv_caution        = 0.165,
    cv_drop           = 0.33,
    near_zero         = NULL
  ),
  
  below_min = list(
    var               = "below_min",
    stability_var     = "below_min",
    label             = "Share below min wage",
    estimator         = "proportion",
    n_min             = 30,
    n_warn            = 50,
    cv_caution        = 0.165,
    cv_drop           = 0.33,
    near_zero         = 0.03             # < 3% compliance failure = structural near-zero
  ),
  
  informal = list(
    var               = "informal",
    stability_var     = "informal",
    label             = "Share informal",
    estimator         = "proportion",
    n_min             = 30,
    n_warn            = 50,
    cv_caution        = 0.165,
    cv_drop           = 0.33,
    near_zero         = 0.05             # < 5% informality = structural near-zero
  )
)

# COVID quarters to exclude from temporal stability calculation
# (furlough recording distorts all outcomes mechanically)
COVID_QUARTERS <- c("2020Q1", "2020Q2", "2020Q3", "2020Q4", "2021Q1", "2021Q2")


# ==============================================================================
# 2. PART A — UNWEIGHTED CELL COUNTS
#
# Raw n of ENCFT respondents per sector × firmsize × quarter cell.
# Computed from the microdata (panel_emp_privsec_wagegrp), not the panel.
# This is the most fundamental screen — no weighting fixes a thin cell.
#
# NOTE: n_min differs by outcome (30 for means/proportions, 50 for quantiles).
# We compute counts once, then apply outcome-specific thresholds in Part C.
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("PART A: UNWEIGHTED CELL COUNTS\n")
cat(strrep("=", 70), "\n\n")

# Raw counts per cell × quarter
cell_counts_sfq <- panel_emp_privsec_wagegrp %>%
  group_by(Employment_Sector, Wage_group, year_quarter) %>%
  summarise(n_unweighted = n(), .groups = "drop") %>%
  rename(time = year_quarter)

# Summary across quarters per cell (min/median/max)
cell_counts_summary <- cell_counts_sfq %>%
  group_by(Employment_Sector, Wage_group) %>%
  summarise(
    n_quarters  = n(),
    n_min       = min(n_unweighted),
    n_median    = median(n_unweighted),
    n_max       = max(n_unweighted),
    .groups     = "drop"
  )

# Apply outcome-specific n thresholds and flag
# Result: one row per cell × outcome with count-based flags
count_flags <- map_dfr(names(OUTCOMES), function(out_name) {
  meta <- OUTCOMES[[out_name]]
  cell_counts_summary %>%
    mutate(
      outcome      = out_name,
      n_min_thresh = meta$n_min,
      n_warn_thresh = meta$n_warn,
      # Share of quarters failing the outcome-specific minimum
      n_below_min  = map2_int(Employment_Sector, Wage_group, function(s, w) {
        cell_counts_sfq %>%
          filter(Employment_Sector == s, Wage_group == w) %>%
          summarise(n = sum(n_unweighted < meta$n_min)) %>%
          pull(n)
      }),
      pct_below_min = n_below_min / n_quarters,
      # Separate borderline flag (between n_min and n_warn)
      n_borderline  = map2_int(Employment_Sector, Wage_group, function(s, w) {
        cell_counts_sfq %>%
          filter(Employment_Sector == s, Wage_group == w) %>%
          summarise(n = sum(n_unweighted >= meta$n_min &
                              n_unweighted < meta$n_warn)) %>%
          pull(n)
      }),
      pct_borderline = n_borderline / n_quarters,
      # Flag: chronically thin = > % of quarters below minimum
      flag_count_thin = pct_below_min > thin_cut
    )
}) %>%
  select(outcome, Employment_Sector, Wage_group,
         n_min, n_median, n_max, n_quarters,
         n_min_thresh, n_below_min, pct_below_min,
         n_borderline, pct_borderline,
         flag_count_thin)

cat("Count flags summary by outcome:\n")
count_flags %>%
  group_by(outcome) %>%
  summarise(
    n_cells        = n(),
    n_thin         = sum(flag_count_thin),
    pct_thin       = scales::percent(mean(flag_count_thin), accuracy = 1),
    .groups = "drop"
  ) %>%
  print()

cat("Count flags summary by firm size:\n")
count_flags %>%
  group_by(Wage_group) %>%
  summarise(
    n_cells        = n(),
    n_thin         = sum(flag_count_thin),
    pct_thin       = scales::percent(mean(flag_count_thin), accuracy = 1),
    .groups = "drop"
  ) %>%
  print()


cat("Count flags summary by Sector:\n")
count_flags %>%
  group_by(Employment_Sector) %>%
  summarise(
    n_cells        = n(),
    n_thin         = sum(flag_count_thin),
    pct_thin       = scales::percent(mean(flag_count_thin), accuracy = 1),
    .groups = "drop"
  ) %>%
  print()


cat("Count flags summary by Sector and Firm Size:\n")
sec_size_tab <- count_flags %>%
  group_by(Employment_Sector, Wage_group) %>%
  summarise(
    n_cells        = n(),
    n_thin         = sum(flag_count_thin),
    pct_thin       = scales::percent(mean(flag_count_thin), accuracy = 1),
    .groups = "drop"
  ) %>%
  arrange(Employment_Sector, Wage_group) %>%
  print()

plot <- sec_size_tab %>%
  ggplot(aes(x = Employment_Sector, y = n_thin)) +
  geom_bar(stat = "identity")

#table showing how many outcomes are thin in sector x firm size
count_flags %>%
  group_by(Employment_Sector, Wage_group) %>%
  summarise(n_thin = sum(flag_count_thin), .groups = "drop") %>%
  pivot_wider(names_from = Wage_group, values_from = n_thin) %>%
  arrange(Employment_Sector)


count_flags %>%
  group_by(Employment_Sector, Wage_group) %>%
  summarise(n_thin = sum(flag_count_thin), .groups = "drop") %>%
  filter(n_thin == 1) %>%
  left_join(
    count_flags %>% 
      filter(flag_count_thin) %>%
      select(Employment_Sector, Wage_group, outcome),
    by = c("Employment_Sector", "Wage_group")
  )

cat("\nCells flagged as chronically thin (>20% quarters below outcome-specific n_min):\n")
count_flags %>%
  filter(flag_count_thin) %>%
  select(outcome, Employment_Sector, Wage_group,
         n_min_thresh, n_min, n_median, pct_below_min) %>%
  arrange(outcome, pct_below_min) %>%
  print(n = Inf)



# ==============================================================================
# 3. PART B — COEFFICIENT OF VARIATION
#
# CV = SE / estimate from the survey design, accounts for clustering/stratification.
# Computed for each outcome from the panel_sf values using the survey design.
#
# IMPORTANT NOTES BY ESTIMATOR TYPE:
#
# log_wage (mean): 
#   Use svymean on log_wage. CV = se / mean. Straightforward.
#
# log_50_10 (quantile ratio):
#   svyquantile gives linearised SEs for individual quantiles.
#   We approximate CV of the ratio using delta method:
#     CV(p50/p10) ≈ sqrt(CV(p50)^2 + CV(p10)^2)
#   This is a conservative upper bound. p10 almost always dominates.
#
# below_min / informal (proportions):
#   Use svymean on the 0/1 indicator. CV = se / estimate.
#   WARNING: near-zero proportions produce enormous CVs even with good precision.
#   Separate near_zero flag handles this (see Part D).
#
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("PART B: COEFFICIENT OF VARIATION\n")
cat(strrep("=", 70), "\n\n")

# Rebuild survey design for CV computation
# (needs group interaction from 01_build_panels.R — assumed in memory)
design_cv <- update(design,
                    group = interaction(Employment_Sector, Wage_group, sep = "__"))

# ---- B1: CV for log_wage (mean of log wages) ----
cat("Computing CV: log_wage (mean)...\n")

cv_logwage <- svyby(
  ~log_wage,
  ~year_quarter + group,
  design  = design_cv,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = "cv"
) %>%
  as_tibble() %>%
  rename(time = year_quarter, est = log_wage) %>%
  separate(group, into = c("Employment_Sector", "Wage_group"), sep = "__") %>%
  mutate(outcome = "log_wage")


# ---- B2: CV for log_50_10 (quantile ratio via delta method) ----
cat("Computing CV: log_50_10 (quantile ratio, delta method)...\n")

# Get SE for p50 and p10 separately via linearisation
cv_p50 <- svyby(
  ~real_salary_income_primary,
  ~year_quarter + group,
  design  = design_cv,
  FUN     = svyquantile,
  quantiles = 0.50,
  na.rm   = TRUE,
  vartype = "se",
  keep.var = TRUE
) %>%
  as_tibble() %>%
  rename(time = year_quarter,
         p50_est = real_salary_income_primary,
         p50_se  = se.real_salary_income_primary) %>%
  separate(group, into = c("Employment_Sector", "Wage_group"), sep = "__")

cv_p10 <- svyby(
  ~real_salary_income_primary,
  ~year_quarter + group,
  design  = design_cv,
  FUN     = svyquantile,
  quantiles = 0.10,
  na.rm   = TRUE,
  vartype = "se",
  keep.var = TRUE
) %>%
  as_tibble() %>%
  rename(time = year_quarter,
         p10_est = real_salary_income_primary,
         p10_se  = se.real_salary_income_primary) %>%
  separate(group, into = c("Employment_Sector", "Wage_group"), sep = "__")

# Delta method: CV(log p50 - log p10) ≈ sqrt((se_p50/p50)^2 + (se_p10/p10)^2)
# This is the CV of the ratio, not the log difference — close approximation
cv_log5010 <- cv_p50 %>%
  left_join(cv_p10, by = c("time", "Employment_Sector", "Wage_group")) %>%
  mutate(
    cv_p50    = p50_se / p50_est,
    cv_p10    = p10_se / p10_est,
    # Delta method CV of ratio — p10 dominates because it's smaller and noisier
    cv        = sqrt(cv_p50^2 + cv_p10^2),
    est       = log(p50_est) - log(p10_est),   # the actual outcome value
    outcome   = "log_50_10"
  ) %>%
  select(time, Employment_Sector, Wage_group, est, cv, outcome)

# ---- B3: CV for below_min (proportion) ----
cat("Computing CV: below_min (proportion)...\n")

cv_belowmin <- svyby(
  ~below_min,
  ~year_quarter + group,
  design  = design_cv,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = "cv"
) %>%
  as_tibble() %>%
  rename(time = year_quarter, est = below_min) %>%
  separate(group, into = c("Employment_Sector", "Wage_group"), sep = "__") %>%
  mutate(outcome = "below_min")

# ---- B4: CV for informal (proportion) ----
cat("Computing CV: informal (proportion)...\n")

cv_informal <- svyby(
  ~informal,
  ~year_quarter + group,
  design  = design_cv,
  FUN     = svymean,
  na.rm   = TRUE,
  vartype = "cv"
) %>%
  as_tibble() %>%
  rename(time = year_quarter, est = informal) %>%
  separate(group, into = c("Employment_Sector", "Wage_group"), sep = "__") %>%
  mutate(outcome = "informal")

# ---- Combine all CV results ----
cv_all <- bind_rows(cv_logwage, cv_log5010, cv_belowmin, cv_informal)


# ---- Summarise CV by cell × outcome ----
cv_summary <- cv_all %>%
  group_by(outcome, Employment_Sector, Wage_group) %>%
  summarise(
    cv_median      = median(cv, na.rm = TRUE),
    cv_p75         = quantile(cv, 0.75, na.rm = TRUE),
    cv_max         = max(cv[is.finite(cv)], na.rm = TRUE),  # exclude -Inf/Inf
    n_cv_caution   = sum(cv > map_dbl(outcome, ~ OUTCOMES[[.x]]$cv_caution), na.rm = TRUE),
    n_cv_drop      = sum(cv > map_dbl(outcome, ~ OUTCOMES[[.x]]$cv_drop),    na.rm = TRUE),
    pct_cv_drop    = mean(cv > map_dbl(outcome, ~ OUTCOMES[[.x]]$cv_drop),   na.rm = TRUE),
    n_valid        = sum(is.finite(cv)),   # add this — tells you how many quarters had valid CV
    .groups = "drop"
  ) %>%
  mutate(flag_cv_unreliable = if_else(
    n_valid == 0, NA,                     
    pct_cv_drop > cv_cut
  ))


cat("\nCV flags summary by outcome:\n")
cv_summary %>%
  group_by(outcome) %>%
  summarise(
    n_cells      = n(),
    n_valid_cells = sum(!is.na(flag_cv_unreliable)),  # cells with any valid CV data
    n_flagged    = sum(flag_cv_unreliable, na.rm = TRUE),
    pct_flagged  = scales::percent(
      sum(flag_cv_unreliable, na.rm = TRUE) / sum(!is.na(flag_cv_unreliable)), 
      accuracy = 1
    ),
    median_cv    = round(median(cv_median, na.rm = TRUE), 3),
    .groups = "drop"
    ) %>%
  print()

  
cat("\nCells flagged for high CV (>20% quarters exceeding cv_drop threshold):\n")
cv_summary %>%
  filter(flag_cv_unreliable) %>%
  select(outcome, Employment_Sector, Wage_group,
         cv_median, cv_max, pct_cv_drop) %>%
  arrange(outcome, desc(pct_cv_drop)) %>%
  print(n = Inf)


# ==============================================================================
# 4. PART C — NEAR-ZERO SCREEN (proportions only)
#
# High CV for a proportion can mean two different things:
#   (a) Noisy cell — true value is meaningful but imprecisely estimated (BAD)
#   (b) Structural near-zero — true value is genuinely near zero, estimated
#       with good absolute precision but high relative precision (NOT BAD,
#       just means this outcome is not relevant for this cell)
#
# Example: Finance Small has 2% informality every quarter, SE = 0.8%.
#   CV = 0.8/2 = 40% — looks "unreliable" but is actually well-measured.
#   The cell should be EXCLUDED from the informality regression because there
#   is no meaningful informal margin, not because the data is noisy.
#
# We flag near-zero cells separately using mean estimate across all quarters.
# These cells get a different recommendation: "Out of scope" not "Drop/Noisy".
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("PART C: NEAR-ZERO SCREEN (proportions)\n")
cat(strrep("=", 70), "\n\n")

near_zero_flags <- cv_all %>%
  filter(outcome %in% c("below_min", "informal")) %>%
  group_by(outcome, Employment_Sector, Wage_group) %>%
  summarise(
    mean_est        = mean(est, na.rm = TRUE),
    median_est      = median(est, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    near_zero_thresh = map_dbl(outcome, ~ OUTCOMES[[.x]]$near_zero),
    flag_near_zero   = median_est < near_zero_thresh
  )

cat("Near-zero flags by outcome:\n")
near_zero_flags %>%
  group_by(outcome) %>%
  summarise(
    n_cells     = n(),
    n_near_zero = sum(flag_near_zero),
    .groups = "drop"
  ) %>%
  print()

cat("\nCells flagged as structural near-zero:\n")
near_zero_flags %>%
  filter(flag_near_zero) %>%
  select(outcome, Employment_Sector, Wage_group,
         mean_est, median_est, near_zero_thresh) %>%
  arrange(outcome, median_est) %>%
  mutate(across(c(mean_est, median_est, near_zero_thresh),
                ~ scales::percent(.x, accuracy = 0.1))) %>%
  print(n = Inf)



cv_summary %>%
  filter(outcome == "informal", flag_cv_unreliable == TRUE) %>%
  left_join(
    near_zero_flags %>% 
      filter(outcome == "informal") %>%
      select(Employment_Sector, Wage_group, 
             flag_near_zero, median_est),
    by = c("Employment_Sector", "Wage_group")
  ) %>%
  filter(!flag_near_zero) %>%
  select(Employment_Sector, Wage_group, 
         cv_median, pct_cv_drop, median_est) %>%
  arrange(desc(cv_median))


# ==============================================================================
# 5. PART D — TEMPORAL STABILITY
#
# Measures how much each cell's outcome bounces quarter-to-quarter.
# High time-series volatility = sampling noise, not real economic variation.
#
# Metric: time-series CV = sd(outcome over time) / mean(outcome over time)
#   For means and ratios, values 0.1–0.4 suggest real variation; >1.0 suspicious.
#   For proportions same logic applies.
#
# COVID quarters EXCLUDED from stability calculation (2020Q1–2021Q2).
# Including them inflates volatility for all cells due to furlough recording.
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("PART D: TEMPORAL STABILITY\n")
cat(strrep("=", 70), "\n\n")

stability_all <- map_dfr(names(OUTCOMES), function(out_name) {
  meta    <- OUTCOMES[[out_name]]
  stab_var <- meta$stability_var
  
  panel_sf %>%
    filter(!time %in% COVID_QUARTERS) %>%        # exclude COVID quarters
    filter(!is.na(.data[[stab_var]])) %>%
    group_by(Employment_Sector, Wage_group) %>%
    summarise(
      ts_mean      = mean(.data[[stab_var]], na.rm = TRUE),
      ts_sd        = sd(.data[[stab_var]],   na.rm = TRUE),
      ts_iqr       = IQR(.data[[stab_var]],  na.rm = TRUE),
      ts_cv        = ts_sd / abs(ts_mean),          # abs() for log values that could be negative
      n_obs        = sum(!is.na(.data[[stab_var]])),
      .groups = "drop"
    ) %>%
    mutate(
      outcome          = out_name,
      flag_unstable    = ts_cv > temp_cut   # sd > mean = very noisy
    )
})

stability_all <- stability_all %>%
  left_join(
    near_zero_flags %>%
      select(outcome, Employment_Sector, Wage_group, flag_near_zero),
    by = c("outcome", "Employment_Sector", "Wage_group")
  ) %>%
  mutate(
    flag_near_zero = replace_na(flag_near_zero, FALSE),
    # Override instability flag for near-zero cells — instability is
    # mathematical artifact of near-zero denominator, not real noise
    flag_unstable = case_when(
      flag_near_zero ~ NA,
      TRUE           ~ flag_unstable
    )
  )



cat("Stability flags by outcome:\n")
stability_all %>%
  group_by(outcome) %>%
  summarise(
    n_cells    = n(),
    n_unstable = sum(flag_unstable, na.rm = TRUE),
    median_tscv = round(median(ts_cv, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  print()

cat("\nCells flagged as temporally unstable (ts_cv > 1.0):\n")
stability_all %>%
  filter(flag_unstable) %>%
  select(outcome, Employment_Sector, Wage_group,
         ts_mean, ts_sd, ts_cv, n_obs) %>%
  arrange(outcome, desc(ts_cv)) %>%
  print(n = Inf)

#check stability for informal without 0 or near 0
stability_all %>%
  filter(outcome == "informal", !flag_near_zero) %>%
  summarise(
    median_tscv_real = round(median(ts_cv, na.rm = TRUE), 3),
    n_cells          = n()
  )



# ==============================================================================
# 6. FINAL DECISION TABLE
#
# Consolidates all four checks into one recommendation per cell × outcome.
#
# Scoring logic:
#   flag_count_thin      — chronically below n_min (>20% quarters)     = 1 point
#   flag_cv_unreliable   — chronically high CV (>20% quarters)         = 1 point
#   flag_unstable        — time-series CV > 1.0 (excl. COVID)          = 1 point
#   flag_near_zero       — structural near-zero (proportions only)      = separate track
#
# Recommendation:
#   near-zero                        → "Out of scope"  (not a reliability failure)
#   flag_count >= 2                  → "Drop — multiple reliability failures"
#   flag_count == 1                  → "Review — one reliability concern"
#   borderline n in >30% of quarters → "Caution — thin but usable"
#   else                             → "Keep"
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("FINAL DECISION TABLE\n")
cat(strrep("=", 70), "\n\n")

decision_table <- count_flags %>%
  left_join(
    cv_summary %>%
      select(outcome, Employment_Sector, Wage_group,
             cv_median, pct_cv_drop, flag_cv_unreliable),
    by = c("outcome", "Employment_Sector", "Wage_group")
  ) %>%
  left_join(
    stability_all %>%
      select(outcome, Employment_Sector, Wage_group,
             ts_cv, n_obs, flag_unstable),
    by = c("outcome", "Employment_Sector", "Wage_group")
  ) %>%
  # Near-zero only relevant for proportions; fill FALSE for others
  left_join(
    near_zero_flags %>%
      select(outcome, Employment_Sector, Wage_group,
             median_est, flag_near_zero),
    by = c("outcome", "Employment_Sector", "Wage_group")
  ) %>%
  mutate(
    flag_near_zero = replace_na(flag_near_zero, FALSE),
    
    # Total reliability failure count (near-zero does NOT add to this — it's separate)
    n_flags = as.integer(coalesce(flag_count_thin, FALSE)) +
      as.integer(coalesce(flag_cv_unreliable, FALSE)) +
      as.integer(coalesce(flag_unstable, FALSE)),
    
    recommendation = case_when(
      flag_near_zero                         ~ "Out of scope — structural near-zero",
      pct_below_min >= 0.75                  ~ "Drop — majority of quarters below minimum n",
      n_flags >= 2                           ~ "Drop — multiple reliability failures",
      n_flags == 1 & pct_borderline > 0.30   ~ "Drop — thin + borderline concern",
      n_flags == 1                           ~ "Review — one reliability concern",
      pct_borderline > 0.30                  ~ "Caution — borderline n in many quarters",
      TRUE                                   ~ "Keep"
    )
  ) %>%
  arrange(outcome, desc(n_flags), desc(pct_below_min))


# ---- Print full decision table ----
cat("Full decision table (all cells × outcomes):\n\n")
decision_table %>%
  select(
    outcome, Employment_Sector, Wage_group,
    n_median, pct_below_min,
    cv_median, pct_cv_drop,
    ts_cv,
    n_flags, flag_near_zero,
    recommendation
  ) %>%
  mutate(
    pct_below_min = scales::percent(pct_below_min, accuracy = 1),
    pct_cv_drop   = scales::percent(pct_cv_drop,   accuracy = 1),
    cv_median     = round(cv_median, 3),
    ts_cv         = round(ts_cv, 2)
  ) %>%
  print(n = Inf)

# ---- Summary: cells kept per outcome ----
cat("\n=== KEEP/DROP SUMMARY BY OUTCOME ===\n")

x0 <- decision_table %>%
  group_by(outcome, recommendation) %>%
  summarise(n_cells = n(), .groups = "drop") %>%
  pivot_wider(names_from = recommendation, values_from = n_cells, values_fill = 0) %>%
  print()

# ---- Cells cleared to use in regression (per outcome) ----
cat("\n=== CELLS CLEARED FOR REGRESSION (Keep + Review) ===\n")
regression_cells <- decision_table %>%
  filter(str_detect(recommendation, "Keep|Review")) %>%
  select(outcome, Employment_Sector, Wage_group, recommendation, cv_median, n_median)

x1 <- regression_cells %>%
  group_by(outcome) %>%
  summarise(
    n_cells = n(),
    cells   = paste(paste0(Employment_Sector, " (", Wage_group, ")"), collapse = ", "),
    .groups = "drop"
  ) %>%
  print(width = Inf)


# ==============================================================================
# 7. CROSS-OUTCOME MATRIX VIEW
#
# Wide table showing recommendation for each cell across all four outcomes.
# Most useful for understanding which cells survive for which outcomes.
# This becomes your Data Appendix Table.
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("CROSS-OUTCOME MATRIX\n")
cat(strrep("=", 70), "\n\n")

# Compact recommendation codes for matrix display
recode_rec <- function(x) case_when(
  str_detect(x, "Keep")        ~ "KEEP",
  str_detect(x, "Review")      ~ "REVIEW",
  str_detect(x, "Caution")     ~ "CAUTION",
  str_detect(x, "Out of scope") ~ "SCOPE",
  str_detect(x, "Drop")        ~ "DROP",
  TRUE                          ~ "?"
)

cross_matrix <- decision_table %>%
  mutate(rec_code = recode_rec(recommendation)) %>%
  select(Employment_Sector, Wage_group, outcome, rec_code) %>%
  pivot_wider(
    id_cols     = c(Employment_Sector, Wage_group),
    names_from  = outcome,
    values_from = rec_code
  ) %>%
  arrange(Employment_Sector, Wage_group)

cat("Recommendation matrix (KEEP/REVIEW/CAUTION/SCOPE/DROP per outcome):\n\n")
print(cross_matrix, n = Inf)

# Also show median n alongside for context
cross_n <- cell_counts_summary %>%
  select(Employment_Sector, Wage_group, n_median)

cross_matrix_with_n <- cross_matrix %>%
  left_join(cross_n, by = c("Employment_Sector", "Wage_group")) %>%
  relocate(n_median, .after = Wage_group)

cat("\nSame matrix with median quarterly n:\n\n")
print(cross_matrix_with_n, n = Inf)


# ==============================================================================
# 8. SAVE OUTPUTS
# ==============================================================================

saveRDS(cell_counts_sfq,
        file.path(config$paths$processed_data, "validation_cell_counts_sfq.rds"))

saveRDS(cv_all,
        file.path(config$paths$processed_data, "validation_cv_all_outcomes.rds"))

saveRDS(decision_table,
        file.path(config$paths$processed_data, "validation_decision_table.rds"))

saveRDS(regression_cells,
        file.path(config$paths$processed_data, "validation_regression_cells.rds"))

write_csv(cross_matrix_with_n,
          file.path(config$paths$processed_data, "validation_cross_outcome_matrix.csv"))

cat("\nAll validation outputs saved.\n")
cat("Key file for regression setup: validation_regression_cells.rds\n")
cat("Key file for data appendix:    validation_cross_outcome_matrix.csv\n")
