#===============================================================================
#
# Scope: This file takes in panels of survey data prepared in previous script
#        and conducts initial processing to create panels suitable for running
#        event study regression analysis.
# steps: 
#       1) Load in all data and remove variables not used for regressions.
#       2) Set up survey designs with critical sample filters for regressions.
#       3) Calculate exposure to minimum wage, key variable in defining
#          treatment groups.
#       4) Generate a Regression Panel at Sector x Firm Size x Quarter Level.
#
#
#===============================================================================



source("Code/R/00_setup.R")



#===============================================================================
# STEP 1. Load Data and Remove Extra Variables
#===============================================================================


Full_ENCFT_clean <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))


# -- simplify data keeping only needed vars -- #

srvy_vars <- c("psu_unique", "strata_unique", "weight_annual", "weight_quarter", "FACTOR_EXPANSION", "DES_ESTRATO",
               "quarter", "month", "year", "year_quarter")

analysis_vars <- c(
  # -core income and min wage
  
  "real_salary_income_primary",
  "Wage_group",             #defines firm size based on official buckets of min wage (micro small med large)
  "real_minwage_harmonized",
  
  #  -other traits
  
  "Employment_Sector",      #sector of work
  "Employment_Status",      #formal vs informal
  "Employment_Type",
  "OCUPADO",                 #binary for currently employed
  
  "year",
  "quarter"
)

analysis_df <- Full_ENCFT_clean %>%
  select(
    all_of(srvy_vars),
    all_of(analysis_vars)
  )



#===============================================================================
# STEP 2. Set Basic Designs
# 
# NOTES:
# We filter out government because they have a different minimum wage.
#
# Electricity and water are removed because of sparsity of data in several 
# quarter X  firm size cells. There are not many private workers in the survey.
#
# Sample is restricted to private sector employees to remove self employed and
# business owner. Even though this is a large share of employment in Dom Rep it
# is necessary because the minimum wage is not necessarily binding for these
# types of workers
#
# ==============================================================================

# -- Define samples of analysis and designs ---- #

#restrictive design with every employee matched to firm size for regression
panel_emp_privsec_wagegrp <- analysis_df %>%
  filter(OCUPADO == 1) %>%
  filter(!(Employment_Sector == "Government")) %>%
  filter(!(Employment_Sector == "Electricity and Water")) %>%
  filter(Employment_Type == "private employee") %>%
  filter(real_salary_income_primary > 0) %>%
  filter(!is.na(Wage_group)) %>%
  filter(!(Wage_group == "Dont Know"))

design <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = panel_emp_privsec_wagegrp,
  nest    = TRUE
)

#Broad sample design for variance decomposition
#allow for missing/dont know wage groups

panel_emp_privsec <- analysis_df %>%
  filter(OCUPADO == 1) %>%
  filter(!(Employment_Sector == "Government")) %>%
  filter(!(Employment_Sector == "Electricity and Water")) %>%
  filter(Employment_Type == "private employee") %>%
  filter(real_salary_income_primary > 0)

design_broad <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = panel_emp_privsec %>% 
    filter(real_salary_income_primary > 0),
  nest    = TRUE
)


# -- Add some variables needed for outcomes 
design <- update(design,
                 log_wage = log(real_salary_income_primary),
                 below_min = as.integer(
                   real_salary_income_primary < real_minwage_harmonized
                 ),
                 informal = as.integer(Employment_Status == "Informal")
)

#group variable to be sector x firm size
design <- update(design,
                 group = interaction(Employment_Sector, Wage_group, sep = "__"))

design_broad <- update(design_broad,
                       log_wage = log(real_salary_income_primary),
                       below_min = as.integer(
                         real_salary_income_primary < real_minwage_harmonized
                       ),
                       informal = as.integer(Employment_Status == "Informal")
)

#===============================================================================
# STEP 3. Generate Baseline Minimum Wage Exposure Fixed in Time
# NOTES:
# We choose to use 2016 annual average by sector fixed over time. This choice is
# driven by: 
#          1) seasonality concerns so we wanted to use a full year of data as
#             as baseline.
#          2) The fact that there was a min wage increse in 2015Q2 but our data
#             starts in 2014Q3 Setting the baseline before this point would mean
#             there would be very limited pre treatment data for this increase.
#             Therefore We decided to incorporate the 2015 increase into
#             the baseline and use the 2017Q2 increase as our first treatment of
#             the study.
#
# We calculate the exposure for formal sector workers only even though our sample
# includes some informal workers because the min wage is not legally binding in 
# informal sector. We use these informal workers later to attempt to study the
# impact of the minimum wage changes in the formal sector on the share of workers
# that work in the formal sector within that economic sector.
#===============================================================================



# -- define the baseline period for exposure calculations
baseline_df <- panel_emp_privsec_wagegrp %>%
  filter(Employment_Status == "Formal") %>%
  filter((year == 2016)) %>%
  mutate(baseline_dummy = "2016")



# --- Step 1: Calculate share of workers near min wage at sector X firm size X quarter
# This number is used directly for exposure in sector x firm size x quarter panel
# It is also used to aggregate up as a weighted average for sector x quarter panel

near_mw_baseline <- near_mw_share(
  df          = baseline_df,
  time_var    = "baseline_dummy",
  time_subset = NULL,
  by_vars     = c("Employment_Sector", "Wage_group"),
  min_wage = "real_minwage_harmonized",
  income = "real_salary_income_primary",
  out_col = "near_min",
  mw_lower    = 1,
  mw_upper    = 1.1,
  formal_only = TRUE
)


# --- Step 2: Firm size employment shares within sector ---
# Used for aggregation to sector level only
pi_baseline <- firmsize_pi(
  df          = baseline_df,
  time_var    = "baseline_dummy",
  time_subset = NULL,
  by_vars     = c("Employment_Sector", "Wage_group"),
  size_var    = "Wage_group",
  formal_only = TRUE
)



# --- Step 3a: SECTOR × FIRMSIZE exposure (disaggregated) ---
# Keep near_min directly — no aggregation
# Add pi weights for later use as regression weights or for aggregation

exposure_sf_baseline <- near_mw_baseline %>%
  left_join(
    pi_baseline %>% 
      select(Employment_Sector, Wage_group, pi),
    by = c("Employment_Sector", "Wage_group")
  ) %>%
  rename(exposure_sf_val = near_min) %>%
  select(Employment_Sector, Wage_group, exposure_sf_val, pi) %>%
  # Add tercile WITHIN firm size tier — ranks sectors within each tier
  group_by(Wage_group) %>%
  mutate(
    tercile_within_tier = ntile(exposure_sf_val, 3),
    exposure_group_within_tier = factor(
      c("Low exposure", "Medium exposure", "High exposure")[tercile_within_tier],
      levels = c("Low exposure", "Medium exposure", "High exposure")
    )
  ) %>%
  ungroup() %>%
  # Also add overall tercile across all sector × firmsize cells
  mutate(
    tercile_overall = ntile(exposure_sf_val, 3),
    exposure_group_overall = factor(
      c("Low exposure", "Medium exposure", "High exposure")[tercile_overall],
      levels = c("Low exposure", "Medium exposure", "High exposure")
    )
  )



# --- Step 3b: SECTOR exposure (aggregated) ---
# Weighted sum over firm sizes
exposure_s_baseline <- weighted_exposure(
  near_tbl     = near_mw_baseline,
  pi_tbl       = pi_baseline,
  time_var     = "baseline_dummy",
  by_vars      = c("Employment_Sector"),
  weight_dim   = "Wage_group",
  exposure_col = "near_min",
  pi_col       = "pi",
  out_col      = "exposure_baseline_val"
) %>%
  arrange(desc(exposure_baseline_val)) %>%
  mutate(
    tercile = ntile(exposure_baseline_val, 3),
    exposure_group = factor(
      c("Low exposure", "Medium exposure", "High exposure")[tercile],
      levels = c("Low exposure", "Medium exposure", "High exposure")
    )
  ) %>%
  select(Employment_Sector, exposure_baseline_val, exposure_group)


# --- Step 4: Data Checks ---
check_consistency <- exposure_sf_baseline %>%
  group_by(Employment_Sector) %>%
  summarise(
    exposure_agg = sum(exposure_sf_val * pi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    exposure_s_baseline %>% 
      select(Employment_Sector, exposure_baseline_val),
    by = "Employment_Sector"
  ) %>%
  mutate(diff = abs(exposure_agg - exposure_baseline_val))

# Should all be < 0.001 (rounding only)
cat("Max discrepancy:", max(check_consistency$diff, na.rm = TRUE), "\n")
print(check_consistency)





#===============================================================================
# 4. Generate Regression Panel at the Sector X Firm Size X Quarter Level
# 
# Notes:
#
# STEPS:
#       1) Calculate Outcome Variables for regressions.
#       2) Define Continuous Treatment Variable and Time Dummies for Treatments.
#       3) Combine these to create panel.
#       4) conduct some basic data validation.
#
#===============================================================================




# --- Step 1: Calculate Outcome Variables from Survey Data ---



# -- Step 1A: Calc Share Based Outcomes -- #
# First using all workers with min wage (formal and informal)
# Second calc non compliance for only formal workers


shares <- compute_prop_indicator(
  design = design,
  var = c("below_min", "informal"),
  time_var = "year_quarter",
  group_var = c("group")
)

#Formal-only non-compliance
shares_formal <- compute_prop_indicator(
  design = subset(design, Employment_Status == "Formal"),
  var = "below_min",
  time_var = "year_quarter",
  group_var = "group"
) %>%
  rename(below_min_formal = estimate) %>%
  select(time, group, below_min_formal)



share_outcomes <-shares %>%
  pivot_wider(id_cols = c("time", "group"), names_from = var, values_from = estimate)

share_outcomes <- share_outcomes %>%
  left_join(shares_formal, by = c("time", "group"))


# --- Step 1B: Calc Percentile and Inequality Outcomes --- 

#Calc percentiles and inequality measures
probs <- c(.10, .50, .90)
quant <- compute_quantiles(
  design = design,
  var = "real_salary_income_primary",
  time_var = "year_quarter",
  group_var = "group",
  probs = probs
) %>%
  select(-var, -var_label, -prob)


ineq_ratios <- quant %>%
  pivot_wider( id_cols = c("time", "group"), names_from = measure, values_from = estimate) %>%
  mutate(p90_p10 = p90/p10,
         p50_10 = p50/p10,
         log_50_10 = log(p50)- log(p10),
         log_90_10 = log(p90)- log(p10),
         log_50 = log(p50),
         log_10 = log(p10))



## ------------------------------------------------------------------------------
# STEP 1C: Log-Variance of Wages
# Uses the same restrictive design as all other regression outcomes for
# consistency — same sample, same cells, same denominator.
# design_broad is kept for the descriptive decomposition figure only.
# ------------------------------------------------------------------------------


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





# ---Step 2: Calculations for Shock/Treatment Variables ---
# NOTE: Treatment can represent increases in min wage (2015Q2, 2017Q2, 2017Q4 2019Q2 2021Q3 2022Q1, 2023Q2, 2024Q1, 2025Q2)
# but need to consider that 2017Q4, 2022Q1, 2024Q1 are extensions of treatment (phase in off single announcement)
#
# Treatment can also be continuous to take into account variation in the magnitude of min wage increases.


# ---STEP 2A Calculate a Continuous Shock Variable ---
# Continuous variable takes into account magnitude of changes

# load min wage data
min_wage<- readRDS(file.path(config$paths$processed_data, "Min_Wage.rds"))

# keep only the firm-size schedules used in analysis
mw_sizes <- min_wage %>%
  filter(Wage_group %in% c("Large","Medium","Small","Micro")) %>%
  select(year, quarter, Wage_group, real_minwage_harmonized, nom_minwage_harmonized)


#Calc Shock
mw_cont_shock <- mw_sizes %>%
  arrange(Wage_group, year, quarter) %>%
  mutate(time = paste0(year, "Q", quarter)) %>%
  group_by(Wage_group) %>%
  mutate(
    shock_qoq_real = log(real_minwage_harmonized) - lag(log(real_minwage_harmonized)),
    shock_yoy_real = log(real_minwage_harmonized) - lag(log(real_minwage_harmonized), 4),
    shock_qoq_nom  = log(nom_minwage_harmonized)  - lag(log(nom_minwage_harmonized)),
    shock_yoy_nom  = log(nom_minwage_harmonized)  - lag(log(nom_minwage_harmonized),  4)
  ) %>%
  ungroup() %>%
  select(time, Wage_group, shock_qoq_real, shock_yoy_real, shock_qoq_nom, shock_yoy_nom)

# ------------------------------------------------------------------------------
# STEP 2B: Treatment Dummy and Event-Time Variables
#
# MW change events (announcement-based, phase-ins treated as single event):
#   Event A: 2017Q2  (announced + implemented; 2017Q4 is phase-in of same announcement)
#   Event B: 2019Q2  (new announcement)
#   Event C: 2021Q3  (new announcement; 2022Q1 is phase-in)
#   Event D: 2023Q2  (new announcement; 2024Q1 is phase-in)
#
# Design choices:
#   1. Treatment quarter EXCLUDED from both pre and post (partial exposure,
#      avoids contamination). Flagged with its own indicator.
#   2. Phase-in quarters are INSIDE the post period — not separate events.
#   3. For the collapsed regression: post = 1 for all quarters strictly after
#      the treatment quarter, pre = 0 for all quarters strictly before it.
#   4. Event-time lags/leads also constructed for event study robustness.
# ------------------------------------------------------------------------------

# All quarters present in the panel
all_quarters <- tibble(time = sort(unique(design$variables$year_quarter))) %>%
  mutate(
    year_num    = as.integer(substr(time, 1, 4)),
    quarter_num = as.integer(substr(time, 6, 6)),
    # Numeric index: 2017Q1 = 2017.00, 2017Q2 = 2017.25, etc.
    time_index  = year_num + (quarter_num - 1) / 4
  )

# -- Define treatment event dates (announcement quarter) --
# These are the quarters to EXCLUDE from pre/post (partial treatment)
treatment_quarters <- c(2017.25,   # 2017Q2 — Event A (2017Q4 phase-in is NOT a new event)
                        2019.25,   # 2019Q2 — Event B
                        2021.50,   # 2021Q3 — Event C (2022Q1 phase-in is NOT a new event)
                        2023.25)   # 2023Q2 — Event D (2024Q1 phase-in is NOT a new event)

# -- Phase-in quarters: inside post period but flag separately for robustness --
phasein_quarters <- c(2017.75,   # 2017Q4 — phase-in of Event A
                      2022.00,   # 2022Q1 — phase-in of Event C
                      2024.00)   # 2024Q1 — phase-in of Event D

treatment_dummy <- all_quarters %>%
  mutate(
    
    # --- Flag treatment and phase-in quarters ---
    is_treatment_quarter = as.integer(time_index %in% treatment_quarters),
    is_phasein_quarter   = as.integer(time_index %in% phasein_quarters),
    
    # --- Pre/post for Event A (2017Q2): main collapsed regression dummy ---
    # pre  = strictly before 2017Q2
    # post = strictly after 2017Q2 (includes 2017Q4 phase-in — same announcement)
    # treatment quarter itself = NA / excluded from pre-post
    pre_post_17 = case_when(
      time_index <  2017.25 ~ "pre",
      time_index == 2017.25 ~ NA_character_,   # exclude treatment quarter
      time_index >  2017.25 ~ "post"
    ),
    post_17 = case_when(
      time_index <  2017.25 ~ 0L,
      time_index == 2017.25 ~ NA_integer_,     # excluded
      time_index >  2017.25 ~ 1L
    ),
    
    # --- Pre/post for Event B (2019Q2) ---
    post_19 = case_when(
      time_index <  2019.25 ~ 0L,
      time_index == 2019.25 ~ NA_integer_,
      time_index >  2019.25 ~ 1L
    ),
    
    # --- Pre/post for Event C (2021Q3) ---
    post_21 = case_when(
      time_index <  2021.50 ~ 0L,
      time_index == 2021.50 ~ NA_integer_,
      time_index >  2021.50 ~ 1L
    ),
    
    # --- Pre/post for Event D (2023Q2) ---
    post_23 = case_when(
      time_index <  2023.25 ~ 0L,
      time_index == 2023.25 ~ NA_integer_,
      time_index >  2023.25 ~ 1L
    ),
    
    # --- Event-time relative to Event A (2017Q2) ---
    # In quarters. Used for event study / leads-lags robustness.
    # e.g. event_time_17 = -4 means 4 quarters before 2017Q2
    # Phase-in quarter 2017Q4 will have event_time_17 = +2
    event_time_17 = round((time_index - 2017.25) * 4),
    
    # --- Event-time relative to Event B (2019Q2) ---
    event_time_19 = round((time_index - 2019.25) * 4),
    
    # --- Event-time relative to Event B (2019Q3) ---
    event_time_21 = round((time_index - 2021.5) * 4),
    
    # --- Event-time relative to Event B (2023Q1) ---
    event_time_23 = round((time_index - 2023.25) * 4),
    
    
    # --- Collapsed pre/post period labels (for averaging in regression) ---
    # For the 2-period collapsed spec around Event A:
    #   Average all pre quarters into one observation per cell
    #   Average all post quarters (strictly post-2017Q2) into one observation
    # Quarters to exclude: 2017Q2 itself (partial treatment)
    period_collapsed_17 = case_when(
      time_index <  2017.25 ~ "pre",
      time_index == 2017.25 ~ NA_character_,   # dropped from collapsed regression
      time_index >  2017.25 ~ "post"
    )
    
  )


# ---- STEP 3: Combine into Panel ------

panel_outcomes <- share_outcomes %>%
  left_join(ineq_ratios,  by = c("time", "group")) %>%
  left_join(var_raw %>% select(time, group, var_log_wage, log_var_wage),
            by = c("time", "group")) %>%
  # Split group back into sector and firm size
  separate(group,
           into = c("Employment_Sector", "Wage_group"),
           sep  = "__",
           remove = FALSE)


panel_with_treatment <- panel_outcomes %>%
  left_join(treatment_dummy %>%
              select(time, time_index, post_17, post_19, post_21, post_23),
            by = "time") %>%
  left_join(mw_cont_shock,
            by = c("time", "Wage_group"))


panel_sf <- panel_with_treatment %>%
  left_join(
    exposure_sf_baseline %>%
      select(Employment_Sector, Wage_group,
             exposure_sf_val, pi,
             tercile_within_tier, exposure_group_within_tier,
             tercile_overall,    exposure_group_overall),
    by = c("Employment_Sector", "Wage_group")
  )


panel_sf <- panel_sf %>%
  mutate(
    time        = factor(time, levels = sort(unique(time))),  # ordered quarters
    cell_id     = paste(Employment_Sector, Wage_group, sep = "__"),
    # numeric time index already in time_index — useful for trends
  ) %>%
  arrange(Employment_Sector, Wage_group, time)




# ---STEP 4: Data Diagnostics ---

#basic dimension check
cat("\n=== Panel Dimensions ===\n")
cat("Rows:         ", nrow(panel_sf), "\n")
cat("Unique cells: ", n_distinct(panel_sf$cell_id), "\n")
cat("Unique quarters:", n_distinct(panel_sf$time), "\n")
cat("Sectors:      ", n_distinct(panel_sf$Employment_Sector), "\n")
cat("Wage groups:  ", n_distinct(panel_sf$Wage_group), "\n")


#missing value investigation
cat("\n=== Missing values in key variables ===\n")
key_vars <- c("informal", "log_var_wage", "exposure_sf_val",
              "post_17", "shock_yoy_real")
panel_sf %>%
  summarise(across(all_of(key_vars), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "share_missing") %>%
  print()


# Full grid of all expected combinations
full_grid <- expand.grid(
  cell_id = unique(panel_sf$cell_id),
  time    = unique(panel_sf$time),
  stringsAsFactors = FALSE
) %>%
  as_tibble()

# Check what's missing
n_expected <- nrow(full_grid)
n_actual   <- nrow(panel_sf)
cat("Expected:", n_expected, "| Actual:", n_actual, 
    "| Missing:", n_expected - n_actual, "\n")

# Identify which cell × quarter combinations are missing
missing_cells <- full_grid %>%
  anti_join(panel_sf %>% select(cell_id, time) %>% 
              mutate(time = as.character(time)),
            by = c("cell_id", "time"))

cat("\nMissing cell × quarter combinations:\n")
print(missing_cells %>% arrange(time, cell_id), n = Inf)


cat("\n=== Exposure distribution across cells ===\n")
panel_sf %>%
  distinct(cell_id, exposure_sf_val) %>%
  summarise(
    min    = min(exposure_sf_val, na.rm = TRUE),
    p25    = quantile(exposure_sf_val, .25, na.rm = TRUE),
    median = median(exposure_sf_val, na.rm = TRUE),
    p75    = quantile(exposure_sf_val, .75, na.rm = TRUE),
    max    = max(exposure_sf_val, na.rm = TRUE)
  ) %>%
  print()

cat("\n=== Time coverage (should see pre and post 2017Q2) ===\n")
panel_sf %>%
  count(time, post_17) %>%
  print(n = Inf)



# ------------------------------------------------------------------------------
# STEP 7: Save
# ------------------------------------------------------------------------------


saveRDS(exposure_sf_baseline,
        file.path(config$paths$processed_data, 
                  "sector_firmsize_mw_exposure_baseline.rds"))


saveRDS(exposure_s_baseline,
        file.path(config$paths$processed_data, 
                  "sector_mw_exposure_baseline.rds"))

saveRDS(panel_sf,
        file.path(config$paths$processed_data,
                  "panel_sector_firmsize_quarter.rds"))

# for data checks
saveRDS(panel_emp_privsec_wagegrp,
        file.path(config$paths$processed_data, "panel_emp_privsec_wagegrp.rds"))

saveRDS(design,
        file.path(config$paths$processed_data, "design_restrictive.rds"))




























