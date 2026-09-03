#===============================================================================
#
# Script: 10_build_individual_panel.R
#
# Purpose: Build the individual-level panel dataset for Method 2 — a
#          difference-in-difference design exploiting the 2021Q3 creation of
#          a dedicated Micro minimum wage tier.
#
#          SAMPLE LOGIC (critical):
#            BASELINE restrictions define the treatment/control groups:
#              - working-age, employed, private-sector, MW-covered,
#                positive salary, positive hours, known tier (Micro or Small)
#            POST-PERIOD: track the SAME individuals regardless of what
#              happens to them. Transitions to self-employment, informality,
#              unemployment, or out of the labor force are OUTCOMES, not
#              reasons to drop observations. Restricting post-period
#              observations to eligible wage earners would condition on a
#              post-treatment outcome and bias wage estimates.
#
#          Two outcome types result from this design:
#            EXTENSIVE MARGIN: employment transitions (defined for all)
#            INTENSIVE MARGIN: wages, hours (defined only for wage earners;
#              missingness is itself informative)
#
#          Treatment = micro-firm workers (smaller MW increase)
#          Control   = small-firm workers (larger MW increase)
#          beta captures the DIFFERENTIAL effect of the larger MW hike.
#
# Pipeline position:
#   01A -> 01B -> 02 -> [10] -> 11 -> 12
#   Reads Full_ENCFT_clean.rds (02).
#   Does NOT use 03's survey design objects.
#
# Outputs (to config$data_dirs$method2):
#   individual_panel_<event_tag>_<window_tag>.rds
#   panel_diagnostics_<event_tag>.rds
#
#===============================================================================


# --- Setup ------------------------------------------------------------------ #

# Skip re-sourcing if already loaded (e.g., by the runner script)
if (!exists("config")) {
  source(here::here("Code", "R", "clean scripts", "00_setup.R"))
}


cat("=== 10_build_individual_panel.R ===\n\n")

#===============================================================================
# STEP 0. Parameters
#===============================================================================



# --- Event definition ---
M2_EVENT <- config$method2$events$event_2021q3_micro

# --- Control group bandwidth ---
# "all"  = Small 11-50 (Firm_size bins 11-20, 20-30, 31-50)
# "narrow" = Small 11-20 only (Firm_size bin "11-20") — tighter comparability
M2_CONTROL_BW <- config$method2$control_bandwidth

# --- Treatment minimum firm size ---
# Minimum value of CANTIDAD_PERSONAS_TRABAJAN_EMP for micro workers.
# NULL or 1 = keep all micro (1-9). Set to 3 to exclude quasi-self-employed
# 1-2 person firms.
M2_TREAT_MIN_FS <- config$method2$treatment_min_firmsize

# --- Sample tag for folder naming ---
# Combines control BW and treatment restriction into one folder label
sample_tag <- M2_CONTROL_BW
if (!is.null(M2_TREAT_MIN_FS) && M2_TREAT_MIN_FS > 1) {
  sample_tag <- paste0(sample_tag, "_micro", M2_TREAT_MIN_FS, "plus")
}

# --- Window configurations ---
M2_WINDOWS <- config$method2$windows

M2_ACTIVE_WINDOW <- config$method2$active_window

# --- Age band ---
M2_AGE_MIN <- config$age$regression$min   
M2_AGE_MAX <- config$age$regression$max   

# --- Balance modes to run ---
# Both saved to separate subfolders; downstream scripts select via config.
M2_BALANCE_MODES <- c(balanced = TRUE, unbalanced = FALSE)

# --- Output directories ---
# Processed data: panel .rds files
m2_data_dir <- file.path(
  config$paths$processed_data %||% here::here("Processed Data"),
  "Panel Regressions", M2_EVENT$event_tag, sample_tag
)
dir.create(m2_data_dir, recursive = TRUE, showWarnings = FALSE)

cat(sprintf("  Event:  %s\n", M2_EVENT$event_tag))
cat(sprintf("  Active window: %s\n", M2_ACTIVE_WINDOW))
cat(sprintf("  Output: %s\n\n", m2_data_dir))


#===============================================================================
# STEP 1. Load data
#===============================================================================

cat("[1] Loading Full_ENCFT_clean...\n")

full_data <- readRDS(
  file.path(config$paths$processed_data, "Full_ENCFT_clean.rds")
)

cat(sprintf("  Full data: %s rows x %d cols\n",
            format(nrow(full_data), big.mark = ","), ncol(full_data)))


#===============================================================================
# STEP 2. Define the time window and identify BASELINE-eligible individuals
#
# The key design choice: restrictive filters apply only at BASELINE (pre-period)
# to identify who is treatment and who is control. In post-period quarters, we
# bring back ALL observations for those same people from the full data — even
# if they are now self-employed, informal, unemployed, or out of the labor force.
#
# This avoids conditioning on post-treatment outcomes and lets us estimate
# both extensive-margin (transition) and intensive-margin (wage) effects.
#===============================================================================

cat("[2] Identifying baseline-eligible individuals...\n")

# Widest time range across all window configurations
all_qtrs <- unique(unlist(lapply(M2_WINDOWS, function(w) {
  c(w$pre_qtrs, M2_EVENT$event_qtr, w$post_qtrs)
})))

qtr_range <- range(all_qtrs)

# --- 2a. Extract all person-quarter records in the time windows ---
# Minimal filters: age and present in the survey. No employment restrictions.

all_records <- full_data %>%
  filter(year_quarter >= qtr_range[1], year_quarter <= qtr_range[2],
         EDAD >= M2_AGE_MIN, EDAD <= M2_AGE_MAX)

cat(sprintf("  All working-age records in window: %s\n",
            format(nrow(all_records), big.mark = ",")))


# --- 2b. Identify baseline-eligible individuals from PRE-PERIOD quarters ---
# These are the people who, in at least one pre-period quarter, satisfy the
# full set of restrictions needed to assign them to treatment or control.



table(all_records$Employment_Type, useNA = "ifany")
table(all_records$Employment_Type, all_records$OCUPADO, useNA = "ifany")
table(all_records$Principal_Category)
table(all_records$OCUPADO, useNA= "ifany")

#filter to quarters for the currently selected window
all_pre_qtrs <- unique(unlist(lapply(M2_WINDOWS, function(w) w$pre_qtrs)))

baseline_eligible <- all_records %>%
  filter(year_quarter %in% all_pre_qtrs) %>%
  # Must be employed
  filter(OCUPADO == 1) %>%
  # Private-sector employee
  filter(Employment_Type == "private employee") %>%
  # MW-covered (excl. domestic, FTZ, utilities)
  filter(!Principal_Category %in% "Domestic Worker",
         !Principal_Category %in% "Free Trade Zone",
         !Employment_Sector %in% "Electricity and Water") %>%
  # Positive salary and hours (so we can define hourly wage at baseline)
  filter(!is.na(real_salary_income_wage_primary),
         real_salary_income_wage_primary > 0,
         !is.na(hours_worked_primary),
         hours_worked_primary > 0) %>%
  # Known firm size tier
  filter(has_tier) %>%
  # Known formality
  filter(!is.na(Employment_Status)) %>%
  # TREATMENT + CONTROL TIERS ONLY
  filter(wage_group %in% c(M2_EVENT$treatment$tier, M2_EVENT$control$tier))


# --- Apply control bandwidth restriction ---
if (M2_CONTROL_BW == "narrow") {
  baseline_eligible <- baseline_eligible %>%
    filter(wage_group == M2_EVENT$treatment$tier | Firm_size == "11-20")
  bw_label <- "Small 11-20"
} else {
  bw_label <- "Small 11-50"
}

# --- Apply treatment min firm size restriction ---
if (!is.null(M2_TREAT_MIN_FS) && M2_TREAT_MIN_FS > 1) {
  n_before <- sum(baseline_eligible$wage_group == M2_EVENT$treatment$tier)
  baseline_eligible <- baseline_eligible %>%
    filter(wage_group != M2_EVENT$treatment$tier |
             CANTIDAD_PERSONAS_TRABAJAN_EMP >= M2_TREAT_MIN_FS)
  n_after <- sum(baseline_eligible$wage_group == M2_EVENT$treatment$tier)
  treat_label <- sprintf("Micro %d-9", M2_TREAT_MIN_FS)
  cat(sprintf("  Treatment restriction: firm size >= %d (%d -> %d micro obs)\n",
              M2_TREAT_MIN_FS, n_before, n_after))
} else {
  treat_label <- "Micro 1-9"
}

cat(sprintf("  Control: %s | Treatment: %s\n", bw_label, treat_label))
cat(sprintf("  Sample tag: %s\n", sample_tag))

baseline_ids <- unique(baseline_eligible$ID_PERSONA)

cat(sprintf("  Baseline-eligible individuals: %s (from %s pre-period obs)\n",
            format(length(baseline_ids), big.mark = ","),
            format(nrow(baseline_eligible), big.mark = ",")))

# Quick counts by tier x quarter in pre-period
cat("  Baseline tier x quarter counts:\n")
print(
  as.data.frame(
    baseline_eligible %>%
      count(year_quarter, wage_group) %>%
      tidyr::pivot_wider(names_from = wage_group, values_from = n, values_fill = 0)
  ),
  row.names = FALSE
)
cat("\n")


#===============================================================================
# STEP 3. Validate ID_PERSONA as a panel identifier
#
# Run on ALL records for baseline-eligible individuals (not just pre-period).
# Check time-invariant characteristics.
#===============================================================================

cat("[3] Validating ID_PERSONA...\n")

# All records for baseline-eligible people across the full window
tracking_pool <- all_records %>% filter(ID_PERSONA %in% baseline_ids)

person_qtrs <- tracking_pool %>%
  group_by(ID_PERSONA) %>%
  summarise(
    n_qtrs       = n_distinct(year_quarter),
    qtrs         = paste(sort(unique(year_quarter)), collapse = ","),
    n_sex        = n_distinct(SEXO),
    min_age      = min(EDAD),
    max_age      = max(EDAD),
    age_range    = max(EDAD) - min(EDAD),
    n_provinces  = n_distinct(ID_PROVINCIA),
    .groups      = "drop"
  )

n_total_persons   <- nrow(person_qtrs)
n_repeat_persons  <- sum(person_qtrs$n_qtrs > 1)
n_single_obs      <- sum(person_qtrs$n_qtrs == 1)

cat(sprintf("  Unique baseline-eligible persons: %s\n",
            format(n_total_persons, big.mark = ",")))
cat(sprintf("  Appearing in >1 quarter (full window): %s (%.1f%%)\n",
            format(n_repeat_persons, big.mark = ","),
            100 * n_repeat_persons / n_total_persons))
cat(sprintf("  Single observation only: %s (%.1f%%)\n\n",
            format(n_single_obs, big.mark = ","),
            100 * n_single_obs / n_total_persons))

cat("  Distribution of quarters observed per person:\n")
print(table(pmin(person_qtrs$n_qtrs, 7)), useNA = "ifany")
cat("\n")

# --- Validation checks ---
sex_violations <- person_qtrs %>% filter(n_sex > 1)
n_sex_viol <- nrow(sex_violations)
cat(sprintf("  Sex inconsistencies: %d (%.2f%%)\n",
            n_sex_viol, 100 * n_sex_viol / n_total_persons))

age_violations <- person_qtrs %>%
  filter(n_qtrs > 1) %>%
  filter(age_range > ceiling(n_qtrs / 4) + 1)
n_age_viol <- nrow(age_violations)
cat(sprintf("  Age inconsistencies:  %d of %d repeat persons (%.2f%%)\n",
            n_age_viol, n_repeat_persons,
            100 * n_age_viol / max(n_repeat_persons, 1)))

province_movers <- person_qtrs %>%
  filter(n_qtrs > 1, n_provinces > 1)
n_movers <- nrow(province_movers)
cat(sprintf("  Province movers:      %d of %d repeat persons (%.2f%%)\n\n",
            n_movers, n_repeat_persons,
            100 * n_movers / max(n_repeat_persons, 1)))

# Drop sex violations
if (n_sex_viol > 0) {
  cat(sprintf("  -> Dropping %d ID_PERSONA values with sex inconsistency.\n\n",
              n_sex_viol))
  bad_ids <- sex_violations$ID_PERSONA
  baseline_ids <- setdiff(baseline_ids, bad_ids)
  tracking_pool <- tracking_pool %>% filter(!ID_PERSONA %in% bad_ids)
  baseline_eligible <- baseline_eligible %>% filter(!ID_PERSONA %in% bad_ids)
}


#===============================================================================
# STEP 4. Build panel for each window configuration
#
# For each window:
#   a) Assign treatment from FIRST pre-period baseline-eligible observation
#   b) Pull ALL records for those individuals in the window quarters from
#      the tracking pool (not just baseline-eligible records)
#   c) Build transition outcome variables
#   d) Flag tier-switchers
#   e) Save
#===============================================================================

cat("[4] Building individual panels by window...\n\n")

diagnostics_all <- list()

for (win_name in names(M2_WINDOWS)) {
  
  win <- M2_WINDOWS[[win_name]]
  cat(sprintf("  --- Window: %s (%s) ---\n", win_name, win$label))
  
  pre_qtrs  <- win$pre_qtrs
  post_qtrs <- win$post_qtrs
  event_qtr <- M2_EVENT$event_qtr
  
  # Quarters to keep
  keep_qtrs <- c(pre_qtrs, post_qtrs)
  if (!win$exclude_event) keep_qtrs <- c(keep_qtrs, event_qtr)
  
  
  # --- 4a. Assign treatment from first baseline-eligible pre observation ---
  # Only use pre-period observations that meet all baseline criteria.
  #could be any quarter in pre not only first
  
  baseline_tier <- baseline_eligible %>%
    filter(year_quarter %in% pre_qtrs) %>%
    arrange(ID_PERSONA, year_quarter) %>%
    group_by(ID_PERSONA) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(
      ID_PERSONA,
      baseline_tier      = wage_group,
      baseline_wage_group = as.character(wage_group),
      treat = as.integer(wage_group == M2_EVENT$treatment$tier),
      # Baseline characteristics (from the assignment observation)
      baseline_salary    = real_salary_income_wage_primary,
      baseline_hwage     = real_salary_primary_hourly_base,
      baseline_hours     = hours_worked_primary,
      baseline_formal    = as.integer(Employment_Status == "Formal"),
      baseline_qtr       = year_quarter
    )
  
  panel_ids <- baseline_tier$ID_PERSONA
  
  
  # --- 4b. Pull ALL records for panel individuals ---
  # This is the crucial difference: we pull from tracking_pool (minimally
  # filtered) not from baseline_eligible (restrictively filtered).
  
  panel_win <- tracking_pool %>%
    filter(ID_PERSONA %in% panel_ids,
           year_quarter %in% keep_qtrs)
  
  # --- 4c. Require appearance in at least one pre AND one post quarter ---
  person_periods <- panel_win %>%
    mutate(period_check = case_when(
      year_quarter %in% pre_qtrs  ~ "pre",
      year_quarter %in% post_qtrs ~ "post",
      TRUE                        ~ "event"
    )) %>%
    group_by(ID_PERSONA) %>%
    summarise(
      has_pre      = any(period_check == "pre"),
      has_post     = any(period_check == "post"),
      n_pre_qtrs   = n_distinct(year_quarter[period_check == "pre"]),
      n_post_qtrs  = n_distinct(year_quarter[period_check == "post"]),
      n_total_qtrs = n_distinct(year_quarter),
      .groups      = "drop"
    )
  
  # Save unfiltered panel for reuse across balance modes
  panel_win_all <- panel_win
  
  
  # =========================================================================
  # Inner loop: run both balanced and unbalanced, save each separately
  # =========================================================================
  
  for (bal_name in names(M2_BALANCE_MODES)) {
    
    bal <- M2_BALANCE_MODES[[bal_name]]
    balance_label <- bal_name
    
    cat(sprintf("\n    -- Balance mode: %s --\n", balance_label))
    
    if (bal) {
      keep_ids <- person_periods %>%
        filter(n_pre_qtrs == length(pre_qtrs),
               n_post_qtrs == length(post_qtrs)) %>%
        pull(ID_PERSONA)
    } else {
      keep_ids <- person_periods %>%
        filter(has_pre, has_post) %>%
        pull(ID_PERSONA)
    }
    
    panel_win <- panel_win_all %>% filter(ID_PERSONA %in% keep_ids)
    baseline_tier_win <- baseline_tier %>% filter(ID_PERSONA %in% keep_ids)
    
    cat(sprintf("    Panel individuals (%s): %s\n",
                balance_label, format(length(keep_ids), big.mark = ",")))
    
    
    # --- 4d. Join treatment assignment and build variables ---
    
    panel_win <- panel_win %>%
      left_join(baseline_tier_win %>%
                  select(ID_PERSONA, treat, baseline_tier,
                         baseline_salary, baseline_hwage, baseline_hours,
                         baseline_formal, baseline_qtr),
                by = "ID_PERSONA") %>%
      mutate(
        # Period assignment
        period = case_when(
          year_quarter %in% pre_qtrs  ~ "pre",
          year_quarter %in% post_qtrs ~ "post",
          year_quarter == event_qtr   ~ "event",
          TRUE                        ~ NA_character_
        ),
        post = as.integer(period == "post"),
        
        # --- EXTENSIVE MARGIN OUTCOMES ---
        # These are defined for ALL observations regardless of employment status.
        is_employed          = as.integer(OCUPADO == 1),
        is_private_employee  = as.integer(OCUPADO == 1 &
                                            Employment_Type == "private employee"),
        is_selfemp_now       = as.integer(OCUPADO == 1 &
                                            Employment_Type == "self-employed"),
        is_owner_now         = as.integer(OCUPADO == 1 &
                                            Employment_Type == "owner or shareholder"),
        is_independent_now   = as.integer(OCUPADO == 1 &
                                            Employment_Type %in% c("self-employed",
                                                                   "owner or shareholder")),
        is_informal_now      = as.integer(OCUPADO == 1 &
                                            !is.na(Employment_Status) &
                                            Employment_Status == "Informal"),
        is_formal_private    = as.integer(OCUPADO == 1 &
                                            Employment_Type == "private employee" &
                                            !is.na(Employment_Status) &
                                            Employment_Status == "Formal"),
        is_unemployed        = as.integer(OCUPADO != 1 & PEA == 1),
        is_out_of_lf         = as.integer(PEA != 1),
        # Still in original tier (only for those still employed with known tier)
        is_same_tier         = as.integer(OCUPADO == 1 & has_tier &
                                            wage_group == baseline_tier),
        
        # --- INTENSIVE MARGIN OUTCOMES ---
        # Only defined for wage earners with positive salary and hours.
        # NA for everyone else — missingness is informative.
        has_wage = as.integer(
          OCUPADO == 1 &
            !is.na(real_salary_income_wage_primary) &
            real_salary_income_wage_primary > 0 &
            !is.na(hours_worked_primary) &
            hours_worked_primary > 0
        ),
        
        log_real_hwage = if_else(
          has_wage == 1L,
          log(real_salary_primary_hourly_base),
          NA_real_
        ),
        log_real_mwage = if_else(
          has_wage == 1L,
          log(real_salary_income_wage_primary),
          NA_real_
        ),
        
        # Total primary labor income (salary + independent), defined for
        # anyone employed with positive total income. Captures earnings
        # even if the person transitions to self-employment post-event.
        has_total_income = as.integer(
          OCUPADO == 1 &
            !is.na(real_total_income_all_primary) &
            real_total_income_all_primary > 0
        ),
        log_real_total_income = if_else(
          has_total_income == 1L,
          log(real_total_income_all_primary),
          NA_real_
        ),
        
        # Event time
        event_time = case_when(
          year_quarter == "2020Q4" ~ -3L,
          year_quarter == "2021Q1" ~ -2L,
          year_quarter == "2021Q2" ~ -1L,
          year_quarter == "2021Q3" ~  0L,
          year_quarter == "2021Q4" ~  1L,
          year_quarter == "2022Q1" ~  2L,
          year_quarter == "2022Q2" ~  3L,
          TRUE                     ~ NA_integer_
        ),
        
        qtr_idx = as.integer(factor(year_quarter))
      )
    
    
    # --- 4e. Flag tier-switchers and switching direction ----------------------
    #
    # tier_switcher: did the person ever report a different tier than baseline
    #   (across all quarters where they are employed with known tier).
    # switch_direction: classifies the transition from baseline_tier to the
    #   LAST observed post-period tier. Persons not employed with a known tier
    #   in any post quarter get "Exited tier coverage".
    # ----------------------------------------------------------------------- #
    
    tier_changes <- panel_win %>%
      filter(OCUPADO == 1, has_tier) %>%
      group_by(ID_PERSONA) %>%
      summarise(
        n_tiers        = n_distinct(wage_group),
        tiers_observed = paste(sort(unique(as.character(wage_group))), collapse = ","),
        .groups        = "drop"
      ) %>%
      mutate(tier_switcher = n_tiers > 1)
    
    # Determine last post-period tier for direction classification
    last_post_tier <- panel_win %>%
      filter(period == "post", OCUPADO == 1, has_tier) %>%
      arrange(ID_PERSONA, desc(year_quarter)) %>%
      group_by(ID_PERSONA) %>%
      slice(1) %>%
      ungroup() %>%
      transmute(ID_PERSONA, last_post_tier = as.character(wage_group))
    
    # Build person-level switching summary
    tier_switch_detail <- baseline_tier_win %>%
      select(ID_PERSONA, baseline_tier, treat) %>%
      left_join(tier_changes %>% select(ID_PERSONA, tier_switcher, tiers_observed),
                by = "ID_PERSONA") %>%
      left_join(last_post_tier, by = "ID_PERSONA") %>%
      mutate(
        tier_switcher = coalesce(tier_switcher, FALSE),
        switch_direction = case_when(
          is.na(last_post_tier)                        ~ "Exited tier coverage",
          last_post_tier == as.character(baseline_tier) ~ "Same tier",
          TRUE ~ paste0(as.character(baseline_tier), " -> ", last_post_tier)
        )
      )
    
    # Left join flags to panel (people who exit employment get FALSE)
    panel_win <- panel_win %>%
      left_join(tier_switch_detail %>%
                  select(ID_PERSONA, tier_switcher, switch_direction),
                by = "ID_PERSONA") %>%
      mutate(
        tier_switcher    = coalesce(tier_switcher, FALSE),
        switch_direction = coalesce(switch_direction, "Exited tier coverage")
      )
    
    n_switchers <- sum(tier_switch_detail$tier_switcher)
    cat(sprintf("    Tier switchers: %d (%.1f%%)\n",
                n_switchers,
                100 * n_switchers / length(keep_ids)))
    
    # Tier switching summary by treatment group and direction
    tier_switch_summary <- tier_switch_detail %>%
      count(treat, baseline_tier, switch_direction, name = "n_persons") %>%
      group_by(treat) %>%
      mutate(pct = round(100 * n_persons / sum(n_persons), 1)) %>%
      ungroup() %>%
      arrange(treat, desc(n_persons))
    
    cat("    Tier switching by treatment group:\n")
    print(as.data.frame(tier_switch_summary), row.names = FALSE)
    
    
    # --- 4f. Province mover flag ---
    
    panel_win <- panel_win %>%
      left_join(
        person_qtrs %>% transmute(ID_PERSONA, province_mover = n_provinces > 1),
        by = "ID_PERSONA"
      ) %>%
      mutate(province_mover = coalesce(province_mover, FALSE))
    
    
    # --- 4g. Treatment / control counts ---
    
    treat_counts <- baseline_tier_win %>%
      count(treat, baseline_tier, name = "n_persons")
    cat("    Treatment/control:\n")
    print(as.data.frame(treat_counts), row.names = FALSE)
    
    # --- Transition summary ---
    post_status <- panel_win %>%
      filter(period == "post") %>%
      group_by(treat) %>%
      summarise(
        n_obs             = n(),
        pct_employed      = 100 * mean(is_employed),
        pct_private_emp   = 100 * mean(is_private_employee),
        pct_selfemp       = 100 * mean(is_selfemp_now),
        pct_independent   = 100 * mean(is_independent_now),
        pct_informal      = 100 * mean(is_informal_now),
        pct_has_wage      = 100 * mean(has_wage),
        .groups = "drop"
      )
    cat("    Post-period status by treatment group:\n")
    print(as.data.frame(post_status %>% mutate(across(where(is.numeric), ~round(., 1)))),
          row.names = FALSE)
    
    
    # --- 4h. MW compliance for continuously-employed-with-tier subset ---------
    #
    # Subset: workers employed (OCUPADO == 1) with known tier (has_tier) and
    # a usable wage (has_wage == 1) in EVERY quarter of the window.
    # This is the subset for which compliance can be meaningfully tracked
    # across the full pre/post span.
    # ----------------------------------------------------------------------- #
    
    compliance_ids <- panel_win %>%
      group_by(ID_PERSONA) %>%
      summarise(
        n_qtrs_in_panel     = n_distinct(year_quarter),
        n_qtrs_emp_tier_wage = sum(OCUPADO == 1 & has_tier & has_wage == 1L),
        .groups = "drop"
      ) %>%
      filter(n_qtrs_emp_tier_wage == n_qtrs_in_panel) %>%
      pull(ID_PERSONA)
    
    compliance_subset <- panel_win %>%
      filter(ID_PERSONA %in% compliance_ids) %>%
      group_by(year_quarter, treat, period) %>%
      summarise(
        n_obs                  = n(),
        pct_below_hourly       = 100 * mean(below_min_hourly_base_salary == 1,
                                            na.rm = TRUE),
        pct_below_monthly      = 100 * mean(below_min_monthly_salary == 1,
                                            na.rm = TRUE),
        mean_log_hwage         = mean(log_real_hwage, na.rm = TRUE),
        mean_log_mwage         = mean(log_real_mwage, na.rm = TRUE),
        .groups = "drop"
      )
    
    cat(sprintf("    Compliance subset (employed + tier + wage all qtrs): %d persons (%.1f%%)\n",
                length(compliance_ids),
                100 * length(compliance_ids) / length(keep_ids)))
    if (nrow(compliance_subset) > 0) {
      cat("    Compliance rates by quarter and treatment:\n")
      print(as.data.frame(compliance_subset %>%
                            mutate(across(where(is.numeric), ~round(., 1)))),
            row.names = FALSE)
    }
    
    
    # --- 4i. Select columns ---
    
    out_cols <- c(
      # IDs
      "ID_PERSONA", "ID_HOGAR", "MIEMBRO", "year_quarter", "year", "quarter",
      
      # Panel structure
      "period", "post", "event_time", "qtr_idx",
      
      # Treatment
      "treat", "baseline_tier", "tier_switcher", "switch_direction",
      "baseline_salary", "baseline_hwage", "baseline_hours",
      "baseline_formal", "baseline_qtr",
      
      # Current-period tier
      "wage_group",
      
      # Demographics
      "SEXO", "Sex", "EDAD", "edu4", "edu7",
      "is_female", "is_sec_complete", "is_tert_complete",
      
      # Geography
      "ID_PROVINCIA", "DES_PROVINCIA", "GRUPO_REGION",
      "province_mover",
      
      # Current employment status (extensive margin)
      "OCUPADO", "Employment_Status", "Employment_Type",
      "Employment_Sector", "Principal_Category",
      "is_employed", "is_private_employee", "is_selfemp_now",
      "is_owner_now", "is_independent_now", "is_informal_now",
      "is_formal_private", "is_same_tier",
      
      # Wage availability
      "has_wage",
      
      # Firm characteristics (may be NA if not employed)
      "TOTAL_PERSONAS_TRABAJAN_EMP", "CANTIDAD_PERSONAS_TRABAJAN_EMP",
      "Firm_size", "has_tier",
      "EMPRESA_INSCRITA_RNC", "TIENE_CONTRATO", "AFILIADO_AFP_PRINC",
      
      # Hours (NA if not working)
      "hours_worked_primary", "HORAS_TRABAJO_EFECT_TOTAL",
      
      # Income (NA if no wage)
      "real_salary_income_wage_primary", "real_salary_primary_hourly_base",
      "real_total_income_all_primary",
      "log_real_hwage", "log_real_mwage", "log_real_total_income",
      "has_total_income",
      
      # Minimum wage and compliance
      "real_minwage_harmonized", "real_minwage_hourly",
      "below_min_monthly_salary", "below_min_hourly_base_salary",
      
      # Survey design
      "FACTOR_EXPANSION", "psu_unique", "strata_unique",
      "UPM", "ESTRATO"
    )
    
    out_cols_present <- intersect(out_cols, names(panel_win))
    missing_cols <- setdiff(out_cols, names(panel_win))
    if (length(missing_cols) > 0) {
      cat(sprintf("    Note: %d requested columns not in data: %s\n",
                  length(missing_cols), paste(missing_cols, collapse = ", ")))
    }
    
    panel_out <- panel_win %>% select(all_of(out_cols_present))
    
    
    # --- 4j. Diagnostics ---
    
    diag_key <- paste(win_name, bal_name, sep = "_")
    
    diag <- list(
      event          = M2_EVENT$event_tag,
      window         = win_name,
      window_label   = win$label,
      balance_type   = balance_label,
      pre_qtrs       = pre_qtrs,
      post_qtrs      = post_qtrs,
      exclude_event  = win$exclude_event,
      
      n_persons      = length(keep_ids),
      n_obs          = nrow(panel_out),
      n_treat        = sum(baseline_tier_win$treat == 1),
      n_control      = sum(baseline_tier_win$treat == 0),
      n_switchers    = n_switchers,
      pct_switchers  = 100 * n_switchers / length(keep_ids),
      n_movers       = length(unique(
        panel_out$ID_PERSONA[panel_out$province_mover %in% TRUE]
      )),
      n_compliance_subset = length(compliance_ids),
      
      panel_structure = person_periods %>%
        filter(ID_PERSONA %in% keep_ids) %>%
        count(n_pre_qtrs, n_post_qtrs, name = "n_persons"),
      
      treat_by_qtr = panel_out %>%
        count(year_quarter, treat, name = "n_obs"),
      
      post_status          = post_status,
      tier_switch_summary  = tier_switch_summary,
      compliance_summary   = compliance_subset,
      
      id_validation = list(
        sex_violations  = n_sex_viol,
        age_violations  = n_age_viol,
        province_movers = n_movers
      ),
      
      built_at = Sys.time()
    )
    
    diagnostics_all[[diag_key]] <- diag
    
    
    # --- 4k. Save ---
    # Path: .../Panel Regressions/<event>/<sample_tag>/<win_tag>/<balance>/
    
    win_data_dir <- file.path(m2_data_dir, balance_label, win$tag)
    dir.create(win_data_dir, recursive = TRUE, showWarnings = FALSE)
    
    out_file <- file.path(win_data_dir, "individual_panel.rds")
    saveRDS(panel_out, out_file)
    cat(sprintf("    Saved: %s (%s obs, %s persons)\n",
                file.path(balance_label, win$tag, "individual_panel.rds"),
                format(nrow(panel_out), big.mark = ","),
                format(length(keep_ids), big.mark = ",")))
    
    # Save tier switching detail (person-level) for downstream analysis
    saveRDS(tier_switch_detail,
            file.path(win_data_dir, "tier_switch_detail.rds"))
    
    # Save compliance subset summary
    saveRDS(compliance_subset,
            file.path(win_data_dir, "compliance_subset.rds"))
    
    cat(sprintf("    + tier_switch_detail.rds, compliance_subset.rds\n\n"))
    
  } # end balance loop
  
} # end window loop


#===============================================================================
# STEP 5. Save diagnostics
#===============================================================================

diag_file <- file.path(m2_data_dir, "panel_diagnostics.rds")
saveRDS(diagnostics_all, diag_file)
cat(sprintf("[5] Diagnostics saved: %s\n", basename(diag_file)))


#===============================================================================
# STEP 6. Summary
#===============================================================================

cat("\n[6] Panel summary across windows and balance modes:\n\n")

summary_tbl <- purrr::map_dfr(diagnostics_all, function(d) {
  tibble::tibble(
    window       = d$window,
    balance      = d$balance_type,
    label        = d$window_label,
    n_persons    = d$n_persons,
    n_obs        = d$n_obs,
    n_treat      = d$n_treat,
    n_control    = d$n_control,
    pct_switch   = round(d$pct_switchers, 1),
    n_compliance = d$n_compliance_subset,
    pre_qtrs     = paste(d$pre_qtrs, collapse = ","),
    post_qtrs    = paste(d$post_qtrs, collapse = ",")
  )
})

print(as.data.frame(summary_tbl), row.names = FALSE)

cat("\n=== 10_build_individual_panel.R complete ===\n")