
#===============================================================================
# STEP 6. Create an aggregated Panel for Household Level Analysis
#===============================================================================


#drop previous unique psu strata
all_ENCFT_clean <- all_ENCFT_clean %>%
  select(-psu_unique, -strata_unique)



#aggregate data by household
ENCFT_quarterly_household <- all_ENCFT_clean %>%
  group_by(ID_HOGAR, ID_PROVINCIA, DES_PROVINCIA, Region4, year_quarter, year, quarter) %>%
  summarise(
    # Aggregate income and other variables across 3 months
    salary_income_primary = sum(salary_income_primary, na.rm = TRUE),
    salary_income_secondary = sum(salary_income_secondary, na.rm = TRUE),
    salary_income_total = sum(salary_income_total, na.rm = TRUE),
    
    benefits_income_primary = sum(benefits_income_primary, na.rm = TRUE),
    benefits_income_secondary = sum(benefits_income_secondary, na.rm = TRUE),
    benefits_income_total = sum(benefits_income_total, na.rm = TRUE),
    
    independent_income_primary = sum(independent_income_primary, na.rm = TRUE),
    independent_income_secondary = sum(independent_income_secondary, na.rm = TRUE),
    independent_income_total = sum(independent_income_total, na.rm = TRUE),
    
    total_income_primary = sum(total_income_primary, na.rm = TRUE),
    total_income_secondary = sum(total_income_secondary, na.rm = TRUE),
    total_income_other = sum(total_income_other, na.rm = TRUE),
    total_income_total = sum(total_income_total, na.rm = TRUE),
    
    
    recieve_any_primary = as.integer(sum(total_income_primary > 0, na.rm = TRUE) > 0),
    recieve_any_secondary = as.integer(sum(total_income_secondary > 0, na.rm = TRUE) > 0),
    recieve_any_other = as.integer(sum(total_income_other > 0, na.rm = TRUE) > 0),
    recieve_any_total = as.integer(sum(total_income_total > 0, na.rm = TRUE) > 0),
    
    adj_income_primary = case_when( (salary_income_primary >0) ~  salary_income_primary,
                                    (independent_income_primary >0) ~independent_income_primary,
                                    TRUE ~ 0
    ),
    
    hh_size = n_distinct(ID_PERSONA),
    
    
    # Keep constant design variables
    UPM = first(UPM),
    ESTRATO = first(ESTRATO),
    FACTOR_EXPANSION = first(FACTOR_EXPANSION),
    CPI = first(CPI),
    nominal_minwage = first(nom_minwage),
    real_minwage = first(real_minwage),
    
    .groups = "drop"
  )


#deflate incomes
ENCFT_quarterly_household <- ENCFT_quarterly_household %>%
  mutate(real_salary_income_primary = salary_income_primary/base_val * 100,
         real_salary_income_secondary = salary_income_secondary/base_val * 100,
         real_salary_income_total = salary_income_total/base_val * 100,
         
         real_benefits_income_primary = benefits_income_primary/base_val * 100,
         real_benefits_income_secondary =  benefits_income_secondary/base_val * 100,
         real_benefits_income_total = benefits_income_total/base_val * 100,
         
         real_independent_income_primary = independent_income_primary/base_val * 100,
         real_independent_income_secondary = independent_income_secondary/base_val * 100,
         real_independent_income_total = independent_income_total/base_val * 100,
         
         real_total_income_primary = total_income_primary/base_val * 100,
         real_total_income_secondary = total_income_secondary/base_val * 100,
         real_total_income_other = total_income_other/base_val * 100,
         real_total_income_total = total_income_total/base_val * 100,
         
         real_adj_income_primary = adj_income_primary/base_val * 100,
         
         real_minwage_harmonized = nom_minwage_harmonized / base_val * 100,
         real_min_wage = nom_min_wage / base_val * 100
  )

#generate a weight for annual pooled data
ENCFT_quarterly_household <- ENCFT_quarterly_household %>%
  mutate(weight_annual  = FACTOR_EXPANSION / 4,
         weight_quarter = FACTOR_EXPANSION) %>%
  ungroup()



#function call to to create unique PSU/STRATA variable
ENCFT_quarterly_household <- check_and_fix_survey_ids(ENCFT_quarterly_household, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")


out_file <-file.path(config$paths$processed_data, "ENCFT_quarterly_household.rds")
saveRDS(ENCFT_quarterly_household, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))
