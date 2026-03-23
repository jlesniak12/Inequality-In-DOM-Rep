source("Code/R/00_setup.R")



# ---- 1. Load Data and Fix Ids ---- #
Full_ENCFT_clean <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))


#function to create unique PSU/STRATA
individual_level_unique_id <- check_and_fix_survey_ids(Full_ENCFT_clean, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")


# ---- 2. Simplify keeping needed variables and create sub samples ---- #

# -- simnplify dataset keeping only needed vars -- #

srvy_vars <- c("psu_unique", "strata_unique", "weight_annual", "weight_quarter", "FACTOR_EXPANSION", "DES_ESTRATO",
               "quarter", "month", "year", "year_quarter")

analysis_vars <- c(
  # -core income and min wage
  
  "real_salary_income_total",
  "real_salary_income_primary",
  "real_independent_income_primary",
  "real_total_income_total",
  "adj_income_primary",
  
  "Wage_group",             #defines firm size based on official buckets of min wage (micro small med large)
  "real_minwage_harmonized",
  
  #  -other traits
  
  "Employment_Sector",      #sector of work
  "Employment_Status",      #formal vs informal,
  "Employment_Type",        # employment class
  "OCUPADO"                 #binary for currently employed
)



#key variables
analysis_all <- individual_level_unique_id %>%
  select(
    all_of(srvy_vars),
    all_of(analysis_vars)
  )


#all employed
analysis_all_emp <- analysis_all %>%
  filter(OCUPADO == 1)


table(analysis_all_emp$Employment_Sector, useNA = "ifany")
table(analysis_all_emp$Employment_Status, useNA = "ifany")


#formal employed
analysis_formal_emp <- analysis_all %>%
  filter(OCUPADO == 1 & Employment_Status == "Formal")

table(analysis_formal_emp$Employment_Sector, useNA = "ifany")
table(analysis_formal_emp$Employment_Status, useNA = "ifany")
table(analysis_formal_emp$Wage_group, useNA = "ifany")


#formal employed with wage group
analysis_formal_emp_wagegroup <-  analysis_formal_emp %>%
  filter(!is.na(Wage_group)) %>%
  filter(!(Wage_group == "Dont Know"))


table(analysis_formal_emp_wagegroup$Employment_Sector, useNA = "ifany")
table(analysis_formal_emp_wagegroup$Employment_Status, useNA = "ifany")
table(analysis_formal_emp_wagegroup$Wage_group, useNA = "ifany")


#formal employed with wage group private sector no gov
analysis_formal_emp_wagegroup_priv <-  analysis_formal_emp_wagegroup %>%
  filter(!(Employment_Sector == "Government"| Employment_Sector == "Unclassified" )) %>%
  filter(Employment_Type == "private employee")

table(analysis_formal_emp_wagegroup_priv$Employment_Sector, useNA = "ifany")
table(analysis_formal_emp_wagegroup_priv$Employment_Status, useNA = "ifany")
table(analysis_formal_emp_wagegroup_priv$Wage_group, useNA = "ifany")


#formal employed with wage group private sector no gov
analysis_all_emp_wagegroup_priv <-  analysis_all_emp %>%
  filter(!is.na(Wage_group)) %>%
  filter(!(Wage_group == "Dont Know")) %>%
  filter(!(Employment_Sector == "Government"| Employment_Sector == "Unclassified" )) %>%
  filter(Employment_Type == "private employee")

table(analysis_all_emp_wagegroup_priv$Employment_Sector, useNA = "ifany")
table(analysis_all_emp_wagegroup_priv$Employment_Status, useNA = "ifany")
table(analysis_all_emp_wagegroup_priv$Wage_group, useNA = "ifany")


# ---- 3. Save Outputs ---- #


out_file <-file.path(config$paths$processed_data, "analysis_all.rds")
saveRDS(analysis_all, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))

out_file <-file.path(config$paths$processed_data, "analysis_all_emp.rds")
saveRDS(analysis_all_emp, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))

out_file <-file.path(config$paths$processed_data, "analysis_formal_emp.rds")
saveRDS(analysis_formal_emp, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))

out_file <-file.path(config$paths$processed_data, "analysis_formal_emp_wagegroup.rds")
saveRDS(analysis_formal_emp_wagegroup, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))

out_file <-file.path(config$paths$processed_data, "analysis_formal_emp_wagegroup_priv.rds")
saveRDS(analysis_formal_emp_wagegroup_priv, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))

out_file <-file.path(config$paths$processed_data, "analysis_all_emp_wagegroup_priv.rds")
saveRDS(analysis_all_emp_wagegroup_priv, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))



