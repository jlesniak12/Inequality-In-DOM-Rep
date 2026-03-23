#===============================================================================
#
# Scope: This file takes in survey data from the ENCFT and does some data
#        cleaning as well as creates new variables that are necessary for the 
#        analysis performed in this project. It creates new cleaned survey data
#        panels that can be used for descriptive data analysis of the full
#        survey data to document trends in the Dom Rep economy overall.
#
# steps: 
#       1) Load in all data
#       2) Create factors and time variables useful for later analysis.
#       3) Calculate new variables representing concepts used in later analysis.
#       4) Create an aggregated panel at the household level that may be used.
#
#
#===============================================================================



source("Code/R/00_setup.R")


#===============================================================================
# STEP 1. Load Data
#===============================================================================


all_ENCFT_data <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT.rds"))
min_wage <- readRDS(file.path(config$paths$processed_data, "Min_Wage.rds"))
CPI <- readRDS(file.path(config$paths$processed_data, "CPI.rds"))
                    


#===============================================================================
# STEP 2. Create Factor Variables
#===============================================================================

# --- Add Date and Time variables --- #
all_ENCFT_clean <- all_ENCFT_data %>%
  mutate(
    date         = ym(PERIODO),
    year         = year(date),
    quarter      = quarter(date),
    month        = month(date),
    year_quarter = paste0(year, "Q", quarter, sep="")
  )


#function call to to create unique PSU/STRATA variable
all_ENCFT_clean <- check_and_fix_survey_ids(all_ENCFT_clean, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")




# --- Create Factors and Labels Useful for Analysis Scripts --- #
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    # ---- Region ----
    ORDEN_REGION = as.integer(ORDEN_REGION),
    Region4 = dplyr::recode(
      ORDEN_REGION,
      `1` = "Gran Santo Domingo",
      `2` = "Norte",
      `3` = "Sur",
      `4` = "Este",
      .default = NA_character_
    ),
    Region4 = factor(Region4, levels = c("Gran Santo Domingo", "Norte", "Sur", "Este")),
    
    # ---- Employment status ----
    #create a version for only workers
    Employment_Status = case_when(
      OCUPADO == 1 & GRUPO_EMPLEO == "Empleo Formal"   ~ "Formal",
      OCUPADO == 1 & GRUPO_EMPLEO == "Empleo Informal" ~ "Informal",
      TRUE ~ NA_character_
    ),
    Employment_Status = factor(Employment_Status, levels = c("Formal","Informal")),
    
    
    # Coerce to character to make fct_recode robust regardless of source type
    Employment_Status_All = factor(as.character(GRUPO_EMPLEO)),
    Employment_Status_All = fct_recode(
      Employment_Status_All,
      "Formal"   = "Empleo Formal",
      "Informal" = "Empleo Informal",
      "No Work"  = "Sin empleo"
    ),
    Employment_Status_All = fct_relevel(Employment_Status_All, "Formal", "Informal", "No Work"),
    

    
    # ---- Employment sector (detailed) ----
    Employment_Sector = factor(as.character(GRUPO_RAMA)),
    Employment_Sector = fct_recode(
      Employment_Sector,
      "Government"              = "Administración pública y defensa",
      "Agriculture"             = "Agrícultura y ganadería",
      "Commerce"                = "Comercio",
      "Construction"            = "Construcción",
      "Electricity and Water"   = "Electricidad y agua",
      "Education"               = "Enseñanza",
      "Tourism"                 = "Hoteles, bares y restaurantes",
      "Manufacturing"           = "Industrias",
      "Finance"                 = "Intermediarios y financieras",
      "Rest of Service Sector"  = "Otros servicios",
      "Unclassified"            = "Población sin rama de actividad",
      "Health"                  = "Salud y asistencia social",
      "Transportation"          = "Transporte y comunicaciones"
    ),
    
    # ---- Employment sector (simplified) ----
    Employment_Sector_Simplified = dplyr::case_when(
      Employment_Sector %in% c("Government") ~ "Government",
      Employment_Sector %in% c("Tourism") ~ "Tourism",
      Employment_Sector %in% c("Finance") ~ "Finance",
      Employment_Sector %in% c("Commerce") ~ "Commerce",
      Employment_Sector %in% c("Agriculture") ~ "Agriculture",
      Employment_Sector %in% c("Manufacturing", "Construction") ~ "Manufacturing/Construction",
      Employment_Sector %in% c("Education", "Health", "Transportation",
                               "Electricity and Water", "Rest of Service Sector") ~ "Rest of Services",
      Employment_Sector %in% c("Unclassified") ~ "Unclassified",
      TRUE ~ NA_character_
    ),
    Employment_Sector_Simplified = factor(
      Employment_Sector_Simplified,
      levels = c("Government", "Tourism", "Finance", "Commerce",
                 "Agriculture", "Manufacturing/Construction", "Rest of Services", "Unclassified")
    ),
    
    # ---- Employment category/type ----
    Employment_Type = factor(as.character(GRUPO_CATEGORIA)),
    Employment_Type = fct_recode(
      Employment_Type,
      "self-employed"           = "Cuenta propia",
      "public employee"         = "Empleado del estado",
      "private employee"        = "Empleado privado",
      "non-renumerated relative"= "Familiar no remunerado",
      "owner or shareholder"    = "Patrono o socio activo",
      "unclassified"            = "Población sin categoría"
    ),
    
    # ---- Education ----
    education = factor(as.character(GRUPO_EDUCACION)),
    education = fct_recode(
      education,
      "None"       = "Ninguno",
      "Primary"    = "Primario",
      "Secondary"  = "Secundario",
      "University" = "Universitario"
    ),
    education = fct_relevel(education, "None", "Primary", "Secondary", "University"),
    
    # ---- Sex ----
    Sex = factor(as.character(SEXO)),
    Sex = fct_recode(Sex, "Male" = "1", "Female" = "2"),
    Sex = fct_relevel(Sex, "Male", "Female"),
    
    # ---- Firm size ----
    Firm_size = factor(as.character(TOTAL_PERSONAS_TRABAJAN_EMP)),
    Firm_size = fct_recode(
      Firm_size,
      "1-10"       = "1",
      "11-20"      = "2",
      "20-30"      = "3",
      "31-50"      = "4",
      "51-99"      = "5",
      "100+"       = "6",
      "Dont Know"  = "98"
    ),
    Firm_size = fct_relevel(Firm_size, "1-10","11-20","20-30","31-50","51-99","100+","Dont Know"),
    
    # ---- Wage group (based on firm size, should be undefined if not working) ----
    Wage_group = dplyr::case_when(
      Firm_size == "1-10" ~ "Micro",
      Firm_size %in% c("11-20","20-30","31-50") ~ "Small",
      Firm_size == "51-99" ~ "Medium",
      Firm_size == "100+" ~ "Large",
      Firm_size == "Dont Know" ~ "Dont Know",
      is.na(Firm_size) ~ "Unknown",
      TRUE ~ "Unknown"
    ),
    Wage_group = if_else(OCUPADO == 1, as.character(Wage_group), NA_character_),
    Wage_group = factor(Wage_group, levels = c("Micro","Small","Medium","Large","Dont Know")),
    
    # ---- Alternate wage group ----
    Alt_wage_group = dplyr::case_when(
      CANTIDAD_PERSONAS_TRABAJAN_EMP == 1 ~ "Independent",
      Wage_group == "Micro" & CANTIDAD_PERSONAS_TRABAJAN_EMP > 1 ~ "Micro",
      TRUE ~ as.character(Wage_group)
    ),
    Alt_wage_group = factor(
      Alt_wage_group,
      levels = c("Independent","Micro","Small","Medium","Large","Dont Know","Unknown")
    )
  )


# --- Merge Min Wage and CPI Data in --- #

all_ENCFT_clean <- all_ENCFT_clean %>%
  left_join(CPI, by = c("year", "quarter"))

all_ENCFT_clean <- all_ENCFT_clean %>%
  left_join(min_wage, by = c("year", "quarter", "Wage_group"))


#===============================================================================
# STEP 3. Calculate new variables and concepts used in analysis
#===============================================================================

# --- Calculate Needed Variables for Analysis --- #

#income variables
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(salary_income_primary = INGRESO_ASALARIADO,
         salary_income_secondary = INGRESO_ASALARIADO_SECUN,
         salary_income_total = salary_income_primary + salary_income_secondary,
         
         benefits_income_primary = COMISIONES + PROPINAS + HORAS_EXTRA + OTROS_PAGOS,
         benefits_income_secondary =  OTROS_PAGOS_SECUN,
         benefits_income_total = benefits_income_primary + benefits_income_secondary,
         
         independent_income_primary = INGRESO_INDEPENDIENTES,
         independent_income_secondary = INGRESO_INDEPENDIENTES_SECUN,
         independent_income_total = independent_income_primary + independent_income_secondary,
         
         total_income_primary = salary_income_primary  + independent_income_primary,
         total_income_secondary = salary_income_secondary + independent_income_secondary,
         total_income_other = OTROS_TRABAJOS,
         total_income_total = total_income_primary + total_income_secondary + total_income_other,
         
         recieve_any_primary = as.integer(total_income_primary >0),
         recieve_any_secondary = as.integer(total_income_secondary >0),
         recieve_any_total = as.integer(total_income_total >0),
         
         adj_income_primary = case_when( (salary_income_primary >0) ~  salary_income_primary,
                                         (independent_income_primary >0) ~independent_income_primary,
                                         TRUE ~ 0
         )
  )

#deflate incomes
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(real_salary_income_primary = salary_income_primary/CPI * 100,
         real_salary_income_secondary = salary_income_secondary/CPI * 100,
         real_salary_income_total = salary_income_total/CPI * 100,
         
         real_benefits_income_primary = benefits_income_primary/CPI * 100,
         real_benefits_income_secondary =  benefits_income_secondary/CPI * 100,
         real_benefits_income_total = benefits_income_total/CPI * 100,
         
         real_independent_income_primary = independent_income_primary/CPI * 100,
         real_independent_income_secondary = independent_income_secondary/CPI * 100,
         real_independent_income_total = independent_income_total/CPI * 100,
         
         real_total_income_primary = total_income_primary/CPI * 100,
         real_total_income_secondary = total_income_secondary/CPI * 100,
         real_total_income_other = total_income_other/CPI * 100,
         real_total_income_total = total_income_total/CPI * 100,
         
         real_adj_income_primary = adj_income_primary/CPI * 100
         
  )


#generate a weight for annual pooled data at an individual level
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(weight_annual  = FACTOR_EXPANSION /4,
         weight_quarter = FACTOR_EXPANSION ) %>%
  ungroup()



out_file <-file.path(config$paths$processed_data, "Full_ENCFT_clean.rds")
saveRDS(all_ENCFT_clean, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))



#===============================================================================
# STEP 4. Create an aggregated Panel for Household Level Analysis
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
  mutate(real_salary_income_primary = salary_income_primary/CPI * 100,
         real_salary_income_secondary = salary_income_secondary/CPI * 100,
         real_salary_income_total = salary_income_total/CPI * 100,
         
         real_benefits_income_primary = benefits_income_primary/CPI * 100,
         real_benefits_income_secondary =  benefits_income_secondary/CPI * 100,
         real_benefits_income_total = benefits_income_total/CPI * 100,
         
         real_independent_income_primary = independent_income_primary/CPI * 100,
         real_independent_income_secondary = independent_income_secondary/CPI * 100,
         real_independent_income_total = independent_income_total/CPI * 100,
         
         real_total_income_primary = total_income_primary/CPI * 100,
         real_total_income_secondary = total_income_secondary/CPI * 100,
         real_total_income_other = total_income_other/CPI * 100,
         real_total_income_total = total_income_total/CPI * 100,
         
         real_adj_income_primary = adj_income_primary/CPI * 100
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





