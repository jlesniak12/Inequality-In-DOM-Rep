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
#       3) Merge CPI and Wage data prepared in other script.
#       4) Define different concepts of income and earnings.
#       5) Deflate Incomes and minimum wage to create real variables with a
#          constant base. 2025Q2 used.
#       6) Define measures for minimum wage compliance. Convert earnings into
#          hourly wage for proper comparisons and also another measure accounting
#          for non compliance in overtime payments.
#
#
#===============================================================================



source("Code/R/setup/00_setup.R")


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
  rename( hours_worked_primary  = HORAS_TRABAJA_SEMANA_PRINCIPAL) %>%
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
    
    Principal_Category = case_when(
      CATEGORIA_PRINCIPAL == 1 ~ "Government",
      CATEGORIA_PRINCIPAL == 2 ~ "State Owned Company",
      CATEGORIA_PRINCIPAL == 3 ~ "Private Company",
      CATEGORIA_PRINCIPAL == 4 ~ "Free Trade Zone",
      CATEGORIA_PRINCIPAL == 5 ~ "Domestic Worker",
      CATEGORIA_PRINCIPAL == 6 ~ "Owner",
      CATEGORIA_PRINCIPAL == 7 ~ "Self Employed",
      CATEGORIA_PRINCIPAL == 8 ~ "non-renumerated relative",
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


#===============================================================================
# STEP 3. Merge CPI and Min Wage data in
#===============================================================================

all_ENCFT_clean <- all_ENCFT_clean %>%
  left_join(CPI, by = c("year", "quarter"))

all_ENCFT_clean <- all_ENCFT_clean %>%
  left_join(min_wage, by = c("year", "quarter", "Wage_group"))


#===============================================================================
# STEP 4. Calculate Income Concepts Used in Analysis
#===============================================================================


# 1. concepts defined for primary job

all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    
    # A) Income concepts
    salary_income_primary = INGRESO_ASALARIADO,
    comission_income_primary = COMISIONES,
    tips_income_primary = PROPINAS,
    overtime_income_primary = HORAS_EXTRA,
    other_income_primary = OTROS_PAGOS,
    
    # B) In Kind payments and other Benefits
    
    # These are amortized because survey question (B.4.4) asks about 12 month time
    vacation_benefit_primary = BONO_VACACIONES/12,
    bonus_benefit_primary = BONIFICACIONES/12,
    christmas_benefit_primary = REGALIA_PASCUAL/12,
    senority_benefit_primary = INCENTIVO_ANTIGUEDAD/12,
    other_benefit_primary = OTROS_BENEFICIOS/12,
    
    food_inkind_primary = ESPECIE_ALIMENTOS,
    housing_inkind_primary = ESPECIE_VIVIENDA,
    transport_inkind_primary = ESPECIE_TRANSPORTE,
    gas_inkind_primary = ESPECIE_COMBUSTIBLE,
    cell_inkind_primary = ESPECIE_CELULAR,
    other_inkind_primary = OTROS_ESPECIE,

    # C) Independent worker incomes
    independent_income_primary = INGRESO_INDEPENDIENTES,
    independent_benefit_primary = CONSUMO_BIENES,
    independent_inkind_primary = ESPECIE_INDEPENDIENTES,
    
    # D) Aggregate Concepts
    
    nonsalary_income_primary = comission_income_primary +  tips_income_primary + overtime_income_primary +  other_income_primary + independent_income_primary,
    
    total_income_primary = salary_income_primary + nonsalary_income_primary,
    total_benefit_primary = vacation_benefit_primary + bonus_benefit_primary + christmas_benefit_primary + senority_benefit_primary + other_benefit_primary + independent_benefit_primary,
    total_inkind_primary = food_inkind_primary + housing_inkind_primary + transport_inkind_primary + gas_inkind_primary + cell_inkind_primary + other_inkind_primary + independent_inkind_primary,
    
    total_comp_primary = total_income_primary + total_benefit_primary + total_inkind_primary,
    
    # E) Definitions of Wages for compliance
    
    #salary + comisions are counted for wage floor
    wage_compliance_primary = salary_income_primary + comission_income_primary
    
  )



# 2. concepts defined for Secondary Job

all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    
    # A) Income Concepts
    salary_income_secondary = INGRESO_ASALARIADO_SECUN,
    
    #includes tips, overtime, comissions, other payments
    other_income_secondary = OTROS_PAGOS_SECUN,
    
    # B) Other benefits
    
    #includes all categories of other payments from primary
    #also asked for a 12 month period in survey (B.7.3)
    benefits_income_secondary =  OTROS_BENEFICIOS_SECUN/12,
    
    #includes all categories from primary in kind benefits
    inkind_secondary = PAGO_ESPECIE_SECUN,
    
    # C) Independent worker incomes
    independent_income_secondary = INGRESO_INDEPENDIENTES_SECUN,
    independent_benefit_secondary  = CONSUMO_BIENES_SECUN,
    independent_inkind_secondary  = ESPECIE_INDEPENDIENTES_SECUN,
    
    # D) Aggregate Concepts
    
    nonsalary_income_secondary = other_income_secondary + independent_income_secondary,
    total_income_secondary = salary_income_secondary + nonsalary_income_secondary,
    total_benefit_secondary = benefits_income_secondary + independent_benefit_secondary,
    total_inkind_secondary = inkind_secondary +  independent_inkind_secondary,
    
    total_comp_secondary = total_income_secondary + total_benefit_secondary + total_inkind_secondary
    
  )


#3. Total Income and Other Concepts
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    
    #Defining Total Income as Primary + Secondary
    salary_income_total = salary_income_primary + salary_income_secondary,
    nonsalary_income_total =  nonsalary_income_primary + nonsalary_income_secondary,
    income_total = salary_income_total + nonsalary_income_total,
    benefits_total = total_benefit_primary + total_benefit_secondary,
    inkind_total = total_inkind_primary + total_inkind_secondary,
    
    comp_total = income_total + benefits_total + inkind_total,
      
    independent_income_total= independent_income_primary + independent_income_secondary,
    
    #income from any other jobs
    total_income_otherjobs = OTROS_TRABAJOS,
    
    
    adj_income_primary = case_when( (salary_income_primary >0) ~  salary_income_primary,
                                    (independent_income_primary >0) ~independent_income_primary,
                                    TRUE ~ 0
    )
  )





    
    
  
#===============================================================================
# STEP 5: Deflate Income and Min Wages
#===============================================================================


#NOTE Using 2025Q2 as base year
base_val <- CPI$CPI[(CPI$year == 2025 & CPI$quarter == 2)]

all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    
    # --- Real Income Values --- 
    real_salary_income_primary = salary_income_primary/base_val * 100,
    real_salary_income_secondary = salary_income_secondary/base_val * 100,
    real_salary_income_total = salary_income_total/base_val * 100,
    
    real_independent_income_primary = independent_income_primary/base_val * 100,
    real_independent_income_secondary = independent_income_secondary/base_val * 100,
    real_independent_income_total = independent_income_total/base_val * 100,
    
    real_nonsalary_income_primary = nonsalary_income_primary / base_val * 100,
    real_nonsalary_income_secondary = nonsalary_income_secondary / base_val * 100,
    real_nonsalary_income_total = nonsalary_income_total / base_val * 100,
    
    real_total_income_primary = total_income_primary / base_val * 100,
    real_total_income_secondary = total_income_secondary / base_val * 100,
    real_total_income_total = income_total/ base_val * 100,
    
    real_adj_income_primary = adj_income_primary/base_val * 100,
    
    
    # --- Real Benefits and in Kind Transfers ---
    
    real_benefits_income_primary = total_benefit_primary/base_val * 100,
    real_benefits_income_secondary =  total_benefit_secondary/base_val * 100,
    real_benefits_income_total = benefits_total/base_val * 100,
    
    real_inkind_income_primary = total_inkind_primary/base_val * 100,
    real_inkind_income_secondary =  total_inkind_secondary/base_val * 100,
    real_inkind_income_total = inkind_total/base_val * 100,
    
    # --- Real Min Wages
    real_minwage_harmonized = nom_minwage_harmonized / base_val * 100,
    real_min_wage = nom_minwage / base_val * 100,
    
    # --- Real Compliance Income (salary + commissions — what counts toward min wage floor)
    real_wage_compliance_primary = wage_compliance_primary / base_val * 100,
  )


#===============================================================================
# STEP 6. Minimum Wage Compliance Measures
#
# The Dominican minimum wage is a MONTHLY amount for a standard 44-hour week.
# The compliance question is: does the worker earn at least the monthly minimum
# wage for their hours, evaluated at the standard 44-hour rate?
#
# INCOME CONCEPTS
#   wage_compliance_primary  salary + commissions — the only components an
#                            employer may legally count toward the minimum wage
#                            floor (Art. 194 Labor Code). Used for all main
#                            compliance measures.
#
#   salary_income_primary    base salary only — stricter bound, shown as
#                            secondary measure.
#
# THREE COMPLIANCE MEASURES
#
#   MEASURE 1 — Monthly [upper bound]
#     Direct comparison: monthly income vs monthly minimum wage, no hours
#     adjustment. Overstates non-compliance for part-time workers (their
#     monthly earnings are low not because they are underpaid per hour but
#     because they work few hours). Used just to show how min wage compares to
#     earnings on aggregate.
#
#   MEASURE 2 — Earnings per hour [PRIMARY]
#     Use government provided conversion factors to create an hourly minimum wage.
#     Use the equivalent conversion factors to convert worker monthly earnings to
#     an hourly earning figure based on their reported typical weekly hours.
#     
#     Comparison: Looking at earnings per hour removes the issue of part time
#     workers being non compliant simply for being part time. However it does not
#     account for the fact that workers who do more than 44 hours a week should
#     theoretically be paid overtime (higher rate) for those hours. These workers
#     could be flagged as compliant on an hourly basis with min wage even though
#     technically some of their hours should have been paid a higher rate.
#
#   MEASURE 3 — Overtime-adjusted [robustness / appendix only]
#     Inflates hours above 44 by the legal overtime premiums (1.35× up to
#     68 hrs, 2× above 68 hrs). This asks: "What is compliance with the minimum
#     wage considering overtime payment requirements for workers who work more
#     than the standard 44 hrs?
#
#     Comparison: Comparing measure 3 and measure 2 gives a measure of overtime
#     noncompliance
#
# EXEMPTIONS FROM OVERTIME USED (affects Measure 3 only)
#   Managers (CIUO Group 1, Art. 149 LC)
#   Domestic workers (Art. 258 LC)
#   Agricultural workers (Art. 281 LC)
#
#===============================================================================


# --- Constants ---
STANDARD_HOURS  <- 44          # Legal standard work week (Art. 147 LC)
CNS_FACTOR      <- 23.83       # Legal constant for MW daily/hourly conversion
LEGAL_HOURS     <- 8           # Standard daily hours (for MW hourly formula)
WEEKS_PER_MONTH <- 52 / 12    # Calendar weeks per month (for worker hourly rate)

# --- 1. Overtime exemption flags (Measure 3 only) ---

all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    is_manager         = GRUPO_OCUPACION == "Gerentes y administradores" |
      grepl("^1", as.character(OCUPACION_PRINCIPAL_COD)),
    is_domestic        = CATEGORIA_PRINCIPAL == 5,
    is_agri            = GRUPO_RAMA == "Agrícultura y ganadería",
    is_overtime_exempt = is_manager | is_domestic | is_agri
  )


# --- 2. Compliance measures ---

all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    
    # -- Measure 1 Monthly Min Wage Compliance --
    below_min_monthly = case_when(
      is.na(real_wage_compliance_primary) |
        real_wage_compliance_primary <= 0            ~ NA_integer_,
      TRUE ~ as.integer(
        real_wage_compliance_primary < real_minwage_harmonized
      )
    ),
    
    # -- Measure 2: Earnings per Hour --
    
    # Legal Minimum Wage per Hour (The standard government yardstick)
    real_minwage_hourly = real_minwage_harmonized / (CNS_FACTOR * LEGAL_HOURS),
    
    #workers hourly wage
    real_wage_compliance_primary_hourly = real_wage_compliance_primary / (WEEKS_PER_MONTH * hours_worked_primary),
    
    
    below_min_hourly = case_when(
      is.na(real_wage_compliance_primary) |
        real_wage_compliance_primary <= 0            ~ NA_integer_,
      TRUE ~ as.integer(
        real_wage_compliance_primary_hourly < real_minwage_hourly
      )
    ),
    
    
    
    # C) Measure 3: Earnings per Hour With Overtime Exemptions
    
    eff_weekly_hours = case_when(
      # If exempt, they are paid a flat rate; 1 hour = 1 unit
      is_overtime_exempt ~ hours_worked_primary,
      
      # Standard workers: Normal hours
      !is_overtime_exempt & hours_worked_primary <= 44 ~ hours_worked_primary,
      
      # Standard workers: Overtime Tier 1 (44-68 hrs)
      !is_overtime_exempt & hours_worked_primary > 44 & hours_worked_primary <= 68 ~ 
        44 + ((hours_worked_primary - 44) * 1.35),
      
      # Standard workers: Overtime Tier 2 (68+ hrs)
      !is_overtime_exempt & hours_worked_primary > 68 ~ 
        44 + (24 * 1.35) + ((hours_worked_primary - 68) * 2.0)
    ),
    
    # Worker's Actual Hourly Rate (Standardized to the Base Price)
    real_wage_compliance_primary_hourly_eff = real_wage_compliance_primary / (WEEKS_PER_MONTH * eff_weekly_hours),
    
    
    below_min_hourly_eff = case_when(
      is.na(real_wage_compliance_primary) |
        real_wage_compliance_primary <= 0            ~ NA_integer_,
      TRUE ~ as.integer(
        real_wage_compliance_primary_hourly_eff < real_minwage_hourly
      )
    ),
    
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





