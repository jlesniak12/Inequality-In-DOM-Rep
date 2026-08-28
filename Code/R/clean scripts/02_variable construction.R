#===============================================================================
#
# Purpose: This file takes in survey data from the ENCFT and does some data
#        cleaning as well as creates new variables that are necessary for the 
#        analysis performed in this project. It creates new cleaned survey data
#        panels that can be used for descriptive data analysis of the full
#        survey data to document trends in the Dom Rep economy overall.
#
# steps: 
#       1) Load in all data
#       2) Basic variable clean up
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



source(here::here("Code","R","clean scripts","00_setup.R"))


#===============================================================================
# STEP 1. Load Data
#===============================================================================


all_ENCFT_data <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT.rds"))
min_wage <- readRDS(file.path(config$paths$processed_data, "Min_Wage.rds"))
CPI <- readRDS(file.path(config$paths$processed_data, "CPI.rds"))
                    

# ---- Parameters from Config --- #

TIER_LEVELS <- config$TIER_LEVELS



# --- Constants for hourly wage conversion---
STANDARD_WEEK   <- config$hours$standard_week
WEEKS_PER_MONTH <- config$hours$weeks_per_month

# --- error band for compliance
ERROR           <- config$exposure$mw_compliance_tolerance





#===============================================================================
# STEP 2. Basic Variable Clean Up
#===============================================================================

# --- Add Date and Time variables --- #
all_ENCFT_clean <- all_ENCFT_data %>%
  mutate(
    date         = ym(PERIODO),
    year         = year(date),
    quarter      = quarter(date),
    month        = month(date),
    year_quarter = sprintf("%dQ%d", year, quarter)
  )


#function call to to create unique PSU/STRATA variable
all_ENCFT_clean <- check_and_fix_survey_ids(all_ENCFT_clean, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")


# --- Rename Variables Used Directly --- #

all_ENCFT_clean <- all_ENCFT_clean %>%
  rename( hours_worked_primary = HORAS_TRABAJA_SEMANA_PRINCIPAL
  )

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
    
    
    # ---- Region10 (10 Development Regions, Decreto 710-2004) ----
    # Built from province because there is no native 10-region code in the data
    # (ORDEN_REGION only carries the 4 inference regions). Mapping per
    # Diseno_muestral.pdf p.1. NOTE: survey uses the OLD province name SALCEDO
    # for what is officially Hermanas Mirabal — matched on the survey string.
    # Region10 is NOT a certified survey inference domain; it is offered as an
    # intermediate geography between province (construction) and Region4
    # (official inference domain). See exposure scripts for usage.
    Region10 = dplyr::case_when(
      DES_PROVINCIA %in% c("DISTRITO NACIONAL", "SANTO DOMINGO")            ~ "Ozama o Gran Santo Domingo",
      DES_PROVINCIA %in% c("SANTIAGO", "ESPAILLAT", "PUERTO PLATA")         ~ "Cibao Norte",
      DES_PROVINCIA %in% c("LA VEGA", "MONSEÑOR NOUEL", "SANCHEZ RAMIREZ")  ~ "Cibao Sur",
      DES_PROVINCIA %in% c("DUARTE", "SALCEDO", "MARIA TRINIDAD SANCHEZ",
                           "SAMANA")                                       ~ "Cibao Nordeste",
      DES_PROVINCIA %in% c("VALVERDE", "MONTE CRISTI", "DAJABON",
                           "SANTIAGO RODRIGUEZ")                           ~ "Cibao Noroeste",
      DES_PROVINCIA %in% c("SAN CRISTOBAL", "PERAVIA", "AZUA",
                           "SAN JOSE DE OCOA")                             ~ "Valdesia",
      DES_PROVINCIA %in% c("SAN JUAN", "ELIAS PIÑA")                       ~ "El Valle",
      DES_PROVINCIA %in% c("BARAHONA", "BAHORUCO", "INDEPENDENCIA",
                           "PEDERNALES")                                   ~ "Enriquillo",
      DES_PROVINCIA %in% c("SAN PEDRO DE MACORIS", "HATO MAYOR",
                           "MONTE PLATA")                                  ~ "Higuamo",
      DES_PROVINCIA %in% c("LA ROMANA", "LA ALTAGRACIA", "EL SEIBO")       ~ "Yuma",
      TRUE ~ NA_character_
    ),
    Region10 = factor(Region10, levels = c(
      "Ozama o Gran Santo Domingo", "Cibao Norte", "Cibao Sur",
      "Cibao Nordeste", "Cibao Noroeste", "Valdesia",
      "El Valle", "Enriquillo", "Higuamo", "Yuma"
    )),
    
    
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
    
    # ---- Education: attainment (Q9, NIVEL_ULTIMO_ANO_APROBADO) ----
    
    # Highest level ATTENDED. Input to edu7; not a completion measure.
    education = factor(as.character(GRUPO_EDUCACION)),
    education = fct_recode(education,
                           "None" = "Ninguno", "Primary" = "Primario",
                           "Secondary" = "Secundario", "University" = "Universitario"),
    education = fct_relevel(education, "None", "Primary", "Secondary", "University"),
    
    # ---- Education: level x completion (Q9 x Q18) ----
    # Cascade logic: highest CREDENTIAL wins first, then highest LEVEL ATTENDED.
    # This is what resolves the ~230-per-file cell of people with secondary
    # attainment but only a primary certificate -> "Some secondary".
    edu7 = dplyr::case_when(
      as.integer(MAYOR_NIVEL_OBTENIDO) %in% c(3,4,5,6) ~ "Tertiary complete",
      education == "University"                        ~ "Some tertiary",
      as.integer(MAYOR_NIVEL_OBTENIDO) %in% c(1,2)     ~ "Secondary complete",
      education == "Secondary"                         ~ "Some secondary",
      as.integer(MAYOR_NIVEL_OBTENIDO) == 8            ~ "Primary complete",
      education == "Primary"                           ~ "Some primary",
      education == "None"                              ~ "No schooling",
      TRUE                                             ~ NA_character_
    ),
    edu7 = factor(edu7, levels = c(
      "No schooling", "Some primary", "Primary complete",
      "Some secondary", "Secondary complete",
      "Some tertiary", "Tertiary complete")),
    
    edu4 = forcats::fct_collapse(edu7,
                                 "Less than secondary" = c("No schooling","Some primary",
                                                           "Primary complete","Some secondary"),
                                 "Secondary complete"  = "Secondary complete",
                                 "Some tertiary"       = "Some tertiary",
                                 "Tertiary complete"   = "Tertiary complete"),
    
    

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
    wage_group = dplyr::case_when(
      Firm_size == "1-10" ~ "Micro",
      Firm_size %in% c("11-20","20-30","31-50") ~ "Small",
      Firm_size == "51-99" ~ "Medium",
      Firm_size == "100+" ~ "Large",
      Firm_size == "Dont Know" ~ "Dont Know",
      is.na(Firm_size) ~ "Unknown",
      TRUE ~ "Unknown"
    ),
    wage_group = if_else(OCUPADO == 1, as.character(wage_group), NA_character_),
    wage_group = factor(wage_group, levels = c("Micro","Small","Medium","Large","Dont Know")),
    
    # --- wage group legal tier --- 
    
    
    # ---- Alternate wage group ----
    Alt_wage_group = dplyr::case_when(
      CANTIDAD_PERSONAS_TRABAJAN_EMP == 1 ~ "Independent",
      wage_group == "Micro" & CANTIDAD_PERSONAS_TRABAJAN_EMP > 1 ~ "Micro",
      TRUE ~ as.character(wage_group)
    ),
    Alt_wage_group = factor(
      Alt_wage_group,
      levels = c("Independent","Micro","Small","Medium","Large","Dont Know","Unknown")
    )
  )



all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    has_tier      = wage_group %in% TIER_LEVELS,
    
    # Explicit DK: worker was asked and answered "don't know" (code 98)
    firm_size_dk  = OCUPADO == 1 & TOTAL_PERSONAS_TRABAJAN_EMP == 98,
    
    # Blank/not asked: employed worker with no response recorded at all.
    # Covers routing (domestic workers not asked), refusal, and fieldwork gaps.
    # Distinct from DK: a rise here is a survey-operations problem; a rise in
    # DK is respondent or interviewer behaviour.
    firm_size_blank = OCUPADO == 1 & is.na(TOTAL_PERSONAS_TRABAJAN_EMP),
    
    # Combined non-response flag (matches has_tier == FALSE for employed workers)
    firm_size_nr  = firm_size_dk | firm_size_blank
  )

# Invariant: for employed workers, exactly one of has_tier / firm_size_dk /
# firm_size_blank is true.
stopifnot(
  all(
    (all_ENCFT_clean$has_tier | all_ENCFT_clean$firm_size_dk | all_ENCFT_clean$firm_size_blank) |
      all_ENCFT_clean$OCUPADO != 1,
    na.rm = TRUE
  )
)

# Invariant: wage_group's "Dont Know" level and the raw 98 code must agree.
stopifnot(all((all_ENCFT_clean$wage_group == "Dont Know") ==
                all_ENCFT_clean$firm_size_dk, na.rm = TRUE))





#===============================================================================
# STEP 3. Merge CPI and Min Wage data in
#===============================================================================

all_ENCFT_clean <- all_ENCFT_clean %>%
  left_join(CPI, by = c("year_quarter"))

all_ENCFT_clean <- all_ENCFT_clean %>%
  left_join(min_wage, by = c("year_quarter", "wage_group"))


# --- checking min wage merge
table(all_ENCFT_clean$OCUPADO, all_ENCFT_clean$nom_minwage, useNA = "ifany")
table(all_ENCFT_clean$OCUPADO, all_ENCFT_clean$nom_minwage_harmonized, useNA = "ifany")

table(all_ENCFT_clean$nom_minwage, all_ENCFT_clean$wage_group, useNA = "ifany")
table(all_ENCFT_clean$nom_minwage_harmonized, all_ENCFT_clean$wage_group, useNA = "ifany")


# -- wage group and employment
table(all_ENCFT_clean$OCUPADO, all_ENCFT_clean$wage_group, useNA = "ifany")


#===============================================================================
# STEP 4. Calculate Income Concepts Used in Analysis
#===============================================================================


# Naming: {level}_{worker scope}_{job scope}
#   worker scope: wage  = employee income only (no independent earnings)
#                 all   = employee + independent earnings
#                 indep = independent earnings only
#   job scope:    primary | secondary | all

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
    
    #for wage earners
    nonsalary_income_wage_primary = comission_income_primary +  tips_income_primary + overtime_income_primary +  other_income_primary,
    total_income_wage_primary = salary_income_primary + nonsalary_income_wage_primary,
    total_benefit_wage_primary = vacation_benefit_primary + bonus_benefit_primary + christmas_benefit_primary + senority_benefit_primary + other_benefit_primary,
    total_inkind_wage_primary = food_inkind_primary + housing_inkind_primary + transport_inkind_primary + gas_inkind_primary + cell_inkind_primary + other_inkind_primary,
    total_comp_wage_primary = total_income_wage_primary + total_benefit_wage_primary + total_inkind_wage_primary,
    
    #including independent income and self employed
    salary_income_all_primary = INGRESO_ASALARIADO + INGRESO_INDEPENDIENTES,
    nonsalary_income_all_primary = COMISIONES + PROPINAS + HORAS_EXTRA + OTROS_PAGOS,
    total_income_all_primary = salary_income_all_primary + nonsalary_income_all_primary,
    total_benefit_all_primary = vacation_benefit_primary + bonus_benefit_primary + christmas_benefit_primary + senority_benefit_primary + other_benefit_primary + independent_benefit_primary,
    total_inkind_all_primary = food_inkind_primary + housing_inkind_primary + transport_inkind_primary + gas_inkind_primary + cell_inkind_primary + other_inkind_primary + independent_inkind_primary,
    
    total_comp_all_primary = total_income_all_primary + total_benefit_all_primary + total_inkind_all_primary,
    
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
    
    #wage only
    nonsalary_income_wage_secondary = other_income_secondary,
    total_income_wage_secondary = salary_income_secondary + nonsalary_income_wage_secondary,
    total_benefit_wage_secondary = benefits_income_secondary,
    total_inkind_wage_secondary = inkind_secondary,
    
    total_comp_wage_secondary = total_income_wage_secondary + total_benefit_wage_secondary + total_inkind_wage_secondary,
    
    
    #wage and independent
    nonsalary_income_all_secondary = other_income_secondary + independent_income_secondary,
    total_income_all_secondary = salary_income_secondary + nonsalary_income_all_secondary,
    total_benefit_all_secondary = benefits_income_secondary + independent_benefit_secondary,
    total_inkind_all_secondary = inkind_secondary +  independent_inkind_secondary,
    
    total_comp_all_secondary = total_income_all_secondary + total_benefit_all_secondary + total_inkind_all_secondary
    
  )


#3. Total Income and Other Concepts
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    
    #Defining Total Income as Primary + Secondary
    
    #no independent earnings
    salary_income_wage_all = INGRESO_ASALARIADO + INGRESO_ASALARIADO_SECUN,
    nonsalary_income_wage_all = nonsalary_income_wage_primary + nonsalary_income_wage_secondary,
    total_income_wage_all = total_income_wage_primary + total_income_wage_secondary,
    total_benefit_wage_all = total_benefit_wage_primary + total_benefit_wage_secondary,
    total_inkind_wage_all = total_inkind_wage_primary + total_inkind_wage_secondary,
    total_comp_wage_all = total_income_wage_all + total_benefit_wage_all + total_inkind_wage_all,
    
    #all earnings including independent earnings
    salary_income_all_all = INGRESO_ASALARIADO + INGRESO_ASALARIADO_SECUN + INGRESO_INDEPENDIENTES + INGRESO_INDEPENDIENTES_SECUN,
    nonsalary_income_all_all = nonsalary_income_all_primary + nonsalary_income_all_secondary,
    total_income_all_all = total_income_all_primary + total_income_all_secondary,
    total_benefit_all_all = total_benefit_all_primary + total_benefit_all_secondary,
    total_inkind_all_all = total_inkind_all_primary + total_inkind_all_secondary,
    total_comp_all_all = total_income_all_all + total_benefit_all_all + total_inkind_all_all,
    
    #independent income aggregates
    total_comp_indep_primary = INGRESO_INDEPENDIENTES + CONSUMO_BIENES + ESPECIE_INDEPENDIENTES,
    total_comp_indep_secondary = INGRESO_INDEPENDIENTES_SECUN + CONSUMO_BIENES_SECUN + ESPECIE_INDEPENDIENTES_SECUN,
    
    salary_income_indep_all = INGRESO_INDEPENDIENTES + INGRESO_INDEPENDIENTES_SECUN,
    total_benefit_indep_all = CONSUMO_BIENES + CONSUMO_BIENES_SECUN,
    total_inkind_indep_all = ESPECIE_INDEPENDIENTES + PAGO_ESPECIE_SECUN,
    total_comp_indep_all   = salary_income_indep_all + total_benefit_indep_all + total_inkind_indep_all,
    
    #income from any other jobs
    total_income_otherjobs = OTROS_TRABAJOS
    
    
    )

#remove extra vars

drops <- c("INGRESO_ASALARIADO", "INGRESO_ASALARIADO_SECUN", "COMISIONES", "PROPINAS", "HORAS_EXTRA", "OTROS_PAGOS", "INGRESO_INDEPENDIENTES", "OTROS_PAGOS_SECUN", "INGRESO_INDEPENDIENTES_SECUN", 
              "BONO_VACACIONES", "BONIFICACIONES", "REGALIA_PASCUAL", "INCENTIVO_ANTIGUEDAD", "OTROS_BENEFICIOS", "CONSUMO_BIENES", "OTROS_BENEFICIOS_SECUN", "CONSUMO_BIENES_SECUN", 
              "ESPECIE_ALIMENTOS", "ESPECIE_VIVIENDA", "ESPECIE_TRANSPORTE", "ESPECIE_COMBUSTIBLE",  "ESPECIE_CELULAR", "OTROS_ESPECIE", "ESPECIE_INDEPENDIENTES", "PAGO_ESPECIE_SECUN", "ESPECIE_INDEPENDIENTES_SECUN"
              )

all_ENCFT_clean <- all_ENCFT_clean %>%
  select(-all_of(drops))
  
#===============================================================================
# STEP 5: Deflate Income and Min Wages
#===============================================================================

#set deflate from config
base_val <- CPI$CPI[CPI$year_quarter == config$CPI_base_qtr]
stopifnot(length(base_val) == 1, !is.na(base_val))

all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    
    # --- Real salary and Income Values --- 
    real_salary_income_wage_primary = salary_income_primary / CPI * base_val,
    real_salary_income_wage_secondary = salary_income_secondary / CPI * base_val,
    real_salary_income_wage_all = salary_income_wage_all / CPI * base_val,
    
    real_overtime_income_primary = overtime_income_primary / CPI * base_val,
    
    real_salary_income_indep_primary = independent_income_primary / CPI * base_val,
    real_salary_income_indep_secondary = independent_income_secondary / CPI * base_val,
    real_salary_income_indep_all = salary_income_indep_all/ CPI * base_val,
    
    real_nonsalary_income_wage_primary = nonsalary_income_wage_primary / CPI * base_val,
    real_nonsalary_income_wage_secondary = nonsalary_income_wage_secondary / CPI * base_val,
    real_nonsalary_income_wage_all = nonsalary_income_wage_all / CPI * base_val,
    
    real_total_income_wage_primary = total_income_wage_primary / CPI * base_val,
    real_total_income_wage_secondary = total_income_wage_secondary / CPI * base_val,
    real_total_income_wage_all = total_income_wage_all / CPI * base_val,
    
    real_total_income_all_primary = total_income_all_primary / CPI * base_val,
    real_total_income_all_secondary = total_income_all_secondary / CPI * base_val,
    real_total_income_all_all = total_income_all_all / CPI * base_val,
    
    
    # --- Real Benefits and in Kind Transfers ---
    
    real_benefit_wage_primary = total_benefit_wage_primary / CPI * base_val,
    real_benefit_wage_secondary =  total_benefit_wage_secondary / CPI * base_val,
    real_benefit_wage_total = total_benefit_wage_all / CPI * base_val,
    
    real_inkind_wage_primary = total_inkind_wage_primary / CPI * base_val,
    real_inkind_wage_secondary =  total_inkind_wage_secondary / CPI * base_val,
    real_inkind_wage_total = total_inkind_wage_all / CPI * base_val,
    
    # --- Real Min Wages
    real_minwage_harmonized = nom_minwage_harmonized / CPI * base_val
    
  )



#===============================================================================
# STEP 6. Minimum Wage Compliance Measures
#
# The Dominican minimum wage is a MONTHLY amount for a standard 44-hour week.
# The compliance question is: does the worker earn at least the monthly minimum
# wage for their hours, evaluated at the standard 44-hour rate?
#
# INCOME CONCEPTS
#   real_salary_income_wage_primary: base salary from the primary job. Used to
#                                   determine compliance with minimum wage.
#
#   real_overtime_income_primary: overtime pay from primary job. Used to track
#                                 compliance with overtime regulation.
#
#
# THREE COMPLIANCE MEASURES
#
#   MEASURE 1 — Monthly compliance
#     Direct comparison: monthly income vs monthly minimum wage, no hours
#     adjustment. Overstates non-compliance for part-time workers (their
#     monthly earnings are low not because they are underpaid per hour but
#     because they work few hours). Used just to show how min wage compares to
#     observed monthly earnings on aggregate.
#
#   MEASURE 2 — Earnings per hour [PRIMARY]
#     create a legal hourly minimum wage based on average weeks per month and the
#     44 standard work week. Use the equivalent conversion factors to convert 
#     worker monthly earnings to an hourly earning figure based on their reported
#     typical weekly hours. Assumes the typical weeky hours reported reflects their
#     entire month.
#     
#     Looking at earnings per hour removes the issue of part time
#     workers being non compliant simply for being part time (ie someone only
#     works 22 hours a week should actually be compared to HALF the monthly minimum
#     wage level or else there will be overstated non compliance simply for low hours)
#     
#     This partially addresses the issue of workers who work more than 44 hours a week.
#     A worker working 66 hours a week should theoretically be compared to 1.5x the legal min.
#     Doing the minimum wage hourly and setting max hours at the standard 44 hour week
#     allows us to see if the worker is below minimum wage for standard hours.
#
#     What is not accounted for here is the issue that these workers should receive
#     overtime pay for working more than 44 hours per week. This is a separate
#     legal compliance question.
#    
#
#   MEASURE 3 — Overtime Compliance estimate
#     
#     3A - Simple overtime recipient flag
#       A variable defined as 1 if a worker who is eligible for overtime based on
#       reporting a typical workweek more than 44 hours received any overtime 
#       payments at all for the month. A simple check for blatant noncompliance
#       with overtime terms.
#
#     3B - estimating overtime premium
#     For workers who typically work more than standard week calculate what their
#     monthly overtime should be assuming that standard week reflects the entire
#     month. Add this to value of minimum wage for the month. Compare this to the
#     salary + overtime payments reported for the month by the worker to check if
#     workers receive the proper amount of overtime.
#
#     Measure 3A and 3B offer 2 different and imperfect ways of addressing overtime
#     noncompliance given data limitations. It is imprecise because the survey 
#     gives overtime payments reported monthly while only reporting weekly hours.
#     It is conceivable (in fact likely) that overtime hours vary week by week in
#     ways we cannot capture here.
#     
#     Measure 3A provides a simple measure of blatant non compliance with overtime
#     laws. Measure 3B trys to identify employers who pay some overtime but not
#     enough.
#
# EXEMPTIONS FROM OVERTIME USED (affects Measure 3 only)
#   Managers (CIUO Group 1, Art. 149 LC)
#   Domestic workers (Art. 258 LC)
#   Agricultural workers (Art. 281 LC)
#
#===============================================================================



# --- 1. Overtime exemption flags ---

all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    is_manager         = GRUPO_OCUPACION == "Gerentes y administradores" |
      grepl("^1", as.character(OCUPACION_PRINCIPAL_COD)),
    is_domestic        = CATEGORIA_PRINCIPAL == 5,
    is_agri            = GRUPO_RAMA == "Agrícultura y ganadería",
    is_overtime_exempt = is_manager | is_domestic | is_agri
  )


# --- 2. Hourly Wages ---
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    
    # Legal minimum wage per hour — uses exact 52/12 on both sides to avoid
    # CNS rounding asymmetry (23.83 * 8 = 190.64 vs 52/12 * 44 = 190.667)
    real_minwage_hourly = real_minwage_harmonized / (WEEKS_PER_MONTH * STANDARD_WEEK),
    
    #workers hourly wage base for 44 hour work week
    real_salary_primary_hourly_base = real_salary_income_wage_primary / (WEEKS_PER_MONTH * pmin(hours_worked_primary, STANDARD_WEEK)),
    
    #workers hourly wage base for 44 hour work week
    real_salary_primary_hourly_obs = real_salary_income_wage_primary / (WEEKS_PER_MONTH * hours_worked_primary),
    
    
    # log wage on analysis concept (base = capped hours). Robustness arm uses
    # monthly (see below). NA for non-positive wages.
    log_hwage = if_else(real_salary_primary_hourly_base > 0,
                        log(real_salary_primary_hourly_base), NA_real_),
    log_mwage = if_else(real_salary_income_wage_primary > 0,
                        log(real_salary_income_wage_primary), NA_real_)
    
)

# --- 3. Individual-level outcome indicators ---
# One-liners on top of Employment_Status and Employment_Type. Averaged in 08
# to produce cell-level shares (informality, self-employment, compliance).
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    is_informal = as.integer(Employment_Status == "Informal"),
    is_selfemp  = as.integer(Employment_Type   == "self-employed"),  # VERIFY LEVEL
    
    is_sec_complete = as.integer(edu4 %in% c("Secondary complete", "Some tertiary", "Tertiary complete")),
    is_tert_complete = as.integer(edu4 == "Tertiary complete"),
    is_female = as.integer(Sex == "Female")
    
    
  )



# --- 3. Compliance measures ---

all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    
    # Measure 1 Monthly Min Wage Compliance
    below_min_monthly_salary = case_when(
      is.na(real_salary_income_wage_primary) |
        real_salary_income_wage_primary <= 0            ~ NA_integer_,
      TRUE ~ as.integer(
        real_salary_income_wage_primary < real_minwage_harmonized * (1 - ERROR)
      )
    ),
    
    
    # Measure 2: Earnings per Hour base wage for 44 hours/week
    below_min_hourly_base_salary  = case_when(
      is.na(real_salary_income_wage_primary) |
        real_salary_income_wage_primary <= 0            ~ NA_integer_,
      TRUE ~ as.integer(
        real_salary_primary_hourly_base < real_minwage_hourly * (1 - ERROR)
      )
    ),
    
    
    #did worker who is eligible for overtime receive anything in the month?
    overtime_receipt_flag = case_when(
      is_overtime_exempt ~ NA_integer_,
      hours_worked_primary <= STANDARD_WEEK ~ NA_integer_, # not applicable
      hours_worked_primary > STANDARD_WEEK &(is.na(real_overtime_income_primary) | real_overtime_income_primary == 0) ~ 0L,  # works OT, paid nothing
      TRUE                                                ~ 1L   # works OT, paid something
    ),
    
    #expected pay with overtime
    min_expected_total = case_when(
      is_overtime_exempt ~ 
        real_minwage_hourly * WEEKS_PER_MONTH * hours_worked_primary,
      hours_worked_primary <= STANDARD_WEEK ~ 
        real_minwage_hourly * WEEKS_PER_MONTH * hours_worked_primary,
      hours_worked_primary > STANDARD_WEEK & hours_worked_primary <= 68 ~ 
        real_minwage_hourly * WEEKS_PER_MONTH * (STANDARD_WEEK + (hours_worked_primary - STANDARD_WEEK) * 1.35),
      hours_worked_primary > 68 ~
        real_minwage_hourly * WEEKS_PER_MONTH * (STANDARD_WEEK + (24 * 1.35) + (hours_worked_primary - 68) * 2.0)
    ),
    
    total_cash = real_salary_income_wage_primary + real_overtime_income_primary,
    
    #Measure 3: Did worker receive expected overtime/salary for month?
    below_min_total = case_when(
      is.na(real_salary_income_wage_primary) |
        real_salary_income_wage_primary <= 0             ~ NA_integer_,
      TRUE ~ as.integer(total_cash < min_expected_total)
    )
  )
    

out_file <-file.path(config$paths$processed_data, "Full_ENCFT_clean.rds")
saveRDS(all_ENCFT_clean, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))




