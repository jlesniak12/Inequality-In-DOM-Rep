



load(file = "./Processed Data/Full_ENCFT.rda" )
load(file = "./Processed Data/Min_wage.rda" )
load(file = "./Processed Data/CPI.rda" )


#helper functions used in this project
source("E:/Research Projects/Frank Chapter 3/Code/R/Helper Functions.R")


###### Add Factor Variables and Labels for Analysis ######

#add dates and time factor
all_ENCFT_clean <- all_ENCFT_data %>%
  mutate(date = ym(PERIODO),
         year = year(date),
         quarter = quarter(date),
         month = month(date),
         year_quarter = paste(year, "Q", quarter, sep=""))

#Factor Variable and Label Creation
all_ENCFT_clean$year_quarter <- factor(all_ENCFT_clean$year_quarter,
                                    levels = sort(unique(all_ENCFT_clean$year_quarter)))


table(all_ENCFT_clean$GRUPO_REGION, useNA = "ifany")
table(all_ENCFT_clean$ORDEN_REGION, useNA = "ifany")


#regional factor
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(
    Region4 = dplyr::recode(
      ORDEN_REGION,
      `1` = "Gran Santo Domingo",
      `2` = "Norte",
      `3` = "Sur",
      `4` = "Este"
    ),
    Region4 = factor(Region4,
                     levels = c("Gran Santo Domingo", "Norte", "Sur", "Este"))
  )

table(all_ENCFT_clean$GRUPO_REGION, useNA = "ifany")
table(all_ENCFT_clean$Region4, useNA = "ifany")



#employment type factor
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(Employment_Status = factor(all_ENCFT_clean$GRUPO_EMPLEO, levels = sort(unique(all_ENCFT_clean$GRUPO_EMPLEO))),
         Employment_status = fct_recode(Employment_Status,
                                   "Formal" = "Empleo Formal",
                                   "Informal" = "Empleo Informal",
                                   "No Work" = "Sin empleo"))

#sectoral factor
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(Employment_Sector = factor(all_ENCFT_clean$GRUPO_RAMA, levels = sort(unique(all_ENCFT_clean$GRUPO_RAMA))),
         Employment_Sector = fct_recode(Employment_Sector,
                                 "Government" = "Administración pública y defensa",
                                 "Agriculture" = "Agrícultura y ganadería",
                                 "Commerce" = "Comercio",
                                 "Construction" = "Construcción",
                                 "Electricity and Water" = "Electricidad y agua",
                                 "Education" = "Enseñanza",
                                 "Tourism" = "Hoteles, bares y restaurantes",
                                 "Manufacturing" = "Industrias",
                                 "Finance" = "Intermediarios y financieras",
                                 "Rest of Service Sector" = "Otros servicios",
                                 "Unclassified" ="Población sin rama de actividad",
                                 "Health" = "Salud y asistencia social",
                                 "Transportation" = "Transporte y comunicaciones"))

#sectoral factor simple
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(Employment_Sector_Simplified = case_when(Employment_Sector == "Government" ~ "Government",
                                                  Employment_Sector == "Tourism" ~ "Tourism",
                                                  Employment_Sector == "Finance" ~ "Finance",
                                                  Employment_Sector == "Commerce" ~ "Commerce",
                                                  Employment_Sector == "Education" ~ "Rest of Services",
                                                  Employment_Sector == "Health" ~ "Rest of Services",
                                                  Employment_Sector == "Transportation" ~ "Rest of Services",
                                                  Employment_Sector == "Electricity and Water" ~ "Rest of Services",
                                                  Employment_Sector == "Rest of Service Sector" ~ "Rest of Services",
                                                  Employment_Sector == "Agriculture" ~ "Agriculture",
                                                  Employment_Sector == "Manufacturing" ~ "Manufacturing/Construction",
                                                  Employment_Sector == "Construction" ~ "Manufacturing/Construction",
                                                  Employment_Sector == "Unclassified" ~ "Unclassified")
  )



table(all_ENCFT_clean$Employment_Sector_Simplified, all_ENCFT_clean$Employment_Status, useNA = "ifany")


#Employment Category factor
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(Employment_Type = factor(all_ENCFT_clean$GRUPO_CATEGORIA, levels = sort(unique(all_ENCFT_clean$GRUPO_CATEGORIA))),
         Employment_Type = fct_recode(Employment_Type,
                                      "self-employed" = "Cuenta propia",
                                      "public employee" = "Empleado del estado",
                                      "private employee" = "Empleado privado",
                                      "non-renumerated relative" = "Familiar no remunerado",
                                      "owner or shareholderr" = "Patrono o socio activo",
                                      "unclassified" = "Población sin categoría"))

#Education levels
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(education = factor(all_ENCFT_clean$GRUPO_EDUCACION, levels = sort(unique(all_ENCFT_clean$GRUPO_EDUCACION))),
         education = fct_recode(education,
                                      "None" = "Ninguno",
                                      "Primary" = "Primario",
                                      "Secondary" = "Secundario",
                                      "University" = "Universitario"))


#Gender
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(Sex = factor(all_ENCFT_clean$SEXO, levels = sort(unique(all_ENCFT_clean$SEXO))),
         Sex = fct_recode(Sex,
                          "Male" = "1",
                         "Female" = "2"))

#staff
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(Firm_size = factor(all_ENCFT_clean$TOTAL_PERSONAS_TRABAJAN_EMP, levels = sort(unique(all_ENCFT_clean$TOTAL_PERSONAS_TRABAJAN_EMP))),
         Firm_size = fct_recode(Firm_size,
                          "1-10" = "1",
                          "11-20" = "2",
                          "20-30" = "3",
                          "31-50" = "4",
                          "51-99" = "5",
                          "100+" = "6",
                          "Dont Know" = "98"))



table(all_ENCFT_clean$Firm_size, useNA = "ifany")

tab <- table(all_ENCFT_clean$Firm_size, all_ENCFT_clean$Employment_Status,  useNA="ifany")

100 * prop.table(tab)


tab_employed <- table(subset(all_ENCFT_clean, OCUPADO == 1)$Firm_size,  useNA="ifany")

100 * prop.table(tab_employed)


svytable()

#min wage class
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(Wage_group = case_when(Firm_size == "1-10" ~ "micro_firm",
                               ((Firm_size == "11-20") | (Firm_size =="20-30") | (Firm_size == "31-50")) ~ "small_firm",
                               Firm_size == "51-99" ~ "medium_firm",
                               Firm_size == "Dont Know" ~ "Dont Know",
                               is.na(Firm_size) ~ "Unknown",
                               TRUE ~ "large_firm"),
         Alt_wage_group = case_when(CANTIDAD_PERSONAS_TRABAJAN_EMP == 1 ~ "Independent",
                                    ((Wage_group == "Micro") & (CANTIDAD_PERSONAS_TRABAJAN_EMP> 1)) ~ "Micro",
                                    TRUE ~ Wage_group)
         )



table(all_ENCFT_clean$Wage_group, useNA = "ifany")


##Merge in Min Wage and CPI Data ##

all_ENCFT_clean <- all_ENCFT_clean %>%
  left_join(CPI, by = c("year", "quarter"))

all_ENCFT_clean <- all_ENCFT_clean %>%
  left_join(min_wage, by = c("year", "quarter", "Wage_group"))






###### Calculate other needed Variables ######
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
         
         total_income_primary = salary_income_primary +  benefits_income_primary + independent_income_primary,
         total_income_secondary = salary_income_secondary + benefits_income_secondary + independent_income_secondary,
         total_income_other = OTROS_TRABAJOS,
         total_income_total = total_income_primary + total_income_secondary + total_income_other,
         
         recieve_any_primary = as.integer(total_income_primary >0),
         recieve_any_secondary = as.integer(total_income_secondary >0),
         recieve_any_total = as.integer(total_income_total >0)
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
         
  )



save(all_ENCFT_clean,  file = "./Processed Data/Full_ENCFT_clean.rda" )





###### Create Quarterly Aggregation for Individuals ######

ENCFT_quarterly_individual <- all_ENCFT_clean %>%
  group_by(ID_PERSONA, ID_HOGAR, ID_PROVINCIA, DES_PROVINCIA, Region4, year_quarter, year, quarter, Sex, education, Employment_Status, Employment_Sector, Employment_Sector_Simplified, Employment_Type, Firm_size, Wage_group, Alt_wage_group) %>%
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
    
    #real incomes
    real_salary_income_primary = sum(real_salary_income_primary, na.rm = TRUE),
    real_salary_income_secondary = sum(real_salary_income_secondary, na.rm = TRUE),
    real_salary_income_total = sum(real_salary_income_total, na.rm = TRUE),
    
    real_benefits_income_primary = sum(real_benefits_income_primary, na.rm = TRUE),
    real_benefits_income_secondary = sum(real_benefits_income_secondary, na.rm = TRUE),
    real_benefits_income_total = sum(real_benefits_income_total, na.rm = TRUE),
    
    real_independent_income_primary = sum(real_independent_income_primary, na.rm = TRUE),
    real_independent_income_secondary = sum(real_independent_income_secondary, na.rm = TRUE),
    real_independent_income_total = sum(real_independent_income_total, na.rm = TRUE),
    
    real_total_income_primary = sum(real_total_income_primary, na.rm = TRUE),
    real_total_income_secondary = sum(real_total_income_secondary, na.rm = TRUE),
    real_total_income_other = sum(real_total_income_other, na.rm = TRUE),
    real_total_income_total = sum(real_total_income_total, na.rm = TRUE),
    
    
    recieve_any_primary = as.integer(sum(total_income_primary > 0, na.rm = TRUE) > 0),
    recieve_any_secondary = as.integer(sum(total_income_secondary > 0, na.rm = TRUE) > 0),
    recieve_any_other = as.integer(sum(total_income_other > 0, na.rm = TRUE) > 0),
    recieve_any_total = as.integer(sum(total_income_total > 0, na.rm = TRUE) > 0),
    
    # Keep constant design variables
    UPM = first(UPM),
    ESTRATO = first(ESTRATO),
    FACTOR_EXPANSION = first(FACTOR_EXPANSION),
    
    #keep first value of other individual vars
    PEA = first(PEA),
    OCUPADO = first(OCUPADO),
    DESOCUPADO = first(DESOCUPADO),
    SUBOCUPADO = first(SUBOCUPADO),
    INACTIVO = first(INACTIVO),
    age = first(EDAD),
    bith_place = first(PAIS_NACIMIENTO),
    parent = first(PARENTESCO),
    CPI = first(CPI),
    nominal_minwage = first(nominal_minwage),
    real_minwage = first(real_minwage),
    
    .groups = "drop"
  )


tab <- table(ENCFT_quarterly_individual$Firm_size, ENCFT_quarterly_individual$Employment_Status,  useNA="ifany")

100 * prop.table(tab)


table(ENCFT_quarterly_individual$Firm_size, useNA = "ifany")


###### Generate Weight for Pooled Data ######
ENCFT_quarterly_individual <- ENCFT_quarterly_individual %>%
  group_by(ID_PERSONA) %>%
  mutate(quarters_in_sample = n_distinct(year_quarter),
         adj_weight = FACTOR_EXPANSION / quarters_in_sample) %>%
  ungroup()



save(ENCFT_quarterly_individual,  file = "./Processed Data/Individual_quarter_ENCFT_clean.rda" )



###### Create Quarterly Aggregation for Households ######

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
    
    
    #real incomes
    real_salary_income_primary = sum(real_salary_income_primary, na.rm = TRUE),
    real_salary_income_secondary = sum(real_salary_income_secondary, na.rm = TRUE),
    real_salary_income_total = sum(real_salary_income_total, na.rm = TRUE),
    
    real_benefits_income_primary = sum(real_benefits_income_primary, na.rm = TRUE),
    real_benefits_income_secondary = sum(real_benefits_income_secondary, na.rm = TRUE),
    real_benefits_income_total = sum(real_benefits_income_total, na.rm = TRUE),
    
    real_independent_income_primary = sum(real_independent_income_primary, na.rm = TRUE),
    real_independent_income_secondary = sum(real_independent_income_secondary, na.rm = TRUE),
    real_independent_income_total = sum(real_independent_income_total, na.rm = TRUE),
    
    real_total_income_primary = sum(real_total_income_primary, na.rm = TRUE),
    real_total_income_secondary = sum(real_total_income_secondary, na.rm = TRUE),
    real_total_income_other = sum(real_total_income_other, na.rm = TRUE),
    real_total_income_total = sum(real_total_income_total, na.rm = TRUE),
    
    recieve_any_primary = as.integer(sum(total_income_primary > 0, na.rm = TRUE) > 0),
    recieve_any_secondary = as.integer(sum(total_income_secondary > 0, na.rm = TRUE) > 0),
    recieve_any_other = as.integer(sum(total_income_other > 0, na.rm = TRUE) > 0),
    recieve_any_total = as.integer(sum(total_income_total > 0, na.rm = TRUE) > 0),
    
    hh_size = n(),
    
    
    # Keep constant design variables
    UPM = first(UPM),
    ESTRATO = first(ESTRATO),
    FACTOR_EXPANSION = first(FACTOR_EXPANSION),
    CPI = first(CPI),
    nominal_minwage = first(nominal_minwage),
    real_minwage = first(real_minwage),
    
    .groups = "drop"
  )


###### Generate Weight for Pooled Data ######
ENCFT_quarterly_household <- ENCFT_quarterly_household %>%
  group_by(ID_HOGAR) %>%
  mutate(
    quarters_in_sample = n_distinct(year_quarter),
    adj_weight = FACTOR_EXPANSION / quarters_in_sample
  ) %>%
  ungroup()

save(ENCFT_quarterly_household,  file = "./Processed Data/household_quarter_ENCFT_clean.rda" )



