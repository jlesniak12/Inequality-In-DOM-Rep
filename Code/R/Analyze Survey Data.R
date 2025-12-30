



source("E:/Research Projects/Frank Chapter 3/Code/R/check_and_fix_survey_ids.R")


load(file = "./Processed Data/member_data.rda" )



######Analyze Survey Data Time Trends using Quarterly Estimates ########

#function to check if PSU and STRATA ID repeat across years. If yes returns information on overlap and new data with unique PSU/STRATA ID
check <- check_and_fix_survey_ids(members_data, psu_var = "UPM", strata_var = "ID_PROVINCIA", time_var = "year_quarter")

survey_data_fixed_id <- check$data



#define survey object
survey_design_adj <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = survey_data_fixed_id,
                               nest = TRUE)


###### General Labor Force #####

#### Macro Trends ####

#share working age pop active vs inactive
plot_data <- svyby(~PEA + INACTIVO, ~year_quarter, subset(survey_design_adj, EDAD >=15 & EDAD<=64), svymean, na.rm = TRUE) %>%
  rename(active_share = PEA,
         inactive_share = INACTIVO)


#labor force participation
p_paricipation_rate <- plot_data %>%
  select(year_quarter, active_share) %>%
  ggplot( aes(x = year_quarter, y = active_share, group = 1)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Economically Active Population",
    subtitle = "Share of Total Population 15-64") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 12))





#employment/unemployment shares for working age people
plot_data <- svyby(~OCUPADO + DESOCUPADO + SUBOCUPADO, ~year_quarter, subset(survey_design_adj, PEA == 1), svymean, na.rm = TRUE) %>%
  rename(emp_share = OCUPADO,
         unemp_share = DESOCUPADO,
         underemp_share = SUBOCUPADO
         )


#unemployment chart
p_unemployment_rate <- plot_data %>%
  select(year_quarter, unemp_share) %>%
  ggplot( aes(x = year_quarter, y = unemp_share, group = 1)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Unemployed Population",
    subtitle = "Share of Laborforce") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### Employment Characteristics ####

##share formal/informal workers of  employed people##
plot_data <- svyby(~GRUPO_EMPLEO,  ~year_quarter, subset(survey_design_adj, OCUPADO == 1), svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_EMPLEO")


p_formal_informal_emp <- plot_data %>%
  select(year_quarter, Formal, Informal) %>%
  pivot_longer( cols = c("Formal", "Informal"),
                names_to = "employment_type",
                values_to = "shares"
  ) %>%
  ggplot(aes(x = year_quarter, y = shares, color = employment_type, group = employment_type)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Informal and Formal population over time",
    subtitle = "Share of Employed Population",
    color = "Employment Type") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##Category of Workers ##
plot_data <- svyby(~GRUPO_CATEGORIA,  ~year_quarter, subset(survey_design_adj, PEA == 1), svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_CATEGORIA")


#plot category shares of labor force
p_category_emp <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -year_quarter,
                names_to = "employment_category",
                values_to = "shares"
  ) %>%
  mutate(employment_category = fct_reorder(employment_category, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_category)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "Period",
    y = "Share",
    title = "Category of Workers",
    subtitle = "Share of Labor Force",
    fill = "Worker Category") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##Major Sectors of Employment ##
plot_data <- svyby(~GRUPO_RAMA,  ~year_quarter, subset(survey_design_adj, PEA == 1), svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_RAMA")

p_sector_emp <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -year_quarter,
                names_to = "employment_sector",
                values_to = "shares"
  ) %>%
  mutate(employment_sector = fct_reorder(employment_sector, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_sector)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(
    x = "Period",
    y = "Share",
    title = "Employment Share by Sector",
    subtitle = "Share of Labor Force",
    fill = "Employment Sector") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#### Worker Traits ####

##Gender in Labor Force##
plot_data <- svyby(~SEX, ~year_quarter, subset(survey_design_adj, PEA == 1), svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "SEX")

p_labor_force_by_gender <- plot_data %>%
  select(year_quarter, Male, Female) %>%
  pivot_longer( cols = c("Male", "Female"),
                names_to = "Sex",
                values_to =  "Share"
  ) %>%
  ggplot( aes( x = year_quarter, y = Share, color = Sex, group = Sex)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Breakdown of Labor Force by Sex",
    subtitle = "Share of Labor Force",
    color = "Sex") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##Education of Labor Force##
plot_data <- svyby(~GRUPO_EDUCACION,  ~year_quarter, subset(survey_design_adj, PEA == 1), svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_EDUCACION")


#plot education shares of labor force
p_education_emp <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -year_quarter,
                names_to = "employment_sector",
                values_to = "shares"
  ) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = fct_rev(employment_sector))) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "Period",
    y = "Share",
    title = "Education Levels in the Labor Force",
    subtitle = "Share of Labor Force",
    fill = "Education Level") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## Income Levels ##
plot_data <- svyby(~INGRESO_ASALARIADO + INGRESO_ASALARIADO_SECUN, ~year_quarter, subset(survey_design_adj, OCUPADO == 1), svymean, na.rm = TRUE) %>%
  rename(primary_salary = INGRESO_ASALARIADO,
         secondary_salary = INGRESO_ASALARIADO_SECUN)


#Income amounts
p_Income_amounts <- plot_data %>%
  select(-starts_with("se.") ) %>%
  pivot_longer(cols = -year_quarter,
               names_to = "Income_Source",
               values_to = "Income") %>%
  ggplot( aes(x = year_quarter, y = Income, color = Income_Source, group = Income_Source)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Amount",
    title = "Income",
    subtitle = "Amount in Dominican Pesos, Average Monthly Income in a Quarter",
    group = "Income Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Income shares among employed persons
p_Income_shares <- plot_data %>%
  select(-starts_with("se.") ) %>%
  mutate(total_income = primary_salary + secondary_salary,
         share_primary = primary_salary / total_income,
         share_secondary = secondary_salary / total_income ) %>%
  pivot_longer(cols = c("share_primary", "share_secondary") ,
               names_to = "Income_Source",
               values_to = "share") %>%
  ggplot( aes(x = year_quarter, y = share, fill = Income_Source)) +
  geom_bar( position = "stack", stat = "identity") +
  labs(
    x = "Period",
    y = "Share",
    title = "Share of Income From Different Sources",
    subtitle = "Share of Average Monthly Income in a Quarterr",
    fill = "Income Source") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



###### Formal vs Informal Employment ######



#### Employment Characteristics ####

##Category of Workers ##
plot_data <- svyby(~GRUPO_CATEGORIA,  ~year_quarter + GRUPO_EMPLEO, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_CATEGORIA") %>%
  filter(!(GRUPO_EMPLEO == "No Work"))


#plot category shares of labor force
p_form_inform_by_cat <- plot_data %>%
  filter(!(GRUPO_EMPLEO == "No Work")) %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, GRUPO_EMPLEO),
                names_to = "employment_category",
                values_to = "shares"
  ) %>%
  mutate(employment_category = fct_reorder(employment_category, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_category)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~GRUPO_EMPLEO) +
  labs(
    x = "Period",
    y = "Share",
    title = "Type of Workers in the Formal and Informal Sector",
    subtitle = "Share of Group",
    fill = "Worker Category") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



##Major Sectors of Employment ##
plot_data <- svyby(~GRUPO_RAMA,  ~year_quarter + GRUPO_EMPLEO, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_RAMA") %>%
  filter(!(GRUPO_EMPLEO == "No Work"))

p_form_inform_by_sector <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, GRUPO_EMPLEO),
                names_to = "employment_sector",
                values_to = "shares"
  ) %>%
  mutate(employment_sector = fct_reorder(employment_sector, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_sector)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~GRUPO_EMPLEO) +
  labs(
    x = "Period",
    y = "Share",
    title = "Sectoral Employment Shares in the Formal and Informal Sector",
    subtitle = "Share of Group",
    fill = "Employment Sector") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#### Worker Traits ####

### Gender ###
plot_data <- svyby(~SEX, ~year_quarter + GRUPO_EMPLEO, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "SEX") %>%
  filter(!(GRUPO_EMPLEO == "No Work"))



p_form_inform_by_gender <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = c(Male, Female),
                names_to = "Sex",
                values_to =  "share"
  ) %>%
  ggplot( aes( x = year_quarter, y = share, fill = Sex)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~GRUPO_EMPLEO) +
  labs(
    x = "Period",
    y = "Share",
    title = "Sex Breakdown in Formal Versus Informal Sector",
    subtitle = "Share of Group",
    fill = "Sex") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



### Education of Labor Force ###
plot_data <- svyby(~GRUPO_EDUCACION,  ~year_quarter + GRUPO_EMPLEO, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_EDUCACION") %>%
  filter(!(GRUPO_EMPLEO == "No Work"))


#plot education shares of labor force
p_form_inform_by_educ <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, GRUPO_EMPLEO),
                names_to = "eduction",
                values_to = "shares"
  ) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = fct_rev(eduction))) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~GRUPO_EMPLEO) + 
  labs(
    x = "Period",
    y = "Share",
    title = "Education Levels in Formal vs Informal Sector",
    subtitle = "Share of Group",
    fill = "Education Level") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Income Levels ##

#Income among employed persons
plot_data <- svyby(~INGRESO_ASALARIADO + INGRESO_ASALARIADO_SECUN, ~year_quarter + GRUPO_EMPLEO, survey_design_adj, svymean, na.rm = TRUE) %>%
  rename(primary_salary = INGRESO_ASALARIADO,
         secondary_salary = INGRESO_ASALARIADO_SECUN) %>%
  filter(!(GRUPO_EMPLEO == "No Work"))


#Income amounts
p_form_inform_income_amt <- plot_data %>%
  select(-starts_with("se.") ) %>%
  pivot_longer(cols = -c(year_quarter, GRUPO_EMPLEO), 
               names_to = "Income_Source",
               values_to = "Income") %>%
  filter(!(GRUPO_EMPLEO == "Sin empleo")) %>%
  ggplot( aes(x = year_quarter, y = Income, color = Income_Source, group = Income_Source)) +
  geom_line() +
  facet_wrap(~GRUPO_EMPLEO) +
  labs(
    x = "Period",
    y = "Amount",
    title = "Income",
    subtitle = "Amount in Dominican Pesos, Average Monthly Income in a Quarter",
    group = "Income Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Income shares among employed persons
p_form_inform_income_share <- plot_data %>%
  select(-starts_with("se.") ) %>%
  mutate(total_income = primary_salary + secondary_salary,
         share_primary = primary_salary / total_income,
         share_secondary = secondary_salary / total_income ) %>%
  pivot_longer(cols = c("share_primary", "share_secondary") ,
               names_to = "Income_Source",
               values_to = "share") %>%
  filter(!(GRUPO_EMPLEO == "Sin empleo")) %>%
  ggplot( aes(x = year_quarter, y = share, fill = Income_Source)) +
  geom_bar( position = "stack", stat = "identity") +
  facet_wrap(~GRUPO_EMPLEO) +
  labs(
    x = "Period",
    y = "share",
    title = "Share of Income From Different Sources",
    subtitle = "Percent of Average Monthly Income in a Quarter",
    fill = "Income Source") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


###### Gender Trends ######


#### Macro Trends ####


##Male/Female Participation Rate ###
plot_data <- svyby(~PEA, ~SEX + year_quarter, survey_design_adj, svymean, na.rm = TRUE)


p_participation_by_sex <- plot_data %>%
  select(year_quarter, PEA, SEX) %>%
  ggplot( aes( x = year_quarter, y = PEA, fill = SEX)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    x = "Period",
    y = "Share",
    title = "Participation Rate by Sex",
    subtitle = "Share of Working Age Population in Each Sex",
    color = "Sex") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





## Male and Female Unemployment Rate ##
plot_data <- svyby(~DESOCUPADO, ~SEX + year_quarter, subset(survey_design_adj, PEA == 1) , svymean, na.rm = TRUE)


p_unemp_by_sex <- plot_data %>%
  select(year_quarter, DESOCUPADO, SEX) %>%
  ggplot( aes( x = year_quarter, y = DESOCUPADO, fill = SEX)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    x = "Period",
    y = "Share",
    title = "Unemployment Rate by Sex",
    subtitle = "Share of Labor Force in Each Sex",
    color = "Sex") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





####Job Trends ####


##share formal/informal workers of each Sex
plot_data <- svyby(~GRUPO_EMPLEO, ~SEX + year_quarter, subset(survey_design_adj, OCUPADO == 1) , svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_EMPLEO") %>% 
  select(-`No Work`)

p_typework_by_sex <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer(cols = -c(year_quarter, SEX),
                names_to = "work_type",
                values_to = "share") %>%
  ggplot( aes( x = year_quarter, y = share, fill = work_type)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~SEX) +
  labs(
    x = "Period",
    y = "Share",
    title = "Employment Type by Sex",
    subtitle = "Share of Employment Type",
    fill = "Work Type") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



##Category of Workers ##
plot_data <- svyby(~GRUPO_CATEGORIA,  ~year_quarter + SEX, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_CATEGORIA")


#plot category shares of labor force
p_cat_by_sex <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, SEX),
                names_to = "employment_category",
                values_to = "shares"
  ) %>%
  mutate(employment_category = fct_reorder(employment_category, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_category)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~SEX) +
  labs(
    x = "Period",
    y = "Share",
    title = "Job Category by Sex",
    subtitle = "Share of of Group",
    fill = "Worker Category") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



##Major Sectors of Employment ##
plot_data <- svyby(~GRUPO_RAMA,  ~year_quarter + SEX, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_RAMA")

p_sector_by_sex <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, SEX),
                names_to = "employment_sector",
                values_to = "shares"
  ) %>%
  mutate(employment_sector = fct_reorder(employment_sector, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_sector)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~SEX) +
  labs(
    x = "Period",
    y = "Share",
    title = "Sectoral Employment Shares by Sex",
    subtitle = "Share of Group",
    fill = "Employment Sector") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




#### Worker Characteristics ####



### Education of Labor Force ###
plot_data <- svyby(~GRUPO_EDUCACION,  ~year_quarter + SEX, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_EDUCACION")


#plot education shares of labor force
p_educ_by_sex <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, SEX),
                names_to = "eduction",
                values_to = "shares"
  ) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = fct_rev(eduction))) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~SEX) + 
  labs(
    x = "Period",
    y = "Share",
    title = "Education Levels by Sex",
    subtitle = "Share of Group",
    fill = "Education Level") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Income Levels ##

#Income among employed persons
plot_data <- svyby(~INGRESO_ASALARIADO + INGRESO_ASALARIADO_SECUN, ~year_quarter + SEX, survey_design_adj, svymean, na.rm = TRUE) %>%
  rename(primary_salary = INGRESO_ASALARIADO,
         secondary_salary = INGRESO_ASALARIADO_SECUN)


#Income amounts
p_income_amt_by_sex <- plot_data %>%
  select(-starts_with("se.") ) %>%
  pivot_longer(cols = -c(year_quarter, SEX), 
               names_to = "Income_Source",
               values_to = "Income") %>%
  ggplot( aes(x = year_quarter, y = Income, color = Income_Source, group = Income_Source)) +
  geom_line() +
  facet_wrap(~SEX) +
  labs(
    x = "Period",
    y = "Amount",
    title = "Income by Sex",
    subtitle = "Amount in Dominican Pesos, Average Monthly Income in a Quarter",
    group = "Income Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Income shares among employed persons
p_form_inform_income_share <- plot_data %>%
  select(-starts_with("se.") ) %>%
  mutate(total_income = primary_salary + secondary_salary,
         share_primary = primary_salary / total_income,
         share_secondary = secondary_salary / total_income ) %>%
  pivot_longer(cols = c("share_primary", "share_secondary") ,
               names_to = "Income_Source",
               values_to = "share") %>%
  ggplot( aes(x = year_quarter, y = share, fill = Income_Source)) +
  geom_bar( position = "stack", stat = "identity") +
  facet_wrap(~SEX) +
  labs(
    x = "Period",
    y = "share",
    title = "Share of Income From Different Sources by Sex",
    subtitle = "Percent of Average Monthly Income in a Quarter",
    fill = "Income Source") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




###### Trends by Education ######


#### Macro Trends ####


## Participation Rate by education group ###
plot_data <- svyby(~PEA, ~GRUPO_EDUCACION + year_quarter, survey_design_adj, svymean, na.rm = TRUE)


p_participation_by_edu <- plot_data %>%
  select(year_quarter, PEA, GRUPO_EDUCACION) %>%
  ggplot( aes( x = year_quarter, y = PEA, color = GRUPO_EDUCACION, group = GRUPO_EDUCACION)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Participation Rate by Education",
    subtitle = "Share of Working Age Population in Each Group that is Economically Active",
    color = "Education Level") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





##Unemployment Rate by education group ##
plot_data <- svyby(~DESOCUPADO, ~year_quarter + GRUPO_EDUCACION , subset(survey_design_adj, PEA == 1) , svymean, na.rm = TRUE)


p_unemp_by_edu <- plot_data %>%
  select(year_quarter, DESOCUPADO, GRUPO_EDUCACION) %>%
  ggplot( aes( x = year_quarter, y = DESOCUPADO, color = GRUPO_EDUCACION, group = GRUPO_EDUCACION)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Unemployment Rate by Education Level",
    subtitle = "Share of Population Unemployed",
    color = "Education Level") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





####Job Trends ####


##share formal/informal workers by education
plot_data <- svyby(~GRUPO_EMPLEO, ~GRUPO_EDUCACION + year_quarter, subset(survey_design_adj, OCUPADO == 1) , svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_EMPLEO")


p_typework_by_edu <- plot_data %>%
  select(-`No Work`) %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, GRUPO_EDUCACION),
                names_to = "work_type",
                values_to = "share") %>%
  ggplot( aes( x = year_quarter, y = share, fill = work_type)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~GRUPO_EDUCACION) +
  labs(
    x = "Period",
    y = "Share",
    title = "Employment Type by Education",
    subtitle = "Share of Employment Type",
    fill = "Work Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



##Category of Workers ##
plot_data <- svyby(~GRUPO_CATEGORIA,  ~year_quarter + GRUPO_EDUCACION, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_CATEGORIA")


#plot category shares of labor force
p_cat_by_edu <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, GRUPO_EDUCACION),
                names_to = "employment_category",
                values_to = "shares"
  ) %>%
  mutate(employment_category = fct_reorder(employment_category, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_category)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~GRUPO_EDUCACION) +
  labs(
    x = "Period",
    y = "Share",
    title = "Job Category by Education",
    subtitle = "Share of of Group",
    fill = "Worker Category") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



##Major Sectors of Employment ##
plot_data <- svyby(~GRUPO_RAMA,  ~year_quarter + GRUPO_EDUCACION, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_RAMA")

p_sector_by_edu <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, GRUPO_EDUCACION),
                names_to = "employment_sector",
                values_to = "shares"
  ) %>%
  mutate(employment_sector = fct_reorder(employment_sector, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_sector)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~GRUPO_EDUCACION) +
  labs(
    x = "Period",
    y = "Share",
    title = "Sectoral Employment Shares by Education",
    subtitle = "Share of Group",
    fill = "Employment Sector") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




#### Worker Characteristics ####

## Income Levels ##

#Income among employed persons
plot_data <- svyby(~INGRESO_ASALARIADO + INGRESO_ASALARIADO_SECUN, ~year_quarter + GRUPO_EDUCACION, survey_design_adj, svymean, na.rm = TRUE) %>%
  rename(primary_salary = INGRESO_ASALARIADO,
         secondary_salary = INGRESO_ASALARIADO_SECUN)


#Income amounts
p_income_amt_by_edu <- plot_data %>%
  select(-starts_with("se.") ) %>%
  pivot_longer(cols = -c(year_quarter, GRUPO_EDUCACION), 
               names_to = "Income_Source",
               values_to = "Income") %>%
  ggplot( aes(x = year_quarter, y = Income, color = Income_Source, group = Income_Source)) +
  geom_line() +
  facet_wrap(~GRUPO_EDUCACION) +
  labs(
    x = "Period",
    y = "Amount",
    title = "Income by Education",
    subtitle = "Amount in Dominican Pesos, Average Monthly Income in a Quarter",
    group = "Income Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Income shares among employed persons
p_income_shares_by_edu <- plot_data %>%
  select(-starts_with("se.") ) %>%
  mutate(total_income = primary_salary + secondary_salary,
         share_primary = primary_salary / total_income,
         share_secondary = secondary_salary / total_income ) %>%
  pivot_longer(cols = c("share_primary", "share_secondary") ,
               names_to = "Income_Source",
               values_to = "share") %>%
  ggplot( aes(x = year_quarter, y = share, fill = Income_Source)) +
  geom_bar( position = "stack", stat = "identity") +
  facet_wrap(~GRUPO_EDUCACION) +
  labs(
    x = "Period",
    y = "share",
    title = "Share of Income From Different Sources by Education",
    subtitle = "Percent of Average Monthly Income in a Quarter",
    fill = "Income Source") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))














## Income Levels ##

#Income among employed persons
plot_data <- svyby(~INGRESO_ASALARIADO + INGRESO_ASALARIADO_SECUN, ~year_quarter, subset(survey_design_adj, OCUPADO == 1), svymean, na.rm = TRUE) %>%
  rename(primary_salary = INGRESO_ASALARIADO,
         secondary_salary = INGRESO_ASALARIADO_SECUN)


#Income amounts
p_Income_amounts <- plot_data %>%
  select(-starts_with("se.") ) %>%
  pivot_longer(cols = -year_quarter,
               names_to = "Income_Source",
               values_to = "Income") %>%
  ggplot( aes(x = year_quarter, y = Income, color = Income_Source, group = Income_Source)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Amount",
    title = "Income",
    subtitle = "Amount in Dominican Pesos, Average Monthly Income in a Quarter",
    group = "Income Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Income shares among employed persons
p_Income_shares <- plot_data %>%
  select(-starts_with("se.") ) %>%
  mutate(total_income = primary_salary + secondary_salary,
         share_primary = primary_salary / total_income,
         share_secondary = secondary_salary / total_income ) %>%
  pivot_longer(cols = c("share_primary", "share_secondary") ,
               names_to = "Income_Source",
               values_to = "share") %>%
  ggplot( aes(x = year_quarter, y = share, fill = Income_Source)) +
  geom_bar( position = "stack", stat = "identity") +
  labs(
    x = "Period",
    y = "Amount",
    title = "Income",
    subtitle = "Amount in Dominican Pesos, Average Monthly Income in a Quarter",
    fill = "Income_Source") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))











####share formal/informal workers of Labor Force###

plot_data <- svyby(~GRUPO_EMPLEO,  ~year_quarter, subset(survey_design_adj, PEA == 1), svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_EMPLEO")


#plot formal/informal share of employed population
p_formal_informal_emp <- plot_data %>%
  select(year_quarter, Formal, Informal) %>%
  pivot_longer( cols = c("Formal", "Informal"),
                names_to = "employment_type",
                values_to = "shares"
  ) %>%
  ggplot(aes(x = year_quarter, y = shares, color = employment_type, group = employment_type)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Informal and Formal population over time",
    subtitle = "Share of Labor Force",
    color = "Employment Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))












#share formal/informal workers of working people
plot_data <- svyby(~GRUPO_EMPLEO,  ~year_quarter, subset(survey_design_adj, EDAD >=15 & EDAD<=64), svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "GRUPO_EMPLEO")



#plot formal/informal share of total working age
p_formal_informal <- plot_data %>%
  select(year_quarter, Formal, Informal) %>%
  pivot_longer( cols = c("Formal", "Informal"),
                names_to = "employment_type",
                values_to = "shares"
  ) %>%
  ggplot(aes(x = year_quarter, y = shares, color = employment_type, group = employment_type)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Period",
    y = "Share",
    title = "Informal and Formal population over time",
    subtitle = "Share of Working Age Population",
    color = "Employment Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))














###### Trends in Formal vs Informal Labor ######



##### Gender Patterns of Formal/Informal Employment #####



###Male/Female breakdown of labor force
plot_data <- svyby(~SEX, ~year_quarter, subset(survey_design_adj, PEA == 1), svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "SEX")

p_labor_force_by_gender <- plot_data %>%
  select(year_quarter, Male, Female) %>%
  pivot_longer( cols = c("Male", "Female"),
                names_to = "Gender",
                values_to =  "Share"
                ) %>%
  ggplot( aes( x = year_quarter, y = Share, color = Gender, fill = Gender)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    x = "Period",
    y = "Share",
    title = "Breakdown of Labor Force by Sex",
    subtitle = "Share of Labor Force",
    color = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







#### Male and Female INformality
















plot_data <- reduce(list(share_working_nonworking, share_employed_unemployed, share_type),
                     left_join,
                      by = c("year_quarter")
)



















plot_data <- plot_data %>%
  mutate(check_PEA_inactive = active_share + inactive_share,
         check_working_unemployed = emp_share + unemp_share,
         check_share_formal_informal = formal + informal,
         check_share_formal_informal_unemp = formal + informal + unemp_share
  )



######Gender participation ######








share_pop_labor_force <- svyby(~PEA, ~year_quarter, survey_design_adj, svymean, na.rm = TRUE)
share_pop_unemployed <- svyby(~DESOCUPADO, ~year_quarter, survey_design_adj, svymean, na.rm = TRUE)
share_pop_inactive <- svyby(~INACTIVO, ~year_quarter, survey_design_adj, svymean, na.rm = TRUE)





share_PEA_inactive <- svyby(~INACTIVO, ~year_quarter, survey_design_adj, svymean, na.rm = TRUE)


share_pop_inactive <- svyby(~INACTIVO, ~year_quarter, survey_design_adj, svymean, na.rm = TRUE)


share_pop_inactive <- svyby(~INACTIVO, ~year_quarter, survey_design_adj, svymean, na.rm = TRUE)
share_pop_inactive <- svyby(~INACTIVO, ~year_quarter, survey_design_adj, svymean, na.rm = TRUE)


















#check PSU and STRATA
members_data %>%
  group_by(TRIMESTRE) %>%
  summarise(
    n_psu = n_distinct(UPM),
    n_strata = n_distinct(ID_PROVINCIA)
  )

members_data %>%
  group_by(ID_PROVINCIA) %>%
  summarize(n_quarters = n_distinct(TRIMESTRE)) %>%
  filter(n_quarters > 1)







######Analyze Survey Data TIme Trends using Quarterly Estimates ########


#test results unadjusted survey data

survey_design_orig <- svydesign(id = ~UPM , weights = ~FACTOR_EXPANSION, strata = ~ID_PROVINCIA, data = members_data,
                              nest = TRUE, survey.lonely.psu = "adjust")

income_estimate1 <- -svyby(~INGRESO_ASALARIADO, ~TRIMESTRE, survey_design_orig, svymean, na.rm = TRUE )



#test with adjusted data
survey_design_adj <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = survey_data_fixed_id,
                              nest = TRUE, survey.lonely.psu = "adjust")

income_estimate2 <- -svyby(~INGRESO_ASALARIADO, ~TRIMESTRE, survey_design_adj, svymean, na.rm = TRUE )



length(unique(survey_design_orig$cluster))
length(unique(survey_design_adj$cluster))


#use survey package
survey_data_2016Q1 <- svydesign(id = ~UPM , weights = ~FACTOR_EXPANSION, strata = ~ID_PROVINCIA, data = members_data[members_data$TRIMESTRE == "20161",],
                                nest = TRUE, survey.lonely.psu = "adjust")



















#test survey package
survey_data_full <- svydesign(id = ~UPM , weights = ~FACTOR_EXPANSION, strata = ~ID_PROVINCIA, data = members_data,
                              nest = TRUE, survey.lonely.psu = "adjust")

income_estimate1 <- -svyby(~INGRESO_ASALARIADO, ~TRIMESTRE, survey_data_full, svymean, na.rm = TRUE )


income_Estimate2 <- svymean(~INGRESO_ASALARIADO, subset(survey_data_full, TRIMESTRE == "20161"))



#use survey package
survey_data_2016Q1 <- svydesign(id = ~UPM , weights = ~FACTOR_EXPANSION, strata = ~ID_PROVINCIA, data = members_data[members_data$TRIMESTRE == "20161",],
                                nest = TRUE, survey.lonely.psu = "adjust")

income_Estimate3 <- svymean(~INGRESO_ASALARIADO, survey_data_2016Q1)







survey_data <- svydesign(id = ~UPM , weights = ~FACTOR_EXPANSION, strata = ~ID_PROVINCIA, data = members_data,
                         nest = TRUE, survey.lonely.psu = "adjust")


income_estimate <- -svyby(~INGRESO_ASALARIADO, ~TRIMESTRE, survey_data, svymean, na.rm = TRUE )


#use survey package
survey_data_2016Q1 <- svydesign(id = ~UPM , weights = ~FACTOR_EXPANSION, strata = ~ID_PROVINCIA, data = members_data[members_data$TRIMESTRE == "20161",],
                                nest = TRUE, survey.lonely.psu = "adjust")


summary(members_data$PAIS_NACIMIENTO)


cross_tab <- table(members_data$SEXO, members_data$GRUPO_EDUCACION)
print(cross_tab)

table1 <- members_data %>%
  select(SEXO, GRUPO_EDUCACION, GRUPO_EMPLEO) %>%
  tbl_summary(by = SEXO) %>%
  add_overall() %>%
  bold_labels()

print(table1)

table2 <- members_data %>%
  select(SEXO, GRUPO_EDUCACION, GRUPO_EMPLEO) %>%
  tbl_summary(include =c("SEXO", "GRUPO_EDUCACION"))

print(table2)


test <- members_data[members_data$year == 2016,]


#use survey package
survey_data_2016 <- svydesign(id = ~UPM , weights = ~FACTOR_EXPANSION, strata = ~ID_PROVINCIA, data = members_data[members_data$year == 2016,],
                              nest = TRUE, survey.lonely.psu = "adjust")


summary(survey_data_2016)

svymean(~EDAD, survey_data_2016)


srvy_table1 <- tbl_svysummary(survey_data_2016,
               include = c("GRUPO_EDUCACION", "GRUPO_EMPLEO"),
               by = SEXO,
               statistic = 
)

print(srvy_table1)




#define a survey object for each quarterly survey



#use survey package
survey_data_2016Q1 <- svydesign(id = ~UPM , weights = ~FACTOR_EXPANSION, strata = ~ID_PROVINCIA, data = members_data[members_data$TRIMESTRE == "20161",],
                              nest = TRUE, survey.lonely.psu = "adjust")


summary(survey_data_2016Q1)

svymean(~EDAD, survey_data_2016Q1)


srvy_tableQ1 <- tbl_svysummary(survey_data_2016Q1,
                              include = c("GRUPO_EDUCACION", "GRUPO_EMPLEO"),
                              by = SEXO,
                              statistic = 
)

print(srvy_tableQ1)



