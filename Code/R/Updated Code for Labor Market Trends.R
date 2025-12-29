
#install.packages("gtsummary")
#install.packages("survey")





library(tidyverse)
library(gtsummary)
library(survey)
library(purrr)



source("E:/Research Projects/Frank Chapter 3/Code/R/check_and_fix_survey_ids.R")




load(file = "./Processed Data/Individual_quarter_ENCFT_clean.rda" )




######Analyze Survey Data Time Trends using Quarterly Estimates ########

#function to check if PSU and STRATA ID repeat across years. If yes returns information on overlap and new data with unique PSU/STRATA ID
check <- check_and_fix_survey_ids(ENCFT_quarterly_individual, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")

survey_data_fixed_id <- check$data

#define survey object
survey_design_adj <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = survey_data_fixed_id,
                               nest = TRUE)


#define breaks
cat <- levels(survey_data_fixed_id$year_quarter)
selected_breaks <- cat[seq(1, length(cat), by = 2)]


#design
design_PEA <- subset(survey_design_adj, PEA == 1)

design_OCUPADO <- subset(survey_design_adj, OCUPADO == 1)

design_working_age <- subset(survey_design_adj, age >=15 & age<=64)





###### General Labor Force #####


## Plot Labor Force Participation ##

plot_data <- svyby(~PEA + INACTIVO, ~year_quarter, design_working_age, svymean, na.rm = TRUE) %>%
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




## Plot Unemployment Rate ##

#employment/unemployment shares for working age people
plot_data <- svyby(~OCUPADO + DESOCUPADO + SUBOCUPADO, ~year_quarter, design_PEA, svymean, na.rm = TRUE) %>%
  rename(emp_share = OCUPADO,
         unemp_share = DESOCUPADO,
         underemp_share = SUBOCUPADO
         )


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


## Plot Share of Workers Formal and Informal ##

plot_data <- svyby(~Employment_Status,  ~year_quarter, design_OCUPADO, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "Employment_Status")


p_formal_informal_emp <- plot_data %>%
  select(year_quarter, Formal, Informal) %>%
  pivot_longer( cols = c("Formal", "Informal"),
                names_to = "Employment_Status",
                values_to = "shares"
  ) %>%
  ggplot(aes(x = year_quarter, y = shares, color = Employment_Status, group = Employment_Status)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Informal and Formal population over time",
    subtitle = "Share of Employed Population",
    color = "Employment Status") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## Plot Share of Workers in Each Category ##

plot_data <- svyby(~Employment_Type,  ~year_quarter, design_PEA, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "Employment_Type")


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


## Plot Share of Workers in Each Sector of Employment ##

plot_data <- svyby(~Employment_Sector,  ~year_quarter, design_PEA, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "Employment_Sector")

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





## Plot Share of Labor Force by Gender ##

plot_data <- svyby(~Sex, ~year_quarter, design_PEA, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "Sex")

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



## Plot Share of Labor Force by Education ##

plot_data <- svyby(~education,  ~year_quarter, design_PEA, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "education")



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


## Plot Average Incomes ##


#Income among employed persons
plot_data <- svyby(~primary_income + secondary_income, ~year_quarter, design_OCUPADO, svymean, na.rm = TRUE) %>%
  rename(primary_salary = primary_income,
         secondary_salary = secondary_income)


#Income amounts
p_income_amt <- plot_data %>%
  select(-starts_with("se.") ) %>%
  pivot_longer(cols = -c(year_quarter), 
               names_to = "Income_Source",
               values_to = "Income") %>%
  ggplot( aes(x = year_quarter, y = Income, color = Income_Source, group = Income_Source)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Amount",
    title = "Average Income for Employed Persons",
    subtitle = "Amount in Dominican Pesos, Average Monthly Income in a Quarter",
    group = "Income Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))


## Plot Income Shares##

p_income_share <- plot_data %>%
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
    y = "share",
    title = "Share of Income From Different Sources",
    subtitle = "Percent of Average Monthly Income in a Quarter",
    fill = "Income Source") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))





###### Formal vs Informal Employment ######




## Plot Share of Formal/Informal workers by Category ##

plot_data <- svyby(~Employment_Type,  ~year_quarter + Employment, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "Employment_Type") %>%
  filter(!(Employment == "No Work"))


#plot category shares of labor force
p_form_inform_by_cat <- plot_data %>%
  filter(!(Employment == "No Work")) %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, Employment),
                names_to = "employment_category",
                values_to = "shares"
  ) %>%
  mutate(employment_category = fct_reorder(employment_category, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_category)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~Employment) +
  labs(
    x = "Period",
    y = "Share",
    title = "Type of Workers in the Formal and Informal Sector",
    subtitle = "Share of Group",
    fill = "Worker Category") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



## Plot Share of Formal/Informal workers by Sector ##

plot_data <- svyby(~Employment_Sector,  ~year_quarter + Employment, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "Employment_Sector") %>%
  filter(!(Employment == "No Work"))

p_form_inform_by_sector <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, Employment),
                names_to = "employment_sector",
                values_to = "shares"
  ) %>%
  mutate(employment_sector = fct_reorder(employment_sector, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_sector)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~Employment) +
  labs(
    x = "Period",
    y = "Share",
    title = "Sectoral Employment Shares in the Formal and Informal Sector",
    subtitle = "Share of Group",
    fill = "Employment Sector") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



## Plot Share of Formal/Informal workers by Gender ##

plot_data <- svyby(~Sex, ~year_quarter + Employment, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "Sex") %>%
  filter(!(Employment == "No Work"))



p_form_inform_by_gender <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = c(Male, Female),
                names_to = "Sex",
                values_to =  "share"
  ) %>%
  ggplot( aes( x = year_quarter, y = share, fill = Sex)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~Employment) +
  labs(
    x = "Period",
    y = "Share",
    title = "Sex Breakdown in Formal Versus Informal Sector",
    subtitle = "Share of Group",
    fill = "Sex") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



## Plot Share of Formal/Informal workers by Education ##

plot_data <- svyby(~education,  ~year_quarter + Employment, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "education") %>%
  filter(!(Employment == "No Work"))



p_form_inform_by_educ <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, Employment),
                names_to = "eduction",
                values_to = "shares"
  ) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = fct_rev(eduction))) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~Employment) + 
  labs(
    x = "Period",
    y = "Share",
    title = "Education Levels in Formal vs Informal Sector",
    subtitle = "Share of Group",
    fill = "Education Level") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




## Plot Income of Formal/Informal workers ##


#Income among employed persons
plot_data <- svyby(~primary_income + secondary_income, ~year_quarter + Employment, survey_design_adj, svymean, na.rm = TRUE) %>%
  rename(primary_salary = primary_income,
         secondary_salary = secondary_income)


#Income amounts
p_income_amt_by_type <- plot_data %>%
  select(-starts_with("se.") ) %>%
  pivot_longer(cols = -c(year_quarter, Employment), 
               names_to = "Income_Source",
               values_to = "Income") %>%
  ggplot( aes(x = year_quarter, y = Income, color = Income_Source, group = Income_Source)) +
  geom_line() +
  facet_wrap(~Employment) +
  labs(
    x = "Period",
    y = "Amount",
    title = "Income by Employment Type",
    subtitle = "Amount in Dominican Pesos, Average Monthly Income in a Quarter",
    group = "Income Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))


## Plot Income Shares of Formal/Informal workers ##


p_income_share_by_type <- plot_data %>%
  select(-starts_with("se.") ) %>%
  mutate(total_income = primary_salary + secondary_salary,
         share_primary = primary_salary / total_income,
         share_secondary = secondary_salary / total_income ) %>%
  pivot_longer(cols = c("share_primary", "share_secondary") ,
               names_to = "Income_Source",
               values_to = "share") %>%
  ggplot( aes(x = year_quarter, y = share, fill = Income_Source)) +
  geom_bar( position = "stack", stat = "identity") +
  facet_wrap(~Employment) +
  labs(
    x = "Period",
    y = "share",
    title = "Share of Income From Different Sources by Employment Type",
    subtitle = "Percent of Average Monthly Income in a Quarter",
    fill = "Income Source") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))


###### Gender Trends ######




##plot Male and Female Participation Rate ##

plot_data <- svyby(~PEA, ~Sex + year_quarter, survey_design_adj, svymean, na.rm = TRUE)


p_participation_by_sex <- plot_data %>%
  select(year_quarter, PEA, Sex) %>%
  ggplot( aes( x = year_quarter, y = PEA, fill = Sex)) +
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





##plot Male and Female Unemployment Rate ##

plot_data <- svyby(~DESOCUPADO, ~Sex + year_quarter, subset(survey_design_adj, PEA == 1) , svymean, na.rm = TRUE)


p_unemp_by_sex <- plot_data %>%
  select(year_quarter, DESOCUPADO, Sex) %>%
  ggplot( aes( x = year_quarter, y = DESOCUPADO, fill = Sex)) +
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



##plot Male and Female Share by Empoyment Type ##

plot_data <- svyby(~Employment, ~Sex + year_quarter, subset(survey_design_adj, OCUPADO == 1) , svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "Employment") %>% 
  select(-`No Work`)

p_typework_by_sex <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer(cols = -c(year_quarter, Sex),
                names_to = "work_type",
                values_to = "share") %>%
  ggplot( aes( x = year_quarter, y = share, fill = work_type)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~Sex) +
  labs(
    x = "Period",
    y = "Share",
    title = "Employment Type by Sex",
    subtitle = "Share of Employment Type",
    fill = "Work Type") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



##plot Male and Female Share by Employment Category ##

plot_data <- svyby(~Employment_Type,  ~year_quarter + Sex, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "Employment_Type")



p_cat_by_sex <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, Sex),
                names_to = "employment_category",
                values_to = "shares"
  ) %>%
  mutate(employment_category = fct_reorder(employment_category, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_category)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~Sex) +
  labs(
    x = "Period",
    y = "Share",
    title = "Job Category by Sex",
    subtitle = "Share of of Group",
    fill = "Worker Category") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



##plot Male and Female Share by Employment Sector ##


plot_data <- svyby(~Employment_Sector,  ~year_quarter + Sex, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "Employment_Sector")

p_sector_by_sex <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, Sex),
                names_to = "employment_sector",
                values_to = "shares"
  ) %>%
  mutate(employment_sector = fct_reorder(employment_sector, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_sector)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~Sex) +
  labs(
    x = "Period",
    y = "Share",
    title = "Sectoral Employment Shares by Sex",
    subtitle = "Share of Group",
    fill = "Employment Sector") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##plot Male and Female Share by Education ##

plot_data <- svyby(~education,  ~year_quarter + Sex, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "education")



p_educ_by_sex <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, Sex),
                names_to = "eduction",
                values_to = "shares"
  ) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = fct_rev(eduction))) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~Sex) + 
  labs(
    x = "Period",
    y = "Share",
    title = "Education Levels by Sex",
    subtitle = "Share of Group",
    fill = "Education Level") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






##plot Male and Female Incomes##

plot_data <- svyby(~primary_income + secondary_income, ~year_quarter + Sex, survey_design_adj, svymean, na.rm = TRUE) %>%
  rename(primary_salary = primary_income,
         secondary_salary = secondary_income)


#Income amounts
p_income_amt_by_sex <- plot_data %>%
  select(-starts_with("se.") ) %>%
  pivot_longer(cols = -c(year_quarter, Sex), 
               names_to = "Income_Source",
               values_to = "Income") %>%
  ggplot( aes(x = year_quarter, y = Income, color = Income_Source, group = Income_Source)) +
  geom_line() +
  facet_wrap(~Sex) +
  labs(
    x = "Period",
    y = "Amount",
    title = "Income by Sex",
    subtitle = "Amount in Dominican Pesos, Average Monthly Income in a Quarter",
    group = "Income Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))


##plot Male and Female Income Shares##

p_income_share_by_sex <- plot_data %>%
  select(-starts_with("se.") ) %>%
  mutate(total_income = primary_salary + secondary_salary,
         share_primary = primary_salary / total_income,
         share_secondary = secondary_salary / total_income ) %>%
  pivot_longer(cols = c("share_primary", "share_secondary") ,
               names_to = "Income_Source",
               values_to = "share") %>%
  ggplot( aes(x = year_quarter, y = share, fill = Income_Source)) +
  geom_bar( position = "stack", stat = "identity") +
  facet_wrap(~Sex) +
  labs(
    x = "Period",
    y = "share",
    title = "Share of Income From Different Sources by Sex",
    subtitle = "Percent of Average Monthly Income in a Quarter",
    fill = "Income Source") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))


###### Trends by Education ######


## Plot Participation Rate by Educational Groups ##

plot_data <- svyby(~PEA, ~education + year_quarter, survey_design_adj, svymean, na.rm = TRUE)


p_participation_by_edu <- plot_data %>%
  select(year_quarter, PEA, education) %>%
  ggplot( aes( x = year_quarter, y = PEA, color = education, group = education)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Participation Rate by Education",
    subtitle = "Share of Working Age Population in Each Group that is Economically Active",
    color = "Education Level") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 12))





## Plot Unemployment Rate by Educational Groups ##

plot_data <- svyby(~DESOCUPADO, ~year_quarter + education , subset(survey_design_adj, PEA == 1) , svymean, na.rm = TRUE)


p_unemp_by_edu <- plot_data %>%
  select(year_quarter, DESOCUPADO, education) %>%
  ggplot( aes( x = year_quarter, y = DESOCUPADO, color = education, group = education)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Unemployment Rate by Education Level",
    subtitle = "Share of Population Unemployed",
    color = "Education Level") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 12))


## Plot Share Formal and Informal Employment by Educational Group ##

plot_data <- svyby(~Employment, ~education + year_quarter, subset(survey_design_adj, OCUPADO == 1) , svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "Employment")


p_typework_by_edu <- plot_data %>%
  select(-`No Work`) %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, education),
                names_to = "work_type",
                values_to = "share") %>%
  ggplot( aes( x = year_quarter, y = share, fill = work_type)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~education) +
  labs(
    x = "Period",
    y = "Share",
    title = "Employment Type by Education",
    subtitle = "Share of Employment Type",
    fill = "Work Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))



## Plot Category of Workers by Educational Group ##

plot_data <- svyby(~Employment_Type,  ~year_quarter + education, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "Employment_Type")


p_cat_by_edu <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, education),
                names_to = "employment_category",
                values_to = "shares"
  ) %>%
  mutate(employment_category = fct_reorder(employment_category, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_category)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~education) +
  labs(
    x = "Period",
    y = "Share",
    title = "Job Category by Education",
    subtitle = "Share of of Group",
    fill = "Worker Category") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))



## Plot Sectoral Employment by Educational Group ##

plot_data <- svyby(~Employment_Sector,  ~year_quarter + education, survey_design_adj, svymean, na.rm = TRUE)
plot_data <- clean_svy_names(plot_data, "Employment_Sector")

p_sector_by_edu <- plot_data %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c(year_quarter, education),
                names_to = "employment_sector",
                values_to = "shares"
  ) %>%
  mutate(employment_sector = fct_reorder(employment_sector, shares)) %>%
  ggplot(aes(x = year_quarter, y = shares, fill = employment_sector)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~education) +
  labs(
    x = "Period",
    y = "Share",
    title = "Sectoral Employment Shares by Education",
    subtitle = "Share of Group",
    fill = "Employment Sector") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))






## Plot Incomes by Educational Group ##


#Income among employed persons
plot_data <- svyby(~primary_income + secondary_income, ~year_quarter + education, survey_design_adj, svymean, na.rm = TRUE) %>%
  rename(primary_salary = primary_income,
         secondary_salary = secondary_income)


#Income amounts
p_income_amt_by_edu <- plot_data %>%
  select(-starts_with("se.") ) %>%
  pivot_longer(cols = -c(year_quarter, education), 
               names_to = "Income_Source",
               values_to = "Income") %>%
  ggplot( aes(x = year_quarter, y = Income, color = Income_Source, group = Income_Source)) +
  geom_line() +
  facet_wrap(~education) +
  labs(
    x = "Period",
    y = "Amount",
    title = "Income by Education",
    subtitle = "Amount in Dominican Pesos, Average Monthly Income in a Quarter",
    group = "Income Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))


## Plot Income Shares by Educational Group ##


p_income_share_by_sex <- plot_data %>%
  select(-starts_with("se.") ) %>%
  mutate(total_income = primary_salary + secondary_salary,
         share_primary = primary_salary / total_income,
         share_secondary = secondary_salary / total_income ) %>%
  pivot_longer(cols = c("share_primary", "share_secondary") ,
               names_to = "Income_Source",
               values_to = "share") %>%
  ggplot( aes(x = year_quarter, y = share, fill = Income_Source)) +
  geom_bar( position = "stack", stat = "identity") +
  facet_wrap(~education) +
  labs(
    x = "Period",
    y = "share",
    title = "Share of Income From Different Sources by Education",
    subtitle = "Percent of Average Monthly Income in a Quarter",
    fill = "Income Source") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))





###### Testing Code for Cross Tabs ######

cross_tab <- table(ENCFT_quarterly_individual$Sex, ENCFT_quarterly_individual$education)
print(cross_tab)

table1 <- ENCFT_quarterly_individual %>%
  select(Sex, education, Employment) %>%
  tbl_summary(by = Sex) %>%
  add_overall() %>%
  bold_labels()

print(table1)

table2 <- ENCFT_quarterly_individual %>%
  select(Sex, education, Employment) %>%
  tbl_summary(include =c("Sex", "education"))

print(table2)



