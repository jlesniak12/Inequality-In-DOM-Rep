




nom_min_wage <- read_excel("./Raw Datasets/Min Wage/min_wage_and_CPI.xlsx",
                       sheet = "Nominal Wages")

nom_min_wage <- pivot_longer(cols = !(Year | Quarter), nom_min_wage, names_to = "Wage_group", values_to = "nominal_minwage")


real_min_wage <- read_excel("./Raw Datasets/Min Wage/min_wage_and_CPI.xlsx",
                           sheet = "Real Wages")

real_min_wage <- pivot_longer(cols = !(Year | Quarter), real_min_wage, names_to = "Wage_group", values_to = "real_minwage")


min_wage <- inner_join(nom_min_wage, real_min_wage, by = c("Year", "Quarter", "Wage_group")) %>%
  rename(year = Year) %>%
  mutate(quarter = as.numeric(substr(Quarter,2,2))) %>%
  select(-Quarter)





CPI <- read_excel("./Raw Datasets/Min Wage/min_wage_and_CPI.xlsx",
                            sheet = "CPI") %>%
  rename(year = Year) %>%
  mutate(quarter = as.numeric(substr(Quarter,2,2))) %>%
  select(-Quarter)
  



save(min_wage,  file = "./Processed Data/Min_wage.RDA" )
save(CPI,  file = "./Processed Data/CPI.RDA" )
