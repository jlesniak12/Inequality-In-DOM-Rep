

library(tidyverse)
library(gtsummary)
library(survey)
library(purrr)
library(ggplot2)

library(patchwork)


#helper functions used in this project
source("E:/Research Projects/Frank Chapter 3/Code/R/Helper Functions.R")




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
  

#design to study only Workers
design <- subset(survey_design_adj, OCUPADO == 1)





###### Real Income Trends Over Time ######

#define variables of interest to track over time
vars <- c("real_total_income_primary", "real_salary_income_primary", "real_benefits_income_primary", "real_independent_income_primary",
          "real_total_income_secondary", "real_salary_income_secondary", "real_benefits_income_secondary", "real_independent_income_secondary",
          "real_total_income_total", "real_salary_income_total", "real_benefits_income_total", "real_independent_income_total")

#variables to do by group analysis
groups <- c("Sex", "education", "Employment_Status", "Employment_Sector_Simplified", "Employment_Type", "Firm_size", "Wage_group")



## Plotting Variables over Time ##

#defining plot titles
titles <- c("Total Income From Primary Job", "Salary Income From Primary Job", "Benefits From Primary Job", "Independent Income from Primary Job",
            "Total Income From Secondary Job", "Salary Income From Second Job", "Benefits From Secondary Job", "Independent Income from Secondary Job",
            "Total Income From All Jobs", "Salary Income From All Jobs", "Benefits From All Jobs", "Independent Income from All Jobs")

names(titles) <- vars


subtitles <- c("Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos",
               "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos",
               "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos")


names(subtitles) <- vars


#list to store plots
plot_list <- list()

#list to store data
data_list <- list()



#### Plots for Simple Average Over Time without groups ####


start_avg_overtime <- Sys.time()

inc_time <- svyby_multi_numeric_fast(
  design   = design,
  vars     = vars,
  by_time  = "year_quarter",
  fun   = svymean,
  vartype = "se"
)

for (var in vars){
  
  plot_data <- inc_time %>%
    filter(variable == var)
  
  plot_name <- paste(var)
  
  plot <- plot_data %>%
    ggplot( aes(x = year_quarter, y = estimate, color = variable, group = variable )) +
    geom_line() +
    labs(
      x = "Period",
      y = "Amount",
      title = titles[[var]],
      subtitle = subtitles[[var]],
      color = var
    ) +
    scale_x_discrete(breaks = selected_breaks) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          strip.text = element_text(size = 12))
  
  plot_list[[plot_name]] <- plot
}

end_avg_overtime <- Sys.time()
time_avg <- end_avg_overtime - start_avg_overtime

time_avg



#### Plots for Average over time with sub groupings ####

start_avg_by_group <- Sys.time()


for (grp in groups) {
  
  data_name <- paste("inc_by_", grp)
  
  data_list[[data_name]] <- svyby_multi_numeric_fast_bygroup (
    design   = design,
    vars     = vars,
    by_time  = "year_quarter",
    by_group = grp,
    fun   = svymean,
    vartype = "se"
  )
  
  for (var in vars){
    
    plot_data <- data_list[[data_name]] %>%
      filter(variable == var)
    
    plot_name <- paste(var, "_by_", grp, sep = "")
    
    plot <- plot_data %>%
      ggplot( aes(x = year_quarter, y = estimate, color = .data[[grp]], group = .data[[grp]] )) +
      geom_line() +
      labs(
        x = "Period",
        y = "Amount",
        title = titles[[var]],
        subtitle = subtitles[[var]],
        color = grp
      ) +
      scale_x_discrete(breaks = selected_breaks) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            strip.text = element_text(size = 12))
    
    plot_list[[plot_name]] <- plot
  }
}

end_by_group <- Sys.time()

time_avg_by_group <- end_by_group - start_avg_by_group
time_avg_by_group



#Printing to PDF
print_order <- c("real_total_income_primary", "real_total_income_secondary", "real_total_income_total", 
                 "real_salary_income_primary", "real_salary_income_secondary", "real_salary_income_total", 
                 "real_benefits_income_primary", "real_benefits_income_secondary", "real_benefits_income_total",
                 "real_independent_income_primary", "real_independent_income_secondary", "real_independent_income_total",
                 "real_total_income_total_by_Sex", "real_total_income_total_by_education", 
                 "real_total_income_total_by_Employment_Status", "real_total_income_total_by_Employment_Type", "real_total_income_total_by_Employment_Sector_Simplified", "real_total_income_total_by_Wage_group",
                 "real_salary_income_total_by_Sex", "real_salary_income_total_by_education", 
                 "real_salary_income_total_by_Employment_Status", "real_salary_income_total_by_Employment_Type", "real_salary_income_total_by_Employment_Sector_Simplified", "real_salary_income_total_by_Wage_group",
                 "real_benefits_income_total_by_Sex", "real_benefits_income_total_by_education", 
                 "real_benefits_income_total_by_Employment_Status", "real_benefits_income_total_by_Employment_Type", "real_benefits_income_total_by_Employment_Sector_Simplified", "real_benefits_income_total_by_Wage_group",
                 "real_independent_income_total_by_Sex", "real_independent_income_total_by_education", 
                 "real_independent_income_total_by_Employment_Status", "real_independent_income_total_by_Employment_Type", "real_independent_income_total_by_Employment_Sector_Simplified", "real_independent_income_total_by_Wage_group"
                 )

print_section_header <- list("Trends in Types of Income Over Time" = 1:12,
                             "Trends in Total Income by Group" = 13:18,
                             "Trends in Total Salary Income by Group" = 19:24,
                             "Trends in Total Benefits Value by Group" = 25:30,
                             "Trends in Total Independent Income by Group" = 31:36)


#note function only prints plots specified in order vector if given
save_plots_pdf_grid(plot_list, file = "./Outputs/Charts and Tables/Real_Income_Trends_Over_Time.pdf", ncol = 1, nrow = 2,
                    order = print_order,
                    sections = print_section_header,
                    break_sections = TRUE)



###### Save Individual Graphics ######

for (plot in names(plot_list)) {
  
  filename <- paste()
  ggsave(filename = paste("./Outputs/Charts and Tables/Individual Charts/Income/Real/", plot, ".png", sep = ""),
         plot = plot_list[[plot]],
         width = 8,
         height = 6,
         dpi = 300)
}




###### Nominal Income Trends Over Time ######

#define variables of interest to track over time
vars <- c("total_income_primary", "salary_income_primary", "benefits_income_primary", "independent_income_primary", 
          "total_income_secondary", "salary_income_secondary", "benefits_income_secondary", "independent_income_secondary",
          "total_income_total", "salary_income_total", "benefits_income_total", "independent_income_total")



#variables to do by group analysis
groups <- c("Sex", "education", "Employment_Status", "Employment_Sector_Simplified", "Employment_Type", "Firm_size", "Wage_group")



## Plotting Variables over Time ##

#defining plot titles
titles <- c("Total Income From Primary Job", "Salary Income From Primary Job", "Benefits From Primary Job", "Independent Income Primary Job", 
            "Total Income From Secondary Job", "Salary Income From Second Job", "Benefits From Secondary Job", "Independent Income Secondary Job",
            "Total Income From All Jobs", "Salary Income From All Jobs", "Benefits From All Jobs", "Independent Income All Jobs")

names(titles) <- vars


subtitles <- c("Monthly Value in DOM Pesos", "Monthly Value in DOM Pesos", "Monthly Value in DOM Pesos", "Monthly Value in DOM Pesos",
               "Monthly Value in DOM Pesos", "Monthly Value in DOM Pesos", "Monthly Value in DOM Pesos", "Monthly Value in DOM Pesos",
               "Monthly Value in DOM Pesos", "Monthly Value in DOM Pesos", "Monthly Value in DOM Pesos", "Monthly Value in DOM Pesos")


names(subtitles) <- vars


#list to store plots
plot_list <- list()

#list to store data
data_list <- list()



#### Plots for Simple Average Over Time without groups ####


start_avg_overtime <- Sys.time()

inc_time <- svyby_multi_numeric_fast(
  design   = design,
  vars     = vars,
  by_time  = "year_quarter",
  fun   = svymean,
  vartype = "se"
)

for (var in vars){
  
  plot_data <- inc_time %>%
    filter(variable == var)
  
  plot_name <- paste(var)
  
  plot <- plot_data %>%
    ggplot( aes(x = year_quarter, y = estimate, color = variable, group = variable )) +
    geom_line() +
    labs(
      x = "Period",
      y = "Amount",
      title = titles[[var]],
      subtitle = subtitles[[var]],
      color = var
    ) +
    scale_x_discrete(breaks = selected_breaks) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          strip.text = element_text(size = 12))
  
  plot_list[[plot_name]] <- plot
}

end_avg_overtime <- Sys.time()
time_avg <- end_avg_overtime - start_avg_overtime

time_avg



#### Plots for Average over time with sub groupings ####

start_avg_by_group <- Sys.time()


for (grp in groups) {
  
  data_name <- paste("inc_by_", grp)
  
  data_list[[data_name]] <- svyby_multi_numeric_fast_bygroup (
    design   = design,
    vars     = vars,
    by_time  = "year_quarter",
    by_group = grp,
    fun   = svymean,
    vartype = "se"
  )
  
  for (var in vars){
    
    plot_data <- data_list[[data_name]] %>%
      filter(variable == var)
    
    plot_name <- paste(var, "_by_", grp, sep = "")
    
    plot <- plot_data %>%
      ggplot( aes(x = year_quarter, y = estimate, color = .data[[grp]], group = .data[[grp]] )) +
      geom_line() +
      labs(
        x = "Period",
        y = "Amount",
        title = titles[[var]],
        subtitle = subtitles[[var]],
        color = grp
      ) +
      scale_x_discrete(breaks = selected_breaks) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            strip.text = element_text(size = 12))
    
    plot_list[[plot_name]] <- plot
  }
}

end_by_group <- Sys.time()

time_avg_by_group <- end_by_group - start_avg_by_group
time_avg_by_group



#Printing to PDF
print_order <- c("total_income_primary", "total_income_secondary", "total_income_total",
                 "salary_income_primary", "salary_income_secondary", "salary_income_total",
                 "benefits_income_primary", "benefits_income_secondary", "benefits_income_total",
                 "independent_income_primary", "independent_income_secondary", "independent_income_total",
                 "total_income_total_by_Sex", "total_income_total_by_education", 
                 "total_income_total_by_Employment_Status", "total_income_total_by_Employment_Type", "total_income_total_by_Employment_Sector_Simplified", "total_income_total_by_Wage_group",
                 "salary_income_total_by_Sex", "salary_income_total_by_education", 
                 "salary_income_total_by_Employment_Status", "salary_income_total_by_Employment_Type", "salary_income_total_by_Employment_Sector_Simplified", "salary_income_total_by_Wage_group",
                 "benefits_income_total_by_Sex", "benefits_income_total_by_education", 
                 "benefits_income_total_by_Employment_Status", "benefits_income_total_by_Employment_Type", "benefits_income_total_by_Employment_Sector_Simplified", "benefits_income_total_by_Wage_group",
                 "independent_income_total_by_Sex", "independent_income_total_by_education", 
                 "independent_income_total_by_Employment_Status", "independent_income_total_by_Employment_Type", "independent_income_total_by_Employment_Sector_Simplified", "independent_income_total_by_Wage_group"
)

print_section_header <- list("Trends in Types of Income Over Time" = 1:12,
                             "Trends in Total Income by Group" = 13:18,
                             "Trends in Total Salary Income by Group" = 19:24,
                             "Trends in Total Benefits Value by Group" = 25:30,
                             "Trends in Total Independent Income by Group" = 31:36)


#note function only prints plots specified in order vector if given
save_plots_pdf_grid(plot_list, file = "./Outputs/Charts and Tables/Nominal_Income_Trends_Over_Time.pdf", ncol = 1, nrow = 2,
                    order = print_order,
                    sections = print_section_header,
                    break_sections = TRUE)


###### Save Individual Graphics ######

for (plot in names(plot_list)) {
  
  filename <- paste()
  ggsave(filename = paste("./Outputs/Charts and Tables/Individual Charts/Income/Nominal/", plot, ".png", sep = ""),
         plot = plot_list[[plot]],
         width = 8,
         height = 6,
         dpi = 300)
}






###### Part 1. Assessing Sources of Income in the Full Labor Force ######


## Plot Share of Labor Force Receiving Income ##

inc_type_share_workers <-svyby(~recieve_any_primary, ~year_quarter, design = subset(survey_design_adj, PEA == 1) , svymean)


p_inc_shares <- inc_type_share_workers %>%
  select(-starts_with("se")) %>%
  pivot_longer( cols = -c("year_quarter"),
                names_to = "income_type",
                values_to = "shares"
  ) %>%
  ggplot(aes(x = year_quarter, y = shares, color = income_type, group = income_type)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Share of Labor Force Recieving Income",
    subtitle = "Share of Economically Active Population",
    color = "Income Type") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(breaks = selected_breaks) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))



## Plot Average Income of Labor Force Participants ##

inc_type_amounts_workers <-svyby(~total_income_primary + total_income_secondary + total_income_total, ~year_quarter, design = subset(survey_design_adj, PEA == 1) , svymean)


p_inc_amounts <- inc_type_amounts_workers %>%
  select(-starts_with("se.")) %>%
  pivot_longer( cols = -c("year_quarter"),
                names_to = "income_type",
                values_to = "amount"
  ) %>%
  ggplot(aes(x = year_quarter, y = amount, color = income_type, group = income_type)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Amount",
    title = "Average Income of Labor Force by Type",
    subtitle = "Domincan Pesos",
    color = "Income Type") +
  scale_x_discrete(breaks = selected_breaks) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))







###### Part 2 Assessing Income by Worker Traits ######



## Plot Avg Income in Formal and Informal Work ##
plot_data <-svyby(~total_income_primary, ~year_quarter + Employment, design = subset(survey_design_adj, PEA == 1) , svymean) %>%
  filter(!(Employment == "No Work"))

p_avg_income_by_emp_type <- plot_data %>%
  select(-starts_with(".se")) %>%
  ggplot(aes(x = year_quarter, y = total_income_primary, color = Employment, group = Employment)) + 
  geom_line() +
  labs(
    x = "Period",
    y = "Amount",
    title = "Average Income of Labor Force by Type",
    subtitle = "Domincan Pesos",
    color = "Employment_Type") +
  scale_x_discrete(breaks = selected_breaks) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))




## Plot Avg Income by Gender ##
plot_data <-svyby(~total_income_primary, ~year_quarter + Sex, design = subset(survey_design_adj, PEA == 1) , svymean)


p_avg_income_by_sex <- plot_data %>%
  select(-starts_with(".se")) %>%
  ggplot(aes(x = year_quarter, y = total_income_primary, color = Sex, group = Sex)) + 
  geom_line() +
  labs(
    x = "Period",
    y = "Amount",
    title = "Average Income of Labor Force by Sex",
    subtitle = "Domincan Pesos",
    color = "Sex") +
  scale_x_discrete(breaks = selected_breaks) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))





## Plot Avg Income by Gender ##
plot_data_emp <-svyby(~total_income_primary, ~year_quarter + Sex, design = subset(survey_design_adj, OCUPADO == 1) , svymean)


p_avg_income_by_sex_employed <- plot_data %>%
  select(-starts_with(".se")) %>%
  ggplot(aes(x = year_quarter, y = total_income_primary, color = Sex, group = Sex)) + 
  geom_line() +
  labs(
    x = "Period",
    y = "Amount",
    title = "Average Income of Labor Force by Sex",
    subtitle = "Domincan Pesos",
    color = "Sex") +
  scale_x_discrete(breaks = selected_breaks) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))






## Plot Avg Income by Education ##
plot_data <-svyby(~total_income_primary, ~year_quarter + education, design = subset(survey_design_adj, PEA == 1) , svymean)


p_avg_income_by_education <- plot_data %>%
  select(-starts_with(".se")) %>%
  ggplot(aes(x = year_quarter, y = total_income_primary, color = education, group = education)) + 
  geom_line() +
  labs(
    x = "Period",
    y = "Amount",
    title = "Average Income of Labor Force by Education Level",
    subtitle = "Domincan Pesos",
    color = "Education") +
  scale_x_discrete(breaks = selected_breaks) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))



## Plot Avg Income by Sector##
plot_data <-svyby(~primary_income, ~year_quarter + Employment_Sector, design = subset(survey_design_adj, PEA == 1) , svymean)


p_avg_income_by_sector <- plot_data %>%
  select(-starts_with(".se")) %>%
  ggplot(aes(x = year_quarter, y = primary_income, color = Employment_Sector, group = Employment_Sector)) + 
  geom_line() +
  labs(
    x = "Period",
    y = "Amount",
    title = "Average Income of Labor Force by Economic Sector",
    subtitle = "Domincan Pesos",
    color = "Employment Sector") +
  scale_x_discrete(breaks = selected_breaks) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))



## Plot Avg Income by Type of Role##
plot_data <-svyby(~primary_income, ~year_quarter + Employment_Type, design = subset(survey_design_adj, PEA == 1) , svymean)

p_avg_income_by_job_type <- plot_data %>%
  select(-starts_with(".se")) %>%
  ggplot(aes(x = year_quarter, y = primary_income, color = Employment_Type, group = Employment_Type)) + 
  geom_line() +
  labs(
    x = "Period",
    y = "Amount",
    title = "Average Income of Labor Force by Job Type",
    subtitle = "Domincan Pesos",
    color = "Employment Type") +
  scale_x_discrete(breaks = selected_breaks) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))






###### Part 3 Further correlartions ######

















plot_data2 <-svyby(~primary_income, ~year_quarter + Sex, design = subset(survey_design_adj, OCUPADO== 1) , svymean)

p_avg_income_by_sex2 <- plot_data2 %>%
  select(-starts_with(".se")) %>%
  ggplot(aes(x = year_quarter, y = primary_income, color = Sex, group = Sex)) + 
  geom_line() +
  labs(
    x = "Period",
    y = "Amount",
    title = "Average Income of Labor Force by Sex",
    subtitle = "Domincan Pesos",
    color = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))

plot_data3 <-svyby(~primary_income, ~year_quarter + Sex, design = survey_design_adj , svymean)

p_avg_income_by_sex3 <- plot_data3 %>%
  select(-starts_with(".se")) %>%
  ggplot(aes(x = year_quarter, y = primary_income, color = Sex, group = Sex)) + 
  geom_line() +
  labs(
    x = "Period",
    y = "Amount",
    title = "Average Income of Labor Force by Sex",
    subtitle = "Domincan Pesos",
    color = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))




## Plot Avg Income by Education ##
plot_data <-svyby(~primary_income, ~year_quarter + Employment, design = survey_design_adj , svymean) %>%
  select(Employment == "No Work")

p_avg_income_by_emp_type <- plot_data %>%
  select(-starts_with(".se")) %>%
  ggplot(aes(x = year_quarter, y = primary_income, color = Employment, group = Employment)) + 
  geom_line() +
  labs(
    x = "Period",
    y = "Amount",
    title = "Average Income of Labor Force by Type",
    subtitle = "Domincan Pesos",
    color = "Employment_Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 12))








income_share_table <- inc_type_share %>%
  transmute(
    year_quarter,
    share_primary   = has_primary_income,
    share_secondary = has_secondary_income,
  )


income_share_table_workers <- inc_type_share_workers %>%
  transmute(
    year_quarter,
    share_primary   = has_primary_income,
    share_secondary = has_secondary_income,
  )


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





###### Part 1 Trends in Income by Type ######

plot_data <- svyby(~INGRESO_ASALARIADO, ~year_quarter + GRUPO_EMPLEO, survey_design_adj, svymean, na.rm = TRUE)

p_primaryincome_sector <- plot_data %>%
  select(-starts_with("se")) %>%
  rename(income = INGRESO_ASALARIADO) %>%
  mutate(employment_sector = fct_reorder(GRUPO_EMPLEO, income)) %>%
  ggplot(aes(x = year_quarter, y = income, color = employment_sector, group = employment_sector)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Sectoral Employment Shares in the Formal and Informal Sector",
    subtitle = "Share of Group",
    color = "Employment Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





plot_data <- svyby(~INGRESO_ASALARIADO_SECUN, ~year_quarter + GRUPO_EMPLEO, survey_design_adj, svymean, na.rm = TRUE)

p_secondaryincome_sector <- plot_data %>%
  select(-starts_with("se")) %>%
  rename(income = INGRESO_ASALARIADO_SECUN) %>%
  mutate(employment_sector = fct_reorder(GRUPO_EMPLEO, income)) %>%
  ggplot(aes(x = year_quarter, y = income, color = employment_sector, group = employment_sector)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Sectoral Employment Shares in the Formal and Informal Sector",
    subtitle = "Share of Group",
    color = "Employment Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



plot_data <- svyby(~INGRESO_ASALARIADO_SECUN, ~year_quarter + GRUPO_EMPLEO, subset(survey_design_adj, INGRESO_ASALARIADO_SECUN >0) , svymean, na.rm = TRUE)

p_secondaryincome_nonzero_sector <- plot_data %>%
  select(-starts_with("se")) %>%
  rename(income = INGRESO_ASALARIADO_SECUN) %>%
  mutate(employment_sector = fct_reorder(GRUPO_EMPLEO, income)) %>%
  ggplot(aes(x = year_quarter, y = income, color = employment_sector, group = employment_sector)) +
  geom_line() +
  labs(
    x = "Period",
    y = "Share",
    title = "Sectoral Employment Shares in the Formal and Informal Sector",
    subtitle = "Share of Group",
    color = "Employment Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






plot_data <- survey_design_adj %>%
  group_by(year) %>%
             summarise(mean_prim_income = survey_mean(INGRESO_ASALARIADO, na.rm = TRUE))



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
