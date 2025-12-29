
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

#design
design_PEA <- subset(survey_design_adj, PEA == 1)

design_OCUPADO <- subset(survey_design_adj, OCUPADO == 1)

design_working_age <- subset(survey_design_adj, age >=15 & age<=64)


#define breaks
cat <- levels(survey_data_fixed_id$year_quarter)
selected_breaks <- cat[seq(1, length(cat), by = 2)]





## Plotting Basic Macro Variables over Time For Regions ##


#define variables of interest to track over time
vars <- c("PEA", "DESOCUPADO", "Employment_Status", "Employment_Type", "Employment_Sector_Simplified", "Wage_group", "Sex",
          "education")



#defining plot titles
titles <- c("Particpation Rate", "Unemployment Rate", "Status of Employment", 
            "Employment Category", "Employment Sector", "Size of Firms",
            "Gender Breakdown of Workers", "Educational Breakdown of Workers")

names(titles) <- vars


subtitles <- c("Percent of Working Age Population", "Percent of Labor Force", "Percent of Labor Force",
               "Percent of Labor Force", "Percent of Labor Force", "Percent of Labor Force", "Percent of Labor Force",
               "Percent of Labor Force")

names(subtitles) <- vars

#list to store plots
plot_list_labor <- list()

#list to store data
data_list <- list()


for (var in vars) {
  
  
  ## 1. Build the svytable formula dynamically:  ~ var + Region4 + year_quarter
  f <- as.formula(paste("~", var, "+ Region4 + year_quarter"))
  
  
  ##Calculate using proper survey object based on variable
  if( var == "PEA") {
    tab <- svytable(f, design = design_working_age) #need working age to calc part rate
    
  } 
  else if ( var == "DESOCUPADO") {
    tab <- svytable(f, design = design_PEA) #need labor force to calc unemployment
  } 
  else {
    tab <- svytable(f, design = design_OCUPADO) #all others are for workers
  }
 
  
  ## 2. Convert counts to shares: P(var | Region4, year_quarter)
  # dimension order is: 1 = var, 2 = Region4, 3 = year_quarter
  prop_tab <- prop.table(tab, margin = c(2, 3))
  
  ## 3. Turn into data frame
  prop_df <- as.data.frame(prop_tab)
  
  # first column is the variable (its name is whatever 'var' is)
  names(prop_df)[1] <- var
  names(prop_df)[names(prop_df) == "Freq"] <- "share"
  
  # For unemployment and participation filter
  if ((var == "PEA") | (var == "DESOCUPADO")) {
    prop_df <- prop_df %>%
      filter(.data[[var]] == 1)
  }
  
  # store data
  data_name <- paste0("share_", var, "_by_region")
  data_list[[data_name]] <- prop_df

  ## 4. Make regional panel plot (one panel per Region4)
  plot_name <- paste0(var, "_by_Region")
  
  plot <- ggplot(
    prop_df,
    aes(
      x     = year_quarter,
      y     = share,
      color = .data[[var]],
      group = .data[[var]]
    )
  ) +
    geom_line() +
    facet_wrap(~ Region4, ncol = 2) +
    labs(
      x        = "Time",
      y        = "Share of workers (weighted)",
      color    = var,
      title    = titles[[var]],
      subtitle = subtitles[[var]]
    ) +
    scale_x_discrete(breaks = selected_breaks) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  plot_list_labor[[plot_name]] <- plot
}


###### Save Individual Graphics ######

for (plot in names(plot_list_labor)) {
  
  filename <- paste()
  ggsave(filename = paste("./Outputs/Charts and Tables/Individual Charts/Labor Market and Demographics/Region Panels", plot, ".png", sep = ""),
         plot = plot_list_labor[[plot]],
         width = 8,
         height = 6,
         dpi = 300)
}



####Plotting Averages of Continuous Variables ####


#define variables of interest to track over time
cont_vars <- c("real_total_income_primary", "real_salary_income_primary", "real_benefits_income_primary", "real_independent_income_primary",
          "real_total_income_secondary", "real_salary_income_secondary", "real_benefits_income_secondary", "real_independent_income_secondary",
          "real_total_income_total", "real_salary_income_total", "real_benefits_income_total", "real_independent_income_total")

groups <- c("Region4")

#defining plot titles
titles <- c("Total Income From Primary Job", "Salary Income From Primary Job", "Benefits From Primary Job", "Independent Income from Primary Job",
            "Total Income From Secondary Job", "Salary Income From Second Job", "Benefits From Secondary Job", "Independent Income from Secondary Job",
            "Total Income From All Jobs", "Salary Income From All Jobs", "Benefits From All Jobs", "Independent Income from All Jobs")

names(titles) <- cont_vars


subtitles <- c("Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos",
               "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos",
               "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos", "Monthly Value in 2019 DOM Pesos")


names(subtitles) <- cont_vars

#list to store plots
plot_list_inc <- list()

for (grp in groups) {
  
  data_name <- paste("inc_by_", grp)
  
  data_list[[data_name]] <- svyby_multi_numeric_fast_bygroup (
    design   = design_OCUPADO,
    vars     = cont_vars,
    by_time  = "year_quarter",
    by_group = grp,
    fun   = svymean,
    vartype = "se"
  )


  for (var in cont_vars) {
    
    plot_data <- data_list[[data_name]] %>%
      filter(variable == var)
    
    plot_name <- paste(var, "_by_", grp, sep = "")
    
    plot <- ggplot(plot_data,
                aes(x = year_quarter, y = estimate, group = 1)) +
      geom_line() +
      facet_wrap(vars(.data[[grp]]), ncol = 2) +
      labs(
        x = "Period",
        y = "Pesos",
        color    = var,
        title    = titles[[var]],
        subtitle = subtitles[[var]]
      ) +
      scale_x_discrete(breaks = selected_breaks) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot_list_inc[[plot_name]] <- plot
  }
}



###### Save Individual Graphics ######

for (plot in names(plot_list_inc)) {
  
  filename <- paste()
  ggsave(filename = paste("./Outputs/Charts and Tables/Individual Charts/Income/Real/Region Panels", plot, ".png", sep = ""),
         plot = plot_list_inc[[plot]],
         width = 8,
         height = 6,
         dpi = 300)
}



keep1 <- grep("Region", names(plot_list_labor), value = TRUE)
keep2 <- grep("Region", names(plot_list_inc), value = TRUE)

combnined_plot_list <- c(
  plot_list_labor[keep1],
  plot_list_inc[keep2]
)

#Printing to PDF
print_order <- c("PEA_by_Region", "DESOCUPADO_by_Region", "Employment_Status_by_Region", "Employment_Type_by_Region", "Employment_Sector_Simplified_by_Region", "Wage_group_by_Region",
                 "Sex_by_Region", "education_by_Region", "real_total_income_total_by_Region4", "real_total_income_primary_by_Region4", "real_total_income_secondary_by_Region4" )


print_section_header <- list("Trends in Labor Market Outcomes Across Major Regions" = 1:8,
                             "Trends in Total Income Across Major Regions" = 9:11)


#note function only prints plots specified in order vector if given
save_plots_pdf_grid(combnined_plot_list, file = "./Outputs/Charts and Tables/Trends_For_Major_Regions.pdf", ncol = 1, nrow = 1,
                    order = print_order,
                    sections = print_section_header,
                    break_sections = TRUE)




