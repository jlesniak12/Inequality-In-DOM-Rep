
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



svytable(~Wage_group, subset(design_PEA, (year_quarter == "2019Q4") & (Employment_Status  == "Empleo Formal")), Ntotal = 100)




###### Bar Plots for Macro Shares ######


#define variables of interest to track over time
vars <- c("PEA", "DESOCUPADO", "Employment_Status", "Employment_Type", "Employment_Sector_Simplified", "Wage_group", "Sex",
          "education")


## Plotting Variables over Time ##

#defining plot titles
titles <- c("Particpation Rate", "Unemployment Rate", "Status of Employment", 
            "Employment Category", "Employment Sector", "Firm Size", 
            "Gender Breakdowns", "Education")

names(titles) <- vars


subtitles <- c("Percent of Working Age Population", "Percent of Labor Force", "Percent of Workers",
               "Percent of Workers", "Percent of Workers", "Percent of Workers",
               "Percent of Workers", "Percent of Workers")

names(subtitles) <- vars


#list to store plots
plot_list_basic_shares <- list()

#list to store data
data_list <- list()


#### Plots for Share Bar Graphs ####

start_avg_overtime <- Sys.time()

inc_time_ocupado <- svyby_multi_numeric_fast(
  design   = design_OCUPADO,
  vars     = vars,
  by_time  = "year_quarter",
  fun   = svymean,
  vartype = NULL
)

inc_time_age <- svyby_multi_numeric_fast(
  design   = design_working_age,
  vars     = vars,
  by_time  = "year_quarter",
  fun   = svymean,
  vartype = NULL
)

inc_time_active <- svyby_multi_numeric_fast(
  design   = design_PEA,
  vars     = vars,
  by_time  = "year_quarter",
  fun   = svymean,
  vartype = NULL
)


for (var in vars) {
  
  
  #select proper design subset and variable to plot
  if (var == "PEA") {
    
    plot_data <- inc_time_age %>%
      filter(grepl(var, variable)) %>%
      filter(!(grepl("^se", variable)))
    
  } else if (var == "DESOCUPADO") {
    
    plot_data <- inc_time_active %>%
      filter(grepl(var, variable)) %>%
      filter(!(grepl("^se", variable)))
    
  } else {
    
    plot_data <- inc_time_ocupado %>%
      filter(grepl(var, variable)) %>%
      filter(!(grepl("^se", variable))) %>%
      mutate(variable = str_remove(variable, var))
  }
  
  plot_name <- paste(var)
  
  plot <- plot_data %>%
    ggplot( aes(x = year_quarter, y = estimate, fill = variable )) +
    geom_bar(position = "stack", stat = "identity") +
    labs(
      x = "Period",
      y = "Share",
      title = titles[[var]],
      subtitle = subtitles[[var]],
      fill = var
    ) +
    scale_x_discrete(breaks = selected_breaks) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          strip.text = element_text(size = 12))
  
  plot_list_basic_shares[[plot_name]] <- plot
  
}



###### Save Individual Graphics ######

for (plot in names(plot_list_basic_shares)) {
  
  filename <- paste()
  ggsave(filename = paste("./Outputs/Charts and Tables/Individual Charts/Labor Market and Demographics/", plot, ".png", sep = ""),
         plot = plot_list_basic_shares[[plot]],
         width = 8,
         height = 6,
         dpi = 300)
}


###### Plots by Group ######


#list to store plots
plot_list_groups <- list()

#list to store data
data_list <- list()


vars <- c("PEA", "DESOCUPADO", "education", "Sex", "Employment_Status", "Employment_Type", "Employment_Sector_Simplified", "Wage_group" )

## Put groups in the order you want to PRIORITIZE as group variables (ie facet in graphs)
groups <- c("PEA", "DESOCUPADO", "education", "Sex", "Employment_Status", "Employment_Type", "Employment_Sector_Simplified", "Wage_group")


var_labels <- c("Labor Force Participation Rate", "Unemployment Rate", "Education Level","Sex", "Employment Status", "Employment Type", "Employment Sector", "Size of Firm")
names(var_labels) <- vars

label_var <- function(x) {
  # x can be a vector
  out <- x
  idx <- x %in% names(var_labels)
  out[idx] <- var_labels[x[idx]]
  out
}

data_working_age <- list()
data_active      <- list()
data_default     <- list()

start_time <- Sys.time()

for (grp in groups) {
  
  data_working_age[[grp]] <- svyby_multi_numeric_fast_bygroup(
    design   = design_working_age,
    vars     = vars,
    by_time  = "year_quarter",
    by_group = grp,
    fun      = svymean,
    vartype  = "se"
  )
  
  data_active[[grp]] <- svyby_multi_numeric_fast_bygroup(
    design   = design_PEA,
    vars     = vars,
    by_time  = "year_quarter",
    by_group = grp,
    fun      = svymean,
    vartype  = "se"
  )
  
  data_default[[grp]] <- svyby_multi_numeric_fast_bygroup(
    design   = design_OCUPADO,
    vars     = vars,
    by_time  = "year_quarter",
    by_group = grp,
    fun      = svymean,
    vartype  = "se"
  )
}
end_time <- Sys.time()
time <- end_time - start_time
print(time)


var_working_age <- c("PEA")
var_economically_active <- c("DESOCUPADO")


## helper: subtitle depending on denominator
subtitle_for_var <- function(var) {
  if (var == "PEA") {
    "Percent of Working Age Population"
  } else if (var == "DESOCUPADO") {
    "Percent of Labor Force"
  } else {
    "Percent of Workers"
  }
}

for (grp in groups) {
  for (var in vars) {
    
    # no need to plot var against itself
    if (var == grp) next
    
    #set proper data depending on var
    if (var %in% var_working_age) {
      plot_data <- data_working_age[[grp]]
    }
    else if (var %in% var_economically_active) {
      plot_data <- data_active[[grp]]
    } 
    else {
      plot_data <- data_default[[grp]]
    }
    
    ## --- keep only rows corresponding to the current `var` --- ##
    ## this assumes that the `variable` column coming from svyby contains
    ## names like "education", "education_somecollege", "Sex_male", etc.
    ## Using startsWith() works both for pure matches and prefixed level names
    plot_data <- plot_data[startsWith(plot_data$variable, var), , drop = FALSE]
    
    if (nrow(plot_data) == 0) next
    
    base_title <- paste("Breakdown of", label_var(var), "Within", label_var(grp))
    base_subtitle <- subtitle_for_var(var)
    
    ## --- 1) STACKED BAR PANELS --- ##
    stacked_name <- paste(base_title, "_stacked", sep = "")
    
    plot_stacked <- ggplot(
      plot_data,
      aes(x = year_quarter, y = estimate, fill = variable)
    ) +
      geom_bar(position = "fill", stat = "identity") +
      facet_wrap(vars(.data[[grp]]), labeller = labeller(.cols = label_var)) +
      labs(
        x        = "Period",
        y        = "Share",
        title    = base_title,
        subtitle = base_subtitle,
        fill     = ""
      ) +
      scale_x_discrete(breaks = selected_breaks) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    plot_list_groups[[stacked_name]] <- plot_stacked
    
    ## --- 2) LINE PANELS (same data) --- ##
    line_name <- paste0(base_title, "_line", sep ="")
    
    plot_line <- ggplot(
      plot_data,
      aes(x = year_quarter, y = estimate,
          color = variable, group = variable)
    ) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(.data[[grp]]), labeller = labeller(.cols = label_var)) +
      labs(
        x        = "Period",
        y        = "Share",
        title    = base_title,
        subtitle = base_subtitle,
        color    = ""
      ) +
      scale_x_discrete(breaks = selected_breaks) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    plot_list_groups[[line_name]] <- plot_line
    
  }
}






###### Save Individual Graphics ######

for (plot in names(plot_list_groups)) {
  
  ggsave(filename = paste("./Outputs/Charts and Tables/Individual Charts/Labor Market and Demographics/", plot, ".png", sep = ""),
         plot = plot_list_groups[[plot]],
         width = 8,
         height = 6,
         dpi = 300)
}







######Printing to PDF######

combnined_plot_list <- c(
  plot_list_groups,
  plot_list_basic_shares
)

print_order <- c("PEA", "DESOCUPADO", "Employment_Status", "Employment_Type", "Employment_Sector_Simplified", "Wage_group", "Sex", "education",
                 
                 
                 "Breakdown of Employment Type Within Employment Status_stacked", "Breakdown of Employment Sector Within Employment Status_stacked", "Breakdown of Size of Firm Within Employment Status_stacked",
                 "Breakdown of Sex Within Employment Status_line", "Breakdown of Education Level Within Employment Status_stacked",
                 
                  
                 "Breakdown of Employment Status Within Size of Firm_stacked", "Breakdown of Employment Type Within Size of Firm_stacked", "Breakdown of Employment Sector Within Size of Firm_stacked", 
                 "Breakdown of Sex Within Size of Firm_line", "Breakdown of Education Level Within Size of Firm_stacked",
                 
                 "Breakdown of Labor Force Participation Rate Within Sex_line", "Breakdown of Unemployment Rate Within Sex_line", 
                 "Breakdown of Employment Status Within Sex_line", "Breakdown of Employment Type Within Sex_stacked", "Breakdown of Employment Sector Within Sex_stacked", "Breakdown of Size of Firm Within Sex_stacked",
                 "Breakdown of Education Level Within Sex_stacked",
                 
                 "Breakdown of Labor Force Participation Rate Within Education Level_line", "Breakdown of Unemployment Rate Within Education Level_line", 
                 "Breakdown of Employment Status Within Education Level_stacked", "Breakdown of Employment Type Within Education Level_stacked", "Breakdown of Employment Sector Within Education Level_stacked", "Breakdown of Size of Firm Within Education Level_stacked",
                 "Breakdown of Sex Within Education Level_line"
)
                 

print_section_header <- list("Trends in Lavor Market: Overall" = 1:8,
                            "Trends in Labor Market: Formal vs Informal Workers" = 9:13,
                            "Trends in Labor Market: Firm Size" = 14:18,
                            "Trends in Labor Market: Male and Female Workers" = 19:25,
                            "Trends in Labor Market: By Education Levels" = 26:32)
                 
                   
#note function only prints plots specified in order vector if given
save_plots_pdf_grid(combnined_plot_list, file = "./Outputs/Charts and Tables/Labor Market Trends Over Time.pdf", ncol = 1, nrow = 2,
                    order = print_order,
                    sections = print_section_header,
                    break_sections = TRUE)
                   
                   
                  
      









