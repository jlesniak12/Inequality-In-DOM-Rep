
library(tidyverse)
library(gtsummary)
library(survey)
library(purrr)
library(ggplot2)
library(convey)

library(patchwork)
library(tibble)
library(SurveyTools)


source("D:/Research Projects/Frank Chapter 3/Code/test.R")


load(file = "./Processed Data/Individual_quarter_ENCFT_clean.rda" )

save_path <- "./Outputs/Charts and Tables/Income Inequality Trends/Gini/"

###### set Design for Quarterly Data ######


check <- check_and_fix_survey_ids(ENCFT_quarterly_individual, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")
individual_level_fixedid <- check$data


#define and prep survey object at the individual level
design_indiv_q <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = individual_level_fixedid,
                            nest = TRUE)

design_indiv_q <- convey_prep(design_indiv_q)






###### Calculate and Plot Gini Coefficients Over Time #####


#### 1. Define a Spec table for subsetting survey design ####

#--each row of the table represents a population sub group for analysis


#define concepts/outcomes variables to analyze
outcomes <- c(
  salary = "real_salary_income_total",
  indep  = "real_independent_income_total",
  total  = "real_total_income_total"
)

#human readable labels for concepts. Used in default graphics
outcome_labels <- c(
  salary = "Real salary Income - All Jobs",
  indep  = "Real independent Income - All Jobs",
  total  = "Real Total Labor Income - All Jobs"
)


#base filters (outcome-specific) which help define the concept properly
base_filters <- list(
  salary      = quote(real_salary_income_total > 0),
  indep       = quote(real_independent_income_total > 0),
  total       = quote(real_total_income_total > 0)
)


#define dimensions for Analysis and Sub Grouping
dims <- list(
  
  #conditional for base population groups
  population = list(
    employed = quote(OCUPADO == 1)
  ),
  
  #conditional for company sizes 
  wage_group = list(
    all       = quote(TRUE),
    micro     = quote(Wage_group == "micro_firm"),
    small     = quote(Wage_group == "small_firm"),
    medium    = quote(Wage_group == "medium_firm"),
    large     = quote(Wage_group == "large_firm"),
    Dont_know = quote(Wage_group == "Dont Know")
  ),
  
  #conditional for worker status
  work_status = list(
    all      = quote(TRUE),
    formal   = quote(Employment_Status == "Empleo Formal"),
    informal = quote(Employment_Status == "Empleo Informal")
  )
)

# Label suffixes for nicer names in plots/tables
dim_label_suffix <- list(
  
  population = c(
    employed = " – Employed Population"
  ),
  
  wage_group = list(
    all       = "",
    micro     = "- Micro Size Firms",
    small     = "- Small Size Firms",
    medium    = "- Medium Size Firms",
    large     = "- Large Size Firms",
    Dont_know = "- Dont Know Firm Size"
  ),
  
  work_status = c(
    all      = "",
    formal   = " – Formal workers",
    informal = " – Informal workers"
  )
)


#generate a data frame of samples using above inputs
#creates all combinations of the dimension filters
gini_specs <- make_stat_specs(
  outcomes         = outcomes,
  outcome_labels   = outcome_labels,
  base_filters     = base_filters,
  dims             = dims,
  dim_label_suffix = dim_label_suffix
)


#Filter out specs that have few observations
gini_specs_ok <- filter_specs_with_data(
  specs   = gini_specs,
  design  = design_indiv_q,
  min_n   = 10, 
  min_unique = 10
)




#### Generate Gini for Each Spec and Plot ####


#generate time series of gini coeff for different subgroups
gini_outputs <- gini_specs_ok %>%
  dplyr::mutate(
    gini_res = purrr::pmap(
      list(outcome_var, subset_expr),
      ~ compute_stat_time(
        design      = design_indiv_q,
        vars        = ..1,
        by_time     = "year_quarter",
        stat        = "gini",
        use_se      = TRUE,
        subset_expr = ..2
      )
    ),
    stat_time = purrr::map(gini_res, "stat_time")
  )