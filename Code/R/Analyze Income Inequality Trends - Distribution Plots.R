
library(tidyverse)
library(gtsummary)
library(survey)
library(purrr)
library(ggplot2)
library(convey)

library(patchwork)
library(tibble)
library(SurveyTools)

#devtools::install_github("jlesniak12/SurveyTools")





load(file = "./Processed Data/Individual_quarter_ENCFT_clean.rda" )




###### set Design for Quarterly Data ######


check <- check_and_fix_survey_ids(ENCFT_quarterly_individual, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")
individual_level_fixedid <- check$data


#define and prep survey object at the individual level
design_indiv_q <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = individual_level_fixedid,
                            nest = TRUE)
design_indiv_q <- convey_prep(design_indiv_q)






########## Distributional Charts ##########

#logs for distribution of those with income
design_indiv_q <- update(
  design_indiv_q,
  log_salary_inc = log(real_salary_income_total),
  log_indep_inc  = log(real_independent_income_total),
  log_total_inc  = log(real_total_income_total)
)


#concepts/outcomes to analyze
outcomes <- c(
  salary = "log_salary_inc",
  indep  = "log_indep_inc",
  total  = "log_total_inc"
)

#human readable labels for concepts. Used in default graphics
outcome_labels <- c(
  salary = "Log Real salary income (total)",
  indep  = "Log Real independent income",
  total  = "Log Real total labor income"
)


#base filters (outcome-specific) which help define the concept
base_filters <- list(
  salary      = quote(real_salary_income_total > 0),
  indep       = quote(real_independent_income_total > 0),
  total       = quote(real_total_income_total > 0)
)

# build a named list of expressions: "2020" = quote(year == 2020), etc.
years <- as.integer(list(2015,2024))

year_filters <- setNames(
  lapply(years, function(y) bquote(year == .(y))),
  as.character(years)
)


#define dimensions for filtering data
dims <- list(
  population = list(
    employed  = quote(OCUPADO == 1)
  ),
  
  work_status = list(
    all      = quote(TRUE),
    formal   = quote(Employment_Status == "Empleo Formal"),
    informal = quote(Employment_Status == "Empleo Informal")
  ),
  
  wage_group = list(
    all       = quote(TRUE),
    micro     = quote(Wage_group == "micro_firm"),
    small     = quote(Wage_group == "small_firm"),
    medium    = quote(Wage_group == "medium_firm"),
    large     = quote(Wage_group == "large_firm"),
    Dont_know = quote(Wage_group == "Dont Know")
  ),
  
  year = year_filters
)

#labels for title
dim_label_suffix <- list(
  population = c(
    employed  = " – Employed"
  ),
  
  work_status = c(
    all      = "",
    formal   = " – Formal workers",
    informal = " – Informal workers"
  ),
  
  wage_group = list(
    all       = "",
    micro     = "- Micro Size Firms",
    small     = "- Small Size Firms",
    medium    = "- Medium Size Firms",
    large     = "- Large Size Firms",
    Dont_know = "- Dont Know Firm Size"
  ),
  
  year = setNames(
    paste0(" – Year ", years),
    as.character(years)
  )
)

#create spec table with years individually
dist_specs <- make_stat_specs(
  outcomes         = outcomes,
  outcome_labels   = outcome_labels,
  base_filters     = base_filters,
  dims             = dims,
  dim_label_suffix = dim_label_suffix
)


#Filter to specs with enough data
dist_specs_ok <- filter_specs_with_data(
  specs   = dist_specs,
  design  = design_indiv_q,
  min_n   = 10,  # tweak as you like
  min_unique = 10
)



## ---- OVERLAY SPECS: employed + (2015 vs 2024) ----

dims_overlay <- list(
  population = list(
    employed = quote(OCUPADO == 1)
  ),
  
  work_status = list(
    all      = quote(TRUE),
    formal   = quote(Employment_Status == "Empleo Formal"),
    informal = quote(Employment_Status == "Empleo Informal")
  ),
  
  wage_group = list(
    all      = quote(TRUE),
    micro    = quote(Wage_group == "micro_firm"),
    small    = quote(Wage_group == "small_firm"),
    medium   = quote(Wage_group == "medium_firm"),
    large    = quote(Wage_group == "large_firm"),
    Dont_know= quote(Wage_group == "Dont Know")
  ),
  
  yearpair = list(
    `2015_2024` = quote(year %in% c(2015, 2024))
  )
)

dim_label_suffix_overlay <- list(
  population = c(
    employed = " – Employed"
  ),
  
  work_status = c(
    all      = "",
    formal   = " – Formal workers",
    informal = " – Informal workers"
  ),
  
  wage_group = list(
    all      = "",
    micro    = "- Micro Size Firms",
    small    = "- Small Size Firms",
    medium   = "- Medium Size Firms",
    large    = "- Large Size Firms",
    Dont_know= "- Dont Know Firm Size"
  ),
  
  yearpair = c(
    `2015_2024` = " – 2015 vs 2024"
  )
)


#create spec table with years individually
dist_specs_overlay <- make_stat_specs(
  outcomes         = outcomes,
  outcome_labels   = outcome_labels,
  base_filters     = base_filters,
  dims             = dims_overlay,
  dim_label_suffix = dim_label_suffix_overlay
)


#Filter to specs with enough data
dist_specs_overlay_ok <- filter_specs_with_data(
  specs   = dist_specs_overlay,
  design  = design_indiv_q,
  min_n   = 10,  
  min_unique = 10
)


##### Kdensity of Log #####

#individual years plots
density_outputs <- dist_specs_ok %>%
  mutate(
    plot = purrr::pmap(
      list(outcome_var, subset_expr, nice_name),
      ~ plot_svy_distribution (
        design                  = design_indiv_q,
        var                     = ..1,
        type                    = "density",            
        density_bw              = 0.2,                         
        density_gridsize        = 2001L,
        subset_expr             = ..2,
        by_group                = "year",
        overlay_groups          = TRUE,
        title                   = paste(..3, "– Income distribution"),
        xlab                    = "Log Monthly real salary income (DOP)"
      )
    )
  )

# Check a plot
density_outputs$plot[[60]]

#overlay plots
density_outputs_overlay <- dist_specs_overlay_ok %>%
  mutate(
    plot = purrr::pmap(
      list(outcome_var, subset_expr, nice_name),
      ~ plot_svy_distribution (
        design                  = design_indiv_q,
        var                     = ..1,
        type                    = "density",            
        density_bw              = 0.4,                         
        density_gridsize        = 2001L,
        subset_expr             = ..2,
        by_group                = "year",
        overlay_groups          = TRUE,
        title                   = paste(..3),
        subtitle                = paste("Income Distribution"),
        xlab                    = "Log Monthly real salary income (DOP)"
      )
    )
  )


# Check a plot
density_outputs_overlay$plot[[33]]

# Build a named plot list
density_plot_list <- density_outputs_overlay$plot
names(density_plot_list) <- density_outputs_overlay$spec_id

# Save individual PNGs
purrr::iwalk(
  density_plot_list,
  ~ ggsave(
    filename = paste0(
      "./Outputs/Charts and Tables/Individual Charts/Income Inequality/Density/Dist_",
      .y, ".png"
    ),
    plot   = .x,
    width  = 4,
    height = 4
  )
)


# Sections: each is a character vector of *plot_list names* 
dist_sections <- list(
  "Total Labor Income - All Workers" = density_outputs_overlay$spec_id[
    density_outputs_overlay$outcome_id  == "total" &
      density_outputs_overlay$work_status == "all"
  ],
  "Total Labor Income - Formal Workers" = density_outputs_overlay$spec_id[
    density_outputs_overlay$outcome_id  == "total" &
      density_outputs_overlay$work_status == "formal"
  ],
  "Total Labor Income - Informal Workers" = density_outputs_overlay$spec_id[
    density_outputs_overlay$outcome_id  == "total" &
      density_outputs_overlay$work_status == "informal"
  ],
  "Salary Income - All Workers" = density_outputs_overlay$spec_id[
    density_outputs_overlay$outcome_id  == "salary" &
      density_outputs_overlay$work_status == "all"
  ],
  "Salary Income - Formal Workers" = density_outputs_overlay$spec_id[
    density_outputs_overlay$outcome_id  == "salary" &
      density_outputs_overlay$work_status == "formal"
  ],
  "Salary Income - Informal Workers" = density_outputs_overlay$spec_id[
    density_outputs_overlay$outcome_id  == "salary" &
      density_outputs_overlay$work_status == "informal"
  ],
  "Independent Income - All Workers" = density_outputs_overlay$spec_id[
    density_outputs_overlay$outcome_id  == "indep" &
      density_outputs_overlay$work_status == "all"
  ],
  "Independent Income - Formal Workers" = density_outputs_overlay$spec_id[
    density_outputs_overlay$outcome_id  == "indep" &
      density_outputs_overlay$work_status == "formal"
  ],
  "Independent Income - Informal Workers" = density_outputs_overlay$spec_id[
    density_outputs_overlay$outcome_id  == "indep" &
      density_outputs_overlay$work_status == "informal"
  ]
)



save_plots_pdf_grid(
  plot_list      = density_plot_list,
  file           = "./Outputs/Charts and Tables/Income Density Work status and Wage Groups.pdf",
  ncol           = 1,
  nrow           = 2,
  sections       = dist_sections,
  break_sections = TRUE,
  add_page_numbers = TRUE
)






###### Plotting Histograms of Income######

#logs for distribution of those with income
design_indiv_q <- update(
  design_indiv_q,
  log_salary_inc = log(real_salary_income_total),
  log_indep_inc  = log(real_independent_income_total),
  log_total_inc  = log(real_total_income_total)
)


#concepts/outcomes to analyze
outcomes <- c(
  salary = "real_salary_income_total",
  indep  = "real_independent_income_total",
  total  = "real_total_income_total"
)

#human readable labels for concepts. Used in default graphics
outcome_labels <- c(
  salary = "Log Real salary income (total)",
  indep  = "Log Real independent income",
  total  = "Log Real total labor income"
)


#base filters (outcome-specific) which help define the concept
base_filters <- list(
  salary      = quote(real_salary_income_total > 0),
  indep       = quote(real_independent_income_total > 0),
  total       = quote(real_total_income_total > 0)
)

# build a named list of expressions: "2020" = quote(year == 2020), etc.
years <- as.integer(list(2015,2024))

year_filters <- setNames(
  lapply(years, function(y) bquote(year == .(y))),
  as.character(years)
)


#define dimensions for filtering data
dims <- list(
  population = list(
    employed  = quote(OCUPADO == 1)
  ),
  
  work_status = list(
    all      = quote(TRUE),
    formal   = quote(Employment_Status == "Empleo Formal"),
    informal = quote(Employment_Status == "Empleo Informal")
  ),
  
  wage_group = list(
    all       = quote(TRUE),
    micro     = quote(Wage_group == "micro_firm"),
    small     = quote(Wage_group == "small_firm"),
    medium    = quote(Wage_group == "medium_firm"),
    large     = quote(Wage_group == "large_firm"),
    Dont_know = quote(Wage_group == "Dont Know")
  ),
  
  year = year_filters
)

#labels for title
dim_label_suffix <- list(
  population = c(
    employed  = " – Employed"
  ),
  
  work_status = c(
    all      = "",
    formal   = " – Formal workers",
    informal = " – Informal workers"
  ),
  
  wage_group = list(
    all       = "",
    micro     = "- Micro Size Firms",
    small     = "- Small Size Firms",
    medium    = "- Medium Size Firms",
    large     = "- Large Size Firms",
    Dont_know = "- Dont Know Firm Size"
  ),
  
  year = setNames(
    paste0(" – Year ", years),
    as.character(years)
  )
)

#create spec table with years individually
hist_specs <- make_stat_specs(
  outcomes         = outcomes,
  outcome_labels   = outcome_labels,
  base_filters     = base_filters,
  dims             = dims,
  dim_label_suffix = dim_label_suffix
)


#Filter to specs with enough data
hist_specs_ok <- filter_specs_with_data(
  specs   = hist_specs,
  design  = design_indiv_q,
  min_n   = 10,  # tweak as you like
  min_unique = 10
)




##Truncate 99th percentile of income
p99 <- as.numeric(
  survey::svyquantile(~real_total_income_total,
                      design_indiv_q,
                      quantiles = 0.99,
                      na.rm = TRUE)
)

# round up to a nice number
max_cut <- ceiling(p99 / 1000) * 1000

# choose bin width (e.g. 1,000 DOP)
binwidth <- 1000

income_breaks <- seq(0, max_cut, by = binwidth)




#individual years plots
hist_outputs <- hist_specs_ok %>%
  mutate(
    plot = purrr::pmap(
      list(outcome_var, subset_expr, nice_name),
      ~ plot_svy_distribution (
        design                  = design_indiv_q,
        var                     = ..1,
        type                    = "hist",
        subset_expr             = ..2,
        by_group                = NULL,             
        title                   = paste(..3),
        subtitle                = paste("Income Distribution"),
        xlab                   = "Log Monthly real salary income (DOP)"
      )
    )
  )



# Check a plot
hist_outputs$plot[[60]]

# Build a named plot list
hist_plot_list <- hist_outputs$plot
names(hist_plot_list) <- hist_outputs$spec_id

# Save individual PNGs
purrr::iwalk(
  hist_plot_list,
  ~ ggsave(
    filename = paste0(
      "./Outputs/Charts and Tables/Individual Charts/Income Inequality/Histograms/hist",
      .y, ".png"
    ),
    plot   = .x,
    width  = 8,
    height = 5
  )
)


# Sections: each is a character vector of *plot_list names* 
hist_sections <- list(
  "Total Labor Income - All Workers" = hist_outputs$spec_id[
    hist_outputs$outcome_id  == "total" &
      hist_outputs$work_status == "all"
  ],
  "Total Labor Income - Formal Workers" = hist_outputs$spec_id[
    hist_outputs$outcome_id  == "total" &
      hist_outputs$work_status == "formal"
  ],
  "Total Labor Income - Informal Workers" = hist_outputs$spec_id[
    hist_outputs$outcome_id  == "total" &
      hist_outputs$work_status == "informal"
  ],
  "Salary Income - All Workers" = hist_outputs$spec_id[
    hist_outputs$outcome_id  == "salary" &
      hist_outputs$work_status == "all"
  ],
  "Salary Income - Formal Workers" = hist_outputs$spec_id[
    hist_outputs$outcome_id  == "salary" &
      hist_outputs$work_status == "formal"
  ],
  "Salary Income - Informal Workers" = hist_outputs$spec_id[
    hist_outputs$outcome_id  == "salary" &
      hist_outputs$work_status == "informal"
  ],
  "Independent Income - All Workers" = hist_outputs$spec_id[
    hist_outputs$outcome_id  == "indep" &
      hist_outputs$work_status == "all"
  ],
  "Independent Income - Formal Workers" = hist_outputs$spec_id[
    hist_outputs$outcome_id  == "indep" &
      hist_outputs$work_status == "formal"
  ],
  "Independent Income - Informal Workers" = hist_outputs$spec_id[
    hist_outputs$outcome_id  == "indep" &
      hist_outputs$work_status == "informal"
  ]
)


save_plots_pdf_grid(
  plot_list      = hist_plot_list,
  file           = "./Outputs/Charts and Tables/Histogram Log Income Work Status and Wage Group .pdf",
  ncol           = 1,
  nrow           = 2,
  sections       = hist_sections,
  break_sections = TRUE
)








