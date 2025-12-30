


#devtools::install_github("jlesniak12/SurveyTools")


load(file = "./Processed Data/Individual_quarter_ENCFT_clean.rda" )

save_path <- "./Outputs/Charts and Tables/Income Inequality Trends/Quantile/"


###### set Design for Quarterly Data ######


check <- check_and_fix_survey_ids(ENCFT_quarterly_individual, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")
individual_level_fixedid <- check$data


#define and prep survey object at the individual level
design_indiv_q <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = individual_level_fixedid,
                            nest = TRUE)
design_indiv_q <- convey_prep(design_indiv_q)







###### Calculate and Plot Quantiles Fan Chart #####



#### Define a Spec table for subsetting survey design ####

#--each row of the table represents a population sub group for analysis


#concepts/outcomes to analyze
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
  
  population = list(
    employed = quote(OCUPADO == 1)
  ),
  
  wage_group = list(
    all       = quote(TRUE),
    micro     = quote(Wage_group == "micro_firm"),
    small     = quote(Wage_group == "small_firm"),
    medium    = quote(Wage_group == "medium_firm"),
    large     = quote(Wage_group == "large_firm"),
    Dont_know = quote(Wage_group == "Dont Know")
  ),
  
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
fan_specs <- make_stat_specs(
  outcomes         = outcomes,
  outcome_labels   = outcome_labels,
  base_filters     = base_filters,
  dims             = dims,
  dim_label_suffix = dim_label_suffix
)


#Filter out specs that have few observations
fan_specs_ok <- filter_specs_with_data(
  specs   = fan_specs,
  design  = design_indiv_q,
  min_n   = 10, 
  min_unique = 10
)



#### Calculate Quantiles####

#define all p used in charts
quant_probs <- c(.05, .10, .25, .50, .60, .70, .75, .80, .90, .95)



#define spec table for quarterly (limit small groupings)
spec_qtr <- fan_specs_ok[fan_specs_ok$wage_group == "all",]


#generate time series of quantiles for different groups
quant_outputs_long <- spec_qtr %>%
  dplyr::mutate(
    quant_res = purrr::pmap(
      list(outcome_var, subset_expr),
      ~ svy_quantiles_over_time_core(
        design      = design_indiv_q,
        vars        = ..1,
        by_time     = "year_quarter",
        se          = FALSE,
        probs       = quant_probs,
        min_cell_neff = 50,
        weight_var  = "FACTOR_EXPANSION",
        subset_expr = ..2
      )
    )
  )


quant <- quant_outputs_long %>%
  mutate(stat_time = purrr::map(
    quant_res,
      ~ prepare_stat_time(
        stat_long = .x,
        time_var  = "year_quarter",
        use_se    = FALSE   # no SEs from svyquantile
      )
  )
  )

check1 <- quant$stat_time[[1]]
check2 <-quant_outputs_long$quant_res[[1]]


###### Plot Quantile Fan Charts ######


#create time series plots of p10 p25 p50 p90 for different groups
quant_outputs <- quant_outputs %>%
  dplyr::mutate(
    plot = purrr::map2(
      stat_time,
      nice_name,
      ~ {
        # all quantile series names, e.g. "real_salary_income_total_p10", etc.
        q_vars <- unique(.x$qtr$variable)
        
        # derive labels from the variable names themselves
        # e.g. "real_salary_income_total_p10" -> "P10"
        q_labels <- sub(".*_p", "P", q_vars)
        names(q_labels) <- q_vars
        
        plot_stat_multi(
          stat_time     = .x,
          vars          = q_vars,
          series_labels = q_labels,
          view          = "quarterly",
          title         = paste(..2, "Income Quantiles"),
          y_label       = "Income (DOP)"
        )
      }
    )
  )





#generate time series of quantiles for different groups
quant_outputs <- fan_specs_ok %>%
  dplyr::mutate(
    quant_res = purrr::pmap(
      list(outcome_var, subset_expr),
      ~ svy_quantiles_over_time_core(
        design      = design_indiv_q,
        vars        = ..1,
        by_time     = "year_quarter",
        probs   = quant_probs,
        subset_expr = ..2
      )
    ),
    stat_time = purrr::map(
      quant_res,
      ~ prepare_stat_time(
        stat_long = .x,
        time_var  = "year_quarter",
        use_se    = FALSE   # no SEs from svyquantile
      )
    )
  )

#check output
quant_outputs$stat_time[[1]]


plot <- quant_outputs$stat_time[[1]]

plotting <- plot[["qtr"]]


#create time series plots of p10 p25 p50 p90 for different groups
quant_outputs <- quant_outputs %>%
  dplyr::mutate(
    plot = purrr::map2(
      stat_time,
      nice_name,
      ~ {
        # all quantile series names, e.g. "real_salary_income_total_p10", etc.
        q_vars <- unique(.x$qtr$variable)
        
        # derive labels from the variable names themselves
        # e.g. "real_salary_income_total_p10" -> "P10"
        q_labels <- sub(".*_p", "P", q_vars)
        names(q_labels) <- q_vars
        
        plot_stat_multi(
          stat_time     = .x,
          vars          = q_vars,
          series_labels = q_labels,
          view          = "quarterly",
          title         = paste(..2, "Income Quantiles"),
          y_label       = "Income (DOP)"
        )
      }
    )
  )

# Check a plot
quant_outputs$plot[[1]]

# Build a named plot list
quant_plot_list <- quant_outputs$plot
names(quant_plot_list) <- quant_outputs$spec_id

# Save individual PNGs
purrr::iwalk(
  quant_plot_list,
  ~ ggsave(
    filename = paste0(
      "./Outputs/Charts and Tables/Individual Charts/Income Inequality/Quantiles/Fan_Chart_",
      .y, ".png"
    ),
    plot   = .x,
    width  = 4,
    height = 4
  )
)

# Sections: each is a character vector of *plot_list names* 
fan_sections <- list(
  "Total Labor Income - All Workers" = quant_outputs$spec_id[
    quant_outputs$outcome_id  == "total" &
      quant_outputs$work_status == "all"
  ],
  "Total Labor Income - Formal Workers" = quant_outputs$spec_id[
    quant_outputs$outcome_id  == "total" &
      quant_outputs$work_status == "formal"
  ],
  "Total Labor Income - Informal Workers" = quant_outputs$spec_id[
    quant_outputs$outcome_id  == "total" &
      quant_outputs$work_status == "informal"
  ],
  "Salary Income - All Workers" = quant_outputs$spec_id[
    quant_outputs$outcome_id  == "salary" &
      quant_outputs$work_status == "all"
  ],
  "Salary Income - Formal Workers" = quant_outputs$spec_id[
    quant_outputs$outcome_id  == "salary" &
      quant_outputs$work_status == "formal"
  ],
  "Salary Income - Informal Workers" = quant_outputs$spec_id[
    quant_outputs$outcome_id  == "salary" &
      quant_outputs$work_status == "informal"
  ],
  "Independent Income - All Workers" = quant_outputs$spec_id[
    quant_outputs$outcome_id  == "indep" &
      quant_outputs$work_status == "all"
  ],
  "Independent Income - Formal Workers" = quant_outputs$spec_id[
    quant_outputs$outcome_id  == "indep" &
      quant_outputs$work_status == "formal"
  ],
  "Independent Income - Informal Workers" = quant_outputs$spec_id[
    quant_outputs$outcome_id  == "indep" &
      quant_outputs$work_status == "informal"
  ]
)


save_plots_pdf_grid(
  plot_list      = quant_plot_list,
  file           = "./Outputs/Charts and Tables/Fan Charts Work Status and Wage Groups.pdf",
  ncol           = 1,
  nrow           = 2,
  sections       = fan_sections,
  break_sections = TRUE,   # each section paginated separately
  add_page_numbers = TRUE
)










###### Plotting Quantile Ratios ######

