
library(tidyverse)
library(gtsummary)
library(survey)
library(purrr)
library(ggplot2)
library(convey)

library(patchwork)
library(tibble)
library(SurveyTools)
library(openxlsx)
#devtools::install_github("jlesniak12/SurveyTools")


svy_totals_by_time <- function(design, vars, by = "year_quarter") {
  # vars: named character vector, e.g. c(PET="ONE", PEA="PEA_ind")
  out <- purrr::imap_dfr(vars, function(v, nm) {
    f <- as.formula(paste0("~", v))
    b <- as.formula(paste0("~", by))
    res <- survey::svyby(f, b, design, survey::svytotal, na.rm = TRUE, drop.empty.groups = FALSE)
    
    tibble::tibble(
      item = nm,
      !!by := res[[by]],
      estimate = as.numeric(res[[v]])
    )
  })
  out
}


load(file = "./Processed Data/Individual_quarter_ENCFT_clean.rda" )

###### set Design for Quarterly Data ######

check <- check_and_fix_survey_ids(ENCFT_quarterly_individual, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")
individual_level_fixedid <- check$data

#define and prep survey object at the individual level
design_indiv_q <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = individual_level_fixedid,
                            nest = TRUE)



#### Generate Table ####

#prep designs for subsetting

#add variable to count population
design_indiv_q <- update(design_indiv_q, 
                         ONE = 1, 
                         Formal = as.numeric((Employment_Status == "Empleo Formal")),
                         Informal = as.numeric((Employment_Status == "Empleo Informal"))
                         )

#define sub populations
design_workingAge <- subset(design_indiv_q, (age>=15))
design_active <- subset(design_indiv_q, (PEA == 1 & age>=15))





#### Build Conditional Population Table ###

cond_all <- svy_totals_by_time(
  design_indiv_q,
  vars = c("Total Population" = "ONE"),
  by = "year_quarter"
)

cond_working_age <- svy_totals_by_time(
  design_workingAge,
  vars = c("Working Age Population" = "ONE",
           "Active Population"                 = "PEA",
           "Inactive Population"    = "INACTIVO"),
  by = "year_quarter"
)

cond_economically_active <- svy_totals_by_time(
  design_active,
  vars = c("Employed"              = "OCUPADO",
           "UnderEmployed"         = "SUBOCUPADO",
           "Formal Sector"         = "Formal",
           "Informal Sector"       = "Informal",
           "Unemployed"            = "DESOCUPADO"),
  by = "year_quarter"
)

table1 <- bind_rows(cond_all, cond_working_age, cond_economically_active)
table1_wide <- table1 %>%
  pivot_wider(names_from = year_quarter, values_from = estimate)


### Build Indicator Rates Table ####

rate_from_totals <- function(totals_df, num_item, den_item, rate_name) {
  num <- dplyr::filter(totals_df, item == num_item) |>
    dplyr::select(year_quarter, num = estimate)
  den <- dplyr::filter(totals_df, item == den_item) |>
    dplyr::select(year_quarter, den = estimate)
  
  dplyr::left_join(num, den, by = "year_quarter") |>
    dplyr::mutate(item = rate_name,
                  estimate = 100 * num / den) |>
    dplyr::select(item, year_quarter, estimate)
}


# We’ll reuse cond_pet totals for PET/PEA/Ocupados/etc.

table2 <- dplyr::bind_rows(
  rate_from_totals(table1, "Active Population", "Working Age Population",
                   "Participation Rate"),
  rate_from_totals(table1, "Employed", "Working Age Population",
                   "Percent of Active Population Employed"),
  rate_from_totals(table1, "UnderEmployed", "Employed",
                   "Percent of Workers Underemployed"),
  rate_from_totals(table1, "Formal Sector", "Employed",
                   "Percent of Workers in Formal Sector"),
  rate_from_totals(table1, "Informal Sector", "Employed",
                   "Percent of Workers in Informal Sector"),
  rate_from_totals(table1, "Inactive Population", "Working Age Population",
                   "Percent of Working Age Pop Inactive"),
  rate_from_totals(table1, "Unemployed", "Active Population",
                   "Percent of Active Population Unemployed")
)

table2_wide <- table2 %>%
  pivot_wider(names_from = year_quarter, values_from = estimate)




##### Export Tables to Excel #####

tables <- list(
  Condicion  = table1_wide,
  Indicador  = table2_wide
)

sheets <- list(
  "Indicadores" = c("Condicion", "Indicador")
)

export_tables_xlsx(
  tables = tables,
  sheets = sheets,
  file   = "./Outputs/Indicadores.xlsx",
  include_table_titles = TRUE,
  gap_rows = 2,
  with_filter = FALSE
)







###### Calculate and Plot Gini Coefficients Over Time #####


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






tbl1 <- tbl_svysummary(
  data = design_indiv_q,
  by = year_quarter,
  include = Sex
)


#### Test Plotting ####

#Example gtsummary tables
tbl_qtr_sex <- tbl_svysummary(
  data = design_indiv_q,
  by = year_quarter,
  include = Sex,
  statistic = all_categorical() ~ "{p}%"
)

tbl_qtr_edu <- tbl_svysummary(
  data = design_indiv_q,
  by = year_quarter,
  include = education,
  statistic = all_categorical() ~ "{p}%"
)


#testing just plots
# Sections: each is a character vector of *plot_list names* 
sections1 <- list(
  "Total Labor Income - All Workers" = gini_outputs$spec_id[
    gini_outputs$outcome_id  == "total" &
      gini_outputs$work_status == "all"
  ],
  "Total Labor Income - Formal Workers" = gini_outputs$spec_id[
    gini_outputs$outcome_id  == "total" &
      gini_outputs$work_status == "formal"
  ],
  "Total Labor Income - Informal Workers" = gini_outputs$spec_id[
    gini_outputs$outcome_id  == "total" &
      gini_outputs$work_status == "informal"
  ],
  "Salary Income - All Workers" = gini_outputs$spec_id[
    gini_outputs$outcome_id  == "salary" &
      gini_outputs$work_status == "all"
  ],
  "Salary Income - Formal Workers" = gini_outputs$spec_id[
    gini_outputs$outcome_id  == "salary" &
      gini_outputs$work_status == "formal"
  ],
  "Salary Income - Informal Workers" = gini_outputs$spec_id[
    gini_outputs$outcome_id  == "salary" &
      gini_outputs$work_status == "informal"
  ],
  "Independent Income - All Workers" = gini_outputs$spec_id[
    gini_outputs$outcome_id  == "indep" &
      gini_outputs$work_status == "all"
  ],
  "Independent Income - Formal Workers" = gini_outputs$spec_id[
    gini_outputs$outcome_id  == "indep" &
      gini_outputs$work_status == "formal"
  ],
  "Independent Income - Informal Workers" = gini_outputs$spec_id[
    gini_outputs$outcome_id  == "indep" &
      gini_outputs$work_status == "informal"
  ]
)

 


save_items_pdf_grid(
  plot_list,
  file = "report1.pdf",
  ncol = 2, nrow = 2,
  section = sections1,
  break_sections = TRUE,
  add_page_numbers = TRUE
)



items <- list(
  list(type="section", title="Quarterly Demographics"),
  list(type="table", obj=tbl_qtr_sex),
  list(type="table", obj=tbl_qtr_edu),
  
  list(type="section", title="Figures"),
  list(type="table", obj=tbl1),
  list(type="table", obj=tbl1)
)

save_report_pdf(
  items,
  file = "ENCFT_report.pdf",
  ncol = 2, nrow = 2,          # 4 items per page
  page_title = "ENCFT Summary Tables and Figures",
  width = 11, height = 8.5
)




svy_crosstab_bcrd(
  design = design_indiv_q,
  row_var = "Sex",
  col_var = "Employment_Type",
  subset_expr = "PEA == 1"
)


