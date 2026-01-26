source("Code/R/00_setup.R")



remotes::install_github("jlesniak12/SurveyTools", force = TRUE)


# --- 1. Load Data and Set Basic Design --- #
Full_ENCFT_clean <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))


#function to create unique PSU/STRATA
individual_level_unique_id <- check_and_fix_survey_ids(Full_ENCFT_clean, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")


#set overall design for individual level quarterly analysis
design_indiv_q <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = individual_level_unique_id,
                            nest = TRUE)


# --- 2. Define a set of Spec tables for computation --- #


# -- Define the outcome variables (what is measured) -- #
outcomes <- c(
  
  #binary vars
  unemployed              = "DESOCUPADO",
  participation           = "PEA",
  
  #continous vars
  salary                  = "real_salary_income_total",
  indep                   = "real_independent_income_total",
  total                   = "real_total_income_total",
  
  #factor vars for shares   
  formality               = "Employment_Status",
  sector                  = "Employment_Sector_Simplified",
  type                    = "Employment_Type",
  size                    = "Wage_group"
)

#human readable labels for outcomes and plot title
outcome_labels <- c(
  unemployed              = "Unemployment Rate",
  participation           = "Participation Rate",
  
  salary                  = "Real salary Income - All Jobs",
  indep                   = "Real independent Income - All Jobs",
  total                   = "Real Total Labor Income - All Jobs",
  
  formality               = "Formal and Informal Employment",
  sector                  = "Employment by Economic Sector",
  type                    = "Employment Type",
  size                    = "Employment by Firm Size"
)

# -- Define Base Filter Conditions where needed to define concept--#

base_filters <- list(
  
  salary      = quote(real_salary_income_total > 0),
  indep       = quote(real_independent_income_total > 0),
  total       = quote(real_total_income_total > 0)
)


# -- define the population (the denominator) -- #

pop <- list(
  #base population groups
    active_pop   = quote(PEA == 1),
    working_age  = quote(EDAD > 15),
    employed     = quote(OCUPADO == 1)
)

# Label suffixes for nicer names in plots/tables

pop_labels = c(
    active_pop     = " - Economically Active Population",
    working_age    = " - Age 15 + Population",
    employed       = " –Employed Population"
  )



#creates all combinations of the dimension filters
labor_market_outcome_specs <- make_spec_table(
  outcomes          = outcomes,
  outcome_labels     = outcome_labels,
  populations        = pop,
  population_labels  = pop_labels,
  base_filters       = base_filters
)

# -- Create spec tables to separate binary, continuous and factor vars for computations-- #


#filter by vars for computation function type (average, proportion, shares of factor)
#population filters reduce extra computations where possible since each row of spec table is computed

binary_outcomes <- c("participation", "unemployed")
binary_pops <- c("working_age", "active_pop")

spec_binary <- dplyr::filter(
  labor_market_outcome_specs,
  (outcome_id %in% binary_outcomes & population_id %in% binary_pops)
)


cont_outcomes <- c("salary", "indep", "total")
cont_pops     <- c("employed")

spec_continous <- dplyr::filter(
  labor_market_outcome_specs,
  (outcome_id %in% cont_outcomes & population_id %in% cont_pops)
)


factor_outcomes <- c("formality", "sector", "type", "size")
factor_pops     <- c("employed")

spec_factor <- dplyr::filter(
  labor_market_outcome_specs,
  (outcome_id %in% factor_outcomes & population_id %in% factor_pops)
)


# --- Income share specs (1-row spec tables) --- #
spec_income_type <- make_spec_table(
  outcomes = c(income_type_share = "IGNORED"),
  outcome_labels = c(income_type_share = "Share of Job Income by Source"),
  populations = list(employed = quote(OCUPADO == 1)),
  population_labels = c(employed = " –Employed Population"),
  base_filters = list(income_type_share = NULL)
)

spec_income_job <- make_spec_table(
  outcomes = c(income_job_share = "IGNORED"),
  outcome_labels = c(income_job_share = "Share of Job Income by Job"),
  populations = list(employed = quote(OCUPADO == 1)),
  population_labels = c(employed = " –Employed Population"),
  base_filters = list(income_job_share = NULL)
)

#define compute function with components
compute_income_type_share <- make_compute_component_share(
  components = c(
    Salary      = "real_salary_income_total",
    Benefits    = "real_benefits_income_total",
    Independent = "real_independent_income_total"
  ),
  measure = "income_type_share"
)

compute_income_job_share <- make_compute_component_share(
  components = c(
    "Primary Job"   = "real_total_income_primary",
    "Secondary Job" = "real_total_income_secondary",
    "Other Jobs"    = "real_total_income_other"
  ),
  measure = "income_job_share"
)

# ---- 3. Run specs --- #

run_plan <- tibble(
  name    = c("Shares", "Mean", "Factor Shares", "Income Type Shares", "Income Job Shares"),
  specs   = list(spec_binary, spec_continous, spec_factor, spec_income_type, spec_income_job),
  compute = list(compute_prop_indicator, compute_mean, compute_prop_factor, compute_income_type_share, compute_income_job_share )
)

group_plan <- c(None = NA_character_, Sex = "Sex", Education = "education", Regions = "Region4", Formality = "Employment_Status", Size = "Wage_group")


run_allowed_groups <- tibble::tibble(
  run = c("Shares", "Mean", "Factor Shares", "Income Type Shares", "Income Job Shares"),
  allowed = list(
    c("None","Sex","Education","Regions"),                        # Shares (binary rates) — no Size, no formal/informal
    c("None","Sex","Education","Regions","Formality","Size"),     # Mean  - all fine
    c("None","Sex","Education","Regions","Formality","Size"),     # Factor Shares — all fine
    c("None","Sex","Education","Regions","Formality","Size"),                 # Income shares — all
    c("None","Sex","Education","Regions","Formality","Size")
  )
)




run_one <- function(specs, compute, group_var) {
  run_specs(
    specs      = specs,
    design     = design_indiv_q,
    compute_fn = compute,
    time_var   = "year_quarter",
    group_var  = if (is.na(group_var)) NULL else group_var,
    return_se  = TRUE
  )
}


#filter out nonsense groups
full_results <- tidyr::expand_grid(run = run_plan$name, group = names(group_plan)) %>%
  dplyr::left_join(run_allowed_groups, by = "run") %>%
  dplyr::rowwise() %>%
  dplyr::filter(.data$group %in% .data$allowed) %>%
  dplyr::ungroup() %>%
  dplyr::select(-allowed)



full_results <- full_results %>%
  mutate(
    specs     = run_plan$specs[match(run, run_plan$name)],
    compute   = run_plan$compute[match(run, run_plan$name)],
    group_var = unname(group_plan[group])
  )

#run all specs
full_results <- full_results %>%
  mutate(
    data = pmap(list(specs, compute, group_var), run_one)
  )




# ---- 4. Create Plots ---- #


plots_all <- c(
  plots_from_jobs(full_results, run_name = "Shares",          group_name = "None"),
  plots_from_jobs(full_results, run_name = "Shares",          group_name = "Sex"),
  plots_from_jobs(full_results, run_name = "Shares",          group_name = "Education"),
  plots_from_jobs(full_results, run_name = "Shares",          group_name = "Regions"),
  
  plots_from_jobs(full_results, run_name = "Mean",            group_name = "None"),
  plots_from_jobs(full_results, run_name = "Mean",            group_name = "Sex"),
  plots_from_jobs(full_results, run_name = "Mean",            group_name = "Education"),
  plots_from_jobs(full_results, run_name = "Mean",            group_name = "Regions"),
  plots_from_jobs(full_results, run_name = "Mean",            group_name = "Formality"),
  plots_from_jobs(full_results, run_name = "Mean",            group_name = "Size"),
  
  plots_from_jobs(full_results, run_name = "Factor Shares",   group_name = "None"),
  plots_from_jobs(full_results, run_name = "Factor Shares",   group_name = "Sex"),
  plots_from_jobs(full_results, run_name = "Factor Shares",   group_name = "Education"),
  plots_from_jobs(full_results, run_name = "Factor Shares",   group_name = "Regions"),
  plots_from_jobs(full_results, run_name = "Factor Shares",   group_name = "Formality"),
  plots_from_jobs(full_results, run_name = "Factor Shares",  group_name = "Size"),
  
  plots_from_jobs(full_results, run_name = "Income Type Shares", group_name = "None"),
  plots_from_jobs(full_results, run_name = "Income Type Shares", group_name = "Sex"),
  plots_from_jobs(full_results, run_name = "Income Type Shares", group_name = "Education"),
  plots_from_jobs(full_results, run_name = "Income Type Shares", group_name = "Regions"),
  plots_from_jobs(full_results, run_name = "Income Type Shares", group_name = "Formality"),
  plots_from_jobs(full_results, run_name = "Income Type Shares", group_name = "Size"),
  
  plots_from_jobs(full_results, run_name = "Income Job Shares", group_name = "None"),
  plots_from_jobs(full_results, run_name = "Income Job Shares", group_name = "Sex"),
  plots_from_jobs(full_results, run_name = "Income Job Shares", group_name = "Education"),
  plots_from_jobs(full_results, run_name = "Income Job Shares", group_name = "Regions"),
  plots_from_jobs(full_results, run_name = "Income Job Shares", group_name = "Formality"),
  plots_from_jobs(full_results, run_name = "Income Job Shares", group_name = "Size")
  
)





# ---- 5. Save plots and create PDF output ---- #

save_path <- file.path(".", config$paths$outputs, config$output_stage, config$out_subdirs$charts)
save_type <- paste(".", config$fig_defaults$format, sep = "")

#save individual plots
purrr::iwalk(
  plots_all,
  ~ ggsave(
    filename = file.path(save_path, "Labor Market Trends", paste(.y, save_type)),  # .y = name (spec_id)
    plot     = .x,                           # .x = ggplot object
    width    = config$fig_defaults$width,
    height   = config$fig_defaults$height
  )
)


# --- Printing PDF --- #

# Sections: each is a character vector of *plot_list names* 
sections <- list(
  "Overall Labor Market Outcomes: General" = c(
    "Participation Rate shares working_age none participation__working_age",
    "Unemployment Rate shares active_pop none unemployed__active_pop"
  ),
  "Overall Labor Market Outcomes: Income" = c(
    "Real Total Labor Income - All Jobs mean employed none total__employed",
    "Share of Job Income by Source income type shares employed none income_type_share__employed",
    "Share of Job Income by Job income job shares employed none income_job_share__employed"
  ),
  "Overall Labor Market Outcomes: Types of Work" = c(
    "Formal and Informal Employment factor shares employed none formality__employed",
    "Employment by Firm Size factor shares employed none size__employed",
    "Employment Type factor shares employed none type__employed",
    "Employment by Economic Sector factor shares employed none sector__employed"
  ),
  
  
  "Formal vs Informal Labor Market Outcomes: Income" = c(
    "Real Total Labor Income - All Jobs mean employed formality total__employed",
    "Share of Job Income by Source income type shares employed formality income_type_share__employed",
    "Share of Job Income by Job income job shares employed formality income_job_share__employed"
  ),
  "Formal vs Informal Labor Market Outcomes: Types of Work" = c(
    "Employment by Firm Size factor shares employed formality size__employed",
    "Employment Type factor shares employed formality type__employed",
    "Employment by Economic Sector factor shares employed formality sector__employed"
  ),
  
  
  "Labor Market Outcomes by Firm Size: Income" = c(
    "Real Total Labor Income - All Jobs mean employed size total__employed",
    "Share of Job Income by Source income type shares employed size income_type_share__employed",
    "Share of Job Income by Job income job shares employed size income_job_share__employed"
  ),
  "Labor Market Outcomes by Firm Size: Types of Work" = c(
    "Formal and Informal Employment factor shares employed size formality__employed",
    "Employment Type factor shares employed size type__employed",
    "Employment by Economic Sector factor shares employed size sector__employed"
  ),
  
  
  "Labor Market Outcomes by Sex: General" = c(
    "Participation Rate shares working_age sex participation__working_age",
    "Unemployment Rate shares active_pop sex unemployed__active_pop"
  ),
  "Labor Market Outcomes by Sex: Income" = c(
    "Real Total Labor Income - All Jobs mean employed sex total__employed",
    "Share of Job Income by Source income type shares employed sex income_type_share__employed",
    "Share of Job Income by Job income job shares employed sex income_job_share__employed"
  ),
  "Labor Market Outcomes by Sex: Types of Work" = c(
    "Formal and Informal Employment factor shares employed sex formality__employed",
    "Employment by Firm Size factor shares employed sex size__employed",
    "Employment Type factor shares employed sex type__employed",
    "Employment by Economic Sector factor shares employed sex sector__employed"
  ),
  
  
  "Labor Market Outcomes by Education: General" = c(
    "Participation Rate shares working_age education participation__working_age",
    "Unemployment Rate shares active_pop education unemployed__active_pop"
  ),
  "Labor Market Outcomes by Education: Income" = c(
    "Real Total Labor Income - All Jobs mean employed education total__employed",
    "Share of Job Income by Source income type shares employed education income_type_share__employed",
    "Share of Job Income by Job income job shares employed education income_job_share__employed"
  ),
  "Labor Market Outcomes by Education: Types of Work" = c(
    "Formal and Informal Employment factor shares employed education formality__employed",
    "Employment by Firm Size factor shares employed education size__employed",
    "Employment Type factor shares employed education type__employed",
    "Employment by Economic Sector factor shares employed education sector__employed"
  ),
  
  
  "Labor Market Outcomes by Region: General" = c(
    "Participation Rate shares working_age regions participation__working_age",
    "Unemployment Rate shares active_pop regions unemployed__active_pop"
  ),
  "Labor Market Outcomes by Region: Income" = c(
    "Real Total Labor Income - All Jobs mean employed regions total__employed",
    "Share of Job Income by Source income type shares employed regions income_type_share__employed",
    "Share of Job Income by Job income job shares employed regions income_job_share__employed"
  ),
  "Labor Market Outcomes by Region: Types of Work" = c(
    "Formal and Informal Employment factor shares employed regions formality__employed",
    "Employment by Firm Size factor shares employed regions size__employed",
    "Employment Type factor shares employed regions type__employed",
    "Employment by Economic Sector factor shares employed regions sector__employed"
  )
  
)

#filtering only plots needed for pdf
keys <- unlist(sections, use.names = FALSE)

# keep only keys that exist
keys <- keys[keys %in% names(plots_all)]

plots_pdf <- plots_all[keys]




save_plots_pdf_grid(
  plot_list      = plots_pdf,
  file           = file.path(save_path, paste("Labor Market Trends", ".pdf", sep ="")),
  ncol           = 1,
  nrow           = 2,
  sections       = sections,
  break_sections = TRUE,   # each section paginated separately
  add_page_numbers = TRUE
)




###### Extra codes


#test individual plots



















#groups
design_indiv_q <- update(
  design_indiv_q,
  group1 =interaction(Sex, education, drop = TRUE)
)


test <- run_specs( specs = spec_binary,
                   design     = design_indiv_q,
                   compute_fn = compute_prop_indicator,
                   time_var   = "year_quarter",
                   group_var  = "group1",
                   return_se  = TRUE
)


d <- test %>%
  tidyr::separate(group,
                  into = c("Sex", "Employment"),
                  sep = "\\.")