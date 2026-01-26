source("Code/R/00_setup.R")




# --- 1. Load Data and Set Basic Design --- #
Full_ENCFT_clean <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))


#function to create unique PSU/STRATA
individual_level_unique_id <- check_and_fix_survey_ids(Full_ENCFT_clean, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")


#set overall design for individual level quarterly analysis
design_indiv_q <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = individual_level_unique_id, nest = TRUE)



# --- 2. Set up Spec Table --- #

#--each row of the table represents a population sub group for analysis

#define concepts/outcomes variables to analyze
outcomes <- c(
  salary      = "real_salary_income_total",
  indep       = "real_independent_income_total",
  total       = "real_total_income_total"
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


#define the populations

pop <- list(
  #base population groups
  employed     = quote(OCUPADO == 1)
)

# Label suffixes for nicer names in plots/tables
pop_labels = c(
  employed       = " –Employed Population"
)

#creates all combinations of the dimension filters
palma_ratio_specs <- make_spec_table(
  outcomes          = outcomes,
  outcome_labels     = outcome_labels,
  populations        = pop,
  population_labels  = pop_labels,
  base_filters       = base_filters
)



# ---- 3. Run specs --- #




run_plan <- tibble(
  name    = c("Palma", "Gini"),
  specs   = list(palma_ratio_specs, palma_ratio_specs),
  compute = list(compute_palma, compute_gini)
)


group_plan <- c(None = NA_character_, Sex = "Sex", Education = "education", Regions = "Region4", Formality = "Employment_Status", Size = "Wage_group")


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


full_results <- tidyr::expand_grid(
  run   = run_plan$name,
  group = names(group_plan)
)

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



# ---- 4. Create Plot lists ---- #

plots_palma <- c(
  plots_from_jobs(full_results, run_name = "Palma",          group_name = "None"),
  plots_from_jobs(full_results, run_name = "Palma",          group_name = "Sex"),
  plots_from_jobs(full_results, run_name = "Palma",          group_name = "Education"),
  plots_from_jobs(full_results, run_name = "Palma",          group_name = "Regions"),
  plots_from_jobs(full_results, run_name = "Palma",          group_name = "Formality")
  
)

plots_gini <- c(
  plots_from_jobs(full_results, run_name = "Gini",          group_name = "None"),
  plots_from_jobs(full_results, run_name = "Gini",          group_name = "Sex"),
  plots_from_jobs(full_results, run_name = "Gini",          group_name = "Education"),
  plots_from_jobs(full_results, run_name = "Gini",          group_name = "Regions"),
  plots_from_jobs(full_results, run_name = "Gini",          group_name = "Formality")
  
)



# ---- 5. Save plots and create PDF output ---- #

save_path <- file.path(".", config$paths$outputs, config$output_stage, config$out_subdirs$charts)
save_type <- paste(".", config$fig_defaults$format, sep = "")

#save individual plots
purrr::iwalk(
  plots_palma,
  ~ ggsave(
    filename = file.path(save_path, "Income Inequality Trends", "Palma Ratio", paste(.y, save_type)),  # .y = name (spec_id)
    plot     = .x,                           # .x = ggplot object
    width    = config$fig_defaults$width,
    height   = config$fig_defaults$height
  )
)

purrr::iwalk(
  plots_gini,
  ~ ggsave(
    filename = file.path(save_path, "Income Inequality Trends", "Gini", paste(.y, save_type)),  # .y = name (spec_id)
    plot     = .x,                           # .x = ggplot object
    width    = config$fig_defaults$width,
    height   = config$fig_defaults$height
  )
)

# --- Printing PDF --- #

# --- Palma Ratio --- #
# Sections: each is a character vector of *plot_list names* 
sections <- list(
  
  "Total Income" = c(
    "Real Total Labor Income - All Jobs palma employed none total__employed",
    "Real Total Labor Income - All Jobs palma employed formality total__employed",
    "Real Total Labor Income - All Jobs palma employed sex total__employed",
    "Real Total Labor Income - All Jobs palma employed education total__employed",
    "Real Total Labor Income - All Jobs palma employed regions total__employed"
  ),
  
  "Salary Income" = c(
    "Real salary Income - All Jobs palma employed none salary__employed",
    "Real salary Income - All Jobs palma employed formality salary__employed",
    "Real salary Income - All Jobs palma employed sex salary__employed",
    "Real salary Income - All Jobs palma employed education salary__employed",
    "Real salary Income - All Jobs palma employed regions salary__employed"
  ),
  
  "Independent Income" = c(
    "Real independent Income - All Jobs palma employed none indep__employed",
    "Real independent Income - All Jobs palma employed formality indep__employed",
    "Real independent Income - All Jobs palma employed sex indep__employed",
    "Real independent Income - All Jobs palma employed education indep__employed",
    "Real independent Income - All Jobs palma employed regions indep__employed"
  )
)

#filtering only plots needed for pdf
keys <- unlist(sections, use.names = FALSE)

# keep only keys that exist
keys <- keys[keys %in% names(plots_palma)]

plots_pdf <- plots_palma[keys]

save_plots_pdf_grid(
  plot_list      = plots_pdf,
  file           = file.path(save_path, paste("Palma Ratios", ".pdf", sep ="")),
  ncol           = 1,
  nrow           = 2,
  sections       = sections,
  break_sections = TRUE,   # each section paginated separately
  add_page_numbers = TRUE
)




# --- Gini Coeff --- #
# Sections: each is a character vector of *plot_list names* 
sections <- list(
  
  "Total Income" = c(
    "Real Total Labor Income - All Jobs gini employed none total__employed",
    "Real Total Labor Income - All Jobs gini employed formality total__employed",
    "Real Total Labor Income - All Jobs gini employed sex total__employed",
    "Real Total Labor Income - All Jobs gini employed education total__employed",
    "Real Total Labor Income - All Jobs gini employed regions total__employed"
  ),
  
  "Salary Income" = c(
    "Real salary Income - All Jobs gini employed none salary__employed",
    "Real salary Income - All Jobs gini employed formality salary__employed",
    "Real salary Income - All Jobs gini employed sex salary__employed",
    "Real salary Income - All Jobs gini employed education salary__employed",
    "Real salary Income - All Jobs gini employed regions salary__employed"
  ),
  
  "Independent Income" = c(
    "Real independent Income - All Jobs gini employed none indep__employed",
    "Real independent Income - All Jobs gini employed formality indep__employed",
    "Real independent Income - All Jobs gini employed sex indep__employed",
    "Real independent Income - All Jobs gini employed education indep__employed",
    "Real independent Income - All Jobs gini employed regions indep__employed"
  )
)

#filtering only plots needed for pdf
keys <- unlist(sections, use.names = FALSE)

# keep only keys that exist
keys <- keys[keys %in% names(plots_gini)]

plots_pdf <- plots_gini[keys]

save_plots_pdf_grid(
  plot_list      = plots_pdf,
  file           = file.path(save_path, paste("Gini Coefficients", ".pdf", sep ="")),
  ncol           = 1,
  nrow           = 2,
  sections       = sections,
  break_sections = TRUE,   # each section paginated separately
  add_page_numbers = TRUE
)

