source("Code/R/00_setup.R")





# --- Load Data and Set Basic Design --- #
Full_ENCFT_clean <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))


.fill_template_tokens <- function(template, population = "", subset = "", outcome = "") {
  # enforce true scalars
  template   <- as.character(template)[1]
  population <- as.character(population)[1]
  subset     <- as.character(subset)[1]
  outcome    <- as.character(outcome)[1]
  
  out <- template
  out <- sub("\\{population\\}", population, out)
  out <- sub("\\{subset\\}", subset, out)
  out <- sub("\\{outcome\\}", outcome, out)
  out
}


#function to create unique PSU/STRATA
individual_level_unique_id <- check_and_fix_survey_ids(Full_ENCFT_clean, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")


#set overall design for individual level quarterly analysis
design_indiv_q <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = individual_level_unique_id,
                            nest = TRUE)


# --- Define a Spec table for analysis --- #


# -- Define the outcome variables (what is measured) -- #
outcomes <- c(
  
  #binary vars
  unemployed              = "DESOCUPADO",
  employed                = "OCUPADO",
  participation           = "PEA",
  
  #continous vars
  salary         = "real_salary_income_total",
  indep          = "real_independent_income_total",
  total          = "real_total_income_total"
)

#human readable labels for outcomes
outcome_labels <- c(
  unemployed        = "Unemployment Rate",
  employed            = "Employment Rate",
  participation       = "Participation Rate",
  
  salary = "Real salary Income - All Jobs",
  indep  = "Real independent Income - All Jobs",
  total  = "Real Total Labor Income - All Jobs"
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
    active_pop     = " - Economically Active",
    working_age    = " - Age 15 +",
    employed       = " – Employed"
  )



#creates all combinations of the dimension filters
labor_market_outcome_specs <- make_spec_table(
  outcomes          = outcomes,
  outcome_labels     = outcome_labels,
  populations        = pop,
  population_labels  = pop_labels,
  base_filters       = base_filters
)

# -- Create 2 spec tables to seperate binary and continue vars for run -- #
binary_outcomes <- c("participation", "unemployed", "employed")

spec_binary <- dplyr::filter(
  labor_market_outcome_specs,
  outcome_id %in% binary_outcomes
)

cont_outcomes <- c("salary", "indep", "total")

spec_continous <- dplyr::filter(
  labor_market_outcome_specs,
  outcome_id %in% cont_outcomes
)


# ---- Run specs --- #


res_binary_shares <- run_specs(
  specs = spec_binary,
  design = design_indiv_q,
  compute_fn = compute_prop_indicator,
  time_var = "year_quarter",
  group_var = NULL,   # or NULL, or any subgroup
  vartype = "se"
)


res_inc_avg <- run_specs(
  specs = spec_continous,
  design = design_indiv_q,
  compute_fn = compute_mean,
  time_var = "year_quarter",
  group_var = NULL,   # or NULL, or any subgroup
  vartype = "se"
)


labor_market_outcome_specs |>
  dplyr::distinct(population_id, subtitle) |>
  print(n = Inf)



# --- Distribution of Worker Plots

outcomes_share <- c(
  employed = "OCUPADO"
)

outcome_labels_share <- c(
  employed = "Employment Structure"
)

pop_employed <- list(
  employed = quote(OCUPADO == 1)
)

pop_labels_employed <- c(
  employed = " – Employed"
)

spec_employed_share <- make_spec_table(
  outcomes          = outcomes_share,
  outcome_labels    = outcome_labels_share,
  populations       = pop_employed,
  population_labels = pop_labels_employed
)

res_formality <- run_specs(
  specs      = spec_employed_share,
  design     = design_indiv_q,
  compute_fn = compute_prop_indicator,
  time_var   = "year_quarter",
  group_var  = "Employment_Status",
  vartype    = "se"
)

res_sector <- run_specs(
  specs      = spec_employed_share,
  design     = design_indiv_q,
  compute_fn = compute_prop_indicator,
  time_var   = "year_quarter",
  group_var  = "Employment_Sector_Simplified",
  vartype    = "se"
)

res_employer_type <- run_specs(
  specs      = spec_employed_share,
  design     = design_indiv_q,
  compute_fn = compute_prop_indicator,
  time_var   = "year_quarter",
  group_var  = "Employment_Type",
  vartype    = "se"
)

res_firm_size <- run_specs(
  specs      = spec_employed_share,
  design     = design_indiv_q,
  compute_fn = compute_prop_indicator,
  time_var   = "year_quarter",
  group_var  = "Wage_group",
  vartype    = "se"
)



p1 <- plot_share_stacked(
  res_formality,
  time_var     = "time",
  estimate_col = "estimate",
  level        = "group",
  facet_by_group = FALSE,
  title        = "Employment structure over time"
)

res_formality_nat <- compute_prop_factor_se(
  design     = design_indiv_q,
  subset_expr= quote(OCUPADO == 1),
  var        = "Employment_Status",
  time_var   = "year_quarter",
  vartype    = NULL,
  group_var  = NULL,
  
)


p2 <- plot_share_stacked(res_formality_nat, time_var="time")

