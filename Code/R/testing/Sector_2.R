source("Code/R/00_setup.R")


#remotes::install_github("jlesniak12/SurveyTools", force = TRUE)


# --- 1. Load Data and Set Basic Design --- #
Full_ENCFT_clean <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))


#function to create unique PSU/STRATA
individual_level_unique_id <- check_and_fix_survey_ids(Full_ENCFT_clean, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")


#set overall design for individual level quarterly analysis
design <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = individual_level_unique_id,
                            nest = TRUE)


table(Full_ENCFT_clean$recieve_any_secondary, Full_ENCFT_clean$Employment_Sector)



# -- Add some needed variables
design <- update(
  design,
  has_salary_income = is.finite(real_salary_income_primary) & real_salary_income_primary > 0,
  has_indep_income  = is.finite(real_independent_income_primary) & real_independent_income_primary > 0,
  zero_salary       = is.finite(real_salary_income_primary) & real_salary_income_primary == 0
)



table(design$variables$recieve_any_secondary, design$variables$Employment_Sector)


# income source type (4-way)
design <- update(
  design,
  income_source_type = factor(
    dplyr::case_when(
      has_salary_income & has_indep_income  ~ "Both",
      has_salary_income & !has_indep_income ~ "Salary only",
      !has_salary_income & has_indep_income ~ "Independent only",
      TRUE                                  ~ "Neither"
    ),
    levels = c("Salary only", "Independent only", "Both", "Neither")
  )
)

# job count type (1 vs 2+)
design <- update(
  design,
  has_secondary = (recieve_any_secondary == 1),
  job_count_type = factor(
    dplyr::if_else(recieve_any_secondary == 1, "2+ jobs", "1 job", missing = NA_character_),
    levels = c("1 job", "2+ jobs")
  )
)



# ---- 2. Define Sectors, Time, Population which are universal ---- #
sector_var <- "Employment_Sector"
time_var   <- "year_quarter"

# assumes design exists and has Employment_Sector as factor
sector_levels <- setdiff(
  levels(design$variables[[sector_var]]),
  c("Government", "Unclassified")
)



dims_sector <- list(
  sector = setNames(
    lapply(sector_levels, \(s) rlang::expr(!!rlang::sym(sector_var) == !!s)),
    sector_levels
  )
)


populations <- list(
  employed = quote(OCUPADO == 1)
)

population_labels <- c(
  employed = "Employed"
)


# ---- 3. Calculate Worker Traits for Sectors---- #

#-- A. Define Vars

# Outcomes here are the *factor variables* whose composition you want
outcomes_workers <- c(
  sex     = "Sex",
  formal  = "Employment_Status",
  empType = "Employment_Type",
  mwGroup = "Wage_group"
)

outcome_labels_workers <- c(
  sex     = "Gender composition",
  formal  = "Formal vs informal",
  empType = "Employment type mix",
  mwGroup = "Wage group mix"
)

# Optional base filters by outcome (e.g., drop Don’t Know just for Wage_group)
base_filters_workers <- list(
  sex     = NULL,
  formal  = NULL,
  empType = NULL,
  mwGroup = quote(Wage_group != "Don’t Know")
)


# -- B Set Spec Table 
spec_workers <- make_spec_table(
  outcomes            = outcomes_workers,
  outcome_labels      = outcome_labels_workers,
  base_filters        = base_filters_workers,
  populations         = populations,
  population_labels   = population_labels,
  dims_subset         = dims_sector,
  subtitle_template   = "{population}{subset}",
  suffix_template     = "{population}{subset}"
)

outcome_registry <- tribble(
  ~outcome_id, ~outcome_var,           ~outcome_label,             ~section,   ~compute_key,     ~excel_sheet,
  "sex",       "Sex",                  "Gender composition",       "workers",  "prop_factor",    "Workers",
  "formal",    "Employment_Status",    "Formal vs Informal",       "workers",  "prop_factor",    "Workers",
  "empType",   "Employment_Type",      "Employment type mix",      "workers",  "prop_factor",    "Workers",
  "mwGroup",   "Wage_group",           "Wage group mix",           "workers",  "prop_factor",    "Workers"
)

spec_workers <- spec_workers %>%
  left_join(
    outcome_registry %>% select(outcome_id, section, compute_key, excel_sheet),
    by = "outcome_id"
  )

spec_workers
# -- Run all specs
res_workers <- run_specs(
  specs      = spec_workers,
  design     = design,
  compute_fn = compute_prop_factor,
  time_var   = time_var,
  group_var  = NULL,
  return_se  = FALSE,
  return_ci  = FALSE,
  measure    = "share"
)



# ---- 4. Calculate Sector Level Statistics ---- #

# -- A. define variables and set spec table 

sector_registry <- tibble::tribble(
  ~outcome_id,     ~outcome_var, ~outcome_label,                 ~section,  ~compute_key,   ~excel_sheet,
  "emp_level",     NA,           "Employed (level)",             "sector",  "pop_total",    "Sector",
  "emp_share_nat", NA,           "Share of national employment", "sector",  "derived",      "Sector",
  "emp_yoy",       NA,           "YoY growth (employment)",      "sector",  "derived",      "Sector",
  "emp_index",     NA,           "Employment index",             "sector",  "derived",      "Sector"
)

outcomes_sector <- c(emp_level = "IGNORED")
outcome_labels_sector <- c(emp_level = "Employed (level)")

spec_sector_level <- make_spec_table(
  outcomes          = outcomes_sector,
  outcome_labels    = outcome_labels_sector,
  populations       = populations,
  population_labels = population_labels,
  dims_subset       = dims_sector,
  subtitle_template = "{population}{subset}",
  suffix_template   = "{population}{subset}"
)

spec_sector_level <- spec_sector_level %>%
  left_join(
    sector_registry %>% select(outcome_id, section, compute_key, excel_sheet),
    by = "outcome_id"
  )


# -- B. Run specs calculating sector level results
res_sector_level <- run_specs(
  specs      = spec_sector_level,
  design     = design,
  compute_fn = compute_pop_total,
  time_var   = time_var,
  measure    = "emp_level"
)

# -- C. define specs for national population and calc at national level

spec_nat_level <- make_spec_table(
  outcomes          = outcomes_sector,
  outcome_labels    = outcome_labels_sector,
  populations       = populations,
  population_labels = population_labels,
  dims_subset       = NULL,
  subtitle_template = "{population}",
  suffix_template   = "{population}"
) %>%
  dplyr::mutate(
    subset_dim   = "sector",
    subset_level = "ALL"
  )

res_nat_level <- run_specs(
  specs      = spec_nat_level,
  design     = design,
  compute_fn = compute_pop_total,
  time_var   = time_var,
  measure    = "nat_emp_level"
)

# D. Combine national level calc and sector level calc then calc growth/index
sector_series <- res_sector_level %>%
  dplyr::transmute(
    time = .data$time,
    sector = .data$subset_level,
    emp_level = .data$estimate
  )

nat_series <- res_nat_level %>%
  dplyr::transmute(
    time = .data$time,
    nat_emp_level = .data$estimate
  )

sector_metrics <- sector_series %>%
  dplyr::left_join(nat_series, by = "time") %>%
  dplyr::group_by(sector) %>%
  dplyr::arrange(time, .by_group = TRUE) %>%
  dplyr::mutate(
    emp_share_nat = dplyr::if_else(nat_emp_level > 0, emp_level / nat_emp_level, NA_real_),
    emp_yoy = (emp_level / dplyr::lag(emp_level, 4)) - 1,
  ) %>%
  dplyr::ungroup()


# helper: extract year from "YYYYQn"
get_year <- function(x) as.integer(str_sub(as.character(x), 1, 4))

sector_metrics <- sector_metrics %>%
  mutate(year = get_year(time))

# baseline = mean of 2015 quarters within each sector
baseline_2015 <- sector_metrics %>%
  filter(year == 2015) %>%
  group_by(sector) %>%
  summarise(emp_2015 = mean(emp_level, na.rm = TRUE), .groups = "drop")

# compute index
sector_metrics <- sector_metrics %>%
  left_join(baseline_2015, by = "sector") %>%
  mutate(emp_index_2015 = if_else(emp_2015 > 0, 100 * emp_level / emp_2015, NA_real_)) %>%
  select(-emp_2015)



# ---- Income Structure ---- #

income_types_registry <- tibble::tribble(
  ~outcome_id,          ~outcome_var,         ~outcome_label,                         ~section,        ~compute_key,    ~excel_sheet,
  "income_source_type", "income_source_type", "Income source: salary vs independent", "income_types",   "prop_factor",   "Income Types",
  "job_count_type",     "job_count_type",     "Jobs: 1 vs 2+ jobs",                   "income_types",   "prop_factor",   "Income Types",
  "zero_salary",        "zero_salary",        "Share with zero salary income",        "income_types",   "prop_indicator","Income Types"
)

outcomes_income_types <- setNames(income_types_registry$outcome_var, income_types_registry$outcome_id)
labels_income_types   <- setNames(income_types_registry$outcome_label, income_types_registry$outcome_id)

spec_income_types <- make_spec_table(
  outcomes          = outcomes_income_types,
  outcome_labels    = labels_income_types,
  base_filters      = list(income_source_type = NULL, job_count_type = NULL, zero_salary = NULL),
  populations       = populations,
  population_labels = population_labels,
  dims_subset       = dims_sector,
  subtitle_template = "{population}{subset}",
  suffix_template   = "{population}{subset}"
) %>%
  left_join(income_types_registry %>% select(outcome_id, section, compute_key, excel_sheet),
            by = "outcome_id")

# factor compositions
res_income_types_factor <- run_specs(
  specs      = spec_income_types %>% filter(compute_key == "prop_factor"),
  design     = design,
  compute_fn = compute_prop_factor,
  time_var   = time_var,
  group_var  = NULL,
  measure    = "share"
)

# indicator shares
res_income_types_ind <- run_specs(
  specs      = spec_income_types %>% filter(compute_key == "prop_indicator"),
  design     = design,
  compute_fn = compute_prop_indicator,
  time_var   = time_var,
  group_var  = NULL,
  na.rm      = TRUE,
  measure    = "share"
)

res_income_types <- dplyr::bind_rows(res_income_types_factor, res_income_types_ind)


# ---- 5. Calculate Income Statistics for Sectors ---- #

# -- salary Workers Only
salary_facts_registry <- tibble::tribble(
  ~outcome_id,        ~outcome_var,                  ~outcome_label,                                   ~section,          ~compute_key,    ~excel_sheet,
  "mean_salary_pos",  "real_salary_income_primary", "Mean salary (salary earners only)",              "earnings_salary", "mean",          "Earnings - Salary",
  "med_salary_pos",   "real_salary_income_primary", "Median salary (salary earners only)",            "earnings_salary", "quantile_p50",   "Earnings - Salary"
)

outcomes_salary_facts <- setNames(salary_facts_registry$outcome_var, salary_facts_registry$outcome_id)
labels_salary_facts   <- setNames(salary_facts_registry$outcome_label, salary_facts_registry$outcome_id)

spec_salary_facts <- make_spec_table(
  outcomes          = outcomes_salary_facts,
  outcome_labels    = labels_salary_facts,
  base_filters      = list(
    mean_salary_pos = quote(has_salary_income),
    med_salary_pos  = quote(has_salary_income)
  ),
  populations       = populations,
  population_labels = population_labels,
  dims_subset       = dims_sector,
  subtitle_template = "{population}{subset}",
  suffix_template   = "{population}{subset}"
) %>%
  left_join(salary_facts_registry %>% select(outcome_id, section, compute_key, excel_sheet),
            by = "outcome_id")

res_mean_salary_pos <- run_specs(
  specs      = spec_salary_facts %>% filter(compute_key == "mean"),
  design     = design,
  compute_fn = compute_mean,
  time_var   = time_var,
  measure    = "mean"
)

res_med_salary_pos <- run_specs(
  specs      = spec_salary_facts %>% filter(compute_key == "quantile_p50"),
  design     = design,
  compute_fn = compute_quantiles,
  time_var   = time_var,
  probs      = 0.5,
  measure    = "p50"
)



# -- Palma and Density 

# -- Palma
palma_registry <- tribble(
  ~outcome_id,   ~outcome_var,                   ~outcome_label,            ~section,    ~compute_key, ~excel_sheet,
  "palma_sal",   "real_salary_income_primary",   "Palma ratio (salary)",    "earnings",  "palma",      "Earnings"
)

outcomes_palma <- setNames(palma_registry$outcome_var, palma_registry$outcome_id)
labels_palma   <- setNames(palma_registry$outcome_label, palma_registry$outcome_id)

base_filters_palma <- list(
  palma_sal = quote(has_salary_income)
)

spec_palma <- make_spec_table(
  outcomes          = outcomes_palma,
  outcome_labels    = labels_palma,
  base_filters      = base_filters_palma,
  populations       = populations,
  population_labels = population_labels,
  dims_subset       = dims_sector,
  subtitle_template = "{population}{subset}",
  suffix_template   = "{population}{subset}"
) %>%
  left_join(palma_registry %>% select(outcome_id, section, compute_key, excel_sheet),
            by = "outcome_id")


res_palma_salary <- run_specs(
  specs      = spec_palma,
  design     = design,
  compute_fn = compute_palma,
  time_var   = time_var,
  group_var  = NULL,
  na.rm      = TRUE,
  method     = "linear",
  measure    = "palma",
  return_se  = FALSE,
  return_ci  = FALSE
)

# density (salary earners only)
dens_salary_all <- map_dfr(sector_levels, \(s) {
  compute_density_svy(
    design      = design,
    subset_expr = rlang::expr(
      OCUPADO == 1 &
        Employment_Sector == !!s &
        year %in% c(2015, 2024) &
        has_salary_income
    ),
    var       = "real_salary_income_primary",
    group_var = "year",
    n         = 512,
    adjust    = 1,
    na.rm     = TRUE
  ) %>% mutate(sector = s)
})




# ---_Indep Workers ----#
indep_facts_registry <- tibble::tribble(
  ~outcome_id,        ~outcome_var,                    ~outcome_label,                                        ~section,         ~compute_key,    ~excel_sheet,
  "mean_indep_pos",   "real_independent_income_primary", "Mean independent income (indep earners only)",        "earnings_indep", "mean",          "Earnings - Indep",
  "med_indep_pos",    "real_independent_income_primary", "Median independent income (indep earners only)",      "earnings_indep", "quantile_p50",   "Earnings - Indep"
)

spec_indep_facts <- make_spec_table(
  outcomes          = setNames(indep_facts_registry$outcome_var, indep_facts_registry$outcome_id),
  outcome_labels    = setNames(indep_facts_registry$outcome_label, indep_facts_registry$outcome_id),
  base_filters      = list(
    mean_indep_pos = quote(has_indep_income),
    med_indep_pos  = quote(has_indep_income)
  ),
  populations       = populations,
  population_labels = population_labels,
  dims_subset       = dims_sector,
  subtitle_template = "{population}{subset}",
  suffix_template   = "{population}{subset}"
) %>%
  left_join(indep_facts_registry %>% select(outcome_id, section, compute_key, excel_sheet),
            by="outcome_id")

res_mean_indep_pos <- run_specs(
  specs      = spec_indep_facts %>% filter(compute_key=="mean"),
  design     = design,
  compute_fn = compute_mean,
  time_var   = time_var,
  measure    = "mean"
)


#independent relevance indicator
indep_sector_stats <- res_income_types_factor %>%
  dplyr::filter(outcome_id == "income_source_type",
                level %in% c("Independent only", "Both")) %>%
  dplyr::group_by(subset_level, time) %>%
  dplyr::summarise(indep_any = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(subset_level) %>%
  dplyr::summarise(indep_any_mean = mean(indep_any, na.rm = TRUE), .groups = "drop")
indep_sectors <- indep_sector_stats %>%
  dplyr::filter(indep_any_mean >= 0.30) %>%
  dplyr::pull(subset_level)

# -- Palma
palma_registry <- tribble(
  ~outcome_id,   ~outcome_var,                   ~outcome_label,            ~section,    ~compute_key, ~excel_sheet,
  "palma_ind",   "real_independent_income_primary",   "Palma ratio (independent)",    "earnings",  "palma",      "Earnings"
)

outcomes_palma <- setNames(palma_registry$outcome_var, palma_registry$outcome_id)
labels_palma   <- setNames(palma_registry$outcome_label, palma_registry$outcome_id)

base_filters_palma_ind <- list(
  palma_ind = quote(has_indep_income)
)

spec_palma <- make_spec_table(
  outcomes          = outcomes_palma,
  outcome_labels    = labels_palma,
  base_filters      = base_filters_palma_ind,
  populations       = populations,
  population_labels = population_labels,
  dims_subset       = dims_sector,
  subtitle_template = "{population}{subset}",
  suffix_template   = "{population}{subset}"
) %>%
  left_join(palma_registry %>% select(outcome_id, section, compute_key, excel_sheet),
            by = "outcome_id")


spec_indep_facts_hi <- spec_indep_facts %>%
  dplyr::filter(subset_level %in% indep_sectors)

spec_palma_hi <- spec_palma %>%
  dplyr::filter(subset_level %in% indep_sectors)


res_med_indep_pos <- run_specs(
  specs      = spec_indep_facts_hi %>% dplyr::filter(compute_key=="quantile_p50"),
  design     = design,
  compute_fn = compute_quantiles,
  time_var   = time_var,
  probs      = 0.5,
  measure    = "p50"
)

res_palma_indep <- run_specs(
  specs      = spec_palma_hi,
  design     = design,
  compute_fn = compute_palma,
  time_var   = time_var,
  na.rm      = TRUE,
  method     = "linear",
  measure    = "palma"
)


dens_indep_all <- purrr::map_dfr(indep_sectors, \(s) {
  compute_density_svy(
    design      = design,
    subset_expr = rlang::expr(
      OCUPADO == 1 &
        Employment_Sector == !!s &
        year %in% c(2015, 2024) &
        has_indep_income
    ),
    var       = "real_independent_income_primary",
    group_var = "year",
    n         = 512,
    adjust    = 1,
    na.rm     = TRUE
  ) %>%
    dplyr::mutate(sector = s)
})




# ---- Total Income ---- #
total_facts_registry <- tibble::tribble(
  ~outcome_id,        ~outcome_var,              ~outcome_label,                       ~section,        ~compute_key,    ~excel_sheet,
  "mean_total",       "real_total_income_total", "Mean total income (all employed)",   "earnings_total", "mean",          "Earnings - Total",
  "med_total_pos",    "real_total_income_total", "Median total income (>0)",           "earnings_total", "quantile_p50",   "Earnings - Total"
)

spec_total_facts <- make_spec_table(
  outcomes          = setNames(total_facts_registry$outcome_var, total_facts_registry$outcome_id),
  outcome_labels    = setNames(total_facts_registry$outcome_label, total_facts_registry$outcome_id),
  base_filters      = list(
    mean_total    = quote(is.finite(real_total_income_total) & real_total_income_total >= 0),
    med_total_pos = quote(is.finite(real_total_income_total) & real_total_income_total > 0)
  ),
  populations       = populations,
  population_labels = population_labels,
  dims_subset       = dims_sector,
  subtitle_template = "{population}{subset}",
  suffix_template   = "{population}{subset}"
)

res_mean_total <- run_specs(
  specs      = spec_total_facts %>% filter(outcome_id=="mean_total"),
  design     = design,
  compute_fn = compute_mean,
  time_var   = time_var,
  measure    = "mean"
)

res_med_total <- run_specs(
  specs      = spec_total_facts %>% filter(outcome_id=="med_total_pos"),
  design     = design,
  compute_fn = compute_quantiles,
  time_var   = time_var,
  probs      = 0.5,
  measure    = "p50"
)



# -- Palma and Density 

# -- Palma
palma_registry <- tribble(
  ~outcome_id,   ~outcome_var,                   ~outcome_label,            ~section,    ~compute_key, ~excel_sheet,
  "palma_total",   "real_total_income_total",   "Palma ratio (total)",    "earnings",  "palma",      "Earnings"
)

outcomes_palma <- setNames(palma_registry$outcome_var, palma_registry$outcome_id)
labels_palma   <- setNames(palma_registry$outcome_label, palma_registry$outcome_id)

base_filters_palma_total <- list(
  palma_total = quote(is.finite(real_total_income_total) & real_total_income_total > 0)
)

spec_palma <- make_spec_table(
  outcomes          = outcomes_palma,
  outcome_labels    = labels_palma,
  base_filters      = base_filters_palma_total,
  populations       = populations,
  population_labels = population_labels,
  dims_subset       = dims_sector,
  subtitle_template = "{population}{subset}",
  suffix_template   = "{population}{subset}"
) %>%
  left_join(palma_registry %>% select(outcome_id, section, compute_key, excel_sheet),
            by = "outcome_id")


res_palma_total <- run_specs(
  specs      = spec_palma,
  design     = design,
  compute_fn = compute_palma,
  time_var   = time_var,
  group_var  = NULL,
  na.rm      = TRUE,
  method     = "linear",
  measure    = "palma",
  return_se  = FALSE,
  return_ci  = FALSE
)

# density (total income)
dens_total_all <- map_dfr(sector_levels, \(s) {
  compute_density_svy(
    design      = design,
    subset_expr = rlang::expr(
      OCUPADO == 1 &
        Employment_Sector == !!s &
        year %in% c(2015, 2024) &
        (real_total_income_total > 0)
    ),
    var       = "real_total_income_total",
    group_var = "year",
    n         = 512,
    adjust    = 1,
    na.rm     = TRUE
  ) %>% mutate(sector = s)
})





# ---- Generating Plots ---- #

make_sector_plots <- function(sector_name) {
  
  plots <- list()
  
  # ---- Sector overview (from sector_metrics) ----
  sm <- sector_metrics %>% filter(sector == sector_name)
  
  plots[["sector_emp_level"]] <- plot_ts_single(
    sm %>% transmute(time, estimate = emp_level),
    title = paste0(sector_name, " — Employment (level)")
  )
  
  plots[["sector_share_nat"]] <- plot_ts_single(
    sm %>% transmute(time, estimate = emp_share_nat),
    title = paste0(sector_name, " — Share of national employment"),
    percent = TRUE
  )
  
  plots[["sector_yoy"]] <- plot_ts_single(
    sm %>% transmute(time, estimate = emp_yoy),
    title = paste0(sector_name, " — YoY employment growth"),
    percent = TRUE
  )
  
  plots[["sector_index_2015"]] <- plot_ts_single(
    sm %>% transmute(time, estimate = emp_index_2015),
    title = paste0(sector_name, " — Employment index (2015=100)")
  )
  
  # ---- Worker composition (from res_workers) ----
  for (oid in c("sex","formal","empType","mwGroup")) {
    dfw <- res_workers %>%
      filter(subset_level == sector_name, outcome_id == oid)
    
    plots[[paste0("workers_", oid)]] <- plot_share_stacked(
      dfw,
      title = paste0(sector_name, " — ", unique(dfw$outcome_label)[1]),
      y_percent = TRUE
    )
  }
  
  # ---- Income structure (new) ----
  for (oid in c("income_source_type", "job_count_type")) {
    dfi <- res_income_types %>%
      filter(subset_level == sector_name, outcome_id == oid)
    
    plots[[paste0("income_types_", oid)]] <- plot_share_stacked(
      dfi,
      title = paste0(sector_name, " — ", unique(dfi$outcome_label)[1]),
      y_percent = TRUE
    )
  }
  
  
  # ---- Salary: mean + median salary (two-series TS) ----
  mean_df <- res_mean_salary_pos %>%
    filter(subset_level == sector_name) %>%
    transmute(time, estimate, group = "Mean")
  
  med_df <- res_med_salary_pos %>%
    filter(subset_level == sector_name, measure == "p50") %>%   # compute_quantiles returns measure
    transmute(time, estimate, group = "Median")
  
  sal_df <- bind_rows(mean_df, med_df)
  
  plots[["earnings_salary_mean_median"]] <- plot_ts_multi(
    sal_df,
    series_col = "group",
    title = paste0(sector_name, " — Salary income (primary job): mean vs median")
  )
  
  # ---- Palma ----
  palma_df <- res_palma_salary %>% filter(subset_level == sector_name)
  
  plots[["earnings_salary_palma"]] <- plot_ts_single(
    palma_df,
    title = paste0(sector_name, " — Palma ratio (salary)")
  )
  
  # ---- Density overlay (2015 vs 2024) ----
  dens_df <- dens_salary_all %>% filter(sector == sector_name)
  
  plots[["earnings_salary_density_2015_2024"]] <- plot_density_svy(
    dens_df,
    series_col = "group",   # compute_density_svy uses "group" for the grouping var
    title = paste0(sector_name, " — Salary density (2015 vs 2024)")
  )
  
  # ---- Zero Salary Shares ---- #
  zs <- res_income_types %>%
    filter(subset_level == sector_name, outcome_id == "zero_salary") %>%
    transmute(time, estimate)
  
  plots[["income_types_zero_salary"]] <- plot_ts_single(
    zs,
    title = paste0(sector_name, " — Share with zero salary income"),
    percent = TRUE
  )
  
  if (sector_name %in% indep_sectors) {
    # add indep mean/median, palma, density plots
  
    #---- Independent Income ---- #
       mean_df <- res_mean_indep_pos %>%
      filter(subset_level == sector_name) %>%
      transmute(time, estimate, group = "Mean")
       
       med_df <- res_med_indep_pos %>%
         filter(subset_level == sector_name, measure == "p50") %>%   # compute_quantiles returns measure
         transmute(time, estimate, group = "Median")
       
       sal_df <- bind_rows(mean_df, med_df)
       
       plots[["earnings_independent_mean_median"]] <- plot_ts_multi(
         sal_df,
         series_col = "group",
         title = paste0(sector_name, " — Independent income (primary job): mean vs median")
       )
       
       # ---- Palma ----
     palma_df <- res_palma_indep %>% filter(subset_level == sector_name)
     
     plots[["earnings_independent_palma"]] <- plot_ts_single(
       palma_df,
       title = paste0(sector_name, " — Palma ratio (independent income)")
     )
     
     # ---- Density overlay (2015 vs 2024) ----
     dens_df <- dens_indep_all %>% filter(sector == sector_name)
     
     plots[["earnings_independent_density_2015_2024"]] <- plot_density_svy(
       dens_df,
       series_col = "group",    
       title = paste0(sector_name, " — Independent Income density (2015 vs 2024)")
    )
    }
  
  # ---- Total Income ---- #
  mean_df <- res_mean_total %>%
    filter(subset_level == sector_name) %>%
    transmute(time, estimate, group = "Mean")
  
  med_df <- res_med_total %>%
    filter(subset_level == sector_name, measure == "p50") %>%   # compute_quantiles returns measure
    transmute(time, estimate, group = "Median")
  
  sal_df <- bind_rows(mean_df, med_df)
  
  plots[["earnings_total_mean_median"]] <- plot_ts_multi(
    sal_df,
    series_col = "group",
    title = paste0(sector_name, " — Total income (all jobs, salary  and indep): mean vs median")
  )
  
  # ---- Palma ----
  palma_df <- res_palma_total %>% filter(subset_level == sector_name)
  
  plots[["earnings_total_palma"]] <- plot_ts_single(
    palma_df,
    title = paste0(sector_name, " — Palma ratio (all jobs, salary  and indep)")
  )
  
  # ---- Density overlay (2015 vs 2024) ----
  dens_df <- dens_total_all %>% filter(sector == sector_name)
  
  plots[["total_earnings_density_2015_2024"]] <- plot_density_svy(
    dens_df,
    series_col = "group",   # compute_density_svy uses "group" for the grouping var
    title = paste0(sector_name, " — Total Income (all jobs, salary and indep) density (2015 vs 2024)")
  )
  
  
  plots
}



export_one_sector_tables <- function(sector_name, out_dir = "Outputs/sector_reports") {
  
  # ---- Sector overview table (levels + derived) ----
  tbl_sector <- sector_metrics %>%
    filter(sector == sector_name) %>%
    transmute(
      time,
      emp_level,
      emp_share_nat,
      emp_yoy,
      emp_index_2015
    )
  
  # ---- Workers: one wide table per outcome (levels x time) ----
  worker_tables <- lapply(c("sex","formal","empType","mwGroup"), function(oid) {
    res_workers %>%
      filter(subset_level == sector_name, outcome_id == oid) %>%
      build_table_wide(
        row_keys  = "level",
        col_keys  = "time",
        value_col = "estimate"
      )
  })
  names(worker_tables) <- paste0("workers_", c("sex","formal","empType","mwGroup"))
  
  # ---- Earnings: mean + median tables (time series) ----
  tbl_mean <- res_mean_salary_pos %>%
    filter(subset_level == sector_name) %>%
    select(time, estimate) %>%
    rename(mean_salary = estimate)
  
  tbl_med <- res_med_salary_pos %>%
    filter(subset_level == sector_name, measure == "p50") %>%
    select(time, estimate) %>%
    rename(median_salary = estimate)
  
  tbl_earn <- tbl_mean %>% left_join(tbl_med, by = "time")
  
  # shares (time series)
  tbl_shares <- res_shares %>%
    filter(subset_level == sector_name) %>%
    transmute(time, indicator = outcome_label, estimate) %>%
    build_table_wide(row_keys = "indicator", col_keys = "time", value_col = "estimate")
  
  # palma (time series)
  tbl_palma <- res_palma %>%
    filter(subset_level == sector_name) %>%
    select(time, estimate) %>%
    rename(palma = estimate)
  
  tables <- c(
    list(sector_overview = tbl_sector,
         earnings_mean_median = tbl_earn,
         earnings_shares = tbl_shares,
         earnings_palma = tbl_palma),
    worker_tables
  )
  
  file <- file.path(out_dir, sector_name, paste0("sector_tables_", sector_name, ".xlsx"))
  
  export_tables_xlsx(
    tables = tables,
    file   = file,
    sheets = list(
      "Sector"   = c("sector_overview"),
      "Workers"  = names(worker_tables),
      "Earnings" = c("earnings_mean_median", "earnings_shares", "earnings_palma")
    )
  )
  
  invisible(file)
}



#save plots and pdf

save_path_base <- file.path(".", config$paths$outputs, config$output_stage, config$out_subdirs$labor_market)

for (sec in sector_levels) {
  
  pl <- make_sector_plots(sec)
  
  # Optional: group plots into sections in the PDF
  sections <- list(
    "Sector overview" = grep("^sector_", names(pl), value = TRUE),
    "Workers"         = grep("^workers_", names(pl), value = TRUE),
    "Income structure"= grep("^income_types_", names(pl), value = TRUE),
    "Earnings"        = grep("^earnings_", names(pl), value = TRUE)
  )
  
  pdf_file <- file.path(save_path_base, paste0("Sector Report ", sec, ".pdf"))
  
  save_plots_pdf_grid(
    plot_list = pl,
    file      = pdf_file,
    ncol      = 1,
    nrow      = 2,
    sections  = sections,
    break_sections = TRUE,
    add_page_numbers = TRUE
  )
  
  save_plots_dir(
    plot_list = pl,
    dir = file.path(save_path_base, "Sectors", sec),
    format = "png"
  )
  
  # excel_file <- file.path(save_path_base, "Sectors", sec, paste0("Tables_", sec, ".xls"))
  # 
  # export_one_sector_tables(sec, out_dir = excel_file )

}



