source("Code/R/00_setup.R")


#remotes::install_github("jlesniak12/SurveyTools", force = TRUE)


# --- 1. Load Data and Set Basic Design --- #
Full_ENCFT_clean <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))


#function to create unique PSU/STRATA
individual_level_unique_id <- check_and_fix_survey_ids(Full_ENCFT_clean, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")


#set overall design for individual level quarterly analysis
design_indiv_q <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = individual_level_unique_id,
                            nest = TRUE)


# --- 2. Define a set of Spec tables for computation --- #

design_employed_all <- subset(design_indiv_q, OCUPADO == 1)
design_employed_all <- update(design_employed_all, one = 1)


outcomes_sector_within <- c(
  # continuous
  salary = "real_salary_income_total",
  indep  = "real_independent_income_total",
  total  = "real_total_income_total",
  
  # factor composition inside sector
  education = "education",
  sex       = "Sex",
  region    = "Region4",
  formality = "Employment_Status",
  type      = "Employment_Type",
  size      = "Wage_group"
)

outcome_labels_sector_within <- c(
  salary    = "Real Salary Income - All Jobs",
  indep     = "Real Independent Income - All Jobs",
  total     = "Real Total Labor Income - All Jobs",
  education = "Education Composition",
  sex       = "Sex Composition",
  region    = "Regional Composition",
  formality = "Formal vs Informal",
  type      = "Employment Type",
  size      = "Firm Size"
)


# -- Define Base Filter Conditions where needed to define concept--#

base_filters_sector_within  <- list(
  
  salary      = quote(real_salary_income_total > 0),
  indep       = quote(real_independent_income_total > 0),
  total       = quote(real_total_income_total > 0),
  
  education = NULL, sex = NULL, region = NULL, formality = NULL, type = NULL, size = NULL
)




pop_sector_within <- list(employed_sector = quote(one == 1))
pop_labels_sector_within <- c(employed_sector = " — Employed in Sector")


dplyr::count(spec_sector_within, population_id)
dplyr::count(spec_sector_within, is.null(subset_expr))


#creates all combinations of the dimension filters
spec_sector_within <- make_spec_table(
  outcomes          = outcomes_sector_within,
  outcome_labels    = outcome_labels_sector_within,
  populations       = pop_sector_within,
  population_labels = pop_labels_sector_within,
  base_filters      = base_filters_sector_within
)

# -- Create spec tables to separate binary, continuous and factor vars for computations-- #

within_cont_outcomes  <- c("salary","indep","total")
within_factor_outcomes<- c("education","sex","region","formality","type","size")

spec_sector_within_mean <- dplyr::filter(spec_sector_within, outcome_id %in% within_cont_outcomes)
spec_sector_within_fct  <- dplyr::filter(spec_sector_within, outcome_id %in% within_factor_outcomes)



# -- specs for employment and shares -- #
spec_sector_size <- make_spec_table(
  outcomes = c(
    sector_employment_level = "IGNORED",
    sector_employment_share = "IGNORED"
  ),
  outcome_labels = c(
    sector_employment_level = "Employment Level in Sector",
    sector_employment_share = "Sector Share of Total Employment"
  ),
  populations = list(employed_all = quote(OCUPADO == 1)),
  population_labels = c(employed_all = " — Employed (All Sectors)"),
  base_filters = list(
    sector_employment_level = NULL,
    sector_employment_share = NULL
  )
)

spec_sector_level <- dplyr::filter(spec_sector_size, outcome_id == "sector_employment_level")
spec_sector_share <- dplyr::filter(spec_sector_size, outcome_id == "sector_employment_share")



# --- Income share specs (1-row spec tables) --- #
spec_income_type <- make_spec_table(
  outcomes = c(income_type_share = "IGNORED"),
  outcome_labels = c(income_type_share = "Share of Job Income by Source"),
  populations = list(employed_sector = quote(TRUE)),
  population_labels = c(employed_sector = " — Employed in Sector"),
  base_filters = list(income_type_share = NULL)
)

spec_income_job <- make_spec_table(
  outcomes = c(income_job_share = "IGNORED"),
  outcome_labels = c(income_job_share = "Share of Job Income by Job"),
  populations = list(employed_sector = quote(TRUE)),
  population_labels = c(employed_sector = " — Employed in Sector"),
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


# --- define Quantiles --- #

spec_sector_quantiles <- make_spec_table(
  outcomes = c(sector_quantiles = "IGNORED"),
  outcome_labels = c(sector_quantiles = "Income Quantiles (p10/p50/p90)"),
  populations = list(employed_sector = quote(TRUE)),
  population_labels = c(employed_sector = " — Employed in Sector"),
  base_filters = list(sector_quantiles = quote(real_total_income_total > 0))
)

compute_sector_quantiles <- function(design, subset_expr=NULL, time_var, group_var=NULL, ...) {
  compute_quantiles_up(
    design = design,
    subset_expr = subset_expr,
    time_var = time_var,
    group_var = group_var,
    var = "real_total_income_total",
    probs = c(.1,.5,.9),
    fast = TRUE
  ) %>% dplyr::mutate(measure = "sector_quantiles")
}

make_compute_sector_employment_share <- function(sector_value,
                                                 sector_var = "Employment_Sector_Simplified",
                                                 measure = "sector_employment_share") {
  force(sector_value); force(sector_var); force(measure)
  
  function(design,
           var = NULL,
           subset_expr = NULL,
           time_var,
           group_var = NULL,
           na.rm = TRUE,
           return_se = FALSE,
           ...) {
    
    d <- .apply_subset(design, subset_expr)
    d <- .coerce_byvars_for_svyby(d, time_var = time_var, group_var = group_var)
    
    d$variables[[".__in_sector"]] <- as.numeric(d$variables[[sector_var]] == sector_value)
    d$variables[[".__one"]] <- 1
    
    by  <- c(time_var, group_var) |> stats::na.omit()
    byf <- stats::as.formula(paste0("~", paste(by, collapse = "+")))
    
    num_res <- survey::svyby(~.__in_sector, byf, d, survey::svytotal, na.rm = na.rm, keep.names = TRUE)
    den_res <- survey::svyby(~.__one,      byf, d, survey::svytotal, na.rm = na.rm, keep.names = TRUE)
    
    num <- .tidy_svyby_multivar_result(
      res = num_res,
      time_var = time_var,
      group_var = group_var,
      vars = ".__in_sector",
      measure = measure,
      return_se = FALSE,
      return_ci = FALSE,
      require_complete = FALSE,
      var_col = "var"
    )
    
    den <- .tidy_svyby_multivar_result(
      res = den_res,
      time_var = time_var,
      group_var = group_var,
      vars = ".__one",
      measure = measure,
      return_se = FALSE,
      return_ci = FALSE,
      require_complete = FALSE,
      var_col = "var"
    ) %>%
      dplyr::rename(denom_est = estimate) %>%
      dplyr::select(-var)
    
    out <- num %>%
      dplyr::left_join(den, by = intersect(c("time","group"), names(num))) %>%
      dplyr::mutate(
        estimate = dplyr::if_else(.data$denom_est > 0, .data$estimate / .data$denom_est, NA_real_),
        level = sector_value
      ) %>%
      dplyr::select(-denom_est)
    
    # make spec join happy
    out$var <- if (is.null(var)) "IGNORED" else var
    
    d$variables[[".__in_sector"]] <- NULL
    d$variables[[".__one"]] <- NULL
    
    out
  }
}


make_compute_sector_employment_level <- function(sector_value,
                                                 sector_var = "Employment_Sector_Simplified",
                                                 measure = "sector_employment_level") {
  force(sector_value); force(sector_var); force(measure)
  
  function(design,
           var = NULL,
           subset_expr = NULL,
           time_var,
           group_var = NULL,
           na.rm = TRUE,
           return_se = TRUE,
           return_ci = FALSE,
           level = 0.95,
           ...) {
    
    d <- .apply_subset(design, subset_expr)
    d <- .coerce_byvars_for_svyby(d, time_var = time_var, group_var = group_var)
    
    d$variables[[".__in_sector"]] <- as.numeric(d$variables[[sector_var]] == sector_value)
    
    by  <- c(time_var, group_var) |> stats::na.omit()
    byf <- stats::as.formula(paste0("~", paste(by, collapse = "+")))
    vt  <- .vartype_from_flags(return_se = return_se, return_ci = return_ci)
    
    res <- survey::svyby(
      ~.__in_sector, byf, d, survey::svytotal,
      na.rm = na.rm, vartype = vt, level = level, keep.names = TRUE
    )
    
    out <- .tidy_svyby_multivar_result(
      res = res,
      time_var = time_var,
      group_var = group_var,
      vars = ".__in_sector",
      measure = measure,
      return_se = return_se,
      return_ci = return_ci,
      require_complete = FALSE,
      var_col = "var"
    )
    
    # make spec join happy
    out$var <- if (is.null(var)) out$var else var
    out$level <- sector_value
    
    d$variables[[".__in_sector"]] <- NULL
    out
  }
}


# ---- 3. Run specs --- #

run_plan_sector <- tibble::tibble(
  name    = c("Mean", "Factor Shares", "Income Type Shares", "Income Job Shares", "Quantiles"),
  specs   = list(spec_sector_within_mean, spec_sector_within_fct, spec_income_type, spec_income_job, spec_sector_quantiles),
  compute = list(compute_mean,            compute_prop_factor,    compute_income_type_share, compute_income_job_share, compute_sector_quantiles)
)



group_plan_sector <- c(
  None     = NA_character_,
  Sex      = "Sex",
  Regions  = "Region4",
  Formality= "Employment_Status",
  Size     = "Wage_group"
)



run_allowed_groups_sector <- tibble::tibble(
  run = c("Mean","Factor Shares","Income Type Shares","Income Job Shares","Quantiles",
          "Sector Employment Level","Sector Employment Share"),
  allowed = list(
    c("None","Sex","Regions","Formality","Size"),
    c("None"),                         # composition charts: don't also group them
    c("None","Sex","Regions","Formality","Size"),
    c("None","Sex","Regions","Formality","Size"),
    c("None","Sex","Regions","Formality","Size"),
    c("None"),                         # sector size typically just overall
    c("None")
  )
)

run_one <- function(specs, compute, group_var, design) {
  run_specs(
    specs      = specs,
    design     = design,
    compute_fn = compute,
    time_var   = "year_quarter",
    group_var  = if (is.na(group_var)) NULL else group_var,
    return_se  = TRUE
  )
}


sanitize_filename <- function(x) {
  x <- gsub("[/\\\\:*?\"<>|]", "_", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

compute_sector_size_outputs <- function(sector_value,
                                        design_employed_all,
                                        time_var = "year_quarter",
                                        sector_var = "Employment_Sector_Simplified") {
  
  # overall employed by time
  d_all <- update(design_employed_all, one = 1)
  
  tot_all <- survey::svyby(
    ~one, stats::as.formula(paste0("~", time_var)),
    d_all, survey::svytotal,
    keep.names = FALSE
  ) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(time = !!time_var, total_employed = one, se_total = se) %>%
    dplyr::mutate(time = as.character(time))
  
  # sector employed by time (subset first)
  d_sec <- subset(design_employed_all, Employment_Sector_Simplified == sector_value)
  d_sec <- update(d_sec, one = 1)
  
  tot_sec <- survey::svyby(
    ~one, stats::as.formula(paste0("~", time_var)),
    d_sec, survey::svytotal,
    keep.names = FALSE
  ) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(time = !!time_var, sector_employed = one, se_sector = se) %>%
    dplyr::mutate(time = as.character(time))
  
  # level table (formatted like run_specs output)
  level_df <- tot_sec %>%
    dplyr::transmute(
      time,
      estimate = sector_employed,
      se = se_sector,
      measure = "sector_employment_level",
      outcome_id = "sector_employment_level",
      outcome_label = "Employment Level in Sector",
      population_id = "employed_all",
      population_label = " — Employed (All Sectors)",
      level = sector_value
    )
  
  # share table
  share_df <- dplyr::left_join(tot_sec, tot_all, by = "time") %>%
    dplyr::transmute(
      time,
      estimate = dplyr::if_else(total_employed > 0, sector_employed / total_employed, NA_real_),
      se = NA_real_,  # ratio SE not computed here
      measure = "sector_employment_share",
      outcome_id = "sector_employment_share",
      outcome_label = "Sector Share of Total Employment",
      population_id = "employed_all",
      population_label = " — Employed (All Sectors)",
      level = sector_value
    )
  
  list(level = level_df, share = share_df)
}


group_keys_present <- function(x) intersect(c("time","group","level","measure","outcome_id","population_id"), names(x))

build_sector_plots <- function(
    sector_value,
    design_employed_all,
    run_plan_sector,
    spec_sector_size,
    group_plan_sector,
    run_allowed_groups_sector,
    time_var = "year_quarter",
    sector_var = "Employment_Sector_Simplified"
) {
  # sector subset design
  design_sector <- subset(design_employed_all, Employment_Sector_Simplified == sector_value)
  
  run_plan_sector_size <- tibble::tibble(
    name    = c("Sector Employment Level", "Sector Employment Share"),
    specs   = list(spec_sector_level, spec_sector_share),
    compute = list(
      make_compute_sector_employment_level(sector_value, sector_var = sector_var),
      make_compute_sector_employment_share(sector_value, sector_var = sector_var)
    )
  )
  
  
  run_one <- function(specs, compute, group_var, design) {
    tryCatch(
      run_specs(
        specs      = specs,
        design     = design,
        compute_fn = compute,
        time_var   = time_var,
        group_var  = if (is.na(group_var)) NULL else group_var,
        return_se  = TRUE
      ),
      error = function(e) {
        message("\n--- FAIL ---")
        message("sector: ", sector_value)
        message("run compute fn: ", deparse(substitute(compute)))
        message("group_var: ", if (is.na(group_var)) "NULL" else group_var)
        message("specs outcome_ids: ", paste(unique(specs$outcome_id), collapse = ", "))
        message("specs population_ids: ", paste(unique(specs$population_id), collapse = ", "))
        stop(e)
      }
    )
  }
  
  
  make_jobs <- function(run_plan, design) {
    tidyr::expand_grid(run = run_plan$name, group = names(group_plan_sector)) %>%
      dplyr::left_join(run_allowed_groups_sector, by = "run") %>%
      dplyr::rowwise() %>%
      dplyr::filter(.data$group %in% .data$allowed) %>%
      dplyr::ungroup() %>%
      dplyr::select(-allowed) %>%
      dplyr::mutate(
        specs     = run_plan$specs[match(run, run_plan$name)],
        compute   = run_plan$compute[match(run, run_plan$name)],
        group_var = unname(group_plan_sector[group])
      ) %>%
      dplyr::mutate(
        data = purrr::pmap(list(specs, compute, group_var),
                           ~ run_one(..1, ..2, ..3, design = design))
      )
  }
  
  sizes <- compute_sector_size_outputs(
    sector_value = sector_value,
    design_employed_all = design_employed_all,
    time_var = time_var,
    sector_var = sector_var
  )
  
  jobs_size <- tibble::tibble(
    run = c("Sector Employment Share", "Sector Employment Level"),
    group = c("None", "None"),
    data = list(sizes$share, sizes$level)
  )
  
  
  jobs_within <- make_jobs(run_plan_sector,      design_sector)
  jobs <- dplyr::bind_rows(jobs_size, jobs_within)
  
  plots_all <- c(
    plots_from_jobs(jobs, "Sector Employment Share", "None"),
    plots_from_jobs(jobs, "Sector Employment Level", "None"),
    
    plots_from_jobs(jobs, "Mean", "None"),
    plots_from_jobs(jobs, "Mean", "Sex"),
    plots_from_jobs(jobs, "Mean", "Regions"),
    plots_from_jobs(jobs, "Mean", "Formality"),
    plots_from_jobs(jobs, "Mean", "Size"),
    
    plots_from_jobs(jobs, "Quantiles", "None"),
    plots_from_jobs(jobs, "Factor Shares", "None"),
    
    plots_from_jobs(jobs, "Income Type Shares", "None"),
    plots_from_jobs(jobs, "Income Type Shares", "Sex"),
    plots_from_jobs(jobs, "Income Type Shares", "Regions"),
    plots_from_jobs(jobs, "Income Type Shares", "Formality"),
    plots_from_jobs(jobs, "Income Type Shares", "Size"),
    
    plots_from_jobs(jobs, "Income Job Shares", "None"),
    plots_from_jobs(jobs, "Income Job Shares", "Sex"),
    plots_from_jobs(jobs, "Income Job Shares", "Regions"),
    plots_from_jobs(jobs, "Income Job Shares", "Formality"),
    plots_from_jobs(jobs, "Income Job Shares", "Size")
  )
  
  list(
    sector = sector_value,
    jobs   = jobs,
    plots  = plots_all
  )
}


save_sector_outputs <- function(
    sector_value,
    plots_all,
    save_path_base,
    sections_sector = NULL
) {
  sector_dir <- file.path(save_path_base, "Sector Reports", sanitize_filename(as.character(sector_value)))
  dir.create(sector_dir, recursive = TRUE, showWarnings = FALSE)
  
  save_type <- paste0(".", config$fig_defaults$format)
  purrr::iwalk(
    plots_all,
    ~ ggplot2::ggsave(
      filename = file.path(sector_dir, paste0(.y, save_type)),
      plot     = .x,
      width    = config$fig_defaults$width,
      height   = config$fig_defaults$height
    )
  )
  
  if (is.null(sections_sector)) {
    sections_sector <- setNames(list(names(plots_all)), paste0("Sector: ", sector_value))
  }
  
  keys <- unlist(sections_sector, use.names = FALSE)
  keys <- keys[keys %in% names(plots_all)]
  plots_pdf <- plots_all[keys]
  
  save_plots_pdf_grid(
    plot_list        = plots_pdf,
    file             = file.path(sector_dir, paste0("Sector Report - ", sanitize_filename(as.character(sector_value)), ".pdf")),
    ncol             = 1,
    nrow             = 2,
    sections         = sections_sector,
    break_sections   = TRUE,
    add_page_numbers = TRUE
  )
  
  invisible(sector_dir)
}



fin <- build_sector_plots(
  sector_value = "Finance",
  design_employed_all = design_employed_all,
  run_plan_sector = run_plan_sector,
  spec_sector_size = spec_sector_size,
  group_plan_sector = group_plan_sector,
  run_allowed_groups_sector = run_allowed_groups_sector
)


names(fin$plots)        # inspect plot keys
save_path_base <- file.path(".", config$paths$outputs, config$output_stage, config$out_subdirs$labor_market)

save_sector_outputs(
  sector_value = "Finance",
  plots_all = fin$plots,
  save_path_base = save_path_base
)