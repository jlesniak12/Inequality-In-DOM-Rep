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
quantile_specs <- make_spec_table(
  outcomes          = outcomes,
  outcome_labels     = outcome_labels,
  populations        = pop,
  population_labels  = pop_labels,
  base_filters       = base_filters
)


probs_compute <- c(0.10, 0.25, 0.50, 0.75, 0.90)

fan_center <- "p50"
fan_bands  <- list(c("p10","p90"), c("p25","p75"))


# ---- 3. Helper Functions ---- #

fan_required_measures <- function(fan_bands, fan_center = NULL) {
  req <- unlist(fan_bands, use.names = FALSE)
  if (!is.null(fan_center)) req <- c(req, fan_center)
  unique(req)
}


quantiles_to_fan <- function(df,
                             time_col = "time",
                             group_col = "group",
                             measure_col = "measure",
                             estimate_col = "estimate",
                             required_measures = c("p10","p25","p50","p75","p90")) {
  
  stopifnot(is.data.frame(df))
  
  # if no group column, add it
  if (!group_col %in% names(df)) df[[group_col]] <- "All"
  

  # reshape to wide: p10,p25,p50,p75,p90 become columns
  w <- df %>%
    dplyr::filter(.data[[measure_col]] %in% required_measures) %>%
    dplyr::mutate(.m = factor(.data[[measure_col]], levels = required_measures)) %>%
    dplyr::select(
      time  = .data[[time_col]],
      group = .data[[group_col]],
      .m,
      estimate = .data[[estimate_col]]
    ) %>%
    tidyr::pivot_wider(names_from = .m, values_from = estimate)
  
  # ensure columns exist (useful if some measures are missing in some groups/times)
  for (m in required_measures) {
    if (!m %in% names(w)) w[[m]] <- NA_real_
  }
  
  w
}


run_quantile_job <- function(spec_row, group_var, chart,
                             time_var = "year_quarter",
                             probs_compute = c(0.10,0.25,0.50,0.75,0.90),
                             fan_bands = list(c("p10","p90"), c("p25","p75")),
                             fan_center = "p50",
                             band_alphas = c(0.15, 0.25)) {
  
  s <- quantile_specs[spec_row, , drop = FALSE]
  varname <- s$outcome_var[[1]]
  
  subset_expr <- rlang::expr((!!s$subset_expr[[1]]) & !is.na(!!rlang::sym(varname)))
  
  q <- compute_quantiles(
    design      = design_indiv_q,
    var         = varname,
    time_var    = time_var,
    group_var   = if (is.na(group_var)) NULL else group_var,
    probs       = probs_compute,
    subset_expr = subset_expr,
    return_se   = FALSE,
    return_ci   = FALSE
  )
  
  # attach metadata
  q$outcome_label     <- s$outcome_label[[1]]
  q$population_label  <- s$population_label[[1]]
  q$title             <- s$title[[1]]
  q$subtitle          <- s$subtitle[[1]]
  
  if (identical(chart, "quantile_lines")) {
    
    # Ensure a group column exists (for faceting) even when group_var is NULL
    if (!"group" %in% names(q)) q$group <- "All"
    
    return(
      plot_ts_quantiles(
        data         = q,
        time_var     = "time",
        estimate_col = "estimate",
        quantile_col = "measure",
        series_col   = "measure",  # lines = quantiles
        facet_col    = "group"     # panels = groups
      )
    )
  }
  
  if (identical(chart, "quantile_fan")) {
    
    req_measures <- fan_required_measures(fan_bands, fan_center)
    
    fan_df <- quantiles_to_fan(
      df = q,
      required_measures = req_measures
    )
    
    return(
      plot_ts_fan(
        data        = fan_df,
        time_var    = "time",
        center_col  = fan_center,     # can be NULL
        bands       = fan_bands,
        band_alphas = band_alphas,
        series_col  = "group",
        title       = s$outcome_label[[1]],
        subtitle    = s$population_label[[1]]
      )
    )
  }
  
  stop("Unknown chart type: ", chart, call. = FALSE)
}



 # ---- 4. Run Specs ---- #

group_plan <- c(
  None      = NA_character_,
  Sex       = "Sex",
  Education = "education",
  Regions   = "Region4",
  Formality = "Employment_Status",
  Size      = "Wage_group"
)

chart_plan <- tibble::tibble(
  chart = c("quantile_lines"),
  plot_fn = list("lines")
)


jobs_quantile <- tidyr::expand_grid(
  spec_row   = seq_len(nrow(quantile_specs)),
  group_name = names(group_plan),
  chart      = c("quantile_lines")
) %>%
  dplyr::mutate(
    group_var = unname(group_plan[group_name]),
    
    plot = purrr::pmap(
      list(spec_row, group_var, chart),
      ~ run_quantile_job(..1, ..2, ..3,
                         probs_compute = probs_compute,
                         fan_bands = fan_bands,
                         fan_center = fan_center)
    ),
    
    # --- pieces for naming ---
    outcome_id    = quantile_specs$outcome_id[spec_row],
    outcome_label = quantile_specs$outcome_label[spec_row],
    population_id = quantile_specs$population_id[spec_row],
    spec_id       = quantile_specs$spec_id[spec_row],
    
    # --- preferred key: outcome first ---
    key = paste(
      outcome_id,          # salary / indep / total
      chart,               # quantile_lines
      group_name,          # Sex / None / ...
      population_id,       # employed
      spec_id,             # unique
      sep = "___"
  )
)


jobs_quantile2 <- jobs_quantile %>%
  mutate(
    outcome_id    = quantile_specs$outcome_id[spec_row],
    outcome_label = quantile_specs$outcome_label[spec_row],
    population_id = quantile_specs$population_id[spec_row],
    spec_id       = quantile_specs$spec_id[spec_row],
    
    # --- preferred key: outcome first ---
    key = paste(
      chart,               # quantile_lines
      group_name,          # Sex / None / ...
      spec_id,             # unique
      sep = "___"
  )
)
  

plots_quantile <- stats::setNames(jobs_quantile2$plot, jobs_quantile2$key)



# ---- 5. saving Plots and PDF ---- #


save_path <- file.path(".", config$paths$outputs, config$output_stage, config$out_subdirs$inequality)
save_type <- paste(".", config$fig_defaults$format, sep = "")

#save individual plots
purrr::iwalk(
  plots_quantile,
  ~ ggsave(
    filename = file.path(save_path, "Quantile Fan", paste(.y, save_type)),  # .y = name (spec_id)
    plot     = .x,                           # .x = ggplot object
    width    = config$fig_defaults$width,
    height   = config$fig_defaults$height
  )
)


# --- Printing PDF --- #


# Sections: each is a character vector of *plot_list names* 
sections <- list(
  
  "Total Income" = c(
    "quantile_lines___None___total__employed",
    "quantile_lines___Formality___total__employed",
    "quantile_lines___Size___total__employed",
    "quantile_lines___Sex___total__employed",
    "quantile_lines___Education___total__employed",
    "quantile_lines___Regions___total__employed"
  ),
  
  "Salary Income" = c(
    "quantile_lines___None___salary__employed",
    "quantile_lines___Formality___salary__employed",
    "quantile_lines___Size___salary__employed",
    "quantile_lines___Sex___salary__employed",
    "quantile_lines___Education___salary__employed",
    "quantile_lines___Regions___salary__employed"
  ),
  
  "Independent Income" = c(
    "quantile_lines___None___indep__employed",
    "quantile_lines___Formality___indep__employed",
    "quantile_lines___Size___indep__employed",
    "quantile_lines___Sex___indep__employed",
    "quantile_lines___Education___indep__employed",
    "quantile_lines___Regions___indep__employed"
  )
)
  
  

#filtering only plots needed for pdf
keys <- unlist(sections, use.names = FALSE)

# keep only keys that exist
keys <- keys[keys %in% names(plots_quantile)]

plots_pdf <- plots_quantile[keys]

save_plots_pdf_grid(
  plot_list      = plots_pdf,
  file           = file.path(save_path, paste("Income Quantiles", ".pdf", sep ="")),
  ncol           = 1,
  nrow           = 2,
  sections       = sections,
  break_sections = TRUE,   # each section paginated separately
  add_page_numbers = TRUE
)

