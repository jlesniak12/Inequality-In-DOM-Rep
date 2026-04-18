source("Code/R/00_setup.R")




# --- 1. Load Data and Set Basic Design --- #
Full_ENCFT_clean <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))


#function to create unique PSU/STRATA
individual_level_unique_id <- check_and_fix_survey_ids(Full_ENCFT_clean, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")


#set overall design for individual level quarterly analysis
design_indiv_q <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = individual_level_unique_id, nest = TRUE)


# --- 2. Define a set of Spec tables for computation --- #



#logs for distribution of those with income
design_indiv_q <- update(
  design_indiv_q,
  log_salary_inc = ifelse(real_salary_income_total > 0, log(real_salary_income_total), NA_real_),
  log_indep_inc  = ifelse(real_independent_income_total > 0, log(real_independent_income_total), NA_real_),
  log_total_inc  = ifelse(real_total_income_total > 0, log(real_total_income_total), NA_real_)
)

years_overlay <- c(2015L, 2019L, 2024L)  # change/add years here

# -- Spec Table Set up -- #


#concepts/outcomes to analyze
outcomes <- c(
  salary = "log_salary_inc",
  indep  = "log_indep_inc",
  total  = "log_total_inc"
)

#human readable labels for concepts. Used in default graphics
outcome_labels <- c(
  salary = "Log Real Salary Income (total)",
  indep  = "Log Real Independent Income",
  total  = "Log Real Total Labor Income"
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



#create spec table
dist_specs_overlay <- make_spec_table(
  outcomes           = outcomes,
  outcome_labels     = outcome_labels,
  populations        = pop,
  population_labels  = pop_labels
)




# --- 3. Define Helper Functions --- #


.escape_regex <- function(x) {
  # escape regex metacharacters for base R / stringi regex
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

compute_density_roles <- function(design,
                                  subset_expr,
                                  var,
                                  series_var,
                                  facet_var = NULL,
                                  sep = "|||",
                                  bw = 0.2,
                                  na.rm = TRUE) {
  
  # --- No facet: compute once grouped by series_var ---
  if (is.null(facet_var) || is.na(facet_var) || !nzchar(facet_var)) {
    
    dens <- compute_density_svy(
      design      = design,
      subset_expr = subset_expr,
      var         = var,
      group_var   = series_var,
      bw          = bw,
      na.rm       = na.rm
    )
    
    return(dplyr::rename(dens, series = group))
  }
  
  # --- With facet: create interaction key series|||facet ---
  design2 <- design
  design2$variables <- dplyr::mutate(
    design2$variables,
    .key = interaction(.data[[series_var]], .data[[facet_var]],
                       sep = sep, drop = TRUE) |>
      as.character()
  )
  
  dens <- compute_density_svy(
    design      = design2,
    subset_expr = subset_expr,
    var         = var,
    group_var   = ".key",
    bw          = bw,
    na.rm       = na.rm
  )
  
  # IMPORTANT: tidyr::separate() needs a plain string/regex, not stringr::fixed()
  sep_regex <- .escape_regex(sep)
  
  dens <- tidyr::separate(
    dens,
    col   = group,
    into  = c("series", "facet"),
    sep   = sep_regex,
    remove = FALSE
  )
  
  dens
}



plot_density_roles <- function(dens_df,
                               title = NULL,
                               subtitle = NULL,
                               x_label = NULL) {
  
  has_facet <- "facet" %in% names(dens_df)
  
  plot_density_svy(
    dens_df   = dens_df,
    series_col = "series",
    facet_col  = if (has_facet) "facet" else NULL,
    title     = title,
    subtitle  = subtitle,
    x_label   = x_label
  )
}


run_year_overlay_one_spec <- function(spec_row,
                                      facet_var,
                                      years_overlay,
                                      bw = 0.2) {
  
  s <- dist_specs_overlay[spec_row, , drop = FALSE]
  varname <- s$outcome_var[[1]]
  
  subset_expr <- rlang::expr(
    (!!s$subset_expr[[1]]) &
      (year %in% !!years_overlay) &
      !is.na(!!rlang::sym(varname))
  )
  
  dens <- compute_density_roles(
    design      = design_indiv_q,
    subset_expr = subset_expr,
    var         = varname,
    series_var  = "year",
    facet_var   = facet_var,
    bw          = bw
  )
  
  title <- s$outcome_label[[1]]
  subtitle <- paste0(
    s$population_label[[1]],
    " — Years: ", paste(years_overlay, collapse = ", ")
  )
  
  plot_density_roles(dens, title = title, subtitle = subtitle, x_label = title)
}


run_group_overlay_one_spec <- function(spec_row,
                                       overlay_group,
                                       year_fixed,
                                       bw = 0.2) {
  
  s <- dist_specs_overlay[spec_row, , drop = FALSE]
  varname <- s$outcome_var[[1]]
  
  subset_expr <- rlang::expr(
    (!!s$subset_expr[[1]]) &
      (year == !!year_fixed) &
      !is.na(!!rlang::sym(varname))
  )
  
  dens <- compute_density_roles(
    design      = design_indiv_q,
    subset_expr = subset_expr,
    var         = varname,
    series_var  = overlay_group,
    facet_var   = NULL,
    bw          = bw
  )
  
  title <- s$outcome_label[[1]]
  subtitle <- paste0(s$population_label[[1]], " — ", year_fixed)
  
  plot_density_roles(dens, title = title, subtitle = subtitle, x_label = title)
}


# --- 4. Run Specs --- #

group_plan <- c(
  None      = NA_character_,
  Sex       = "Sex",
  Education = "education",
  Regions   = "Region4",
  Formality = "Employment_Status",
  Size      = "Wage_group"
)

# Set these once, change anytime
years_overlay_plan <- c(2015L, 2019L, 2024L)
group_overlay_years_plan <- c(2024L)  # or just 2024L if you want one year



jobs_year_overlay <- tidyr::expand_grid(
  spec_row   = seq_len(nrow(dist_specs_overlay)),
  group_name = names(group_plan)
) %>%
  dplyr::mutate(
    facet_var = unname(group_plan[group_name]),
    years_overlay = list(years_overlay_plan),  # same vector repeated for all rows
    plot = purrr::pmap(
      list(spec_row, facet_var, years_overlay),
      run_year_overlay_one_spec
    ),
    key = paste(
      "year_overlay",
      paste(years_overlay_plan, collapse = "-"),
      group_name,
      dist_specs_overlay$spec_id[spec_row],
      sep = "___"
    )
  )

plots_year_overlay <- stats::setNames(jobs_year_overlay$plot, jobs_year_overlay$key)



# Build plots for each spec and each group variable (excluding None)
jobs_group_overlay <- tidyr::expand_grid(
  spec_row   = seq_len(nrow(dist_specs_overlay)),
  group_name = names(group_plan)[names(group_plan) != "None"],
  year_fixed = group_overlay_years_plan
) %>%
  dplyr::mutate(
    overlay_group = unname(group_plan[group_name]),
    plot = purrr::pmap(
      list(spec_row, overlay_group, year_fixed),
      run_group_overlay_one_spec
    ),
    key = paste(
      "group_overlay",
      year_fixed,
      group_name,
      dist_specs_overlay$spec_id[spec_row],
      sep = "___"
    )
  )

plots_group_overlay <- stats::setNames(jobs_group_overlay$plot, jobs_group_overlay$key)





plots_all <- c(plots_group_overlay, plots_year_overlay)

# ---- 5. Save plots and create PDF output ---- #

save_path <- file.path(".", config$paths$outputs, config$output_stage, config$out_subdirs$inequality)
save_type <- paste(".", config$fig_defaults$format, sep = "")

#save individual plots
purrr::iwalk(
  plots_all,
  ~ ggsave(
    filename = file.path(save_path, "Density", paste(.y, save_type)),  # .y = name (spec_id)
    plot     = .x,                           # .x = ggplot object
    width    = config$fig_defaults$width,
    height   = config$fig_defaults$height
  )
)



# --- Printing PDF --- #



# Sections: each is a character vector of *plot_list names* 
sections <- list(
  
  "Income Distribution 2024: Total Income" = c(
    "group_overlay___2024___Formality___total__employed",
    "group_overlay___2024___Size___total__employed",
    "group_overlay___2024___Sex___total__employed",
    "group_overlay___2024___Education___total__employed",
    "group_overlay___2024___Regions___total__employed"

  ),
  
  "Income Distribution 2024: Salary Income" = c(
    "group_overlay___2024___Formality___salary__employed",
    "group_overlay___2024___Size___salary__employed",
    "group_overlay___2024___Sex___salary__employed",
    "group_overlay___2024___Education___salary__employed",
    "group_overlay___2024___Regions___salary__employed"
  ),
  
  "Income Distribution 2024: Independent Income" = c(
    "group_overlay___2024___Formality___indep__employed",
    "group_overlay___2024___Size___indep__employed",
    "group_overlay___2024___Sex___indep__employed",
    "group_overlay___2024___Education___indep__employed",
    "group_overlay___2024___Regions___indep__employed"
  ),
  
  "Income Distribution Time Comp: Total Income" = c(
    "year_overlay___2015-2019-2024___None___total__employed",
    "year_overlay___2015-2019-2024___Formality___total__employed",
    "year_overlay___2015-2019-2024___Size___total__employed",
    "year_overlay___2015-2019-2024___Sex___total__employed",
    "year_overlay___2015-2019-2024___Education___total__employed",
    "year_overlay___2015-2019-2024___Regions___total__employed"
    
  ),
  
  "Income Distribution Time Comp: Salary Income" = c(
    "year_overlay___2015-2019-2024___None___salary__employed",
    "year_overlay___2015-2019-2024___Formality___salary__employed",
    "year_overlay___2015-2019-2024___Size___salary__employed",
    "year_overlay___2015-2019-2024___Sex___salary__employed",
    "year_overlay___2015-2019-2024___Education___salary__employed",
    "year_overlay___2015-2019-2024___Regions___salary__employed"
  ),
  
  "Income Distribution Time Comp: Independent Income" = c(
    "year_overlay___2015-2019-2024___None___indep__employed",
    "year_overlay___2015-2019-2024___Formality___indep__employed",
    "year_overlay___2015-2019-2024___Size___indep__employed",
    "year_overlay___2015-2019-2024___Sex___indep__employed",
    "year_overlay___2015-2019-2024___Education___indep__employed",
    "year_overlay___2015-2019-2024___Regions___indep__employed"
  )
)



#filtering only plots needed for pdf
keys <- unlist(sections, use.names = FALSE)

# keep only keys that exist
keys <- keys[keys %in% names(plots_all)]

plots_pdf <- plots_all[keys]

save_plots_pdf_grid(
  plot_list      = plots_pdf,
  file           = file.path(save_path, paste("Income Distribution Plots", ".pdf", sep ="")),
  ncol           = 1,
  nrow           = 2,
  sections       = sections,
  break_sections = TRUE,   # each section paginated separately
  add_page_numbers = TRUE
)


