#===============================================================================
#
# File defines additional functions used in this project.
#
# split_by_indicator: Function to create list of data frames split by indicators

# Plots_from_jobs: Function to batch run plots.
#                
#
#
#
#===============================================================================






source("E:/Packages/R/SurveyTools/R/internal_helpers.R")
source("E:/Packages/R/SurveyTools/R/internal_tidyhelpers.R")
source("E:/Packages/R/SurveyTools/R/utilities.R")
source("E:/Packages/R/SurveyTools/R/internal_subset_helpers.R")
source("E:/Packages/R/SurveyTools/R/time_helpers.R")




split_by_indicator <- function(wide_tbl,
                               group_var = "Employment_Sector",
                               indicator_col = "indicator") {
  
  stopifnot(all(c(group_var, indicator_col) %in% names(wide_tbl)))
  
  split(wide_tbl, wide_tbl[[indicator_col]]) |>
    lapply(function(d) {
      d |>
        select(-dplyr::all_of(indicator_col)) |>
        arrange(.data[[group_var]])
    })
}


make_compute_component_share <- function(components,
                                         drop_nonpos = TRUE,
                                         measure = "share_total") {
  
  if (is.null(components) || !is.character(components) ||
      is.null(names(components)) || any(names(components) == "")) {
    stop("`components` must be a *named* character vector: level -> var name.", call. = FALSE)
  }
  
  # Return a compute function compatible with run_specs() variants
  function(design,
           var = NULL,
           subset_expr = NULL,
           time_var,
           group_var = NULL,
           ...) {
    
    compute_share_components_total(
      design      = design,
      subset_expr = subset_expr,
      components  = components,
      time_var    = time_var,
      group_var   = group_var,
      drop_nonpos = drop_nonpos,
      measure     = measure,
      ...
    )
  }
}



compute_n_unw <- function(design,
                          subset_expr = NULL,
                          time_var,
                          group_var = NULL,
                          measure = "n_unw") {
  if (!inherits(design, "survey.design")) stop("design must be survey.design", call. = FALSE)
  design <- .apply_subset(design, subset_expr)
  
  keys <- c(time_var, group_var) |> stats::na.omit()
  
  out <- design$variables |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(estimate = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(
      time = .data[[time_var]],
      measure = measure
    )
  
  if (!is.null(group_var)) out <- dplyr::mutate(out, group = .data[[group_var]])
  dplyr::select(out, dplyr::any_of("time"), dplyr::any_of("group"), measure, estimate)
}



ranking <- function(df,
                    rank_var,
                    time_var  = NULL,
                    group_var = NULL,
                    desc      = TRUE,
                    ties      = c("min", "dense", "first"),
                    rank_col  = "rank") {
  stopifnot(is.data.frame(df))
  
  ties <- match.arg(ties)
  
  # accept bare names OR strings OR strings stored in variables
  to_name <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.character(x) && length(x) == 1) return(x)
    rlang::as_name(rlang::ensym(x))
  }
  
  rank_var  <- to_name(rank_var)
  time_var  <- to_name(time_var)
  group_var <- to_name(group_var)
  
  group_vars <- c(time_var, group_var)
  group_vars <- group_vars[!is.null(group_vars)]
  
  rank_fun <- switch(
    ties,
    min   = dplyr::min_rank,
    dense = dplyr::dense_rank,
    first = dplyr::row_number
  )
  
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::mutate(
      "{rank_col}" := rank_fun(if (isTRUE(desc)) dplyr::desc(.data[[rank_var]]) else .data[[rank_var]])
    ) %>%
    dplyr::ungroup()
}



compute_checks <- function(design,
                                 subset_expr = NULL,
                                 var,
                                 time_var,
                                 group_var = NULL,
                                 cut_var,
                                 na.rm = FALSE) {
  design <- .apply_subset(design, subset_expr)
  
  v <- design$variables[[var]]
  c <- design$variables[[cut_var]]
  
  design$variables[["flag_zero"]] <- as.numeric(!is.na(v) & v == 0)
  design$variables[["flag_na"]]   <- as.numeric(is.na(v))
  design$variables[["flag_below_cut"]] <- as.numeric(!is.na(v) & !is.na(c) & v < c)
  
  # compute three proportions (means)
  fml <- ~ flag_zero + flag_na + flag_below_cut
  by  <- c(time_var, group_var) |> stats::na.omit()
  byf <- stats::as.formula(paste0("~", paste(by, collapse = "+")))
  
  design <- .coerce_byvars_for_svyby(design, time_var = time_var, group_var = group_var)
  
  res <- survey::svyby(fml, byf, design, survey::svymean, na.rm = na.rm, vartype = NULL)
  
  res <- tibble::as_tibble(res) |>
    dplyr::mutate(
      !!time_var := factor(
        as.character(.data[[time_var]]),
        levels  = levels(.data[[time_var]]),
        ordered = FALSE
      )
    )
  
  out <- res |>
    dplyr::transmute(
      time  = .data[[time_var]],
      group = if (!is.null(group_var)) .data[[group_var]] else NULL,
      share_zero      = .data[["flag_zero"]],
      share_na        = .data[["flag_na"]],
      share_below_cut = .data[["flag_below_cut"]]
    ) |>
    tidyr::pivot_longer(
      cols = c("share_zero","share_na","share_below_cut"),
      names_to = "measure",
      values_to = "estimate"
    )
  
  out
}




