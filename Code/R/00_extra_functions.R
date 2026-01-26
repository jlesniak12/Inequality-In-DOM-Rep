plots_from_jobs <- function(jobs, run_name, group_name,
                            facet_labeller = ggplot2::label_value) {
  
  # ---- helpers (local) ----
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  
  one_or_na <- function(dat, col) {
    if (!col %in% names(dat)) return(NA_character_)
    v <- unique(dat[[col]])
    v <- v[!is.na(v)]
    if (!length(v)) NA_character_ else as.character(v[[1]])
  }
  
  format_ratio_run <- function(run_name) {
    # "Log p90_p10" -> "log(p90/p10)"
    m <- regexec("^Log\\s+p(\\d+)_p(\\d+)$", run_name)
    mm <- regmatches(run_name, m)[[1]]
    if (length(mm) == 3) {
      paste0("log(p", mm[2], "/p", mm[3], ")")
    } else {
      NULL
    }
  }
  
  append_title <- function(title, ratio_label) {
    if (is.null(ratio_label) || !nzchar(ratio_label)) return(title)
    title <- title %||% ""
    if (!nzchar(title)) return(ratio_label)
    paste0(title, " (", ratio_label, ")")
  }
  
  
  ratio_label <- format_ratio_run(run_name)
  
  # ---- select job row ----
  rows <- jobs %>%
    dplyr::filter(.data$run == run_name, .data$group == group_name)
  
  if (nrow(rows) == 0) {
    stop(
      "plots_from_jobs(): No rows found for run='", run_name,
      "' and group='", group_name, "'.\n",
      "Available runs: ", paste(unique(jobs$run), collapse = ", "), "\n",
      "Available groups: ", paste(unique(jobs$group), collapse = ", "),
      call. = FALSE
    )
  }
  if (nrow(rows) > 1) {
    warning(
      "plots_from_jobs(): Multiple rows found for run='", run_name,
      "' and group='", group_name, "'. Using the first row.",
      call. = FALSE
    )
  }
  
  if (!"data" %in% names(rows)) {
    stop(
      "plots_from_jobs(): `jobs` has no `data` list-column. Did you run `mutate(data = pmap(...))`?\n",
      "Columns present: ", paste(names(jobs), collapse = ", "),
      call. = FALSE
    )
  }
  
  df <- rows[["data"]][[1]]
  
  if (!is.data.frame(df)) {
    stop("plots_from_jobs(): `data` column did not contain a data.frame/tibble.", call. = FALSE)
  }
  if (!"spec_id" %in% names(df)) {
    stop("plots_from_jobs(): `df` is missing required column `spec_id`.", call. = FALSE)
  }
  
  has_group <- "group" %in% names(df)
  split_list <- df %>% dplyr::group_split(.data$spec_id)
  
  # name plots
  plot_names <- purrr::map_chr(split_list, function(x) {
    paste(
      one_or_na(x, "outcome_label"),
      tolower(run_name),
      one_or_na(x, "population_id"),
      tolower(group_name),
      one_or_na(x, "spec_id"),
      sep = " "
    )
  })
  plot_names <- make.unique(plot_names)
  
  # flags
  is_factor_output <- "level" %in% names(df)
  ts_percent <- identical(run_name, "Shares")
  
  # subtitle augmentation line for ratio runs
  ratio_line <- format_ratio_run(run_name)
  
  plots <- if (is_factor_output) {
    
    bad <- purrr::keep(split_list, ~ !"level" %in% names(.x))
    if (length(bad)) {
      stop(
        "Factor-share output expects column `level`, but it is missing for spec_id(s): ",
        paste(purrr::map_chr(bad, ~ unique(.x$spec_id)[1]), collapse = ", "),
        call. = FALSE
      )
    }
    
    purrr::map(
      split_list,
      ~ plot_share_stacked(
        .x,
        facet_col = if (has_group) "group" else NULL,
        facet_labeller = facet_labeller,
        title     = one_or_na(.x, "title"),
        subtitle  = append_subtitle(one_or_na(.x, "subtitle"), ratio_line),
        y_percent = TRUE
      )
    )
    
  } else {
    
    if (has_group) {
      purrr::map(
        split_list,
        ~ plot_ts_multi(
          .x,
          series_col = "group",
          facet_labeller = facet_labeller,
          title = append_title(one_or_na(.x, "title"), ratio_label),
          subtitle = one_or_na(.x, "subtitle"),
          percent  = ts_percent
        )
      )
    } else {
      purrr::map(
        split_list,
        ~ plot_ts_single(
          .x,
          facet_labeller = facet_labeller,
          title = append_title(one_or_na(.x, "title"), ratio_label),
          subtitle = one_or_na(.x, "subtitle"),
          percent  = ts_percent
        )
      )
    }
  }
  
  rlang::set_names(plots, plot_names)
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

plot_share_stacked
