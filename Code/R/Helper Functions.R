
library(survey)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

library(tibble)

check_and_fix_survey_ids <- function(data, psu_var, strata_var = NULL, time_var, fix = TRUE) {
  library(dplyr)
  library(rlang)
  
  # Capture variable names
  psu_var <- ensym(psu_var)
  time_var <- ensym(time_var)
  strata_var <- if (!is.null(strata_var)) ensym(strata_var) else NULL
  
  # Check PSU overlap across time
  psu_overlap <- data %>%
    distinct(!!psu_var, !!time_var) %>%
    count(!!psu_var, name = "n_periods") %>%
    filter(n_periods > 1)
  
  # Check strata overlap across time (if provided)
  if (!is.null(strata_var)) {
    strata_overlap <- data %>%
      distinct(!!strata_var, !!time_var) %>%
      count(!!strata_var, name = "n_periods") %>%
      filter(n_periods > 1)
  } else {
    strata_overlap <- tibble()
  }
  
  # Messages
  if (nrow(psu_overlap) == 0 && nrow(strata_overlap) == 0) {
    message("✅ PSU and strata IDs are unique across time periods.")
    fixed <- FALSE
  } else {
    warning(paste0(
      "⚠️ PSU/strata IDs repeat across time.\n",
      "  PSU overlaps: ", nrow(psu_overlap),
      if (!is.null(strata_var)) paste0(" | Strata overlaps: ", nrow(strata_overlap)) else "",
      if (fix) "\n→ Creating unique PSU/strata identifiers." else ""
    ))
    
    fixed <- TRUE
  }
  
  # Optionally fix by making IDs unique
  if (fix && fixed) {
    data <- data %>%
      mutate(
        psu_unique = paste0(!!time_var, "_", !!psu_var),
        strata_unique = if (!is.null(strata_var)) paste0(!!time_var, "_", !!strata_var) else NA
      )
    
    message("✅ New variables created: `psu_unique` and `strata_unique`.")
  }
  
  # Return results
  return(list(
    data = data,
    psu_overlap = psu_overlap,
    strata_overlap = strata_overlap,
    fixed = fixed
  ))
}



clean_svy_names <- function(df, varname) {
  df %>%
    rename_with(~gsub(paste0(varname), "", .x)) %>%      # ① remove the variable name prefix
    rename_with(~gsub("^[[:punct:]]+", "", .x)) %>%           # ② remove leftover punctuation (like "." or "_")
    rename_with(trimws, everything())                         # ③ remove leading/trailing spaces
}



svyby_multi_numeric_fast <- function(design, vars, by_time, by_group = NULL,
                                     fun = svymean, na.rm = TRUE,
                                     vartype = "se", level = 0.95) {
  present <- intersect(vars, names(design$variables))
  if (!length(present)) stop("None of the requested variables are in the design.")
  
  # Build one formula with ALL variables
  form_vars <- reformulate(present)   # ~ v1 + v2 + ...
  by_form   <- if (is.null(by_group)) reformulate(by_time) else reformulate(c(by_time, by_group))
  
  est <- svyby(
    form_vars, by_form, design = design, FUN = fun,
    na.rm = na.rm, vartype = vartype, level = level,
    keep.names = FALSE, covmat = FALSE
  )
  
  # Tidy: wide (var per column) -> long rows
  id_cols <- c(by_time, if (!is.null(by_group)) by_group else NULL)
  core <- est %>%
    select(any_of(id_cols), all_of(present)) %>%
    pivot_longer(cols = all_of(present), names_to = "variable", values_to = "estimate")
  
  if (!is.null(vartype)) {
    # collect matching SE/CI columns for each variable
    se_ci <- est %>%
      select(any_of(id_cols), matches(paste0("^(", paste(present, collapse="|"), ")\\.(se|ci)$"))) %>%
      pivot_longer(-all_of(id_cols), names_to = "var_stat", values_to = "value") %>%
      tidyr::separate(var_stat, into = c("variable","stat"), sep="\\.(?=[^\\.]+$)") %>%
      pivot_wider(names_from = stat, values_from = value)
    # Normalize CI colnames if present
    se_ci <- se_ci %>%
      rename(se = se, lcl = ci_l, ucl = ci_u)
    out <- core %>% left_join(se_ci, by = c(id_cols, "variable"))
  } else {
    out <- core
  }
  
  out
}



svyby_multi_numeric <- function(design, vars, by_time, by_group = NULL,
                                fun = svymean, na.rm = TRUE,
                                vartype = c("se","ci"), level = 0.95) {
  by_form <- if (is.null(by_group)) reformulate(by_time) else reformulate(c(by_time, by_group))
  map_dfr(vars, function(v) {
    est <- svyby(
      as.formula(paste0("~", v)),
      by_form,
      design = design,
      FUN = fun,
      na.rm = na.rm,
      vartype = vartype,
      level = level,
      keep.names = FALSE,
      covmat = FALSE
    )
    # Standardize names and add "variable"
    est %>%
      rename_with(~sub("^ci_l$", "lcl", .x)) %>%
      rename_with(~sub("^ci_u$", "ucl", .x)) %>%
      mutate(variable = v, .before = 1)
  }) %>%
    # Tidy to long (estimate/se/lcl/ucl in columns; value in 'estimate')
    pivot_longer(
      cols = all_of(vars),
      names_to = "drop_me",
      values_to = "estimate",
      values_drop_na = TRUE
    ) %>%
    select(-drop_me)
}




# ---------- with grouping ----------
svyby_multi_numeric_fast_bygroup <- function(design, vars, by_time, by_group,
                                             fun = survey::svymean, na.rm = TRUE,
                                             vartype = "se", level = 0.95) {
  stopifnot(inherits(design, "survey.design"))
  if (!by_time %in% names(design$variables))
    stop("Time variable not found in design: ", by_time)
  if (!by_group %in% names(design$variables))
    stop("Group variable not found in design: ", by_group)
  
  present <- intersect(vars, names(design$variables))
  if (!length(present))
    stop("None of the requested vars exist in the design: ", paste(vars, collapse=", "))
  
  des <- design
  if (!is.ordered(des$variables[[by_time]])) {
    ord <- sort(unique(des$variables[[by_time]]))
    des$variables$`..time` <- factor(des$variables[[by_time]], levels = ord, ordered = TRUE)
    time_col <- "..time"
  } else {
    time_col <- by_time
  }
  
  form_vars <- reformulate(present)                   # ~ v1 + v2 + ...
  by_form   <- reformulate(c(time_col, by_group))     # ~ ..time + group
  
  est <- survey::svyby(form_vars, by_form, design = des, FUN = fun,
                       na.rm = na.rm, vartype = vartype, level = level,
                       keep.names = FALSE, covmat = FALSE)
  
  id_cols    <- c(time_col, by_group)
  se_ci_cols <- grep("\\.(se|ci)$", names(est), value = TRUE)
  value_cols <- setdiff(names(est), c(id_cols, se_ci_cols))
  if (!length(value_cols))
    stop("svyby returned no value columns. Names: ", paste(names(est), collapse=", "))
  
  core <- est %>%
    dplyr::select(dplyr::all_of(id_cols), dplyr::all_of(value_cols)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(value_cols),
                        names_to = "variable", values_to = "estimate")
  
  if (length(se_ci_cols)) {
    se_ci <- est %>%
      dplyr::select(dplyr::all_of(id_cols), dplyr::all_of(se_ci_cols)) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(se_ci_cols),
                          names_to = "var_stat", values_to = "value") %>%
      tidyr::separate(var_stat, into = c("variable","stat"), sep = "\\.(?=[^\\.]+$)") %>%
      tidyr::pivot_wider(names_from = stat, values_from = value) %>%
      dplyr::rename(se = se, lcl = ci_l, ucl = ci_u)
    
    out <- dplyr::left_join(core, se_ci, by = c(setNames(list(id_cols), id_cols), "variable"))
  } else {
    out <- core
  }
  
  if (time_col == "..time")
    out <- dplyr::rename(out, !!by_time := `..time`)
  
  out
}




# ---------- core: no-group ----------
svyby_multi_numeric_fast <- function(design, vars, by_time,
                                     fun = survey::svymean, na.rm = TRUE,
                                     vartype = "se", level = 0.95) {
  # sanity
  stopifnot(inherits(design, "survey.design"))
  if (!by_time %in% names(design$variables))
    stop("Time variable not found in design: ", by_time)
  
  present <- intersect(vars, names(design$variables))
  if (!length(present))
    stop("None of the requested vars exist in the design: ", paste(vars, collapse=", "))
  
  # ensure ordered time WITHOUT using .data pronoun
  des <- design
  if (!is.ordered(des$variables[[by_time]])) {
    ord <- sort(unique(des$variables[[by_time]]))
    des$variables$`..time` <- factor(des$variables[[by_time]], levels = ord, ordered = TRUE)
    time_col <- "..time"
  } else {
    time_col <- by_time
  }
  
  form_vars <- reformulate(present)         # ~ v1 + v2 + ...
  by_form   <- reformulate(time_col)        # ~ ..time (or ~ year_quarter)
  
  est <- survey::svyby(form_vars, by_form, design = des, FUN = fun,
                       na.rm = na.rm, vartype = vartype, level = level,
                       keep.names = FALSE, covmat = FALSE)
  
  # detect columns instead of assuming names == vars
  id_cols    <- time_col
  se_ci_cols <- grep("\\.(se|ci)$", names(est), value = TRUE)
  value_cols <- setdiff(names(est), c(id_cols, se_ci_cols))
  if (!length(value_cols))
    stop("svyby returned no value columns. Names: ", paste(names(est), collapse=", "))
  
  core <- est %>%
    dplyr::select(dplyr::all_of(id_cols), dplyr::all_of(value_cols)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(value_cols),
                        names_to = "variable", values_to = "estimate")
  
  if (length(se_ci_cols)) {
    se_ci <- est %>%
      dplyr::select(dplyr::all_of(id_cols), dplyr::all_of(se_ci_cols)) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(se_ci_cols),
                          names_to = "var_stat", values_to = "value") %>%
      tidyr::separate(var_stat, into = c("variable","stat"), sep = "\\.(?=[^\\.]+$)") %>%
      tidyr::pivot_wider(names_from = stat, values_from = value) %>%
      dplyr::rename(se = se, lcl = ci_l, ucl = ci_u)
    out <- dplyr::left_join(core, se_ci, by = c(setNames(list(id_cols), id_cols), "variable"))
  } else {
    out <- core
  }
  
  if (time_col == "..time")
    out <- dplyr::rename(out, !!by_time := `..time`)
  
  out
}


library(ggplot2)
library(grid)
library(gridExtra)

save_plots_pdf_grid <- function(
    plot_list,
    file         = "plots_grid.pdf",
    ncol         = 2,
    nrow         = 2,
    width        = 11,
    height       = 8.5,
    use_cairo    = TRUE,
    order        = NULL,          # optional: reordering / subsetting of plots
    sections     = NULL,          # optional: section info (titles)
    break_sections = FALSE        # if TRUE, do not mix sections on a page
) {
  is_plot <- function(x) inherits(x, "ggplot")
  
  ## ---- Keep only ggplot objects ----
  plottable <- plot_list[vapply(plot_list, is_plot, logical(1L))]
  if (!length(plottable)) stop("No ggplot objects in plot_list.")
  n_plots <- length(plottable)
  
  ## ---- Apply 'order' (subset + reorder) ----
  # If order is NULL: keep all plottable as-is.
  if (!is.null(order)) {
    if (is.character(order)) {
      if (is.null(names(plottable))) {
        stop("order is character but plottable has no names.")
      }
      if (!all(order %in% names(plottable))) {
        stop("All names in 'order' must exist in names(plot_list).")
      }
      plottable <- plottable[order]  # subset+reorder by name
    } else {
      order <- as.integer(order)
      if (any(is.na(order))) stop("'order' must be coercible to integer indices.")
      if (any(order < 1 | order > n_plots)) {
        stop("'order' indices must be between 1 and ", n_plots, ".")
      }
      plottable <- plottable[order]  # subset+reorder by index
    }
    n_plots <- length(plottable)
  }
  
  if (n_plots == 0) stop("No plots left after applying 'order'.")
  
  ## ---- Section labels (per final plot) ----
  # section_labels[i] is the section name for plottable[[i]] (after ordering).
  # NA means "no section".
  section_labels <- rep(NA_character_, n_plots)
  
  if (!is.null(sections)) {
    if (is.list(sections)) {
      # sections as named list, e.g.:
      #   list("Income" = c(1,2), "Employment" = c(3,4))
      # Indices are positions in the *final* plottable list (after 'order').
      if (is.null(names(sections)) || any(names(sections) == "")) {
        stop("If 'sections' is a list, it must be a *named* list (section titles as names).")
      }
      
      for (sec_name in names(sections)) {
        idx <- sections[[sec_name]]
        
        # Allow indices OR names inside each section element
        if (is.character(idx)) {
          if (is.null(names(plottable))) {
            stop("Section indices for '", sec_name, "' are character but plottable has no names.")
          }
          if (!all(idx %in% names(plottable))) {
            stop("All names in sections[['", sec_name, "']] must exist in names(plot_list) *after* 'order'.")
          }
          idx <- match(idx, names(plottable))
        } else {
          idx <- as.integer(idx)
        }
        
        if (any(idx < 1 | idx > n_plots)) {
          stop("Section indices out of range for section '", sec_name, "'.")
        }
        section_labels[idx] <- sec_name
      }
      
    } else {
      # sections as a vector: one label per final plot
      if (length(sections) != n_plots) {
        stop("If 'sections' is a vector, its length must equal the number of plots after applying 'order'.")
      }
      section_labels <- as.character(sections)
    }
  }
  
  ## ---- Helper: compare with NA handling ----
  equal_with_na <- function(a, b) {
    # returns TRUE/FALSE, never NA
    ifelse(
      is.na(a) & is.na(b), TRUE,
      ifelse(is.na(a) | is.na(b), FALSE, a == b)
    )
  }
  
  ## ---- Build pages ----
  plots_per_page <- ncol * nrow
  grobs <- lapply(plottable, ggplotGrob)
  pages <- list()
  
  if (!break_sections) {
    ## ---- Simple pagination, sections can mix on a page ----
    n_pages <- ceiling(n_plots / plots_per_page)
    pages <- vector("list", n_pages)
    
    for (i in seq_len(n_pages)) {
      from <- (i - 1) * plots_per_page + 1
      to   <- min(i * plots_per_page, n_plots)
      idx  <- from:to
      
      page_grobs <- grobs[idx]
      
      labs_page <- unique(na.omit(section_labels[idx]))
      page_title <- if (!length(labs_page)) NULL else paste(labs_page, collapse = " / ")
      
      pages[[i]] <- gridExtra::arrangeGrob(
        grobs = page_grobs,
        ncol  = ncol,
        nrow  = nrow,
        top   = page_title
      )
    }
    
  } else {
    ## ---- Break between sections: do not mix different section labels on a page ----
    
    # Group consecutive plots that share the same section label.
    if (n_plots == 1) {
      section_group <- 1L
    } else {
      section_group <- integer(n_plots)
      current_group <- 1L
      section_group[1] <- current_group
      
      for (i in 2:n_plots) {
        same_label <- equal_with_na(section_labels[i], section_labels[i - 1])
        if (!same_label) current_group <- current_group + 1L
        section_group[i] <- current_group
      }
    }
    
    # Paginate within each group separately
    for (g in unique(section_group)) {
      idx_block <- which(section_group == g)
      block_labels <- unique(na.omit(section_labels[idx_block]))
      block_title <- if (!length(block_labels)) NULL else block_labels[1]
      
      n_block <- length(idx_block)
      n_block_pages <- ceiling(n_block / plots_per_page)
      
      for (p in seq_len(n_block_pages)) {
        from_pos <- (p - 1) * plots_per_page + 1
        to_pos   <- min(p * plots_per_page, n_block)
        idx_page <- idx_block[from_pos:to_pos]
        
        page_grobs <- grobs[idx_page]
        
        pages[[length(pages) + 1L]] <- gridExtra::arrangeGrob(
          grobs = page_grobs,
          ncol  = ncol,
          nrow  = nrow,
          top   = block_title
        )
      }
    }
  }
  
  ## ---- Write PDF ----
  dev_open <- if (use_cairo && capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  dev_open(file = file, width = width, height = height, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  for (pg in pages) {
    grid::grid.newpage()
    grid::grid.draw(pg)
  }
  
  message("Saved: ", normalizePath(file, winslash = "/"))
}







svyby_multi_convey <- function(design, vars, by_time,
                               fun, na.rm = TRUE,
                               vartype = "se", level = 0.95) {
  stopifnot(inherits(design, "survey.design"))
  if (!by_time %in% names(design$variables))
    stop("Time variable not found in design: ", by_time)
  
  present <- intersect(vars, names(design$variables))
  if (!length(present))
    stop("None of the requested vars exist in the design: ", paste(vars, collapse = ", "))
  
  # ensure ordered time
  des <- design
  if (!is.ordered(des$variables[[by_time]])) {
    ord <- sort(unique(des$variables[[by_time]]))
    des$variables$`..time` <- factor(des$variables[[by_time]], levels = ord, ordered = TRUE)
    time_col <- "..time"
  } else {
    time_col <- by_time
  }
  
  by_form <- reformulate(time_col)  # ~ ..time or ~ year_quarter
  
  out_list <- lapply(present, function(v) {
    form_var <- reformulate(v)  # ~ var
    
    est <- survey::svyby(
      form_var, by_form,
      design = des,
      FUN    = fun,
      na.rm  = na.rm,
      vartype = vartype,
      level   = level,
      keep.names = FALSE,
      covmat    = FALSE
    )
    
    # base tibble
    df <- tibble::tibble(
      !!time_col := est[[time_col]],
      variable   = v,
      estimate   = est[[v]]
    )
    
    ## ---- SE extraction ----
    if ("se" %in% vartype) {
      se_col <- NULL
      
      # most common pattern for convey: column literally called "se"
      if ("se" %in% names(est)) {
        se_col <- "se"
      } else {
        # fallback: try "varname.se"
        cand <- paste0(v, ".se")
        if (cand %in% names(est)) se_col <- cand
      }
      
      if (!is.null(se_col)) {
        df$se <- est[[se_col]]
      } else {
        df$se <- NA_real_
      }
    }
    
    ## ---- CI extraction (if you ever use it) ----
    if (any(grepl("ci", vartype))) {
      lcl_col <- NULL
      ucl_col <- NULL
      
      # common pattern: "ci_l", "ci_u"
      if ("ci_l" %in% names(est)) lcl_col <- "ci_l"
      if ("ci_u" %in% names(est)) ucl_col <- "ci_u"
      
      # fallback: "varname.ci_l", "varname.ci_u"
      if (is.null(lcl_col)) {
        cand_l <- paste0(v, ".ci_l")
        if (cand_l %in% names(est)) lcl_col <- cand_l
      }
      if (is.null(ucl_col)) {
        cand_u <- paste0(v, ".ci_u")
        if (cand_u %in% names(est)) ucl_col <- cand_u
      }
      
      if (!is.null(lcl_col)) df$lcl <- est[[lcl_col]]
      if (!is.null(ucl_col)) df$ucl <- est[[ucl_col]]
    }
    
    df
  })
  
  out <- dplyr::bind_rows(out_list)
  
  # restore original time variable name if we used ..time
  if (time_col == "..time") {
    out <- dplyr::rename(out, !!by_time := `..time`)
  }
  
  out
}


prepare_stat_time <- function(stat_long,
                              time_var      = "year_quarter",
                              use_se        = TRUE,
                              full_quarters = 4) {
  df <- stat_long %>%
    dplyr::mutate(!!time_var := as.character(.data[[time_var]])) %>%
    tidyr::separate(
      !!time_var,
      into   = c("year", "qtr"),
      sep    = "Q",
      remove = FALSE
    ) %>%
    dplyr::mutate(
      year = as.integer(year),
      qtr  = as.integer(qtr)
    ) %>%
    dplyr::group_by(variable, year) %>%
    dplyr::mutate(n_quarters = dplyr::n()) %>%
    dplyr::ungroup()
  
  has_se <- use_se && ("se" %in% names(df))
  
  # ---------- Quarterly series ----------
  # Keep se if present; CIs will be built in the plotting function
  stat_qtr <- df %>%
    dplyr::mutate(time = year + (qtr - 1) / 4)
  
  # ---------- Annual full-year (only years with full_quarters) ----------
  if (has_se) {
    stat_ann_full <- df %>%
      dplyr::filter(n_quarters == full_quarters, !is.na(se)) %>%
      dplyr::group_by(variable, year) %>%
      dplyr::summarise(
        w        = sum(1 / (se^2), na.rm = TRUE),
        estimate = sum(estimate / (se^2), na.rm = TRUE) / w,
        se       = sqrt(1 / w),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(time = year + 0.5) %>%
      dplyr::select(-w)
  } else {
    stat_ann_full <- df %>%
      dplyr::filter(n_quarters == full_quarters) %>%
      dplyr::group_by(variable, year) %>%
      dplyr::summarise(
        estimate = mean(estimate, na.rm = TRUE),
        se       = NA_real_,
        .groups  = "drop"
      ) %>%
      dplyr::mutate(time = year + 0.5)
  }
  
  # ---------- Partial-year annual summary ----------
  if (has_se) {
    stat_ann_part <- df %>%
      dplyr::filter(n_quarters < full_quarters, !is.na(se)) %>%
      dplyr::group_by(variable, year) %>%
      dplyr::summarise(
        w         = sum(1 / (se^2), na.rm = TRUE),
        estimate  = sum(estimate / (se^2), na.rm = TRUE) / w,
        se        = sqrt(1 / w),
        n_quarters = dplyr::first(n_quarters),
        .groups   = "drop"
      ) %>%
      dplyr::mutate(time = year + 0.5) %>%
      dplyr::select(-w)
  } else {
    stat_ann_part <- df %>%
      dplyr::filter(n_quarters < full_quarters) %>%
      dplyr::group_by(variable, year) %>%
      dplyr::summarise(
        estimate   = mean(estimate, na.rm = TRUE),
        se         = NA_real_,
        n_quarters = dplyr::first(n_quarters),
        .groups    = "drop"
      ) %>%
      dplyr::mutate(time = year + 0.5)
  }
  
  list(
    qtr      = stat_qtr,
    ann_full = stat_ann_full,
    ann_part = stat_ann_part
  )
}

plot_stat_var <- function(stat_time,
                          var,
                          nice_name        = NULL,
                          view             = c("both", "quarterly", "annual"),
                          include_partial  = TRUE,
                          y_label          = "Statistic",
                          show_ci_quarterly = FALSE,
                          show_ci_annual    = FALSE,
                          ci_level         = 0.95,
                          title            = NULL,      # <<< NEW
                          subtitle         = NULL,
                          caption          = NULL) {
  
  view <- match.arg(view)
  if (is.null(nice_name)) nice_name <- var
  
  # basic sanity check for ci_level
  if (ci_level <= 0 || ci_level >= 1) {
    stop("ci_level must be between 0 and 1, e.g. 0.90, 0.95, 0.99.")
  }
  ci_pct <- round(ci_level * 100)
  
  # ---------- sensible default captions ----------
  if (is.null(caption)) {
    if (view == "quarterly") {
      caption <- "Quarterly estimates with survey design and weights."
    } else {
      caption <- "Annual values are inverse-variance-weighted across quarters."
    }
  }
  
  q_df <- stat_time$qtr %>%
    dplyr::filter(variable == var, !is.na(estimate), !is.na(time))
  
  full_df <- stat_time$ann_full %>%
    dplyr::filter(variable == var, !is.na(estimate), !is.na(time))
  
  part_df <- stat_time$ann_part %>%
    dplyr::filter(variable == var, !is.na(estimate), !is.na(time))
  
  if (view %in% c("both", "quarterly") && nrow(q_df) == 0) {
    stop("No quarterly data available for variable: ", var)
  }
  if (view == "annual" && nrow(full_df) == 0 &&
      (!include_partial || nrow(part_df) == 0)) {
    stop("No annual data available for variable: ", var)
  }
  
  # x-axis breaks
  years_q    <- if (nrow(q_df)    > 0) unique(q_df$year)    else integer(0)
  years_full <- if (nrow(full_df) > 0) unique(full_df$year) else integer(0)
  years_part <- if (nrow(part_df) > 0) unique(part_df$year) else integer(0)
  year_breaks <- sort(unique(c(years_q, years_full, years_part)))
  
  # ---------- plotting ----------
  p <- ggplot2::ggplot()
  z <- stats::qnorm(0.5 + ci_level / 2)
  
  ## Quarterly CIs
  if (view %in% c("both", "quarterly") && show_ci_quarterly &&
      "se" %in% names(q_df) && any(!is.na(q_df$se))) {
    ci_q <- q_df %>%
      dplyr::mutate(
        ymin = estimate - z * se,
        ymax = estimate + z * se
      )
    
    p <- p +
      ggplot2::geom_ribbon(
        data = ci_q,
        ggplot2::aes(x = time, ymin = ymin, ymax = ymax),
        alpha = 0.15
      )
  }
  
  ## Quarterly line + points
  if (view %in% c("both", "quarterly") && nrow(q_df) > 0) {
    p <- p +
      ggplot2::geom_line(
        data = q_df,
        ggplot2::aes(x = time, y = estimate),
        linewidth = 0.4,
        alpha = 0.6
      ) +
      ggplot2::geom_point(
        data = q_df,
        ggplot2::aes(x = time, y = estimate),
        size = 1.2,
        alpha = 0.6
      )
  }
  
  ## Annual CIs
  if (view %in% c("both", "annual") && show_ci_annual &&
      "se" %in% names(full_df) && any(!is.na(full_df$se))) {
    ci_a <- full_df %>%
      dplyr::mutate(
        ymin = estimate - z * se,
        ymax = estimate + z * se
      )
    
    p <- p +
      ggplot2::geom_ribbon(
        data = ci_a,
        ggplot2::aes(x = time, ymin = ymin, ymax = ymax),
        alpha = 0.15
      )
  }
  
  ## Annual line + points
  if (view %in% c("both", "annual") && nrow(full_df) > 0) {
    p <- p +
      ggplot2::geom_line(
        data = full_df,
        ggplot2::aes(x = time, y = estimate),
        linewidth = 1
      ) +
      ggplot2::geom_point(
        data = full_df,
        ggplot2::aes(x = time, y = estimate),
        size = 2.5
      )
  }
  
  ## Partial years
  if (view %in% c("both", "annual") && include_partial && nrow(part_df) > 0) {
    p <- p +
      ggplot2::geom_point(
        data = part_df,
        ggplot2::aes(x = time, y = estimate),
        size  = 3,
        shape = 21,
        stroke = 1
      )
  }
  
  # ---------- Subtitle logic ----------
  if (is.null(subtitle)) {
    subtitle_parts <- c()
    if (view %in% c("both", "quarterly")) {
      s_q <- "Quarterly values (thin line)"
      if (show_ci_quarterly) s_q <- paste0(s_q, " with ", ci_pct, "% CI band")
      subtitle_parts <- c(subtitle_parts, s_q)
    }
    if (view %in% c("both", "annual")) {
      s_a <- "Annual averages (solid line/points)"
      if (show_ci_annual) s_a <- paste0(s_a, " with ", ci_pct, "% CI band")
      if (include_partial) s_a <- paste0(s_a, "; partial years hollow")
      subtitle_parts <- c(subtitle_parts, s_a)
    }
    subtitle <- paste(subtitle_parts, collapse = " · ")
  }
  
  # ---------- Title ----------
  if (is.null(title)) {
    title <- paste("Time path –", nice_name)
  }
  
  p +
    ggplot2::scale_x_continuous(
      breaks = year_breaks,
      labels = year_breaks
    ) +
    ggplot2::labs(
      x = "Year",
      y = y_label,
      title    = title,     # <--- CUSTOM TITLE HERE
      subtitle = subtitle,
      caption  = caption
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )
}



svy_quantiles_over_time <- function(design,
                                    vars,
                                    by_time   = "year_quarter",
                                    probs     = c(0.10, 0.25, 0.50, 0.75, 0.90)) {
  stopifnot(inherits(design, "survey.design"))
  
  if (!by_time %in% names(design$variables))
    stop("Time variable not found in design: ", by_time)
  
  present <- intersect(vars, names(design$variables))
  if (!length(present))
    stop("None of the requested vars exist in the design: ",
         paste(vars, collapse = ", "))
  
  time_vec <- design$variables[[by_time]]
  times    <- sort(unique(time_vec))
  
  out_list <- list()
  
  for (tt in times) {
    # subset design to a specific time period
    des_t <- subset(design, get(by_time) == tt)
    
    for (v in present) {
      form_var <- reformulate(v)  # ~ var
      
      # one svyquantile call per var/time, all probs at once
      q_obj <- survey::svyquantile(
        form_var,
        design    = des_t,
        quantiles = probs,
        se        = FALSE   # no SE here; just point estimates
      )
      
      # q_obj may be slightly weird (list/matrix/etc.); unlist then
      # keep ONLY the first length(probs) values (the actual quantiles)
      est_raw <- unlist(q_obj)
      est_vec <- as.numeric(est_raw)[seq_along(probs)]
      
      out_list[[length(out_list) + 1]] <- tibble::tibble(
        !!by_time := tt,
        base_var   = v,
        percentile = probs,
        estimate   = est_vec,
        variable   = paste0(v, "_p", round(probs * 100))  # e.g. real_salary_income_total_p10
      )
    }
  }
  
  dplyr::bind_rows(out_list)
}




plot_stat_multi <- function(stat_time,
                            vars,
                            series_labels = NULL,
                            view          = c("quarterly", "annual", "both"),
                            include_partial = TRUE,
                            y_label       = "Statistic",
                            title         = NULL,
                            subtitle      = NULL,
                            caption       = NULL) {
  view <- match.arg(view)
  
  if (is.null(series_labels)) {
    # default: use variable names as labels
    series_labels <- setNames(vars, vars)
  }
  
  # ---------- Filter and prep data ----------
  q_df <- stat_time$qtr %>%
    dplyr::filter(variable %in% vars,
                  !is.na(estimate),
                  !is.na(time)) %>%
    dplyr::mutate(
      series = factor(
        variable,
        levels = vars,
        labels = series_labels[vars]
      )
    )
  
  full_df <- stat_time$ann_full %>%
    dplyr::filter(variable %in% vars,
                  !is.na(estimate),
                  !is.na(time)) %>%
    dplyr::mutate(
      series = factor(
        variable,
        levels = vars,
        labels = series_labels[vars]
      )
    )
  
  part_df <- stat_time$ann_part %>%
    dplyr::filter(variable %in% vars,
                  !is.na(estimate),
                  !is.na(time)) %>%
    dplyr::mutate(
      series = factor(
        variable,
        levels = vars,
        labels = series_labels[vars]
      )
    )
  
  if (view %in% c("both", "quarterly") && nrow(q_df) == 0) {
    stop("No quarterly data available for the requested variables.")
  }
  if (view %in% c("both", "annual") && nrow(full_df) == 0 &&
      (!include_partial || nrow(part_df) == 0)) {
    stop("No annual data available for the requested variables.")
  }
  
  years_q    <- if (nrow(q_df)    > 0) unique(q_df$year)    else integer(0)
  years_full <- if (nrow(full_df) > 0) unique(full_df$year) else integer(0)
  years_part <- if (nrow(part_df) > 0) unique(part_df$year) else integer(0)
  year_breaks <- sort(unique(c(years_q, years_full, years_part)))
  
  if (is.null(title)) {
    title <- "Time path – multiple series"
  }
  
  if (is.null(caption)) {
    if (view == "quarterly") {
      caption <- "Quarterly estimates with survey design and weights."
    } else {
      caption <- "Annual values are (when SEs were used upstream) inverse-variance-weighted across quarters."
    }
  }
  
  # ---------- Build plot ----------
  p <- ggplot2::ggplot()
  
  # Quarterly layer
  if (view %in% c("both", "quarterly") && nrow(q_df) > 0) {
    p <- p +
      ggplot2::geom_line(
        data = q_df,
        ggplot2::aes(x = time, y = estimate, color = series),
        linewidth = 0.5,
        alpha = 0.8
      ) +
      ggplot2::geom_point(
        data = q_df,
        ggplot2::aes(x = time, y = estimate, color = series),
        size = 1.4,
        alpha = 0.9
      )
  }
  
  # Annual full-year layer
  if (view %in% c("both", "annual") && nrow(full_df) > 0) {
    p <- p +
      ggplot2::geom_line(
        data = full_df,
        ggplot2::aes(x = time, y = estimate, color = series),
        linewidth = 0.9
      ) +
      ggplot2::geom_point(
        data = full_df,
        ggplot2::aes(x = time, y = estimate, color = series),
        size = 2.2
      )
  }
  
  # Annual partial-year layer (hollow points)
  if (view %in% c("both", "annual") && include_partial && nrow(part_df) > 0) {
    p <- p +
      ggplot2::geom_point(
        data = part_df,
        ggplot2::aes(x = time, y = estimate, color = series),
        size  = 2.4,
        shape = 21,
        stroke = 0.9
      )
  }
  
  # Default subtitle if not supplied
  if (is.null(subtitle)) {
    subtitle_parts <- c()
    if (view %in% c("both", "quarterly")) {
      subtitle_parts <- c(subtitle_parts, "Quarterly values (thin lines)")
    }
    if (view %in% c("both", "annual")) {
      s_a <- "Annual averages (thicker lines / solid points)"
      if (include_partial) s_a <- paste0(s_a, "; partial years hollow")
      subtitle_parts <- c(subtitle_parts, s_a)
    }
    subtitle <- paste(subtitle_parts, collapse = " · ")
  }
  
  p +
    ggplot2::scale_x_continuous(
      breaks = year_breaks,
      labels = year_breaks
    ) +
    ggplot2::labs(
      x = "Year",
      y = y_label,
      title    = title,
      subtitle = subtitle,
      color    = NULL,
      caption  = caption
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom"
    )
}

compute_stat_time <- function(design,
                              vars,
                              by_time = "year_quarter",
                              stat    = c("mean", "prop", "gini"),
                              use_se  = TRUE,
                              ...) {
  stat <- match.arg(stat)
  
  stopifnot(inherits(design, "survey.design"))
  
  if (!by_time %in% names(design$variables)) {
    stop("Time variable not found in design: ", by_time)
  }
  
  present <- intersect(vars, names(design$variables))
  if (!length(present)) {
    stop("None of the requested vars exist in the design: ",
         paste(vars, collapse = ", "))
  }
  
  if (stat %in% c("mean", "prop")) {
    # Means or proportions (proportions are just means of 0/1 indicators)
    stat_long <- svyby_multi_numeric_fast(
      design  = design,
      vars    = present,
      by_time = by_time,
      fun     = survey::svymean,
      vartype = "se",
      ...
    )
  } else if (stat == "gini") {
    # Gini via convey
    stat_long <- svyby_multi_convey(
      design  = design,
      vars    = present,
      by_time = by_time,
      fun     = convey::svygini,
      vartype = "se",
      ...
    )
  } else {
    stop("Unsupported stat type: ", stat)
  }
  
  # Build quarterly + annual versions
  stat_time <- prepare_stat_time(
    stat_long,
    time_var = by_time,
    use_se   = use_se
  )
  
  list(
    stat     = stat,
    vars     = present,
    by_time  = by_time,
    stat_long = stat_long,
    stat_time = stat_time
  )
}