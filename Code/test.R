
# ======================================================================
# CLEAN, TESTABLE SURVEY WORKFLOW (Compute layer + Specs + Plotting)
# Drop this whole script into one file and source() it.
# Designed for ENCFT-style survey.design objects (survey + convey optional)
# ======================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(ggplot2)
  library(survey)
  library(convey)
})

# ----------------------------
# Small helpers
# ----------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Add year/quarter/time_num robustly (handles "2014Q3" and "2014 Q3")
add_time_fields <- function(df, time_col = "time") {
  stopifnot(time_col %in% names(df))
  tv <- df[[time_col]]
  if (is.factor(tv)) tv <- as.character(tv)
  
  if (inherits(tv, c("Date", "POSIXct", "POSIXt"))) {
    year    <- as.integer(format(tv, "%Y"))
    month   <- as.integer(format(tv, "%m"))
    quarter <- ((month - 1L) %/% 3L) + 1L
  } else if (is.character(tv)) {
    year <- suppressWarnings(as.integer(substr(tv, 1L, 4L)))
    
    q <- stringr::str_match(tv, "Q\\s*([1-4])")[, 2]
    quarter <- suppressWarnings(as.integer(q))
    
    # fallback: last char
    if (all(is.na(quarter))) {
      q_char  <- substr(tv, nchar(tv), nchar(tv))
      quarter <- suppressWarnings(as.integer(q_char))
      quarter[!(quarter %in% 1:4)] <- NA_integer_
    }
  } else {
    year    <- as.integer(floor(as.numeric(tv)))
    quarter <- NA_integer_
  }
  
  df %>%
    dplyr::mutate(
      time = tv,
      year = year,
      quarter = quarter,
      time_num = dplyr::if_else(!is.na(quarter),
                                year + (quarter - 1) / 4,
                                as.numeric(year))
    )
}

# Effective sample size (Kish) from weights
neff_kish <- function(w) {
  w <- w[is.finite(w) & !is.na(w)]
  if (!length(w)) return(NA_real_)
  s2 <- sum(w^2)
  if (s2 <= 0) return(NA_real_)
  (sum(w)^2) / s2
}

# Create composite group variable in a design (or raw df)
# vars: character vector of variable names to combine
make_composite_group <- function(df, vars, new_var = "group_comp", sep = " | ") {
  stopifnot(all(vars %in% names(df)))
  df[[new_var]] <- interaction(!!!rlang::syms(vars), drop = TRUE, sep = sep)
  df
}

# ----------------------------
# Spec generator (v2)
# Outcomes -> title; Populations -> subtitle
# ----------------------------

make_stat_specs_v2 <- function(outcomes,
                               outcome_labels,
                               base_filters,
                               populations,                # named list of quoted expressions
                               population_labels = NULL,   # named character vector; defaults to pop_id
                               dims_subset = NULL,         # optional additional subset dims
                               dim_label_suffix = NULL,    # optional suffix map for dims
                               include_all_suffix = FALSE,
                               sep_subtitle = " · ") {
  
  # checks
  if (is.null(names(outcomes)) || any(names(outcomes) == "")) {
    stop("`outcomes` must be a named character vector (names are outcome IDs).")
  }
  if (is.null(names(outcome_labels)) || any(names(outcome_labels) == "")) {
    stop("`outcome_labels` must be a named character vector.")
  }
  if (!all(names(outcomes) %in% names(outcome_labels))) {
    stop("All names(outcomes) must appear in names(outcome_labels).")
  }
  if (is.null(names(base_filters)) || any(names(base_filters) == "")) {
    stop("`base_filters` must be a named list. Names should match names(outcomes).")
  }
  if (!all(names(outcomes) %in% names(base_filters))) {
    stop("All names(outcomes) must appear in names(base_filters).")
  }
  if (is.null(populations) || length(populations) == 0) {
    stop("`populations` must be a non-empty named list of quoted expressions.")
  }
  if (is.null(names(populations)) || any(names(populations) == "")) {
    stop("`populations` must be a named list (names are pop IDs).")
  }
  
  if (is.null(population_labels)) {
    population_labels <- stats::setNames(names(populations), names(populations))
  }
  if (!all(names(populations) %in% names(population_labels))) {
    stop("All population IDs must appear in names(population_labels).")
  }
  
  outcome_ids <- names(outcomes)
  outcome_df <- tibble(
    outcome_id    = outcome_ids,
    outcome_var   = unname(outcomes[outcome_ids]),
    outcome_base  = base_filters[outcome_ids],
    outcome_label = unname(outcome_labels[outcome_ids])
  )
  
  pop_ids <- names(populations)
  pop_df <- tibble(
    pop_id    = pop_ids,
    pop_base  = populations[pop_ids],
    pop_label = unname(population_labels[pop_ids])
  )
  
  # dims grid
  if (is.null(dims_subset) || length(dims_subset) == 0) {
    dim_grid <- tibble(.dummy = 1) %>% select(-.dummy)
    dim_names <- character(0)
  } else {
    dim_names <- names(dims_subset)
    dim_tibbles <- lapply(dim_names, function(dn) {
      levs <- names(dims_subset[[dn]])
      if (is.null(levs) || any(levs == "")) {
        stop("dims_subset[['", dn, "']] must be a named list with non-empty names.")
      }
      tibble(!!dn := levs)
    })
    dim_grid <- tidyr::expand_grid(!!!dim_tibbles)
  }
  
  specs <- tidyr::expand_grid(outcome_df, pop_df, dim_grid)
  
  and_exprs <- function(expr_list) {
    Reduce(function(a, b) bquote(.(a) & .(b)), expr_list)
  }
  
  specs <- specs %>%
    mutate(
      subset_expr = pmap(
        select(., outcome_id, pop_id, all_of(dim_names)),
        function(outcome_id, pop_id, ...) {
          filt_list <- list(
            base_filters[[outcome_id]],
            populations[[pop_id]]
          )
          lev_ids <- list(...)
          for (i in seq_along(lev_ids)) {
            dn  <- dim_names[[i]]
            lev <- lev_ids[[i]]
            filt_list[[length(filt_list) + 1L]] <- dims_subset[[dn]][[lev]]
          }
          if (any(vapply(filt_list, is.null, logical(1)))) {
            stop("One or more filters are NULL. Check base_filters/populations/dims_subset.")
          }
          and_exprs(filt_list)
        }
      )
    )
  
  build_suffix <- function(...) {
    lev_ids <- list(...)
    parts <- character(0)
    for (i in seq_along(lev_ids)) {
      dn  <- dim_names[[i]]
      lev <- lev_ids[[i]]
      if (!include_all_suffix && identical(lev, "all")) next
      
      suffix_map <- if (!is.null(dim_label_suffix) && !is.null(dim_label_suffix[[dn]])) {
        dim_label_suffix[[dn]]
      } else NULL
      
      if (!is.null(suffix_map) && lev %in% names(suffix_map)) {
        sfx <- suffix_map[[lev]]
      } else {
        sfx <- paste0(" – ", dn, ": ", lev)
      }
      if (!is.null(sfx) && nzchar(sfx)) parts <- c(parts, sfx)
    }
    if (length(parts) == 0) "" else paste(parts, collapse = "")
  }
  
  if (length(dim_names) > 0) {
    specs <- specs %>%
      mutate(dim_suffix = pmap_chr(select(., all_of(dim_names)), build_suffix))
  } else {
    specs$dim_suffix <- ""
  }
  
  specs %>%
    mutate(
      title     = outcome_label,
      subtitle  = paste0(pop_label, dim_suffix),
      nice_name = paste0(title, sep_subtitle, subtitle),
      spec_id   = pmap_chr(
        select(., outcome_id, pop_id, all_of(dim_names)),
        ~ paste(c(...), collapse = "_")
      )
    )
}

# ----------------------------
# Core: cell adequacy filtering for time/group (optional)
# ----------------------------

filter_design_cells <- function(design,
                                time_var,
                                group_var = NULL,
                                weight_var = NULL,
                                min_cell_n = NULL,
                                min_cell_neff = NULL,
                                min_cell_unique = NULL,
                                unique_var = NULL,
                                subset_expr = NULL,
                                verbose = TRUE) {
  stopifnot(inherits(design, c("survey.design", "svyrep.design")))
  if (!time_var %in% names(design$variables)) stop("time_var not found.")
  if (!is.null(group_var) && !group_var %in% names(design$variables)) stop("group_var not found.")
  if (!is.null(weight_var) && !weight_var %in% names(design$variables)) stop("weight_var not found.")
  if (!is.null(min_cell_unique) && (is.null(unique_var) || !unique_var %in% names(design$variables))) {
    stop("If min_cell_unique is set, unique_var must be a variable name in design$variables.")
  }
  
  des <- design
  if (!is.null(subset_expr)) {
    des <- subset(des, eval(subset_expr, des$variables, parent.frame()))
  }
  
  df <- des$variables %>% mutate(.row_id__ = row_number())
  grp_vars <- c(time_var, group_var)
  grp_vars <- grp_vars[!is.na(grp_vars)]
  
  # build cell table
  cell <- df %>%
    group_by(across(all_of(grp_vars))) %>%
    summarise(
      n_cell = n(),
      neff = if (!is.null(weight_var)) neff_kish(.data[[weight_var]]) else NA_real_,
      n_unique = if (!is.null(min_cell_unique)) n_distinct(.data[[unique_var]], na.rm = TRUE) else NA_integer_,
      .groups = "drop"
    ) %>%
    mutate(
      ok = TRUE,
      ok = if (!is.null(min_cell_n))    ok & (n_cell >= min_cell_n) else ok,
      ok = if (!is.null(min_cell_neff) && !is.null(weight_var)) ok & (neff >= min_cell_neff) else ok,
      ok = if (!is.null(min_cell_unique)) ok & (n_unique >= min_cell_unique) else ok
    )
  
  keep_cells <- cell %>% filter(ok) %>% select(all_of(grp_vars))
  if (!nrow(keep_cells)) {
    if (verbose) message("No (time×group) cells meet minimum requirements.")
    return(NULL)
  }
  
  df_keep <- df %>% semi_join(keep_cells, by = grp_vars)
  keep_ids <- df_keep$.row_id__
  
  subset(des, df$.row_id__ %in% keep_ids)
}

# ----------------------------
# Compute functions (return a tidy long df)
# Convention: output columns include:
# - time (character/factor/numeric)
# - estimate
# - se (when available)
# - optional group / measure columns
# ----------------------------

# 1) Mean (numeric outcome), optionally by group
compute_mean <- function(design,
                         outcome,
                         time_var,
                         group_var = NULL,
                         subset_expr = NULL,
                         min_cell_n = NULL,
                         min_cell_neff = NULL,
                         weight_var_for_neff = NULL,
                         na.rm = TRUE,
                         verbose = TRUE) {
  stopifnot(outcome %in% names(design$variables))
  des <- filter_design_cells(
    design,
    time_var = time_var,
    group_var = group_var,
    weight_var = weight_var_for_neff,
    min_cell_n = min_cell_n,
    min_cell_neff = min_cell_neff,
    subset_expr = subset_expr,
    verbose = verbose
  )
  if (is.null(des)) return(tibble())
  
  f_by <- if (is.null(group_var)) reformulate(time_var) else as.formula(paste0("~", time_var, "+", group_var))
  
  res <- survey::svyby(
    reformulate(outcome),
    by = f_by,
    design = des,
    FUN = survey::svymean,
    na.rm = na.rm,
    vartype = "se",
    keep.var = TRUE
  ) %>% as_tibble()
  
  # Standardize columns
  if (is.null(group_var)) {
    res %>%
      transmute(
        time = .data[[time_var]],
        estimate = .data[[outcome]],
        se = .data[[paste0(outcome, "se")]],
        n = NA_integer_,
        neff = NA_real_
      )
  } else {
    res %>%
      transmute(
        time = .data[[time_var]],
        group = .data[[group_var]],
        estimate = .data[[outcome]],
        se = .data[[paste0(outcome, "se")]],
        n = NA_integer_,
        neff = NA_real_
      )
  }
}

# 2) Proportion for a binary indicator variable (0/1), optionally by group
compute_prop_indicator <- function(design,
                                   indicator,
                                   time_var,
                                   group_var = NULL,
                                   subset_expr = NULL,
                                   min_cell_n = NULL,
                                   min_cell_neff = NULL,
                                   weight_var_for_neff = NULL,
                                   na.rm = TRUE,
                                   verbose = TRUE) {
  stopifnot(indicator %in% names(design$variables))
  compute_mean(
    design = design,
    outcome = indicator,
    time_var = time_var,
    group_var = group_var,
    subset_expr = subset_expr,
    min_cell_n = min_cell_n,
    min_cell_neff = min_cell_neff,
    weight_var_for_neff = weight_var_for_neff,
    na.rm = na.rm,
    verbose = verbose
  )
}

# 3) Composition of a factor variable (shares of each level), with SE, optionally by group
# Output: measure = level of factor_var; estimate = share
compute_prop_factor_se <- function(design,
                                   factor_var,
                                   time_var,
                                   group_var = NULL,
                                   subset_expr = NULL,
                                   min_cell_n = NULL,
                                   min_cell_neff = NULL,
                                   weight_var_for_neff = NULL,
                                   na.rm = TRUE,
                                   verbose = TRUE) {
  
  stopifnot(factor_var %in% names(design$variables))
  des <- filter_design_cells(
    design,
    time_var = time_var,
    group_var = group_var,
    weight_var = weight_var_for_neff,
    min_cell_n = min_cell_n,
    min_cell_neff = min_cell_neff,
    subset_expr = subset_expr,
    verbose = verbose
  )
  if (is.null(des)) return(tibble())
  
  # Ensure factor
  des$variables[[factor_var]] <- as.factor(des$variables[[factor_var]])
  
  # svymean(~factor) returns shares for each level
  f_y  <- reformulate(factor_var)
  f_by <- if (is.null(group_var)) reformulate(time_var) else as.formula(paste0("~", time_var, "+", group_var))
  
  q <- survey::svyby(
    formula = f_y,
    by = f_by,
    design = des,
    FUN = survey::svymean,
    na.rm = na.rm,
    vartype = "se",
    keep.var = TRUE
  ) %>% as_tibble()
  
  id_cols <- c(time_var, group_var)
  id_cols <- id_cols[!is.na(id_cols)]
  
  # Columns come out like factor_varLevelName and factor_varLevelNamese
  # We'll pivot and parse
  long <- q %>%
    pivot_longer(
      cols = -all_of(id_cols),
      names_to = "name",
      values_to = "value"
    ) %>%
    mutate(
      is_se = str_ends(name, "se$"),
      base  = str_remove(name, "se$"),
      # base is like factor_varLevel; remove factor_var prefix to get level
      measure = str_remove(base, paste0("^", factor_var)),
      measure = str_replace_all(measure, "^\\.", ""),  # sometimes levels come with leading dot
      measure = ifelse(measure == "", base, measure)
    )
  
  est <- long %>% filter(!is_se) %>% rename(estimate = value) %>% select(all_of(id_cols), measure, estimate)
  se  <- long %>% filter(is_se)  %>% rename(se = value)       %>% select(all_of(id_cols), measure, se)
  
  out <- est %>% left_join(se, by = c(setNames(id_cols, id_cols), "measure"))
  
  if (is.null(group_var)) {
    out %>% transmute(time = .data[[time_var]], measure, estimate, se)
  } else {
    out %>% transmute(time = .data[[time_var]], group = .data[[group_var]], measure, estimate, se)
  }
}

# 4) Quantiles over time, optionally by group
# Robust fallback loop; can attempt svyby first (often fails for quantiles in small/sparse cells)
compute_quantiles <- function(design,
                              outcome,
                              time_var,
                              group_var = NULL,
                              probs = c(.10, .25, .50, .75, .90),
                              subset_expr = NULL,
                              min_cell_n = NULL,
                              min_cell_neff = NULL,
                              min_cell_unique = NULL,
                              weight_var_for_neff = NULL,
                              na.rm = TRUE,
                              try_svyby = TRUE,
                              verbose = TRUE) {
  
  stopifnot(outcome %in% names(design$variables))
  probs <- sort(unique(as.numeric(probs)))
  if (any(!is.finite(probs)) || any(probs <= 0 | probs >= 1)) stop("probs must be in (0,1).")
  
  des <- filter_design_cells(
    design,
    time_var = time_var,
    group_var = group_var,
    weight_var = weight_var_for_neff,
    min_cell_n = min_cell_n,
    min_cell_neff = min_cell_neff,
    min_cell_unique = min_cell_unique,
    unique_var = outcome,
    subset_expr = subset_expr,
    verbose = verbose
  )
  if (is.null(des)) return(tibble())
  
  # fast path (svyby + svyquantile) can be very fast but brittle
  if (isTRUE(try_svyby)) {
    f_by <- if (is.null(group_var)) reformulate(time_var) else as.formula(paste0("~", time_var, "+", group_var))
    q_by <- tryCatch(
      survey::svyby(
        formula = reformulate(outcome),
        by = f_by,
        design = des,
        FUN = survey::svyquantile,
        quantiles = probs,
        ci = FALSE,
        na.rm = na.rm
      ),
      error = function(e) e
    )
    if (!inherits(q_by, "error")) {
      q_df <- as_tibble(q_by)
      id_cols <- c(time_var, group_var); id_cols <- id_cols[!is.na(id_cols)]
      # columns look like outcome0.1, outcome0.25, ...
      out <- q_df %>%
        pivot_longer(cols = -all_of(id_cols), names_to = "name", values_to = "estimate") %>%
        mutate(
          percentile = as.numeric(str_extract(name, "(\\d*\\.?\\d+)$")),
          measure = paste0("p", round(100 * percentile))
        ) %>%
        select(all_of(id_cols), measure, percentile, estimate)
      
      if (is.null(group_var)) {
        return(out %>% transmute(time = .data[[time_var]], measure, percentile, estimate, se = NA_real_))
      } else {
        return(out %>% transmute(time = .data[[time_var]], group = .data[[group_var]], measure, percentile, estimate, se = NA_real_))
      }
    } else if (verbose) {
      message("compute_quantiles: svyby failed; using fallback loop. (", q_by$message, ")")
    }
  }
  
  # fallback loop: reliable
  times <- sort(unique(des$variables[[time_var]]))
  groups <- if (is.null(group_var)) NA else sort(unique(des$variables[[group_var]]))
  groups <- groups[!is.na(groups)]
  
  out <- list(); k <- 0L
  for (g in groups) {
    for (t in times) {
      des_sub <- if (is.null(group_var)) {
        subset(des, get(time_var) == t)
      } else {
        subset(des, get(time_var) == t & get(group_var) == g)
      }
      if (!nrow(des_sub$variables)) next
      
      q <- tryCatch(
        survey::svyquantile(reformulate(outcome), des_sub, quantiles = probs, se = FALSE, na.rm = na.rm),
        error = function(e) NULL
      )
      if (is.null(q)) next
      
      est <- as.numeric(unlist(q))[seq_along(probs)]
      k <- k + 1L
      out[[k]] <- tibble(
        time = t,
        group = if (!is.null(group_var)) g else NULL,
        percentile = probs,
        measure = paste0("p", round(100 * probs)),
        estimate = est,
        se = NA_real_
      )
    }
  }
  bind_rows(out)
}

# 5) Gini over time (requires convey_prep(design) beforehand)
compute_gini <- function(design,
                         outcome,
                         time_var,
                         group_var = NULL,
                         subset_expr = NULL,
                         min_cell_n = NULL,
                         min_cell_neff = NULL,
                         weight_var_for_neff = NULL,
                         na.rm = TRUE,
                         verbose = TRUE) {
  
  if (!requireNamespace("convey", quietly = TRUE)) {
    stop("Package 'convey' is required for compute_gini(). Install and run convey_prep(design).")
  }
  stopifnot(outcome %in% names(design$variables))
  
  des <- filter_design_cells(
    design,
    time_var = time_var,
    group_var = group_var,
    weight_var = weight_var_for_neff,
    min_cell_n = min_cell_n,
    min_cell_neff = min_cell_neff,
    subset_expr = subset_expr,
    verbose = verbose
  )
  if (is.null(des)) return(tibble())
  
  f_by <- if (is.null(group_var)) reformulate(time_var) else as.formula(paste0("~", time_var, "+", group_var))
  
  g <- survey::svyby(
    reformulate(outcome),
    by = f_by,
    design = des,
    FUN = convey::svygini,
    na.rm = na.rm,
    vartype = "se",
    keep.var = TRUE
  ) %>% as_tibble()
  
  # column names: outcome and outcomese
  if (is.null(group_var)) {
    g %>% transmute(time = .data[[time_var]], estimate = .data[[outcome]], se = .data[[paste0(outcome, "se")]])
  } else {
    g %>% transmute(time = .data[[time_var]], group = .data[[group_var]], estimate = .data[[outcome]], se = .data[[paste0(outcome, "se")]])
  }
}

# 6) Palma ratio over time (Top 10% income share / Bottom 40% income share)
# Robust fallback loop because Palma requires within-cell quantiles and totals.
compute_palma <- function(design,
                          outcome,
                          time_var,
                          group_var = NULL,
                          subset_expr = NULL,
                          min_cell_n = NULL,
                          min_cell_neff = NULL,
                          min_cell_unique = NULL,
                          weight_var_for_neff = NULL,
                          na.rm = TRUE,
                          verbose = TRUE) {
  stopifnot(outcome %in% names(design$variables))
  
  des <- filter_design_cells(
    design,
    time_var = time_var,
    group_var = group_var,
    weight_var = weight_var_for_neff,
    min_cell_n = min_cell_n,
    min_cell_neff = min_cell_neff,
    min_cell_unique = min_cell_unique,
    unique_var = outcome,
    subset_expr = subset_expr,
    verbose = verbose
  )
  if (is.null(des)) return(tibble())
  
  times <- sort(unique(des$variables[[time_var]]))
  groups <- if (is.null(group_var)) NA else sort(unique(des$variables[[group_var]]))
  groups <- groups[!is.na(groups)]
  
  out <- list(); k <- 0L
  
  for (g in groups) {
    for (t in times) {
      des_sub <- if (is.null(group_var)) {
        subset(des, get(time_var) == t)
      } else {
        subset(des, get(time_var) == t & get(group_var) == g)
      }
      if (!nrow(des_sub$variables)) next
      
      # quantile cutpoints in this cell
      qs <- tryCatch(
        survey::svyquantile(reformulate(outcome), des_sub, quantiles = c(0.4, 0.9), se = FALSE, na.rm = na.rm),
        error = function(e) NULL
      )
      if (is.null(qs)) next
      qs_vec <- as.numeric(unlist(qs))[1:2]
      q40 <- qs_vec[1]; q90 <- qs_vec[2]
      if (!is.finite(q40) || !is.finite(q90)) next
      
      # total income in cell
      tot_all <- tryCatch(as.numeric(survey::svytotal(reformulate(outcome), des_sub, na.rm = na.rm)), error = function(e) NA_real_)
      if (!is.finite(tot_all) || tot_all == 0) next
      
      # income share top10: y * I(y > q90)
      des_sub$variables$.top10 <- as.numeric(des_sub$variables[[outcome]] > q90)
      des_sub$variables$.bot40 <- as.numeric(des_sub$variables[[outcome]] <= q40)
      
      tot_top <- tryCatch(as.numeric(survey::svytotal(as.formula(paste0("~I(", outcome, " * .top10)")), des_sub, na.rm = na.rm)), error = function(e) NA_real_)
      tot_bot <- tryCatch(as.numeric(survey::svytotal(as.formula(paste0("~I(", outcome, " * .bot40)")), des_sub, na.rm = na.rm)), error = function(e) NA_real_)
      if (!is.finite(tot_top) || !is.finite(tot_bot) || tot_bot == 0) next
      
      share_top <- tot_top / tot_all
      share_bot <- tot_bot / tot_all
      palma <- share_top / share_bot
      
      k <- k + 1L
      out[[k]] <- tibble(
        time = t,
        group = if (!is.null(group_var)) g else NULL,
        estimate = palma,
        se = NA_real_
      )
    }
  }
  
  bind_rows(out)
}

# ----------------------------
# Runner: apply compute fn to each spec row and attach spec metadata
# ----------------------------

run_specs <- function(specs,
                      design,
                      compute_fn,
                      time_var,
                      group_var = NULL,
                      outcome_col = "outcome_var",
                      subset_col = "subset_expr",
                      attach_cols = c("spec_id", "title", "subtitle", "nice_name",
                                      "outcome_id", "outcome_var", "pop_id", "pop_label"),
                      ...){
  
  stopifnot(is.data.frame(specs))
  stopifnot(outcome_col %in% names(specs))
  stopifnot(subset_col %in% names(specs))
  
  specs %>%
    mutate(.row = row_number()) %>%
    mutate(
      .res = pmap(
        list(.data[[outcome_col]], .data[[subset_col]], .data[[".row"]]),
        function(outcome, subset_expr, .row) {
          df <- compute_fn(
            design = design,
            outcome = outcome,
            time_var = time_var,
            group_var = group_var,
            subset_expr = subset_expr,
            ...
          )
          if (!nrow(df)) return(df)
          # Attach spec fields
          meta <- specs %>% filter(.row == !!.row) %>% select(any_of(attach_cols))
          if (!nrow(meta)) return(df)
          df %>% mutate(across(everything(), ~ .)) %>%
            bind_cols(meta[rep(1, nrow(df)), , drop = FALSE])
        }
      )
    ) %>%
    select(-.row) %>%
    unnest(.res)
}

# ----------------------------
# Plotting
# ----------------------------

plot_ts_single <- function(df,
                           time_col = "time",
                           y_col = "estimate",
                           se_col = "se",
                           title = NULL,
                           subtitle = NULL,
                           y_label = NULL,
                           level = 0.95,
                           show_ci = TRUE,
                           x_breaks = NULL,
                           x_labels = NULL,
                           title_width = 60,
                           subtitle_width = 80) {
  
  df <- df %>%
    filter(!is.na(.data[[y_col]])) %>%
    add_time_fields(time_col)
  
  if (nrow(df) == 0) {
    return(ggplot() + theme_void() + labs(title = title %||% "No data"))
  }
  
  has_se <- !is.null(se_col) && se_col %in% names(df) && any(!is.na(df[[se_col]]))
  z <- stats::qnorm(1 - (1 - level) / 2)
  
  if (show_ci && has_se) {
    df <- df %>%
      mutate(
        lo = .data[[y_col]] - z * .data[[se_col]],
        hi = .data[[y_col]] + z * .data[[se_col]]
      )
  }
  
  if (is.null(title)) title <- "Time series"
  if (is.null(y_label)) y_label <- y_col
  
  if (is.null(x_breaks)) x_breaks <- sort(unique(df$year))
  if (is.null(x_labels)) x_labels <- as.character(x_breaks)
  if (length(x_labels) != length(x_breaks)) stop("x_labels must match x_breaks length.")
  
  p <- ggplot(df, aes(x = time_num, y = .data[[y_col]]))
  
  if (show_ci && has_se) {
    p <- p + geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20)
  }
  
  p +
    geom_line(linewidth = 0.7, alpha = 0.9) +
    geom_point(size = 1.8, alpha = 0.9) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    labs(
      x = "Year",
      y = y_label,
      title = str_wrap(title, width = title_width),
      subtitle = str_wrap(subtitle %||% "", width = subtitle_width)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 11),
      legend.position = "bottom"
    )
}

plot_ts_multi <- function(df,
                          time_col = "time",
                          y_col = "estimate",
                          se_col = "se",
                          series_col = "series",     # e.g. "group" or "measure" or "spec_id"
                          series_labels = NULL,      # named vector mapping raw -> pretty
                          title = NULL,
                          subtitle = NULL,
                          y_label = NULL,
                          level = 0.95,
                          show_ci = FALSE,
                          facet_cols = NULL,         # c("Region4") or c("Sex","Region4")
                          facet_scales = "fixed",
                          x_breaks = NULL,
                          x_labels = NULL,
                          title_width = 60,
                          subtitle_width = 80) {
  
  stopifnot(time_col %in% names(df), y_col %in% names(df))
  if (!series_col %in% names(df)) stop("series_col '", series_col, "' not found in df.")
  
  df <- df %>%
    filter(!is.na(.data[[y_col]]), !is.na(.data[[series_col]])) %>%
    add_time_fields(time_col) %>%
    mutate(.series_raw = as.character(.data[[series_col]]))
  
  if (!is.null(series_labels)) {
    present <- unique(df$.series_raw)
    keep <- intersect(names(series_labels), present)
    lab_map <- series_labels[keep]
    df <- df %>% mutate(series = factor(.series_raw, levels = names(lab_map), labels = unname(lab_map)))
  } else {
    df <- df %>% mutate(series = factor(.series_raw))
  }
  
  if (nrow(df) == 0) {
    return(ggplot() + theme_void() + labs(title = title %||% "No data"))
  }
  
  has_se <- !is.null(se_col) && se_col %in% names(df) && any(!is.na(df[[se_col]]))
  z <- stats::qnorm(1 - (1 - level) / 2)
  
  if (show_ci && has_se) {
    df <- df %>%
      mutate(
        lo = .data[[y_col]] - z * .data[[se_col]],
        hi = .data[[y_col]] + z * .data[[se_col]]
      )
  }
  
  if (is.null(title)) title <- "Time series"
  if (is.null(y_label)) y_label <- y_col
  if (is.null(x_breaks)) x_breaks <- sort(unique(df$year))
  if (is.null(x_labels)) x_labels <- as.character(x_breaks)
  if (length(x_labels) != length(x_breaks)) stop("x_labels must match x_breaks length.")
  
  p <- ggplot(df, aes(x = time_num, y = .data[[y_col]], color = series, group = series))
  
  if (show_ci && has_se) {
    p <- p + geom_ribbon(aes(ymin = lo, ymax = hi, fill = series), alpha = 0.18, color = NA, show.legend = FALSE)
  }
  
  p <- p +
    geom_line(linewidth = 0.65, alpha = 0.9) +
    geom_point(size = 1.5, alpha = 0.9) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    labs(
      x = "Year",
      y = y_label,
      title = str_wrap(title, width = title_width),
      subtitle = str_wrap(subtitle %||% "", width = subtitle_width),
      color = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 11),
      legend.position = "bottom"
    )
  
  if (!is.null(facet_cols)) {
    missing_f <- setdiff(facet_cols, names(df))
    if (length(missing_f)) stop("facet_cols not found in df: ", paste(missing_f, collapse = ", "))
    
    if (length(facet_cols) == 1) {
      p <- p + facet_wrap(as.formula(paste0("~", facet_cols[[1]])), scales = facet_scales)
    } else if (length(facet_cols) == 2) {
      p <- p + facet_grid(as.formula(paste(facet_cols[[1]], "~", facet_cols[[2]])), scales = facet_scales)
    } else {
      stop("facet_cols supports length 1 or 2 for now.")
    }
  }
  
  p
}

# ======================================================================
# QUICK SMOKE TEST (example usage)
# ======================================================================
# After you source() this file:
#
# 1) Build a spec table:
# populations <- list(
#   employed_all = quote(OCUPADO == 1),
#   employed_women = quote(OCUPADO == 1 & Sex == "Female")
# )
# population_labels <- c(employed_all="Employed – All", employed_women="Employed – Women")
#
# specs <- make_stat_specs_v2(
#   outcomes = c(total="real_total_income_total"),
#   outcome_labels = c(total="Real total labor income"),
#   base_filters = list(total=quote(real_total_income_total > 0)),
#   populations = populations,
#   population_labels = population_labels
# )
#
# 2) Run a compute across specs (means):
# df_mean <- run_specs(
#   specs = specs,
#   design = design_indiv_q,
#   compute_fn = compute_mean,
#   time_var = "year_quarter",
#   weight_var_for_neff = "FACTOR_EXPANSION",
#   min_cell_neff = 50
# )
#
# 3) Plot:
# p <- df_mean %>% filter(spec_id == first(spec_id))
# plot_ts_single(p, title = p$title[1], subtitle = p$subtitle[1], y_label="DOP (real)", show_ci=TRUE)
#
# 4) Factor composition example (formal/informal shares among employed women):
# df_shares <- compute_prop_factor_se(
#   design = design_indiv_q,
#   factor_var = "Employment_Status",
#   time_var = "year_quarter",
#   subset_expr = quote(OCUPADO == 1 & Sex == "Female"),
#   weight_var_for_neff = "FACTOR_EXPANSION",
#   min_cell_neff = 50
# )
# plot_ts_multi(df_shares, series_col="measure", title="Employment status shares", subtitle="Employed – Women",
#              y_label="Share", show_ci=TRUE)
# ======================================================================
