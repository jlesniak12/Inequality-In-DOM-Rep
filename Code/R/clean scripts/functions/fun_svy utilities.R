
#===============================================================================
# SAMPLE ACCESSORS
#
# `samples` element structure has changed; resolve the microdata frame through
# one accessor so a future change is a one-function edit rather than a
# find-and-replace. Fails loudly with the actual available names instead of
# handing NULL to dplyr (which throws an opaque "no applicable method for
# 'filter' applied to an object of class NULL" several lines later).
#===============================================================================

samp_df <- function(id) {
  s <- samples[[id]]
  if (is.null(s)) {
    stop("Sample '", id, "' not found. Available: ",
         paste(names(samples), collapse = ", "), call. = FALSE)
  }
  if (!is.null(s$data))               return(s$data)
  if (!is.null(s$design$variables))   return(s$design$variables)
  stop("Sample '", id, "' has neither $data nor $design$variables. Slots: ",
       paste(names(s), collapse = ", "), call. = FALSE)
}

samp_design <- function(id) {
  s <- samples[[id]]
  if (is.null(s) || is.null(s$design)) {
    stop("Design for sample '", id, "' not found. Available: ",
         paste(names(samples), collapse = ", "), call. = FALSE)
  }
  s$design
}


#===============================================================================
# HELPERS — tidy wrappers around the survey package
#
# Every helper returns the same schema so plot scripts can treat all objects
# uniformly:
#   year_quarter chr | <group_var> chr | estimate dbl | se dbl | n_obs int |
#   sparse lgl
#
# min_n: minimum unweighted cell count to flag as non-sparse. Defaults to
# config$figures$min_cell_n (resolved at call time, not source time, so the
# function works even if config changes between sourcing and calling).
#===============================================================================

.n_by <- function(design, time_var, group_var = NULL) {
  grp <- c(time_var, group_var)
  design$variables %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(n_obs = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
}

.standardise_se <- function(df, keys) {
  leftover <- setdiff(names(df), c(keys, "estimate"))
  se_col <- NULL
  if ("se" %in% leftover) {
    se_col <- "se"
  } else {
    cand <- leftover[grepl("(^|\\.)se(\\.|$)|se$", leftover, ignore.case = TRUE)]
    cand <- setdiff(cand, c("ci_l", "ci_u", "ci.l", "ci.u"))
    if (length(cand)) se_col <- cand[[1]]
  }
  if (is.null(se_col)) df$se <- NA_real_ else names(df)[names(df) == se_col] <- "se"
  df[, c(keys, "estimate", "se"), drop = FALSE]
}

svy_mean_by <- function(design, var, time_var, group_var = NULL,
                        na_rm = TRUE, min_n = config$figures$min_cell_n) {
  if (isTRUE(na_rm)) design <- design[!is.na(design$variables[[var]]), ]
  
  grp <- c(time_var, group_var)
  est <- svyby(stats::as.formula(paste0("~", var)),
               stats::as.formula(paste0("~", paste(grp, collapse = "+"))),
               design, svymean, na.rm = na_rm, vartype = "se",
               keep.names = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(estimate = !!var) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
  est <- .standardise_se(est, keys = grp)
  
  est %>%
    dplyr::left_join(.n_by(design, time_var, group_var), by = grp) %>%
    dplyr::rename(year_quarter = !!time_var) %>%
    dplyr::mutate(sparse = n_obs < min_n)
}

svy_quantile_by <- function(design, var, time_var, group_var = NULL,
                            prob = 0.5, na_rm = TRUE,
                            min_n = config$figures$min_cell_n) {
  if (isTRUE(na_rm)) design <- design[!is.na(design$variables[[var]]), ]
  
  grp <- c(time_var, group_var)
  est <- svyby(stats::as.formula(paste0("~", var)),
               stats::as.formula(paste0("~", paste(grp, collapse = "+"))),
               design, FUN = svyquantile, quantiles = prob, ci = TRUE,
               vartype = "se", keep.names = FALSE, na.rm = na_rm) %>%
    tibble::as_tibble()
  
  # svyby(svyquantile) names the estimate column `var` or `var.0.5` depending
  # on the survey version. Find it as the non-key, non-SE/CI column.
  non_key <- setdiff(names(est), grp)
  se_ci   <- non_key[grepl("se|ci", non_key, ignore.case = TRUE)]
  est_col <- setdiff(non_key, se_ci)
  if (length(est_col) != 1) est_col <- if (var %in% non_key) var else est_col[[1]]
  names(est)[names(est) == est_col] <- "estimate"
  
  est <- est %>% dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
  est <- .standardise_se(est, keys = grp)
  
  est %>%
    dplyr::left_join(.n_by(design, time_var, group_var), by = grp) %>%
    dplyr::rename(year_quarter = !!time_var) %>%
    dplyr::mutate(sparse = n_obs < min_n)
}