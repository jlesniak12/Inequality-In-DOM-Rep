compute_quantiles_up <- function(design,
                              var,
                              time_var = NULL,
                              group_var = NULL,
                              probs = c(0.1, 0.5, 0.9),
                              subset_expr = NULL,
                              na.rm = TRUE,
                              method = "default",
                              return_se = FALSE,
                              return_ci = FALSE,
                              level = 0.95,
                              require_complete = FALSE,
                              quantile_base = "p",
                              measure_prefix = NULL,
                              fast = TRUE,
                              verbose = FALSE) {
  
  if (!inherits(design, "survey.design")) {
    stop("`design` must be a survey.design object.", call. = FALSE)
  }
  
  if (is.null(time_var) || !nzchar(time_var)) {
    stop("`time_var` must be provided.", call. = FALSE)
  }
  
  probs <- .validate_probs(probs)
  
  # ---- allow multiple vars (vector or named vector) ----
  if (length(var) > 1) {
    var_labels <- names(var)
    vars <- unname(var)
    
    out_list <- lapply(seq_along(vars), function(i) {
      v <- vars[[i]]
      v <- as.character(v)[1]     # force length-1 character scalar
      lbl <- if (!is.null(var_labels) && nzchar(var_labels[[i]])) var_labels[[i]] else NA_character_
      
      res <- compute_quantiles_up(
        design           = design,
        var              = v,     # MUST be scalar
        time_var         = time_var,
        group_var        = group_var,
        probs            = probs,
        subset_expr      = subset_expr,
        na.rm            = na.rm,
        method           = method,
        return_se        = return_se,
        return_ci        = return_ci,
        level            = level,
        require_complete = require_complete,
        quantile_base    = quantile_base,
        measure_prefix   = measure_prefix,
        fast             = fast,
        verbose          = verbose
      )
      
      # add columns AFTER compute
      res$variable <- v
      res$variable_label <- lbl
      res
    })
    
    return(dplyr::bind_rows(out_list))
  }
  
  
  design <- .coerce_byvars_for_svyby(design, time_var = time_var, group_var = group_var)
  
  
  # Standardized subset behavior
  design2 <- .apply_subset(design, subset_expr)
  
  # required columns
  .require_cols_df(design2$variables, c(var, time_var, if (!is.null(group_var)) group_var))
  
  # Optional NA removal of target variable only
  if (isTRUE(na.rm)) {
    v <- design2$variables[[var]]
    design2 <- subset(design2, !is.na(v))
  }
  
  # ---- Fast path (svyby) ----
  if (isTRUE(fast)) {
    out_fast <- try(
      .compute_quantiles_svyby(
        design = design2,
        var = var,
        time_var = time_var,
        group_var = group_var,
        probs = probs,
        na.rm = na.rm,
        method = method,
        return_se = return_se,
        return_ci = return_ci,
        level = level,
        quantile_base = quantile_base,
        measure_prefix = measure_prefix,
        require_complete = require_complete
      ),
      silent = TRUE
    )
    
    if (!inherits(out_fast, "try-error") &&
        !is.null(out_fast) &&
        nrow(out_fast) > 0) {
      return(out_fast)
    }
  }
  
  # ---- Fallback loop (estimates-only; schema-stable) ----
  .compute_quantiles_loop(
    design = design2,
    var = var,
    time_var = time_var,
    group_var = group_var,
    probs = probs,
    na.rm = na.rm,
    method = method,
    return_se = return_se,
    return_ci = return_ci,
    level = level,
    quantile_base = quantile_base,
    measure_prefix = measure_prefix,
    verbose = verbose
  )
}



.compute_quantiles_loop <- function(design,
                                    var,
                                    time_var,
                                    group_var = NULL,
                                    probs = c(0.1, 0.5, 0.9),
                                    na.rm = TRUE,
                                    method = "default",
                                    return_se = FALSE,
                                    return_ci = FALSE,
                                    level = 0.95,
                                    quantile_base = "p",
                                    measure_prefix = NULL,
                                    verbose = FALSE) {
  
  dat <- design$variables
  # time_var always required; group_var optional
  .require_cols_df(dat, c(var, time_var, if (!is.null(group_var)) group_var))
  
  # Determine time levels present in the (already subset) design
  time_vals <- dat[[time_var]]
  time_levels <- sort(unique(time_vals[!is.na(time_vals)]))
  
  # Determine group levels if provided
  if (!is.null(group_var)) {
    g_vals <- dat[[group_var]]
    group_levels <- sort(unique(g_vals[!is.na(g_vals)]))
  } else {
    group_levels <- NA_character_
  }
  
  # Helper: build measure name from prob
  measures <- .prob_to_measure(probs, base = quantile_base)
  if (!is.null(measure_prefix) && nzchar(measure_prefix)) {
    measures <- paste0(measure_prefix, "_", measures)
  }
  
  
  # Initialize output rows collector
  rows <- vector("list", length = 0L)
  
  warned <- FALSE
  
  for (tt in time_levels) {
    if (is.null(group_var)) {
      
      # Subset design to time cell
      d_cell <- try(subset(design, design$variables[[time_var]] == tt), silent = TRUE)
      
      # Compute quantiles safely
      est <- rep(NA_real_, length(probs))
      if (!inherits(d_cell, "try-error")) {
        qtry <- try(
          survey::svyquantile(
            stats::as.formula(paste0("~", var)),
            design = d_cell,
            quantiles = probs,
            na.rm = na.rm,
            method = method,
            se = FALSE
          ),
          silent = TRUE
        )
        
        if (inherits(qtry, "try-error")) {
          if (isTRUE(verbose) && !warned) {
            message("compute_quantiles(): fallback loop encountered error; returning NA for that cell. First error: ",
                    conditionMessage(attr(qtry, "condition")))
            warned <- TRUE
          }
        } else {
          est <- .coef_svyquantile(qtry, length(probs))
        }
      }
      
      # Add one row per prob
      for (i in seq_along(probs)) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          time = tt,
          prob = probs[[i]],
          estimate = est[[i]],
          measure = measures[[i]]
        )
      }
      
    } else {
      
      for (gg in group_levels) {
        
        d_cell <- try(
          subset(design,
                 design$variables[[time_var]] == tt &
                   design$variables[[group_var]] == gg),
          silent = TRUE
        )
        
        est <- rep(NA_real_, length(probs))
        if (!inherits(d_cell, "try-error")) {
          qtry <- try(
            survey::svyquantile(
              stats::as.formula(paste0("~", var)),
              design = d_cell,
              quantiles = probs,
              na.rm = na.rm,
              method = method,
              se = FALSE
            ),
            silent = TRUE
          )
          
          if (inherits(qtry, "try-error")) {
            if (isTRUE(verbose) && !warned) {
              message("compute_quantiles(): fallback loop encountered error; returning NA for that cell. First error: ",
                      conditionMessage(attr(qtry, "condition")))
              warned <- TRUE
            }
          } else {
            est <- .coef_svyquantile(qtry, length(probs))
          }
        }
        
        for (i in seq_along(probs)) {
          rows[[length(rows) + 1L]] <- tibble::tibble(
            time = tt,
            group = as.character(gg),
            prob = probs[[i]],
            estimate = est[[i]],
            measure = measures[[i]]
          )
        }
      }
    }
  }
  
  out <- dplyr::bind_rows(rows)
  
  # Attach schema-stable variance columns (NA) if requested.
  # (Per your compute_quantiles() docs: loop path does not provide variance.) :contentReference[oaicite:2]{index=2}
  if (isTRUE(return_se) && !"se" %in% names(out)) out$se <- NA_real_
  if (isTRUE(return_ci)) {
    if (!"ci_l" %in% names(out)) out$ci_l <- NA_real_
    if (!"ci_u" %in% names(out)) out$ci_u <- NA_real_
  }
  
  out
}

# ---- internal helpers ---------------------------------------------------------

#' Convert probabilities to standardized measure labels (internal)
#'
#' Examples:
#'   0.10 -> "p10"
#'   0.50 -> "p50"
#'   0.90 -> "p90"
#'
#' @param probs Numeric vector of quantile probabilities in the interval (0, 1).
#'   Each value corresponds to a quantile to be estimated (e.g., 0.1 = 10th percentile,
#'   0.5 = median). Order is preserved and used for labeling and output ordering.

#' @param base Prefix for measure labels. Default "p".
#'
#' @return Character vector same length as probs.
#' @keywords internal
.prob_to_measure <- function(probs, base = "p") {
  stopifnot(is.numeric(probs))
  # guard floating point (e.g., 0.1 * 100 = 9.999999)
  pct <- as.integer(round(probs * 100))
  paste0(base, pct)
}

.validate_probs <- function(probs) {
  if (!is.numeric(probs) || length(probs) == 0L || anyNA(probs)) {
    stop("`probs` must be a non-empty numeric vector with no NA.", call. = FALSE)
  }
  if (any(!is.finite(probs))) {
    stop("`probs` must be finite.", call. = FALSE)
  }
  if (any(probs <= 0 | probs >= 1)) {
    stop("`probs` must be strictly between 0 and 1.", call. = FALSE)
  }
  sort(unique(as.numeric(probs)))
}



#' Extract quantile estimates from svyquantile output (internal)
#'
#' survey::svyquantile() can return different shapes (numeric, matrix, svystat, list)
#' depending on options and survey version. This helper safely returns a numeric
#' vector of length k (or NA_real_ if extraction fails).
#'
#' @param q svyquantile result
#' @param k expected length
#' @keywords internal
.coef_svyquantile <- function(q, k) {
  if (is.null(q)) return(rep(NA_real_, k))
  
  # already numeric
  if (is.numeric(q) && length(q) == k) return(as.numeric(q))
  
  # matrix/data.frame
  if ((is.matrix(q) || is.data.frame(q))) {
    v <- suppressWarnings(as.numeric(q))
    if (length(v) == k) return(v)
  }
  
  # typical survey objects: coef() works
  v <- tryCatch(suppressWarnings(as.numeric(stats::coef(q))),
                error = function(e) NULL)
  if (!is.null(v) && length(v) == k) return(v)
  
  # list-ish fallback (varies by version)
  if (is.list(q)) {
    if (!is.null(q$quantiles)) {
      v <- tryCatch(suppressWarnings(as.numeric(q$quantiles)),
                    error = function(e) NULL)
      if (!is.null(v) && length(v) == k) return(v)
    }
    if (!is.null(q$coef)) {
      v <- tryCatch(suppressWarnings(as.numeric(q$coef)),
                    error = function(e) NULL)
      if (!is.null(v) && length(v) == k) return(v)
    }
  }
  
  rep(NA_real_, k)
}



#' Fast quantiles via svyby(svyquantile), returned as long/tidy (internal)
#'
#' Uses survey::svyby() with FUN=svyquantile() to compute quantiles by time
#' (and optionally group). Then converts the result to long/tidy:
#'   time, (group), prob, estimate, measure, (se), (ci_l, ci_u)
#'
#' This path should be used when it succeeds; a loop fallback can be used
#' for sparse cells / failures.
#'
#' @param design survey.design object.
#' @param var Outcome variable name (string).
#' @param time_var Time variable name (string).
#' @param group_var Optional group variable name (string or NULL).
#' @param probs Numeric vector of quantiles.
#' @param na.rm Logical.
#' @param method Passed to svyquantile().
#' @param return_se Logical.
#' @param return_ci Logical.
#' @param level Confidence level for CI.
#' @param quantile_base Base label used by `.prob_to_measure()`, e.g. "p" -> p10.
#' @param measure_prefix Optional prefix for measure name, e.g. "income" -> income_p10.
#'
#' @return tibble long/tidy quantiles.
#' @keywords internal
.compute_quantiles_svyby <- function(design,
                                     var,
                                     time_var,
                                     group_var = NULL,
                                     probs = c(0.1, 0.5, 0.9),
                                     na.rm = TRUE,
                                     method = "default",
                                     return_se = FALSE,
                                     return_ci = FALSE,
                                     level = 0.95,
                                     quantile_base = "p",
                                     measure_prefix = NULL,
                                     require_complete = FALSE) {
  
  dat <- design$variables
  .require_cols_df(dat, c(var, time_var, if (!is.null(group_var)) group_var))
  
  f_y <- stats::as.formula(paste0("~", var))
  f_by <- if (is.null(group_var)) {
    stats::as.formula(paste0("~", time_var))
  } else {
    stats::as.formula(paste0("~", time_var, " + ", group_var))
  }
  
  need_var <- isTRUE(return_se) || isTRUE(return_ci)
  
  sb <- survey::svyby(
    formula   = f_y,
    by        = f_by,
    design    = design,
    FUN       = survey::svyquantile,
    quantiles = probs,
    na.rm     = na.rm,
    method    = method,
    keep.var  = need_var,
    ci        = isTRUE(return_ci),
    level     = level,
    covmat    = FALSE
  )
  
  by_df <- tibble::as_tibble(sb[, all.vars(f_by), drop = FALSE])
  n_by  <- nrow(by_df)
  

  # ---- estimates (reshape + validate orientation) ----
  
  byrow_used <- NA  # will become TRUE/FALSE if we reshape from a vector
  
  est_vec <- stats::coef(sb)
  
  if (is.vector(est_vec)) {
    if (length(est_vec) != n_by * length(probs)) {
      stop("Unexpected coef() length from svyby(svyquantile); cannot align quantiles reliably.", call. = FALSE)
    }
    
    est_byrow_true  <- matrix(est_vec, nrow = n_by, ncol = length(probs), byrow = TRUE)
    est_byrow_false <- matrix(est_vec, nrow = n_by, ncol = length(probs), byrow = FALSE)
    
    # validate using a by-row against direct svyquantile
    k <- min(5L, n_by)  # pick row 5 if possible
    time_k <- by_df[[time_var]][k]
    
    # IMPORTANT: if you have group_var, subset on both time and group for validation
    if (is.null(group_var)) {
      d_k <- subset(design, design$variables[[time_var]] == time_k)
    } else {
      group_k <- by_df[[group_var]][k]
      d_k <- subset(design,
                    design$variables[[time_var]] == time_k &
                      design$variables[[group_var]] == group_k)
    }
    
    q_k <- survey::svyquantile(
      f_y, d_k,
      quantiles = probs,
      na.rm = na.rm,
      method = method,
      se = FALSE,
      ci = FALSE
    )
    truth <- as.numeric(stats::coef(q_k))
    
    ok_true  <- isTRUE(all.equal(as.numeric(est_byrow_true[k, ]),  truth, tolerance = 1e-6))
    ok_false <- isTRUE(all.equal(as.numeric(est_byrow_false[k, ]), truth, tolerance = 1e-6))
    
    if (ok_true && !ok_false) {
      est <- est_byrow_true
      byrow_used <- TRUE
    } else if (ok_false && !ok_true) {
      est <- est_byrow_false
      byrow_used <- FALSE
    } else if (ok_true && ok_false) {
      est <- est_byrow_false
      byrow_used <- FALSE
    } else {
      stop("Could not validate quantile alignment for svyby(svyquantile).", call. = FALSE)
    }
    
    
  } else {
    est <- as.matrix(est_vec)
    byrow_used <- FALSE  # arbitrary; won't be used unless se_mat is a vector
  }

  
  if (nrow(est) != n_by || ncol(est) != length(probs)) {
    stop("Unexpected coef() shape from svyby(svyquantile); cannot align quantiles reliably.", call. = FALSE)
  }
  
  if (is.na(byrow_used)) byrow_used <- TRUE
  # ---- SE ----
  
  se_mat <- NULL
  if (need_var) {
    se_try <- try(survey::SE(sb), silent = TRUE)
    if (!inherits(se_try, "try-error")) {
      se_mat <- se_try
      if (is.vector(se_mat)) {
        if (length(se_mat) != n_by * length(probs)) {
          se_mat <- NULL
        } else {
          se_mat <- matrix(se_mat, nrow = n_by, ncol = length(probs), byrow = byrow_used)
        }
      } else {
        se_mat <- as.matrix(se_mat)
      }
    }
  }
  
  # ---- CI ----
  ci_arr <- NULL
  if (isTRUE(return_ci)) {
    ci_try <- try(stats::confint(sb, level = level), silent = TRUE)
    if (!inherits(ci_try, "try-error")) ci_arr <- ci_try
  }
  
  measures <- .prob_to_measure(probs, base = quantile_base)
  if (!is.null(measure_prefix) && nzchar(measure_prefix)) {
    measures <- paste0(measure_prefix, "_", measures)
  }
  
  out <- purrr::map_dfr(seq_along(probs), function(j) {
    dfj <- by_df
    dfj$prob <- probs[[j]]
    dfj$measure <- measures[[j]]
    dfj$estimate <- as.numeric(est[, j])
    
    if (isTRUE(return_se)) {
      dfj$se <- if (!is.null(se_mat)) as.numeric(se_mat[, j]) else NA_real_
    }
    
    if (isTRUE(return_ci)) {
      ci_l <- ci_u <- rep(NA_real_, nrow(dfj))
      if (is.matrix(ci_arr) && ncol(ci_arr) == 2L && nrow(ci_arr) == nrow(dfj)) {
        ci_l <- ci_arr[, 1]; ci_u <- ci_arr[, 2]
      } else if (length(dim(ci_arr)) == 3L) {
        ci_l <- ci_arr[, 1, j]
        ci_u <- ci_arr[, 2, j]
      }
      dfj$ci_l <- as.numeric(ci_l)
      dfj$ci_u <- as.numeric(ci_u)
    }
    
    dfj
  })
  
  # Standardize column names
  out <- out |>
    dplyr::rename(time = !!rlang::sym(time_var))
  
  if (!is.null(group_var) && group_var %in% names(out)) {
    out <- out |>
      dplyr::rename(group = !!rlang::sym(group_var)) |>
      dplyr::mutate(group = as.character(.data$group))
  }
  
  # Enforce completeness if requested and variance was requested
  if (isTRUE(require_complete) && need_var) {
    if (isTRUE(return_se) && ("se" %in% names(out)) && any(!is.finite(out$se))) {
      stop("Fast quantiles path returned incomplete SE coverage; falling back to loop.", call. = FALSE)
    }
    if (isTRUE(return_ci) &&
        (("ci_l" %in% names(out) && any(!is.finite(out$ci_l))) ||
         ("ci_u" %in% names(out) && any(!is.finite(out$ci_u))))) {
      stop("Fast quantiles path returned incomplete CI coverage; falling back to loop.", call. = FALSE)
    }
  }
  
  out
}
