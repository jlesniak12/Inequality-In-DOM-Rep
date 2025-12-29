svy_quantiles_over_time_core <- function(design,
                                         vars,
                                         by_time     = "year_quarter",
                                         by_group    = NULL,
                                         probs       = c(.10,.25,.50,.75,.90),
                                         subset_expr = NULL,
                                         weight_var,
                                         min_cell_n     = NULL,
                                         min_cell_neff  = NULL,
                                         min_cell_unique = NULL,
                                         se          = FALSE,
                                         na.rm       = TRUE,
                                         fallback    = TRUE,
                                         verbose     = TRUE) {
  
  stopifnot(inherits(design, c("survey.design","svyrep.design")))
  if (!by_time %in% names(design$variables))
    stop("by_time not found in design variables.")
  if (!is.null(by_group) && !by_group %in% names(design$variables))
    stop("by_group not found in design variables.")
  if (!weight_var %in% names(design$variables))
    stop("weight_var not found in design variables.")
  
  present <- intersect(vars, names(design$variables))
  if (!length(present))
    stop("None of vars exist in design.")
  
  probs <- sort(unique(as.numeric(probs)))
  if (any(!is.finite(probs)) || any(probs <= 0 | probs >= 1))
    stop("All probs must be in (0,1).")
  
  # ---- subset once ----
  if (!is.null(subset_expr)) {
    design <- subset(design, eval(subset_expr, design$variables, parent.frame()))
  }
  
  # ---- build working df with stable row id + weights ----
  df <- design$variables %>%
    dplyr::mutate(
      .row_id__ = dplyr::row_number(),
      .w__      = .data[[weight_var]]
    )
  
  # ---- effective sample size ----
  neff <- function(w) {
    w <- w[is.finite(w) & !is.na(w)]
    if (!length(w)) return(NA_real_)
    s2 <- sum(w^2)
    if (s2 <= 0) return(NA_real_)
    (sum(w)^2) / s2
  }
  
  grp_vars <- c(by_time, by_group)
  grp_vars <- grp_vars[!is.na(grp_vars)]
  
  # ---- cell adequacy table ----
  cell_ok <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars))) %>%
    dplyr::summarise(
      n_cell   = dplyr::n(),
      neff     = neff(.w__),
      .groups  = "drop"
    ) %>%
    dplyr::mutate(
      ok = TRUE,
      ok = if (!is.null(min_cell_n))    ok & (n_cell >= min_cell_n) else ok,
      ok = if (!is.null(min_cell_neff)) ok & (neff   >= min_cell_neff) else ok
    ) %>%
    dplyr::filter(ok) %>%
    dplyr::select(dplyr::all_of(grp_vars))
  
  if (!nrow(cell_ok)) {
    if (verbose) message("No cells meet minimum sample requirements.")
    return(tibble::tibble())
  }
  
  # restrict rows to ok cells
  df_good <- df %>%
    dplyr::semi_join(cell_ok, by = grp_vars)
  
  keep_ids <- df_good$.row_id__
  
  # subset design using stable row id
  # NOTE: .row_id__ exists in df (parent frame) for subset() evaluation
  design_good <- subset(design, df$.row_id__ %in% keep_ids)
  
  # ---- fallback loop ----
  manual_loop <- function(v) {
    times <- sort(unique(design_good$variables[[by_time]]))
    
    if (is.null(by_group)) {
      out <- list(); k <- 0L
      for (t in times) {
        des_sub <- subset(design_good, get(by_time) == t)
        if (!nrow(des_sub$variables)) next
        
        q <- tryCatch(
          survey::svyquantile(reformulate(v), des_sub,
                              quantiles = probs, se = se, na.rm = na.rm),
          error = function(e) NULL
        )
        if (is.null(q)) next
        
        est <- as.numeric(unlist(q))[seq_along(probs)]
        k <- k + 1L
        out[[k]] <- tibble::tibble(
          !!by_time  := t,
          base_var   = v,
          percentile = probs,
          estimate   = est,
          variable   = paste0(v, "_p", round(probs*100))
        )
      }
      return(dplyr::bind_rows(out))
    }
    
    # grouped case
    groups <- sort(unique(design_good$variables[[by_group]]))
    groups <- groups[!is.na(groups)]
    
    out <- list(); k <- 0L
    for (g in groups) {
      for (t in times) {
        des_sub <- subset(design_good, get(by_time) == t & get(by_group) == g)
        if (!nrow(des_sub$variables)) next
        
        q <- tryCatch(
          survey::svyquantile(reformulate(v), des_sub,
                              quantiles = probs, se = se, na.rm = na.rm),
          error = function(e) NULL
        )
        if (is.null(q)) next
        
        est <- as.numeric(unlist(q))[seq_along(probs)]
        k <- k + 1L
        out[[k]] <- tibble::tibble(
          !!by_time  := t,
          !!by_group := g,
          base_var   = v,
          percentile = probs,
          estimate   = est,
          variable   = paste0(v, "_p", round(probs*100))
        )
      }
    }
    dplyr::bind_rows(out)
  }
  
  # ---- fast path (svyby) ----
  f_by <- if (is.null(by_group)) {
    as.formula(paste0("~", by_time))
  } else {
    as.formula(paste0("~", by_time, "+", by_group))
  }
  
  out_all <- vector("list", length(present))
  
  for (i in seq_along(present)) {
    v <- present[i]
    
    # optional unique-values filter for this variable (cell-level)
    design_v <- design_good
    if (!is.null(min_cell_unique)) {
      dv <- design_good$variables
      cell_u <- dv %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars))) %>%
        dplyr::summarise(n_unique = dplyr::n_distinct(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(n_unique >= min_cell_unique) %>%
        dplyr::select(dplyr::all_of(grp_vars))
      
      if (!nrow(cell_u)) {
        if (verbose) message("No cells meet min_cell_unique for var: ", v)
        out_all[[i]] <- tibble::tibble()
        next
      }
      
      dv_good <- dv %>% dplyr::semi_join(cell_u, by = grp_vars)
      keep2 <- dv_good$.row_id__
      design_v <- subset(design_good, dv$.row_id__ %in% keep2)
    }
    
    q_by <- tryCatch(
      survey::svyby(
        reformulate(v),
        by = f_by,
        design = design_v,
        FUN = survey::svyquantile,
        quantiles = probs,
        se = se,
        na.rm = na.rm,
        keep.var = isTRUE(se),
        vartype = if (isTRUE(se)) "se" else NULL
      ),
      error = function(e) e
    )
    
    if (inherits(q_by, "error")) {
      if (!fallback) stop(q_by)
      if (verbose) message("svyby failed for ", v, "; falling back.")
      out_all[[i]] <- manual_loop(v)
      next
    }
    
    q_df <- tibble::as_tibble(q_by)
    
    id_cols <- c(by_time, by_group)
    id_cols <- id_cols[!is.na(id_cols)]
    
    out_all[[i]] <- q_df %>%
      tidyr::pivot_longer(
        cols = -dplyr::all_of(id_cols),
        names_to = "name",
        values_to = "estimate"
      ) %>%
      dplyr::mutate(
        percentile = as.numeric(stringr::str_extract(name, "(\\d*\\.?\\d+)$")),
        base_var   = v,
        variable   = paste0(v, "_p", round(percentile*100))
      ) %>%
      dplyr::select(dplyr::all_of(id_cols), base_var, percentile, estimate, variable)
  }
  
  dplyr::bind_rows(out_all)
}
