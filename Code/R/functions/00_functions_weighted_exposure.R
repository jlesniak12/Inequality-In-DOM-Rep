#===============================================================================
#
# File defines functions necessary for calculating minimum wage exposure. These
# are written assuming df input is survey data and use survey package functions
# to creare design and do proper weighted calculation.
#
#
# near_mw_share: Calculates share of of obs where "income" is within +/- range 
#                of "min_wage". The "by_vars" character vector along with with 
#                "time_var" define the cells over which the proportion of obs 
#                 within the range defined by "mw_lower" and "mw_upper" are 
#                 calculated.
#
# firmsize_pi: Calculates proportion of total population present within each
#              grouping of "size_var". The "by_var" along with "time_var" define
#              cells used for calculating total population observations.
#              Output used for calculating weighted averages.
#
#
# weighted_exposure: Function to calculate a weighted average of the variable in
#                    "exposure_col" given weights variable in "pi_col"
#
#
#===============================================================================









#calculate share of workers near minimum wage within a group x time cell
near_mw_share <- function(df,
                          time_var = "year",
                          time_subset = NULL,
                          by_vars,
                          min_wage = "real_minwage_harmonized",
                          income = "real_salary_income_total",
                          out_col = "share_near_minwage",
                          mw_lower = 0.9,
                          mw_upper = 1.1,
                          formal_only = TRUE) {
  
  # -- check/validate inputs
  
  stopifnot(is.character(time_var), length(time_var) == 1)
  stopifnot(is.character(by_vars), length(by_vars) >= 1)
  
  if (!is.null(time_subset)) {
    df <- df %>% filter(.data[[time_var]] %in% time_subset)
  }
  
  df <- df %>% filter(OCUPADO == 1)
  
  if (isTRUE(formal_only)) {
    df <- df %>% filter(Employment_Status == "Formal")
  }
  
  # -- check required variables for calc min wage exposure
  required <- c(time_var, by_vars, min_wage, income)
  
  df <- df %>%
    filter(if_all(all_of(required), ~ !is.na(.x))) %>%
    
    # -- define binary var if salary income is within band of min wage for that worker
    mutate(
      near_mw = if_else(
        .data[[income]] >= mw_lower * .data[[min_wage]] &
          .data[[income]] <= mw_upper * .data[[min_wage]],
        1, 0
      )
    )
  
  
  # -- create svy design object and calc prop near mw 
  des <- svydesign(
    id      = ~psu_unique,
    strata  = ~strata_unique,
    weights = ~FACTOR_EXPANSION,
    data    = df,
    nest    = TRUE
  )
  
  by_fml <- stats::as.formula(
    paste0("~", paste(c(time_var, by_vars), collapse = " + "))
  )
  
  svyby(
    ~near_mw,
    by_fml,
    design = des,
    FUN = svymean,
    na.rm = TRUE
  ) %>%
    tibble::as_tibble() %>%
    dplyr::rename(!!out_col := near_mw)
  
}



#calculate shares within a group (used for weighting in summation)
firmsize_pi <- function(df,
                        time_var = "year",
                        time_subset = NULL,
                        by_vars,
                        size_var = "Wage_group",
                        formal_only = TRUE) {
  
  # -- check/validate inputs
  
  stopifnot(size_var %in% by_vars)
  
  if (!is.null(time_subset)) {
    df <- df %>% filter(.data[[time_var]] %in% time_subset)
  }
  
  df <- df %>% filter(OCUPADO == 1)
  
  if (isTRUE(formal_only)) {
    df <- df %>% filter(Employment_Status == "Formal")
  }
  
  required <- c(time_var, by_vars)
  df <- df %>% filter(if_all(all_of(required), ~ !is.na(.x)))
  
  # -- create svy design object and calc prop near mw 
  
  des <- svydesign(
    id      = ~psu_unique,
    strata  = ~strata_unique,
    weights = ~FACTOR_EXPANSION,
    data    = df,
    nest    = TRUE
  )
  
  by_fml <- stats::as.formula(
    paste0("~", paste(c(time_var, by_vars), collapse = " + "))
  )
  
  # calc total svy weighted pop within  time x by vars cell (denominator, should include size_var)
  cell_totals <- svyby(
    ~one,
    by_fml,
    design = update(des, one = 1),
    FUN = svytotal,
    na.rm = TRUE
  ) %>%
    tibble::as_tibble() %>%
    rename(total = one)
  
  
  # pi is defined within time × (by_vars minus size_var)
  grp_keys <- setdiff(c(time_var, by_vars), size_var)
  
  cell_totals %>%
    group_by(across(all_of(grp_keys))) %>%
    mutate(pi = total / sum(total)) %>%
    ungroup()
}


weighted_exposure <- function(near_tbl,
                              pi_tbl,
                              time_var,
                              by_vars,
                              weight_dim,
                              exposure_col = "near_mw_share",
                              pi_col = "pi",
                              out_col = "exposure_weighted") {
  
  # -- check/validate inputs
  stopifnot(weight_dim %in% names(near_tbl))
  stopifnot(weight_dim %in% names(pi_tbl))
  stopifnot(!weight_dim %in% by_vars)
  
  
  keys <- c(time_var, by_vars, weight_dim)
  
  # -- generate weighted sum of exposure measure
  near_tbl %>%
    left_join(
      pi_tbl %>% select(all_of(c(keys, pi_col))),
      by = keys
    ) %>%
    mutate(weighted = .data[[exposure_col]] * .data[[pi_col]]) %>%
    group_by(across(all_of(c(time_var, by_vars)))) %>%
    summarise(
      !!out_col := sum(weighted, na.rm = TRUE),
      .groups = "drop"
    )
}
