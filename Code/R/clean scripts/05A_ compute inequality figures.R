#===============================================================================
#
# Script: 05A_Compute_Inequality.R
#
# Purpose: Compute wage inequality / compression metrics for the inequality
#          figures. Documents how the formal private wage distribution has
#          compressed over the sample period as the minimum wage rose.
#          Saves RDS objects for 05B_Plot_Inequality.R. No plotting here.
#
# SURVEY CONVENTION:
#   Uses the `survey` package directly via the same tidy helpers as 04A
#   (svy_mean_by, svy_quantile_by) plus a variance-of-log helper. Every
#   output object shares the schema: year_quarter, (group), estimate, se,
#   n_obs, sparse.
#
# FIGURES THIS SCRIPT FEEDS:
#   Fig INEQ-1  Variance of log real hourly earnings over time
#               → ineq_var_log.rds
#   Fig INEQ-2  Percentile ratios (p90/p10, p50/p10, p90/p50) over time
#               → ineq_pctile_ratios.rds
#   Fig INEQ-3  Cleaner 2016-vs-2024 density overlay (microdata extract)
#               → ineq_density_extract.rds
#
# POPULATION:
#   All FORMAL PRIVATE wage earners (samples$wage_earners subset to
#   Employment_Status == "Formal" and private employee). No firm-size tier
#   exclusion — inequality is a property of the whole wage distribution and
#   does not depend on which MW floor applies, so the Large-bin contamination
#   issue (which is about floor assignment) is irrelevant here.
#
# OUTCOME:
#   log of real hourly base salary (real_salary_primary_hourly_base).
#   Hourly rather than monthly so the measure is not confounded by hours
#   variation — consistent with the primary compliance measure in 04A.
#   Real terms (2025Q2 pesos) so levels are comparable across quarters.
#
# OUTPUTS → config$paths$processed_data / "Inequality":
#   ineq_var_log.rds
#   ineq_pctile_ratios.rds
#   ineq_density_extract.rds
#
# READS:
#   samples object (from 03_Sample_Definitions.R)
#
#===============================================================================

source("Code/R/setup/00_setup.R")
source("Code/R/clean scripts/03_Sample Definitions.R")

library(survey)

cat("=== 05A_Compute_Inequality.R ===\n\n")


#===============================================================================
# SHARED CONSTANTS
#===============================================================================

MW_EVENT_QTR    <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")
STANDARD_WEEK   <- 44
WEEKS_PER_MONTH <- 52 / 12
MIN_CELL_N      <- 30
DIST_FOCAL_YEARS <- c(2016L, 2024L)

WAGE_VAR <- "real_salary_primary_hourly_base"   # real hourly base salary

out_dir <- file.path(config$paths$processed_data, "Inequality")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

save_rds <- function(obj, name) {
  path <- file.path(out_dir, paste0(name, ".rds"))
  saveRDS(obj, path)
  cat("  Saved:", path, "\n")
}


#===============================================================================
# HELPERS — tidy survey wrappers (same contract as 04A)
#===============================================================================

.n_by <- function(design, time_var, group_var = NULL) {
  grp <- c(time_var, group_var)
  design$variables %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(n_obs = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
}

.standardise_se <- function(df, keys) {
  known    <- c(keys, "estimate")
  leftover <- setdiff(names(df), known)
  se_col <- NULL
  if ("se" %in% leftover) {
    se_col <- "se"
  } else {
    cand <- leftover[grepl("(^|\\.)se(\\.|$)", leftover, ignore.case = TRUE) |
                       grepl("se$", leftover, ignore.case = TRUE)]
    cand <- setdiff(cand, c("ci_l", "ci_u", "ci.l", "ci.u"))
    if (length(cand)) se_col <- cand[[1]]
  }
  if (is.null(se_col)) df$se <- NA_real_ else
    names(df)[names(df) == se_col] <- "se"
  df[, c(keys, "estimate", "se"), drop = FALSE]
}

# Survey-weighted single quantile by time (+ optional group)
svy_quantile_by <- function(design, var, time_var, group_var = NULL,
                            prob = 0.5, na_rm = TRUE) {
  if (isTRUE(na_rm)) {
    keep   <- !is.na(design$variables[[var]])
    design <- design[keep, ]
  }
  grp     <- c(time_var, group_var)
  by_fml  <- stats::as.formula(paste0("~", paste(grp, collapse = "+")))
  var_fml <- stats::as.formula(paste0("~", var))
  est <- svyby(var_fml, by_fml, design,
               FUN = svyquantile, quantiles = prob, ci = TRUE,
               vartype = "se", keep.names = FALSE, na.rm = na_rm) %>%
    tibble::as_tibble()
  non_key <- setdiff(names(est), grp)
  se_ci   <- non_key[grepl("se|ci", non_key, ignore.case = TRUE)]
  est_col <- setdiff(non_key, se_ci)
  if (length(est_col) != 1)
    est_col <- if (var %in% non_key) var else setdiff(non_key, se_ci)[[1]]
  names(est)[names(est) == est_col] <- "estimate"
  est <- est %>% dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
  est <- .standardise_se(est, keys = grp)
  .n_by(design, time_var, group_var) -> n_df
  est %>%
    dplyr::left_join(n_df, by = grp) %>%
    dplyr::rename(year_quarter = !!time_var) %>%
    dplyr::mutate(sparse = n_obs < MIN_CELL_N)
}

# Survey-weighted VARIANCE OF LOG of a positive variable, by time.
# svyvar on log(var) gives the (weighted) variance; SE via the svyvar object.
svy_var_log_by <- function(design, var, time_var = "year_quarter") {
  # Restrict to strictly positive, non-missing values (log requires > 0)
  keep   <- !is.na(design$variables[[var]]) & design$variables[[var]] > 0
  design <- design[keep, ]
  
  # Add the log variable to the design
  design <- update(design, .logvar = log(get(var)))
  
  vlog <- svyby(~.logvar, stats::as.formula(paste0("~", time_var)),
                design, svyvar, vartype = "se", keep.names = FALSE) %>%
    tibble::as_tibble()
  
  # svyby(svyvar) names the estimate after the variable; SE as "se"
  vlog <- vlog %>%
    dplyr::rename(estimate = dplyr::any_of(".logvar")) %>%
    dplyr::rename(se = dplyr::any_of("se"))
  # Some survey versions name the var column "var" or ".logvar.var"
  if (!"estimate" %in% names(vlog)) {
    non_key <- setdiff(names(vlog), c(time_var, "se"))
    names(vlog)[names(vlog) == non_key[[1]]] <- "estimate"
  }
  
  n_df <- .n_by(design, time_var)
  vlog %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(time_var), as.character)) %>%
    dplyr::left_join(n_df, by = time_var) %>%
    dplyr::rename(year_quarter = !!time_var) %>%
    dplyr::mutate(sparse = n_obs < MIN_CELL_N)
}


#===============================================================================
# STEP 0. Build the formal private wage-earner design
#
# samples$wage_earners = salaried (private + public) with positive real salary.
# Subset to FORMAL PRIVATE here. Employment_Type identifies private employees;
# Employment_Status == "Formal" restricts to the legally-covered formal sector.
#===============================================================================

cat("[0] Building formal private FULL-TIME wage-earner design...\n")

# Population: formal private wage earners working a near-standard week.
#
# WHY THE HOURS RESTRICTION (40-48 hrs):
#   The hourly wage is derived as monthly_salary / (weeks_per_month *
#   pmin(hours, 44)). The pmin caps the denominator at 44, which correctly
#   prevents long-hours workers from getting an artificially LOW hourly rate.
#   But it does nothing on the low end: a worker reporting very few hours
#   (often a recall/coding error rather than a real 1-hour job) gets a tiny
#   denominator and therefore an absurdly HIGH implied hourly wage. A handful
#   of these inflate the variance of log dramatically (the COVID-era spike).
#
#   Restricting to a near-standard week (40-48 hrs) removes the noisy-denominator
#   tail at the source and yields a clean, like-for-like comparison: hourly wage
#   inequality AMONG FULL-TIME workers. It also makes the pmin cap rarely bind,
#   so the measure is simpler to reason about. This is a standard population
#   choice in the minimum-wage inequality literature (full-time / FTE workers),
#   and it removes part-time status as a confound. Trade-off: the claim is now
#   about full-time workers specifically, which we state in the figure notes.
FT_HOURS_LO <- 40
FT_HOURS_HI <- 48

design_fp <- subset(
  samples$wage_earners$design,
  Employment_Status == "Formal" &
    Employment_Type == "private employee" &
    !is.na(hours_worked_primary) &
    hours_worked_primary >= FT_HOURS_LO &
    hours_worked_primary <= FT_HOURS_HI
)

n_total <- nrow(design_fp$variables)
n_valid <- sum(!is.na(design_fp$variables[[WAGE_VAR]]) &
                 design_fp$variables[[WAGE_VAR]] > 0)
cat(sprintf("  Formal private FT (%d-%d hrs) wage earners: %d rows | valid hourly wage: %d\n",
            FT_HOURS_LO, FT_HOURS_HI, n_total, n_valid))


#===============================================================================
# STEP 1. INEQ-1: Variance of log real hourly earnings over time
#===============================================================================

cat("[1] Variance of log real hourly earnings...\n")

ineq_var_log <- svy_var_log_by(design_fp, WAGE_VAR, "year_quarter")

cat(sprintf("  Quarters: %d | Var(log) range: [%.3f, %.3f]\n",
            dplyr::n_distinct(ineq_var_log$year_quarter),
            min(ineq_var_log$estimate, na.rm = TRUE),
            max(ineq_var_log$estimate, na.rm = TRUE)))

save_rds(ineq_var_log, "ineq_var_log")


#===============================================================================
# STEP 2. INEQ-2: Percentile ratios over time
#
# Compute p10, p50, p90 of real hourly earnings per quarter, then form:
#   p90/p10  — overall dispersion
#   p50/p10  — lower-tail dispersion (most MW-sensitive)
#   p90/p50  — upper-tail dispersion (MW-insensitive; placebo-ish)
# Ratios of levels; we also keep the raw percentiles for reference.
#===============================================================================

cat("[2] Percentile ratios...\n")

pctiles <- purrr::map_dfr(c(0.10, 0.50, 0.90), function(p) {
  svy_quantile_by(design_fp, WAGE_VAR, "year_quarter", prob = p) %>%
    dplyr::transmute(year_quarter, p = paste0("p", p * 100),
                     value = estimate, n_obs, sparse)
})

ineq_pctile_ratios <- pctiles %>%
  dplyr::select(year_quarter, p, value) %>%
  tidyr::pivot_wider(names_from = p, values_from = value) %>%
  dplyr::mutate(
    `p90/p10` = p90 / p10,
    `p50/p10` = p50 / p10,
    `p90/p50` = p90 / p50
  ) %>%
  # attach n_obs (same per quarter across percentiles; take p50's)
  dplyr::left_join(
    dplyr::filter(pctiles, p == "p50") %>%
      dplyr::select(year_quarter, n_obs, sparse),
    by = "year_quarter"
  )

cat(sprintf("  Quarters: %d | p90/p10 range: [%.2f, %.2f]\n",
            nrow(ineq_pctile_ratios),
            min(ineq_pctile_ratios$`p90/p10`, na.rm = TRUE),
            max(ineq_pctile_ratios$`p90/p10`, na.rm = TRUE)))

save_rds(ineq_pctile_ratios, "ineq_pctile_ratios")


#===============================================================================
# STEP 3. INEQ-3: Density extract for the 2016-vs-2024 overlay
#
# Individual-level extract of log real hourly earnings for the two focal years,
# all formal private wage earners (no tier split — this is the overall
# distribution). Weights normalised within year so each year's density
# integrates to 1. KDE computed at plot time.
#===============================================================================

cat("[3] Density extract (2016 vs 2024)...\n")

ineq_density_extract <- design_fp$variables %>%
  dplyr::filter(year %in% DIST_FOCAL_YEARS,
                !is.na(.data[[WAGE_VAR]]), .data[[WAGE_VAR]] > 0) %>%
  dplyr::transmute(
    year, year_quarter,
    log_real_hourly = log(.data[[WAGE_VAR]]),
    FACTOR_EXPANSION,
    focal_year = factor(year, levels = DIST_FOCAL_YEARS)
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(is.finite(log_real_hourly))

cat(sprintf("  Rows: %d | %s: %d | %s: %d\n",
            nrow(ineq_density_extract),
            DIST_FOCAL_YEARS[1],
            sum(ineq_density_extract$year == DIST_FOCAL_YEARS[1]),
            DIST_FOCAL_YEARS[2],
            sum(ineq_density_extract$year == DIST_FOCAL_YEARS[2])))

save_rds(ineq_density_extract, "ineq_density_extract")


#===============================================================================
# STEP 4. Validation
#===============================================================================

cat("\n[4] Validation...\n")
cat(sprintf("  Var(log) first/last quarter: %.3f / %.3f\n",
            ineq_var_log$estimate[1],
            ineq_var_log$estimate[nrow(ineq_var_log)]))
cat(sprintf("  p90/p10 first/last quarter: %.2f / %.2f\n",
            ineq_pctile_ratios$`p90/p10`[1],
            ineq_pctile_ratios$`p90/p10`[nrow(ineq_pctile_ratios)]))

cat("\n=== 05A_Compute_Inequality.R complete ===\n")
cat("Outputs saved to:", out_dir, "\n\n")