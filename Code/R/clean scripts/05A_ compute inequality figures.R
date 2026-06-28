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
#
#   HEADLINE SET — formal private full-time (40-48h) wage earners, HOURLY:
#     Fig INEQ-1  Variance of log real hourly earnings over time
#                 → ineq_var_log.rds
#     Fig INEQ-2  Percentile ratios (p90/p10, p50/p10, p90/p50) over time
#                 → ineq_pctile_ratios.rds
#     Fig INEQ-3  2016-vs-2024 density overlay (microdata extract)
#                 → ineq_density_extract.rds
#
#   PARENTE SET — all wage earners, MONTHLY earnings, by formality:
#     Fig INEQ-4  Variance of log monthly earnings, Overall/Formal/Informal
#                 (replicates Parente 2024 Fig 1)  → ineq_var_log_formality.rds
#     Fig INEQ-5  Percentile ratios by formality   → ineq_pctile_ratios_formality.rds
#     Fig INEQ-6  Density overlay 2016 vs 2024 by formality
#                 → ineq_density_formality.rds
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

WAGE_VAR <- "real_salary_income_wage_primary"   # real MONTHLY earnings (2025Q2 DOP)

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
  # STEP 0. Build the formal private wage-earner design (monthly earnings)
  #
  # samples$wage_earners = salaried (private + public) with positive real monthly
  # salary. Subset to FORMAL PRIVATE here. No hours restriction: the outcome is
  # MONTHLY earnings, which has no hours denominator, so part-time vs full-time is
  # not a measurement problem — it is part of the earnings distribution we want.
  # Keeping part-timers also makes the population consistent with the Parente set.
  #===============================================================================

cat("[0] Building formal private wage-earner design (monthly earnings)...\n")

design_fp <- subset(
  samples$wage_earners$design,
  Employment_Status == "Formal" &
    Employment_Type == "private employee"
)

n_total <- nrow(design_fp$variables)
n_valid <- sum(!is.na(design_fp$variables[[WAGE_VAR]]) &
                 design_fp$variables[[WAGE_VAR]] > 0)
cat(sprintf("  Formal private wage earners: %d rows | valid monthly earnings: %d\n",
            n_total, n_valid))



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
    log_real_earn = log(.data[[WAGE_VAR]]),          # was log_real_hourly
    FACTOR_EXPANSION,
    focal_year = factor(year, levels = DIST_FOCAL_YEARS)
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(is.finite(log_real_earn))

cat(sprintf("  Rows: %d | %s: %d | %s: %d\n",
            nrow(ineq_density_extract),
            DIST_FOCAL_YEARS[1],
            sum(ineq_density_extract$year == DIST_FOCAL_YEARS[1]),
            DIST_FOCAL_YEARS[2],
            sum(ineq_density_extract$year == DIST_FOCAL_YEARS[2])))

save_rds(ineq_density_extract, "ineq_density_extract")

#===============================================================================
# ============================  PARENTE SET  ==================================
# Monthly earnings, ALL wage earners, split Overall / Formal / Informal.
#
# This set replicates Parente (2024) Fig 1 (variance of log earnings by
# formality) and extends it to percentile ratios and densities. It differs
# from the headline set in three deliberate ways:
#   (a) MONTHLY earnings (real_salary_income_wage_primary), not derived hourly
#       — matches Parente and travels cleanly across formality (no hours
#       denominator problem; includes part-time workers).
#   (b) ALL wage earners (no full-time hours restriction) — the broad
#       population the informal-sector story requires.
#   (c) Split by Employment_Status into Formal and Informal, plus an Overall
#       (pooled) series.
#
# Population: samples$wage_earners (salaried private + public, positive real
# salary). We keep public employees here to match the broad "wage earners"
# definition used for inequality; formality is the split of interest.
#===============================================================================

MONTHLY_VAR <- "real_salary_income_wage_primary"   # monthly real earnings

cat("\n[PARENTE SET] Monthly earnings by formality...\n")

design_we <- samples$wage_earners$design   # all wage earners

# Helper: compute a measure for Overall + each formality group and stack them,
# tagging the `group` column. `fun` is one of the svy_*_by helpers and must
# accept (design, var, time_var, ...). For grouped (Formal/Informal) we subset
# the design; Overall uses the full design.
by_formality <- function(design, fun, ...) {
  overall <- fun(design, ...) %>% dplyr::mutate(group = "Overall")
  formal  <- fun(subset(design, Employment_Status == "Formal"), ...) %>%
    dplyr::mutate(group = "Formal")
  informal <- fun(subset(design, Employment_Status == "Informal"), ...) %>%
    dplyr::mutate(group = "Informal")
  dplyr::bind_rows(overall, formal, informal) %>%
    dplyr::mutate(group = factor(group, levels = c("Overall", "Formal", "Informal")))
}


#===============================================================================
# STEP 5. INEQ-4: Variance of log monthly earnings, Overall/Formal/Informal
#         (the Parente Fig 1 replication)
#===============================================================================

cat("[5] INEQ-4 variance of log monthly earnings by formality...\n")

ineq_var_log_formality <- by_formality(
  design_we, svy_var_log_by, var = MONTHLY_VAR, time_var = "year_quarter"
)

cat(sprintf("  Rows: %d | groups: %s\n",
            nrow(ineq_var_log_formality),
            paste(levels(ineq_var_log_formality$group), collapse = ", ")))
save_rds(ineq_var_log_formality, "ineq_var_log_formality")


#===============================================================================
# STEP 6. INEQ-5: Percentile ratios of monthly earnings by formality
#===============================================================================

cat("[6] INEQ-5 percentile ratios by formality...\n")

# Compute p10/p50/p90 for each group, then form ratios.
pctiles_formality <- purrr::map_dfr(c(0.10, 0.50, 0.90), function(p) {
  by_formality(design_we, svy_quantile_by,
               var = MONTHLY_VAR, time_var = "year_quarter", prob = p) %>%
    dplyr::transmute(year_quarter, group, p = paste0("p", p * 100),
                     value = estimate, n_obs, sparse)
})

ineq_pctile_ratios_formality <- pctiles_formality %>%
  dplyr::select(year_quarter, group, p, value) %>%
  tidyr::pivot_wider(names_from = p, values_from = value) %>%
  dplyr::mutate(
    `p90/p10` = p90 / p10,
    `p50/p10` = p50 / p10,
    `p90/p50` = p90 / p50
  ) %>%
  dplyr::left_join(
    dplyr::filter(pctiles_formality, p == "p50") %>%
      dplyr::select(year_quarter, group, n_obs, sparse),
    by = c("year_quarter", "group")
  )

cat(sprintf("  Rows: %d\n", nrow(ineq_pctile_ratios_formality)))
save_rds(ineq_pctile_ratios_formality, "ineq_pctile_ratios_formality")


#===============================================================================
# STEP 7. INEQ-6: Density extract of log monthly earnings, 2016 vs 2024,
#         tagged by formality (for faceted overlay)
#===============================================================================

cat("[7] INEQ-6 density extract by formality...\n")

ineq_density_formality <- design_we$variables %>%
  dplyr::filter(year %in% DIST_FOCAL_YEARS,
                !is.na(.data[[MONTHLY_VAR]]), .data[[MONTHLY_VAR]] > 0,
                !is.na(Employment_Status)) %>%
  dplyr::transmute(
    year, year_quarter,
    Employment_Status,
    log_real_monthly = log(.data[[MONTHLY_VAR]]),
    FACTOR_EXPANSION,
    focal_year = factor(year, levels = DIST_FOCAL_YEARS)
  ) %>%
  # Normalise weights within year x formality so each panel's curves integrate to 1
  dplyr::group_by(year, Employment_Status) %>%
  dplyr::mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(is.finite(log_real_monthly))

cat(sprintf("  Rows: %d | Formal: %d | Informal: %d\n",
            nrow(ineq_density_formality),
            sum(ineq_density_formality$Employment_Status == "Formal"),
            sum(ineq_density_formality$Employment_Status == "Informal")))
save_rds(ineq_density_formality, "ineq_density_formality")


#===============================================================================
# STEP 8. Validation
#===============================================================================

cat("\n[8] Validation...\n")
cat(sprintf("  [Headline] Var(log) first/last quarter: %.3f / %.3f\n",
            ineq_var_log$estimate[1],
            ineq_var_log$estimate[nrow(ineq_var_log)]))
cat(sprintf("  [Headline] p90/p10 first/last quarter: %.2f / %.2f\n",
            ineq_pctile_ratios$`p90/p10`[1],
            ineq_pctile_ratios$`p90/p10`[nrow(ineq_pctile_ratios)]))
cat("  [Parente] Var(log) monthly, last quarter by group:\n")
ineq_var_log_formality %>%
  dplyr::group_by(group) %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::select(group, year_quarter, estimate) %>%
  print()

cat("\n=== 05A_Compute_Inequality.R complete ===\n")
cat("Outputs saved to:", out_dir, "\n\n")