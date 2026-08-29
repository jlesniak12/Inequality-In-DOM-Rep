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
#   Real terms (config$CPI_base_qtr pesos) so levels are comparable across quarters.
#
# OUTPUTS → config$data_dirs$desc_fig:
#   ineq_var_log.rds
#   ineq_pctile_ratios.rds
#   ineq_density_extract.rds
#
# READS:
#   samples object (from 03_Sample_Definitions.R)
#
#===============================================================================

source(here::here("Code","R","clean scripts","00_setup.R"))

source(here::here("Code", "R", "clean scripts", "03_sample definitions.R"))


cat("=== 05A_Compute_Inequality.R ===\n\n")


#===============================================================================
# SHARED CONSTANTS — all resolved from config
#===============================================================================

MW_EVENT_QTR    <- config$events$event_qtrs
STANDARD_WEEK   <- config$hours$standard_week
WEEKS_PER_MONTH <- config$hours$weeks_per_month
MIN_CELL_N      <- config$figures$min_cell_n
MICRO_START     <- config$events$micro_tier_start_qtr
DIST_FOCAL_YEARS <- c(2016L, 2024L)

# Income concepts — each figure is computed for both and saved with a tag.
INCOME_CONCEPTS <- list(
  monthly = list(
    var   = "real_salary_income_wage_primary",
    tag   = "monthly",
    label = "monthly real earnings",
    axis  = "monthly earnings"
  ),
  hourly = list(
    var   = "real_salary_primary_hourly_base",
    tag   = "hourly",
    label = "hourly real earnings (44h cap)",
    axis  = "hourly earnings"
  )
)

out_dir <- config$data_dirs$desc_fig
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

save_rds <- function(obj, name) {
  path <- file.path(out_dir, paste0(name, ".rds"))
  saveRDS(obj, path)
  cat("  Saved:", path, "\n")
}


#===============================================================================
# HELPERS
#
# svy_mean_by, svy_quantile_by, .n_by, .standardise_se are in
# functions/fun_svy_utilities.R (sourced via 00_setup.R). Only
# svy_var_log_by is 05A-specific and defined here.
#===============================================================================

# Survey-weighted VARIANCE OF LOG of a positive variable, by time.
# svyvar on log(var) gives the (weighted) variance; SE via the svyvar object.
svy_var_log_by <- function(design, var, time_var = "year_quarter",
                           group_var = NULL, min_n = MIN_CELL_N) {
  # Restrict to strictly positive, non-missing values (log requires > 0)
  keep   <- !is.na(design$variables[[var]]) & design$variables[[var]] > 0
  design <- design[keep, ]
  
  # Add the log variable to the design
  design <- update(design, .logvar = log(get(var)))
  
  grp <- c(time_var, group_var)
  vlog <- svyby(~.logvar, stats::as.formula(paste0("~", paste(grp, collapse = "+"))),
                design, svyvar, vartype = "se", keep.names = FALSE) %>%
    tibble::as_tibble()
  
  # svyby(svyvar) names the estimate after the variable; SE as "se"
  vlog <- vlog %>%
    dplyr::rename(estimate = dplyr::any_of(".logvar")) %>%
    dplyr::rename(se = dplyr::any_of("se"))
  # Some survey versions name the var column "var" or ".logvar.var"
  if (!"estimate" %in% names(vlog)) {
    non_key <- setdiff(names(vlog), c(grp, "se"))
    names(vlog)[names(vlog) == non_key[[1]]] <- "estimate"
  }
  
  n_df <- .n_by(design, time_var, group_var)
  vlog %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character)) %>%
    dplyr::left_join(n_df, by = grp) %>%
    dplyr::rename(year_quarter = !!time_var) %>%
    dplyr::mutate(sparse = n_obs < min_n)
}

#===============================================================================
# STEP 0. Build the formal private wage-earner design
#
# samples$wage_earners = salaried (private + public) with positive real monthly
# salary. Subset to FORMAL PRIVATE here. Both monthly and hourly variables are
# available on the same design; we loop over INCOME_CONCEPTS below.
#===============================================================================

cat("[0] Building formal private wage-earner design...\n")

design_fp <- subset(
  samples$wage_earners$design,
  Employment_Status == "Formal" &
    Employment_Type == "private employee"
)

cat(sprintf("  Formal private wage earners: %d rows\n",
            nrow(design_fp$variables)))


#===============================================================================
# STEPS 1-3: INEQ-1, INEQ-2, INEQ-3 — looped over income concepts
#
# Each concept produces tagged RDS files:
#   ineq_var_log_{tag}.rds
#   ineq_pctile_ratios_{tag}.rds
#   ineq_density_extract_{tag}.rds
#===============================================================================

for (ic_name in names(INCOME_CONCEPTS)) {
  ic <- INCOME_CONCEPTS[[ic_name]]
  cat(sprintf("\n--- Headline set [%s: %s] ---\n", ic$tag, ic$var))
  
  # INEQ-1: Variance of log earnings
  cat(sprintf("  [1-%s] Variance of log %s...\n", ic$tag, ic$axis))
  vlog <- svy_var_log_by(design_fp, ic$var, "year_quarter")
  cat(sprintf("    Quarters: %d | range: [%.3f, %.3f]\n",
              dplyr::n_distinct(vlog$year_quarter),
              min(vlog$estimate, na.rm = TRUE),
              max(vlog$estimate, na.rm = TRUE)))
  save_rds(vlog, paste0("ineq_var_log_", ic$tag))
  
  # INEQ-2: Percentile ratios
  cat(sprintf("  [2-%s] Percentile ratios...\n", ic$tag))
  pctiles <- purrr::map_dfr(c(0.10, 0.50, 0.90), function(p) {
    svy_quantile_by(design_fp, ic$var, "year_quarter", prob = p) %>%
      dplyr::transmute(year_quarter, p = paste0("p", p * 100),
                       value = estimate, n_obs, sparse)
  })
  ratios <- pctiles %>%
    dplyr::select(year_quarter, p, value) %>%
    tidyr::pivot_wider(names_from = p, values_from = value) %>%
    dplyr::mutate(`p90/p10` = p90 / p10,
                  `p50/p10` = p50 / p10,
                  `p90/p50` = p90 / p50) %>%
    dplyr::left_join(
      dplyr::filter(pctiles, p == "p50") %>%
        dplyr::select(year_quarter, n_obs, sparse),
      by = "year_quarter")
  save_rds(ratios, paste0("ineq_pctile_ratios_", ic$tag))
  
  # INEQ-3: Density extract
  cat(sprintf("  [3-%s] Density extract...\n", ic$tag))
  dens <- design_fp$variables %>%
    dplyr::filter(year %in% DIST_FOCAL_YEARS,
                  !is.na(.data[[ic$var]]), .data[[ic$var]] > 0) %>%
    dplyr::transmute(
      year, year_quarter,
      log_real_earn = log(.data[[ic$var]]),
      FACTOR_EXPANSION,
      focal_year = factor(year, levels = DIST_FOCAL_YEARS)
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(log_real_earn))
  cat(sprintf("    Rows: %d\n", nrow(dens)))
  save_rds(dens, paste0("ineq_density_extract_", ic$tag))
}

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

cat("\n[PARENTE SET] Earnings by formality (looped over income concepts)...\n")

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
# STEPS 5-8: Parente set + Decomposition — looped over income concepts
#
# Each concept produces tagged RDS files:
#   ineq_var_log_formality_{tag}.rds       (INEQ-4)
#   ineq_pctile_ratios_formality_{tag}.rds (INEQ-5)
#   ineq_density_formality_{tag}.rds       (INEQ-6)
#   ineq_variance_decomp_{tag}.rds         (INEQ-7)
#===============================================================================

for (ic_name in names(INCOME_CONCEPTS)) {
  ic <- INCOME_CONCEPTS[[ic_name]]
  cat(sprintf("\n--- Parente set [%s: %s] ---\n", ic$tag, ic$var))
  
  # INEQ-4: Variance of log by formality
  cat(sprintf("  [5-%s] Var(log) by formality...\n", ic$tag))
  vlogf <- by_formality(
    design_we, svy_var_log_by, var = ic$var, time_var = "year_quarter"
  )
  save_rds(vlogf, paste0("ineq_var_log_formality_", ic$tag))
  
  # INEQ-5: Percentile ratios by formality
  cat(sprintf("  [6-%s] Percentile ratios by formality...\n", ic$tag))
  pctf <- purrr::map_dfr(c(0.10, 0.50, 0.90), function(p) {
    by_formality(design_we, svy_quantile_by,
                 var = ic$var, time_var = "year_quarter", prob = p) %>%
      dplyr::transmute(year_quarter, group, p = paste0("p", p * 100),
                       value = estimate, n_obs, sparse)
  })
  ratf <- pctf %>%
    dplyr::select(year_quarter, group, p, value) %>%
    tidyr::pivot_wider(names_from = p, values_from = value) %>%
    dplyr::mutate(`p90/p10` = p90 / p10,
                  `p50/p10` = p50 / p10,
                  `p90/p50` = p90 / p50) %>%
    dplyr::left_join(
      dplyr::filter(pctf, p == "p50") %>%
        dplyr::select(year_quarter, group, n_obs, sparse),
      by = c("year_quarter", "group"))
  save_rds(ratf, paste0("ineq_pctile_ratios_formality_", ic$tag))
  
  # INEQ-6: Density extract by formality
  cat(sprintf("  [7-%s] Density extract by formality...\n", ic$tag))
  densf <- design_we$variables %>%
    dplyr::filter(year %in% DIST_FOCAL_YEARS,
                  !is.na(.data[[ic$var]]), .data[[ic$var]] > 0,
                  !is.na(Employment_Status)) %>%
    dplyr::transmute(
      year, year_quarter, Employment_Status,
      log_real_earn = log(.data[[ic$var]]),
      FACTOR_EXPANSION,
      focal_year = factor(year, levels = DIST_FOCAL_YEARS)
    ) %>%
    dplyr::group_by(year, Employment_Status) %>%
    dplyr::mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(log_real_earn))
  save_rds(densf, paste0("ineq_density_formality_", ic$tag))
  
  # INEQ-7: Variance decomposition
  cat(sprintf("  [8-%s] Variance decomposition...\n", ic$tag))
  
  we_data <- samp_df("wage_earners") %>%
    dplyr::filter(!is.na(Employment_Status),
                  !is.na(.data[[ic$var]]), .data[[ic$var]] > 0)
  
  decomp_form <- decompose_var_log(
    we_data, ic$var, "year_quarter", "Employment_Status", "FACTOR_EXPANSION"
  ) %>% dplyr::mutate(partition = "Formal vs Informal")
  
  we_data_fs <- we_data %>%
    dplyr::filter(!is.na(wage_group), wage_group %in% config$TIER_LEVELS) %>%
    dplyr::mutate(micro_group = dplyr::if_else(wage_group == "Micro",
                                               "Micro", "Non-Micro"))
  decomp_mic <- decompose_var_log(
    we_data_fs, ic$var, "year_quarter", "micro_group", "FACTOR_EXPANSION"
  ) %>% dplyr::mutate(partition = "Micro vs Non-Micro")
  
  decomp_all <- dplyr::bind_rows(decomp_form, decomp_mic) %>%
    dplyr::mutate(partition = factor(partition,
                                     levels = c("Formal vs Informal",
                                                "Micro vs Non-Micro")),
                  sparse = n_obs < MIN_CELL_N)
  save_rds(decomp_all, paste0("ineq_variance_decomp_", ic$tag))
  
  # Decomposition check
  chk <- max(abs(decomp_all$within + decomp_all$between - decomp_all$total))
  cat(sprintf("    Decomp check |W+B-T|: %.2e\n", chk))
}


#===============================================================================
# STEP 9. Validation
#===============================================================================

cat("\n[9] Validation...\n")

# Quick check on the monthly headline set
vlog_m <- readRDS(file.path(out_dir, "ineq_var_log_monthly.rds"))
cat(sprintf("  [Headline monthly] Var(log) first/last: %.3f / %.3f\n",
            vlog_m$estimate[1], vlog_m$estimate[nrow(vlog_m)]))

rat_m <- readRDS(file.path(out_dir, "ineq_pctile_ratios_monthly.rds"))
cat(sprintf("  [Headline monthly] p90/p10 first/last: %.2f / %.2f\n",
            rat_m$`p90/p10`[1], rat_m$`p90/p10`[nrow(rat_m)]))

cat("\n=== 05A_Compute_Inequality.R complete ===\n")
cat("Outputs saved to:", out_dir, "\n\n")