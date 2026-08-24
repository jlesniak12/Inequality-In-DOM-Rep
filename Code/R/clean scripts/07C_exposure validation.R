#===============================================================================
#
# Script: 07C_exposure validation.R
#
# Purpose: Validate the exposure treatment produced by 07A on the geography that
#          07B decided is the analysis unit. 07B answers "which geography?"; 07C
#          answers "does the resulting measure identify what we think it does?"
#
# SCOPE: 4tier only (wage_group + real_minwage_hourly). The 3tier scheme is
#        parked pending a decision on the exposure design (tier-weighted vs
#        single-floor micro). If that decision is made and the 3tier robustness
#        arm becomes relevant again, generalize by reading TIER_VAR / FLOOR_VAR
#        from attr(exposure_geo, "params") instead of hard-coding.
#
# The five checks:
#
#   A. Identity check. Because pi is built from the same weighted counts that
#      form the within-tier rates, the tier-weighted aggregate must equal the
#      ungrouped geo-level share to numerical precision. A non-zero discrepancy
#      is not a design feature; it is evidence of a filter mismatch between
#      near_mw_share and firmsize_pi that must be fixed in 07A.
#
#   B. Sampling noise and reliability. Sd(exposure) across regions is not
#      informative without the survey SE in each region. Reliability ratio
#      lambda = 1 - mean(SE^2) / Var(exposure) is the attenuation bound on the
#      first-stage coefficient. Below ~0.7 is worrying; above ~0.9 is fine.
#
#   C. Ranking-vs-noise plot. Exposure by region with 95% CIs, ordered by
#      sample size. If the min and max regions are the thin ones, the extremes
#      that drive identification are largely noise.
#
#   D. Baseline covariate correlations. Three separate concerns:
#       - below-min share: mechanical, because the band starts at (1 - tol) so
#         exposure and below-min are algebraically disjoint on the same
#         denominator. A strong negative correlation means the below-min
#         outcome inherits a baseline relationship with treatment.
#       - formal share: if strongly negative, exposure is a "poverty of
#         formality" proxy; the informality outcome then rests entirely on
#         pre-trends.
#       - median formal wage: if strongly negative, exposure is a "poor region"
#         proxy more broadly; the wage-variance outcome inherits the same.
#
#   E. Band sensitivity. Moved from 07A. Reports coefficient-relevant statistics
#      (sd, Pearson) alongside the Spearman rank correlation, because the
#      estimator uses continuous exposure, not ranks.
#
# READS   samples$reg_tier$data  (via 03_sample definitions.R)
#         exposure_geo_<geo>_<tier>.rds  (from 07A)
#
# WRITES  config$out_subdirs$exp_validation:
#           tbl_EXP_diagnostics.csv     one row per check
#           tbl_EXP_region.csv          per-region exposure, SE, n, PSU
#           tbl_EXP_band_sensitivity.csv one row per band
#           fig_EXP_A_identity.png      A: ungrouped vs weighted 45-degree
#           fig_EXP_B_reliability.png   B: reliability ratio bar with CI
#           fig_EXP_C_ranking.png       C: exposure by region with 95% CI
#           fig_EXP_D_covariates.png    D: three baseline-covariate scatters
#           fig_EXP_E_band.png          E: exposure by region across bands
#
# CONFIG   Add to 00_config.R under out_subdirs:
#            exp_validation = "Regression Results/Exposure Validation"
#          07A: DELETE Step 9 (band sensitivity) and the corresponding save.
#
#===============================================================================

source(here::here("Code", "R", "clean scripts", "00_setup.R"))
source(file.path(config$paths$scripts, "03_sample definitions.R"))


#===============================================================================
# STEP 0. Parameters and paths
#===============================================================================

cat("=== 07C_exposure validation.R ===\n\n")

pd <- config$data_dirs$regression

GEO        <- config$exposure$construct_geo
BASE_YEAR  <- config$exposure$baseline_year
BAND_LOWER <- 1 - config$exposure$mw_compliance_tolerance
BAND_UPPER <- config$exposure$mw_band_upper
BAND_GRID  <- config$exposure$mw_band_upper_grid
INCOME_VAR <- config$income$income

# Hard-coded 4tier (see header). Change here if the design commits to a
# different tier scheme or a single-floor exposure.
TIER_VAR  <- "wage_group"
FLOOR_VAR <- "real_minwage_hourly"
TIER_KEEP <- config$TIER_LEVELS

save_path <- file.path(config$paths$outputs, config$output_stage,
                       config$out_subdirs$exp_validation)
if (is.null(config$out_subdirs$exp_validation)) {
  stop("Add config$out_subdirs$exp_validation = 'Regression Results/",
       "Exposure Validation' to 00_config.R and re-source setup.")
}
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)

save_fig <- function(p, name, w = config$fig_defaults$width,
                     h = config$fig_defaults$height) {
  ggsave(file.path(save_path, paste0(name, ".", config$fig_defaults$format)),
         plot = p, width = w, height = h, dpi = config$fig_defaults$dpi)
  message("Saved: ", name)
}
save_tbl <- function(df, name) {
  path <- file.path(save_path, paste0(name, ".csv"))
  readr::write_csv(df, path); message("Saved: ", name, ".csv")
}


#===============================================================================
# STEP 1. Baseline frame + design (formal, baseline year, complete cases)
#
# Rebuild the SAME baseline_df 07A uses. Complete cases on income and floor are
# imposed here so the identity check is honest: if 07A silently dropped rows
# that firmsize_pi kept, that will show up in check A.
#===============================================================================

svy <- function(df) svydesign(id = ~psu_unique, strata = ~strata_unique,
                              weights = ~FACTOR_EXPANSION, data = df, nest = TRUE)


baseline_df <- samples$reg_tier$data %>%
  filter(Employment_Status == "Formal", year == BASE_YEAR,
         .data[[TIER_VAR]] %in% TIER_KEEP,
         !is.na(.data[[GEO]]),
         !is.na(.data[[INCOME_VAR]]),
         !is.na(.data[[FLOOR_VAR]])) %>%
  mutate(near_min = as.integer(
    .data[[INCOME_VAR]] >= BAND_LOWER * .data[[FLOOR_VAR]] &
      .data[[INCOME_VAR]] <= BAND_UPPER * .data[[FLOOR_VAR]]
  ),
  below_min_ind = as.integer(
    .data[[INCOME_VAR]] < BAND_LOWER * .data[[FLOOR_VAR]]
  ),
  log_hwage = log(.data[[INCOME_VAR]]))

des <- svy(baseline_df)

exposure_geo <- readRDS(tagged_rds(pd, "exposure_geo"))

cat(sprintf("[07C] baseline rows: %d | geo units: %d | GEO=%s\n",
            nrow(baseline_df), dplyr::n_distinct(baseline_df[[GEO]]), GEO))


#===============================================================================
# STEP 2. Check A - identity: weighted aggregate = ungrouped geo-level share
#===============================================================================

cat("[07C-A] Identity check (ungrouped share vs 07A weighted aggregate)...\n")

by_geo <- stats::as.formula(paste0("~", GEO))
ung <- svyby(~near_min, by_geo, design = des, FUN = svymean, na.rm = TRUE) %>%
  tibble::as_tibble() %>%
  rename(ungrouped = near_min, se_ungrouped = se)

identity_check <- exposure_geo %>%
  select(all_of(c(GEO, "exposure_geo_val"))) %>%
  left_join(ung, by = GEO) %>%
  mutate(diff = ungrouped - exposure_geo_val)

max_diff <- max(abs(identity_check$diff), na.rm = TRUE)
cat(sprintf("  max |diff| = %.2e (expect ~1e-15)\n", max_diff))
if (max_diff > 1e-10) {
  warning("Identity check failed: 07A near_mw_share and firmsize_pi are on ",
          "different populations. Fix filter alignment in 07A.")
}

# Fig A
fig_A <- ggplot(identity_check,
                aes(x = ungrouped, y = exposure_geo_val)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_point(size = 2.4) +
  labs(title = "Identity Check: Weighted Aggregate = Ungrouped Share",
       subtitle = sprintf("Max discrepancy: %.2e (n = %d %s units)",
                          max_diff, nrow(identity_check), GEO),
       x = "Ungrouped survey-weighted share (single call)",
       y = "07A tier-weighted aggregate") +
  theme_surveytools()
save_fig(fig_A, "fig_EXP_A_identity")


#===============================================================================
# STEP 3. Check B - sampling noise, reliability, attenuation
#
# Region-level SEs come from the same svyby that produced the ungrouped estimate
# in check A, so they are the exact variance of the design's point estimate of
# exposure -- not a formula-based approximation.
#
# lambda = 1 - E[SE^2] / Var(exposure) is the classical reliability ratio for a
# noisily measured regressor. 1 - lambda bounds the attenuation on beta if
# exposure is measured with classical error.
#===============================================================================

cat("[07C-B] Reliability and attenuation bound...\n")

n_by_geo <- baseline_df %>%
  group_by(across(all_of(GEO))) %>%
  summarise(n_obs = dplyr::n(),
            n_psu = dplyr::n_distinct(psu_unique), .groups = "drop")

region_tbl <- identity_check %>%
  select(all_of(c(GEO, "exposure_geo_val", "se_ungrouped"))) %>%
  left_join(n_by_geo, by = GEO) %>%
  mutate(ci_lo = pmax(0, exposure_geo_val - 1.96 * se_ungrouped),
         ci_hi = exposure_geo_val + 1.96 * se_ungrouped) %>%
  arrange(exposure_geo_val)

var_true    <- var(region_tbl$exposure_geo_val)
mean_var_se <- mean(region_tbl$se_ungrouped^2)
lambda      <- 1 - mean_var_se / var_true

cat(sprintf("  Var(exposure) = %.5f | mean SE^2 = %.5f | lambda = %.3f\n",
            var_true, mean_var_se, lambda))
cat(sprintf("  Implied attenuation ceiling on beta: up to %.1f%% toward zero\n",
            100 * (1 - lambda)))


#===============================================================================
# STEP 4. Check C - ranking-vs-noise plot (region exposure with 95% CI, by n)
#===============================================================================

cat("[07C-C] Region exposure with 95% CIs, ordered by n...\n")

region_by_n <- region_tbl %>%
  arrange(n_obs) %>%
  mutate(geo_label = factor(.data[[GEO]], levels = .data[[GEO]]))

fig_C <- ggplot(region_by_n,
                aes(x = geo_label, y = exposure_geo_val)) +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi), size = 0.4) +
  geom_text(aes(label = sprintf("n=%d", n_obs)),
            vjust = -0.8, hjust = 0, angle = 45, size = 2.7, colour = "grey40") +
  labs(title = "Baseline Exposure by Region, with Survey 95% CIs",
       subtitle = sprintf(
         "Ordered by sample size (leftmost = thinnest). Reliability lambda = %.2f.",
         lambda),
       x = NULL, y = "Exposure (share near tier MW)",
       caption = "If min/max regions have small n, the extremes driving beta are largely noise.") +
  theme_surveytools() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_fig(fig_C, "fig_EXP_C_ranking",
         w = config$fig_defaults$width * 1.2,
         h = config$fig_defaults$height * 1.1)

save_tbl(region_tbl, "tbl_EXP_region")

#===============================================================================
# STEP 5. Check D - baseline covariate correlations (outcome-matched)
#
# One baseline covariate per outcome family, computed on the same frame the
# outcome uses in 08.  A high correlation flags that the treatment shares
# baseline variation with the pre-period level of the outcome - which means
# mean-reversion / heterogeneous secular trends will show up as spurious
# treatment effects.
#
#   Outcome family (08)              Baseline covariate here          Frame
#   ---------------------------------------------------------------------------
#   log_var_hwage_total              median_lhw_total                 reg_variance
#   log_var_hwage_formal             median_lhw_formal                reg_variance | formal
#   log_var_hwage_informal           median_lhw_informal              reg_variance | informal
#   informal_share                   formal_share                     reg_shares
#   selfemp_share                    selfemp_share (baseline)         reg_shares
#   below_min_share                  below_min_share (baseline)       reg_tier | formal
#
# Monthly variance outcomes (log_var_mwage_*) inherit the same median-wage
# covariate as their hourly analog (median of log wage doesn't depend on the
# time-unit, only its variance does), so no extra covariates for those.
#===============================================================================

cat("[07C-D] Baseline covariate correlations (outcome-matched)...\n")

# Baseline frames at BASE_YEAR
base_variance <- samples$reg_variance$data %>%
  filter(year == BASE_YEAR, !is.na(.data[[GEO]]))
base_shares   <- samples$reg_shares$data %>%
  filter(year == BASE_YEAR, !is.na(.data[[GEO]]))

des_var_tot_b <- svy(base_variance)
des_var_frm_b <- svy(base_variance %>% filter(Employment_Status == "Formal"))
des_var_inf_b <- svy(base_variance %>% filter(Employment_Status == "Informal"))
des_shr_b     <- svy(base_shares)
# des for the below-min baseline is already built as `des` in Step 1 (formal
# workers with known tier at BASE_YEAR).

# --- share outcomes ---
formal_tbl <- svyby(~is_informal, by_geo, des_shr_b, svymean, na.rm = TRUE) %>%
  tibble::as_tibble() %>%
  transmute(!!GEO := .data[[GEO]], formal_share = 1 - is_informal)

selfemp_tbl <- svyby(~is_selfemp, by_geo, des_shr_b, svymean, na.rm = TRUE) %>%
  tibble::as_tibble() %>%
  transmute(!!GEO := .data[[GEO]], selfemp_share = is_selfemp)

# --- compliance outcome (formal, tier known - reuses des from Step 1) ---
below_tbl <- svyby(~below_min_ind, by_geo, design = des, FUN = svymean,
                   na.rm = TRUE) %>%
  tibble::as_tibble() %>%
  transmute(!!GEO := .data[[GEO]], below_min = below_min_ind)

# --- median log wage on three variance populations ---
# svyquantile fails inside svyby in survey 4.1; loop by region.
geo_levels <- sort(unique(baseline_df[[GEO]]))
median_lhw <- function(des_in) {
  vapply(geo_levels, function(g) {
    sub <- des_in[des_in$variables[[GEO]] == g, ]
    if (nrow(sub$variables) == 0L) return(NA_real_)
    q <- survey::svyquantile(~log_hwage, sub, quantiles = 0.5,
                             ci = FALSE, na.rm = TRUE)
    as.numeric(unlist(q))[1]
  }, numeric(1))
}
med_tbl <- tibble::tibble(
  !!GEO             := geo_levels,
  median_lhw_total   = median_lhw(des_var_tot_b),
  median_lhw_formal  = median_lhw(des_var_frm_b),
  median_lhw_informal = median_lhw(des_var_inf_b)
)

# Assemble
cov_tbl <- exposure_geo %>%
  select(all_of(c(GEO, "exposure_geo_val"))) %>%
  left_join(below_tbl,    by = GEO) %>%
  left_join(formal_tbl,   by = GEO) %>%
  left_join(selfemp_tbl,  by = GEO) %>%
  left_join(med_tbl,      by = GEO)

cov_vars <- c("below_min", "formal_share", "selfemp_share",
              "median_lhw_total", "median_lhw_formal", "median_lhw_informal")
cor_vec  <- vapply(cov_vars, function(v)
  cor(cov_tbl$exposure_geo_val, cov_tbl[[v]], use = "complete.obs"),
  numeric(1))

cat("  Pearson corr with exposure:\n")
for (v in cov_vars) cat(sprintf("    %-22s %+.3f\n", v, cor_vec[v]))

cov_long <- cov_tbl %>%
  tidyr::pivot_longer(all_of(cov_vars), names_to = "covariate",
                      values_to = "value") %>%
  mutate(covariate = factor(covariate, levels = cov_vars,
                            labels = c(
                              sprintf("Below-min share (formal, tier)  r = %+.2f", cor_vec["below_min"]),
                              sprintf("Formal share (all employed)     r = %+.2f", cor_vec["formal_share"]),
                              sprintf("Self-emp share (all employed)   r = %+.2f", cor_vec["selfemp_share"]),
                              sprintf("Median log hwage - total        r = %+.2f", cor_vec["median_lhw_total"]),
                              sprintf("Median log hwage - formal       r = %+.2f", cor_vec["median_lhw_formal"]),
                              sprintf("Median log hwage - informal     r = %+.2f", cor_vec["median_lhw_informal"])
                            )))

fig_D <- ggplot(cov_long, aes(x = value, y = exposure_geo_val)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey60",
              linewidth = 0.4, formula = y ~ x) +
  geom_point(size = 2) +
  facet_wrap(~covariate, scales = "free_x", ncol = 3) +
  labs(title = "Baseline Covariates vs Exposure (one per outcome family)",
       subtitle = sprintf("%d %s units, %d baseline.",
                          nrow(cov_tbl), GEO, BASE_YEAR),
       x = NULL, y = "Exposure (share near tier MW)",
       caption = paste(
         "Each covariate is the baseline level of a regression outcome in 08.",
         "|r| >= 0.5 warrants an outcome-specific pre-trend / control robustness.",
         sep = "\n")) +
  theme_surveytools()
save_fig(fig_D, "fig_EXP_D_covariates",
         w = config$fig_defaults$width * 1.6,
         h = config$fig_defaults$height * 1.4)

# Save the numeric correlations for the diagnostics table
cor_df <- tibble::tibble(
  check     = paste0("D_", cov_vars),
  statistic = "pearson_corr",
  value     = as.numeric(cor_vec[cov_vars]),
  pass_threshold = "|.| < 0.5 for substantive outcomes"
)


#===============================================================================
# STEP 6. Check E - band-width sensitivity (coefficient-relevant statistics)
#
# The estimator uses continuous exposure, not ranks. Spearman is kept for
# comparability with 07A's old table, but sd(exposure_at_band) and Pearson vs
# default are the statistics that predict how beta will move if the band is
# changed. pi is band-invariant so weights are reused.
#===============================================================================

cat("[07C-E] Band-width sensitivity...\n")

pi_tbl <- firmsize_pi(df = baseline_df, time_var = "year",
                      by_vars = c(GEO, TIER_VAR), size_var = TIER_VAR,
                      formal_only = FALSE)

band_sensitivity <- purrr::map_dfr(BAND_GRID, function(ub) {
  near_mw_share(df = baseline_df, time_var = "year",
                by_vars = c(GEO, TIER_VAR), min_wage = FLOOR_VAR,
                income = INCOME_VAR, out_col = "near_min",
                mw_lower = BAND_LOWER, mw_upper = ub, formal_only = FALSE) %>%
    weighted_exposure(pi_tbl, "year", GEO, TIER_VAR,
                      "near_min", "pi", "exposure_geo_val") %>%
    mutate(band_upper = ub)
})

ref <- band_sensitivity %>% filter(band_upper == BAND_UPPER) %>%
  select(all_of(GEO), ref_val = exposure_geo_val)

band_summary <- band_sensitivity %>%
  left_join(ref, by = GEO) %>%
  group_by(band_upper) %>%
  summarise(sd_exposure = sd(exposure_geo_val),
            pearson_vs_default  = cor(exposure_geo_val, ref_val, method = "pearson"),
            spearman_vs_default = cor(exposure_geo_val, ref_val, method = "spearman"),
            .groups = "drop")

cat("  Band sensitivity summary:\n"); print(band_summary)

save_tbl(band_summary, "tbl_EXP_band_sensitivity")

fig_E <- band_sensitivity %>%
  mutate(band_label = paste0("Band = [", sprintf("%.2f", BAND_LOWER), ", ",
                             sprintf("%.2f", band_upper), "]"),
         band_label = factor(band_label,
                             levels = paste0("Band = [", sprintf("%.2f", BAND_LOWER), ", ",
                                             sprintf("%.2f", BAND_GRID), "]"))) %>%
  ggplot(aes(x = reorder(.data[[GEO]], exposure_geo_val),
             y = exposure_geo_val, colour = band_label, group = band_label)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 1.8) +
  labs(title = "Exposure by Region Across Band Widths",
       subtitle = sprintf("Regions ordered by exposure at default band [%.2f, %.2f].",
                          BAND_LOWER, BAND_UPPER),
       x = NULL, y = "Exposure (share near tier MW)",
       colour = NULL,
       caption = paste(
         "If lines are near-parallel, the band choice is a normalization.",
         "If they cross, the band does substantive work and needs defending.",
         sep = "\n")) +
  theme_surveytools() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_fig(fig_E, "fig_EXP_E_band",
         w = config$fig_defaults$width * 1.3,
         h = config$fig_defaults$height * 1.1)


#===============================================================================
# STEP 7. Fig B (reliability bar) + master diagnostics table
#===============================================================================

fig_B <- tibble::tibble(metric = c("Var(exposure)", "mean SE^2", "Var - noise"),
                        value = c(var_true, mean_var_se, var_true - mean_var_se)) %>%
  ggplot(aes(x = metric, y = value)) +
  geom_col(fill = "#4575b4", width = 0.6) +
  geom_text(aes(label = sprintf("%.5f", value)), vjust = -0.4, size = 3.2) +
  labs(title = "Signal vs Noise in Baseline Exposure",
       subtitle = sprintf("Reliability lambda = %.3f  (attenuation ceiling %.1f%%)",
                          lambda, 100 * (1 - lambda)),
       x = NULL, y = "Variance across regions",
       caption = "Left = total variance of region exposures. Middle = mean sampling variance. Right = 'signal'.") +
  theme_surveytools()
save_fig(fig_B, "fig_EXP_B_reliability")

diagnostics_tbl <- tibble::tribble(
  ~check,           ~statistic,            ~value,     ~pass_threshold,
  "A_identity",     "max_abs_diff",        max_diff,   "< 1e-10",
  "B_reliability",  "lambda",              lambda,     "> 0.7 acceptable; > 0.9 strong",
  "B_reliability",  "attenuation_ceiling", 1 - lambda, "smaller is better"
) %>%
  bind_rows(cor_df)

save_tbl(diagnostics_tbl, "tbl_EXP_diagnostics")


cat("\n=== 07C_exposure validation.R complete ===\n")
cat("Outputs saved to:\n  ", save_path, "\n\n")

