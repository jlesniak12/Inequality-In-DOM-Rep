#===============================================================================
#
# Script: 07C_exposure_validation.R   [REVISED]
#
# Purpose: Validate the exposure treatment produced by 07A. 07B answers "which
#          geography?"; 07C answers "does the exposure measure identify what we
#          think it does?"
#
# Parameterized by config$active_baseline and config$active_income (same axes
# as 07A / 08 / 08B / 09). Reads the tagged exposure file for the active combo
# and writes diagnostics into a matching folder tree under exp_validation.
#
# The five checks:
#
#   A. Identity check. Because pi is built from the same weighted counts as the
#      within-tier rates, the tier-weighted aggregate must equal the ungrouped
#      geo-level share to numerical precision (multi-tier baselines) or
#      trivially (single-tier baselines). A non-zero discrepancy in the
#      multi-tier case is evidence of a filter mismatch in 07A.
#
#   B. Sampling noise and reliability. Sd(exposure) across regions is not
#      informative without the survey SE. Reliability ratio
#      lambda = 1 - mean(SE^2) / Var(exposure) is the attenuation bound on the
#      first-stage coefficient. Below ~0.7 is worrying; above ~0.9 is fine.
#
#   C. Ranking-vs-noise plot. Exposure by region with 95% CIs, ordered by n.
#      If min and max regions are the thin ones, the extremes are largely noise.
#
#   D. Baseline covariate correlations. Three concerns:
#       - below-min share (mechanical / same denominator as exposure)
#       - formal share (exposure = "poverty of formality" proxy?)
#       - median wage (exposure = "poor region" proxy?)
#
#   E. Band-width sensitivity. Coefficient-relevant stats (sd, Pearson) plus
#      Spearman rank correlation across bands in config$exposure$mw_band_upper_grid.
#
# READS   samples$reg_tier / reg_variance / reg_shares (via 03)
#         exposure_geo file for active (income, baseline) (via mw_file)
#
# WRITES  <exp_validation>/<income>/<baseline>/
#           tbl_EXP_diagnostics.csv
#           tbl_EXP_region.csv
#           tbl_EXP_band_sensitivity.csv
#           fig_EXP_A_identity.png
#           fig_EXP_B_reliability.png
#           fig_EXP_C_ranking.png
#           fig_EXP_D_covariates.png
#           fig_EXP_E_band.png
#
#===============================================================================

if (!exists("config", envir = .GlobalEnv, inherits = FALSE)) {
  source(here::here("Code", "R", "clean scripts", "00_setup.R"))
} else {
  cat("[07C] Reusing existing `config` (00_setup not re-sourced)\n")
}

if (!exists("samples", envir = .GlobalEnv, inherits = FALSE) ||
    is.null(samples$reg_tier$data)) {
  source(file.path(config$paths$scripts, "03_sample definitions.R"))
} else {
  cat("[07C] Reusing existing `samples` (03 not re-sourced)\n")
}


#===============================================================================
# STEP 0. Parameters and paths
#===============================================================================

cat("=== 07C_exposure_validation.R ===\n\n")

BL <- config$baselines[[config$active_baseline]]
IS <- config$income_specs[[config$active_income]]

GEO        <- config$exposure$construct_geo
TIER_VAR   <- "wage_group"
INCOME_VAR <- IS$income
FLOOR_VAR  <- IS$minwage
BAND_LOWER <- 1 - config$exposure$mw_compliance_tolerance
BAND_UPPER <- config$exposure$mw_band_upper
BAND_GRID  <- config$exposure$mw_band_upper_grid

income_word <- if (IS$log_var_prefix == "log_var_hwage") "hourly" else "monthly"
BL_LABEL <- BL$label

# NEW:
in_dir_exp <- config$data_dirs$exposure
out_dir <- file.path(config$out_dirs$exp_validation,
                     config$active_income, config$active_baseline, GEO)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

save_fig <- function(p, name,
                     w = config$fig_defaults$width,
                     h = config$fig_defaults$height) {
  ggsave(file.path(out_dir, paste0(name, ".", config$fig_defaults$format)),
         plot = p, width = w, height = h, dpi = config$fig_defaults$dpi)
  message("Saved: ", name)
}
save_tbl <- function(df, name) {
  path <- file.path(out_dir, paste0(name, ".csv"))
  readr::write_csv(df, path); message("Saved: ", name, ".csv")
}

cat(sprintf("  income=%s | baseline=%s | geo=%s\n",
            config$active_income, config$active_baseline, GEO))
cat(sprintf("  reading exposure from: %s\n", in_dir_exp))
cat(sprintf("  writing to:            %s\n", out_dir))


#===============================================================================
# STEP 1. Baseline frame + design + incoming-floor override
#
# Mirror 07A's baseline_df construction so the identity check (A) is honest.
# For "incoming" baselines, add a constant `floor_incoming` column and use it
# as the reference floor in near_min / below_min_ind. This makes exposure and
# non-compliance both refer to the SAME (incoming) floor - the mechanical
# concern in check D then has its correct denominator.
#===============================================================================

svy <- function(df) svydesign(id = ~psu_unique, strata = ~strata_unique,
                              weights = ~FACTOR_EXPANSION, data = df,
                              nest = TRUE)

tiers_keep <- if (identical(BL$tiers, "all")) config$TIER_LEVELS else BL$tiers

period_expr <- if (BL$period$type == "year") {
  rlang::expr(year == !!BL$period$value)
} else {
  rlang::expr(year_quarter == !!BL$period$value)
}

baseline_df <- samples$reg_tier$data %>%
  filter(Employment_Status == "Formal",
         !!period_expr,
         .data[[TIER_VAR]] %in% tiers_keep,
         !is.na(.data[[GEO]]),
         !is.na(.data[[INCOME_VAR]])) %>%
  mutate(baseline_dummy = paste0(config$active_baseline, IS$tag))

# Floor override for incoming-floor baselines (mirrors 07A STEP 2).
if (BL$floor$source == "incoming") {
  target_yq <- BL$floor$qtr
  incoming_val <- samples$reg_tier$data %>%
    filter(year_quarter == target_yq, wage_group == BL$floor$tier) %>%
    pull(!!sym(FLOOR_VAR)) %>% first()
  if (is.na(incoming_val)) {
    stop("Incoming floor lookup returned NA for ", target_yq, " / ",
         BL$floor$tier, " on column ", FLOOR_VAR)
  }
  baseline_df$floor_incoming <- incoming_val
  FLOOR_VAR_USE <- "floor_incoming"
} else {
  baseline_df <- baseline_df %>% filter(!is.na(.data[[FLOOR_VAR]]))
  FLOOR_VAR_USE <- FLOOR_VAR
}

baseline_df <- baseline_df %>%
  mutate(
    near_min = as.integer(
      .data[[INCOME_VAR]] >= BAND_LOWER * .data[[FLOOR_VAR_USE]] &
        .data[[INCOME_VAR]] <= BAND_UPPER * .data[[FLOOR_VAR_USE]]
    ),
    below_min_ind = as.integer(
      .data[[INCOME_VAR]] < BAND_LOWER * .data[[FLOOR_VAR_USE]]
    ),
    log_income = log(.data[[INCOME_VAR]])
  )

des <- svy(baseline_df)

# Load the tagged exposure file.
exp_file <- mw_file("exposure_geo", dir = in_dir_exp)
if (!file.exists(exp_file)) {
  stop("Missing exposure file: ", exp_file,
       "\nRun 07A for the current (income, baseline) first.")
}
exposure_geo <- readRDS(exp_file)

cat(sprintf("[07C] baseline rows: %d | geo units: %d | tiers kept: %s\n",
            nrow(baseline_df), dplyr::n_distinct(baseline_df[[GEO]]),
            paste(tiers_keep, collapse = "/")))


#===============================================================================
# STEP 2. Check A - identity: weighted aggregate = ungrouped geo-level share
#
# For single-tier baselines, the identity is trivial by construction (both are
# near_mw_share on the same population). Still useful as a sanity check.
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

fig_A <- ggplot(identity_check,
                aes(x = ungrouped, y = exposure_geo_val)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_point(size = 2.4) +
  labs(title = "Identity Check: Weighted Aggregate = Ungrouped Share",
       subtitle = sprintf("Baseline: %s | Max discrepancy: %.2e (n = %d %s units)",
                          BL_LABEL, max_diff, nrow(identity_check), GEO),
       x = "Ungrouped survey-weighted share (single call)",
       y = "07A tier-weighted aggregate") +
  theme_surveytools()
save_fig(fig_A, "fig_EXP_A_identity")


#===============================================================================
# STEP 3. Check B - reliability and attenuation
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
# STEP 4. Check C - ranking-vs-noise plot
#===============================================================================

cat("[07C-C] Region exposure with 95% CIs, ordered by n...\n")

region_by_n <- region_tbl %>%
  arrange(n_obs) %>%
  mutate(geo_label = factor(.data[[GEO]], levels = .data[[GEO]]))

fig_C <- ggplot(region_by_n,
                aes(x = geo_label, y = exposure_geo_val)) +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi), size = 0.4) +
  geom_text(aes(label = sprintf("n=%d", n_obs)),
            vjust = -0.8, hjust = 0, angle = 45, size = 2.7,
            colour = "grey40") +
  labs(title = "Baseline Exposure by Region, with Survey 95% CIs",
       subtitle = sprintf(
         "%s | Ordered by sample size (leftmost = thinnest) | Reliability lambda = %.2f",
         BL_LABEL, lambda),
       x = NULL, y = "Exposure (share near tier MW)",
       caption = "If min/max regions have small n, the extremes driving beta are largely noise.") +
  theme_surveytools() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_fig(fig_C, "fig_EXP_C_ranking",
         w = config$fig_defaults$width * 1.2,
         h = config$fig_defaults$height * 1.1)

save_tbl(region_tbl, "tbl_EXP_region")


#===============================================================================
# STEP 5. Check D - baseline covariate correlations
#
# Same idea as before, but baseline period follows BL$period and log-wage
# median follows the active income concept. `below_min` uses FLOOR_VAR_USE so
# the incoming-floor case measures share below the incoming floor (matches how
# exposure is constructed).
#===============================================================================

cat("[07C-D] Baseline covariate correlations (outcome-matched)...\n")

# Baseline period filter (mirrors 07A / 08).
baseline_period_filter <- function(df) {
  if (BL$period$type == "year") {
    df %>% filter(year == BL$period$value)
  } else {
    df %>% filter(year_quarter == BL$period$value)
  }
}

# Baseline frames.
base_variance <- samples$reg_variance$data %>%
  baseline_period_filter() %>%
  filter(!is.na(.data[[GEO]]), !is.na(.data[[INCOME_VAR]])) %>%
  mutate(log_income = log(.data[[INCOME_VAR]]))
base_shares <- samples$reg_shares$data %>%
  baseline_period_filter() %>%
  filter(!is.na(.data[[GEO]]))

des_var_tot_b <- svy(base_variance)
des_var_frm_b <- svy(base_variance %>% filter(Employment_Status == "Formal"))
des_var_inf_b <- svy(base_variance %>% filter(Employment_Status == "Informal"))
des_shr_b     <- svy(base_shares)

# Share outcomes.
formal_tbl <- svyby(~is_informal, by_geo, des_shr_b, svymean, na.rm = TRUE) %>%
  tibble::as_tibble() %>%
  transmute(!!GEO := .data[[GEO]], formal_share = 1 - is_informal)

selfemp_tbl <- svyby(~is_selfemp, by_geo, des_shr_b, svymean, na.rm = TRUE) %>%
  tibble::as_tibble() %>%
  transmute(!!GEO := .data[[GEO]], selfemp_share = is_selfemp)

# Compliance outcome (baseline_df's formal + tier population).
below_tbl <- svyby(~below_min_ind, by_geo, design = des, FUN = svymean,
                   na.rm = TRUE) %>%
  tibble::as_tibble() %>%
  transmute(!!GEO := .data[[GEO]], below_min = below_min_ind)

# Median log wage on three variance populations (svyquantile fails inside
# svyby in survey 4.1; loop by region).
geo_levels <- sort(unique(baseline_df[[GEO]]))
median_lwage <- function(des_in) {
  vapply(geo_levels, function(g) {
    sub <- des_in[des_in$variables[[GEO]] == g, ]
    if (nrow(sub$variables) == 0L) return(NA_real_)
    q <- survey::svyquantile(~log_income, sub, quantiles = 0.5,
                             ci = FALSE, na.rm = TRUE)
    as.numeric(unlist(q))[1]
  }, numeric(1))
}
med_tbl <- tibble::tibble(
  !!GEO             := geo_levels,
  median_lwage_total    = median_lwage(des_var_tot_b),
  median_lwage_formal   = median_lwage(des_var_frm_b),
  median_lwage_informal = median_lwage(des_var_inf_b)
)

# Assemble
cov_tbl <- exposure_geo %>%
  select(all_of(c(GEO, "exposure_geo_val"))) %>%
  left_join(below_tbl,   by = GEO) %>%
  left_join(formal_tbl,  by = GEO) %>%
  left_join(selfemp_tbl, by = GEO) %>%
  left_join(med_tbl,     by = GEO)

cov_vars <- c("below_min", "formal_share", "selfemp_share",
              "median_lwage_total", "median_lwage_formal",
              "median_lwage_informal")
cor_vec  <- vapply(cov_vars, function(v)
  cor(cov_tbl$exposure_geo_val, cov_tbl[[v]], use = "complete.obs"),
  numeric(1))

cat("  Pearson corr with exposure:\n")
for (v in cov_vars) cat(sprintf("    %-24s %+.3f\n", v, cor_vec[v]))

cov_long <- cov_tbl %>%
  tidyr::pivot_longer(all_of(cov_vars), names_to = "covariate",
                      values_to = "value") %>%
  mutate(covariate = factor(covariate, levels = cov_vars,
                            labels = c(
                              sprintf("Below-min share (formal, tier)      r = %+.2f", cor_vec["below_min"]),
                              sprintf("Formal share (all employed)         r = %+.2f", cor_vec["formal_share"]),
                              sprintf("Self-emp share (all employed)       r = %+.2f", cor_vec["selfemp_share"]),
                              sprintf("Median log %s wage - total     r = %+.2f", income_word, cor_vec["median_lwage_total"]),
                              sprintf("Median log %s wage - formal    r = %+.2f", income_word, cor_vec["median_lwage_formal"]),
                              sprintf("Median log %s wage - informal  r = %+.2f", income_word, cor_vec["median_lwage_informal"])
                            )))

fig_D <- ggplot(cov_long, aes(x = value, y = exposure_geo_val)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey60",
              linewidth = 0.4, formula = y ~ x) +
  geom_point(size = 2) +
  facet_wrap(~covariate, scales = "free_x", ncol = 3) +
  labs(title = "Baseline Covariates vs Exposure (one per outcome family)",
       subtitle = sprintf("%s | %s income | %d %s units",
                          BL_LABEL, income_word, nrow(cov_tbl), GEO),
       x = NULL, y = "Exposure (share near MW)",
       caption = paste(
         "Each covariate is the baseline level of a regression outcome in 08.",
         "|r| >= 0.5 warrants an outcome-specific pre-trend / control robustness.",
         sep = "\n")) +
  theme_surveytools()
save_fig(fig_D, "fig_EXP_D_covariates",
         w = config$fig_defaults$width * 1.6,
         h = config$fig_defaults$height * 1.4)

cor_df <- tibble::tibble(
  check     = paste0("D_", cov_vars),
  statistic = "pearson_corr",
  value     = as.numeric(cor_vec[cov_vars]),
  pass_threshold = "|.| < 0.5 for substantive outcomes"
)


#===============================================================================
# STEP 6. Check E - band-width sensitivity
#
# Same idiom for both baselines. For single-tier baselines pi_tbl collapses to
# 1 per region and weighted_exposure returns near_min unchanged; multi-tier
# baselines get the tier-weighted aggregate. Uses FLOOR_VAR_USE so the
# incoming-floor case varies the band around the incoming floor.
#===============================================================================

cat("[07C-E] Band-width sensitivity...\n")

# Trivial pi (1) for single-tier, real pi for multi-tier.
if (isTRUE(BL$weight_tiers)) {
  pi_tbl <- firmsize_pi(df = baseline_df, time_var = "baseline_dummy",
                        by_vars = c(GEO, TIER_VAR), size_var = TIER_VAR,
                        formal_only = FALSE)
} else {
  # Build a minimal pi = 1 frame shaped like firmsize_pi's output, so
  # weighted_exposure runs identically.
  pi_tbl <- baseline_df %>%
    distinct(baseline_dummy, !!GEO := .data[[GEO]],
             !!TIER_VAR := .data[[TIER_VAR]]) %>%
    mutate(pi = 1)
}

band_sensitivity <- purrr::map_dfr(BAND_GRID, function(ub) {
  near_mw_share(df = baseline_df, time_var = "baseline_dummy",
                by_vars = c(GEO, TIER_VAR), min_wage = FLOOR_VAR_USE,
                income = INCOME_VAR, out_col = "near_min",
                mw_lower = BAND_LOWER, mw_upper = ub,
                formal_only = FALSE) %>%
    weighted_exposure(pi_tbl, "baseline_dummy", GEO, TIER_VAR,
                      "near_min", "pi", "exposure_geo_val") %>%
    mutate(band_upper = ub)
})

ref <- band_sensitivity %>% filter(band_upper == BAND_UPPER) %>%
  select(all_of(GEO), ref_val = exposure_geo_val)

band_summary <- band_sensitivity %>%
  left_join(ref, by = GEO) %>%
  group_by(band_upper) %>%
  summarise(sd_exposure         = sd(exposure_geo_val),
            pearson_vs_default  = cor(exposure_geo_val, ref_val, method = "pearson"),
            spearman_vs_default = cor(exposure_geo_val, ref_val, method = "spearman"),
            .groups = "drop")

cat("  Band sensitivity summary:\n"); print(band_summary)

save_tbl(band_summary, "tbl_EXP_band_sensitivity")

fig_E <- band_sensitivity %>%
  mutate(band_label = paste0("Band = [", sprintf("%.2f", BAND_LOWER), ", ",
                             sprintf("%.2f", band_upper), "]"),
         band_label = factor(band_label,
                             levels = paste0("Band = [", sprintf("%.2f", BAND_LOWER),
                                             ", ", sprintf("%.2f", BAND_GRID), "]"))) %>%
  ggplot(aes(x = reorder(.data[[GEO]], exposure_geo_val),
             y = exposure_geo_val, colour = band_label, group = band_label)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 1.8) +
  labs(title = "Exposure by Region Across Band Widths",
       subtitle = sprintf("%s | Regions ordered by exposure at default band [%.2f, %.2f]",
                          BL_LABEL, BAND_LOWER, BAND_UPPER),
       x = NULL, y = "Exposure (share near MW)",
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
# STEP 7. Fig B + master diagnostics table
#===============================================================================

fig_B <- tibble::tibble(metric = c("Var(exposure)", "mean SE^2", "Var - noise"),
                        value = c(var_true, mean_var_se,
                                  var_true - mean_var_se)) %>%
  ggplot(aes(x = metric, y = value)) +
  geom_col(fill = "#4575b4", width = 0.6) +
  geom_text(aes(label = sprintf("%.5f", value)),
            vjust = -0.4, size = 3.2) +
  labs(title = "Signal vs Noise in Baseline Exposure",
       subtitle = sprintf(
         "%s | Reliability lambda = %.3f  (attenuation ceiling %.1f%%)",
         BL_LABEL, lambda, 100 * (1 - lambda)),
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

cat("\n=== 07C_exposure_validation.R complete ===\n")
cat("Outputs saved to:\n  ", out_dir, "\n\n")