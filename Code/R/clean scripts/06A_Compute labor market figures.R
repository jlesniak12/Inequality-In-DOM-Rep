#===============================================================================
#
# Script: 06A_Compute_Labor_Market.R
#
# Purpose: Compute labor-market outcome statistics for the labor-market figures.
#          Saves RDS objects for 06B_Plot_Labor_Market.R. No plotting here.
#
# SURVEY CONVENTION:
#   Uses the `survey` package directly via the same tidy helpers as 04A/05A.
#   Output schema: year_quarter, (group), estimate, se, n_obs, sparse.
#
# FIGURES THIS SCRIPT FEEDS:
#   Fig LM-1  Employment-to-population ratio (EPOP) over time, by sex
#             → lm_epop.rds
#   Fig LM-2  Mean usual weekly hours over time (overall + by sex)
#             → lm_hours_trend.rds
#   Fig LM-3  Hours distribution: focal-year extract for density/histogram
#             → lm_hours_extract.rds
#   Fig LM-4  Real hourly wage growth at percentiles around each MW event
#             → lm_wage_growth_events.rds
#
# POPULATIONS:
#   EPOP        numerator OCUPADO==1, denominator working-age (15-64).
#               Computed as the survey-weighted mean of OCUPADO within the
#               working_age sample (so the denominator is the WAP).
#   Hours       employed workers with positive primary hours.
#   Wage growth formal private wage earners (positive real hourly base salary),
#               the same population as the inequality script.
#
# OUTPUTS → config$paths$processed_data / "Labor Market":
#   lm_epop.rds, lm_hours_trend.rds, lm_hours_extract.rds,
#   lm_wage_growth_events.rds
#
# READS: samples object (from 03_Sample_Definitions.R)
#
#===============================================================================

source("Code/R/setup/00_setup.R")
source("Code/R/clean scripts/03_Sample Definitions.R")

library(survey)

cat("=== 06A_Compute_Labor_Market.R ===\n\n")


#===============================================================================
# CONSTANTS + HELPERS (same tidy contract as 04A/05A)
#===============================================================================

MW_EVENT_QTR <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")
MIN_CELL_N   <- 30
HOURS_VAR    <- "hours_worked_primary"
WAGE_VAR     <- "real_salary_primary_hourly_base"
HOURS_FOCAL_YEARS <- c(2016L, 2024L)

out_dir <- file.path(config$paths$processed_data, "Labor Market")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
save_rds <- function(obj, name) {
  saveRDS(obj, file.path(out_dir, paste0(name, ".rds")))
  cat("  Saved:", name, "\n")
}

.n_by <- function(design, time_var, group_var = NULL) {
  grp <- c(time_var, group_var)
  design$variables %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(n_obs = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
}

.standardise_se <- function(df, keys) {
  known    <- c(keys, "estimate"); leftover <- setdiff(names(df), known)
  se_col <- NULL
  if ("se" %in% leftover) se_col <- "se" else {
    cand <- leftover[grepl("(^|\\.)se(\\.|$)|se$", leftover, ignore.case = TRUE)]
    cand <- setdiff(cand, c("ci_l","ci_u","ci.l","ci.u"))
    if (length(cand)) se_col <- cand[[1]]
  }
  if (is.null(se_col)) df$se <- NA_real_ else names(df)[names(df)==se_col] <- "se"
  df[, c(keys, "estimate", "se"), drop = FALSE]
}

svy_mean_by <- function(design, var, time_var, group_var = NULL, na_rm = TRUE) {
  if (isTRUE(na_rm)) {
    keep <- !is.na(design$variables[[var]]); design <- design[keep, ]
  }
  grp <- c(time_var, group_var)
  by_fml  <- stats::as.formula(paste0("~", paste(grp, collapse = "+")))
  var_fml <- stats::as.formula(paste0("~", var))
  est <- svyby(var_fml, by_fml, design, svymean,
               na.rm = na_rm, vartype = "se", keep.names = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(estimate = !!var) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
  est <- .standardise_se(est, keys = grp)
  est %>%
    dplyr::left_join(.n_by(design, time_var, group_var), by = grp) %>%
    dplyr::rename(year_quarter = !!time_var) %>%
    dplyr::mutate(sparse = n_obs < MIN_CELL_N)
}

svy_quantile_by <- function(design, var, time_var, group_var = NULL,
                            prob = 0.5, na_rm = TRUE) {
  if (isTRUE(na_rm)) {
    keep <- !is.na(design$variables[[var]]); design <- design[keep, ]
  }
  grp <- c(time_var, group_var)
  by_fml  <- stats::as.formula(paste0("~", paste(grp, collapse = "+")))
  var_fml <- stats::as.formula(paste0("~", var))
  est <- svyby(var_fml, by_fml, design, FUN = svyquantile,
               quantiles = prob, ci = TRUE, vartype = "se",
               keep.names = FALSE, na.rm = na_rm) %>%
    tibble::as_tibble()
  non_key <- setdiff(names(est), grp)
  se_ci   <- non_key[grepl("se|ci", non_key, ignore.case = TRUE)]
  est_col <- setdiff(non_key, se_ci)
  if (length(est_col) != 1)
    est_col <- if (var %in% non_key) var else setdiff(non_key, se_ci)[[1]]
  names(est)[names(est) == est_col] <- "estimate"
  est <- est %>% dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
  est <- .standardise_se(est, keys = grp)
  est %>%
    dplyr::left_join(.n_by(design, time_var, group_var), by = grp) %>%
    dplyr::rename(year_quarter = !!time_var) %>%
    dplyr::mutate(sparse = n_obs < MIN_CELL_N)
}


#===============================================================================
# STEP 1. LM-1: Employment-to-population ratio (EPOP) by sex
#
# Denominator = working-age population (samples$working_age). Numerator =
# OCUPADO == 1. EPOP = survey-weighted mean of OCUPADO within the WAP.
# Computed overall and by Sex.
#===============================================================================

cat("[1] EPOP by sex...\n")

design_wap <- samples$working_age$design

# Overall
epop_all <- svy_mean_by(design_wap, "OCUPADO", "year_quarter") %>%
  dplyr::mutate(Sex = "All")

# By sex
epop_sex <- svy_mean_by(design_wap, "OCUPADO", "year_quarter",
                        group_var = "Sex")

lm_epop <- dplyr::bind_rows(epop_all, epop_sex) %>%
  dplyr::rename(epop = estimate) %>%
  dplyr::mutate(Sex = factor(Sex, levels = c("All", "Male", "Female")))

cat(sprintf("  Quarters: %d | EPOP(all) range: [%.1f%%, %.1f%%]\n",
            dplyr::n_distinct(lm_epop$year_quarter),
            min(epop_all$estimate) * 100, max(epop_all$estimate) * 100))
save_rds(lm_epop, "lm_epop")


#===============================================================================
# STEP 2. LM-2: Mean usual weekly hours over time
#
# Employed workers with positive primary hours. Overall + by sex.
#===============================================================================

cat("[2] Mean weekly hours...\n")

design_emp <- subset(samples$employed$design,
                     !is.na(get(HOURS_VAR)) & get(HOURS_VAR) > 0)

hours_all <- svy_mean_by(design_emp, HOURS_VAR, "year_quarter") %>%
  dplyr::mutate(Sex = "All")
hours_sex <- svy_mean_by(design_emp, HOURS_VAR, "year_quarter",
                         group_var = "Sex")

lm_hours_trend <- dplyr::bind_rows(hours_all, hours_sex) %>%
  dplyr::rename(mean_hours = estimate) %>%
  dplyr::mutate(Sex = factor(Sex, levels = c("All", "Male", "Female")))

cat(sprintf("  Mean hours(all) range: [%.1f, %.1f]\n",
            min(hours_all$estimate), max(hours_all$estimate)))
save_rds(lm_hours_trend, "lm_hours_trend")


#===============================================================================
# STEP 3. LM-3: Hours distribution extract (focal years)
#
# Individual-level usual weekly hours for 2016 and 2024, employed workers.
# Weights normalised within year. Density/histogram built at plot time.
# Also flag the full-time threshold (44 hrs) for reference.
#===============================================================================

cat("[3] Hours distribution extract...\n")

lm_hours_extract <- design_emp$variables %>%
  dplyr::filter(year %in% HOURS_FOCAL_YEARS,
                !is.na(.data[[HOURS_VAR]]), .data[[HOURS_VAR]] > 0,
                .data[[HOURS_VAR]] <= 100) %>%   # drop implausible outliers
  dplyr::transmute(
    year, year_quarter,
    hours = .data[[HOURS_VAR]],
    FACTOR_EXPANSION,
    focal_year = factor(year, levels = HOURS_FOCAL_YEARS)
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
  dplyr::ungroup()

cat(sprintf("  Rows: %d\n", nrow(lm_hours_extract)))
save_rds(lm_hours_extract, "lm_hours_extract")


#===============================================================================
# STEP 3b. LM-3b: Hours distribution by FORMALITY (focal years)
#
# Individual-level usual weekly hours for the two focal years, private
# employees, split Formal vs Informal. Weights normalised within
# year x formality so each panel's bars sum to 1 within year.
#
# NB: NO hours restriction is applied here (we keep part-time and overtime).
# The whole point of this figure is to SHOW the shape of the distribution,
# including part-time prevalence, which a 40-48h restriction would erase.
#===============================================================================

cat("[3b] Hours distribution by formality (focal years)...\n")

FI_LEVELS <- c("Formal", "Informal")

# Private employees with positive primary hours and known formality.
design_emp_fi <- subset(
  samples$employed$design,
  Employment_Type   == "private employee" &
    Employment_Status %in% FI_LEVELS       &
    !is.na(get(HOURS_VAR)) & get(HOURS_VAR) > 0
)

# Defensive check: confirm the status variable behaves as expected.
.fi_levels_present <- sort(unique(as.character(design_emp_fi$variables$Employment_Status)))
if (!all(.fi_levels_present %in% FI_LEVELS)) {
  stop("Employment_Status has unexpected levels: ",
       paste(.fi_levels_present, collapse = ", "))
}

lm_hours_extract_fi <- design_emp_fi$variables %>%
  dplyr::filter(year %in% HOURS_FOCAL_YEARS,
                .data[[HOURS_VAR]] <= 100) %>%             # drop implausible outliers
  dplyr::transmute(
    year, year_quarter,
    hours             = .data[[HOURS_VAR]],
    Employment_Status = factor(Employment_Status, levels = FI_LEVELS),
    FACTOR_EXPANSION,
    focal_year        = factor(year, levels = HOURS_FOCAL_YEARS)
  ) %>%
  dplyr::group_by(year, Employment_Status) %>%
  dplyr::mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
  dplyr::ungroup()

cat(sprintf("  Rows: %d | by status: %s\n",
            nrow(lm_hours_extract_fi),
            paste(utils::capture.output(
              print(table(lm_hours_extract_fi$Employment_Status,
                          lm_hours_extract_fi$year))), collapse = " ")))
save_rds(lm_hours_extract_fi, "lm_hours_extract_fi")


#===============================================================================
# STEP 3c. LM-3c: Mean usual weekly hours over time, by FORMALITY
#
# Survey-weighted mean hours per quarter for Formal vs Informal private
# employees, with MW events / COVID marked at plot time. This is the direct
# analogue of the inequality time-series (formal vs informal lines over time):
# it shows whether the formal/informal hours gap moves around MW events.
#===============================================================================

cat("[3c] Mean weekly hours over time, by formality...\n")

lm_hours_trend_fi <- svy_mean_by(
  design_emp_fi, HOURS_VAR, "year_quarter",
  group_var = "Employment_Status"
) %>%
  dplyr::rename(mean_hours = estimate) %>%
  dplyr::mutate(Employment_Status = factor(Employment_Status, levels = FI_LEVELS))

cat(sprintf("  Quarters: %d | Formal range [%.1f, %.1f] | Informal range [%.1f, %.1f]\n",
            dplyr::n_distinct(lm_hours_trend_fi$year_quarter),
            min(lm_hours_trend_fi$mean_hours[lm_hours_trend_fi$Employment_Status == "Formal"]),
            max(lm_hours_trend_fi$mean_hours[lm_hours_trend_fi$Employment_Status == "Formal"]),
            min(lm_hours_trend_fi$mean_hours[lm_hours_trend_fi$Employment_Status == "Informal"]),
            max(lm_hours_trend_fi$mean_hours[lm_hours_trend_fi$Employment_Status == "Informal"])))
save_rds(lm_hours_trend_fi, "lm_hours_trend_fi")

#===============================================================================
# 06A — STEP 4 (REPLACEMENT v4): Real wage growth by percentile around MW events
#       Pooled PRE vs POST windows. Formal + Informal. Hourly. No hours band.
#
# WHY v4 (the "exact 0%" problem)
#   v3's diagnostic showed every cell well-populated (n ~ 1000-2500), NA=0,
#   sparse=0 — yet many bars were exactly 0.00%. Cause:
#     (a) WAGE HEAPING: workers cluster on round-number salaries and (heavily)
#         the minimum wage, so the wage distribution is a set of tall spikes.
#         A percentile that lands on a spike returns that spike's exact value.
#     (b) WITHIN-YEAR / ANNUAL DEFLATOR: every event/pre pair in the snapshot
#         design was the SAME calendar year, so a nominally stable spike maps to
#         the SAME real value in both quarters -> exactly 0% real growth.
#     Net: single-quarter, within-year percentile change is dominated by whether
#     a percentile stayed on the same spike (exact 0) or jumped (discrete jump),
#     not by the MW signal.
#
#   FIX: compare a POOLED WINDOW of quarters before the event vs a pooled window
#   after it. Pooling smooths the heaps/composition; the post window lets MW
#   pass-through occur (phase-in quarters fall inside it); and because the
#   windows span calendar years, the annual deflator no longer zeroes things.
#   The event quarter itself is EXCLUDED (partial exposure), consistent with the
#   regression design.
#
#   >> ALSO verify the deflator frequency in 01B/02A. If real wages are
#      annually deflated, quarterly real dynamics are flattened project-wide
#      (inequality series, Kaitz, etc.), not just here.
#===============================================================================

cat("[4] Wage growth: pooled PRE vs POST windows (formal + informal, hourly)...\n")

PRE_Q         <- 4L                  # quarters pooled BEFORE the event (excl. event qtr)
POST_Q        <- 4L                  # quarters pooled AFTER  the event (incl. phase-ins)
WG_MIN_CELL_N <- 100L                # pooled-window threshold for the sparsity flag
HRS_FLOOR     <- 0                   # set e.g. 10 to drop clear low-hours coding errors
FI_LEVELS     <- c("Formal", "Informal")
PROBS         <- c(0.10, 0.25, 0.50, 0.75, 0.90)
PCT_INT       <- as.integer(round(PROBS * 100))

# survey-weighted quantile (Hazen), tie-safe, vectorised over probs
wtd_quantile <- function(x, w, p) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0L) return(rep(NA_real_, length(p)))
  o  <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w); pn <- (cw - 0.5 * w) / sum(w)
  stats::approx(pn, x, xout = p, rule = 2, ties = "ordered")$y
}

# shift a "YYYYQq" label by k quarters
qshift <- function(qtr, k) {
  y <- as.integer(substr(qtr, 1, 4)); q <- as.integer(substr(qtr, 6, 6))
  idx <- y * 4L + (q - 1L) + k
  paste0(idx %/% 4L, "Q", idx %% 4L + 1L)
}

# window quarters for each event
win <- purrr::map(MW_EVENT_QTR, function(evt) {
  list(pre  = qshift(evt, -seq_len(PRE_Q)),
       post = qshift(evt,  seq_len(POST_Q)))
})
names(win) <- MW_EVENT_QTR
needed_qtrs <- unique(unlist(win))

# microdata: private employees w/ valid hourly wage, both formalities
base_fp <- samples$wage_earners$design$variables %>%
  dplyr::filter(
    Employment_Type   == "private employee",
    Employment_Status %in% FI_LEVELS,
    !is.na(hours_worked_primary), hours_worked_primary > HRS_FLOOR,
    !is.na(.data[[WAGE_VAR]]), .data[[WAGE_VAR]] > 0,
    year_quarter %in% needed_qtrs
  ) %>%
  dplyr::mutate(Employment_Status = as.character(Employment_Status))

# pooled weighted percentiles over a set of quarters
calc_pct <- function(df, qtrs) {
  d <- df %>% dplyr::filter(year_quarter %in% qtrs)
  tibble::tibble(pctile = PCT_INT,
                 value  = wtd_quantile(d[[WAGE_VAR]], d$FACTOR_EXPANSION, PROBS),
                 n_obs  = nrow(d))
}

lm_wage_growth_events <- purrr::map_dfr(MW_EVENT_QTR, function(evt) {
  w_pre  <- win[[evt]]$pre
  w_post <- win[[evt]]$post
  purrr::map_dfr(FI_LEVELS, function(fi) {
    d    <- base_fp %>% dplyr::filter(Employment_Status == fi)
    pre  <- calc_pct(d, w_pre)  %>% dplyr::rename(value_pre = value, n_pre = n_obs)
    post <- calc_pct(d, w_post) %>% dplyr::rename(value_evt = value, n_evt = n_obs)
    pre %>%
      dplyr::left_join(post, by = "pctile") %>%
      dplyr::mutate(Employment_Status = fi, event = evt,
                    pre_window  = paste(range(w_pre),  collapse = "-"),
                    post_window = paste(range(w_post), collapse = "-"))
  })
}) %>%
  dplyr::mutate(
    n_evt      = dplyr::coalesce(n_evt, 0L),
    n_pre      = dplyr::coalesce(n_pre, 0L),
    n_min      = pmin(n_evt, n_pre),
    pct_growth = (value_evt / value_pre - 1) * 100,
    sparse     = n_min < WG_MIN_CELL_N,
    event             = factor(event, levels = MW_EVENT_QTR),
    Employment_Status = factor(Employment_Status, levels = FI_LEVELS),
    pctile            = factor(paste0("p", pctile), levels = paste0("p", PCT_INT))
  )

cat(sprintf("  Windows: %d quarters pre / %d post (event qtr excluded)\n", PRE_Q, POST_Q))
cat(sprintf("  Rows: %d (expect 40) | NA growth: %d | sparse: %d | exact-zero: %d\n",
            nrow(lm_wage_growth_events),
            sum(is.na(lm_wage_growth_events$pct_growth)),
            sum(lm_wage_growth_events$sparse),
            sum(lm_wage_growth_events$pct_growth == 0, na.rm = TRUE)))
cat("  (If exact-zero is still high, the annual-deflator issue in 02A is the cause.)\n")
print(lm_wage_growth_events %>%
        dplyr::select(Employment_Status, event, pctile, n_pre, n_evt, pct_growth) %>%
        dplyr::arrange(Employment_Status, event, pctile), n = 40)
save_rds(lm_wage_growth_events, "lm_wage_growth_events")