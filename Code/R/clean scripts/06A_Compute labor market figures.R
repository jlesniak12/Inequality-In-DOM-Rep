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
# STEP 4 (REVISED). LM-4: Real hourly wage growth at percentiles around MW events
#
# CHANGES vs the original:
#   (a) FT HOURS RESTRICTION (40-48h). The hourly wage is monthly /
#       (weeks * min(hours,44)); very-low-hours recall errors give a tiny
#       denominator and an absurd implied hourly wage that distorts the tail
#       percentiles. Restricting to a near-standard week removes that noise at
#       the source and makes this figure measure the SAME population as the
#       05A inequality figures (full-time formal private). State this in notes.
#         -> Trade-off: shrinks cells slightly, which is why we also (b).
#   (b) SPARSITY IS NO LONGER SILENTLY DROPPED. A growth rate needs BOTH the
#       event quarter and the pre-event quarter to be adequately sized, so we
#       flag `sparse` from the MINIMUM of the two cell counts. We keep every
#       row (4 events x 5 pctiles = 20) and let 06B render thin bars greyed +
#       labelled instead of deleting them. Threshold relaxed to 25 because a
#       single quantile tolerates smaller cells than a variance/Gini.
#
# Population: FULL-TIME formal private wage earners (same as 05A).
# Reference (pre-event) quarter = the quarter immediately before the event.
#===============================================================================

cat("[4] Wage growth at percentiles around events (FT 40-48h, formal private)...\n")

FT_HOURS_LO   <- 40
FT_HOURS_HI   <- 48
WG_MIN_CELL_N <- 25      # single-quantile threshold (relaxed from MIN_CELL_N=30)

design_fp <- subset(
  samples$wage_earners$design,
  Employment_Status == "Formal" &
    Employment_Type == "private employee" &
    !is.na(hours_worked_primary) &
    hours_worked_primary >= FT_HOURS_LO &
    hours_worked_primary <= FT_HOURS_HI
)

PROBS <- c(0.10, 0.25, 0.50, 0.75, 0.90)

# Pre-event quarter = one quarter before each event
event_prev <- function(evt) {
  y <- as.integer(substr(evt, 1, 4)); q <- as.integer(substr(evt, 6, 6))
  if (q == 1) paste0(y - 1, "Q4") else paste0(y, "Q", q - 1)
}
EVENT_PRE   <- vapply(MW_EVENT_QTR, event_prev, character(1))
needed_qtrs <- unique(c(MW_EVENT_QTR, EVENT_PRE))

# Per-quarter percentile levels (keep n_obs so we can flag growth-cell sparsity)
pctile_levels <- purrr::map_dfr(PROBS, function(p) {
  svy_quantile_by(
    subset(design_fp, year_quarter %in% needed_qtrs),
    WAGE_VAR, "year_quarter", prob = p
  ) %>%
    dplyr::transmute(year_quarter, pctile = p * 100,
                     value = estimate, n_obs)
})

# Form growth from pre-event -> event; sparse if EITHER cell is thin.
lm_wage_growth_events <- purrr::map2_dfr(MW_EVENT_QTR, EVENT_PRE, function(evt, pre) {
  cur <- pctile_levels %>% dplyr::filter(year_quarter == evt) %>%
    dplyr::select(pctile, value_evt = value, n_evt = n_obs)
  prv <- pctile_levels %>% dplyr::filter(year_quarter == pre) %>%
    dplyr::select(pctile, value_pre = value, n_pre = n_obs)
  cur %>%
    dplyr::left_join(prv, by = "pctile") %>%
    dplyr::mutate(
      event       = evt,
      pre_quarter = pre,
      pct_growth  = (value_evt / value_pre - 1) * 100,
      n_min       = pmin(n_evt, n_pre, na.rm = FALSE),
      sparse      = is.na(n_min) | n_min < WG_MIN_CELL_N
    )
}) %>%
  dplyr::mutate(
    event  = factor(event, levels = MW_EVENT_QTR),
    pctile = factor(paste0("p", pctile), levels = paste0("p", PROBS * 100))
  )

cat(sprintf("  Rows: %d (expect 20) | sparse flagged: %d | NA growth: %d\n",
            nrow(lm_wage_growth_events),
            sum(lm_wage_growth_events$sparse),
            sum(is.na(lm_wage_growth_events$pct_growth))))
cat("  Cell counts by event x pctile (n_min):\n")
print(lm_wage_growth_events %>%
        dplyr::select(event, pctile, n_evt, n_pre, n_min, sparse) %>%
        dplyr::arrange(event, pctile))
save_rds(lm_wage_growth_events, "lm_wage_growth_events")