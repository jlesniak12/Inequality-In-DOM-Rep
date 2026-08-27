#===============================================================================
#
# Script: 08_Prepare_Regression_Panel.R
#
# Purpose: Assemble the analysis-ready panel for the Parente-style event study.
#          Produces survey-weighted OUTCOMES at the geo x quarter level (and a
#          finer geo x tier x quarter panel), joins the tagged exposure measure
#          from 07A for the ACTIVE (baseline x income) combination, and attaches
#          event-time / treatment variables for the multiple MW events.
#
# Parameterized by config$active_baseline and config$active_income (same axes
# as 07A). Each run produces one panel per active combination; the driver loops
# to build all four.
#
# Baseline covariates (baseline_emp, baseline_median_lhw_formal) filter to the
# active baseline's PERIOD, not hardcoded 2016. Non-compliance uses the income
# concept's compliance_var (below_min_hourly vs below_min_monthly). Variance
# outcomes computed for BOTH hourly and monthly to keep panels symmetric across
# income runs; 09 picks which set to use via IS$log_var_prefix.
#
# Reads:
#   samples$reg_variance, $reg_shares, $reg_tier   (03)
#   exposure_geo__<income>__<baseline>__<geo>.rds  (07A, via mw_file)
#
# Writes (via mw_file - filename convention <item>__<income>__<baseline>__<geo>):
#   panel_geo_quarter__<...>.rds        geo x quarter (headline panel)
#   panel_geo_tier_quarter__<...>.rds   geo x tier x quarter (finer panel)
#
#===============================================================================

if (!exists("config", envir = .GlobalEnv, inherits = FALSE)) {
  source(here::here("Code","R","clean scripts","00_setup.R"))
} else {
  cat("[08] Reusing existing `config` (00_setup not re-sourced)\n")
}

if (!exists("samples", envir = .GlobalEnv, inherits = FALSE) ||
    is.null(samples$reg_variance$data)) {
  source(file.path(config$paths$scripts, "03_sample definitions.R"))
} else {
  cat("[08] Reusing existing `samples` (03 not re-sourced)\n")
}


#===============================================================================
# STEP 0. Parameters
#===============================================================================

cat("[08] Preparing regression panel\n")

in_dir <- config$data_dirs$exposure
out_dir <- config$data_dirs$regression

BL <- config$baselines[[config$active_baseline]]
IS <- config$income_specs[[config$active_income]]

GEO       <- config$exposure$construct_geo
TIER_VAR  <- "wage_group"
TIER_KEEP <- config$TIER_LEVELS

COMPLIANCE_VAR <- IS$compliance_var   # income-appropriate non-compliance measure

EVENT_QTRS    <- config$events$event_qtrs
PHASE_IN_QTRS <- config$events$phase_in_qtrs
COVID_QTRS    <- config$events$covid_qtrs

# Lonely-PSU handling. Slicing to geo x quarter (and finer) leaves some strata
# with a single PSU per cell, which breaks variance estimation. "adjust"
# centers lonely strata at the grand mean - conservative and standard for
# ENCFT variance work.
options(survey.lonely.psu = "adjust")

cat(sprintf("  baseline=%s | income=%s | geo=%s\n",
            config$active_baseline, config$active_income, GEO))
cat(sprintf("  compliance measure: %s\n", COMPLIANCE_VAR))


#===============================================================================
# STEP 1. Load exposure and build one design per frame
#
# The three regression frames from 03 have different scopes; each outcome must
# be computed on the frame that matches its concept. Do NOT collapse to a
# single analysis_df - filtering out reg_variance's zero-salary rows in order
# to compute informal_share would drop most informals.
#===============================================================================

exposure_geo <- readRDS(mw_file("exposure_geo", dir = in_dir))

svy <- function(df) svydesign(id = ~psu_unique, strata = ~strata_unique,
                              weights = ~FACTOR_EXPANSION, data = df, nest = TRUE)

add_time <- function(df) mutate(df, time = paste0(year, "Q", quarter))

df_variance <- samples$reg_variance$data %>% add_time()
df_shares   <- samples$reg_shares$data   %>% add_time()
df_tier     <- samples$reg_tier$data     %>% add_time() %>%
  filter(.data[[TIER_VAR]] %in% TIER_KEEP)

# Sub-designs on df_variance for the three populations of the variance outcomes
des_var_total    <- svy(df_variance)
des_var_formal   <- svy(df_variance %>% filter(Employment_Status == "Formal"))
des_var_informal <- svy(df_variance %>% filter(Employment_Status == "Informal"))
des_shares       <- svy(df_shares)
des_tier_formal  <- svy(df_tier %>% filter(Employment_Status == "Formal"))

cat(sprintf("  frames: reg_variance=%d | reg_shares=%d | reg_tier(formal, tier)=%d\n",
            nrow(df_variance), nrow(df_shares),
            nrow(df_tier %>% filter(Employment_Status == "Formal"))))


#===============================================================================
# STEP 2. Survey-weighted outcomes at geo x quarter
#
# Variance outcomes computed for BOTH hourly and monthly - the panel stays
# income-symmetric and 09B picks which set based on IS$log_var_prefix. Share
# outcomes and controls are income-agnostic. Non-compliance uses COMPLIANCE_VAR
# (single column, source depends on active income).
#===============================================================================

cat("[08] Computing survey-weighted outcomes (geo x quarter)...\n")

byf <- function(...) as.formula(paste0("~", paste(c(...), collapse = " + ")))
BY_GQ <- byf("time", GEO)

# --- variance of log hourly wage ---
var_h_tot <- svyby(~log_hwage, BY_GQ, des_var_total, svyvar, na.rm = TRUE) %>%
  as_tibble() %>% transmute(time, !!GEO := .data[[GEO]],
                            log_var_hwage_total = log(log_hwage))
var_h_frm <- svyby(~log_hwage, BY_GQ, des_var_formal, svyvar, na.rm = TRUE) %>%
  as_tibble() %>% transmute(time, !!GEO := .data[[GEO]],
                            log_var_hwage_formal = log(log_hwage))
var_h_inf <- svyby(~log_hwage, BY_GQ, des_var_informal, svyvar, na.rm = TRUE) %>%
  as_tibble() %>% transmute(time, !!GEO := .data[[GEO]],
                            log_var_hwage_informal = log(log_hwage))

# --- variance of log monthly wage ---
var_m_tot <- svyby(~log_mwage, BY_GQ, des_var_total, svyvar, na.rm = TRUE) %>%
  as_tibble() %>% transmute(time, !!GEO := .data[[GEO]],
                            log_var_mwage_total = log(log_mwage))
var_m_frm <- svyby(~log_mwage, BY_GQ, des_var_formal, svyvar, na.rm = TRUE) %>%
  as_tibble() %>% transmute(time, !!GEO := .data[[GEO]],
                            log_var_mwage_formal = log(log_mwage))
var_m_inf <- svyby(~log_mwage, BY_GQ, des_var_informal, svyvar, na.rm = TRUE) %>%
  as_tibble() %>% transmute(time, !!GEO := .data[[GEO]],
                            log_var_mwage_informal = log(log_mwage))

# --- share outcomes (all employed) + log transforms ---
inf_share <- svyby(~is_informal, BY_GQ, des_shares, svymean, na.rm = TRUE) %>%
  as_tibble() %>%
  transmute(time, !!GEO := .data[[GEO]],
            informal_share     = is_informal,
            log_informal_share = if_else(is_informal > 0, log(is_informal), NA_real_))

se_share  <- svyby(~is_selfemp, BY_GQ, des_shares, svymean, na.rm = TRUE) %>%
  as_tibble() %>%
  transmute(time, !!GEO := .data[[GEO]],
            selfemp_share      = is_selfemp,
            log_selfemp_share  = if_else(is_selfemp > 0, log(is_selfemp), NA_real_))

# --- non-compliance (formal workers with known tier, below their tier floor) ---
# Uses IS$compliance_var: hourly for hourly income run, monthly for monthly.
# Panel column name stays `below_min_share` (single column, provenance in file
# name).
bm_share <- svyby(byf(COMPLIANCE_VAR), BY_GQ, des_tier_formal,
                  svymean, na.rm = TRUE) %>%
  as_tibble() %>%
  transmute(time, !!GEO := .data[[GEO]],
            below_min_share = .data[[COMPLIANCE_VAR]])

# --- Compositional controls (all on reg_shares - employed population) ---
fem_share <- svyby(~is_female, BY_GQ, des_shares, svymean, na.rm = TRUE) %>%
  as_tibble() %>% transmute(time, !!GEO := .data[[GEO]], share_female = is_female)

sec_share <- svyby(~is_sec_complete, BY_GQ, des_shares, svymean, na.rm = TRUE) %>%
  as_tibble() %>% transmute(time, !!GEO := .data[[GEO]],
                            share_sec_complete = is_sec_complete)

ter_share <- svyby(~is_tert_complete, BY_GQ, des_shares, svymean, na.rm = TRUE) %>%
  as_tibble() %>% transmute(time, !!GEO := .data[[GEO]],
                            share_tert_complete = is_tert_complete)

age_mean  <- svyby(~EDAD, BY_GQ, des_shares, svymean, na.rm = TRUE) %>%
  as_tibble() %>% transmute(time, !!GEO := .data[[GEO]], mean_age = EDAD)


# --- cell support (unweighted, on the widest frame that touches this cell) ---
n_gq <- df_shares %>%
  group_by(across(all_of(c("time", GEO)))) %>%
  summarise(n_obs = dplyr::n(), n_psu = dplyr::n_distinct(psu_unique),
            .groups = "drop")

n_variance <- df_variance %>%
  group_by(across(all_of(c("time", GEO)))) %>%
  summarise(n_variance = dplyr::n(), .groups = "drop")
n_tier <- df_tier %>% filter(Employment_Status == "Formal") %>%
  group_by(across(all_of(c("time", GEO)))) %>%
  summarise(n_tier = dplyr::n(), .groups = "drop")

panel_gq <- reduce(
  list(var_h_tot, var_h_frm, var_h_inf,
       var_m_tot, var_m_frm, var_m_inf,
       inf_share, se_share, bm_share, n_gq,
       fem_share, sec_share, ter_share, age_mean),
  full_join, by = c("time", GEO)
)


#===============================================================================
# STEP 3. Attach exposure + baseline covariates + geography labels
#
# Baseline covariates now filter to the ACTIVE baseline's period (was hardcoded
# to 2016). For base2021q2_micro this means both baseline_emp and
# baseline_median_lhw_formal are measured at 2021Q2, matching the exposure
# construction period. Different baselines therefore produce baseline covariates
# with different values - which is correct (a 2016 employment weight on a
# post-COVID design would be incoherent).
#===============================================================================

# Baseline period filter - same idiom as 07A.
baseline_period_filter <- function(df) {
  if (BL$period$type == "year") {
    df %>% filter(year == BL$period$value)
  } else {
    df %>% filter(paste0(year, "Q", quarter) == BL$period$value)
  }
}

baseline_emp_tbl <- df_shares %>%
  baseline_period_filter() %>%
  group_by(across(all_of(GEO))) %>%
  summarise(baseline_emp = sum(FACTOR_EXPANSION), .groups = "drop") %>%
  mutate(baseline_emp = baseline_emp / sum(baseline_emp))

base_med_formal <- df_variance %>%
  baseline_period_filter() %>%
  filter(Employment_Status == "Formal",
         !is.na(log_hwage), !is.na(FACTOR_EXPANSION)) %>%
  group_by(across(all_of(GEO))) %>%
  summarise(baseline_median_lhw_formal =
              matrixStats::weightedMedian(log_hwage, w = FACTOR_EXPANSION,
                                          na.rm = TRUE),
            .groups = "drop")

exp_keep <- exposure_geo %>%
  select(all_of(GEO), exposure_geo_val, exposure_group,
         any_of(c("Region10", "Region4"))) %>%
  left_join(baseline_emp_tbl, by = GEO) %>%
  left_join(base_med_formal,  by = GEO)

panel_gq <- panel_gq %>% left_join(exp_keep, by = GEO)

INFERENCE_GEO <- config$regression$inference_geo
if (INFERENCE_GEO %in% names(panel_gq)) {
  panel_gq$cluster_id <- panel_gq[[INFERENCE_GEO]]
} else {
  stop("inference_geo '", INFERENCE_GEO, "' not on panel - carry it from ",
       "exposure_geo or ensure it equals construct_geo.")
}
cat(sprintf("[08] cluster_id = %s (%d clusters)\n",
            INFERENCE_GEO, dplyr::n_distinct(panel_gq$cluster_id)))


#===============================================================================
# STEP 4. Event-time / treatment variables
#
# post_<event> columns are event-WINDOW flags: 1 during (event, next-event),
# else 0. NOT usable as a "post since event" Post dummy on the full panel (they
# flip back to 0 at the next event). 09 builds a proper Post locally.
#===============================================================================

cat("[08] Building event-time / treatment variables...\n")

all_qtrs <- panel_gq %>% distinct(time) %>%
  mutate(yr   = as.integer(str_sub(time, 1, 4)),
         q    = as.integer(str_sub(time, 6, 6)),
         qidx = (yr - min(yr)) * 4 + q) %>%
  arrange(qidx)

qidx_of   <- function(tt) all_qtrs$qidx[match(tt, all_qtrs$time)]
event_idx <- qidx_of(EVENT_QTRS)

panel_gq <- panel_gq %>%
  left_join(all_qtrs %>% select(time, qidx), by = "time") %>%
  mutate(is_treatment_qtr = time %in% EVENT_QTRS,
         is_phase_in      = time %in% PHASE_IN_QTRS,
         is_covid         = time %in% COVID_QTRS)

for (i in seq_along(EVENT_QTRS)) {
  e_idx <- event_idx[i]
  nxt   <- if (i < length(EVENT_QTRS)) event_idx[i + 1] else Inf
  lab   <- str_replace(EVENT_QTRS[i], "Q", "q")
  panel_gq[[paste0("post_",     lab)]] <-
    as.integer(panel_gq$qidx > e_idx & panel_gq$qidx < nxt)
  panel_gq[[paste0("evt_time_", lab)]] <- panel_gq$qidx - e_idx
}

panel_gq <- panel_gq %>%
  mutate(post_any = as.integer(qidx > event_idx[1] & !is_treatment_qtr))


#===============================================================================
# STEP 5. Finer geo x tier x quarter panel (for tier-interacted specs)
#
# Only the outcomes that make sense within a tier cell.  Share outcomes on
# reg_shares span workers whose firm size is unknown; those workers do NOT have
# a tier, so a tier-cell version drops them.  Variance and compliance are
# unaffected because they already require known tier / formal-with-tier.
#===============================================================================

cat("[08] Building geo x tier x quarter panel...\n")

BY_GTQ <- byf("time", GEO, TIER_VAR)

var_h_frm_gtq <- svyby(~log_hwage, BY_GTQ, des_var_formal, svyvar, na.rm = TRUE) %>%
  as_tibble() %>% transmute(time, !!GEO := .data[[GEO]],
                            !!TIER_VAR := .data[[TIER_VAR]],
                            log_var_hwage_formal = log(log_hwage))
bm_gtq <- svyby(byf(COMPLIANCE_VAR), BY_GTQ, des_tier_formal,
                svymean, na.rm = TRUE) %>%
  as_tibble() %>% transmute(time, !!GEO := .data[[GEO]],
                            !!TIER_VAR := .data[[TIER_VAR]],
                            below_min_share = .data[[COMPLIANCE_VAR]])

n_gtq <- df_tier %>%
  group_by(across(all_of(c("time", GEO, TIER_VAR)))) %>%
  summarise(n_obs = dplyr::n(), n_psu = dplyr::n_distinct(psu_unique),
            .groups = "drop")

panel_gtq <- var_h_frm_gtq %>%
  full_join(bm_gtq, by = c("time", GEO, TIER_VAR)) %>%
  left_join(n_gtq,  by = c("time", GEO, TIER_VAR)) %>%
  left_join(exp_keep, by = GEO) %>%
  left_join(panel_gq %>%
              select(time, qidx, is_treatment_qtr, is_phase_in, is_covid,
                     starts_with("post_"), starts_with("evt_time_"),
                     post_any) %>%
              distinct(),
            by = "time")


#===============================================================================
# STEP 6. Save
#===============================================================================

saveRDS(panel_gq,  mw_file("panel_geo_quarter", dir = out_dir))
saveRDS(panel_gtq, mw_file("panel_geo_tier_quarter", dir = out_dir))

cat(sprintf("[08] Done. baseline=%s income=%s\n",
            config$active_baseline, config$active_income))
cat(sprintf("     geo x quarter: %d rows | geo x tier x quarter: %d rows\n",
            nrow(panel_gq), nrow(panel_gtq)))
cat("     Wrote:\n")
for (item in c("panel_geo_quarter", "panel_geo_tier_quarter")) {
  cat("       ", basename(mw_file(item, dir = out_dir)), "\n")
}
cat(sprintf("     Cluster id: %s.  Treatment: exposure_geo_val (%s / %s).\n",
            INFERENCE_GEO, config$active_baseline, config$active_income))

