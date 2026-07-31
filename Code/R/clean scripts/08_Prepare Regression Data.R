#===============================================================================
#
# Script: 08_Prepare_Regression_Panel.R
#
# Purpose: Assemble the analysis-ready panel for the Parente-style event study.
#          Produces survey-weighted OUTCOMES at the geo x quarter level (and a
#          finer geo x tier x quarter panel), joins the FIXED 2016 exposure
#          measure from script 07, and attaches event-time / treatment variables
#          for the multiple MW events (with phase-in handling).
#
# Outcomes (Parente analogs), all on the regression_sample, formal-vs-informal
# split where relevant:
#   - log_var_wage      log of survey-weighted variance of log hourly wages
#                       (real_salary_primary_hourly_base), formal sector
#   - below_min         non-compliance: share of FORMAL workers below the tier
#                       hourly floor (matches the tier scheme used for exposure)
#   - informal          informal share among private employees in the cell
#
# Treatment:
#   exposure_geo_val (continuous, FIXED at 2016) interacted with event-time.
#   Events: 2017Q2, 2019Q3, 2021Q3, 2023Q2. Phase-in quarters (2017Q4, 2022Q1,
#   2024Q1) folded into POST. Treatment quarter excluded. COVID flagged.
#
# Geography:
#   Built at the exposure construction geo (province). Region10 and Region4
#   labels carried along so estimation (script 08) can aggregate / cluster at
#   any level. Region4 = official inference domain (default clustering).
#
# Reads:
#   samples$regression_sample           (03_Sample Definitions.R)
#   exposure_geo_<tier>.rds             (07_Construction of mw Exposure.R)
#
# Writes:
#   panel_geo_quarter_<tier>.rds        geo x quarter (headline panel)
#   panel_geo_tier_quarter_<tier>.rds   geo x tier x quarter (finer panel)
#
#===============================================================================

source(here::here("Code","R","clean scripts","00_setup.R"))
source(file.path(config$paths$scripts, "03_sample_definitions.R"))




#===============================================================================
# STEP 0. Parameters
#===============================================================================

cat("[08] Preparing regression panel\n")

pd <- config$data_dirs$regression

GEO         <- config$exposure$construct_geo
TIER_SCHEME <- config$exposure$tier_scheme
TOL         <- config$exposure$mw_compliance_tolerance
INCOME_VAR  <- config$income$income

if (TIER_SCHEME == "4tier") {
  TIER_VAR  <- "Wage_group"
  FLOOR_VAR <- config$income$minwage_4tier_inc
  TIER_KEEP <- c("Micro", "Small", "Medium", "Large")
} else {
  TIER_VAR  <- "Wage_group_3tier"
  FLOOR_VAR <- config$income$minwage_3tier_inc
  TIER_KEEP <- c("Micro", "Small", "Medium/Large")
}

EVENT_QTRS    <- config$events$event_qtrs
PHASE_IN_QTRS <- config$events$phase_in_qtrs
COVID_QTRS    <- config$events$covid_qtrs


#===============================================================================
# STEP 1. Load fixed exposure (from script 07) and the analysis frame
#===============================================================================

exposure_geo <- readRDS(tagged_rds(pd, "exposure_geo"))

# Full analysis frame: regression sample over ALL quarters (not just baseline).
# Build a survey design with an added below_min indicator on the matching tier
# floor and an informal indicator. log hourly wage added for variance outcome.
analysis_df <- samples$regression_sample$data %>%
  filter(.data[[TIER_VAR]] %in% TIER_KEEP) %>%
  mutate(
    time      = paste0(year, "Q", quarter),
    log_hwage = log(.data[[INCOME_VAR]]),
    # non-compliance on the SAME floor concept as exposure (tier-matched, hourly,
    # with the survey-error tolerance) — keeps exposure & below_min coherent
    below_min = dplyr::case_when(
      is.na(.data[[INCOME_VAR]]) | .data[[INCOME_VAR]] <= 0 ~ NA_integer_,
      is.na(.data[[FLOOR_VAR]])                              ~ NA_integer_,
      TRUE ~ as.integer(.data[[INCOME_VAR]] < .data[[FLOOR_VAR]] * (1 - TOL))
    ),
    informal = as.integer(Employment_Status == "Informal")
  )

cat(sprintf("  analysis rows (all quarters, known tier): %d | quarters: %d\n",
            nrow(analysis_df), dplyr::n_distinct(analysis_df$time)))


#===============================================================================
# STEP 2. Survey-weighted outcomes
#
# log_var_wage and below_min are FORMAL-sector concepts (MW binds formals).
# informal is computed over ALL private employees in the cell (the share of the
# cell that is informal — the Parente informality-margin outcome).
#===============================================================================

cat("[08] Computing survey-weighted outcomes...\n")

# Lonely-PSU handling. Slicing to Region10 x quarter (and finer) leaves some
# design strata with a single PSU in a cell, which breaks variance estimation
# (svyvar for log_var_wage). Center lonely strata at the grand mean — the same
# conservative convention used elsewhere in this codebase (compute_palma.R).
# Set globally here (this script runs at top level, so on.exit() would not
# fire); "adjust" is the recommended setting for ENCFT variance work regardless.
options(survey.lonely.psu = "adjust")

# 2a. Formal-only design for wage-variance and non-compliance
df_formal <- analysis_df %>% filter(Employment_Status == "Formal")
des_formal <- svydesign(id = ~psu_unique, strata = ~strata_unique,
                        weights = ~FACTOR_EXPANSION, data = df_formal, nest = TRUE)

# helper to build a by-formula
byf <- function(...) as.formula(paste0("~", paste(c(...), collapse = " + ")))

# --- geo x quarter ---
# variance of log wage (formal)
var_gq <- svyby(~log_hwage, byf("time", GEO), des_formal, svyvar, na.rm = TRUE) %>%
  as_tibble() %>% select(time, all_of(GEO), var_log_hwage = log_hwage) %>%
  mutate(log_var_wage = log(var_log_hwage))

# non-compliance share (formal)
nc_gq <- svyby(~below_min, byf("time", GEO), des_formal, svymean, na.rm = TRUE) %>%
  as_tibble() %>% select(time, all_of(GEO), below_min)

# informality share (all private employees)
des_all <- svydesign(id = ~psu_unique, strata = ~strata_unique,
                     weights = ~FACTOR_EXPANSION, data = analysis_df, nest = TRUE)
inf_gq <- svyby(~informal, byf("time", GEO), des_all, svymean, na.rm = TRUE) %>%
  as_tibble() %>% select(time, all_of(GEO), informal)

# unweighted cell support (for trimming thin cells downstream)
n_gq <- analysis_df %>%
  group_by(across(all_of(c("time", GEO)))) %>%
  summarise(n_obs = dplyr::n(), n_psu = dplyr::n_distinct(psu_unique),
            .groups = "drop")

panel_gq <- var_gq %>%
  full_join(nc_gq,  by = c("time", GEO)) %>%
  full_join(inf_gq, by = c("time", GEO)) %>%
  left_join(n_gq,   by = c("time", GEO))


#===============================================================================
# STEP 3. Attach fixed exposure + geography labels + clustering id
#===============================================================================

exp_keep <- exposure_geo %>%
  select(all_of(GEO), exposure_geo_val, exposure_group,
         any_of(c("Region10", "Region4")))

panel_gq <- panel_gq %>%
  left_join(exp_keep, by = GEO)

# Clustering identifier for estimation (script 09). Set to the inference
# geography from config (default Region10 = the level at which treatment is
# assigned). If construction geo == inference geo, GEO already is that column.
INFERENCE_GEO <- config$regression$inference_geo
if (INFERENCE_GEO %in% names(panel_gq)) {
  panel_gq$cluster_id <- panel_gq[[INFERENCE_GEO]]
} else if (INFERENCE_GEO == GEO) {
  panel_gq$cluster_id <- panel_gq[[GEO]]
} else {
  stop("inference_geo '", INFERENCE_GEO, "' not found on panel — ",
       "ensure it is carried from exposure_geo or equals construct_geo.")
}
cat(sprintf("[08] cluster_id = %s (%d clusters)\n",
            INFERENCE_GEO, dplyr::n_distinct(panel_gq$cluster_id)))


#===============================================================================
# STEP 4. Event-time / treatment variables
#
# For each event we build: a post indicator (1 strictly after the event quarter,
# through the next event), the treatment-quarter exclusion flag, phase-in-in-post
# handling, and event-time (quarters relative to event) for the event study.
# We also flag COVID quarters for exclusion in robustness.
#===============================================================================

cat("[08] Building event-time / treatment variables...\n")

# ordered quarter index
all_qtrs <- panel_gq %>% distinct(time) %>%
  mutate(yr = as.integer(str_sub(time, 1, 4)),
         q  = as.integer(str_sub(time, 6, 6)),
         qidx = (yr - min(yr)) * 4 + q) %>%
  arrange(qidx)

qidx_of <- function(tt) all_qtrs$qidx[match(tt, all_qtrs$time)]

event_idx <- qidx_of(EVENT_QTRS)

panel_gq <- panel_gq %>%
  left_join(all_qtrs %>% select(time, qidx), by = "time") %>%
  mutate(
    is_treatment_qtr = time %in% EVENT_QTRS,   # excluded from pre & post
    is_phase_in      = time %in% PHASE_IN_QTRS,# inside post (not separate event)
    is_covid         = time %in% COVID_QTRS
  )

# Per-event post indicators and event-time (relative quarters).
# "post_e" = 1 for quarters strictly after event e and before the next event.
for (i in seq_along(EVENT_QTRS)) {
  e_idx  <- event_idx[i]
  nxt    <- if (i < length(EVENT_QTRS)) event_idx[i + 1] else Inf
  lab    <- str_replace(EVENT_QTRS[i], "Q", "q")
  panel_gq[[paste0("post_", lab)]] <-
    as.integer(panel_gq$qidx > e_idx & panel_gq$qidx < nxt)
  panel_gq[[paste0("evt_time_", lab)]] <- panel_gq$qidx - e_idx
}

# A single "any post" relative to the FIRST event (for a simple pooled spec)
panel_gq <- panel_gq %>%
  mutate(post_any = as.integer(qidx > event_idx[1] & !is_treatment_qtr))


#===============================================================================
# STEP 5. Finer geo x tier x quarter panel (for tier-interacted specs)
#===============================================================================

cat("[08] Building geo x tier x quarter panel...\n")

var_gtq <- svyby(~log_hwage, byf("time", GEO, TIER_VAR), des_formal, svyvar,
                 na.rm = TRUE) %>%
  as_tibble() %>%
  select(time, all_of(GEO), all_of(TIER_VAR), var_log_hwage = log_hwage) %>%
  mutate(log_var_wage = log(var_log_hwage))

nc_gtq <- svyby(~below_min, byf("time", GEO, TIER_VAR), des_formal, svymean,
                na.rm = TRUE) %>%
  as_tibble() %>% select(time, all_of(GEO), all_of(TIER_VAR), below_min)

inf_gtq <- svyby(~informal, byf("time", GEO, TIER_VAR), des_all, svymean,
                 na.rm = TRUE) %>%
  as_tibble() %>% select(time, all_of(GEO), all_of(TIER_VAR), informal)

n_gtq <- analysis_df %>%
  group_by(across(all_of(c("time", GEO, TIER_VAR)))) %>%
  summarise(n_obs = dplyr::n(), n_psu = dplyr::n_distinct(psu_unique),
            .groups = "drop")

panel_gtq <- var_gtq %>%
  full_join(nc_gtq,  by = c("time", GEO, TIER_VAR)) %>%
  full_join(inf_gtq, by = c("time", GEO, TIER_VAR)) %>%
  left_join(n_gtq,   by = c("time", GEO, TIER_VAR)) %>%
  left_join(exp_keep, by = GEO) %>%
  left_join(panel_gq %>%
              select(time, qidx, is_treatment_qtr, is_phase_in, is_covid,
                     starts_with("post_"), starts_with("evt_time_"), post_any) %>%
              distinct(),
            by = "time")


#===============================================================================
# STEP 6. Save
#===============================================================================

saveRDS(panel_gq,  file.path(pd, paste0("panel_geo_quarter_",      TIER_SCHEME, ".rds")))
saveRDS(panel_gtq, file.path(pd, paste0("panel_geo_tier_quarter_", TIER_SCHEME, ".rds")))

cat(sprintf("[08] Done. geo x quarter: %d rows | geo x tier x quarter: %d rows\n",
            nrow(panel_gq), nrow(panel_gtq)))
cat("     Outcomes: log_var_wage, below_min, informal. Treatment: exposure_geo_val (2016 fixed).\n")
cat("     Clustering var for inference: Region4 (carried in panel). Re-run per tier_scheme.\n")