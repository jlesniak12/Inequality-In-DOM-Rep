#===============================================================================
# Script: 09b_main estimation.R
#
# Purpose: Produce the paper's headline regression outputs from 08's panel:
#          - Table 1: pooled Post (2017Q2+), Panel A (terciles) + Panel B (cts)
#          - Table 2: per-event windows, Panel A (terciles) + Panel B (cts)
#          - Figure : year-by-year event study, one per outcome
#
# Weighting: each table is produced twice (unweighted headline, baseline-emp
#            robustness). Both saved separately.
#
# Reads:  panel_geo_quarter_<geo>_<tier>.rds    (from 08)
# Writes: to config$out_subdirs$reg_results:
#           tbl_reg_table1_unweighted.{html,tex}    Table 1, unweighted
#           tbl_reg_table1_baseline_emp.{html,tex}  Table 1, weighted
#           tbl_reg_table2_unweighted.{html,tex}    Table 2, unweighted
#           tbl_reg_table2_baseline_emp.{html,tex}  Table 2, weighted
#           fig_es_<outcome>_unweighted.png         event study, one per outcome
#===============================================================================

source(here::here("Code", "R", "clean scripts", "00_setup.R"))
source(file.path(config$paths$scripts, "09a_estimation helpers.R"))


#===============================================================================
# STEP 0. Parameters and paths
#===============================================================================

cat("=== 09b_main estimation.R ===\n\n")

pd <- config$data_dirs$regression

COVID_QTRS <- config$events$covid_qtrs

save_path <- file.path(config$paths$outputs, config$output_stage,
                       config$out_subdirs$reg_results, "main")
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)


# --- Control sets ---
# All variance outcomes get the baseline median-wage x time trend as an
# additional heterogeneous-trends control (07C flagged r = -0.53).
COMP_CONTROLS <- c("share_female", "share_sec_complete",
                   "share_tert_complete", "mean_age")

CONTROLS_BY_OUTCOME <- list(
  log_var_hwage_total    = c(COMP_CONTROLS, "baseline_median_lhw_formal:qidx"),
  log_var_hwage_formal   = c(COMP_CONTROLS, "baseline_median_lhw_formal:qidx"),
  log_var_hwage_informal = c(COMP_CONTROLS, "baseline_median_lhw_formal:qidx"),
  log_informal_share     = COMP_CONTROLS,
  log_selfemp_share      = COMP_CONTROLS
)

#===============================================================================
# STEP 1. Load panel and prep
#
# Drop COVID quarters and treatment quarters (already flagged by 08).  Add
# region_int (numeric cluster id for fwildclusterboot) and year.  The `time`
# column stays as "2017Q2" character.
#===============================================================================

panel_gq <- readRDS(tagged_rds(pd, "panel_geo_quarter"))

if (!"baseline_emp" %in% names(panel_gq)) {
  stop("baseline_emp not on panel - add it to 08 (see 09 header for the block).")
}

reg <- panel_gq %>%
  filter(!is_treatment_qtr, !time %in% COVID_QTRS) %>%
  mutate(region_int = as.integer(factor(Region10)),
         year = as.integer(substr(time, 1, 4)))

reg_windowed <- add_window(reg)

cat(sprintf("[09] pooled panel: %d obs | windowed panel: %d obs\n",
            nrow(reg), nrow(reg_windowed)))


#===============================================================================
# STEP 2. Coefficient maps (rows of the tables)
#===============================================================================

# Table 1 (pooled Post) - two coefficient rows for tercile spec, one for cts.
coef_map_t1 <- c(
  "exposure_group::Medium exposure:post_any" = "β₂  (Medium × Post)",
  "exposure_group::High exposure:post_any"   = "β₃  (High × Post)",
  "exposure_geo_val:post_any"                = "Exposure × Post"
)

# Table 2 (per-event) - three windows x two panels.
coef_map_t2 <- c(
  # Panel A: tercile x window
  "window::post_2017:exposure_group::Medium exposure" = "β₂ × Post-2017",
  "window::post_2021:exposure_group::Medium exposure" = "β₂ × Post-2021",
  "window::post_2023:exposure_group::Medium exposure" = "β₂ × Post-2023",
  "window::post_2017:exposure_group::High exposure"   = "β₃ × Post-2017",
  "window::post_2021:exposure_group::High exposure"   = "β₃ × Post-2021",
  "window::post_2023:exposure_group::High exposure"   = "β₃ × Post-2023",
  # Panel B: continuous x window
  "window::post_2017:exposure_geo_val" = "Exposure × Post-2017",
  "window::post_2021:exposure_geo_val" = "Exposure × Post-2021",
  "window::post_2023:exposure_geo_val" = "Exposure × Post-2023"
)
# Helper: pick the control set for an outcome, or NULL if controls turned off
ctrl_for <- function(nm, use_controls) {
  if (isTRUE(use_controls)) CONTROLS_BY_OUTCOME[[nm]] else NULL
}

#===============================================================================
# STEP 3. Table 1 - pooled Post
#===============================================================================

run_table1 <- function(weights, tag, use_controls = TRUE) {
  cat(sprintf("[09] Table 1 - %s\n", tag))
  
  fits_A <- purrr::imap(OUTCOMES, function(v, nm) {
    run_pooled_tier(v, data = reg, weights = weights,
                    controls = ctrl_for(nm, use_controls))
  })
  fits_B <- purrr::imap(OUTCOMES, function(v, nm) {
    run_pooled_cont(v, data = reg, weights = weights,
                    controls = ctrl_for(nm, use_controls))
  })
  
  ctrl_note <- if (use_controls)
    "Controls: share female, share secondary complete, share tertiary complete, mean age. Variance outcomes additionally include baseline formal median log wage x linear time trend."
  else
    "No compositional controls."
  
  save_table_boot(fits_A, coef_map = coef_map_t1,
                  title = paste0("Table 1A. Pooled Post, tercile groups (", tag, ")"),
                  notes = list("Reference: Low exposure tercile.",
                               "Region10 + quarter FE. COVID quarters excluded.",
                               ctrl_note),
                  file_base = paste0("tbl_reg_table1A_tercile_", tag), path = save_path)
  
  save_table_boot(fits_B, coef_map = coef_map_t1,
                  title = paste0("Table 1B. Pooled Post, continuous exposure (", tag, ")"),
                  notes = list("Region10 + quarter FE. COVID quarters excluded.", ctrl_note),
                  file_base = paste0("tbl_reg_table1B_continuous_", tag), path = save_path)
  
  invisible(list(A = fits_A, B = fits_B))
}

t1_unw    <- run_table1(weights = NULL,           tag = "unweighted_ctrl",     use_controls = TRUE)
t1_unw_nc <- run_table1(weights = NULL,           tag = "unweighted_noctrl",   use_controls = FALSE)
t1_bem    <- run_table1(weights = "baseline_emp", tag = "baseline_emp_ctrl",   use_controls = TRUE)
t1_bem_nc <- run_table1(weights = "baseline_emp", tag = "baseline_emp_noctrl", use_controls = FALSE)


#===============================================================================
# STEP 4. Table 2 - per-event windows
#===============================================================================

run_table2 <- function(weights, tag, use_controls = TRUE) {
  cat(sprintf("[09] Table 2 - %s\n", tag))
  
  fits_A <- purrr::imap(OUTCOMES, function(v, nm) {
    run_window_tier(v, data = reg_windowed, weights = weights,
                    controls = ctrl_for(nm, use_controls))
  })
  fits_B <- purrr::imap(OUTCOMES, function(v, nm) {
    run_window_cont(v, data = reg_windowed, weights = weights,
                    controls = ctrl_for(nm, use_controls))
  })
  
  ctrl_note <- if (use_controls)
    "Controls: share female, share secondary complete, share tertiary complete, mean age. Variance outcomes additionally include baseline formal median log wage x linear time trend."
  else
    "No compositional controls."
  
  win_note <- paste("Windows: pre (<2017Q2), post_2017 (2017Q3-2019Q2),",
                    "post_2021 (2021Q4-2023Q1), post_2023 (2023Q3-2025Q1).")
  
  save_table_boot(fits_A, coef_map = coef_map_t2,
                  title = paste0("Table 2A. Per-event windows, tercile groups (", tag, ")"),
                  notes = list("Reference: Low exposure tercile x pre window.",
                               "Region10 + quarter FE.", win_note, ctrl_note),
                  file_base = paste0("tbl_reg_table2A_tercile_", tag), path = save_path)
  
  save_table_boot(fits_B, coef_map = coef_map_t2,
                  title = paste0("Table 2B. Per-event windows, continuous exposure (", tag, ")"),
                  notes = list("Reference: pre window.",
                               "Region10 + quarter FE.", win_note, ctrl_note),
                  file_base = paste0("tbl_reg_table2B_continuous_", tag), path = save_path)
  
  invisible(list(A = fits_A, B = fits_B))
}

t2_unw    <- run_table2(weights = NULL,           tag = "unweighted_ctrl",     use_controls = TRUE)
t2_unw_nc <- run_table2(weights = NULL,           tag = "unweighted_noctrl",   use_controls = FALSE)
t2_bem    <- run_table2(weights = "baseline_emp", tag = "baseline_emp_ctrl",   use_controls = TRUE)
t2_bem_nc <- run_table2(weights = "baseline_emp", tag = "baseline_emp_noctrl", use_controls = FALSE)


#===============================================================================
# STEP 5. Event-study figures - one per outcome, with controls
#===============================================================================

cat("[09] Event studies (unweighted, with controls)...\n")

es_subtitle <- paste(
  "Coefficient on Exposure × Year, reference year 2016.",
  "Region10 + time FE. Controls included. COVID quarters excluded.",
  "Wild cluster bootstrap p-values (Webb, B=9999, cluster = Region10).",
  "Red dotted lines = MW event quarters.",
  sep = "\n"
)

purrr::iwalk(OUTCOMES, function(v, nm) {
  fit <- run_es_cont(v, data = reg, weights = NULL,
                     controls = CONTROLS_BY_OUTCOME[[nm]])
  if (is.null(fit)) { message("Skipping ES for ", nm); return(invisible()) }
  es_terms <- grep("year::", names(coef(fit)), value = TRUE, fixed = TRUE)
  boot_tbl <- bootstrap_ci(fit, terms = es_terms)
  
  p <- plot_event_study(boot_tbl,
                        title    = paste("Event study:", OUTCOME_LABELS[nm]),
                        subtitle = es_subtitle,
                        y_label  = "Coefficient × Year")
  save_plot(p, paste0("fig_es_", nm, "_ctrl"), save_path,
            w = config$fig_defaults$width * 1.2,
            h = config$fig_defaults$height * 0.9)
})

