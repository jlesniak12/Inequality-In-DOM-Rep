#===============================================================================
#
# Script: run_method2_all_specs.R
#
# Purpose: Run scripts 10 -> 10B -> 11 -> 11B -> 12 for every specification
#          in the grid without manually editing config each time.
#
# Usage:   source(here::here("Code", "R", "clean scripts", "run_method2_all_specs.R"))
#
# How it works:
#   1. Sources 00_setup.R once to load packages and the base config.
#   2. Defines a grid of specifications (control bandwidth x treatment min
#      firm size x baseline rule). Each spec gets its own
#      treatment/control/baseline_rule folder path (see scripts 10/11/12
#      for the exact structure: event/treatment/control/baseline_rule/
#      balance/window/...).
#   3. For each row, overrides the relevant config values in memory,
#      rebuilds the dynamic labels, and sources the scripts in order.
#   4. Both balance modes (balanced/unbalanced) are always processed
#      together for every spec -- see CHANGE 1 below for why.
#
# To add/remove specs: edit SPEC_GRID.
# To run a single spec: set RUN_ONLY to its spec_id.
#
# Pipeline: [run_method2_all_specs] calls 10 -> 10B -> 11 -> 11B -> 12 per spec
#
#-------------------------------------------------------------------------------
# CHANGES vs previous version
#
#  1. SPEC_GRID no longer has a "balance" column, and both balance modes are
#     now always requested (active_balance <- c("balanced","unbalanced")).
#     Reason: script 10 already builds BOTH balance modes unconditionally on
#     every run regardless of active_balance -- that config value only ever
#     controlled what 11/12/10B select to process. The previous grid had a
#     "headline" row (narrow, 2, balanced) and an "unbal" row (narrow, 2,
#     unbalanced) that produced the IDENTICAL sample_tag ("narrow_micro2plus").
#     manifest_M2.csv and fits_M2_<event>.rds are written at the sample_tag
#     level (not per-balance), so the second spec to run for a given
#     sample_tag silently overwrote the first's aggregate files -- e.g. with
#     the old grid, running "unbal" last would destroy "headline"'s manifest.
#     Processing both balance modes in one pass per sample_tag removes the
#     collision entirely, since each sample_tag is now only ever run once.
#
#  2. 10B_panel_attrition.R added to the pipeline (was previously left out,
#     so "run everything in one go" silently skipped the attrition tables).
#
#  3. Removed a stray leftover debug line that used to sit after the loop
#     (`table(baseline_obs$baseline_qtr, baseline_obs$treat)`), which
#     referenced a global left over from script 11's last iteration and
#     would error on a fresh session before 11 ever ran.
#
#  4. SCRIPT_DIR file names below use spaces (matching the names scripts
#     10-12 have used historically). CHECK THIS against what's actually on
#     disk -- some recent exports/uploads of these scripts have used
#     underscores instead (e.g. "10_building_individual_panel.R"). If your
#     files are underscore-named, update the SCRIPTS vector below to match,
#     or the file.exists() check at STEP 2 halts before spec 1 starts.
#
#  5. SPEC_GRID gained a baseline_rule column (config$method2$baseline_rule).
#     "first_qtr_only" is the recommended default (fixed calendar baseline);
#     "any_pre_first" and "all_pre_qtrs" are included as explicit robustness
#     rows (bl_anyfirst, bl_allpre) rather than left as an untested option --
#     see 10B_panel_attrition.R and 11B_baseline_cohort_check.R for what each
#     rule means and why first_qtr_only is preferred.
#
#  6. 13_regression_comparisons.R added to the pipeline, run immediately
#     after 12 for each spec so the across-window / across-arm comparison
#     tables get built automatically every time.
#
#===============================================================================

# At the top of run_panel_regs.R, replacing the current bare source() call:
if (exists("config", envir = .GlobalEnv)) rm(config, envir = .GlobalEnv)
source(here::here("Code", "R", "clean scripts", "00_setup.R"), local = FALSE)


cat("=== run_method2_all_specs.R ===\n\n")


#===============================================================================
# STEP 1. Define the specification grid
#===============================================================================

# Each row is one spec. Scripts 10/10B/11/11B/12 run once per row, covering
# BOTH balance modes within that one run.
#
# control_bw:    "all" (Small 11-50) or "narrow" (Small 11-20)
# treat_min_fs:  NA_integer_ = all micro 1-10; 2 = exclude solo; 3 = exclude 1-2 person
# baseline_rule: "first_qtr_only" (recommended), "any_pre_first" (old
#                default, person-specific), or "all_pre_qtrs" (robustness)

SPEC_GRID <- tibble::tribble(
  ~spec_id,     ~control_bw, ~treat_min_fs, ~baseline_rule,
  
  # --- Headline ---
  "headline",   "narrow",    2L,            "first_qtr_only",
  
  # --- Robustness: bandwidth ---
  "wide_bw",    "all",       2L,            "first_qtr_only",
  
  # --- Robustness: firm size floor ---
  "micro3plus", "narrow",    3L,            "first_qtr_only",
  "micro_all",  "narrow",    NA_integer_,   "first_qtr_only",
  
  # --- Robustness: baseline-assignment rule (see 10B_panel_attrition.R /
  # 11B_baseline_cohort_check.R for what each rule means) ---
  "bl_anyfirst", "narrow",   2L,            "any_pre_first",
  "bl_allpre",   "narrow",   2L,            "all_pre_qtrs"
)

# Set to a spec_id string to run only that spec, or NULL to run all.
RUN_ONLY <- NULL

cat("Specification grid:\n")
print(as.data.frame(SPEC_GRID), row.names = FALSE)
cat("\n")

if (!is.null(RUN_ONLY)) {
  SPEC_GRID <- SPEC_GRID %>% filter(spec_id %in% RUN_ONLY)
  cat(sprintf("  ** Running only: %s **\n\n",
              paste(RUN_ONLY, collapse = ", ")))
}


#===============================================================================
# STEP 2. Script paths
#===============================================================================

SCRIPT_DIR <- here::here("Code", "R", "clean scripts")

# NOTE: verify these file names match what's actually on disk -- see CHANGE 4
# in the header above.
SCRIPTS <- c(
  "10_building individual panel.R",
  "10B_panel attrition.R",
  "11_validate panels.R",
  "11B_baseline cohort check.R",
  "12_individual estimation.R",
  "13_regression comparisons.R"
)

# Verify all scripts exist
for (s in SCRIPTS) {
  fp <- file.path(SCRIPT_DIR, s)
  if (!file.exists(fp)) stop("Script not found: ", fp)
}


#===============================================================================
# STEP 3. Run each specification
#===============================================================================

run_log <- list()
t_start_all <- Sys.time()

for (i in seq_len(nrow(SPEC_GRID))) {
  
  # Extract fields as plain scalars (avoids any tibble indexing surprises)
  .run_spec <- list(
    spec_id       = SPEC_GRID$spec_id[i],
    control_bw    = SPEC_GRID$control_bw[i],
    treat_min_fs  = SPEC_GRID$treat_min_fs[i],
    baseline_rule = SPEC_GRID$baseline_rule[i]
  )
  
  cat(sprintf("\n%s\n", strrep("=", 72)))
  cat(sprintf("  SPEC %d/%d: %s\n", i, nrow(SPEC_GRID), .run_spec$spec_id))
  cat(sprintf("    control_bw = %s | treat_min_fs = %s | baseline_rule = %s\n",
              .run_spec$control_bw,
              ifelse(is.na(.run_spec$treat_min_fs), "NA (no restriction)",
                     as.character(.run_spec$treat_min_fs)),
              .run_spec$baseline_rule))
  cat(sprintf("%s\n\n", strrep("=", 72)))
  
  t_spec_start <- Sys.time()
  
  
  # --- Override config in the global environment ---
  # Scripts 10-12 guard their source("00_setup.R") call with
  # if (!exists("config")), so these overrides persist across scripts.
  # NA_integer_ in the grid means "no restriction" -> NULL in config.
  mfs <- .run_spec$treat_min_fs
  if (is.na(mfs)) mfs <- NULL
  
  .GlobalEnv$config$method2$control_bandwidth     <- .run_spec$control_bw
  .GlobalEnv$config$method2$treatment_min_firmsize <- mfs
  .GlobalEnv$config$method2$baseline_rule          <- .run_spec$baseline_rule
  # Always both -- see CHANGE 1 above.
  .GlobalEnv$config$method2$active_balance         <- c("balanced", "unbalanced")
  
  # Rebuild dynamic labels (mirrors the block at end of 00_config.R)
  .GlobalEnv$config$m2_labels <- {
    bw  <- config$method2$control_bandwidth
    mfs <- config$method2$treatment_min_firmsize
    
    ctrl <- if (bw == "narrow") "Small (11-20)" else "Small (11-50)"
    # Must match 00_config.R's fix: Micro is legally up to and including 10
    # workers in the DR (CNS resolutions), so the upper bound here is 10,
    # not 9. This is a label-text fix only -- M2_TREAT_MIN_FS was already a
    # lower-bound-only filter, so the actual sample was always correct.
    treat <- if (!is.null(mfs) && mfs > 1) {
      sprintf("Micro (%d-10)", mfs)
    } else {
      "Micro (1-10)"
    }
    
    list(treatment = treat, control = ctrl)
  }
  
  cat(sprintf("    Labels: Treatment = %s | Control = %s\n\n",
              config$m2_labels$treatment, config$m2_labels$control))
  
  
  # --- Source scripts in order ---
  spec_ok <- TRUE
  
  for (s in SCRIPTS) {
    
    fp <- file.path(SCRIPT_DIR, s)
    cat(sprintf("  >>> Sourcing %s ...\n", s))
    
    tryCatch({
      source(fp, local = FALSE)  # run in global env so config is visible
    }, error = function(e) {
      cat(sprintf("\n  !!! ERROR in %s: %s\n\n", s, e$message))
      spec_ok <<- FALSE
    })
    
    if (!spec_ok) {
      cat(sprintf("  Stopping spec '%s' due to error.\n", .run_spec$spec_id))
      break
    }
  }
  
  t_spec_end <- Sys.time()
  elapsed <- round(difftime(t_spec_end, t_spec_start, units = "mins"), 1)
  
  run_log[[.run_spec$spec_id]] <- list(
    spec_id       = .run_spec$spec_id,
    control_bw    = .run_spec$control_bw,
    treat_min     = ifelse(is.null(mfs), NA_integer_, mfs),
    baseline_rule = .run_spec$baseline_rule,
    success       = spec_ok,
    minutes       = as.numeric(elapsed)
  )
  
  cat(sprintf("\n  Spec '%s' %s in %.1f minutes.\n",
              .run_spec$spec_id,
              ifelse(spec_ok, "completed", "FAILED"),
              as.numeric(elapsed)))
}


#===============================================================================
# STEP 4. Summary
#===============================================================================

t_total <- round(difftime(Sys.time(), t_start_all, units = "mins"), 1)

cat(sprintf("\n%s\n", strrep("=", 72)))
cat(sprintf("  ALL SPECS COMPLETE — %.1f minutes total\n", as.numeric(t_total)))
cat(sprintf("%s\n\n", strrep("=", 72)))

summary_tbl <- purrr::map_dfr(run_log, tibble::as_tibble)
print(as.data.frame(summary_tbl), row.names = FALSE)


#===============================================================================
# STEP 5. Cross-spec comparisons (once, after ALL specs are built)
#
# 14_cross_spec_comparisons.R compares ACROSS treatment/control/
# baseline_rule combinations, so it needs to run once the whole sweep above
# is done -- unlike 10-13, it does not belong inside the per-spec loop
# (running it mid-sweep would only see whichever specs had finished so
# far). Config at this point still reflects the LAST spec in SPEC_GRID, but
# 14 only uses config for M2_EVENT/M2_WINDOWS and discovers everything else
# by scanning the filesystem, so that's harmless.
#===============================================================================

cat(sprintf("\n%s\n", strrep("=", 72)))
cat("  Cross-spec comparisons (14_cross_spec_comparisons.R)\n")
cat(sprintf("%s\n\n", strrep("=", 72)))

tryCatch({
  source(file.path(SCRIPT_DIR, "14_cross spec comparisons.R"), local = FALSE)
}, error = function(e) {
  cat(sprintf("\n  !!! ERROR in 14_cross spec comparisons.R: %s\n\n", e$message))
})

# Restore config to the file defaults by re-sourcing
cat("\nRestoring config to file defaults...\n")
rm(config, envir = .GlobalEnv)
source(here::here("Code", "R", "clean scripts", "00_setup.R"), local = FALSE)

cat("\n=== run_method2_all_specs.R complete ===\n")