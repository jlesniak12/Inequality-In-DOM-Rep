#===============================================================================
#
# Script: run_method2_all_specs.R
#
# Purpose: Run scripts 10 -> 11 -> 12 for every specification in the grid
#          without manually editing config each time.
#
# Usage:   source(here::here("Code", "R", "clean scripts", "run_method2_all_specs.R"))
#
# How it works:
#   1. Sources 00_setup.R once to load packages and the base config.
#   2. Defines a grid of specifications (control bandwidth x treatment min
#      firm size x balance modes).
#   3. For each row in the grid, overrides the relevant config values in
#      memory, rebuilds the dynamic labels, and sources scripts 10-12.
#   4. Folder structure keeps everything separated — each spec gets its own
#      sample_tag directory, and each balance mode gets its own subfolder.
#
# To add/remove specs: edit the SPEC_GRID definition below.
# To run a single spec: set RUN_ONLY to its row number, or just set config
#   directly and source scripts 10-12 individually.
#
# Pipeline: [run_method2_all_specs] calls 10 -> 11 -> 12 per spec
#
#===============================================================================

source(here::here("Code", "R", "clean scripts", "00_setup.R"), local = FALSE)

cat("=== run_method2_all_specs.R ===\n\n")


#===============================================================================
# STEP 1. Define the specification grid
#===============================================================================

# Each row is one spec. Scripts 10/11/12 will run once per row.
#
# control_bw:  "all" (Small 11-50) or "narrow" (Small 11-20)
# treat_min_fs: NULL = all micro 1-9; 2 = exclude solo; 3 = exclude 1-2 person
# balance:     which balance modes scripts 11/12 should run.
#              Script 10 always builds both balanced and unbalanced.

SPEC_GRID <- tibble::tribble(
  ~spec_id, ~control_bw, ~treat_min_fs, ~balance,
  
  # --- Headline ---
  "headline",   "narrow",    2L,           "balanced",
  
  # --- Robustness: bandwidth ---
  "wide_bw",    "all",       2L,           "balanced",
  
  # --- Robustness: firm size floor ---
  "micro3plus", "narrow",    3L,           "balanced",
  "micro_all",  "narrow",    NA_integer_,  "balanced",
  
  # --- Robustness: unbalanced panel ---
  "unbal",      "narrow",    2L,           "unbalanced"
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

SCRIPTS <- c(
  "10_building individual panel.R",
  "11_validate panels.R",
  "12_individual estimation.R"
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
    spec_id      = SPEC_GRID$spec_id[i],
    control_bw   = SPEC_GRID$control_bw[i],
    treat_min_fs = SPEC_GRID$treat_min_fs[i],
    balance      = SPEC_GRID$balance[i]
  )
  
  cat(sprintf("\n%s\n", strrep("=", 72)))
  cat(sprintf("  SPEC %d/%d: %s\n", i, nrow(SPEC_GRID), .run_spec$spec_id))
  cat(sprintf("    control_bw = %s | treat_min_fs = %s | balance = %s\n",
              .run_spec$control_bw,
              ifelse(is.null(.run_spec$treat_min_fs), "NULL",
                     as.character(.run_spec$treat_min_fs)),
              .run_spec$balance))
  cat(sprintf("%s\n\n", strrep("=", 72)))
  
  t_spec_start <- Sys.time()
  
  
  # --- Override config in the global environment ---
  # Scripts 10-12 guard their source("00_setup.R") call with
  # if (!exists("config")), so these overrides persist across scripts.
  # NA_integer_ in the grid means "no restriction" -> NULL in config.
  mfs <- .run_spec$treat_min_fs
  if (is.na(mfs)) mfs <- NULL
  
  .GlobalEnv$config$method2$control_bandwidth      <- .run_spec$control_bw
  .GlobalEnv$config$method2$treatment_min_firmsize  <- mfs
  .GlobalEnv$config$method2$active_balance          <- .run_spec$balance
  
  # Rebuild dynamic labels (mirrors the block at end of 00_config.R)
  .GlobalEnv$config$m2_labels <- {
    bw  <- config$method2$control_bandwidth
    mfs <- config$method2$treatment_min_firmsize
    
    ctrl <- if (bw == "narrow") "Small (11-20)" else "Small (11-50)"
    treat <- if (!is.null(mfs) && mfs > 1) {
      sprintf("Micro (%d-9)", mfs)
    } else {
      "Micro (<10)"
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
    spec_id    = .run_spec$spec_id,
    control_bw = .run_spec$control_bw,
    treat_min  = ifelse(is.null(mfs), NA_integer_, mfs),
    balance    = .run_spec$balance,
    success    = spec_ok,
    minutes    = as.numeric(elapsed)
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

# Restore config to the file defaults by re-sourcing
cat("\nRestoring config to file defaults...\n")
rm(config, envir = .GlobalEnv)
source(here::here("Code", "R", "clean scripts", "00_setup.R"), local = FALSE)

cat("\n=== run_method2_all_specs.R complete ===\n")


table(baseline_obs$baseline_qtr, baseline_obs$treat)
