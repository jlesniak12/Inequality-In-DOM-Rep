#===============================================================================
# run_all.R
#
# Master driver: runs the full pipeline for each geography.
# Loops over geographies, and within each geography the individual runners
# loop over baseline × income.
#
# Usage:
#   source("run_all.R")                    # runs both geographies
#   source("run_all.R"); run_geo("Region10")  # one geography only
#
# What runs (in order):
#   run_exposure.R       -> 07A (exposure construction)
#   run_reg_data.R       -> 08  (panel creation)
#   run_exposure_check.R -> 07C + 08B (validation diagnostics)
#   run_estimation.R     -> 09B (tables, event studies, manifest)
#
# Prerequisites:
#   01A, 01B, 02, 03 must have been run already (data import, variable
#   construction, sample definitions). These are geography-independent.
#===============================================================================

source(here::here("Code", "R", "clean scripts", "00_setup.R"))


run_step <- function(label, script) {
  cat(sprintf("\n--- %s ---\n", label))
  t_step <- Sys.time()
  source(file.path(config$paths$scripts, script), local = FALSE)
  elapsed <- round(difftime(Sys.time(), t_step, units = "mins"), 1)
  cat(sprintf("    %s finished in %s min\n", label, elapsed))
}

run_geo <- function(geo) {
  
  cat("\n")
  cat("================================================================\n")
  cat(sprintf("  GEOGRAPHY: %s\n", geo))
  cat("================================================================\n\n")
  
  t_geo <- Sys.time()
  
  run_step("Exposure (07A)",    "run_exposure.R")
  run_step("Panel (08)",        "run_reg data.R")
  run_step("Validation (07C+08B)", "run_exposure check.R")
  run_step("Estimation (09B)",  "run_estimation.R")
  
  elapsed <- round(difftime(Sys.time(), t_geo, units = "mins"), 1)
  cat(sprintf("\n  %s total: %s min\n\n", geo, elapsed))
}

# --- Run all geographies ---
t0 <- Sys.time()

run_geo("provinces")


elapsed <- round(difftime(Sys.time(), t0, units = "mins"), 1)
cat(sprintf("\nAll done. Total time: %s min.\n", elapsed))