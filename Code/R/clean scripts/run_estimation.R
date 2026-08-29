#===============================================================================
# Script: run_all_estimations.R
#
# Purpose: Loop 09B over the full (income x baseline) grid. Same pattern as
#          run_all_exposures.R. Not part of the numbered pipeline; live drivers
#          file, reorder / trim the grid during development.
#
# Prereqs: 07A must have run first (produces exposure files); 08 must have run
#          for every (income, baseline) combination (produces panel files).
#          The cleanest sequence is:
#              source(run_all_exposures.R)     # 07A across grid
#              source(run_all_panels.R)        # 08 across grid (add if needed)
#              source(run_all_estimations.R)   # this
#
# Usage (fresh R session):
#   source(here::here("Code", "R", "drivers", "run_all_estimations.R"))
#
# Truncates manifest.csv at driver start so a full-grid run produces one fresh
# master CSV.
#===============================================================================

source(here::here("Code", "R", "clean scripts", "00_setup.R"))

# Truncate manifest so 09B's append-mode writes to a fresh file. Standalone
# 09B calls will still append to whatever's here after this driver runs.
mf_path <- file.path(config$out_dirs$reg_results, "manifest.csv")

if (file.exists(mf_path)) {
  cat("[driver] Removing existing manifest.csv\n")
  file.remove(mf_path)
}

grid <- tidyr::expand_grid(
  income   = names(config$income_specs),
  baseline = names(config$baselines)
)

cat("=== run_all_estimations.R ===\n")
cat(sprintf("Grid: %d runs\n\n", nrow(grid)))
print(grid)

for (i in seq_len(nrow(grid))) {
  if (!exists("config", envir = .GlobalEnv, inherits = FALSE)) {
    stop("`config` not found in .GlobalEnv at iteration ", i,
         ". Source 00_setup.R before running this driver.",
         call. = FALSE)
  }
  
  .GlobalEnv$config$active_baseline <- grid$baseline[i]
  .GlobalEnv$config$active_income   <- grid$income[i]
  
  cat(sprintf("\n\n=== [%d/%d] income=%s | baseline=%s ===\n",
              i, nrow(grid), grid$income[i], grid$baseline[i]))
  
  source(file.path(config$paths$scripts, "09B_main estimation.R"),
         local = FALSE)
}

cat("\n=== run_all_estimations.R complete ===\n")
cat(sprintf("Master manifest: %s\n", mf_path))