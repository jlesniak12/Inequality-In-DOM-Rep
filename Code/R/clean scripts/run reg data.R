#==============================================================================
#
# Script: run_all_exposures.R
#
# Purpose: Run 08A across the full baseline x income grid. Four outputs per run.
#
# Not part of the numbered pipeline. Live drivers file - reorder / trim the grid
# during development, expand it back for a full production build.
#
# Usage (fresh R session):
#   source(here::here("Code", "R", "drivers", "run_all_exposures.R"))
#
#===============================================================================

source(here::here("Code", "R", "clean scripts", "00_setup.R"))

# Load `samples` ONCE. 07A skips its own source() of 03 when this exists.
# Big win: 03 builds survey design objects over the full ENCFT panel.
cat("[driver] Sourcing 03 once...\n")
source(file.path(config$paths$scripts, "03_sample definitions.R"))
stopifnot(exists("samples"), !is.null(samples$reg_tier$data))

grid <- tidyr::expand_grid(
  baseline = names(config$baselines),
  income   = names(config$income_specs)
)

cat("=== run_all_exposures.R ===\n")
cat(sprintf("Grid: %d runs\n\n", nrow(grid)))
print(grid)

for (i in seq_len(nrow(grid))) {
  # Defensive: fail here (not 20 lines into 07A) if config isn't where it
  # should be. Assign via .GlobalEnv so the driver still works if invoked
  # from a job pane, local() block, or nested source() where <<- semantics
  # would otherwise be confusing.
  if (!exists("config", envir = .GlobalEnv, inherits = FALSE)) {
    stop("`config` not found in .GlobalEnv at iteration ", i,
         ". Source 00_setup.R (and 03) before running this driver.",
         call. = FALSE)
  }
  
  .GlobalEnv$config$active_baseline <- grid$baseline[i]
  .GlobalEnv$config$active_income   <- grid$income[i]
  
  cat(sprintf("\n\n=== [%d/%d] baseline=%s | income=%s ===\n",
              i, nrow(grid), grid$baseline[i], grid$income[i]))
  
  source(file.path(config$paths$scripts, "08_prepare regression data.R"),
         local = FALSE)
}

cat("\n=== run_all_exposures.R complete ===\n")
