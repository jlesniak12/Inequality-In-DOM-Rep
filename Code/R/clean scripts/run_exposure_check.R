#===============================================================================
  #
  # Script: run_all_validation.R
  #
  # Purpose: Run 07C (exposure validation) + 08B (descriptive exposure/panel
  #          validation) across the full (income x baseline) grid. Both are
  #          diagnostic scripts that read already-computed files - fast, no
  #          survey compute of their own.
  #
  # Re-run whenever 07C / 08B change, or after re-running 07A / 08 with a new
  # spec.
  #
  # Usage (fresh R session):
  #   source(here::here("Code", "R", "drivers", "run_all_validation.R"))
  #
  # Prereqs:
  #   run_all_exposures.R    (produces exposure_geo, exposure_cells files)
  #   run_all_panels.R       (produces panel_geo_quarter files)
  #
  # 07C and 08B both check for their inputs and error early with actionable
  # messages if anything is missing.
  #
  #===============================================================================

source(here::here("Code", "R", "clean scripts", "00_setup.R"))

# 07C needs `samples` from 03; 08B doesn't (reads only tagged files). Source
# 03 once so 07C reuses it across iterations.
cat("[driver] Sourcing 03 once...\n")
source(file.path(config$paths$scripts, "03_sample definitions.R"))
stopifnot(exists("samples"), !is.null(samples$reg_tier$data))

grid <- tidyr::expand_grid(
  income   = names(config$income_specs),
  baseline = names(config$baselines)
)

cat("=== run_all_validation.R ===\n")
cat(sprintf("Grid: %d runs\n\n", nrow(grid)))
print(grid)

for (i in seq_len(nrow(grid))) {
  if (!exists("config", envir = .GlobalEnv, inherits = FALSE)) {
    stop("`config` not found in .GlobalEnv at iteration ", i, ".", call. = FALSE)
  }
  
  .GlobalEnv$config$active_baseline <- grid$baseline[i]
  .GlobalEnv$config$active_income   <- grid$income[i]
  
  cat(sprintf("\n\n=== [%d/%d] income=%s | baseline=%s ===\n",
              i, nrow(grid), grid$income[i], grid$baseline[i]))
  
  # 07C first (exposure diagnostics), then 08B (descriptive on exposure + panel).
  # If 07C fails (e.g. bad identity check), 08B still runs so the panel-side
  # diagnostics are produced; iteration failures don't block subsequent combos.
  tryCatch(
    source(file.path(config$paths$scripts, "07C_exposure validation.R"),
           local = FALSE),
    error = function(e) {
      cat(sprintf("!!! [07C failed at %s/%s]: %s\n",
                  grid$income[i], grid$baseline[i], e$message))
    })
  
  tryCatch(
    source(file.path(config$paths$scripts, "08B_exposure validation.R"),
           local = FALSE),
    error = function(e) {
      cat(sprintf("!!! [08B failed at %s/%s]: %s\n",
                  grid$income[i], grid$baseline[i], e$message))
    })
}

cat("\n=== run_all_validation.R complete ===\n")