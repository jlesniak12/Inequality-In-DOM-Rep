#===============================================================================
# 
# First file to run in project analyzing inequality in Dominican Republic.
# Completes the following setup:
#
# 1) Define list of packages used in project and install from CRAN
# 2) Load custom surveytools package from github.
# 3) Load Config file and extra functions.
# 4) Create project folder structure.

# This Setup script should be run before running most other files in the project.
#
# =============================================================================


# --- 1. Define List of packages and Load --- #
pkgs <- c("here", "readxl", "openxlsx", "zoo", "rlang", "lubridate", "forcats",
          "dplyr", "tidyr", "readr", "purrr", "tibble", "stringr",
          "survey", "convey",
          "ggplot2", "scales", "patchwork", "ggrepel",
          "gtsummary", "gt")

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- 2. Load SurveyTools (GitHub; install only if missing) ---- #
if (!requireNamespace("SurveyTools", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("jlesniak12/SurveyTools")
}
library(SurveyTools)



# --- 3. Load config and extra functions --- #

here::i_am("Code/R/clean scripts/00_setup.R")

source(here::here("Code", "R", "clean scripts", "00_config.R"))

#Load project functions VIA config (single source of truth for the "clean scripts" folder name).
fun_dir <- file.path(config$paths$scripts, "functions")

source(file.path(fun_dir, "functions_weighted_exposure.R"))
source(file.path(fun_dir, "functions_plotting.R"))
source(file.path(fun_dir, "extra_functions.R"))


# --- 4. Create the standard output folder tree defined in config script. --- #
output_root <- file.path(config$paths$outputs, config$output_stage)
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

for (nm in names(config$out_subdirs)) {
  dir.create(file.path(output_root, config$out_subdirs[[nm]]),
             recursive = TRUE, showWarnings = FALSE)
}

for (p in config$data_dirs) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}



MW_EVENT_QTR   <- config$events$event_qtrs
TIER_LEVELS    <- config$TIER_LEVELS
DEFLATOR_LABEL <- sprintf("%dQ%d", config$CPI_base_year, config$CPI_base_qtr)
SAMPLE_LABEL   <- sprintf("%s-%s", config$sample$start_qtr, config$sample$end_qtr)

SRC <- sprintf("Sources: ENCFT %s; Central Bank of Dominican Republic.",
              SAMPLE_LABEL)

MW_NOTE <- paste(
sprintf("Red dashed verticals: MW announcement quarters (%s).",
           paste(MW_EVENT_QTR, collapse = ", ")),
   sprintf("Grey shading: %s-%s (COVID-19).",
          config$events$covid_qtrs[1],
           config$events$covid_qtrs[length(config$events$covid_qtrs)])
 )

# # Event-line positions on a discrete "YYYYQn" x-axis. `qtrs` must be the
# # quarter column of the data actually being plotted (after any filtering),
# # because ggplot orders a discrete axis by sort(unique(x)).
 event_pos <- function(qtrs, events = MW_EVENT_QTR) {
 which(sort(unique(as.character(qtrs))) %in% events)
 }

 covid_rect <- function(qtrs) {
   q    <- sort(unique(as.character(qtrs)))
   xmin <- which(q == config$events$covid_qtrs[1])
   xmax <- which(q == config$events$covid_qtrs[length(config$events$covid_qtrs)])
   if (!length(xmin) || !length(xmax)) return(NULL)
   ggplot2::annotate("rect", xmin = xmin - 0.5, xmax = xmax + 0.5,
                     ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.6)
 }
#
 qtr_breaks <- function(qtrs, every = 4) {
   q <- sort(unique(as.character(qtrs)))
   q[seq(1, length(q), by = every)]
 }
#
# # Quarter arithmetic: shift "YYYYQn" by k quarters.
 qshift <- function(qtr, k) {
   y   <- as.integer(substr(qtr, 1, 4))
   q   <- as.integer(substr(qtr, 6, 6))
   idx <- y * 4L + (q - 1L) + k
   paste0(idx %/% 4L, "Q", idx %% 4L + 1L)
 }
 
 
 
 # Build tagged filenames following: <item>__<income>__<baseline>__<geo>.rds
 #
 # Defaults pull from the currently-active baseline / income / geo in config.
 # Any argument can be overridden explicitly to read a file from a different
 # spec than the active one (e.g. reading a base2016 exposure while running a
 # base2021q2 estimation).
 #
 # Examples:
 #   mw_file("exposure_geo")
 #     -> file.path(pd, "exposure_geo__hourly__base2016_all_tiers__Region10.rds")
 #
 #   mw_file("panel_geo_quarter", baseline_tag = "base2016_all_tiers")
 #     -> reads the 2016 baseline panel even during a 2021q2 driver iteration
 #
 mw_file <- function(item,
                     dir,
                     income_tag   = NULL,
                     baseline_tag = NULL,
                     geo          = NULL) {
   
   income_tag   <- income_tag   %||% config$income_specs[[config$active_income]]$tag
   baseline_tag <- baseline_tag %||% config$baselines[[config$active_baseline]]$tag
   geo          <- geo          %||% config$exposure$construct_geo
   
   missing <- c(dir          = missing(dir) || is.null(dir) || !nzchar(dir),
                income_tag   = is.null(income_tag)   || !nzchar(income_tag),
                baseline_tag = is.null(baseline_tag) || !nzchar(baseline_tag),
                geo          = is.null(geo)          || !nzchar(geo))
   if (any(missing)) {
     stop("mw_file('", item, "'): missing arg(s): ",
          paste(names(missing)[missing], collapse = ", "), ".", call. = FALSE)
   }
   
   file.path(dir,
             paste0(paste(item, income_tag, baseline_tag, geo, sep = "__"),
                    ".rds"))
 }
 
 
 
 