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

source(file.path(fun_dir, "fun_general utilities.R"))
source(file.path(fun_dir, "fun_plotting.R"))
source(file.path(fun_dir, "fun_weighted exposure.R"))
source(file.path(fun_dir, "extra_functions.R"))


# --- 4. Create the standard output folder tree defined in config script. --- #
for (p in c(config$data_dirs, config$out_dirs)) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}



 
 
 
 