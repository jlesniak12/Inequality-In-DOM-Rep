#===============================================================================
# 
# First file to run in project analyzing inequality in Dominican Republic.
# Accomplishes the following:
#
# 1) Load config file for common project parameters.
# 2) Load files containing additional custom functions used throughout 
#    the project.
# 3) Define package list needed for project and check if installed.
# 4) Install necessary packages.
# 5) Make sure output folders defined in config exist.

# This Setup script should be run before running most
# other files in the project.
#
# =============================================================================





#load configs
source("Code/R/00_config.R")


#load other functions
source("Code/R/00_functions_weighted_exposure.R")
source("Code/R/00_functions_plotting.R")

source("Code/R/00_extra_functions.R")


# --- check for and install packages --- #

#packages used
pkgs <- c("readxl", "openxlsx", "zoo", "rlang", "lubridate", "forcats",
          "dplyr","tidyr","readr","purrr", "tibble", "stringr",
          "survey", "convey",
          "ggplot2", "patchwork",
          "gtsummary")

#install any packages needed
INSTALL_MISSING <- TRUE
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing)) {
  if (INSTALL_MISSING) {
    install.packages(missing)
  } else {
    stop("Missing packages: ", paste(missing, collapse = ", "))
  }
}

#load packages
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- SurveyTools (install only if missing) ---- #
if (!requireNamespace("SurveyTools", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("jlesniak12/SurveyTools")
}
library(SurveyTools)




# --- Settings for Files and Paths --- #

#define output root folder and create
output_root <- file.path(config$paths$outputs, config$output_stage)
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

# Create standard output subfolders
for (nm in names(config$out_subdirs)) {
  dir.create(file.path(output_root, config$out_subdirs[[nm]]),
             recursive = TRUE, showWarnings = FALSE)
}

