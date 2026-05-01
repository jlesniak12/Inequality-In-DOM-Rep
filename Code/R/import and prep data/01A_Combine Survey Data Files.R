#===============================================================================
#
# Scope: This file takes Excel data files with ENCFT survey data produced by 
#        Central Bank of Dom Rep and combines them to create an R data object
#===============================================================================



source("Code/R/setup/00_setup.R")

#parameters from project config file
start_yr <- config$first_year
end_yr <- config$last_year
years <- start_yr:end_yr

var_list_members <- config$var_imports
  


#define coverage q1-4 default and partial end years. Used for file names
encft_year_coverage <- function(year) {
  if (year == 2014) return(c(q_start = 3, q_end = 4))
  c(q_start = 1, q_end = 4)
}

#build file names
encft_filename <- function(year) {
  
  cov <- encft_year_coverage(year)
  a <- paste0(year, cov["q_start"])
  b <- paste0(year, cov["q_end"])
  
  candidates <- c(
    sprintf("Base ENCFT %s - %s.xlsx", a, b),
    sprintf("BASE ENCFT %s - %s.xlsx", a, b),
    sprintf("Base ENCFT %s-%s.xlsx", a, b) # just in case someone removes spaces
  )
  
  # Return the first candidate that exists
  dir_year <- file.path(config$paths$raw_data, "ENCFT", "Data", as.character(year))
  hit <- candidates[file.exists(file.path(dir_year, candidates))]
  
  if (!length(hit)) {
    stop("No ENCFT file found for year ", year, " in ", dir_year,
         "\nTried: ", paste(candidates, collapse = " | "),
         call. = FALSE)
  }
  hit[[1]]
}

#build file paths for the excel
encft_path <- function(year) {
  file.path(
    config$paths$raw_data,
    "ENCFT", "Data",
    as.character(year),
    encft_filename(year)
  )
}

#list to store each year loaded
list_df <- vector("list", length(years))
names(list_df) <- as.character(years)

#load files by year
for (year in years) {
  
  fpath <- encft_path(year)
  if (!file.exists(fpath)) stop("Missing ENCFT file: ", fpath, call. = FALSE)
  
 
  
  #filter to variables of interest and make some type corrections
  data <- readxl::read_excel(fpath, sheet = "Miembros") %>%
    dplyr::select(dplyr::all_of(var_list_members)) %>%
    dplyr::mutate(
      year = year,
      
      OCUPACION_PRINCIPAL_COD = as.character(OCUPACION_PRINCIPAL_COD),
      
      BONO_VACACIONES        = suppressWarnings(as.numeric(BONO_VACACIONES)),
      BONIFICACIONES         = suppressWarnings(as.numeric(BONIFICACIONES)),
      REGALIA_PASCUAL        = suppressWarnings(as.numeric(REGALIA_PASCUAL)),
      INCENTIVO_ANTIGUEDAD   = suppressWarnings(as.numeric(INCENTIVO_ANTIGUEDAD)),
      OTROS_BENEFICIOS       = suppressWarnings(as.numeric(OTROS_BENEFICIOS)),
      OTROS_BENEFICIOS_SECUN = suppressWarnings(as.numeric(OTROS_BENEFICIOS_SECUN)),
      INGRESO_INDEPENDIENTES = suppressWarnings(as.numeric(INGRESO_INDEPENDIENTES)),
      
      ID_HOGAR   = stringr::str_pad(as.character(ID_HOGAR),   7, "left", "0"),
      ID_PERSONA = stringr::str_pad(as.character(ID_PERSONA), 9, "left", "0"),
      OCUPACION_PRINCIPAL_COD = stringr::str_pad(OCUPACION_PRINCIPAL_COD, 4, "left", "0")
    )
  
  list_df[[as.character(year)]] <- data
}

all_ENCFT_data <- dplyr::bind_rows(list_df)

out_file <- file.path(config$paths$processed_data, "Full_ENCFT.rds")
saveRDS(all_ENCFT_data, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))





