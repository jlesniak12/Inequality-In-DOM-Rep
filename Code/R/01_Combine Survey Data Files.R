


source("Code/R/00_setup.R")

#parameters
start_yr <- 2014
end_yr <- 2025
years <- start_yr:end_yr

var_list_members <- c("TRIMESTRE", "PERIODO", "ESTRATO",	"DES_ESTRATO",
                 "FACTOR_EXPANSION", "UPM", "VIVIENDA", "ID_HOGAR", "MIEMBRO", "ID_PERSONA",
                 "ID_PROVINCIA", "DES_PROVINCIA", "GRUPO_REGION", "ORDEN_REGION",
                 "SEXO", "EDAD", "PARENTESCO", "PAIS_NACIMIENTO",
                 "OCUPADO", "SUBOCUPADO", "PEA", "DESOCUPADO","INACTIVO",
                 "ORDEN_RAMA", "GRUPO_RAMA", "ORDEN_OCUPACION", "GRUPO_OCUPACION", "ORDEN_CATEGORIA", "GRUPO_CATEGORIA", 
                 "ORDEN_EDUCACION", "GRUPO_EDUCACION", "GRUPO_EMPLEO",
                 "INGRESO_ASALARIADO", "COMISIONES", "PROPINAS", "HORAS_EXTRA", "OTROS_PAGOS",
                 "BONO_VACACIONES", "BONIFICACIONES", "REGALIA_PASCUAL", "INCENTIVO_ANTIGUEDAD", "OTROS_BENEFICIOS",
                 "ESPECIE_ALIMENTOS", "ESPECIE_VIVIENDA", "ESPECIE_TRANSPORTE", "ESPECIE_COMBUSTIBLE", "ESPECIE_CELULAR", "OTROS_ESPECIE",
                 "INGRESO_INDEPENDIENTES", "CONSUMO_BIENES", "ESPECIE_INDEPENDIENTES",
                 "INGRESO_ASALARIADO_SECUN", "OTROS_PAGOS_SECUN", "OTROS_BENEFICIOS_SECUN", "PAGO_ESPECIE_SECUN",
                 "INGRESO_INDEPENDIENTES_SECUN", "CONSUMO_BIENES_SECUN", "ESPECIE_INDEPENDIENTES_SECUN",
                 "OTROS_TRABAJOS",
                 "TOTAL_PERSONAS_TRABAJAN_EMP", "CANTIDAD_PERSONAS_TRABAJAN_EMP", "EMPRESA_INSCRITA_RNC", 
                 "HORAS_TRABAJO_EFECT_TOTAL", "HORAS_TRABAJA_SEMANA_PRINCIPAL", "RAZON_JORNADA_DIFERENTE"
)


#define coverage q1-4 default and partial end years
encft_year_coverage <- function(year) {
  if (year == 2014) return(c(q_start = 3, q_end = 4))
  if (year == 2025) return(c(q_start = 1, q_end = 2))
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

#build paths for the excel
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
  data <- readxl::read_excel(fpath, sheet = "Miembros") |>
    dplyr::select(dplyr::all_of(var_list_members)) |>
    dplyr::mutate(
      year = year,
      BONO_VACACIONES        = suppressWarnings(as.numeric(BONO_VACACIONES)),
      BONIFICACIONES         = suppressWarnings(as.numeric(BONIFICACIONES)),
      REGALIA_PASCUAL        = suppressWarnings(as.numeric(REGALIA_PASCUAL)),
      INCENTIVO_ANTIGUEDAD   = suppressWarnings(as.numeric(INCENTIVO_ANTIGUEDAD)),
      OTROS_BENEFICIOS       = suppressWarnings(as.numeric(OTROS_BENEFICIOS)),
      OTROS_BENEFICIOS_SECUN = suppressWarnings(as.numeric(OTROS_BENEFICIOS_SECUN)),
      INGRESO_INDEPENDIENTES = suppressWarnings(as.numeric(INGRESO_INDEPENDIENTES)),
      ID_HOGAR   = stringr::str_pad(as.character(ID_HOGAR),   7, "left", "0"),
      ID_PERSONA = stringr::str_pad(as.character(ID_PERSONA), 9, "left", "0")
    )
  
  list_df[[as.character(year)]] <- data
}

all_ENCFT_data <- dplyr::bind_rows(list_df)

out_file <- file.path(config$paths$processed_data, "Full_ENCFT.rds")
saveRDS(all_ENCFT_data, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))










#create dataframe using household members data
list_df <- list()
for (x in years) {
  
  
  #handle years with incomplete data
  if (x == 2025) {
    
    file_name <-  paste("/Base ENCFT ", paste(x,"1", sep=""), " - ", paste(x,"2", sep = ""), ".xlsx", sep = "")
  } 
  else if (x == 2014) {
    file_name <-  paste("/Base ENCFT ", paste(x,"3", sep=""), " - ", paste(x,"4", sep = ""), ".xlsx", sep = "")
  }
  
  else {
    file_name <-  paste("/Base ENCFT ", paste(x,"1", sep=""), " - ", paste(x,"4", sep = ""), ".xlsx", sep = "")
  }
 
  
  #import needed variables
  data <- read_excel(paste(paste("./Raw Datasets/ENCFT/Data", x, sep = "/"), file_name, sep=""),
                     sheet = "Miembros")
  
  data <- data %>%
    select(var_list_members)
  
  #set data types explicitly...
  data <- data %>%
    mutate(BONO_VACACIONES = as.numeric(BONO_VACACIONES),
           BONIFICACIONES= as.numeric(BONIFICACIONES),
           REGALIA_PASCUAL = as.numeric(REGALIA_PASCUAL),
           INCENTIVO_ANTIGUEDAD = as.numeric(INCENTIVO_ANTIGUEDAD),
           OTROS_BENEFICIOS = as.numeric(OTROS_BENEFICIOS),
           OTROS_BENEFICIOS_SECUN = as.numeric(OTROS_BENEFICIOS_SECUN),
           INGRESO_INDEPENDIENTES = as.numeric(INGRESO_INDEPENDIENTES),
           )
  
  # convert to character to add leading 0's HH ID has 7 digit person ID has 9
  if (is.numeric(data$ID_HOGAR)) {
    data$ID_HOGAR <- as.character(data$ID_HOGAR)
    data$ID_HOGAR <- str_pad(data$ID_HOGAR, width = 7, side = "left", pad = "0")
  }
  
  if (is.numeric(data$ID_PERSONA)) {
    data$ID_PERSONA <- as.character(data$ID_PERSONA)
    data$ID_PERSONA <- str_pad(data$ID_PERSONA, width = 9, side = "left", pad = "0")
  }
  
  
  list_df[[x]] <- data
}

all_ENCFT_data <- bind_rows(list_df)


save(all_ENCFT_data,  file = "./Processed Data/Full_ENCFT.rda" )





