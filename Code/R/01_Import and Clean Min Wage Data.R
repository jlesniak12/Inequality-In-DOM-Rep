


source("Code/R/00_setup.R")


minwage_file <- file.path(
  config$paths$raw_data,
  "Min Wage",
  "min_wage_and_CPI.xlsx"
)

nom_min_wage <- readxl::read_excel(minwage_file, sheet = "Nominal Wages") %>%
  tidyr::pivot_longer(
    cols = !c(Year, Quarter),
    names_to = "Wage_group",
    values_to = "nominal_minwage"
  )

real_min_wage <- readxl::read_excel(minwage_file, sheet = "Real Wages") %>%
  tidyr::pivot_longer(
    cols = !c(Year, Quarter),
    names_to = "Wage_group",
    values_to = "real_minwage"
  )

min_wage <- dplyr::inner_join(nom_min_wage, real_min_wage,
                              by = c("Year", "Quarter", "Wage_group")) %>%
  dplyr::rename(year = Year) %>%
  dplyr::mutate(quarter = as.numeric(substr(Quarter, 2, 2))) %>%
  dplyr::select(-Quarter)

CPI <- readxl::read_excel(minwage_file, sheet = "CPI") %>%
  dplyr::rename(year = Year) %>%
  dplyr::mutate(quarter = as.numeric(substr(Quarter, 2, 2))) %>%
  dplyr::select(-Quarter)


out_file <-file.path(config$paths$processed_data, "Min_Wage.rds")
saveRDS(min_wage, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))

out_file <-file.path(config$paths$processed_data, "CPI.rds")
saveRDS(CPI, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))

  

