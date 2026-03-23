#===============================================================================
#
# Scope: This file takes minimum wage excel files from Central Bank of Dom Rep
#        that also contains CPI data and creates R data objects.
#
# Steps:
#       1) Import excel files and do basic data clean up.
#
#       2) Refined minimum wage by size. This is necessary because in Dom Rep a
#          a new category (micro) for company size was created in 2021. Before
#          this time we track micro companies but we apply the small company
#          minimum wage to this category as that is what was legally binding to
#          to them before category was created as a separate grouping legally.
#
#
#
#
#===============================================================================


source("Code/R/00_setup.R")


#===============================================================================
# STEP 1. Import and Basic Data Cleaning.
#===============================================================================

minwage_file <- file.path(
  config$paths$raw_data,
  "Min Wage",
  "min_wage_and_CPI.xlsx"
)

nom_min_wage <- readxl::read_excel(minwage_file, sheet = "Nominal Wages") %>%
  tidyr::pivot_longer(
    cols = !c(Year, Quarter),
    names_to = "Wage_group",
    values_to = "nom_minwage"
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


#change names for later merge
min_wage <- min_wage %>%
  mutate(Wage_group = case_when(
    (Wage_group == "micro_firm") ~ "Micro",
    (Wage_group == "small_firm") ~ "Small",
    (Wage_group == "medium_firm") ~ "Medium",
    (Wage_group == "large_firm") ~ "Large",
    TRUE ~ Wage_group
    )
  )

#bring CPI in to R
CPI <- readxl::read_excel(minwage_file, sheet = "CPI") %>%
  dplyr::rename(year = Year) %>%
  dplyr::mutate(quarter = as.numeric(substr(Quarter, 2, 2))) %>%
  dplyr::select(-Quarter)



#===============================================================================
# STEP 2. Redfining  Min wage by Company Size
#===============================================================================

#Map small firm minimum wage to micro firmrs pre 2021
#law was implemented in 2021 but these firms would have fallen under small before\

min_wage <- min_wage %>%
  group_by(year, quarter) %>%
  mutate(
    real_min_wage_small = dplyr::first(real_minwage[Wage_group == "Small"]),
    nom_min_wage_small  = dplyr::first(nom_minwage[Wage_group == "Small"])
  ) %>%
  ungroup()

pre_micro <- (min_wage$year < 2021) | (min_wage$year == 2021 & min_wage$quarter < 3)

min_wage <- min_wage %>%
  mutate(
    real_minwage_harmonized = dplyr::if_else(
      Wage_group == "Micro" & pre_micro,
      real_min_wage_small,
      real_minwage
    ),
    nom_minwage_harmonized = dplyr::if_else(
      Wage_group == "Micro" & pre_micro,
      nom_min_wage_small,
      nom_minwage
    )
  ) %>%
  select(-real_min_wage_small, -nom_min_wage_small)




out_file <-file.path(config$paths$processed_data, "Min_Wage.rds")
saveRDS(min_wage, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))

out_file <-file.path(config$paths$processed_data, "CPI.rds")
saveRDS(CPI, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))

  

