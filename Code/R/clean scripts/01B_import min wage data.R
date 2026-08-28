#===============================================================================
#
# Purpose: This file takes minimum wage excel files from Central Bank of Dom Rep
#        that also contains CPI data and creates R data objects. IT also does some
#        basic cleaning.
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


source(here::here("Code","R","clean scripts","00_setup.R"))


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
    names_to = "wage_group",
    values_to = "nom_minwage"
  )


min_wage <- nom_min_wage %>%
  dplyr::rename(year = Year) %>%
  dplyr::mutate(quarter = as.numeric(substr(Quarter, 2, 2))) %>%
  dplyr::select(-Quarter)


#change names for later merge
min_wage <- min_wage %>%
  mutate(wage_group = case_when(
    (wage_group == "micro_firm") ~ "Micro",
    (wage_group == "small_firm") ~ "Small",
    (wage_group == "medium_firm") ~ "Medium",
    (wage_group == "large_firm") ~ "Large",
    TRUE ~ wage_group
    )
  )

#bring CPI in to R
CPI <- readxl::read_excel(minwage_file, sheet = "CPI") %>%
  dplyr::rename(year = Year) %>%
  dplyr::mutate(quarter = as.numeric(substr(Quarter, 2, 2))) %>%
  dplyr::select(-Quarter)



#===============================================================================
# STEP 2. Create Different Min Wage Groupings by Company Size
#===============================================================================

# --- Map small firm minimum wage to micro firmrs pre 2021 --- #

# NOTE: Law was implemented in 2021 to create new micro category, these firms would
#       have been bound by small minimum wage before 2021 legally.

MICRO_START <- config$events$micro_tier_start_qtr   # "2021Q3"

min_wage <- min_wage %>%
  mutate(year_quarter = sprintf("%dQ%d", year, quarter),
         pre_micro    = year_quarter < MICRO_START) %>%   # lexical compare is safe for YYYYQn
  group_by(year, quarter) %>%
  mutate(nom_minwage_small = nom_minwage[match("Small", wage_group)]) %>%
  ungroup() %>%
  mutate(
    nom_minwage_harmonized = if_else(wage_group == "Micro" & pre_micro,
                                     nom_minwage_small, nom_minwage),
    wage_group_legal       = if_else(wage_group == "Micro" & pre_micro,
                                     "Small", wage_group)
  ) %>%
  select(-nom_minwage_small)



# remove average, free trade other unused columns
tier_map <- c(micro_firm = "Micro", small_firm = "Small",
              medium_firm = "Medium", large_firm = "Large")

min_wage <- min_wage %>%
  filter(wage_group %in% names(tier_map)) %>%
  mutate(wage_group = unname(tier_map[wage_group]))

stopifnot(setequal(min_wage$wage_group, config$TIER_LEVELS))



out_file <-file.path(config$paths$processed_data, "Min_Wage.rds")
saveRDS(min_wage, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))

out_file <-file.path(config$paths$processed_data, "CPI.rds")
saveRDS(CPI, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))






