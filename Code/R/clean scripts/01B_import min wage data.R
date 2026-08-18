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

real_min_wage <- readxl::read_excel(minwage_file, sheet = "Real Wages") %>%
  tidyr::pivot_longer(
    cols = !c(Year, Quarter),
    names_to = "wage_group",
    values_to = "real_minwage"
  )

min_wage <- dplyr::inner_join(nom_min_wage, real_min_wage,
                              by = c("Year", "Quarter", "wage_group")) %>%
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

min_wage <- min_wage %>%
  group_by(year, quarter) %>%
  mutate(
    real_min_wage_small = dplyr::first(real_minwage[wage_group == "Small"]),
    nom_min_wage_small  = dplyr::first(nom_minwage[wage_group == "Small"])
  ) %>%
  ungroup()

pre_micro <- (min_wage$year < 2021) | (min_wage$year == 2021 & min_wage$quarter < 3)

min_wage <- min_wage %>%
  mutate(
    real_minwage_harmonized = dplyr::if_else(
      wage_group == "Micro" & pre_micro,
      real_min_wage_small,
      real_minwage
    ),
    nom_minwage_harmonized = dplyr::if_else(
      wage_group == "Micro" & pre_micro,
      nom_min_wage_small,
      nom_minwage
    )
  ) %>%
  select(-real_min_wage_small, -nom_min_wage_small)

#create a legal wage group var
min_wage <- min_wage %>%
  mutate(wage_group_legal = case_when (
    (pre_micro == 1 & wage_group == "Micro") ~ "Small",
    TRUE ~ wage_group)
  )

out_file <-file.path(config$paths$processed_data, "Min_Wage.rds")
saveRDS(min_wage, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))

out_file <-file.path(config$paths$processed_data, "CPI.rds")
saveRDS(CPI, out_file)
message("Saved: ", normalizePath(out_file, winslash = "/", mustWork = FALSE))






# --- Derive three-tier wage grouping and its associated MW floor ---
# NOTE:
# -----------------------------------------------------------------------

# Pull the Medium MW floor by quarter for the Medium/Large compliance reference.
# This comes from min_wage (already loaded in Step 3) filtered to Wage_group ==
# "Medium". We join it on year x quarter so every row in the main data gets the
# medium-firm floor assigned to Medium/Large workers.

large_mw_ref <- min_wage %>%
  dplyr::filter(Wage_group == "Medium") %>%
  dplyr::select(year, quarter,
                real_minwage_harmonized_medium = real_minwage_harmonized,
                nom_minwage_harmonized_medium  = nom_minwage_harmonized)

min_wage <- min_wage %>%
  dplyr::left_join(medium_mw_ref, by = c("year", "quarter")) %>%
  dplyr::mutate(
    
    # ---- Three-tier wage group ----
    # Micro  → Micro    (unchanged; <10 workers, unambiguous)
    # Small  → Small    (unchanged; 11-50, unambiguous)
    # Medium → Medium/Large  (51-99 survey bin — unambiguous medium)
    # Large  → Medium/Large  (100+ survey bin — straddles medium/large legal boundary)
    # Dont Know / Unknown → Dont Know (retained as-is)
    Wage_group_3tier = dplyr::case_when(
      Wage_group == "Micro"     ~ "Micro",
      Wage_group == "Small"     ~ "Small",
      Wage_group %in% c("Medium", "Large") ~ "Medium/Large",
      Wage_group == "Dont Know" ~ "Dont Know",
      TRUE                      ~ NA_character_
    ),
    Wage_group_3tier = factor(
      Wage_group_3tier,
      levels = c("Micro", "Small", "Medium/Large", "Dont Know")
    ),
    
    # ---- Three-tier harmonized MW floor ----
    # For Micro and Small, reuse the value already merged from min_wage.
    # For Medium/Large, use the medium firm floor (conservative; see rationale).
    # For Dont Know / NA, leave as NA — no valid compliance comparison.
    real_minwage_harmonized_3tier = dplyr::case_when(
      Wage_group_3tier %in% c("Micro", "Small") ~ real_minwage_harmonized,
      Wage_group_3tier == "Medium/Large"         ~ real_minwage_harmonized_medium,
      TRUE                                       ~ NA_real_
    ),
    nom_minwage_harmonized_3tier = dplyr::case_when(
      Wage_group_3tier %in% c("Micro", "Small") ~ nom_minwage_harmonized,
      Wage_group_3tier == "Medium/Large"         ~ nom_minwage_harmonized_medium,
      TRUE                                       ~ NA_real_
    )
    
  ) %>%
  # Drop the temporary medium reference columns (values now in the _3tier columns)
  dplyr::select(-real_minwage_harmonized_medium, -nom_minwage_harmonized_medium)



