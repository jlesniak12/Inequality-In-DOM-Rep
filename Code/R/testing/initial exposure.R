source("Code/R/00_setup.R")



# ---- 1. Load Data and Set Basic Design ---- #
Full_ENCFT_clean <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))


#function to create unique PSU/STRATA
individual_level_unique_id <- check_and_fix_survey_ids(Full_ENCFT_clean, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")


# -- simnplify dataset keeping only needed vars -- #

srvy_vars <- c("psu_unique", "strata_unique", "weight_annual", "weight_quarter", "FACTOR_EXPANSION", "DES_ESTRATO",
               "quarter", "month", "year", "year_quarter")

analysis_vars <- c(
  # -core income and min wage
  
  "real_salary_income_total",
  "real_total_income_total",
  "Wage_group",             #defines firm size based on official buckets of min wage (micro small med large)
  "real_minwage_harmonized",
  
  #  -other traits
  
  "Employment_Sector",      #sector of work
  "Employment_Status",      #formal vs informal
  "OCUPADO"                 #binary for currently employed
)



#key variables for employed persons only
analysis_df <- individual_level_unique_id %>%
  select(
    all_of(srvy_vars),
    all_of(analysis_vars)
  ) %>%
  filter((OCUPADO == 1))


#second data set limiting sample to those with defined min wage grouping
df_mw <- analysis_df %>%
  filter(!is.na(Wage_group)) %>%
  filter(!(Wage_group == "Dont Know"))








###### Ranking tables


##### share informal Workers #####


design <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = analysis_df,
  nest    = TRUE
)


shares <- compute_prop_factor(design = design,
                              time_var = "year",
                              var = "Employment_Status",
                              group_var = "Employment_Sector"
)


shares_informal <- shares %>%
  filter(level == "Informal")


informal_tbl <- build_table_wide(shares_informal, 
                                 value_col = "estimate")

shares_informal_ranked <- shares_informal %>%
  mutate(
    informal_share = estimate
  ) %>%
  group_by(time) %>%
  mutate(
    # choose rank direction:
    # - desc(informal_share): rank 1 = highest informality
    rank_informal = dplyr::min_rank(dplyr::desc(informal_share))
  ) %>%
  ungroup()



rank_years_wide_informal <- shares_informal_ranked %>%
  select(time, group, rank_informal) %>%
  pivot_wider(
    names_from = time,
    values_from = rank_informal
  ) %>%
  arrange(group)




shares_formal <- shares %>%
  filter(level == "Formal")


formal_tbl <- build_table_wide(shares_formal, 
                                 value_col = "estimate")

shares_formal_ranked <- shares_formal %>%
  mutate(
    formal_share = estimate
  ) %>%
  group_by(time) %>%
  mutate(
    # choose rank direction:
    # - desc(informal_share): rank 1 = highest informality
    rank_formal = dplyr::min_rank(dplyr::desc(formal_share))
  ) %>%
  ungroup()



rank_years_wide_formal <- shares_formal_ranked %>%
  select(time, group, rank_formal) %>%
  pivot_wider(
    names_from = time,
    values_from = rank_formal
  ) %>%
  arrange(group)







###### Calculate Inequality Rankings #####


design <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = analysis_df,
  nest    = TRUE
)

#note this is calculated with data for workers who dont fit a min wage category
#INCLUDES FORMAL AND INFORMAL


q_sector_year_total <- compute_quantiles(
  design      = design,
  var         = "real_total_income_total",
  time_var    = "year",
  group_var   = "Employment_Sector",
  probs       = c(0.10, 0.50, 0.90),
  subset_expr = rlang::expr(OCUPADO == 1),
  na.rm       = TRUE,
  fast        = TRUE
)


q_sector_year_salary <- compute_quantiles(
  design      = design,
  var         = "real_salary_income_total",
  time_var    = "year",
  group_var   = "Employment_Sector",
  probs       = c(0.10, 0.50, 0.90),
  subset_expr = rlang::expr(OCUPADO == 1),
  na.rm       = TRUE,
  fast        = TRUE
)



q_tbl_salary <- build_table_wide(q_sector_year_salary, 
                                 value_col = "estimate",
                                 col_keys = c("time"),
                                 row_keys = c("group", "measure"))

q_tbl_total <- build_table_wide(q_sector_year_total, 
                                value_col = "estimate",
                                col_keys = c("time"),
                                row_keys = c("group", "measure"))




#reshape wide for ratios

rank_ineq_from_quantiles <- function(q_df,
                                     time_col = "time",
                                     group_col = "group",
                                     measure_col = "measure",
                                     estimate_col = "estimate") {
  
  q_df %>%
    mutate(
      "{measure_col}" := as.character(.data[[measure_col]])
    ) %>%
    filter(.data[[measure_col]] %in% c("p10","p50","p90")) %>%
    pivot_wider(
      id_cols    = all_of(c(time_col, group_col)),
      names_from = all_of(measure_col),
      values_from = all_of(estimate_col)
    ) %>%
    mutate(
      p90_p50 = if_else(!is.na(p50) & p50 > 0, p90 / p50, NA_real_),
      p50_p10 = if_else(!is.na(p10) & p10 > 0, p50 / p10, NA_real_)
    ) %>%
    group_by(.data[[time_col]]) %>%
    mutate(
      rank_p90_p50 = min_rank(desc(p90_p50)),
      rank_p50_p10 = min_rank(desc(p50_p10))
    ) %>%
    ungroup()
}



q_total_ratio <- rank_ineq_from_quantiles(q_sector_year_total)

q_salary_ratio <- rank_ineq_from_quantiles(q_sector_year_salary)

glimpse(q_total_ratio)




rank_years_wide_salary_90_10 <- q_salary_ratio %>%
  select(time, group, rank_p90_p50, rank_p50_p10) %>%
  pivot_longer(
    cols = starts_with("rank_"),
    names_to = "metric",
    values_to = "rank"
  ) %>%
  pivot_wider(
    names_from = time,
    values_from = rank
  ) %>%
  filter(metric == "rank_p90_p50" )

rank_years_wide_salary_50_10 <- q_salary_ratio %>%
  select(time, group, rank_p90_p50, rank_p50_p10) %>%
  pivot_longer(
    cols = starts_with("rank_"),
    names_to = "metric",
    values_to = "rank"
  ) %>%
  pivot_wider(
    names_from = time,
    values_from = rank
  ) %>%
  filter(metric == "rank_p50_p10" )


rank_years_wide_total90_10 <- q_total_ratio %>%
  select(time, group, rank_p90_p50, rank_p50_p10) %>%
  pivot_longer(
    cols = starts_with("rank_"),
    names_to = "metric",
    values_to = "rank"
  ) %>%
  pivot_wider(
    names_from = time,
    values_from = rank
  ) %>%
  filter(metric == "rank_p90_p50" )

rank_years_wide_total_50_10 <- q_total_ratio %>%
  select(time, group, rank_p90_p50, rank_p50_p10) %>%
  pivot_longer(
    cols = starts_with("rank_"),
    names_to = "metric",
    values_to = "rank"
  ) %>%
  pivot_wider(
    names_from = time,
    values_from = rank
  ) %>%
  filter(metric == "rank_p50_p10" )





###### Trends in share works at binding minimum wage ######

near <- near_mw_share(
  df          = df_mw,
  time_var    = "year",
  time_subset = NULL,
  by_vars     = c("Employment_Sector"),
  out_col     = "near_mw",
  mw_lower    = 0.8,
  mw_upper    = 1.2,
  formal_only = TRUE
)

near_tbl <- build_table_wide(near, 
                             value_col = "near_mw",
                             col_keys = c("year"),
                             row_keys = c("Employment_Sector"))





###### calculate less than min wage


analysis_df <-df_mw %>%
  mutate(
    below_min = case_when((real_salary_income_total < real_minwage_harmonized) ~ 1,
                          TRUE ~ 0)
  )


design <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = analysis_df,
  nest    = TRUE
)


shares_below <- compute_prop_indicator(design = design,
                                       var = "below_min",
                                       time_var = "year",
                                       group_var = "Employment_Sector",
                                       subset_expr = quote(OCUPADO == 1 & real_salary_income_total > 0 &  Employment_Status == "Formal")
)




below_min_tbl <- build_table_wide(shares_below, 
                                  value_col = "estimate")






##### Check Zeros #####

income_zero_table <- function(df,
                              income_var,
                              time_var = "year",
                              time_subset = NULL,
                              group_var = "Employment_Sector",
                              employed_only = TRUE,
                              extra_filter = NULL,
                              probs = c(0.10, 0.50, 0.90)) {
  
  stopifnot(is.character(income_var), length(income_var) == 1)
  stopifnot(is.character(time_var), length(time_var) == 1)
  stopifnot(is.character(group_var), length(group_var) == 1)
  
  dat <- df
  
  if (!is.null(time_subset)) {
    dat <- dat %>% filter(.data[[time_var]] %in% time_subset)
  }
  if (isTRUE(employed_only)) {
    dat <- dat %>% filter(OCUPADO == 1)
  }
  if (!is.null(extra_filter)) {
    dat <- dat %>% filter(!!rlang::enquo(extra_filter))
  }
  
  dat %>%
    group_by(.data[[time_var]], .data[[group_var]]) %>%
    reframe(
      n = n(),
      share_zero = mean(.data[[income_var]] == 0, na.rm = TRUE),
      share_neg  = mean(.data[[income_var]] < 0, na.rm = TRUE),
      share_na   = mean(is.na(.data[[income_var]])),
      p10 = as.numeric(stats::quantile(.data[[income_var]], probs[1], na.rm = TRUE, type = 7)),
      p50 = as.numeric(stats::quantile(.data[[income_var]], probs[2], na.rm = TRUE, type = 7)),
      p90 = as.numeric(stats::quantile(.data[[income_var]], probs[3], na.rm = TRUE, type = 7))
    ) %>%
    ungroup()
}




#Salary income zeros by sector in 2016 and 2025
tab_salary <- income_zero_table(
  analysis_df,
  income_var = "real_salary_income_total",
  time_var = "year",
  time_subset = c(2016),
  group_var = "Employment_Sector"
)


tab_total <- income_zero_table(
  analysis_df,
  income_var = "real_total_income_total",
  time_var = "year",
  time_subset = c(2016),
  group_var = "Employment_Sector"
)




data_salary <-income_zero_table(
  analysis_df,
  income_var = "real_salary_income_total",
  time_var = "year",
  time_subset = NULL,
  group_var = "Employment_Sector"
)

data_total <-income_zero_table(
  analysis_df,
  income_var = "real_total_income_total",
  time_var = "year",
  time_subset = NULL,
  group_var = "Employment_Sector"
)


data_salary_tbl <- build_table_wide(data_salary, 
                                    value_col = "share_zero",
                                    row_keys = c("Employment_Sector"),
                                    col_keys = c("year"))

data_total_tbl <- build_table_wide(data_total, 
                                   value_col = "share_zero",
                                   row_keys = c("Employment_Sector"),
                                   col_keys = c("year"))


p1 <- ggplot(data_salary, aes( x = year, y = share_zero, color = Employment_Sector)) +
  geom_line() +
  labs( title = "Share Reporting Salary Income of 0") +
  theme_bw()




p2 <- ggplot(data_total, aes( x = year, y = share_zero, color = Employment_Sector)) +
  geom_line() +
  labs( title = "Share Reporting Total Income of 0") +
  theme_bw()






###### Export Outputs ######

list_tbls <- list(
  near_tbl                        = near_tbl,
  
  informal_tbl                    = informal_tbl,
  rank_years_wide_informal        = rank_years_wide_informal,
  
  formal_tbl                      = formal_tbl,
  rank_years_wide_formal          = rank_years_wide_formal,
  
  q_tbl_salary                    = q_tbl_salary,
  q_salary_ratio                  = q_salary_ratio,
  rank_years_wide_salary_90_10    = rank_years_wide_salary_90_10,
  rank_years_wide_salary_50_10    = rank_years_wide_salary_50_10,
  
  q_tbl_total                     = q_tbl_total,
  q_total_ratio                   = q_total_ratio,
  rank_years_wide_total90_10      = rank_years_wide_total90_10,
  rank_years_wide_total_50_10     = rank_years_wide_total_50_10,
    
  data_salary_tbl                 = data_salary_tbl,
  data_total_tbl                  = data_total_tbl,
  below_min_tbl                   = below_min_tbl
)

list_sheets <- list(
  
  "informality_shares" = c(
    "informal_tbl",
    "rank_years_wide_informal"
  ),
  
  "Formal_shares" = c(
    "formal_tbl",
    "rank_years_wide_formal"
  ),
  
  "Exposure to Min Wage" = c(
    "near_tbl"
  ),
  
  "Quantiles_salary" = c(
    "q_tbl_salary",
    "rank_years_wide_salary_90_10",
    "rank_years_wide_salary_50_10"
    
  ),
  
  "Quantiles_total" = c(
    "q_tbl_total",
    "rank_years_wide_total90_10",
    "rank_years_wide_total_50_10"
  ),
  
  "Zeros" = c(
    "data_salary_tbl",
    "data_total_tbl"
  ),
  
  "Below Min" = c(
    "below_min_tbl"
  )
  
)

save_path <- file.path(".", config$paths$outputs)


export_tables_xlsx(file = file.path(save_path, "Exposure Checksv3.xlsx"),
                   tables = list_tbls,
                   sheets = list_sheets)
















##### Calculate Baseline Sector Exposure ######


baseline_year <- 2016
baseline_quarters_2016 <- c("2016Q1","2016Q2","2016Q3","2016Q4")



near_2016 <- near_mw_share(
  df          = df_mw,
  time_var    = "year",
  time_subset = 2016,
  by_vars     = c("Employment_Sector", "Wage_group"),
  mw_lower    = 0.8,
  mw_upper    = 1.2,
  formal_only = TRUE
)



pi_2016 <- firmsize_pi(
  df          = df_mw,
  time_var    = "year",
  time_subset = 2016,
  by_vars     = c("Employment_Sector", "Wage_group"),
  size_var    = "Wage_group",
  formal_only = TRUE
)

pi0_2016 <- pi_2016 %>%
  group_by(Employment_Sector, Wage_group) %>%
  summarise(total0 = sum(total, na.rm = TRUE), .groups = "drop") %>%
  group_by(Employment_Sector) %>%
  mutate(pi0 = total0 / sum(total0)) %>%
  ungroup() %>%
  select(Employment_Sector, Wage_group, pi0)


exposure_2016 <- near_2016 %>%
  left_join(pi0_2016, by = c("Employment_Sector","Wage_group")) %>%
  mutate(weighted = near_mw_share * pi0) %>%
  group_by(Employment_Sector) %>%
  summarise(exposure_2016 = sum(weighted, na.rm = TRUE), .groups = "drop")


saveRDS(exposure_2016,
        file.path(config$paths$processed_data, "sector_mw_exposure_baseline_2016.rds"))












##### Check Zeros #####

income_zero_table <- function(df,
                              income_var,
                              time_var = "year",
                              time_subset = NULL,
                              group_var = "Employment_Sector",
                              employed_only = TRUE,
                              extra_filter = NULL,
                              probs = c(0.10, 0.50, 0.90)) {
  
  stopifnot(is.character(income_var), length(income_var) == 1)
  stopifnot(is.character(time_var), length(time_var) == 1)
  stopifnot(is.character(group_var), length(group_var) == 1)
  
  dat <- df
  
  if (!is.null(time_subset)) {
    dat <- dat %>% filter(.data[[time_var]] %in% time_subset)
  }
  if (isTRUE(employed_only)) {
    dat <- dat %>% filter(OCUPADO == 1)
  }
  if (!is.null(extra_filter)) {
    dat <- dat %>% filter(!!rlang::enquo(extra_filter))
  }
  
  dat %>%
    group_by(.data[[time_var]], .data[[group_var]]) %>%
    reframe(
      n = n(),
      share_zero = mean(.data[[income_var]] == 0, na.rm = TRUE),
      share_neg  = mean(.data[[income_var]] < 0, na.rm = TRUE),
      share_na   = mean(is.na(.data[[income_var]])),
      p10 = as.numeric(stats::quantile(.data[[income_var]], probs[1], na.rm = TRUE, type = 7)),
      p50 = as.numeric(stats::quantile(.data[[income_var]], probs[2], na.rm = TRUE, type = 7)),
      p90 = as.numeric(stats::quantile(.data[[income_var]], probs[3], na.rm = TRUE, type = 7))
    ) %>%
    ungroup()
}




#Salary income zeros by sector in 2016 and 2025
tab_salary <- income_zero_table(
  analysis_df,
  income_var = "real_salary_income_total",
  time_var = "year",
  time_subset = c(2016),
  group_var = "Employment_Sector"
)


tab_total <- income_zero_table(
  analysis_df,
  income_var = "real_total_income_total",
  time_var = "year",
  time_subset = c(2016),
  group_var = "Employment_Sector"
)




data_salary <-income_zero_table(
  analysis_df,
  income_var = "real_salary_income_total",
  time_var = "year",
  time_subset = NULL,
  group_var = "Employment_Sector"
)

data_total <-income_zero_table(
  analysis_df,
  income_var = "real_total_income_total",
  time_var = "year",
  time_subset = NULL,
  group_var = "Employment_Sector"
)


data_salary_tbl <- build_table_wide(data_salary, 
                                 value_col = "share_zero",
                                 row_keys = c("Employment_Sector"),
                                 col_keys = c("year"))

data_total_tbl <- build_table_wide(data_total, 
                                    value_col = "share_zero",
                                    row_keys = c("Employment_Sector"),
                                    col_keys = c("year"))


p1 <- ggplot(data_salary, aes( x = year, y = share_zero, color = Employment_Sector)) +
  geom_line() +
  labs( title = "Share Reporting Salary Income of 0") +
  theme_bw()




p2 <- ggplot(data_total, aes( x = year, y = share_zero, color = Employment_Sector)) +
  geom_line() +
  labs( title = "Share Reporting Total Income of 0") +
  theme_bw()









#define period for baseline
baseline_quarters <- c(
  "2014Q3", "2014Q4",
  "2015Q1", "2015Q2"
)




#define and calculate bands of min wage for exposure
mw_lower <- 0.8
mw_upper <- 1.2

baseline_df <- baseline_df %>%
  mutate(
    near_mw = if_else(
      real_salary_income_total >= mw_lower * real_minwage_harmonized &
        real_salary_income_total <= mw_upper * real_minwage_harmonized,
      1, 0
    )
  )


design_baseline <- svydesign(
  id     = ~psu_unique,
  strata = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data   = baseline_df,
  nest   = TRUE
)


#calc withn sectors/wage groups share of workers near min wage
exposure_sf <- svyby(
  ~near_mw,
  ~Employment_Sector + Wage_group,
  design = design_baseline,
  svymean,
  na.rm = TRUE
) %>%
  rename(exposure_sf = near_mw)

table(exposure_sf)


#calc total workers within  each sector x wage group 
cell_totals <- svyby(
  ~one,
  ~Employment_Sector + Wage_group,
  design = update(design_baseline, one = 1),
  FUN = svytotal,
  na.rm = TRUE
) %>%
  as_tibble() %>%
  rename(total_sf = one)

#compute within-sector shares of workers in each firm size π_{s,f}
firm_share_sf <- cell_totals %>%
  group_by(Employment_Sector) %>%
  mutate(pi_sf = total_sf / sum(total_sf)) %>%
  ungroup()


sector_exposure <- exposure_sf %>%
  left_join(
    firm_share_sf %>%
      select(Employment_Sector, Wage_group, pi_sf),
    by = c("Employment_Sector", "Wage_group")
  )


#calc weighted exposure measure
sector_exposure <- sector_exposure %>%
  mutate(
    weighted_exposure = exposure_sf * pi_sf
  ) %>%
  group_by(Employment_Sector) %>%
  summarise(
    exposure = sum(weighted_exposure, na.rm = TRUE),
    .groups = "drop"
  )


saveRDS(sector_exposure,
        file.path(config$paths$processed_data, "sector_mw_exposure_baseline.rds"))















near_base_q <- near_mw_share(
  df          = analysis_df,
  time_var    = "year_quarter",
  time_subset = baseline_quarters,
  by_vars     = c("Employment_Sector"),
  mw_lower    = 0.8,
  mw_upper    = 1.2,
  formal_only = TRUE
)



baseline_quarters <- c("2014Q3","2014Q4","2015Q1","2015Q2")

# -- calculate share of workers near min wage within sector x firm size
near_base_q <- near_mw_share(
  df          = analysis_df,
  time_var    = "year_quarter",
  time_subset = baseline_quarters,
  by_vars     = c("Employment_Sector", "Wage_group"),
  mw_lower    = 0.8,
  mw_upper    = 1.2,
  formal_only = TRUE
)
# columns: year_quarter, Employment_Sector, Wage_group, near_mw_share


# -- calculate the share of workers at a firm size within workers of each sector
pi_base_q <- firmsize_pi(
  df          = analysis_df,
  time_var    = "year_quarter",
  time_subset = baseline_quarters,
  by_vars     = c("Employment_Sector", "Wage_group"),
  size_var    = "Wage_group",
  formal_only = TRUE
)
# columns: year_quarter, Employment_Sector, Wage_group, total, pi

test <- pi_base_q %>%
  group_by(Employment_Sector, Wage_group) %>%
  summarise(total0 = sum(total, na.rm = TRUE), .groups = "drop")



pi0 <- pi_base_q %>%
  group_by(Employment_Sector, Wage_group) %>%
  summarise(total0 = sum(total, na.rm = TRUE), .groups = "drop") %>%
  group_by(Employment_Sector) %>%
  mutate(pi0 = total0 / sum(total0)) %>%
  ungroup() %>%
  select(Employment_Sector, Wage_group, pi0)



near_base_pooled <- near_base_q %>%
  left_join(
    pi_base_q %>% select(year_quarter, Employment_Sector, Wage_group, total),
    by = c("year_quarter","Employment_Sector","Wage_group")
  ) %>%
  group_by(Employment_Sector, Wage_group) %>%
  summarise(
    near_mw_share = sum(near_mw_share * total, na.rm = TRUE) / sum(total, na.rm = TRUE),
    .groups = "drop"
  )


sector_exposure_baseline <- near_base_pooled %>%
  dplyr::left_join(pi0, by = c("Employment_Sector","Wage_group")) %>%
  dplyr::mutate(weighted = near_mw_share * pi0) %>%
  dplyr::group_by(Employment_Sector) %>%
  dplyr::summarise(exposure = sum(weighted, na.rm = TRUE), .groups = "drop")


print(sector_exposure)

print(sector_exposure_baseline)



# -- Set final design -- #

#set overall design for individual level quarterly analysis
design_indiv_q <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = baseline_df,
                            nest = TRUE)


# ---- 2. Calculating Baseline exposure ---- #









