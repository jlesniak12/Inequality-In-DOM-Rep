source("Code/R/00_setup.R")



# ---- 1. Load Data and Set Basic Design ---- #
Full_ENCFT_clean <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))


#function to create unique PSU/STRATA
individual_level_unique_id <- check_and_fix_survey_ids(Full_ENCFT_clean, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")


# -- simplify data keeping only needed vars -- #

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

#drop gov sector
analysis_df <- analysis_df %>%
  filter(!(Employment_Sector == "Government"))

#second data set limiting sample to those with defined min wage grouping
df_mw <- analysis_df %>%
  filter(!is.na(Wage_group)) %>%
  filter(!(Wage_group == "Dont Know"))





# ---- 2. Calculate a baseline exposure used for rankings ---- #


baseline_year <- 2016
baseline_quarters_2016 <- c("2016Q1","2016Q2","2016Q3","2016Q4")

near_2016 <- near_mw_share(
  df          = df_mw,
  time_var    = "year",
  time_subset = 2016,
  by_vars     = c("Employment_Sector", "Wage_group"),
  min_wage = "real_minwage_harmonized",
  income = "real_salary_income_total",
  out_col = "near_min",
  mw_lower    = 0.9,
  mw_upper    = 1.1,
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

exposure_2016 <- weighted_exposure(
  near_tbl      = near_2016,
  pi_tbl        =  pi_2016,
  time_var      = "year",
  by_vars       = c("Employment_Sector"),
  weight_dim    ="Wage_group",
  exposure_col  = "near_min",
  pi_col        = "pi",
  out_col       = "exposure_weighted_baseline"
)

# generate group
exposure_2016 <- exposure_2016 %>%
  arrange(desc(exposure_weighted_baseline)) %>%
  mutate(
    tercile = ntile(exposure_weighted_baseline, 3),
    exposure_group = factor(
      c("Low exposure","Medium exposure","High exposure")[tercile],
      levels = c("Low exposure","Medium exposure","High exposure")
    )
  ) %>%
  select(Employment_Sector, exposure_weighted_baseline, exposure_group) 



saveRDS(exposure_2016,
        file.path(config$paths$processed_data, "sector_mw_exposure_baseline_2016.rds"))



make_sector_plots <- function(data,
                              sector_col = "Employment_Sector",
                              time_var = "year_quarter",
                              estimate_col = "exposure",
                              series_col = NULL,
                              title = "Exposure to Minimum Wage",
                              subtitle = "Share of Workers Near Minimum Wage",
                              y_label = "Share") {
  
  sectors <- sort(unique(data[[sector_col]]))
  
  plots <- map(sectors, ~{
    df_s <- data %>% filter(.data[[sector_col]] == .x)
    
    plot_ts_multi(
      df_s,
      time_var = time_var,
      estimate_col = estimate_col,
      series_col = series_col,
      show_ci = FALSE,
      title = title,
      subtitle = paste0(subtitle, " — ", .x),
      y_label = y_label
    )
  })
  
  names(plots) <- paste(sectors, y_label)
  plots
}


# ---- 3. Assess Min Wag Exposure Trends over Time ---- #

near_mw_sec <- near_mw_share(
  df          = df_mw,
  time_var    = "year_quarter",
  time_subset = NULL,
  by_vars     = c("Employment_Sector"),
  min_wage = "real_minwage_harmonized",
  income = "real_salary_income_total",
  out_col = "near_min",
  mw_lower    = 0.9,
  mw_upper    = 1.1,
  formal_only = TRUE
) %>%
  select(-se)


near_mw_sec_ind <- near_mw_sec %>%
  left_join(exposure_2016, by = "Employment_Sector") %>%
  rename(exposure = near_min) %>%
  mutate(exposure_norm =  exposure / exposure_weighted_baseline)

near_mw_sec_group <- near_mw_sec_ind %>%
  group_by(year_quarter, exposure_group) %>%
  summarise(exposure = mean(exposure, na.rm = TRUE),
            exposure_baseline = mean(exposure_weighted_baseline, na.rm = TRUE),
            exposure_norm = mean(exposure / exposure_baseline , na.rm = TRUE),
            .groups = "drop")


# -- plots for all sectors -- #

p_sector_exposure <- plot_ts_multi(near_mw_sec_ind, time_var = "year_quarter", estimate_col = "exposure", show_ci = FALSE,
              facet_col = "Employment_Sector",
              title = "Exposure to Minimum Wage",
              subtitle = "Share of Workers Near Minimum Wage by Sector",
              y_label = "Share")



p_sector_exposure_norm <- plot_ts_multi(near_mw_sec_ind, time_var = "year_quarter", estimate_col = "exposure_norm", show_ci = FALSE,
              facet_col = "Employment_Sector",
              title = "Exposure to Minimum Wage",
              subtitle = "Index, 2016 average = 1",
              y_label = "Index")


plots_exposure <- make_sector_plots(
  near_mw_sec_ind,
  estimate_col = "exposure",
  subtitle = "Share of Workers Near Minimum Wage"
)

plots_index <- make_sector_plots(
  near_mw_sec_ind,
  estimate_col = "exposure_norm",
  subtitle = "Index (2016 average = 1)",
  y_label = "Index"
)

# -- Tercile Plots -- #

p_tercile_exposure <- plot_ts_multi(near_mw_sec_group, time_var = "year_quarter", series_col = "exposure_group", estimate_col = "exposure", show_ci = FALSE,
              title = "Exposure to Minimum Wage",
              subtitle = "Share of Workers Near Minimum Wage by Sector",
              y_label = "Share")


p_tercile_exposure_norm <- plot_ts_multi(near_mw_sec_group, time_var = "year_quarter", series_col = "exposure_group", estimate_col = "exposure_norm", show_ci = FALSE,
              title = "Exposure to Minimum Wage",
              subtitle = "Index, 2016 average = 1",
              y_label = "Index")




# -- Save plots -- #

save_path <- file.path(".", config$paths$outputs, config$output_stage, config$out_subdirs$charts)
save_type <- paste(".", config$fig_defaults$format, sep = "")

#save individual plots
purrr::iwalk(
  plots_exposure,
  ~ ggsave(
    filename = file.path(save_path, "Sectoral Min Wage Exposure", "Exposure", "individual sectors", paste(.y, save_type, sep ="")),  # .y = name (spec_id)
    plot     = .x,                           # .x = ggplot object
    width    = config$fig_defaults$width,
    height   = config$fig_defaults$height
  )
)

purrr::iwalk(
  plots_index,
  ~ ggsave(
    filename = file.path(save_path, "Sectoral Min Wage Exposure", "Exposure", "individual sectors", paste(.y, save_type, sep ="")),  # .y = name (spec_id)
    plot     = .x,                           # .x = ggplot object
    width    = config$fig_defaults$width,
    height   = config$fig_defaults$height
  )
)

#save panels
panels <- list(p_sector_exposure, p_sector_exposure_norm, p_tercile_exposure, p_tercile_exposure_norm)
names(panels) <- c("Exposure by Sector", "Normalized Exposure by Sector", "Exposure by Tercile", "Normalized Exposure by Tercile")

purrr::iwalk(
  panels,
  ~ ggsave(
    filename = file.path(save_path, "Sectoral Min Wage Exposure", "Exposure", paste(.y, save_type, sep ="")),  # .y = name (spec_id)
    plot     = .x,                           # .x = ggplot object
    width    = config$fig_defaults$width,
    height   = config$fig_defaults$height
  )
)



# ---- 4. Assess Trends in Firm Size over Time ---- #

pi <- firmsize_pi(
  df          = df_mw,
  time_var    = "year_quarter",
  time_subset = NULL,
  by_vars     = c("Employment_Sector", "Wage_group"),
  size_var    = "Wage_group",
  formal_only = TRUE
)



pi_ind <- pi %>%
  left_join(exposure_2016, by = "Employment_Sector") %>%
  left_join(pi_2016, by = c("Employment_Sector", "Wage_group")) %>%
  rename(pi_baseline = pi.y, pi = pi.x) %>%
  select(-se.x, -se.y, total.x, total.y) %>%
  mutate(pi_norm = pi / pi_baseline)


pi_grouped <- pi_ind %>%
  group_by(year_quarter, exposure_group, Wage_group) %>%
  summarise(pi = mean(pi, na.rm = TRUE),
            pi_baseline = mean(pi_baseline, na.rm = TRUE),
            pi_norm = mean(pi / pi_baseline , na.rm = TRUE),
            .groups = "drop")



# -- plots for all sectors -- #


p_sector_weights <- plot_share_stacked(pi_ind, time_var = "year_quarter", level_col = "Wage_group", estimate_col = "pi",
              facet_col = "Employment_Sector",
              title = "Share of Workers in Sector",
              subtitle = "Share",
              y_label = "Share")



p_sector_weights_norm <- plot_ts_multi(pi_ind, time_var = "year_quarter", series_col = "Wage_group",  estimate_col = "pi_norm", show_ci = FALSE,
              facet_col = "Employment_Sector",
              title = "Share of Workers in Sector",
              subtitle = "Index, 2016 average = 1",
              y_label = "Index")




plots_weight <- make_sector_plots(
  pi_ind,
  time_var = "year_quarter",
  estimate_col = "pi",
  series_col =  "Wage_group",
  title = "Weight of Different Firm Sizes",
  subtitle = "Share"
)

plots_index <- make_sector_plots(
  pi_ind,
  time_var = "year_quarter",
  estimate_col = "pi_norm",
  series_col =  "Wage_group",
  title = "Weight of Different Firm Sizes",
  subtitle = "Index (2016 average = 1)",
  y_label = "Index"
)

# -- plots for groups -- #

p_tercile_weights <- plot_share_stacked(pi_grouped, time_var = "year_quarter", level_col = "Wage_group", estimate_col = "pi",
                   facet_col = "exposure_group",
                   title = "Share of Workers by Firm Size",
                   subtitle = "Share",
                   y_label = "Share")


p_tercile_weights_norm <- plot_ts_multi(pi_grouped, time_var = "year_quarter", series_col = "Wage_group",  estimate_col = "pi_norm", show_ci = FALSE,
              facet_col = "exposure_group",
              title = "Share of Workers by Firm Size",
              subtitle = "Index, 2016 average = 1",
              y_label = "Index")




# -- Save plots -- #

save_path <- file.path(".", config$paths$outputs, config$output_stage, config$out_subdirs$charts)
save_type <- paste(".", config$fig_defaults$format, sep = "")

#save individual plots
purrr::iwalk(
  plots_weight,
  ~ ggsave(
    filename = file.path(save_path, "Sectoral Min Wage Exposure", "Weights", "individual sectors", paste(.y, save_type, sep ="")),  # .y = name (spec_id)
    plot     = .x,                           # .x = ggplot object
    width    = config$fig_defaults$width,
    height   = config$fig_defaults$height
  )
)

purrr::iwalk(
  plots_index,
  ~ ggsave(
    filename = file.path(save_path, "Sectoral Min Wage Exposure", "Weights", "individual sectors", paste(.y, save_type, sep ="")),  # .y = name (spec_id)
    plot     = .x,                           # .x = ggplot object
    width    = config$fig_defaults$width,
    height   = config$fig_defaults$height
  )
)

#save panels
panels <- list(p_sector_weights, p_sector_weights_norm, p_tercile_weights, p_tercile_weights_norm)
names(panels) <- c("Weights by Sector", "Normalized Weights by Sector", "Weights by Tercile", "Normalized Weights by Tercile")

purrr::iwalk(
  panels,
  ~ ggsave(
    filename = file.path(save_path, "Sectoral Min Wage Exposure", "Weights", paste(.y, save_type, sep ="")),  # .y = name (spec_id)
    plot     = .x,                           # .x = ggplot object
    width    = config$fig_defaults$width,
    height   = config$fig_defaults$height
  )
)


# ---- 5. Decomposing  Sectoral Minimum Wage Exposure ---- #
  



# ---- quarter baseline ---- #

baseline_quarters <- c("2016Q2")


near_mw_sec_size <- near_mw_share(
  df          = df_mw,
  time_var    = "year_quarter",
  time_subset = NULL,
  by_vars     = c("Employment_Sector", "Wage_group"),
  min_wage = "real_minwage_harmonized",
  income = "real_salary_income_total",
  out_col = "near_min",
  mw_lower    = 0.9,
  mw_upper    = 1.1,
  formal_only = TRUE
)

pi <- firmsize_pi(
  df          = df_mw,
  time_var    = "year_quarter",
  time_subset = NULL,
  by_vars     = c("Employment_Sector", "Wage_group"),
  size_var    = "Wage_group",
  formal_only = TRUE
)



baseline_tbl_Q_Q <- near_mw_sec_size %>%
  filter(year_quarter %in% baseline_quarters) %>%
  group_by(Employment_Sector, Wage_group) %>%
  summarise(
    n0 = mean(near_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    pi %>%
      filter(year_quarter %in% baseline_quarters) %>%
      group_by(Employment_Sector, Wage_group) %>%
      summarise(
        pi0 = mean(pi, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("Employment_Sector","Wage_group")
  )


baseline_E0_exact_Q_Q <- baseline_tbl_Q_Q %>%
  group_by(Employment_Sector) %>%
  summarise(
    E0 = sum(pi0 * n0, na.rm = TRUE),
    .groups = "drop"
  )

pi_check <- pi %>%
  group_by(Employment_Sector, year_quarter) %>%
  summarise(sum_pi = sum(pi, na.rm = TRUE), .groups = "drop")

summary(pi_check$sum_pi)


decomp_panel_Q_Q <- near_mw_sec_size %>%
  left_join(
    pi,
    by = c("Employment_Sector","year_quarter","Wage_group")
  ) %>%
  left_join(
    baseline_tbl_Q_Q,
    by = c("Employment_Sector","Wage_group")
  )


decomp_sector_time_Q_Q <- decomp_panel_Q_Q %>%
  mutate(
    E_level_component = pi * near_min,
    within_component = pi0 * (near_min - n0),
    composition_component = (pi - pi0) * n0,
    interaction_component = (pi - pi0) * (near_min - n0)
  ) %>%
  group_by(Employment_Sector, year_quarter) %>%
  summarise(
    E = sum(E_level_component, na.rm = TRUE),
    within_effect = sum(within_component, na.rm = TRUE),
    composition_effect = sum(composition_component, na.rm = TRUE),
    interaction_effect = sum(interaction_component, na.rm = TRUE),
    total_change = within_effect + composition_effect + interaction_effect,
    .groups = "drop"
  ) %>%
  left_join(baseline_E0_exact_Q_Q, by = "Employment_Sector") %>%
  mutate(E_from_decomp = E0 + total_change)


check <- decomp_sector_time_Q_Q %>%
  mutate(diff = E - E_from_decomp) %>%
  summarise(max_abs_diff = max(abs(diff), na.rm = TRUE))

print(check)


present_quarter <- "2025Q2"

sector_table_Q_Q <- decomp_sector_time_Q_Q %>%
  filter(year_quarter == present_quarter) %>%
  left_join(exposure_2016, by = "Employment_Sector") %>%
  mutate(
    total_pp = 100 * total_change,
    within_pp = 100 * within_effect,
    composition_pp = 100 * composition_effect,
    interaction_pp = 100 * interaction_effect,
    share_within = within_effect / total_change
  ) %>%
  select(
    Employment_Sector,
    total_pp,
    within_pp,
    composition_pp,
    interaction_pp,
    share_within,
    exposure_group
  )



# -- Group summary plot

group_table_Q_Q <- sector_table_Q_Q %>%
  group_by(exposure_group) %>%
  summarise(
    within_pp       = mean(within_pp, na.rm = TRUE),
    composition_pp  = mean(composition_pp, na.rm = TRUE),
    interaction_pp  = mean(interaction_pp, na.rm = TRUE),
    total_pp        = mean(total_pp, na.rm = TRUE),
    .groups = "drop"
  )


group_long_Q_Q <- group_table_Q_Q %>%
  select(exposure_group, within_pp, composition_pp, interaction_pp) %>%
  pivot_longer(
    cols = c(within_pp, composition_pp, interaction_pp),
    names_to = "component",
    values_to = "pp"
  ) %>%
  mutate(
    component = recode(component,
                       within_pp = "within",
                       composition_pp = "composition",
                       interaction_pp = "interaction"
    )
  )

group_long_Q_Q <- group_long_Q_Q %>%
  mutate(component = factor(component, levels = c("composition","interaction","within")))


p_tercile_summary <- ggplot(group_long_Q_Q,
       aes(x = exposure_group, y = pp, fill = component)) +
  geom_col() +
  labs(
    title = "Average Change in Exposure to Min Wage, 2025Q2 vs 2016Q2 Values",
    subtitle ="Percentage Points",
    x = NULL,
    y = "Change in exposure (percentage points)",
    fill = NULL
  ) +
  theme_surveytools()

# -- save Results -- #

save_path <- file.path(".", config$paths$outputs, config$output_stage, config$out_subdirs$charts)
save_type <- paste(".", config$fig_defaults$format, sep = "")


ggsave(
  plot = p_tercile_summary,
  filename = file.path(save_path, "Sectoral Min Wage Exposure", "Decomp", paste("Decomp_Tercile", save_type, sep ="")),  # .y = name (spec_id)
  width    = config$fig_defaults$width,
  height   = config$fig_defaults$height
)
  

# -- save tables 
save_path <- file.path(".", config$paths$outputs, config$output_stage, config$out_subdirs$tables)
save_type <- paste(".", config$fig_defaults$format, sep = "")


sector_table_out <- sector_table_Q_Q %>%
  mutate(
    across(c(total_pp, within_pp, composition_pp, interaction_pp), ~round(.x, 2)),
    share_within = round(share_within, 2)
  ) %>%
  group_by(exposure_group) %>%
  arrange(desc(total_pp), .by_group = TRUE) %>%
  ungroup()


write_csv(sector_table_out, file.path(save_path, "Sectoral Min Wage Exposure", "sector_decomp_2025Q2_vs_2016Q2.csv"))
saveRDS(sector_table_Q_Q, file.path(save_path, "Sectoral Min Wage Exposure", "sector_decomp_2025Q2_vs_2016Q2.rds"))




# -- annual baseline --


near_mw_sec_size <- near_mw_share(
  df          = df_mw,
  time_var    = "year_quarter",
  time_subset = NULL,
  by_vars     = c("Employment_Sector", "Wage_group"),
  min_wage = "real_minwage_harmonized",
  income = "real_salary_income_total",
  out_col = "near_min",
  mw_lower    = 0.9,
  mw_upper    = 1.1,
  formal_only = TRUE
  )

pi <- firmsize_pi(
  df          = df_mw,
  time_var    = "year_quarter",
  time_subset = NULL,
  by_vars     = c("Employment_Sector", "Wage_group"),
  size_var    = "Wage_group",
  formal_only = TRUE
)


baseline_quarters_2016 <- c("2016Q1","2016Q2","2016Q3","2016Q4")

baseline_tbl <- near_mw_sec_size %>%
  filter(year_quarter %in% baseline_quarters_2016) %>%
  group_by(Employment_Sector, Wage_group) %>%
  summarise(
    n0 = mean(near_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    pi %>%
      filter(year_quarter %in% baseline_quarters_2016) %>%
      group_by(Employment_Sector, Wage_group) %>%
      summarise(
        pi0 = mean(pi, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("Employment_Sector","Wage_group")
  )


baseline_E0_exact <- baseline_tbl %>%
  group_by(Employment_Sector) %>%
  summarise(
    E0 = sum(pi0 * n0, na.rm = TRUE),
    .groups = "drop"
  )

pi_check <- pi %>%
  group_by(Employment_Sector, year_quarter) %>%
  summarise(sum_pi = sum(pi, na.rm = TRUE), .groups = "drop")

summary(pi_check$sum_pi)


decomp_panel <- near_mw_sec_size %>%
  left_join(
    pi,
    by = c("Employment_Sector","year_quarter","Wage_group")
  ) %>%
  left_join(
    baseline_tbl,
    by = c("Employment_Sector","Wage_group")
  )


decomp_sector_time <- decomp_panel %>%
  mutate(
    E_level_component = pi * near_min,
    within_component = pi0 * (near_min - n0),
    composition_component = (pi - pi0) * n0,
    interaction_component = (pi - pi0) * (near_min - n0)
  ) %>%
  group_by(Employment_Sector, year_quarter) %>%
  summarise(
    E = sum(E_level_component, na.rm = TRUE),
    within_effect = sum(within_component, na.rm = TRUE),
    composition_effect = sum(composition_component, na.rm = TRUE),
    interaction_effect = sum(interaction_component, na.rm = TRUE),
    total_change = within_effect + composition_effect + interaction_effect,
    .groups = "drop"
  ) %>%
  left_join(baseline_E0_exact, by = "Employment_Sector") %>%
  mutate(E_from_decomp = E0 + total_change)


check <- decomp_sector_time %>%
  mutate(diff = E - E_from_decomp) %>%
  summarise(max_abs_diff = max(abs(diff), na.rm = TRUE))

print(check)


present_quarter <- "2025Q2"

sector_table <- decomp_sector_time %>%
  filter(year_quarter == present_quarter) %>%
  left_join(exposure_2016, by = "Employment_Sector") %>%
  mutate(
    total_pp = 100 * total_change,
    within_pp = 100 * within_effect,
    composition_pp = 100 * composition_effect,
    interaction_pp = 100 * interaction_effect,
    share_within = within_effect / total_change
  ) %>%
  select(
    Employment_Sector,
    total_pp,
    within_pp,
    composition_pp,
    interaction_pp,
    share_within
  )


ggplot(group_decomp,
       aes(x = exposure_group, y = pp_change, fill = component)) +
  geom_col() +
  labs(
    title = "Change in Exposure to Min Wage, 2025Q2 vs 2016 Average Values",
    subtitle ="Percentage Points",
    x = NULL,
    y = "Change in exposure (percentage points)",
    fill = NULL
  ) +
  theme_surveytools()












# -- check decomp -- 

near_mw_sec <- near_mw_share(
  df          = df_mw,
  time_var    = "year_quarter",
  time_subset = NULL,
  by_vars     = c("Employment_Sector"),
  min_wage    = "real_minwage_harmonized",
  income      = "real_salary_income_total",
  mw_lower    = 0.9,
  mw_upper    = 1.1,
  formal_only = TRUE
)

check <- near_mw_sec %>%
  left_join(baseline_E0_exact, by = "Employment_Sector") %>%
  left_join(decomp_sector_time,
            by = c("Employment_Sector","year_quarter")) %>%
  mutate(
    lhs  = near_mw_share - E0,
    diff = lhs - total_change
  ) %>%
  summarise(max_abs_diff = max(abs(diff), na.rm = TRUE))


print(check)

