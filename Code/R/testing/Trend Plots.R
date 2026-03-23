source("Code/R/00_setup.R")


# ---- 1. Load Data---- #

panel_sector_time <- readRDS(file.path(config$paths$processed_data, "sector_time_panel.rds")) %>%
  rename(sector = group)



# --- Check Trends --- #

# Normalize each series to its 2016 average within each exposure group
# Then check whether pre-2017 trends are parallel

pretrend_data <- panel_sector_time %>%
  filter(!is.na(exposure_group)) %>%
  # Compute 2016 mean for each sector-outcome as baseline
  group_by(sector) %>%
  mutate(
    # Normalize to 2016 average
    baseline_var_formal   = mean(var_logwage_formal[year_num == 2016], 
                                 na.rm = TRUE),
    baseline_informal     = mean(informal[year_num == 2016], 
                                 na.rm = TRUE),
    baseline_below_min    = mean(below_min[year_num == 2016], 
                                 na.rm = TRUE),
    # Index each outcome relative to 2016 = 1
    idx_var_formal  = var_logwage_formal / baseline_var_formal,
    idx_informal    = informal / baseline_informal,
    idx_below_min   = below_min / baseline_below_min
  ) %>%
  ungroup() %>%
  # Restrict to pre-treatment window plus first event
  filter(year_num <= 2018) %>%
  # Average within exposure group
  group_by(time, exposure_group) %>%
  summarise(
    idx_var_formal = weighted.mean(idx_var_formal, 
                                   w = sector_employment, na.rm = TRUE),
    idx_informal   = weighted.mean(idx_informal,   
                                   w = sector_employment, na.rm = TRUE),
    idx_below_min  = weighted.mean(idx_below_min,  
                                   w = sector_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(idx_var_formal, idx_informal, idx_below_min),
    names_to = "outcome",
    values_to = "index"
  ) %>%
  mutate(outcome = recode(outcome,
                          "idx_var_formal" = "Formal wage variance",
                          "idx_informal"   = "Informality share",
                          "idx_below_min"  = "Non-compliance rate"
  ))

# Plot
pretrend_data %>%
  ggplot(aes(x = time, y = index,
             colour = exposure_group, group = exposure_group)) +
  geom_line() +
  geom_vline(xintercept = "2017Q2", linetype = "dashed", 
             colour = "red", alpha = 0.6) +
  geom_hline(yintercept = 1, linetype = "dotted", alpha = 0.4) +
  facet_wrap(~outcome, scales = "free_y") +
  scale_colour_manual(
    values = c("High exposure"   = "#d73027",
               "Medium exposure" = "#fee090",
               "Low exposure"    = "#4575b4")
  ) +
  labs(
    title = "Pre-Trend Check: Normalized Outcomes by Exposure Group",
    subtitle = "Outcomes indexed to 2016 average = 1. Red line marks first reform event (2017Q2).",
    x = NULL, y = "Index (2016 = 1)",
    colour = NULL
  ) +
  theme_surveytools() +
  scale_x_discrete(breaks = quarterly_breaks)


# Normalize to 2014Q3 instead of 2016 average
pretrend_data <- panel_sector_time %>%
  filter(!is.na(exposure_group)) %>%
  # Compute baseline as 2014Q3 value within each sector
  group_by(sector) %>%
  mutate(
    baseline_var_formal = mean(
      var_logwage_formal[time == "2014Q3"], na.rm = TRUE),
    baseline_informal   = mean(
      informal[time == "2014Q3"], na.rm = TRUE),
    baseline_below_min  = mean(
      below_min_formal[time == "2014Q3"], na.rm = TRUE),
    idx_var_formal = var_logwage_formal / baseline_var_formal,
    idx_informal   = informal           / baseline_informal,
    idx_below_min  = below_min_formal   / baseline_below_min
  ) %>%
  ungroup() %>%
  filter(year_num <= 2018) %>%
  group_by(time, exposure_group) %>%
  summarise(
    idx_var_formal = weighted.mean(idx_var_formal, 
                                   w = sector_employment, na.rm = TRUE),
    idx_informal   = weighted.mean(idx_informal,   
                                   w = sector_employment, na.rm = TRUE),
    idx_below_min  = weighted.mean(idx_below_min,  
                                   w = sector_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(idx_var_formal, idx_informal, idx_below_min),
    names_to = "outcome", values_to = "index"
  ) %>%
  mutate(outcome = recode(outcome,
                          "idx_var_formal" = "Formal wage variance",
                          "idx_informal"   = "Informality share",
                          "idx_below_min"  = "Non-compliance rate (formal)"
  ))

pretrend_plot <- pretrend_data %>%
  ggplot(aes(x = time, y = index,
             colour = exposure_group, group = exposure_group)) +
  geom_line() +
  # 2015Q2 reform — dark blue
  geom_vline(xintercept = "2015Q2", linetype = "dashed",
             colour = "darkblue", alpha = 0.7) +
  # 2017Q2 reform — red, your main first event
  geom_vline(xintercept = "2017Q2", linetype = "dashed",
             colour = "red", alpha = 0.7) +
  # Baseline reference line
  geom_hline(yintercept = 1, linetype = "dotted", alpha = 0.4) +
  # Shade 2016 as your baseline/omitted period
  annotate("rect",
           xmin = "2016Q1", xmax = "2016Q4",
           ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "grey50") +
  facet_wrap(~outcome, scales = "free_y", ncol = 3) +
  scale_colour_manual(
    values = c("High exposure"   = "#d73027",
               "Medium exposure" = "#fee090",
               "Low exposure"    = "#4575b4")
  ) +
  labs(
    title = "Pre-Trend Check: Normalized Outcomes by Exposure Group",
    subtitle = paste0(
      "Outcomes indexed to 2014Q3 = 1. ",
      "Blue dashed: 2015Q2 reform. Red dashed: 2017Q2 reform (first main event).\n",
      "Grey shading: 2016 baseline year (omitted period in event study)."
    ),
    x = NULL, y = "Index (2014Q3 = 1)",
    colour = NULL
  ) +
  theme_surveytools() +
  scale_x_discrete(breaks = quarterly_breaks)



# --- PRetrend 3 quarter Average ---#

pretrend_data <- panel_sector_time %>%
  filter(!is.na(exposure_group)) %>%
  group_by(sector) %>%
  mutate(
    # Baseline = average of pre-2015Q2 quarters
    pre_reform_quarters = time %in% c("2014Q3", "2014Q4", "2015Q1"),
    baseline_var_formal = mean(
      var_logwage_formal[pre_reform_quarters], na.rm = TRUE),
    baseline_informal   = mean(
      informal[pre_reform_quarters], na.rm = TRUE),
    baseline_below_min  = mean(
      below_min_formal[pre_reform_quarters], na.rm = TRUE),
    idx_var_formal = var_logwage_formal / baseline_var_formal,
    idx_informal   = informal           / baseline_informal,
    idx_below_min  = below_min_formal   / baseline_below_min
  ) %>%
  ungroup() %>%
  filter(year_num <= 2018) %>%
  group_by(time, exposure_group) %>%
  summarise(
    idx_var_formal = weighted.mean(idx_var_formal, 
                                   w = sector_employment, na.rm = TRUE),
    idx_informal   = weighted.mean(idx_informal,
                                   w = sector_employment, na.rm = TRUE),
    idx_below_min  = weighted.mean(idx_below_min,
                                   w = sector_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(idx_var_formal, idx_informal, idx_below_min),
    names_to = "outcome", values_to = "index"
  ) %>%
  mutate(outcome = recode(outcome,
                          "idx_var_formal" = "Formal wage variance",
                          "idx_informal"   = "Informality share",
                          "idx_below_min"  = "Non-compliance rate (formal)"
  ))


pretrend_plot <- pretrend_data %>%
  ggplot(aes(x = time, y = index,
             colour = exposure_group, group = exposure_group)) +
  geom_line() +
  # 2015Q2 reform — dark blue
  geom_vline(xintercept = "2015Q2", linetype = "dashed",
             colour = "darkblue", alpha = 0.7) +
  # 2017Q2 reform — red, your main first event
  geom_vline(xintercept = "2017Q2", linetype = "dashed",
             colour = "red", alpha = 0.7) +
  # Baseline reference line
  geom_hline(yintercept = 1, linetype = "dotted", alpha = 0.4) +
  # Shade 2016 as your baseline/omitted period
  annotate("rect",
           xmin = "2016Q1", xmax = "2016Q4",
           ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "grey50") +
  facet_wrap(~outcome, scales = "free_y", ncol = 3) +
  scale_colour_manual(
    values = c("High exposure"   = "#d73027",
               "Medium exposure" = "#fee090",
               "Low exposure"    = "#4575b4")
  ) +
  labs(
    title = "Pre-Trend Check: Normalized Outcomes by Exposure Group",
    subtitle = paste0(
      "Outcomes indexed to pre-reform average (2014Q3-2015Q1) = 1. ",
      "Blue dashed: 2015Q2 reform. Red dashed: 2017Q2 reform.\n",
      "Grey shading: 2016 baseline year (omitted period in event study)."
    ),
    x = NULL, y = "Index (2014QQ3-2015Q1 = 1)",
    colour = NULL
  ) +
  theme_surveytools() +
  scale_x_discrete(breaks = quarterly_breaks)


# -- Just show 2016 quarterly levels by exposure group
parallel_2016 <- panel_sector_time %>%
  filter(year_num == 2016, 
         !is.na(exposure_group)) %>%
  group_by(time, exposure_group) %>%
  summarise(
    var_formal   = weighted.mean(var_logwage_formal, 
                                 w = sector_employment, na.rm = TRUE),
    informal     = weighted.mean(informal,
                                 w = sector_employment, na.rm = TRUE),
    below_min_f  = weighted.mean(below_min_formal,
                                 w = sector_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>% pivot_longer(
    cols = c(var_formal, informal, below_min_f),
    names_to = "outcome", values_to = "index"
  ) %>%
  mutate(outcome = recode(outcome,
                          "var_formal" = "Formal wage variance",
                          "informal"   = "Informality share",
                          "below_min_f"  = "Non-compliance rate (formal)"
  ))


pretrend_plot <- parallel_2016 %>%
  ggplot(aes(x = time, y = index,
             colour = exposure_group, group = exposure_group)) +
  geom_line() +
  facet_wrap(~outcome, scales = "free_y", ncol = 3) +
  scale_colour_manual(
    values = c("High exposure"   = "#d73027",
               "Medium exposure" = "#fee090",
               "Low exposure"    = "#4575b4")
  ) +
  labs(
    title = "Pre-Trend Check by Exposure Group",
    subtitle = "Raw Outcomes",
    x = NULL, y = "Outcome Var",
    colour = NULL
  ) +
  theme_surveytools()


