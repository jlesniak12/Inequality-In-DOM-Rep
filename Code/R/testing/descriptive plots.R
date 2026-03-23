source("Code/R/00_setup.R")


# ---- 1. Load Data---- #

panel_sector_time <- readRDS(file.path(config$paths$processed_data, "panel_sector_time.rds")) %>%
  rename(sector = group)

var_decomp <- readRDS(file.path(config$paths$processed_data, "variance_decomp_aggregate.rds")) %>%
  rename(sector = group)

exposure_baseline <- readRDS(file.path(config$paths$processed_data, "sector_mw_exposure_baseline.rds"))




# Helper to show only Q1 labels
quarterly_breaks <- panel_sector_time %>%
  filter(quarter_num == 1) %>%
  pull(time) %>%
  unique()



# --- Min Wage Increases --- #

p_minwage <- panel_sector_time %>%
  filter(!is.na(mw_nat_real)) %>%
  ggplot(aes(x = time, y = mw_nat_real, group = 1)) +
  annotate("rect", 
           xmin = "2020Q1", xmax = "2020Q4",
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_line() +
  geom_vline(
    xintercept = c("2015Q2", "2017Q2", "2017Q4", 
                   "2019Q3", "2021Q3", "2022Q1", 
                   "2023Q2"),
    linetype = "dashed", colour = "red", alpha = 0.6
  ) +
  labs(
    title = "National Real Minimum Wage Index",
    subtitle = "Employment-weighted average across firm size tiers, 2016 RD$",
    x = NULL, y = "Real Minimum Wage (RD$)"
  ) +
  theme_surveytools() + scale_x_discrete(breaks = quarterly_breaks)


# --- Exposure Groups --- #

p_expbase <- exposure_baseline %>%
  ggplot(aes(x = reorder(Employment_Sector, exposure_baseline_val),
             y = exposure_baseline_val,
             fill = exposure_group)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("High exposure"   = "#d73027",
               "Medium exposure" = "#fee090", 
               "Low exposure"    = "#4575b4")
  ) +
  labs(
    title = "Baseline Minimum Wage Exposure by Sector",
    subtitle = "Share of formal workers within 10% above applicable minimum wage, 2016",
    x = NULL, y = "Exposure measure",
    fill = NULL
  ) +
  theme_surveytools()

# --- Exposure Trends --- #

p_exptrend_data <- panel_sector_time %>%
  filter(!is.na(exposure_group)) %>%
  group_by(time, exposure_group) %>%
  summarise(
    informal_mean = weighted.mean(informal, w = sector_employment, 
                                  na.rm = TRUE),
    .groups = "drop"
  ) 

p_exptrend <- p_exptrend_data %>%
  ggplot(aes(x = time, y = informal_mean, 
             colour = exposure_group, group = exposure_group)) +
  annotate("rect", 
           xmin = "2020Q1", xmax = "2020Q4",
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_line() +
  geom_vline(
    xintercept = c("2017Q2", "2019Q3", "2021Q3", "2023Q2"),
    linetype = "dashed", alpha = 0.4
  ) +
  scale_colour_manual(
    values = c("High exposure"   = "#d73027",
               "Medium exposure" = "#fee090",
               "Low exposure"    = "#4575b4")
  ) +
  labs(
    title = "Informality Share by Exposure Group",
    subtitle = "Employment-weighted average within tercile group",
    x = NULL, y = "Share informal",
    colour = NULL
  ) +
  theme_surveytools() +
  scale_x_discrete(breaks = quarterly_breaks)


# --- Variance Breakdown --- #

p_var_data <- panel_sector_time %>%
  filter(exposure_group %in% c("High exposure", "Low exposure")) %>%
  filter(in_decomp_sample | exposure_group == "Low exposure") %>%
  group_by(time, exposure_group) %>%
  summarise(
    var_overall  = weighted.mean(var_logwage, w = sector_employment, 
                                 na.rm = TRUE),
    var_formal   = weighted.mean(var_logwage_formal, w = sector_employment,
                                 na.rm = TRUE),
    var_informal = weighted.mean(var_logwage_informal, w = sector_employment,
                                 na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(var_overall, var_formal, var_informal),
    names_to = "component",
    values_to = "variance"
  ) %>%
  mutate(component = recode(component,
                            "var_overall"  = "Overall",
                            "var_formal"   = "Formal sector",
                            "var_informal" = "Informal sector"
  ))

pvar <- p_var_data %>%
  ggplot(aes(x = time, y = variance,
             colour = exposure_group, group = exposure_group)) +
  annotate("rect", 
           xmin = "2020Q1", xmax = "2020Q4",
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_line() +
  geom_vline(
    xintercept = c("2017Q2", "2019Q3", "2021Q3", "2023Q2"),
    linetype = "dashed", alpha = 0.4
  ) +
  facet_wrap(~component, scales = "free_y") +
  scale_colour_manual(
    values = c("High exposure" = "#d73027",
               "Low exposure"  = "#4575b4")
  ) +
  labs(
    title = "Variance of Log Wages by Exposure Group",
    subtitle = "Employment-weighted average, high vs low exposure sectors",
    x = NULL, y = "Variance of log wages",
    colour = NULL
  ) +
  theme_surveytools() +
  scale_x_discrete(breaks = quarterly_breaks)

# --- with moving average

p_var_data <- panel_Sector_time %>%
  filter(exposure_group %in% c("High exposure", "Low exposure")) %>%
  filter(in_decomp_sample | exposure_group == "Low exposure") %>%
  group_by(time, exposure_group) %>%
  summarise(
    var_overall  = weighted.mean(var_logwage, w = sector_employment, 
                                 na.rm = TRUE),
    var_formal   = weighted.mean(var_logwage_formal, w = sector_employment,
                                 na.rm = TRUE),
    var_informal = weighted.mean(var_logwage_informal, w = sector_employment,
                                 na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Sort before rolling average - critical for correct ordering
  arrange(exposure_group, time) %>%
  # Apply rolling average within each exposure group BEFORE pivoting
  group_by(exposure_group) %>%
  mutate(
    var_overall  = zoo::rollmean(var_overall,  k = 4, fill = NA, align = "right"),
    var_formal   = zoo::rollmean(var_formal,   k = 4, fill = NA, align = "right"),
    var_informal = zoo::rollmean(var_informal, k = 4, fill = NA, align = "right")
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(var_overall, var_formal, var_informal),
    names_to = "component",
    values_to = "variance"
  ) %>%
  mutate(component = recode(component,
                            "var_overall"  = "Overall",
                            "var_formal"   = "Formal sector",
                            "var_informal" = "Informal sector"
  ))


pvar <- p_var_data %>%
  ggplot(aes(x = time, y = variance,
             colour = exposure_group, group = exposure_group)) +
  annotate("rect", 
           xmin = "2020Q1", xmax = "2020Q4",
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_line() +
  geom_vline(
    xintercept = c("2017Q2", "2019Q3", "2021Q3", "2023Q2"),
    linetype = "dashed", alpha = 0.4
  ) +
  facet_wrap(~component, scales = "fixed") +
  scale_colour_manual(
    values = c("High exposure" = "#d73027",
               "Low exposure"  = "#4575b4")
  ) +
  labs(
    title = "Variance of Log Wages by Exposure Group",
    subtitle = "Employment-weighted average, high vs low exposure sectors",
    x = NULL, y = "Variance of log wages",
    colour = NULL
  ) +
  theme_surveytools() +
  scale_x_discrete(breaks = quarterly_breaks)

# -- figure with 3 groups --- #

p_var_data <- panel_Sector_time %>%
  group_by(time, exposure_group) %>%
  summarise(
    var_overall  = weighted.mean(var_logwage, 
                                 w = sector_employment, na.rm = TRUE),
    var_formal   = weighted.mean(var_logwage_formal,
                                 w = sector_employment, na.rm = TRUE),
    var_informal = weighted.mean(var_logwage_informal,
                                 w = sector_employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(exposure_group, time) %>%
  group_by(exposure_group) %>%
  mutate(
    var_overall  = zoo::rollmean(var_overall, k = 4, 
                                 fill = NA, align = "right"),
    var_formal   = zoo::rollmean(var_formal,  k = 4, 
                                 fill = NA, align = "right"),
    var_informal = zoo::rollapply(var_informal, width = 4,
                                  FUN = function(x) mean(x, na.rm = TRUE),
                                  fill = NA, align = "right")
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(var_overall, var_formal, var_informal),
               names_to = "component", values_to = "variance") %>%
  mutate(component = recode(component,
                            "var_overall"  = "Overall",
                            "var_formal"   = "Formal sector",
                            "var_informal" = "Informal sector"
  ))

pvar <- p_var_data %>%
  ggplot(aes(x = time, y = variance,
             colour = exposure_group, group = exposure_group)) +
  annotate("rect", 
           xmin = "2020Q1", xmax = "2020Q4",
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_line() +
  geom_vline(
    xintercept = c("2017Q2", "2019Q3", "2021Q3", "2023Q2"),
    linetype = "dashed", alpha = 0.4
  ) +
  facet_wrap(~component, scales = "fixed") +
  scale_colour_manual(
    values = c(
      "High exposure"   = "#d73027",
      "Medium exposure" = "#fee090",
      "Low exposure"    = "#4575b4"
    )
  ) +
  labs(
    title = "Variance of Log Wages by Exposure Group",
    subtitle = "4-quarter moving average, employment-weighted within tercile.\nInformal sector restricted to sectors with reliable informal estimates.",
    x = NULL, y = "Variance of log wages",
    colour = NULL
  ) +
  theme_surveytools() +
  scale_x_discrete(breaks = quarterly_breaks)

# --- Inequality Measures by Group --- #

p_ineq_data <- panel_Sector_time %>%
  filter(!is.na(exposure_group)) %>%
  group_by(time, exposure_group) %>%
  summarise(
    log_50_10_mean = weighted.mean(log_50_10, w = sector_employment,
                                   na.rm = TRUE),
    .groups = "drop"
  )

p_ineq <- p_ineq_data %>%
  ggplot(aes(x = time, y = log_50_10_mean,
             colour = exposure_group, group = exposure_group)) +
  annotate("rect", 
           xmin = "2020Q1", xmax = "2020Q4",
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_line() +
  geom_vline(
    xintercept = c("2017Q2", "2019Q3", "2021Q3", "2023Q2"),
    linetype = "dashed", alpha = 0.4
  ) +
  scale_colour_manual(
    values = c("High exposure"   = "#d73027",
               "Medium exposure" = "#fee090",
               "Low exposure"    = "#4575b4")
  ) +
  labs(
    title = "Bottom-Half Wage Inequality by Exposure Group",
    subtitle = "Log p50/p10 ratio, employment-weighted average within tercile",
    x = NULL, y = "log(p50/p10)",
    colour = NULL
  ) +
  theme_surveytools() +
  scale_x_discrete(breaks = quarterly_breaks)



# -- Rolling 4 quarter avg --- #
p_ineq_data <- panel_Sector_time %>%
  filter(!is.na(exposure_group)) %>%
  group_by(time, exposure_group) %>%
  summarise(
    log_50_10_mean = weighted.mean(log_50_10, w = sector_employment,
                                   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(exposure_group, time) %>%
  group_by(exposure_group) %>%
  mutate(
    log_50_10_mean = zoo::rollmean(log_50_10_mean, k = 4, 
                                   fill = NA, align = "right")
  ) %>%
  ungroup()

p_ineq <- p_ineq_data %>%
  ggplot(aes(x = time, y = log_50_10_mean,
             colour = exposure_group, group = exposure_group)) +
  annotate("rect", 
           xmin = "2020Q1", xmax = "2020Q4",
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_line() +
  geom_vline(
    xintercept = c("2017Q2", "2019Q3", "2021Q3", "2023Q2"),
    linetype = "dashed", alpha = 0.4
  ) +
  scale_colour_manual(
    values = c("High exposure"   = "#d73027",
               "Medium exposure" = "#fee090",
               "Low exposure"    = "#4575b4")
  ) +
  labs(
    title = "Bottom-Half Wage Inequality by Exposure Group",
    subtitle = "Log p50/p10 ratio, employment-weighted average within tercile",
    x = NULL, y = "log(p50/p10)",
    colour = NULL
  ) +
  theme_surveytools() +
  scale_x_discrete(breaks = quarterly_breaks)


# ---- Figures on Non Complaince ---- #

# --- Non-compliance rate by exposure group --- #
p_compliance_all_data <- panel_Sector_time %>%
  filter(!is.na(exposure_group)) %>%
  group_by(time, exposure_group) %>%
  summarise(
    below_min_mean = weighted.mean(below_min, w = sector_employment,
                                   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(exposure_group, time) %>%
  group_by(exposure_group) %>%
  mutate(
    below_min_mean = zoo::rollmean(below_min_mean, k = 4,
                                   fill = NA, align = "right")
  ) %>%
  ungroup()

p_compliance_all <- p_compliance_data %>%
  ggplot(aes(x = time, y = below_min_mean,
             colour = exposure_group, group = exposure_group)) +
  annotate("rect",
           xmin = "2020Q1", xmax = "2020Q4",
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_line() +
  geom_vline(
    xintercept = c("2017Q2", "2019Q3", "2021Q3", "2023Q2"),
    linetype = "dashed", alpha = 0.4
  ) +
  scale_colour_manual(
    values = c("High exposure"   = "#d73027",
               "Medium exposure" = "#fee090",
               "Low exposure"    = "#4575b4")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Non-Compliance Rate by Exposure Group",
    subtitle = "Share of workers earning below applicable minimum wage, 4-quarter moving average",
    x = NULL, y = "Share below minimum wage",
    colour = NULL
  ) +
  theme_surveytools() +
  scale_x_discrete(breaks = quarterly_breaks)




# --- Non-compliance rate by exposure group FORMAL ONLY --- #
p_compliance_data <- panel_Sector_time %>%
  filter(!is.na(exposure_group)) %>%
  group_by(time, exposure_group) %>%
  summarise(
    below_min_mean = weighted.mean(below_min_formal, w = sector_employment,
                                   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(exposure_group, time) %>%
  group_by(exposure_group) %>%
  mutate(
    below_min_mean = zoo::rollmean(below_min_mean, k = 4,
                                   fill = NA, align = "right")
  ) %>%
  ungroup()

p_compliance <- p_compliance_data %>%
  ggplot(aes(x = time, y = below_min_mean,
             colour = exposure_group, group = exposure_group)) +
  annotate("rect",
           xmin = "2020Q1", xmax = "2020Q4",
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_line() +
  geom_vline(
    xintercept = c("2017Q2", "2019Q3", "2021Q3", "2023Q2"),
    linetype = "dashed", alpha = 0.4
  ) +
  scale_colour_manual(
    values = c("High exposure"   = "#d73027",
               "Medium exposure" = "#fee090",
               "Low exposure"    = "#4575b4")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Non-Compliance Rate by Exposure Group",
    subtitle = "Share of workers earning below applicable minimum wage, 4-quarter moving average",
    x = NULL, y = "Share below minimum wage",
    colour = NULL
  ) +
  theme_surveytools() +
  scale_x_discrete(breaks = quarterly_breaks)


