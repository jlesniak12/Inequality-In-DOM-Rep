#===============================================================================
#
# Script: 04a_Figures_Context.R
#
# Scope:  Full-economy context figures for the presentation and paper.
#         Loads pre-computed RDS objects from 03_Compute_Descriptive_Stats
#         and Min_Wage.rds. No survey computation here.
#
# Follows project figure conventions:
#   - theme_surveytools() from 00_functions_plotting.R
#   - config$fig_defaults for width/height/dpi/format
#   - year_quarter character x-axis with 90-degree rotated labels
#   - Saves via ggsave to config$out_subdirs$inequality folder
#   - All-black lines with varying linetypes (Parente/academic style)
#   - Vertical dashed lines at min wage event quarters
#
# Figures:
#   fig_01_minwage           Real min wage levels by firm size tier
#   fig_02_kaitz             Kaitz ratio (min wage / median formal wage) by tier
#   fig_03_variance          Var(log wage) — Overall, Formal, Informal, Self-emp
#   fig_04_variance_decomp   Within/between decomposition
#   fig_05_percentile_ratios log(p90/p10) and log(p50/p10)
#   fig_06_emp_shares        Informal share + self-employed share
#
# Reads:
#   Min_Wage.rds
#   desc_variance_by_status.rds
#   desc_variance_overall.rds
#   desc_variance_decomp.rds
#   desc_percentile_ratios.rds
#   desc_employment_shares.rds
#   desc_selfemployed_variance.rds
#
#===============================================================================

source("Code/R/00_setup.R")
library(patchwork)
library(scales)
 
 
#===============================================================================
# STEP 1. Load Data
#===============================================================================
 
pd  <- config$paths$processed_data
 
min_wage           <- readRDS(file.path(pd, "Min_Wage.rds"))
formal_median_tier <- readRDS(file.path(pd, "desc_formal_median_by_tier.rds"))
var_status         <- readRDS(file.path(pd, "desc_variance_by_status.rds"))
var_overall        <- readRDS(file.path(pd, "desc_variance_overall.rds"))
var_decomp    <- readRDS(file.path(pd, "desc_variance_decomp.rds"))
pct_ratios    <- readRDS(file.path(pd, "desc_percentile_ratios.rds"))
emp_shares    <- readRDS(file.path(pd, "desc_employment_shares.rds"))
var_selfempl  <- readRDS(file.path(pd, "desc_selfemployed_variance.rds"))
 
 
#===============================================================================
# STEP 2. Shared Helpers
#===============================================================================
 
# Min wage event quarter labels — used to draw reference lines
# NOTE: 2019 event is 2019Q3 (not Q2) — nominal wage did not change in 2019Q2,
# the increase was implemented in 2019Q3. Verify against Min_Wage.rds if unsure.
MW_EVENT_QTR <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")
 
# Add vertical event lines — works with character x-axis
mw_events_layer <- function(data_with_time_col) {
  all_qtrs  <- sort(unique(data_with_time_col$time))
  event_pos <- which(all_qtrs %in% MW_EVENT_QTR)
  if (length(event_pos) == 0) return(NULL)
  geom_vline(xintercept = event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4)
}
 
# Source/note strings
SRC <- "Sources: ENCFT 2014Q3-2025Q2; Central Bank of Dominican Republic."
MW_NOTE <- paste(
  "Red dashed vertical lines: min wage announcement quarters",
  "(2017Q2, 2019Q3, 2021Q3, 2023Q2).",
  "Grey shading: 2020Q1-2020Q4 (COVID-19 disruption)."
)
 
# Consistent save helper using project config
save_path <- file.path(
  config$paths$outputs,
  config$output_stage,
  config$out_subdirs$inequality
)
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
 
save_fig <- function(p, name) {
  ggsave(
    filename = file.path(save_path, paste0(name, ".", config$fig_defaults$format)),
    plot     = p,
    width    = config$fig_defaults$width,
    height   = config$fig_defaults$height,
    dpi      = config$fig_defaults$dpi
  )
  message("Saved: ", name)
}
 
# ── Color palettes (all solid lines; red reserved for event lines) ──────────────
 
# Tier palette: 4 distinct colors, no red
TIER_COLORS <- c(
  "Micro"  = "#1b7837",   # dark green
  "Small"  = "#762a83",   # purple
  "Medium" = "#e08214",   # orange
  "Large"  = "#1f78b4"    # blue
)
 
# Multi-series palette for non-tier figures: distinct, no red
# Overall/Formal/Informal/Self-employed
SERIES_COLORS_4 <- c(
  "Overall"       = "black",
  "Formal"        = "#1f78b4",   # blue
  "Informal"      = "#e08214",   # orange
  "Self-employed" = "#762a83"    # purple
)
 
SERIES_COLORS_3 <- c(
  "Overall" = "black",
  "Within"  = "#1f78b4",   # blue
  "Between" = "#e08214"    # orange
)
 
SERIES_COLORS_2_RATIO <- c(
  "log(p90/p10)" = "black",
  "log(p50/p10)" = "#1f78b4"   # blue
)
 
SERIES_COLORS_2_EMP <- c(
  "Informal"      = "black",
  "Self-employed" = "#1f78b4"   # blue
)
 
# ── Covid shading helper ─────────────────────────────────────────────────────────
# Adds a light grey rectangle covering 2020Q1-2020Q4.
# Drawn BEFORE geom_line() so it sits behind the data series.
# xmin/xmax are integer positions of 2020Q1 and 2020Q4 on the character x-axis.
covid_rect <- function(all_qtrs) {
  xmin <- which(all_qtrs == "2020Q1")
  xmax <- which(all_qtrs == "2020Q4")
  if (length(xmin) == 0 || length(xmax) == 0) return(NULL)
  annotate(
    "rect",
    xmin  = xmin - 0.5,
    xmax  = xmax + 0.5,
    ymin  = -Inf,
    ymax  = Inf,
    fill  = "grey85",
    alpha = 0.6
  )
}
 


#===============================================================================
# FIGURE 1: Real Minimum Wage Levels by Firm Size Tier
#===============================================================================

mw_data <- min_wage %>%
  filter(Wage_group %in% c("Micro", "Small", "Medium", "Large")) %>%
  mutate(
    time       = paste0(year, "Q", quarter),
    Wage_group = factor(Wage_group, levels = c("Micro", "Small", "Medium", "Large"))
  ) %>%
  arrange(Wage_group, time)

# Filter to quarters present in the survey period
survey_qtrs <- sort(unique(var_overall$time))
mw_data <- mw_data %>% filter(time %in% survey_qtrs)

# Event positions for this data
mw_event_pos <- which(sort(unique(mw_data$time)) %in% MW_EVENT_QTR)

fig_01 <- ggplot(mw_data,
                 aes(x = time, y = real_minwage_harmonized,
                     color = Wage_group, linetype = Wage_group, group = Wage_group)) +
  covid_rect(sort(unique(mw_data$time))) +
  geom_vline(xintercept = mw_event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = TIER_COLORS) +
  scale_linetype_manual(values = setNames(rep("solid", length(TIER_COLORS)), names(TIER_COLORS)))  +
  labs(
    title    = "Real Minimum Wage by Firm Size Tier",
    subtitle = "2015 Dominican Pesos",
    y        = "Real min wage (2015 DOP)",
    caption  = paste(MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()


#===============================================================================
# FIGURE 2: Log Kaitz Index by Firm Size Tier
#
# Kaitz = log(real min wage / median formal wage within that tier).
# This is the canonical definition from Lee (1999), Autor et al. (2016),
# Parente (2024). A higher value means the floor is more binding.
# Denominator is the formal sector median within each tier — computed in
# 03_Compute_Descriptive_Stats.R from all formal salaried workers with
# known firm size. No regression sample restriction needed.
#===============================================================================

kaitz_data <- mw_data %>%
  left_join(
    formal_median_tier %>% rename(time = time),
    by = c("time", "Wage_group")
  ) %>%
  mutate(
    log_kaitz = log(real_minwage_harmonized) - log(p50_formal)
  ) %>%
  filter(!is.na(log_kaitz))

fig_02 <- ggplot(kaitz_data,
                 aes(x = time, y = log_kaitz,
                     color = Wage_group, group = Wage_group)) +
  covid_rect(sort(unique(kaitz_data$time))) +
  geom_vline(xintercept = mw_event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = TIER_COLORS) +
  scale_linetype_manual(values = setNames(rep("solid", length(TIER_COLORS)), names(TIER_COLORS))) +
  labs(
    title    = "Log Kaitz Index by Firm Size Tier",
    subtitle = "log(real min wage) - log(median formal wage within tier)",
    y        = "Log Kaitz Index",
    caption  = paste(
      "Kaitz = log(tier min wage / median formal salary within that tier).",
      "Positive values = min wage exceeds median; more negative = less binding.",
      "Denominator: formal salaried workers (private + public) with positive earnings.",
      MW_NOTE, SRC, sep = "\n"
    )
  ) +
  theme_surveytools()


#===============================================================================
# FIGURE 3: Variance of Log Wages — Overall, Formal, Informal, Self-employed
#===============================================================================

# Combine all series into one tidy frame
var_all_series <- bind_rows(
  var_overall  %>% mutate(group = "Overall"),
  var_status,                              # already has group = Formal / Informal
  var_selfempl %>% mutate(group = "Self-employed")
) %>%
  mutate(group = factor(group,
                        levels = c("Overall", "Formal", "Informal", "Self-employed")))

var_event_pos <- which(sort(unique(var_all_series$time)) %in% MW_EVENT_QTR)

fig_03 <- ggplot(var_all_series,
                 aes(x = time, y = variance,
                     color = group, linetype = group, group = group)) +
  covid_rect(sort(unique(kaitz_data$time))) +
  geom_vline(xintercept = var_event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = SERIES_COLORS_4) +
  scale_linetype_manual(values = setNames(rep("solid", length(SERIES_COLORS_4)), names(SERIES_COLORS_4))) +
  labs(
    title    = "Variance of Log Earnings",
    subtitle = "Salaried workers (private + public) with positive earnings; self-employed shown separately",
    y        = "Variance of log earnings",
    caption  = paste(MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()


#===============================================================================
# FIGURE 4: Variance Decomposition — Within and Between
#===============================================================================

decomp_long <- var_decomp %>%
  select(time, total_var, within, between) %>%
  pivot_longer(c("total_var", "within", "between"),
               names_to = "component", values_to = "value") %>%
  mutate(
    component = recode(component,
                       "total_var" = "Overall",
                       "within"    = "Within",
                       "between"   = "Between"),
    component = factor(component, levels = c("Overall", "Within", "Between"))
  )

decomp_event_pos <- which(sort(unique(decomp_long$time)) %in% MW_EVENT_QTR)

fig_04 <- ggplot(decomp_long,
                 aes(x = time, y = value,
                     color = component, linetype = component, group = component)) +
  covid_rect(sort(unique(kaitz_data$time))) +
  geom_vline(xintercept = decomp_event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = SERIES_COLORS_3) +
  scale_linetype_manual(values = setNames(rep("solid", length(SERIES_COLORS_3)), names(SERIES_COLORS_3))) +
  labs(
    title    = "Decomposition of Variance of Log Earnings",
    subtitle = "V = Within (formal/informal) + Between (formal/informal) components",
    y        = "Variance of log earnings",
    caption  = paste(
      "Within = sum_j s_jt * Var_j(log w).  Between = sum_j s_jt * (mean_j - mean)^2.",
      "j in {Formal, Informal}. Salaried workers with positive earnings.",
      MW_NOTE, SRC, sep = "\n"
    )
  ) +
  theme_surveytools()


#===============================================================================
# FIGURE 5: Percentile Ratios — log(p90/p10) and log(p50/p10)
#===============================================================================

pct_long <- pct_ratios %>%
  select(time, log_p90_p10, log_p50_p10) %>%
  pivot_longer(c("log_p90_p10", "log_p50_p10"),
               names_to = "ratio", values_to = "value") %>%
  mutate(
    ratio = recode(ratio,
                   "log_p90_p10" = "log(p90/p10)",
                   "log_p50_p10" = "log(p50/p10)"),
    ratio = factor(ratio, levels = c("log(p90/p10)", "log(p50/p10)"))
  )

pct_event_pos <- which(sort(unique(pct_long$time)) %in% MW_EVENT_QTR)

fig_05 <- ggplot(pct_long,
                 aes(x = time, y = value,
                     color = ratio, linetype = ratio, group = ratio)) +
  covid_rect(sort(unique(kaitz_data$time))) +
  geom_vline(xintercept = pct_event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = SERIES_COLORS_2_RATIO) +
  scale_linetype_manual(values = setNames(rep("solid", length(SERIES_COLORS_2_RATIO)), names(SERIES_COLORS_2_RATIO))) +
  labs(
    title    = "Earnings Inequality: Log Percentile Ratios",
    subtitle = "Salaried workers (private + public) with positive earnings",
    y        = "Log Percentile Ratio",
    caption  = paste(MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()


#===============================================================================
# FIGURE 6: Informal and Self-Employed Shares Over Time
#===============================================================================

shares_long <- emp_shares %>%
  pivot_longer(c("informal_share", "selfempl_share"),
               names_to = "series", values_to = "share") %>%
  mutate(
    series = recode(series,
                    "informal_share" = "Informal",
                    "selfempl_share" = "Self-employed"),
    series = factor(series, levels = c("Informal", "Self-employed"))
  )

shares_event_pos <- which(sort(unique(shares_long$time)) %in% MW_EVENT_QTR)

fig_06 <- ggplot(shares_long,
                 aes(x = time, y = share,
                     color = series, linetype = series, group = series)) +
  covid_rect(sort(unique(kaitz_data$time))) +
  geom_vline(xintercept = shares_event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = SERIES_COLORS_2_EMP) +
  scale_linetype_manual(values = setNames(rep("solid", length(SERIES_COLORS_2_EMP)), names(SERIES_COLORS_2_EMP))) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Informality and Self-Employment",
    subtitle = "Share of all employed workers",
    y        = "Share of employed workers",
    caption  = paste(MW_NOTE, SRC, sep = "\n"
    )
  ) +
  theme_surveytools()


#===============================================================================
# STEP 3. Save Individual Figures
#===============================================================================

save_fig(fig_01, "fig_01_minwage_levels")
save_fig(fig_02, "fig_02_kaitz_ratio")
save_fig(fig_03, "fig_03_variance_by_status")
save_fig(fig_04, "fig_04_variance_decomp")
save_fig(fig_05, "fig_05_percentile_ratios")
save_fig(fig_06, "fig_06_emp_shares")


#===============================================================================
# STEP 4. Combined Presentation Layouts
#===============================================================================

# 2x2: the four inequality figures together
fig_combined_ineq <- (fig_03 | fig_06) / (fig_05 | fig_04) +
  plot_annotation(
    title    = "Labor Market Context: Dominican Republic 2014-2025",
    subtitle = "Salaried worker inequality and employment structure",
    theme    = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  filename = file.path(save_path,
                       paste0("fig_combined_context.",
                              config$fig_defaults$format)),
  plot     = fig_combined_ineq,
  width    = config$fig_defaults$width * 2,
  height   = config$fig_defaults$height * 2,
  dpi      = config$fig_defaults$dpi
)
message("Saved: fig_combined_context")

# Min wage context: levels + Kaitz side by side
fig_combined_mw <- (fig_01 | fig_02) +
  plot_annotation(
    title = "The Minimum Wage in Dominican Republic, 2014-2025",
    theme = theme(plot.title = element_text(size = 13, face = "bold"))
  )

ggsave(
  filename = file.path(save_path,
                       paste0("fig_combined_minwage.",
                              config$fig_defaults$format)),
  plot     = fig_combined_mw,
  width    = config$fig_defaults$width * 2,
  height   = config$fig_defaults$height,
  dpi      = config$fig_defaults$dpi
)
message("Saved: fig_combined_minwage")

cat("\n=== 04a_Figures_Context.R complete ===\n")
