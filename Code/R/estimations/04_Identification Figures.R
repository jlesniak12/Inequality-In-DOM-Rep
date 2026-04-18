#===============================================================================
#
# Script: 04b_Figures_Identification.R
#
# Scope:  Figures motivating and justifying the event study identification
#         strategy. Uses the regression sample only.
#
# Follows project figure conventions:
#   - theme_surveytools() from 00_functions_plotting.R
#   - config$fig_defaults for width/height/dpi/format
#   - year_quarter character x-axis with 90-degree rotated labels
#   - Saves via ggsave to config$out_subdirs$inequality_minwage folder
#
# Narrative built by this script:
#   1. Min wage increases are large, discrete, and differ by tier -> variation
#   2. Exposure differs substantially across sectors and firm size tiers -> ID
#   3. Employment is spread across all tiers -> variation is empirically relevant
#   4. Firm size composition may be shifting -> potential confounder to document
#   5. Wage distribution bunches at the floor and shifts at events -> mechanism
#
# Figures:
#   fig_id_01_mw_changes        % change in nominal min wage at each event by tier
#   fig_id_02_exposure_sector   Exposure bar chart by sector (sector-level)
#   fig_id_03_exposure_heatmap  Exposure by sector x firm size
#   fig_id_04a_firmsize_trend   Employment share by tier over time (line)
#   fig_id_04b_firmsize_stack   Employment share by tier over time (stacked area)
#   fig_id_05_wage_dist         Log wage density pre/post 2017Q2
#
# Reads:
#   sector_mw_exposure_baseline.rds
#   sector_firmsize_mw_exposure_baseline.rds
#   desc_firmsize_shares.rds              (computed in 03, full sample)
#   Min_Wage.rds
#   Full_ENCFT_clean.rds                  (inline only for fig_id_05 density)
#
#===============================================================================

source("Code/R/00_setup.R")
library(patchwork)
library(scales)


#===============================================================================
# STEP 1. Load Data
#===============================================================================

pd <- config$paths$processed_data

exposure_s      <- readRDS(file.path(pd, "sector_mw_exposure_baseline.rds"))
exposure_sf     <- readRDS(file.path(pd, "sector_firmsize_mw_exposure_baseline.rds"))
firmsize_shares <- readRDS(file.path(pd, "desc_firmsize_shares.rds"))
min_wage        <- readRDS(file.path(pd, "Min_Wage.rds"))


#===============================================================================
# STEP 2. Shared Helpers
#===============================================================================

MW_EVENT_QTR <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")

SRC     <- "Sources: ENCFT 2014Q3-2025Q2; Central Bank of Dominican Republic."
REG_NOTE <- paste(
  "Regression sample: private sector salaried employees,",
  "positive earnings, known firm size, excl. government and electricity & water."
)
MW_NOTE  <- paste(
  "Red dashed vertical lines: min wage announcement quarters",
  "(2017Q2, 2019Q3, 2021Q3, 2023Q2).",
  "Grey shading: 2020Q1-2020Q4 (COVID-19 disruption)."
)

# Tier palette: distinct colors, all solid lines, no red
TIER_COLORS <- c(
  "Micro"  = "#1b7837",   # dark green
  "Small"  = "#762a83",   # purple
  "Medium" = "#e08214",   # orange
  "Large"  = "#1f78b4"    # blue
)

# Covid shading helper (same as 04a)
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

# Save helper
save_path <- file.path(
  config$paths$outputs,
  config$output_stage,
  config$out_subdirs$inequality_minwage
)
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)

save_fig <- function(p, name, w = config$fig_defaults$width,
                     h = config$fig_defaults$height) {
  ggsave(
    filename = file.path(save_path,
                         paste0(name, ".", config$fig_defaults$format)),
    plot     = p,
    width    = w,
    height   = h,
    dpi      = config$fig_defaults$dpi
  )
  message("Saved: ", name)
}


#===============================================================================
# FIGURE ID-01: Nominal Min Wage % Change at Each Event by Tier
#
# Shows: the shock magnitude differs by tier (pre-2021 micro = small;
# post-2021 micro is a new category). Motivates using tier as a dimension
# of cross-sectional variation.
#===============================================================================

event_qtrs <- tibble(
  time  = c("2017Q2", "2019Q3", "2021Q3", "2023Q2"),
  label = c("2017Q2", "2019Q3", "2021Q3", "2023Q2")
)

mw_changes <- min_wage %>%
  filter(Wage_group %in% c("Micro", "Small", "Medium", "Large")) %>%
  mutate(
    time       = paste0(year, "Q", quarter),
    Wage_group = factor(Wage_group, levels = c("Micro", "Small", "Medium", "Large"))
  ) %>%
  arrange(Wage_group, time) %>%
  group_by(Wage_group) %>%
  mutate(pct_change = (nom_minwage_harmonized / lag(nom_minwage_harmonized) - 1) * 100) %>%
  ungroup() %>%
  inner_join(event_qtrs, by = "time")

fig_id_01 <- ggplot(mw_changes,
                    aes(x = label, y = pct_change,
                        fill = Wage_group, group = Wage_group)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7,
           color = "white") +
  scale_fill_manual(values = TIER_COLORS) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
  labs(
    title    = "Nominal minimum wage increase at each event",
    subtitle = "% change from prior quarter, by firm size tier",
    x        = "Event quarter",
    y        = "% change in nominal min wage",
    caption  = SRC
  ) +
  theme_surveytools(legend_position = "right") +
  # Override x-axis rotation — categorical labels read fine horizontally here
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
        axis.ticks.x = element_line())


#===============================================================================
# FIGURE ID-02: Exposure by Sector — Bar Chart (Sector-Level Aggregate)
#
# Shows: substantial cross-sector variation in share of formal workers near
# the min wage in 2016. The key identifying variation.
#===============================================================================

fig_id_02 <- exposure_s %>%
  mutate(Employment_Sector = fct_reorder(Employment_Sector,
                                         exposure_baseline_val)) %>%
  ggplot(aes(x = Employment_Sector, y = exposure_baseline_val,
             fill = exposure_group)) +
  geom_col(color = "white", width = 0.7) +
  scale_fill_manual(
    values = c(
      "Low exposure"    = "grey80",
      "Medium exposure" = "grey50",
      "High exposure"   = "grey20"
    )
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_flip() +
  labs(
    title    = "Sector exposure to the minimum wage (2016 baseline)",
    subtitle = "Share of formal workers within 10% of firm-size-specific min wage",
    x        = NULL,
    y        = "Exposure (share near min wage)",
    caption  = paste(
      "Exposure = share of formal workers earning between 1x and 1.1x the tier-specific min wage.",
      "2016 annual average. Weighted average over firm size tiers.",
      REG_NOTE, SRC, sep = "\n"
    )
  ) +
  theme_surveytools(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))


#===============================================================================
# FIGURE ID-03: Exposure Heatmap — Sector x Firm Size
#
# Shows: the sector x firm size variation that is the actual unit of analysis.
#===============================================================================

fig_id_03 <- exposure_sf %>%
  mutate(
    Wage_group = factor(Wage_group,
                        levels = c("Micro", "Small", "Medium", "Large")),
    Employment_Sector = fct_reorder(Employment_Sector,
                                    exposure_sf_val, .fun = mean)
  ) %>%
  ggplot(aes(x = Wage_group, y = Employment_Sector,
             fill = exposure_sf_val)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(exposure_sf_val, accuracy = 1)),
            size = 2.8, color = "white") +
  scale_fill_gradient(
    low    = "grey85",
    high   = "grey10",
    labels = percent_format(accuracy = 1),
    name   = "Exposure"
  ) +
  labs(
    title    = "Minimum wage exposure by sector x firm size (2016 baseline)",
    subtitle = "Share of formal workers within 10% of tier-specific min wage",
    x        = "Firm size tier",
    y        = NULL,
    caption  = paste(REG_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools(legend_position = "right") +
  theme(
    axis.text.x  = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.ticks.x = element_line(),
    panel.grid   = element_blank()
  )


#===============================================================================
# FIGURE ID-04: Firm Size Employment Shares Over Time
#
# Panel A: line chart — tracks each tier's share trajectory
# Panel B: stacked area — shows overall composition
#
# Important: a drift toward micro firms is a potential confounder since the
# min wage schedule was different pre-/post-2021 for micro. Document this.
#===============================================================================

fs_event_pos <- which(sort(unique(firmsize_shares$time)) %in% MW_EVENT_QTR)

fig_id_04a <- ggplot(firmsize_shares,
                     aes(x = time, y = share,
                         color = Wage_group, linetype = Wage_group, group = Wage_group)) +
  covid_rect(sort(unique(firmsize_shares$time))) +
  geom_vline(xintercept = fs_event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = TIER_COLORS) +
  scale_linetype_manual(values = setNames(rep("solid", length(TIER_COLORS)), names(TIER_COLORS))) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Employment share by firm size tier over time",
    subtitle = "Share of employed workers with known firm size",
    y        = "Share of employed (known firm size)",
    caption  = paste(
      "All employed (OCUPADO == 1). Shares conditional on known firm size.",
      MW_NOTE, SRC, sep = "\n"
    )
  ) +
  theme_surveytools()

fig_id_04b <- ggplot(firmsize_shares,
                     aes(x = time, y = share,
                         fill = Wage_group, group = Wage_group)) +
  geom_area(position = "stack", alpha = 0.85) +
  geom_vline(xintercept = fs_event_pos, linetype = "dashed",
             color = "white", linewidth = 0.4) +
  scale_fill_manual(values = TIER_COLORS) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Employment composition by firm size (stacked)",
    subtitle = "Cumulative share adds to 100% within each quarter",
    y        = "Cumulative share",
    caption  = paste(
      "All employed (OCUPADO == 1). Shares conditional on known firm size.",
      "White dashed lines: min wage announcement quarters.",
      SRC, sep = "\n"
    )
  ) +
  theme_surveytools(legend_position = "right")


#===============================================================================
# FIGURE ID-05: Log Wage Distribution vs. Min Wage, Pre vs. Post 2017Q2
#
# Shows: bunching at the minimum wage and distributional shift after the event.
# Note: reads Full_ENCFT_clean.rds directly here — justified because kernel
# density on individual wages cannot be pre-computed as a summary statistic.
#===============================================================================

full_df <- readRDS(file.path(pd, "Full_ENCFT_clean.rds"))

dist_data <- full_df %>%
  filter(
    OCUPADO == 1,
    Employment_Type == "private employee",
    real_salary_income_primary > 0,
    !is.na(Wage_group),
    !(Wage_group %in% c("Dont Know", "Unknown")),
    year_quarter %in% c("2017Q1", "2017Q3")
  ) %>%
  mutate(
    period   = if_else(year_quarter == "2017Q1", "Pre (2017Q1)", "Post (2017Q3)"),
    period   = factor(period, levels = c("Pre (2017Q1)", "Post (2017Q3)")),
    log_wage = log(real_salary_income_primary),
    # Normalise weights within each period for density
    w_norm   = FACTOR_EXPANSION / sum(FACTOR_EXPANSION)
  )

# Min wage reference line — large firm tier (most workers, clearest reference)
mw_ref <- min_wage %>%
  filter(
    Wage_group == "Large",
    (year == 2017 & quarter %in% c(1, 3))
  ) %>%
  mutate(
    period  = if_else(quarter == 1, "Pre (2017Q1)", "Post (2017Q3)"),
    period  = factor(period, levels = c("Pre (2017Q1)", "Post (2017Q3)")),
    log_mw  = log(real_minwage_harmonized)
  )

fig_id_05 <- ggplot(dist_data,
                    aes(x = log_wage, color = period, linetype = period)) +
  geom_density(aes(weight = w_norm), linewidth = 0.8, adjust = 0.8) +
  geom_vline(
    data      = mw_ref,
    aes(xintercept = log_mw, color = period),
    linewidth = 0.65
  ) +
  scale_color_manual(
    values = c("Pre (2017Q1)" = "#762a83", "Post (2017Q3)" = "#1f78b4")
  ) +
  scale_linetype_manual(
    values = c("Pre (2017Q1)" = "solid", "Post (2017Q3)" = "solid")
  ) +
  labs(
    title    = "Log wage distribution around 2017Q2 minimum wage increase",
    subtitle = "Survey-weighted kernel density; vertical lines = log(real min wage) for large firms",
    x        = "Log real salary (primary job)",
    y        = "Density",
    caption  = paste(
      "Bandwidth = 0.8. Large-firm min wage used as reference line.",
      REG_NOTE, SRC, sep = "\n"
    )
  ) +
  theme_surveytools() +
  theme(
    axis.text.x  = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.ticks.x = element_line()
  )

rm(full_df)   # free memory — no longer needed


#===============================================================================
# STEP 3. Save Individual Figures
#===============================================================================

save_fig(fig_id_01, "fig_id_01_mw_changes")
save_fig(fig_id_02, "fig_id_02_exposure_sector")
save_fig(fig_id_03, "fig_id_03_exposure_heatmap")
save_fig(fig_id_04a, "fig_id_04a_firmsize_trend")
save_fig(fig_id_04b, "fig_id_04b_firmsize_stack")
save_fig(fig_id_05, "fig_id_05_wage_dist")


#===============================================================================
# STEP 4. Combined Identification Slide
#===============================================================================

fig_id_combined <- (fig_id_02 | fig_id_03) / (fig_id_01 | fig_id_05) +
  plot_annotation(
    title    = "Identification: Minimum Wage Exposure Across Sectors and Firm Sizes",
    subtitle = "Dominican Republic, 2016 baseline",
    theme    = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  filename = file.path(save_path,
                       paste0("fig_id_combined.",
                              config$fig_defaults$format)),
  plot     = fig_id_combined,
  width    = config$fig_defaults$width * 2,
  height   = config$fig_defaults$height * 2,
  dpi      = config$fig_defaults$dpi
)
message("Saved: fig_id_combined")

# Firm size panel: trend + stacked together
fig_firmsize_combined <- (fig_id_04a | fig_id_04b) +
  plot_annotation(
    title = "Firm Size Employment Composition Over Time",
    theme = theme(plot.title = element_text(size = 13, face = "bold"))
  )

ggsave(
  filename = file.path(save_path,
                       paste0("fig_id_04_firmsize_combined.",
                              config$fig_defaults$format)),
  plot     = fig_firmsize_combined,
  width    = config$fig_defaults$width * 2,
  height   = config$fig_defaults$height,
  dpi      = config$fig_defaults$dpi
)
message("Saved: fig_id_04_firmsize_combined")

cat("\n=== 04b_Figures_Identification.R complete ===\n")

