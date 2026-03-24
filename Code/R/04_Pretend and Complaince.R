#===============================================================================
#
# Script: 04c_Figures_PreTrend_Noncompliance.R
#
# Scope:  Figures that complete the identification narrative:
#         1. Non-compliance (share of formal workers below min wage) over time,
#            by firm size tier and by sector — shows the minimum wage is
#            genuinely binding and that binding-ness varies across cells.
#         2. Pre-trend analysis — key outcomes over time split by exposure
#            tercile (High / Medium / Low), averaged within each group.
#            Flat pre-trends + divergence at event = identification support.
#
# Both sets of figures use the regression sample (panel_sf), which already has
# below_min_formal, informal, log_var_wage, exposure_group_overall, and
# exposure_group_within_tier pre-computed in 02_Prepare Regression Panel.
#
# Figures:
#   fig_nc_01_by_tier      Non-compliance over time by firm size tier
#   fig_nc_02_by_sector    Non-compliance over time by sector (faceted)
#   fig_nc_03_snapshot     Non-compliance snapshot bar chart by sector × tier
#   fig_pt_01_var          Log var wages: High vs Medium vs Low exposure
#   fig_pt_02_informal     Informal share: High vs Medium vs Low exposure
#   fig_pt_03_noncompliance Non-compliance: High vs Medium vs Low exposure
#   fig_pt_combined        All three pre-trend panels side by side
#
# Reads:
#   panel_sector_firmsize_quarter.rds  (from 02_Prepare Regression Panel)
#
#===============================================================================

source("Code/R/00_setup.R")
library(patchwork)
library(scales)


#===============================================================================
# STEP 1. Load Data
#===============================================================================

pd       <- config$paths$processed_data
panel_sf <- readRDS(file.path(pd, "panel_sector_firmsize_quarter.rds"))

# Verify key columns present
stopifnot(all(c("below_min_formal", "informal", "log_var_wage",
                "exposure_group_overall", "time",
                "Employment_Sector", "Wage_group") %in% names(panel_sf)))


#===============================================================================
# STEP 2. Shared Helpers
#===============================================================================

MW_EVENT_QTR <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")
SRC      <- "Sources: ENCFT 2014Q3-2025Q2; Central Bank of Dominican Republic."
REG_NOTE <- "Regression sample: private sector employees, positive earnings, known firm size."
MW_NOTE  <- paste(
  "Red dashed lines: min wage announcement quarters (2017Q2, 2019Q3, 2021Q3, 2023Q2).",
  "Grey shading: 2020Q1-2020Q4 (COVID-19)."
)

# Event line positions on character x-axis
event_pos <- function(qtrs) which(sort(unique(qtrs)) %in% MW_EVENT_QTR)

# Covid shading
covid_rect <- function(qtrs) {
  xmin <- which(sort(unique(qtrs)) == "2020Q1")
  xmax <- which(sort(unique(qtrs)) == "2020Q4")
  if (!length(xmin) || !length(xmax)) return(NULL)
  annotate("rect", xmin = xmin - 0.5, xmax = xmax + 0.5,
           ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.6)
}

# Save helper
save_path <- file.path(config$paths$outputs, config$output_stage,
                       config$out_subdirs$inequality_minwage)
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)

save_fig <- function(p, name, w = config$fig_defaults$width,
                     h = config$fig_defaults$height) {
  ggsave(
    filename = file.path(save_path,
                         paste0(name, ".", config$fig_defaults$format)),
    plot = p, width = w, height = h, dpi = config$fig_defaults$dpi
  )
  message("Saved: ", name)
}

# Tier and exposure group color palettes (no red)
TIER_COLORS <- c(
  "Micro"  = "#1b7837",
  "Small"  = "#762a83",
  "Medium" = "#e08214",
  "Large"  = "#1f78b4"
)

EXPOSURE_COLORS <- c(
  "High exposure"   = "#1f78b4",   # blue
  "Medium exposure" = "#e08214",   # orange
  "Low exposure"    = "#762a83"    # purple
)


#===============================================================================
# STEP 3. Non-Compliance Figures
#
# below_min_formal = share of formal workers in that cell earning below the
# tier-specific minimum wage. This is the canonical non-compliance measure.
# It directly shows the minimum wage floor is binding — a necessary condition
# for identification.
#===============================================================================

# ── 3A: Aggregate by tier over time ──────────────────────────────────────────
# Average across sectors within each tier, weighted by pi (employment share)

nc_by_tier <- panel_sf %>%
  filter(!is.na(below_min_formal), !is.na(pi)) %>%
  group_by(time, Wage_group) %>%
  summarise(
    nc_formal = weighted.mean(below_min_formal, pi, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(
    time       = as.character(time),
    Wage_group = factor(Wage_group, levels = c("Micro", "Small", "Medium", "Large"))
  )

nc_tier_qtrs  <- sort(unique(nc_by_tier$time))
nc_tier_epos  <- event_pos(nc_tier_qtrs)

fig_nc_01 <- ggplot(nc_by_tier,
                    aes(x = time, y = nc_formal,
                        color = Wage_group, linetype = Wage_group,
                        group = Wage_group)) +
  covid_rect(nc_tier_qtrs) +
  geom_vline(xintercept = nc_tier_epos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.75) +
  scale_color_manual(values = TIER_COLORS) +
  scale_linetype_manual(values = setNames(rep("solid", 4), names(TIER_COLORS))) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Formal sector non-compliance by firm size tier",
    subtitle = "Share of formal workers earning below tier-specific minimum wage",
    y        = "Share below minimum wage",
    caption  = paste(
      "Non-compliance = formal worker salary < tier-specific real minimum wage.",
      "Weighted average across sectors within each tier (weights = firm size employment share).",
      MW_NOTE, REG_NOTE, SRC, sep = "\n"
    )
  ) +
  theme_surveytools()


# ── 3B: By sector over time — faceted ────────────────────────────────────────
# Average across tiers within each sector

nc_by_sector <- panel_sf %>%
  filter(!is.na(below_min_formal), !is.na(pi)) %>%
  group_by(time, Employment_Sector) %>%
  summarise(
    nc_formal = weighted.mean(below_min_formal, pi, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(time = as.character(time))

nc_sector_qtrs <- sort(unique(nc_by_sector$time))
nc_sector_epos <- event_pos(nc_sector_qtrs)

fig_nc_02 <- ggplot(nc_by_sector,
                    aes(x = time, y = nc_formal, group = Employment_Sector)) +
  covid_rect(nc_sector_qtrs) +
  geom_vline(xintercept = nc_sector_epos, linetype = "dashed",
             color = "red", linewidth = 0.35) +
  geom_line(color = "#1f78b4", linewidth = 0.6) +
  facet_wrap(~Employment_Sector, ncol = 3, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Formal sector non-compliance by economic sector",
    subtitle = "Share of formal workers earning below minimum wage (weighted avg over firm size tiers)",
    y        = "Share below minimum wage",
    caption  = paste(MW_NOTE, REG_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools() +
  theme(
    strip.text       = element_text(size = 8),
    axis.text.x      = element_text(angle = 90, size = 6),
    panel.spacing    = unit(0.3, "lines")
  )


# ── 3C: Snapshot bar chart — latest pre-event year (2016 baseline) ───────────
# Shows the level of non-compliance at baseline by sector, ordered high to low
# This pairs naturally with the exposure bar chart in 04b

nc_snapshot <- panel_sf %>%
  mutate(time_chr = as.character(time)) %>%
  filter(
    !is.na(below_min_formal),
    # Use 2016 annual average as baseline (same year as exposure measure)
    grepl("^2016", time_chr)
  ) %>%
  group_by(Employment_Sector) %>%
  summarise(
    nc_formal = weighted.mean(below_min_formal, pi, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  left_join(
    readRDS(file.path(pd, "sector_mw_exposure_baseline.rds")) %>%
      select(Employment_Sector, exposure_group),
    by = "Employment_Sector"
  ) %>%
  mutate(Employment_Sector = fct_reorder(Employment_Sector, nc_formal))

fig_nc_03 <- ggplot(nc_snapshot,
                    aes(x = Employment_Sector, y = nc_formal,
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
    title    = "Formal sector non-compliance by sector (2016 baseline)",
    subtitle = "Share of formal workers earning below tier-specific minimum wage",
    x        = NULL,
    y        = "Share below minimum wage",
    caption  = paste(
      "2016 annual average. Weighted over firm size tiers by employment share.",
      "Fill = sector exposure group (tercile of exposure measure).",
      REG_NOTE, SRC, sep = "\n"
    )
  ) +
  theme_surveytools(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


#===============================================================================
# STEP 4. Pre-Trend Analysis by Exposure Group
#
# Key idea: if identification is valid, high- and low-exposure cells should
# show parallel trends BEFORE each event, then diverge AFTER.
#
# We use exposure_group_overall (tercile of exposure_sf_val across ALL cells)
# to split into High / Medium / Low groups, then average each outcome within
# group by quarter. This is the DR analog of Parente's Figures 3 and 4.
#
# Outcomes shown:
#   - log_var_wage       (wage inequality)
#   - informal           (informality share)
#   - below_min_formal   (non-compliance)
#===============================================================================

# Aggregate panel to exposure_group × quarter, weighted by pi
pretrend_data <- panel_sf %>%
  filter(
    !is.na(exposure_group_overall),
    !is.na(log_var_wage),
    !is.na(informal),
    !is.na(below_min_formal)
  ) %>%
  group_by(time, exposure_group_overall) %>%
  summarise(
    log_var_wage     = weighted.mean(log_var_wage,     pi, na.rm = TRUE),
    informal         = weighted.mean(informal,         pi, na.rm = TRUE),
    below_min_formal = weighted.mean(below_min_formal, pi, na.rm = TRUE),
    .groups          = "drop"
  ) %>%
  mutate(
    time                 = as.character(time),
    exposure_group_overall = factor(exposure_group_overall,
                                    levels = c("High exposure",
                                               "Medium exposure",
                                               "Low exposure"))
  )

pt_qtrs <- sort(unique(pretrend_data$time))
pt_epos <- event_pos(pt_qtrs)

# Helper to build one pre-trend panel
make_pretrend_fig <- function(data, yvar, ytitle, title, subtitle) {
  ggplot(data, aes(x = time, y = .data[[yvar]],
                   color = exposure_group_overall,
                   linetype = exposure_group_overall,
                   group = exposure_group_overall)) +
    covid_rect(pt_qtrs) +
    geom_vline(xintercept = pt_epos, linetype = "dashed",
               color = "red", linewidth = 0.4) +
    geom_line(linewidth = 0.75) +
    scale_color_manual(values = EXPOSURE_COLORS, name = "Exposure group") +
    scale_linetype_manual(
      values = setNames(rep("solid", 3), names(EXPOSURE_COLORS)),
      name   = "Exposure group"
    ) +
    labs(
      title    = title,
      subtitle = subtitle,
      y        = ytitle,
      caption  = paste(
        "High/Medium/Low = tercile of 2016 baseline exposure (share of formal workers near min wage).",
        "Averages weighted by firm size employment share (pi) within each cell.",
        MW_NOTE, REG_NOTE, SRC, sep = "\n"
      )
    ) +
    theme_surveytools()
}

fig_pt_01 <- make_pretrend_fig(
  pretrend_data,
  yvar     = "log_var_wage",
  ytitle   = "Log variance of log wages",
  title    = "Wage inequality by exposure group",
  subtitle = "Log variance of log real salary — High, Medium, Low exposure cells"
)

fig_pt_02 <- make_pretrend_fig(
  pretrend_data %>%
    mutate(informal = informal),   # already a proportion
  yvar     = "informal",
  ytitle   = "Share informal",
  title    = "Informality share by exposure group",
  subtitle = "Share of workers classified as informal — High, Medium, Low exposure cells"
) +
  scale_y_continuous(labels = percent_format(accuracy = 1))

fig_pt_03 <- make_pretrend_fig(
  pretrend_data,
  yvar     = "below_min_formal",
  ytitle   = "Share below min wage",
  title    = "Non-compliance by exposure group",
  subtitle = "Share of formal workers earning below min wage — High, Medium, Low exposure cells"
) +
  scale_y_continuous(labels = percent_format(accuracy = 1))

# Combined three-panel pre-trend figure
fig_pt_combined <- (fig_pt_01 / fig_pt_02 / fig_pt_03) +
  plot_annotation(
    title    = "Pre-trend analysis: outcomes by minimum wage exposure group",
    subtitle = paste(
      "Parallel trends before events and divergence after support identification.",
      "Exposure measured at 2016 baseline, fixed over time."
    ),
    theme    = theme(
      plot.title    = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
  )


#===============================================================================
# STEP 5. Save Figures
#===============================================================================

save_fig(fig_nc_01, "fig_nc_01_noncompliance_by_tier")
save_fig(fig_nc_02, "fig_nc_02_noncompliance_by_sector",
         w = config$fig_defaults$width * 1.5,
         h = config$fig_defaults$height * 1.8)
save_fig(fig_nc_03, "fig_nc_03_noncompliance_snapshot")

save_fig(fig_pt_01, "fig_pt_01_var_by_exposure")
save_fig(fig_pt_02, "fig_pt_02_informal_by_exposure")
save_fig(fig_pt_03, "fig_pt_03_noncompliance_by_exposure")
save_fig(fig_pt_combined, "fig_pt_combined",
         w = config$fig_defaults$width,
         h = config$fig_defaults$height * 3)

# Presentation layout: non-compliance snapshot + pre-trend combined
fig_id_combined_full <- (fig_nc_01 | fig_nc_03) /
  (fig_pt_01 | fig_pt_02 | fig_pt_03) +
  plot_annotation(
    title = "Identification: Non-Compliance and Pre-Trend Analysis",
    theme = theme(plot.title = element_text(size = 13, face = "bold"))
  )

save_fig(fig_id_combined_full, "fig_id_noncompliance_pretrend_combined",
         w = config$fig_defaults$width * 2,
         h = config$fig_defaults$height * 2.2)

cat("\n=== 04c_Figures_PreTrend_Noncompliance.R complete ===\n")
