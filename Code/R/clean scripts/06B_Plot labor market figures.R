#===============================================================================
#
# Script: 06B_Plot_Labor_Market.R
#
# Purpose: Build labor-market figures from the RDS objects produced by
#          06A_Compute_Labor_Market.R. No survey computation here.
#
# FIGURES PRODUCED (saved to config$out_subdirs$labor_market):
#   fig_LM1_epop              EPOP over time, by sex
#   fig_LM2_hours_trend       Mean weekly hours over time, by sex
#   fig_LM3_hours_dist        Hours distribution, 2016 vs 2024
#   fig_LM4_wage_growth       Real wage growth by percentile around MW events
#
# READS <- config$paths$processed_data / "Labor Market":
#   lm_epop.rds, lm_hours_trend.rds, lm_hours_extract.rds,
#   lm_wage_growth_events.rds
#
# CONVENTIONS: shared with 04B/05B.
#
#===============================================================================

source("Code/R/setup/00_setup.R")

library(ggplot2)
library(scales)

cat("=== 06B_Plot_Labor_Market.R ===\n\n")


#===============================================================================
# SHARED HELPERS AND CONSTANTS
#===============================================================================

MW_EVENT_QTR <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")

SRC <- "Sources: ENCFT 2014Q3-2025Q2; Central Bank of Dominican Republic."
MW_NOTE <- paste(
  "Red dashed verticals: MW announcement quarters (2017Q2, 2019Q3, 2021Q3, 2023Q2).",
  "Grey shading: 2020Q1-2020Q4 (COVID-19)."
)

SEX_COLORS <- c("All" = "#1b1b1b", "Male" = "#1f78b4", "Female" = "#e7298a")

in_dir <- file.path(config$paths$processed_data, "Labor Market")
read_obj <- function(name) readRDS(file.path(in_dir, paste0(name, ".rds")))

save_path <- file.path(config$paths$outputs, config$output_stage,
                       config$out_subdirs$labor_market)
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
save_fig <- function(p, name,
                     w = config$fig_defaults$width, h = config$fig_defaults$height) {
  ggsave(file.path(save_path, paste0(name, ".", config$fig_defaults$format)),
         plot = p, width = w, height = h, dpi = config$fig_defaults$dpi)
  message("Saved: ", name)
}

event_pos <- function(qtrs) which(sort(unique(qtrs)) %in% MW_EVENT_QTR)
covid_rect <- function(qtrs) {
  xmin <- which(sort(unique(qtrs)) == "2020Q1")
  xmax <- which(sort(unique(qtrs)) == "2020Q4")
  if (!length(xmin) || !length(xmax)) return(NULL)
  annotate("rect", xmin = xmin - 0.5, xmax = xmax + 0.5,
           ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.6)
}
qtr_breaks <- function(qtrs) { q <- sort(unique(qtrs)); q[seq(1, length(q), 4)] }


#===============================================================================
# FIGURE LM-1: EPOP over time, by sex
#===============================================================================

cat("[LM-1] EPOP by sex...\n")

epop <- read_obj("lm_epop") %>%
  dplyr::filter(!sparse) %>% dplyr::mutate(time = year_quarter)
qtrs <- epop$time

fig_LM1 <- ggplot(epop, aes(x = time, y = epop, colour = Sex, group = Sex)) +
  covid_rect(qtrs) +
  geom_vline(xintercept = event_pos(qtrs), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = SEX_COLORS, name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(breaks = qtr_breaks(qtrs)) +
  labs(
    title    = "Employment-to-Population Ratio",
    subtitle = "Share of working-age population (15-64) employed",
    x = NULL, y = "Employment / working-age population",
    caption = paste("Numerator: OCUPADO == 1. Denominator: age 15-64.",
                    MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_LM1, "fig_LM1_epop")


#===============================================================================
# FIGURE LM-2: Mean weekly hours over time, by sex
#===============================================================================

cat("[LM-2] Mean weekly hours...\n")

hours <- read_obj("lm_hours_trend") %>%
  dplyr::filter(!sparse) %>% dplyr::mutate(time = year_quarter)
qtrs2 <- hours$time

fig_LM2 <- ggplot(hours, aes(x = time, y = mean_hours, colour = Sex, group = Sex)) +
  covid_rect(qtrs2) +
  geom_vline(xintercept = event_pos(qtrs2), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_hline(yintercept = 44, linetype = "dotted", colour = "grey50") +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = SEX_COLORS, name = NULL) +
  scale_x_discrete(breaks = qtr_breaks(qtrs2)) +
  labs(
    title    = "Mean Usual Weekly Hours",
    subtitle = "Employed workers with positive primary-job hours",
    x = NULL, y = "Mean weekly hours (primary job)",
    caption = paste("Dotted line: 44-hour standard work week.",
                    MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_LM2, "fig_LM2_hours_trend")


#===============================================================================
# FIGURE LM-3: Hours distribution, 2016 vs 2024
#
# Histogram of usual weekly hours for the two focal years, with the 44-hour
# full-time line marked. Shows whether the hours distribution shifted (e.g.
# more part-time work) over the period.
#===============================================================================

cat("[LM-3] Hours distribution...\n")

hd <- read_obj("lm_hours_extract") %>%
  dplyr::mutate(year_lab = factor(
    paste0(year, ifelse(year == min(year), " (baseline)", " (recent)")),
    levels = c(paste0(min(year), " (baseline)"),
               paste0(max(year), " (recent)"))))

# Weighted histogram via binning (5-hour bins)
BINW <- 5
hd_bin <- hd %>%
  dplyr::mutate(bin = BINW * floor(hours / BINW) + BINW / 2) %>%
  dplyr::group_by(year_lab, bin) %>%
  dplyr::summarise(wt = sum(w_norm), .groups = "drop")

fig_LM3 <- ggplot(hd_bin, aes(x = bin, y = wt, fill = year_lab)) +
  geom_col(position = "dodge", width = BINW * 0.9) +
  geom_vline(xintercept = 44, linetype = "dotted", colour = "grey30") +
  scale_fill_manual(values = c("#9ecae1", "#08519c"), name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Distribution of Usual Weekly Hours",
    subtitle = "Employed workers, 2016 vs 2024",
    x = "Usual weekly hours (primary job)", y = "Share of workers",
    caption = paste("Dotted line: 44-hour standard week. 5-hour bins.",
                    "Weights normalised within year.", SRC, sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom")

save_fig(fig_LM3, "fig_LM3_hours_dist")


#===============================================================================
# FIGURE LM-4: Real wage growth by percentile around MW events
#
# For each event, % change in real hourly wage from the pre-event quarter to
# the event quarter, at p10/p25/p50/p75/p90. Downward-sloping bars (low > high)
# indicate the MW compressed the distribution that cycle.
#===============================================================================

cat("[LM-4] Wage growth by percentile around events...\n")

wg <- read_obj("lm_wage_growth_events") %>%
  dplyr::filter(!sparse)

fig_LM4 <- ggplot(wg, aes(x = pctile, y = pct_growth, fill = pctile)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  facet_wrap(~event, nrow = 1) +
  scale_fill_brewer(palette = "Blues", guide = "none") +
  scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
  labs(
    title    = "Real Wage Growth by Percentile Around Each MW Event",
    subtitle = "Change in real hourly wage from pre-event to event quarter, formal private workers",
    x = "Percentile of the wage distribution", y = "% change in real hourly wage",
    caption = paste(
      "Larger gains at low percentiles (left bars taller) indicate the MW",
      "compressed the lower tail that cycle. Pre-event = quarter before the event.",
      SRC, sep = "\n")
  ) +
  theme_surveytools() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(fig_LM4, "fig_LM4_wage_growth",
         w = config$fig_defaults$width * 1.6)


cat("\n=== 06B_Plot_Labor_Market.R complete ===\n")
cat("Figures saved to:", save_path, "\n\n")