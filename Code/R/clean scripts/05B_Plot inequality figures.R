#===============================================================================
#
# Script: 05B_Plot_Inequality.R
#
# Purpose: Build inequality / wage-compression figures from the RDS objects
#          produced by 05A_Compute_Inequality.R. No survey computation here.
#
# FIGURES PRODUCED (saved to config$out_subdirs$inequality):
#   fig_INEQ1_var_log          Variance of log real hourly earnings over time
#   fig_INEQ2_pctile_ratios    Percentile ratios (p90/p10, p50/p10, p90/p50)
#   fig_INEQ3_density_overlay  2016-vs-2024 density overlay (compression)
#
# READS <- config$paths$processed_data / "Inequality":
#   ineq_var_log.rds, ineq_pctile_ratios.rds, ineq_density_extract.rds
#
# CONVENTIONS: shared with 04B — character x-axis "YYYYQn", event lines by
# integer position, COVID shading, theme_surveytools(), save_fig().
#
#===============================================================================

source("Code/R/setup/00_setup.R")

library(ggplot2)
library(scales)

cat("=== 05B_Plot_Inequality.R ===\n\n")


#===============================================================================
# SHARED HELPERS AND CONSTANTS  (mirror 04B)
#===============================================================================

MW_EVENT_QTR <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")
DIST_FOCAL_YEARS <- c(2016L, 2024L)

# Quarters excluded from the inequality TREND lines (INEQ-1, INEQ-2). The
# COVID-era hourly-wage data is unreliable, so we drop
# all of 2020 from the lines and let the line break across the gap. The COVID
# shading remains to show why. Density overlay (2016 vs 2024) is unaffected.
COVID_QTRS <- c("2020Q1", "2020Q2", "2020Q3", "2020Q4")

SRC <- "Sources: ENCFT 2014Q3-2025Q2; Central Bank of Dominican Republic."
MW_NOTE <- paste(
  "Red dashed verticals: MW announcement quarters (2017Q2, 2019Q3, 2021Q3, 2023Q2).",
  "Grey shading: 2020Q1-2020Q4 (COVID-19)."
)
POP_NOTE <- paste(
  "Formal private full-time wage earners (40-48 usual weekly hours),",
  "positive real hourly base salary.")

in_dir <- file.path(config$paths$processed_data, "Inequality")
read_obj <- function(name) readRDS(file.path(in_dir, paste0(name, ".rds")))

save_path <- file.path(config$paths$outputs, config$output_stage,
                       config$out_subdirs$inequality)
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)

save_fig <- function(p, name,
                     w = config$fig_defaults$width,
                     h = config$fig_defaults$height) {
  ggsave(
    filename = file.path(save_path, paste0(name, ".", config$fig_defaults$format)),
    plot = p, width = w, height = h, dpi = config$fig_defaults$dpi
  )
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

qtr_breaks <- function(qtrs) {
  q <- sort(unique(qtrs)); q[seq(1, length(q), by = 4)]
}


#===============================================================================
# FIGURE INEQ-1: Variance of log real hourly earnings
#
# Falling variance = compression. This is the direct analogue of Parente's
# Figure 1 (variance of log earnings) and the paper's core inequality outcome.
#===============================================================================

cat("[INEQ-1] Variance of log earnings...\n")

vlog <- read_obj("ineq_var_log") %>%
  dplyr::filter(!sparse) %>%
  dplyr::mutate(
    yq_chr = as.character(year_quarter),
    time = yq_chr,
    # Set COVID quarters to NA so the line breaks (rows kept so the x-axis
    # span and COVID shading still cover the gap). Coerce to character first
    # because year_quarter may be a factor after readRDS, which would make the
    # %in% comparison against a character vector silently fail.
    estimate = dplyr::if_else(yq_chr %in% COVID_QTRS, NA_real_, estimate),
    se       = dplyr::if_else(yq_chr %in% COVID_QTRS, NA_real_, se)
  )

qtrs1 <- vlog$time

fig_INEQ1 <- ggplot(vlog, aes(x = time, y = estimate, group = 1)) +
  covid_rect(qtrs1) +
  geom_vline(xintercept = event_pos(qtrs1), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_ribbon(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
              fill = "#1f78b4", alpha = 0.18) +
  geom_line(colour = "#1f78b4", linewidth = 0.8) +
  scale_x_discrete(breaks = qtr_breaks(qtrs1)) +
  labs(
    title    = "Wage Inequality Has Fallen as the Minimum Wage Rose",
    subtitle = "Variance of log real hourly earnings, formal private workers",
    x = NULL, y = "Variance of log real hourly earnings",
    caption = paste("Shaded band: 95% CI.",
                    "2020 (COVID) excluded from the line; data unreliable.",
                    POP_NOTE, MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_INEQ1, "fig_INEQ1_var_log")


#===============================================================================
# FIGURE INEQ-2: Percentile ratios over time
#
# p90/p10 (overall), p50/p10 (lower tail — MW-sensitive), p90/p50 (upper tail).
# If the MW compresses the bottom, p50/p10 should fall most; p90/p50 should be
# comparatively flat (a useful internal placebo).
#===============================================================================

cat("[INEQ-2] Percentile ratios...\n")

ratios <- read_obj("ineq_pctile_ratios") %>%
  dplyr::filter(!sparse) %>%
  dplyr::select(year_quarter, `p90/p10`, `p50/p10`, `p90/p50`) %>%
  tidyr::pivot_longer(-year_quarter, names_to = "ratio", values_to = "value") %>%
  dplyr::mutate(
    yq_chr = as.character(year_quarter),
    time = yq_chr,
    ratio = factor(ratio, levels = c("p90/p10", "p50/p10", "p90/p50")),
    value = dplyr::if_else(yq_chr %in% COVID_QTRS, NA_real_, value)
  )

qtrs2 <- ratios$time

fig_INEQ2 <- ggplot(ratios,
                    aes(x = time, y = value, colour = ratio, group = ratio)) +
  covid_rect(qtrs2) +
  geom_vline(xintercept = event_pos(qtrs2), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(
    values = c("p90/p10" = "#1b1b1b", "p50/p10" = "#1f78b4", "p90/p50" = "#e08214"),
    name = "Percentile ratio") +
  scale_x_discrete(breaks = qtr_breaks(qtrs2)) +
  labs(
    title    = "Wage Inequality Has Narrowed Over Time",
    subtitle = "Percentile ratios of real hourly earnings, formal private workers",
    x = NULL, y = "Ratio",
    caption = paste(
      "p90/p10 falls over the period. p50/p10 (lower tail) and p90/p50",
      "(upper tail) shown separately to locate the compression.",
      "2020 (COVID) excluded from the lines; data unreliable.",
      POP_NOTE, MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_INEQ2, "fig_INEQ2_pctile_ratios")


#===============================================================================
# FIGURE INEQ-3: 2016-vs-2024 density overlay
#
# Cleaner single-panel overlay of the log real hourly earnings distribution
# for the two focal years. Compression = the 2024 curve is taller/narrower.
# Vertical lines mark each year's weighted median for reference.
#===============================================================================

cat("[INEQ-3] Density overlay...\n")

dens <- read_obj("ineq_density_extract") %>%
  dplyr::mutate(
    year_lab = factor(
      paste0(year, ifelse(year == min(year), " (baseline)", " (recent)")),
      levels = c(paste0(min(year), " (baseline)"),
                 paste0(max(year), " (recent)"))
    )
  )

# Weighted medians for reference lines
med_lines <- dens %>%
  dplyr::group_by(year_lab) %>%
  dplyr::summarise(
    med = matrixStats::weightedMedian(log_real_hourly, w = w_norm, na.rm = TRUE),
    .groups = "drop"
  )

YEAR_COLS <- c("#9ecae1", "#08519c")
names(YEAR_COLS) <- levels(dens$year_lab)

fig_INEQ3 <- ggplot(dens,
                    aes(x = log_real_hourly, weight = w_norm,
                        colour = year_lab, fill = year_lab)) +
  stat_density(geom = "area", position = "identity",
               alpha = 0.25, bw = 0.15, colour = NA) +
  stat_density(geom = "line", position = "identity",
               bw = 0.15, linewidth = 0.9) +
  geom_vline(data = med_lines,
             aes(xintercept = med, colour = year_lab),
             linetype = "dashed", linewidth = 0.5, show.legend = FALSE) +
  scale_colour_manual(values = YEAR_COLS, name = NULL) +
  scale_fill_manual(values = YEAR_COLS, name = NULL) +
  labs(
    title    = "The Wage Distribution Compressed Toward the Middle",
    subtitle = "Density of log real hourly earnings, formal private workers",
    x = "Log real hourly earnings (2025Q2 DOP)", y = "Density",
    caption = paste(
      "Dashed verticals: weighted median each year. A taller, narrower 2024",
      "curve indicates compression. Weights normalised within year.",
      POP_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom")

save_fig(fig_INEQ3, "fig_INEQ3_density_overlay",
         w = config$fig_defaults$width * 1.2)


cat("\n=== 05B_Plot_Inequality.R complete ===\n")
cat("Figures saved to:", save_path, "\n\n")