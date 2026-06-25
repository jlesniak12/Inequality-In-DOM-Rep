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
library(patchwork)   # for the two-panel INEQ-4 layout

cat("=== 05B_Plot_Inequality.R ===\n\n")


#===============================================================================
# SHARED HELPERS AND CONSTANTS  (mirror 04B)
#===============================================================================

MW_EVENT_QTR <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")
DIST_FOCAL_YEARS <- c(2016L, 2024L)

# COVID quarters, kept as a named constant for reference. Earlier versions
# excluded these from the trend lines; we now KEEP them (the pandemic
# disruption is itself informative and the full-time restriction tamed the
# spike) and rely on the grey COVID shading + a caption caveat instead.
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

# COVID-spike handling: compute a y-axis cap so normal-time variation is
# legible. The cap is `mult` x the maximum value OUTSIDE the COVID+recovery
# window (2020Q1-2021Q1), which excludes both the spike and the elevated
# recovery tail so the cap reflects genuine normal-time variation. The COVID
# spike (and the descending recovery tail) then run off the top of the panel
# via coord_cartesian, which zooms without deleting data. `df` must have a
# `time` column and the value column named by `valcol`.
#
# mult = 2 leaves headroom so the recovery descent re-enters near the top
# rather than being invisibly off-scale, while keeping the normal range
# (typically ~0.25 for variance) comfortably mid-panel.
CAP_EXCLUDE_QTRS <- c("2020Q1", "2020Q2", "2020Q3", "2020Q4", "2021Q1")

covid_cap <- function(df, valcol, mult = 2, exclude = CAP_EXCLUDE_QTRS) {
  normal_time <- df[!df$time %in% exclude, valcol, drop = TRUE]
  spike_vals  <- df[df$time %in% COVID_QTRS, valcol, drop = TRUE]
  list(
    cap  = max(normal_time, na.rm = TRUE) * mult,
    peak = if (length(spike_vals)) max(spike_vals, na.rm = TRUE) else NA_real_
  )
}

# x-position to place the off-scale annotation (centre of the COVID band)
covid_label_x <- function(qtrs) {
  pos <- which(sort(unique(qtrs)) == "2020Q2")
  if (length(pos)) pos else which(sort(unique(qtrs)) == "2020Q1")
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
  dplyr::mutate(time = as.character(year_quarter))

qtrs1 <- vlog$time
# Tight hard cap for INEQ-1 at 0.5: single-series trend figure, so the COVID
# spike adds nothing — we want the normal-time decline (~0.4 -> ~0.25) to fill
# the panel. covid_cap() is still used to retrieve the true peak for the
# annotation; the cap value itself is overridden to a fixed 0.5.
cap1 <- covid_cap(vlog, "estimate")
cap1$cap <- 0.5

fig_INEQ1 <- ggplot(vlog, aes(x = time, y = estimate, group = 1)) +
  covid_rect(qtrs1) +
  geom_vline(xintercept = event_pos(qtrs1), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_ribbon(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
              fill = "#1f78b4", alpha = 0.18) +
  geom_line(colour = "#1f78b4", linewidth = 0.8) +
  annotate("text", x = covid_label_x(qtrs1), y = cap1$cap * 0.95,
           label = sprintf("2020 peak \u2248 %.1f\n(off scale)", cap1$peak),
           size = 3, hjust = 0.5, colour = "grey30") +
  coord_cartesian(ylim = c(0, cap1$cap)) +
  scale_x_discrete(breaks = qtr_breaks(qtrs1)) +
  labs(
    title    = "Wage Inequality Has Fallen as the Minimum Wage Rose",
    subtitle = "Variance of log real hourly earnings, formal private workers",
    x = NULL, y = "Variance of log real hourly earnings",
    caption = paste("Shaded band: 95% CI. y-axis capped; 2020 COVID spike runs off scale.",
                    "COVID-era (2020) estimates reflect pandemic disruption; read with caution.",
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
    time = as.character(year_quarter),
    ratio = factor(ratio, levels = c("p90/p10", "p50/p10", "p90/p50"))
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
      "COVID-era (2020) estimates reflect pandemic disruption; read with caution.",
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


#===============================================================================
# ============================  PARENTE SET  ==================================
# Monthly earnings, all wage earners, Overall / Formal / Informal.
# Matches the established project palette (SERIES_COLORS_3_VAR).
#===============================================================================

# Shared three-series styling (matches Code/R/descriptive figures/04_*.R fig_03)
SERIES_COLORS_3 <- c("Overall" = "black", "Formal" = "#1f78b4",
                     "Informal" = "#e08214")
SERIES_LTY_3    <- c("Overall" = "solid", "Formal" = "solid",
                     "Informal" = "longdash")
POP_NOTE_M <- "All wage earners (salaried private + public), positive real monthly salary."


#===============================================================================
# FIGURE INEQ-4: Variance of log MONTHLY earnings, Overall/Formal/Informal
#                (Parente 2024 Fig 1 replication)
#
# Parente's pattern: formal inequality falls steeply, informal stays roughly
# flat, overall sits between. The DR series can be read the same way.
#===============================================================================

cat("[INEQ-4] Variance of log monthly earnings by formality...\n")

vlogf <- read_obj("ineq_var_log_formality") %>%
  dplyr::filter(!sparse) %>%
  dplyr::mutate(time = as.character(year_quarter),
                group = factor(group, levels = names(SERIES_COLORS_3)))

qtrs4 <- vlogf$time

# INEQ-4 uses TWO side-by-side panels to show two things at once, since the
# ~30:1 ratio between the COVID spike and the normal-time gap makes a single
# axis (or an inset) unworkable:
#   - LEFT  "Normal times": y-axis capped at 1.0 so the formal/informal LEVEL
#     difference (the Parente-replication point) is clearly legible.
#   - RIGHT "Full range":   uncapped, so the COVID response — and the finding
#     that informal spikes LESS than formal/overall — is fully visible.
# Built as two ggplots combined with patchwork (equal width, shared bottom
# legend) rather than facets, for full per-panel scale control.

MAIN_CAP4 <- 1.0

# Shared geometry builder so both panels are visually identical except scale
build_INEQ4 <- function() {
  ggplot(vlogf,
         aes(x = time, y = estimate,
             colour = group, linetype = group, group = group)) +
    covid_rect(qtrs4) +
    geom_vline(xintercept = event_pos(qtrs4), linetype = "dashed",
               colour = "red", linewidth = 0.4) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = SERIES_COLORS_3, name = NULL) +
    scale_linetype_manual(values = SERIES_LTY_3, name = NULL) +
    scale_x_discrete(breaks = qtr_breaks(qtrs4))
}

# LEFT panel: capped to the normal-time range
fig_INEQ4_capped <- build_INEQ4() +
  coord_cartesian(ylim = c(0, MAIN_CAP4)) +
  labs(subtitle = "Normal times (y-axis capped at 1.0)",
       x = NULL, y = "Variance of log monthly earnings") +
  theme_surveytools(legend_position = "none")

# RIGHT panel: full uncapped range
fig_INEQ4_full <- build_INEQ4() +
  labs(subtitle = "Full range (incl. COVID spike)",
       x = NULL, y = NULL) +
  theme_surveytools(legend_position = "none")

# Compose: side by side, equal width, one shared legend at the bottom,
# with overall title/caption via patchwork annotation.
fig_INEQ4 <- (fig_INEQ4_capped | fig_INEQ4_full) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Variance of Log Earnings by Formality",
    subtitle = "Monthly real earnings, wage earners (Overall, Formal, Informal)",
    caption = paste(
      "Replicates Parente (2024) Fig 1 for the Dominican Republic.",
      "Left panel y-axis capped at 1.0 to show the normal-time formal/informal gap;",
      "right panel shows the full range. The COVID (2020) spike is smaller for informal.",
      "COVID-era (2020) estimates reflect pandemic disruption; read with caution.",
      POP_NOTE_M, MW_NOTE, SRC, sep = "\n"),
    theme = theme(legend.position = "bottom",
                  plot.caption = element_text(hjust = 0, size = 8, colour = "grey40"))
  )

save_fig(fig_INEQ4, "fig_INEQ4_var_log_formality",
         w = config$fig_defaults$width * 1.8)


#===============================================================================
# FIGURE INEQ-5: Percentile ratios by formality (faceted)
#
# Same three ratios as INEQ-2, but on monthly earnings, faceted by group so
# the formal/informal/overall trajectories are each legible.
#===============================================================================

cat("[INEQ-5] Percentile ratios by formality...\n")

ratiosf <- read_obj("ineq_pctile_ratios_formality") %>%
  dplyr::filter(!sparse) %>%
  dplyr::select(year_quarter, group, `p90/p10`, `p50/p10`, `p90/p50`) %>%
  tidyr::pivot_longer(c(`p90/p10`, `p50/p10`, `p90/p50`),
                      names_to = "ratio", values_to = "value") %>%
  dplyr::mutate(
    time = as.character(year_quarter),
    ratio = factor(ratio, levels = c("p90/p10", "p50/p10", "p90/p50")),
    group = factor(group, levels = names(SERIES_COLORS_3))
  )

qtrs5 <- ratiosf$time

fig_INEQ5 <- ggplot(ratiosf,
                    aes(x = time, y = value, colour = ratio, group = ratio)) +
  covid_rect(qtrs5) +
  geom_vline(xintercept = event_pos(qtrs5), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~group, nrow = 1) +
  scale_colour_manual(
    values = c("p90/p10" = "#1b1b1b", "p50/p10" = "#1f78b4", "p90/p50" = "#e08214"),
    name = "Percentile ratio") +
  scale_x_discrete(breaks = qtr_breaks(qtrs5)) +
  labs(
    title    = "Percentile Ratios by Formality",
    subtitle = "Monthly real earnings, wage earners",
    x = NULL, y = "Ratio",
    caption = paste(
      "COVID-era (2020) estimates reflect pandemic disruption; read with caution.",
      POP_NOTE_M, MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))

save_fig(fig_INEQ5, "fig_INEQ5_pctile_ratios_formality",
         w = config$fig_defaults$width * 1.7)


#===============================================================================
# FIGURE INEQ-6: Density overlay 2016 vs 2024, faceted by formality
#===============================================================================

cat("[INEQ-6] Density overlay by formality...\n")

densf <- read_obj("ineq_density_formality") %>%
  dplyr::mutate(
    year_lab = factor(
      paste0(year, ifelse(year == min(year), " (baseline)", " (recent)")),
      levels = c(paste0(min(year), " (baseline)"),
                 paste0(max(year), " (recent)"))),
    Employment_Status = factor(Employment_Status, levels = c("Formal", "Informal"))
  )

fig_INEQ6 <- ggplot(densf,
                    aes(x = log_real_monthly, weight = w_norm,
                        colour = year_lab, fill = year_lab)) +
  stat_density(geom = "area", position = "identity",
               alpha = 0.25, bw = 0.15, colour = NA) +
  stat_density(geom = "line", position = "identity",
               bw = 0.15, linewidth = 0.9) +
  facet_wrap(~Employment_Status, nrow = 1) +
  scale_colour_manual(values = c("#9ecae1", "#08519c"), name = NULL) +
  scale_fill_manual(values = c("#9ecae1", "#08519c"), name = NULL) +
  labs(
    title    = "Earnings Distribution by Formality, 2016 vs 2024",
    subtitle = "Density of log real monthly earnings, wage earners",
    x = "Log real monthly earnings (2025Q2 DOP)", y = "Density",
    caption = paste(
      "Weights normalised within year x formality.",
      POP_NOTE_M, SRC, sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom")

save_fig(fig_INEQ6, "fig_INEQ6_density_formality",
         w = config$fig_defaults$width * 1.5)


cat("\n=== 05B_Plot_Inequality.R complete ===\n")
cat("Figures saved to:", save_path, "\n\n")