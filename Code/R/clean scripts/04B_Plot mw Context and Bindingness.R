#===============================================================================
#
# Script: 04B_Plot_MW_Context_and_Bindingness.R
#
# Purpose: Build the minimum wage context and bindingness figures from the RDS
#          objects produced by 04A_Compute_MW_Context_and_Bindingness.R.
#          No survey computation happens here — only plotting.
#
# FIGURES PRODUCED (saved to config$out_subdirs$minwage):
#   fig_MW1_minwage_levels         Real MW levels by tier over time
#   fig_MW2_growth_decomp          MW growth decomposed (real vs inflation)
#   fig_MW3_firmsize_shares        Employment share by tier over time
#   fig_MW4_kaitz                  Log Kaitz index by tier over time
#   fig_MW5a_noncompliance_econ    Non-compliance rate, economy-wide
#   fig_MW5b_noncompliance_tier    Non-compliance rate by tier (formal)
#   fig_MW6_bunching_kde           Wage distribution vs MW (KDE, formal)
#   fig_MW6_bunching_hist          Wage distribution vs MW (histogram, formal)
#
# READS  <- config$paths$processed_data / "MW Context and Bindingness":
#   mw_context_levels.rds, mw_context_growth_decomp.rds,
#   mw_context_firmsize_shares.rds, mw_bind_kaitz.rds,
#   mw_bind_noncompliance_econ.rds, mw_bind_noncompliance_tier.rds,
#   mw_bind_dist_formal.rds, mw_bind_dist_mw_ref.rds
#
# CONVENTIONS (shared with the rest of the project):
#   - Character x-axis of "YYYYQn"; event lines drawn by integer position.
#   - theme_surveytools(), TIER_COLORS, save_fig() from the project functions.
#   - Red dashed verticals = MW events; grey rectangle = COVID (2020Q1-Q4).
#
#===============================================================================

source("Code/R/setup/00_setup.R")

library(ggplot2)
library(scales)

cat("=== 04B_Plot_MW_Context_and_Bindingness.R ===\n\n")


#===============================================================================
# SHARED PLOT HELPERS AND CONSTANTS
#===============================================================================

TIER_LEVELS  <- c("Micro", "Small", "Medium", "Large")
MW_EVENT_QTR <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")

# Four-tier palette (no red — red is reserved for event lines)
TIER_COLORS <- c(
  "Micro"  = "#1b7837",
  "Small"  = "#762a83",
  "Medium" = "#e08214",
  "Large"  = "#1f78b4"
)

SRC <- "Sources: ENCFT 2014Q3-2025Q2; Central Bank of Dominican Republic."
MW_NOTE <- paste(
  "Red dashed verticals: MW announcement quarters (2017Q2, 2019Q3, 2021Q3, 2023Q2).",
  "Grey shading: 2020Q1-2020Q4 (COVID-19)."
)

# Read directory for the 04A outputs
in_dir <- file.path(config$paths$processed_data, "MW Context and Bindingness")
read_obj <- function(name) readRDS(file.path(in_dir, paste0(name, ".rds")))

# Save directory + helper (mirrors project save_fig but lets us pass w/h)
save_path <- file.path(config$paths$outputs, config$output_stage,
                       config$out_subdirs$minwage)
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

# Event-line positions on a character x-axis
event_pos <- function(qtrs) which(sort(unique(qtrs)) %in% MW_EVENT_QTR)

# COVID shading rectangle (drawn first so it sits behind the series)
covid_rect <- function(qtrs) {
  xmin <- which(sort(unique(qtrs)) == "2020Q1")
  xmax <- which(sort(unique(qtrs)) == "2020Q4")
  if (!length(xmin) || !length(xmax)) return(NULL)
  annotate("rect", xmin = xmin - 0.5, xmax = xmax + 0.5,
           ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.6)
}

# Show every 4th quarter label on the x-axis to avoid crowding
qtr_breaks <- function(qtrs) {
  q <- sort(unique(qtrs))
  q[seq(1, length(q), by = 4)]
}


#===============================================================================
# FIGURE MW-1: Real minimum wage levels by tier
#===============================================================================

cat("[MW-1] Real minimum wage levels...\n")

mw_levels <- read_obj("mw_context_levels") %>%
  dplyr::mutate(time = year_quarter)

qtrs1 <- mw_levels$time

fig_MW1 <- ggplot(mw_levels,
                  aes(x = time, y = real_minwage_harmonized,
                      colour = Wage_group, group = Wage_group)) +
  covid_rect(qtrs1) +
  geom_vline(xintercept = event_pos(qtrs1), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
  scale_x_discrete(breaks = qtr_breaks(qtrs1)) +
  labs(
    title    = "Real Minimum Wage by Firm Size Tier",
    subtitle = "CPI-deflated Dominican pesos (2025Q2 base)",
    x = NULL, y = "Real minimum wage (2025Q2 DOP)",
    caption = paste(
      "Pre-2021 Micro uses the Small-firm rate (Micro category created 2021).",
      MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_MW1, "fig_MW1_minwage_levels")


#===============================================================================
# FIGURE MW-2: MW growth decomposition at each event
#
# Stacked bars: for each event x tier, the nominal % increase split into the
# real purchasing-power gain (solid) and the inflation component (hatched/light).
# Because real + inflation = nominal, the stacked bar height equals the nominal
# increase. A point marks the nominal total for reference.
#===============================================================================

cat("[MW-2] MW growth decomposition...\n")

decomp <- read_obj("mw_context_growth_decomp")

# Long form for stacking the two components
decomp_long <- decomp %>%
  dplyr::select(year_quarter, Wage_group, real_pct_chg, inflation_component) %>%
  tidyr::pivot_longer(c(real_pct_chg, inflation_component),
                      names_to = "component", values_to = "pct") %>%
  dplyr::mutate(
    component = dplyr::recode(component,
                              real_pct_chg = "Real gain",
                              inflation_component = "Eroded by inflation"),
    component = factor(component, levels = c("Eroded by inflation", "Real gain"))
  )

fig_MW2 <- ggplot(decomp_long,
                  aes(x = Wage_group, y = pct, fill = component)) +
  geom_col(width = 0.7, colour = "white") +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  facet_wrap(~year_quarter, nrow = 1) +
  scale_fill_manual(values = c("Real gain" = "#1b7837",
                               "Eroded by inflation" = "grey70"),
                    name = NULL) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
  labs(
    title    = "Decomposing Each Minimum Wage Increase",
    subtitle = "Cumulative nominal change since previous MW event, split into real gain and inflation erosion",
    x = "Firm size tier", y = "% change since previous MW event",
    caption = paste(
      "Each bar shows the cumulative % change from the prior event quarter to",
      "the current event quarter (panel 1 base: 2015Q2; then 2017Q2, 2019Q3, 2021Q3).",
      "Real gain + inflation component = nominal change, by construction.",
      "Note the ~53% medium-firm increase at 2021Q3, when the micro tier was created.",
      SRC, sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

save_fig(fig_MW2, "fig_MW2_growth_decomp",
         w = config$fig_defaults$width * 1.6,
         h = config$fig_defaults$height)


#===============================================================================
# FIGURE MW-3: Employment share by firm size tier over time
#
# Two views saved: (a) overlaid lines, (b) stacked area. Stacked area is the
# default for the paper (shows composition adding to 100%); lines are easier
# for reading individual tier trends.
#===============================================================================

cat("[MW-3] Employment share by tier...\n")

fs_shares <- read_obj("mw_context_firmsize_shares") %>%
  dplyr::mutate(time = year_quarter)

qtrs3 <- fs_shares$time

# (a) Lines
fig_MW3a <- ggplot(fs_shares,
                   aes(x = time, y = share_of_known,
                       colour = Wage_group, group = Wage_group)) +
  covid_rect(qtrs3) +
  geom_vline(xintercept = event_pos(qtrs3), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(breaks = qtr_breaks(qtrs3)) +
  labs(
    title    = "Employment Share by Firm Size Tier",
    subtitle = "Share of employed workers with known firm size",
    x = NULL, y = "Share of employed (known firm size)",
    caption = paste(
      "All employed (OCUPADO == 1), conditional on reporting a firm size.",
      MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_MW3a, "fig_MW3_firmsize_shares_lines")

# (b) Stacked area
fig_MW3b <- ggplot(fs_shares,
                   aes(x = time, y = share_of_known,
                       fill = Wage_group, group = Wage_group)) +
  geom_area(position = "stack", alpha = 0.85) +
  geom_vline(xintercept = event_pos(qtrs3), linetype = "dashed",
             colour = "white", linewidth = 0.4) +
  scale_fill_manual(values = TIER_COLORS, name = "Firm size tier") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(breaks = qtr_breaks(qtrs3)) +
  labs(
    title    = "Employment Composition by Firm Size",
    subtitle = "Cumulative share adds to 100% within each quarter (known firm size only)",
    x = NULL, y = "Cumulative share",
    caption = paste(
      "All employed (OCUPADO == 1), conditional on reporting a firm size.",
      "White dashed: MW announcement quarters.", SRC, sep = "\n")
  ) +
  theme_surveytools(legend_position = "right")

save_fig(fig_MW3b, "fig_MW3_firmsize_shares_stacked")


#===============================================================================
# FIGURE MW-4: Log Kaitz index by tier over time
#
# Kaitz = log(real tier MW) - log(median formal real salary within tier).
# Values near 0 mean the floor sits near the median (very binding); more
# negative means the floor is well below the median (less binding).
# Sparse cells are dropped to avoid noisy median estimates.
#===============================================================================

cat("[MW-4] Log Kaitz index...\n")

kaitz <- read_obj("mw_bind_kaitz") %>%
  dplyr::filter(!sparse) %>%
  dplyr::mutate(time = year_quarter,
                Wage_group = factor(Wage_group, levels = TIER_LEVELS))

qtrs4 <- kaitz$time

fig_MW4 <- ggplot(kaitz,
                  aes(x = time, y = log_kaitz,
                      colour = Wage_group, group = Wage_group)) +
  covid_rect(qtrs4) +
  geom_vline(xintercept = event_pos(qtrs4), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dotted",
             colour = "grey50", linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
  scale_x_discrete(breaks = qtr_breaks(qtrs4)) +
  labs(
    title    = "Log Kaitz Index by Firm Size Tier",
    subtitle = "log(real minimum wage) - log(median formal salary within tier)",
    x = NULL, y = "Log Kaitz index",
    caption = paste(
      "Higher (closer to 0) = floor nearer the median = more binding.",
      "Denominator: formal private employees with positive salary, by tier.",
      "Sparse tier x quarter cells (n < 30) omitted.",
      MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_MW4, "fig_MW4_kaitz")


#===============================================================================
# FIGURE MW-5a: Non-compliance rate over time, economy-wide
#
# TWO versions saved:
#   (i)  fig_MW5a_noncompliance_econ — headline. Hourly measure, two scopes:
#        all formal private vs. formal private EXCLUDING Large. Shows the
#        ratchet trend is not an artifact of the contaminated Large tier.
#   (ii) fig_MW5a_noncompliance_measures — the monthly-vs-hourly comparison
#        (formal, all tiers), kept for the measurement-gap discussion.
#===============================================================================

cat("[MW-5a] Non-compliance, economy-wide...\n")

nc_econ_all <- read_obj("mw_bind_noncompliance_econ")

# ── (i) Headline: all-tiers vs ex-Large, hourly measure ──────────────────────
nc_scope <- nc_econ_all %>%
  dplyr::filter(concept == "hourly",
                scope %in% c("formal", "formal_ex_large")) %>%
  dplyr::mutate(
    time = year_quarter,
    scope_label = dplyr::recode(scope,
                                formal          = "All tiers",
                                formal_ex_large = "Excluding Large")
  )

qtrs5 <- nc_scope$time

fig_MW5a <- ggplot(nc_scope,
                   aes(x = time, y = nc_rate,
                       colour = scope_label, group = scope_label)) +
  covid_rect(qtrs5) +
  geom_vline(xintercept = event_pos(qtrs5), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("All tiers" = "#1f78b4",
                                 "Excluding Large" = "#33a02c"),
                      name = "Scope") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(breaks = qtr_breaks(qtrs5)) +
  labs(
    title    = "Minimum Wage Non-Compliance Over Time",
    subtitle = "Share of formal private employees below the tier MW (hourly measure)",
    x = NULL, y = "Non-compliance rate",
    caption = paste(
      "Hourly (Measure 2): hourly rate (capped at 44hrs) vs hourly MW.",
      "'Excluding Large' drops the contaminated 100+ survey bin; the rising",
      "trend survives, so it is not an artifact of large-firm misclassification.",
      MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_MW5a, "fig_MW5a_noncompliance_econ")

# ── (ii) Monthly vs hourly measure comparison (formal, all tiers) ─────────────
nc_measures <- nc_econ_all %>%
  dplyr::filter(scope == "formal") %>%
  dplyr::mutate(time = year_quarter)

fig_MW5a_meas <- ggplot(nc_measures,
                        aes(x = time, y = nc_rate,
                            colour = concept_label, group = concept_label)) +
  covid_rect(qtrs5) +
  geom_vline(xintercept = event_pos(qtrs5), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("#1f78b4", "#e08214"),
                      name = "Income measure") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(breaks = qtr_breaks(qtrs5)) +
  labs(
    title    = "Non-Compliance: Monthly vs Hourly Measure",
    subtitle = "Share of formal private employees below the tier MW (all tiers)",
    x = NULL, y = "Non-compliance rate",
    caption = paste(
      "Monthly (Measure 1): monthly earnings vs monthly MW.",
      "Hourly (Measure 2): hourly rate (capped at 44hrs) vs hourly MW.",
      "The stable gap reflects sub-full-time workers who clear the hourly but",
      "not the monthly floor.",
      MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_MW5a_meas, "fig_MW5a_noncompliance_measures")


#===============================================================================
# FIGURE MW-5b: Non-compliance rate by tier (formal, hourly measure)
#
# Faceted by tier so the level differences across firm sizes are legible.
# Hourly (Measure 2 — primary) shown; sparse cells dropped.
#===============================================================================

cat("[MW-5b] Non-compliance by tier...\n")

nc_tier <- read_obj("mw_bind_noncompliance_tier") %>%
  dplyr::filter(scope == "formal", concept == "hourly", !sparse) %>%
  dplyr::mutate(time = year_quarter,
                Wage_group = factor(Wage_group, levels = TIER_LEVELS))

qtrs5b <- nc_tier$time

fig_MW5b <- ggplot(nc_tier,
                   aes(x = time, y = nc_rate,
                       colour = Wage_group, group = Wage_group)) +
  covid_rect(qtrs5b) +
  geom_vline(xintercept = event_pos(qtrs5b), linetype = "dashed",
             colour = "red", linewidth = 0.35) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~Wage_group, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = TIER_COLORS, guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(breaks = qtr_breaks(qtrs5b)) +
  labs(
    title    = "Non-Compliance by Firm Size Tier (Formal Workers)",
    subtitle = "Share below the tier minimum wage, hourly measure (free y-axis)",
    x = NULL, y = "Non-compliance rate",
    caption = paste(
      "Hourly (Measure 2): hourly rate (capped at 44hrs) vs hourly MW.",
      "Formal private employees with known firm size. Sparse cells (n < 30) omitted.",
      "Large-tier non-compliance partly reflects self-reported firm-size measurement error.",
      MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools() +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey93", colour = "grey70"))

save_fig(fig_MW5b, "fig_MW5b_noncompliance_tier",
         w = config$fig_defaults$width * 1.4,
         h = config$fig_defaults$height * 1.3)


#===============================================================================
# FIGURE MW-6: Wage distribution relative to the minimum wage
#
# Two complementary views from the same microdata extract (formal workers,
# focal years 2016 and 2024), using the HOURLY ratio (Measure 2):
#   (a) KDE — overlaid kernel densities by tier, faceted by year
#   (b) Histogram — binned, coloured by compliance, to show bunching at the floor
#
# x = log2(income / tier MW): 0 = floor, +1 = double, -1 = half.
# Grey region (x < 0) marks non-compliance.
#===============================================================================

cat("[MW-6] Wage distribution vs MW...\n")

dist <- read_obj("mw_bind_dist_formal") %>%
  dplyr::filter(!sparse, is.finite(log2_ratio_hourly)) %>%
  # Trim extreme tails for a readable density
  dplyr::filter(log2_ratio_hourly > -3, log2_ratio_hourly < 4) %>%
  dplyr::mutate(
    Wage_group = factor(Wage_group, levels = TIER_LEVELS),
    year_lab   = factor(paste0(year, ifelse(year == min(year),
                                            " (baseline)", " (recent)")))
  )

# Re-normalise weights within year x tier after trimming so densities integrate to 1
dist <- dist %>%
  dplyr::group_by(year, Wage_group) %>%
  dplyr::mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
  dplyr::ungroup()

x_breaks <- c(-2, -1, 0, 1, 2, 3)
x_labels <- c("\u00bc\u00d7 MW", "\u00bd\u00d7 MW", "= MW",
              "2\u00d7 MW", "4\u00d7 MW", "8\u00d7 MW")

# ── (a) KDE ───────────────────────────────────────────────────────────────────
fig_MW6_kde <- ggplot(dist,
                      aes(x = log2_ratio_hourly, weight = w_norm,
                          colour = Wage_group)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "grey92", alpha = 0.7) +
  geom_vline(xintercept = 0, colour = "grey25", linewidth = 0.8) +
  stat_density(aes(linetype = Wage_group), geom = "line",
               bw = 0.15 / log(2), linewidth = 1.0, alpha = 0.9) +
  facet_wrap(~year_lab, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
  scale_linetype_manual(
    values = c("Micro" = "solid", "Small" = "dashed",
               "Medium" = "longdash", "Large" = "dotdash"),
    name = "Firm size tier") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  labs(
    title    = "Formal Worker Earnings Distribution Relative to the Minimum Wage",
    subtitle = "Survey-weighted density by firm size tier. x = 0 is each tier's annual-average real MW (hourly).",
    x = "Hourly earnings relative to tier MW (log\u2082: +1 = double the MW)",
    y = "Density",
    caption = paste(
      "Formal private employees, known firm size; Govt & Electricity excluded.",
      "Grey = non-compliance zone (below MW). Weights normalised within year x tier.",
      "Pre-2021 Micro used the Small rate.", SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_MW6_kde, "fig_MW6_bunching_kde",
         w = config$fig_defaults$width * 1.7,
         h = config$fig_defaults$height)

# ── (b) Histogram (bunching) ──────────────────────────────────────────────────
# Bin on the log2 scale; colour bars by compliance side of the floor.
BIN_W <- 0.25

hist_df <- dist %>%
  dplyr::mutate(
    bin_mid   = BIN_W * floor(log2_ratio_hourly / BIN_W) + BIN_W / 2,
    compliant = bin_mid >= 0
  ) %>%
  dplyr::group_by(year_lab, Wage_group, bin_mid, compliant) %>%
  dplyr::summarise(wt = sum(w_norm, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(year_lab, Wage_group) %>%
  dplyr::mutate(prop = wt / sum(wt)) %>%
  dplyr::ungroup()

fig_MW6_hist <- ggplot(hist_df,
                       aes(x = bin_mid, y = prop, fill = compliant)) +
  geom_col(width = BIN_W * 0.95) +
  geom_vline(xintercept = 0, colour = "grey25", linewidth = 0.6) +
  facet_grid(Wage_group ~ year_lab, scales = "free_y") +
  scale_fill_manual(values = c("FALSE" = "#C45C30", "TRUE" = "#1f78b4"),
                    labels = c("FALSE" = "Below MW", "TRUE" = "At/above MW"),
                    name = NULL) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels,
                     limits = c(-3, 4)) +
  labs(
    title    = "Bunching at the Minimum Wage — Formal Workers by Tier",
    subtitle = "Weighted share of workers by hourly earnings relative to the tier MW",
    x = "Hourly earnings relative to tier MW (log\u2082: +1 = double the MW)",
    y = "Share of workers",
    caption = paste(
      "Bars left of the floor (red) = non-compliant. Bin width 0.25 log\u2082 units.",
      "Formal private employees, known firm size; Govt & Electricity excluded.",
      SRC, sep = "\n")
  ) +
  theme_surveytools() +
  theme(strip.text = element_text(face = "bold", size = 8),
        strip.background = element_rect(fill = "grey93", colour = "grey70"))

save_fig(fig_MW6_hist, "fig_MW6_bunching_hist",
         w = config$fig_defaults$width * 1.5,
         h = config$fig_defaults$height * 1.8)


#===============================================================================
# DONE
#===============================================================================

cat("\n=== 04B_Plot_MW_Context_and_Bindingness.R complete ===\n")
cat("Figures saved to:", save_path, "\n\n")