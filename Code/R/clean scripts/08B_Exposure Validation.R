#===============================================================================
#
# Script: 08B_Exposure_Validation.R
#
# Purpose: DESCRIPTIVE validation of the exposure treatment before estimation.
#          No estimation, no reference category, no identification claims — this
#          is a checkpoint to eyeball (1) variation in the treatment and (2)
#          whether high/low-exposure regions moved in parallel before the MW
#          events. Tercile grouping is used purely as a lens for looking.
#
# FIGURES PRODUCED (saved to config$out_subdirs$inequality_minwage):
#   fig_EXP1_region_bar     Region exposure ranked, coloured by tercile (Goal 1)
#   fig_EXP2_region_tier    Region x tier exposure heatmap                (Goal 1)
#   fig_EXP3_pretrends       Outcome trends by exposure tercile, events marked
#                            (one panel per outcome)                       (Goal 2)
#   fig_EXP4_bite            "Exposure predicts bite": non-compliance / near-min
#                            over time by exposure tercile                 (Goal 2)
#
# READS:
#   exposure_geo_<tier>.rds       (07_Construction of mw Exposure.R)
#   exposure_cells_<tier>.rds     (07, for the region x tier heatmap)
#   panel_geo_quarter_<tier>.rds  (08_Prepare_Regression_Panel.R)
#
# CONVENTIONS: mirror 04B/05B — character x-axis "YYYYQn", event lines by integer
# position, COVID shading, theme_surveytools(), save_fig().
#
# NOTE ON GROUPING: descriptive terciles here are NOT the estimation design. The
# main estimate (script 09) uses CONTINUOUS exposure (no omitted region). Terciles
# only group regions for legible plotting; with 10 regions each tercile pools ~3
# regions, so trend lines are shown WITH underlying region points to expose noise.
#
#===============================================================================

source(here::here("Code","R","clean scripts","00_setup.R"))


cat("=== 08B_Exposure_Validation.R ===\n\n")


#===============================================================================
# SHARED HELPERS AND CONSTANTS (mirror 04B/05B)
#===============================================================================

TIER_SCHEME <- config$exposure$tier_scheme
GEO         <- config$exposure$construct_geo   # Region10

MW_EVENT_QTR <- config$events$event_qtrs
PHASE_IN     <- config$events$phase_in_qtrs
COVID_QTRS   <- config$events$covid_qtrs

SRC <- "Sources: ENCFT 2014Q3-2025Q2; Central Bank of Dominican Republic."
REG_NOTE <- paste(
  "Regression sample: private sector salaried employees, positive earnings,",
  "known firm size, excl. government and electricity & water."
)
MW_NOTE <- paste(
  "Red dashed verticals: MW announcement quarters (2017Q2, 2019Q3, 2021Q3, 2023Q2).",
  "Grey shading: 2020Q1-2020Q4 (COVID-19)."
)

# Tercile palette (descriptive lens only; no red — red reserved for event lines)
EXP_COLORS <- c(
  "Low exposure"    = "#4575b4",
  "Medium exposure" = "#bdbdbd",
  "High exposure"   = "#d73027"  # dark red-ish for HIGH group is fine (not a line)
)
# For trend LINES we avoid red (reserved for event verticals); use a safe triplet
EXP_LINE_COLORS <- c(
  "Low exposure"    = "#4575b4",  # blue
  "Medium exposure" = "#999999",  # grey
  "High exposure"   = "#1b7837"   # green
)

pd <- config$data_dirs$regression

exposure_geo   <- readRDS(tagged_rds(pd, "exposure_geo"))
exposure_cells <- readRDS(tagged_rds(pd, "exposure_cells"))
panel_gq       <- readRDS(tagged_rds(pd, "panel_geo_quarter"))

save_path <- file.path(config$paths$outputs, config$output_stage,
                       config$out_subdirs$reg_results)

dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
save_fig <- function(p, name,
                     w = config$fig_defaults$width, h = config$fig_defaults$height) {
  ggsave(file.path(save_path, paste0(name, ".", config$fig_defaults$format)),
         plot = p, width = w, height = h, dpi = config$fig_defaults$dpi)
  message("Saved: ", name)
}

# Event lines + COVID shading on a character x-axis (identical idiom to 04B)
event_pos  <- function(qtrs) which(sort(unique(qtrs)) %in% MW_EVENT_QTR)
covid_rect <- function(qtrs) {
  xmin <- which(sort(unique(qtrs)) == "2020Q1")
  xmax <- which(sort(unique(qtrs)) == "2020Q4")
  if (!length(xmin) || !length(xmax)) return(NULL)
  annotate("rect", xmin = xmin - 0.5, xmax = xmax + 0.5,
           ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.6)
}
qtr_breaks <- function(qtrs) { q <- sort(unique(qtrs)); q[seq(1, length(q), by = 4)] }


#===============================================================================
# LOAD exposure + panel
#===============================================================================

exposure_geo   <- readRDS(tagged_rds(pd, "exposure_geo"))
exposure_cells <- readRDS(tagged_rds(pd, "exposure_cells"))
panel_gq       <- readRDS(tagged_rds(pd, "panel_geo_quarter"))

# tier variable name for the heatmap
TIER_VAR <- if (TIER_SCHEME == "4tier") "Wage_group" else "Wage_group_3tier"


#===============================================================================
# FIGURE EXP-1: Region exposure, ranked, coloured by tercile  (Goal 1: variation)
#===============================================================================

cat("[EXP-1] Region exposure bar chart...\n")

exp_bar <- exposure_geo %>%
  mutate(region = factor(.data[[GEO]], levels = .data[[GEO]][order(exposure_geo_val)]))

fig_EXP1 <- ggplot(exp_bar,
                   aes(x = exposure_geo_val, y = region, fill = exposure_group)) +
  geom_col() +
  scale_fill_manual(values = EXP_COLORS, name = "Exposure group") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Minimum-wage exposure by region (2016 baseline)",
    subtitle = "Share of formal workers within the tier-specific MW band, weighted over firm sizes",
    x = "Exposure (share near min wage)", y = NULL,
    caption = paste(REG_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_EXP1, "fig_EXP1_region_bar")


#===============================================================================
# FIGURE EXP-2: Region x tier exposure heatmap  (Goal 1: where exposure sits)
#===============================================================================

cat("[EXP-2] Region x tier heatmap...\n")

# order regions by overall exposure (from EXP-1) so the heatmap reads consistently
region_order <- exp_bar$region %>% levels()

heat <- exposure_cells %>%
  mutate(region = factor(.data[[GEO]], levels = region_order))

fig_EXP2 <- ggplot(heat,
                   aes(x = .data[[TIER_VAR]], y = region, fill = exposure_val)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = percent(exposure_val, accuracy = 1)), size = 3) +
  scale_fill_gradient(low = "grey95", high = "grey20",
                      labels = percent_format(accuracy = 1), name = "Exposure") +
  labs(
    title    = paste0("Minimum-wage exposure by region x firm size (2016 baseline, ",
                      TIER_SCHEME, ")"),
    subtitle = "Share of formal workers within the tier-specific MW band",
    x = "Firm size tier", y = NULL,
    caption = paste(REG_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_EXP2, "fig_EXP2_region_tier")


#===============================================================================
# FIGURE EXP-3: Pre-trends — outcome trends by exposure tercile  (Goal 2)
#
# The core parallel-trends eyeball. For each outcome, plot the survey-weighted
# region-quarter outcome, GROUP-averaged by exposure tercile, with underlying
# region-quarter points shown lightly so tercile-average noise is visible.
# If tercile groups move in parallel PRE-2017 and diverge AFTER, that supports
# the design. This is UNCONDITIONAL (no FE, no regression).
#===============================================================================

cat("[EXP-3] Pre-trends by exposure tercile...\n")

OUTCOMES <- c(
  log_var_wage = "Variance of log hourly wage (formal)",
  below_min    = "Non-compliance: share below tier MW (formal)",
  informal     = "Informal share (private employees)"
)

# tag each region-quarter row with its exposure tercile (from fixed exposure)
panel_grp <- panel_gq %>%
  filter(!is.na(exposure_group)) %>%
  mutate(time = factor(time, levels = sort(unique(time))))

qtrs <- levels(panel_grp$time)

make_pretrend <- function(outcome, label) {
  # group mean per tercile x quarter (simple mean of region values within group;
  # region values are already survey-weighted cell estimates)
  grp <- panel_grp %>%
    filter(!is.na(.data[[outcome]])) %>%
    group_by(exposure_group, time) %>%
    summarise(y = mean(.data[[outcome]], na.rm = TRUE), .groups = "drop")
  
  ggplot() +
    covid_rect(qtrs) +
    geom_vline(xintercept = event_pos(qtrs), linetype = "dashed",
               colour = "red", linewidth = 0.4) +
    # underlying region-quarter points (noise made visible)
    geom_point(data = panel_grp %>% filter(!is.na(.data[[outcome]])),
               aes(x = time, y = .data[[outcome]], colour = exposure_group),
               alpha = 0.20, size = 0.7) +
    # tercile group means
    geom_line(data = grp,
              aes(x = time, y = y, colour = exposure_group, group = exposure_group),
              linewidth = 0.8) +
    scale_colour_manual(values = EXP_LINE_COLORS, name = "Exposure group") +
    scale_x_discrete(breaks = qtr_breaks(qtrs)) +
    labs(title = label, x = NULL, y = NULL) +
    theme_surveytools() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# one file per outcome (keeps each legible; matches per-figure convention)
for (nm in names(OUTCOMES)) {
  p <- make_pretrend(nm, OUTCOMES[[nm]])
  p <- p + labs(caption = paste(MW_NOTE, REG_NOTE, SRC, sep = "\n"))
  save_fig(p, paste0("fig_EXP3_pretrends_", nm))
}


#===============================================================================
# FIGURE EXP-4: Exposure predicts bite  (Goal 2, upstream check)
#
# Does the exposure measure actually proxy MW bite? Plot non-compliance
# (below_min) over time by exposure tercile. High-exposure regions should show
# MORE bite movement around events. If they don't, the treatment variable is
# not capturing what the design needs. (below_min is the cleanest bite proxy in
# the panel; near-min share is the construction target itself.)
#===============================================================================

cat("[EXP-4] Exposure-predicts-bite...\n")

fig_EXP4 <- local({
  grp <- panel_grp %>%
    filter(!is.na(below_min)) %>%
    group_by(exposure_group, time) %>%
    summarise(y = mean(below_min, na.rm = TRUE), .groups = "drop")
  
  ggplot(grp, aes(x = time, y = y, colour = exposure_group, group = exposure_group)) +
    covid_rect(qtrs) +
    geom_vline(xintercept = event_pos(qtrs), linetype = "dashed",
               colour = "red", linewidth = 0.4) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = EXP_LINE_COLORS, name = "Exposure group") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_x_discrete(breaks = qtr_breaks(qtrs)) +
    labs(
      title    = "Does exposure predict minimum-wage bite?",
      subtitle = "Share of formal workers below the tier MW, by 2016 exposure group",
      x = NULL, y = "Share below tier MW",
      caption = paste(MW_NOTE, REG_NOTE, SRC, sep = "\n")
    ) +
    theme_surveytools() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

save_fig(fig_EXP4, "fig_EXP4_bite")


#===============================================================================
# TABLE: exposure variation summary (numeric companion to EXP-1)
#===============================================================================

exp_var_tbl <- exposure_geo %>%
  summarise(
    n_regions = dplyr::n(),
    min = min(exposure_geo_val), median = median(exposure_geo_val),
    max = max(exposure_geo_val), sd = sd(exposure_geo_val),
    iqr = IQR(exposure_geo_val),
    cv  = sd(exposure_geo_val) / mean(exposure_geo_val)
  )
saveRDS(exp_var_tbl, file.path(pd, paste0("exposure_variation_", TIER_SCHEME, ".rds")))
cat("\n[EXP] Exposure variation across regions:\n")
print(as.data.frame(exp_var_tbl), row.names = FALSE)

cat("\n=== 08B_Exposure_Validation.R complete ===\n")