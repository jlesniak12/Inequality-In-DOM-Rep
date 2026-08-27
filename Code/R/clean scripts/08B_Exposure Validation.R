#===============================================================================
#
# Script: 08B_Exposure_Validation.R   [REVISED]
#
# Purpose: DESCRIPTIVE validation of the exposure treatment before estimation.
#          No regression, no reference category, no identification claims - a
#          checkpoint to eyeball (1) variation in the treatment and (2) whether
#          high/low-exposure regions moved in parallel before the MW events.
#          Tercile grouping is used purely as a lens for looking; the estimation
#          in 09 uses continuous exposure.
#
# Parameterized by config$active_baseline and config$active_income (same axes as
# 07A / 08 / 09). Reads the tagged exposure and panel files for the currently
# active combination and writes figures + tables into a matching folder tree.
#
# Output tree:
#   <exp_validation>/<income>/<baseline>/
#     fig_EXP1_region_bar.png             Ranked region exposure (Goal 1)
#     fig_EXP2_region_tier.png            Region x tier heatmap (Goal 1)
#     fig_EXP3_pretrends_<outcome>.png    Outcome by tercile over time (Goal 2)
#     fig_EXP4_bite.png                   Non-compliance by tercile (Goal 2)
#     exposure_variation.rds              Numeric companion to EXP-1
#
# Reads:  exposure_geo   / exposure_cells  (07A, via mw_file, from Exposure dir)
#         panel_geo_quarter                (08,  via mw_file, from Regression dir)
#
#===============================================================================

if (!exists("config", envir = .GlobalEnv, inherits = FALSE)) {
  source(here::here("Code","R","clean scripts","00_setup.R"))
} else {
  cat("[08B] Reusing existing `config` (00_setup not re-sourced)\n")
}


cat("=== 08B_Exposure_Validation.R ===\n\n")


#===============================================================================
# STEP 0. Parameters and paths
#===============================================================================

BL <- config$baselines[[config$active_baseline]]
IS <- config$income_specs[[config$active_income]]

GEO      <- config$exposure$construct_geo
TIER_VAR <- "wage_group"

# Baseline label for subtitles/captions
BL_LABEL <- BL$label

# Baseline period as a set of year_quarter strings, for pretrend shading
BL_QTRS <- if (BL$period$type == "year") {
  paste0(BL$period$value, "Q", 1:4)
} else {
  BL$period$value
}

# Events / COVID from config (was hardcoded).
MW_EVENT_QTR <- config$events$event_qtrs
PHASE_IN     <- config$events$phase_in_qtrs
COVID_QTRS   <- config$events$covid_qtrs

# Panel time range for dynamic captions
PANEL_QTRS <- NULL   # filled after loading panel

SRC <- "Source: ENCFT (Banco Central de la República Dominicana)."
REG_NOTE <- paste(
  "Regression sample: private sector salaried employees, positive earnings,",
  "known firm size, excl. government, domestic workers, FTZ, electricity & water."
)
MW_NOTE <- paste(
  "Red dashed verticals: MW event quarters", paste(MW_EVENT_QTR, collapse = ", "),
  ". Grey shading: 2020Q1-2020Q4 (COVID-19).")

# Tercile palette
EXP_COLORS <- c(
  "Low exposure"    = "#4575b4",
  "Medium exposure" = "#bdbdbd",
  "High exposure"   = "#d73027"
)
EXP_LINE_COLORS <- c(
  "Low exposure"    = "#4575b4",   # blue
  "Medium exposure" = "#999999",   # grey
  "High exposure"   = "#1b7837"    # green (red reserved for event vlines)
)

# Directories (mirror the pattern used in 08 / 09B)
in_dir_exp <- config$data_dirs$exposure
in_dir_reg <- config$data_dirs$regression
out_dir <- file.path(config$paths$outputs, config$output_stage,
                     config$out_subdirs$exp_validation,
                     config$active_income, config$active_baseline)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat(sprintf("  income=%s | baseline=%s | geo=%s\n",
            config$active_income, config$active_baseline, GEO))
cat(sprintf("  reading exposure from: %s\n", in_dir_exp))
cat(sprintf("  reading panel    from: %s\n", in_dir_reg))
cat(sprintf("  writing to:            %s\n", out_dir))


#===============================================================================
# STEP 1. Load
#===============================================================================

exp_geo_file  <- mw_file("exposure_geo",       dir = in_dir_exp)
exp_cell_file <- mw_file("exposure_cells",     dir = in_dir_exp)
panel_file    <- mw_file("panel_geo_quarter",  dir = in_dir_reg)

for (f in c(exp_geo_file, exp_cell_file, panel_file)) {
  if (!file.exists(f)) {
    stop("Missing input: ", f,
         "\nRun 07A and 08 for the current (income, baseline) first.")
  }
}

exposure_geo   <- readRDS(exp_geo_file)
exposure_cells <- readRDS(exp_cell_file)
panel_gq       <- readRDS(panel_file)

PANEL_QTRS <- range(panel_gq$time)
SRC <- sprintf("Source: ENCFT %s-%s (Banco Central de la República Dominicana).",
               PANEL_QTRS[1], PANEL_QTRS[2])


#===============================================================================
# STEP 2. Save helper + shared plot decorations
#===============================================================================

save_fig <- function(p, name,
                     w = config$fig_defaults$width,
                     h = config$fig_defaults$height) {
  fp <- file.path(out_dir, paste0(name, ".", config$fig_defaults$format))
  ggsave(fp, plot = p, width = w, height = h, dpi = config$fig_defaults$dpi)
  message("Saved: ", fp)
}

# Character x-axis idioms (mirror 04B/05B).
event_pos  <- function(qtrs) which(sort(unique(qtrs)) %in% MW_EVENT_QTR)
covid_rect <- function(qtrs) {
  xmin <- which(sort(unique(qtrs)) == "2020Q1")
  xmax <- which(sort(unique(qtrs)) == "2020Q4")
  if (!length(xmin) || !length(xmax)) return(NULL)
  annotate("rect", xmin = xmin - 0.5, xmax = xmax + 0.5,
           ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.6)
}
# Baseline period shading (new: visual anchor for "the baseline")
baseline_rect <- function(qtrs) {
  bl_pos <- which(sort(unique(qtrs)) %in% BL_QTRS)
  if (!length(bl_pos)) return(NULL)
  annotate("rect", xmin = min(bl_pos) - 0.5, xmax = max(bl_pos) + 0.5,
           ymin = -Inf, ymax = Inf, fill = "#fee08b", alpha = 0.35)
}
qtr_breaks <- function(qtrs) { q <- sort(unique(qtrs)); q[seq(1, length(q), by = 4)] }


#===============================================================================
# FIGURE EXP-1: Region exposure, ranked, coloured by tercile
#===============================================================================

cat("[EXP-1] Region exposure bar chart...\n")

exp_bar <- exposure_geo %>%
  mutate(region = factor(.data[[GEO]],
                         levels = .data[[GEO]][order(exposure_geo_val)]))

fig_EXP1 <- ggplot(exp_bar,
                   aes(x = exposure_geo_val, y = region, fill = exposure_group)) +
  geom_col() +
  scale_fill_manual(values = EXP_COLORS, name = "Exposure group") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = sprintf("Minimum-wage exposure by region (%s)", BL_LABEL),
    subtitle = sprintf(
      "Share of formal workers near the MW band | income concept: %s | baseline: %s",
      IS$label, BL_LABEL),
    x = "Exposure (share near min wage)", y = NULL,
    caption = paste(REG_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_EXP1, "fig_EXP1_region_bar")


#===============================================================================
# FIGURE EXP-2: Region x tier exposure heatmap
#
# For single-tier baselines (e.g. base2021q2_micro), this collapses to a single
# column. Rendered for consistency with a subtitle noting the design.
#===============================================================================

cat("[EXP-2] Region x tier heatmap...\n")

region_order <- levels(exp_bar$region)

heat <- exposure_cells %>%
  mutate(region = factor(.data[[GEO]], levels = region_order))

n_tiers <- dplyr::n_distinct(heat[[TIER_VAR]])
tier_note <- if (n_tiers == 1) {
  sprintf(" (single-tier baseline: only %s shown)",
          unique(heat[[TIER_VAR]]))
} else {
  ""
}

fig_EXP2 <- ggplot(heat,
                   aes(x = .data[[TIER_VAR]], y = region, fill = exposure_val)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = percent(exposure_val, accuracy = 1)), size = 3) +
  scale_fill_gradient(low = "grey95", high = "grey20",
                      labels = percent_format(accuracy = 1), name = "Exposure") +
  labs(
    title    = sprintf("Minimum-wage exposure by region x firm size (%s)%s",
                       BL_LABEL, tier_note),
    subtitle = "Share of formal workers within the tier-specific MW band",
    x = "Firm size tier", y = NULL,
    caption = paste(REG_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_EXP2, "fig_EXP2_region_tier")


#===============================================================================
# FIGURE EXP-3: Pretrend eyeball  --  outcome trends by exposure tercile
#
# Column names picked from IS$log_var_prefix so the plot swaps to log_var_mwage_*
# under the monthly income run. Plotted UNCONDITIONALLY (no FE, no regression):
# if tercile groups move in parallel before events and diverge after, the design
# is supported. Underlying region points shown lightly so tercile-average noise
# is visible.
#===============================================================================

cat("[EXP-3] Pre-trends by exposure tercile...\n")

var_col <- paste0(IS$log_var_prefix, "_formal")
income_word <- if (IS$log_var_prefix == "log_var_hwage") "hourly" else "monthly"

OUTCOMES <- setNames(
  c(sprintf("Variance of log %s wage (formal)", income_word),
    "Non-compliance: share below MW (formal)",
    "Informal share (employed)",
    "Self-employment share (employed)"),
  c(var_col, "below_min_share", "informal_share", "selfemp_share")
)

# Sanity: every OUTCOMES column should be on the panel.
missing_cols <- setdiff(names(OUTCOMES), names(panel_gq))
if (length(missing_cols)) {
  warning("Panel is missing outcome column(s): ", paste(missing_cols, collapse = ", "),
          ". Those pretrend figures will be skipped.")
  OUTCOMES <- OUTCOMES[!names(OUTCOMES) %in% missing_cols]
}

panel_grp <- panel_gq %>%
  filter(!is.na(exposure_group)) %>%
  mutate(time = factor(time, levels = sort(unique(time))))
qtrs <- levels(panel_grp$time)

make_pretrend <- function(outcome, label) {
  grp <- panel_grp %>%
    filter(!is.na(.data[[outcome]])) %>%
    group_by(exposure_group, time) %>%
    summarise(y = mean(.data[[outcome]], na.rm = TRUE), .groups = "drop")
  
  ggplot() +
    baseline_rect(qtrs) +
    covid_rect(qtrs) +
    geom_vline(xintercept = event_pos(qtrs), linetype = "dashed",
               colour = "red", linewidth = 0.4) +
    geom_point(data = panel_grp %>% filter(!is.na(.data[[outcome]])),
               aes(x = time, y = .data[[outcome]], colour = exposure_group),
               alpha = 0.20, size = 0.7) +
    geom_line(data = grp,
              aes(x = time, y = y, colour = exposure_group,
                  group = exposure_group),
              linewidth = 0.8) +
    scale_colour_manual(values = EXP_LINE_COLORS, name = "Exposure group") +
    scale_x_discrete(breaks = qtr_breaks(qtrs)) +
    labs(title = label,
         subtitle = sprintf("Baseline: %s (yellow shading = baseline period)",
                            BL_LABEL),
         x = NULL, y = NULL,
         caption = paste(MW_NOTE, REG_NOTE, SRC, sep = "\n")) +
    theme_surveytools() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

for (nm in names(OUTCOMES)) {
  p <- make_pretrend(nm, OUTCOMES[[nm]])
  save_fig(p, paste0("fig_EXP3_pretrends_", nm))
}


#===============================================================================
# FIGURE EXP-4: Exposure predicts bite
#
# Non-compliance by tercile. If exposure genuinely proxies MW bite, more-exposed
# regions should show larger non-compliance movement around events. If not, the
# treatment variable isn't capturing what the design needs.
#===============================================================================

cat("[EXP-4] Exposure-predicts-bite...\n")

if ("below_min_share" %in% names(panel_gq)) {
  
  fig_EXP4 <- local({
    grp <- panel_grp %>%
      filter(!is.na(below_min_share)) %>%
      group_by(exposure_group, time) %>%
      summarise(y = mean(below_min_share, na.rm = TRUE), .groups = "drop")
    
    ggplot(grp, aes(x = time, y = y, colour = exposure_group,
                    group = exposure_group)) +
      baseline_rect(qtrs) +
      covid_rect(qtrs) +
      geom_vline(xintercept = event_pos(qtrs), linetype = "dashed",
                 colour = "red", linewidth = 0.4) +
      geom_line(linewidth = 0.8) +
      scale_colour_manual(values = EXP_LINE_COLORS, name = "Exposure group") +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_x_discrete(breaks = qtr_breaks(qtrs)) +
      labs(
        title    = "Does exposure predict minimum-wage bite?",
        subtitle = sprintf("Share of formal workers below the tier MW, by %s exposure group | %s income",
                           BL_LABEL, income_word),
        x = NULL, y = "Share below tier MW",
        caption = paste(MW_NOTE, REG_NOTE, SRC, sep = "\n")
      ) +
      theme_surveytools() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  save_fig(fig_EXP4, "fig_EXP4_bite")
  
} else {
  warning("`below_min_share` not on panel; skipping EXP-4.")
}


#===============================================================================
# STEP N. Exposure variation summary (numeric companion to EXP-1)
#===============================================================================

exp_var_tbl <- exposure_geo %>%
  summarise(
    n_regions = dplyr::n(),
    min       = min(exposure_geo_val),
    p25       = quantile(exposure_geo_val, .25),
    median    = median(exposure_geo_val),
    p75       = quantile(exposure_geo_val, .75),
    max       = max(exposure_geo_val),
    sd        = sd(exposure_geo_val),
    iqr       = IQR(exposure_geo_val),
    cv        = sd(exposure_geo_val) / mean(exposure_geo_val)
  )

saveRDS(exp_var_tbl, file.path(out_dir, "exposure_variation.rds"))

cat("\n[EXP] Exposure variation across regions:\n")
print(as.data.frame(exp_var_tbl), row.names = FALSE)

cat("\n=== 08B_Exposure_Validation.R complete ===\n")