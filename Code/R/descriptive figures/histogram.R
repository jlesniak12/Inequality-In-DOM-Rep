#===============================================================================
#
# Script: 05d_MW_Distribution_Histogram.R
#
# Purpose: Rebuild figA1 (economy-wide formal worker income distribution vs MW)
#          as a faceted histogram instead of overlaid KDE lines.
#
# DESIGN
#   Layout:    4 rows (one per firm size tier) × 2 columns (2016Q2 | 2025Q2)
#   X-axis:    log2(income / own-tier MW) — same as figA1 KDE version.
#              x = 0 is the MW floor; x < 0 is the non-compliance zone.
#   Y-axis:    Proportion of workers within that tier-snapshot cell.
#              Bars sum to 1 within each facet, so tiers of very different
#              sizes are directly comparable in shape.
#   Colour:    Bars left of x=0 coloured red (non-compliant).
#              Bars right of x=0 coloured by tier (same palette as KDE version).
#   MW line:   Vertical line at x=0 with label.
#
# WHY HISTOGRAM OVER KDE HERE
#   - Non-compliance share is directly readable as the sum of bar heights
#     to the left of x=0 — no integration required by the reader.
#   - Bunching at the floor (a spike in the bar at x=0) is more visible
#     in a histogram than in a KDE where bandwidth smoothing washes it out.
#   - Each tier gets its own facet so there is no overlay clutter.
#   - Proportions within facet make tiers comparable despite size differences.
#
# READS:   mw_bindingness_dist_data.rds  (from 05a_MW_Bindingness_Compute.R)
# SAVES:   figA1_dist_hist_{concept}.png  for each income concept
#
#===============================================================================

source("Code/R/setup/00_setup.R")
source("Code/R/import and prep data/02_Sample Definitions.R")
library(scales)

cat("=== 05d_MW_Distribution_Histogram.R ===\n")


#===============================================================================
# STEP 0.  Paths and helpers
#===============================================================================

save_path <- file.path(
  config$paths$outputs,
  config$output_stage,
  config$out_subdirs$inequality_minwage,
  "Bindingness Exploratory"
)
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)

save_fig <- function(fig, name,
                     w = config$fig_defaults$width,
                     h = config$fig_defaults$height) {
  ggsave(
    file.path(save_path, paste0(name, ".", config$fig_defaults$format)),
    plot = fig, width = w, height = h, dpi = config$fig_defaults$dpi
  )
  message("Saved: ", name)
}

SRC        <- "Sources: ENCFT 2014Q3\u20132025Q2; Central Bank of Dominican Republic."
TIER_LEVELS <- c("Micro", "Small", "Medium", "Large")

# Tier fill colours for compliant bars (non-compliant always red)
TIER_FILL <- c(
  Micro  = "#2C8C3C",
  Small  = "#5B3A8E",
  Medium = "#E07B00",
  Large  = "#1B7BB4"
)
NC_FILL <- "#C45C30"   # colour for non-compliant bars (x < 0)

SNAP_LABELS <- c("2016Q2" = "2016 Q2 (baseline)", "2025Q2" = "2025 Q2 (recent)")

CONCEPT_SHORT <- c(
  "monthly"    = "Monthly (Measure 1)",
  "hourly"     = "Hourly (Measure 2 \u2014 Primary)",
  "hourly_eff" = "Hourly OT-adjusted (Measure 3)"
)

CONCEPT_SUBTITLES <- c(
  "monthly"    = "Monthly income vs monthly MW floor. Measure 1: upper bound; part-timers may appear non-compliant.",
  "hourly"     = "Hourly income vs hourly MW floor. Measure 2: primary; removes part-time bias.",
  "hourly_eff" = "OT-adjusted hourly income vs hourly MW floor. Measure 3: robustness."
)


#===============================================================================
# STEP 1.  Load pre-computed snapshot data
#===============================================================================

cat("[1] Loading snapshot data...\n")

pd        <- config$paths$processed_data
dist_data <- readRDS(file.path(pd, "mw_bindingness_dist_data.rds"))

# Formal scope only (primary analysis sample)
dist_formal <- dist_data %>%
  filter(scope == "formal") %>%
  mutate(
    Wage_group = factor(Wage_group, levels = TIER_LEVELS),
    snapshot   = factor(year_quarter,
                        levels = names(SNAP_LABELS),
                        labels = SNAP_LABELS)
  )

cat("    Rows (formal scope):", nrow(dist_formal), "\n")
cat("    Concepts:", paste(unique(dist_formal$concept), collapse = ", "), "\n")
cat("    Snapshots:", paste(unique(dist_formal$year_quarter), collapse = ", "), "\n")


#===============================================================================
# STEP 2.  Build histograms — one figure per income concept
#
#  The x-axis uses log2_ratio (already in dist_data).
#  We bin on that scale with fixed bin width = 0.25 log2 units.
#    x = 0    → exactly at MW floor
#    x = 1    → twice the MW
#    x = -1   → half the MW
#  Bin width 0.25 means each bin covers a 19% income range, which is
#  narrow enough to show bunching at the floor clearly.
#
#  Bars are coloured by compliance: red for x < 0, tier colour for x >= 0.
#  Weights are w_norm (normalised within snapshot × tier) so bars sum to 1.
#
#  Non-compliance share annotation: computed as sum of weighted proportions
#  in bars to the left of x=0, shown as text in the top-left of each facet.
#===============================================================================

BIN_WIDTH <- 0.25          # log2 units per bar
X_LIMITS  <- c(-3, 4)      # ¼× MW to 16× MW
X_BREAKS  <- c(log2(0.25), log2(0.5), 0, 1, 2, 3)
X_LABELS  <- c("\u00bc MW", "\u00bd MW", "= MW", "2\u00d7", "4\u00d7", "8\u00d7")

concepts  <- c("monthly", "hourly", "hourly_eff")

for (cpt in concepts) {
  
  cat("  Building histogram for concept:", cpt, "\n")
  
  d <- dist_formal %>%
    filter(concept == cpt, !is.na(log2_ratio), is.finite(log2_ratio)) %>%
    # Drop cells with too few obs to be reliable
    filter(!sparse)
  
  # ── Compute weighted histogram proportions manually ─────────────────────
  # geom_histogram with weights gives counts × weight, not proportions.
  # We need proportions within each facet (snapshot × Wage_group).
  # Approach: bin the data, sum weights per bin, divide by total weight.
  
  bin_breaks <- seq(X_LIMITS[1], X_LIMITS[2], by = BIN_WIDTH)
  
  hist_df <- d %>%
    mutate(
      bin_mid = BIN_WIDTH * floor(log2_ratio / BIN_WIDTH) + BIN_WIDTH / 2,
      # Clip to plot range
      bin_mid = dplyr::if_else(bin_mid < X_LIMITS[1] | bin_mid > X_LIMITS[2],
                               NA_real_, bin_mid),
      compliant = bin_mid >= 0
    ) %>%
    filter(!is.na(bin_mid)) %>%
    group_by(snapshot, Wage_group, bin_mid, compliant) %>%
    summarise(wt = sum(w_norm, na.rm = TRUE), .groups = "drop") %>%
    # Normalise within snapshot × Wage_group so bars sum to 1
    group_by(snapshot, Wage_group) %>%
    mutate(prop = wt / sum(wt)) %>%
    ungroup() %>%
    mutate(
      Wage_group = factor(Wage_group, levels = TIER_LEVELS),
      # Fill colour: tier colour if compliant, NC colour if not
      fill_colour = dplyr::if_else(compliant,
                                   as.character(TIER_FILL[as.character(Wage_group)]),
                                   NC_FILL)
    )
  
  # ── Non-compliance rate annotation per facet ─────────────────────────────
  nc_ann <- hist_df %>%
    group_by(snapshot, Wage_group) %>%
    summarise(nc_rate = sum(prop[!compliant], na.rm = TRUE), .groups = "drop") %>%
    mutate(label = paste0("NC: ", round(nc_rate * 100, 1), "%"))
  
  # ── Plot ─────────────────────────────────────────────────────────────────
  fig <- ggplot(hist_df,
                aes(x = bin_mid, y = prop, fill = fill_colour)) +
    # Non-compliance zone shading
    annotate("rect",
             xmin = X_LIMITS[1], xmax = 0,
             ymin = -Inf, ymax = Inf,
             fill = "#C45C30", alpha = 0.06) +
    # MW floor line
    geom_vline(xintercept = 0, colour = "grey20",
               linewidth = 0.6, linetype = "solid") +
    # Histogram bars
    geom_col(width = BIN_WIDTH * 0.9, colour = "white", linewidth = 0.1) +
    # Non-compliance rate annotation
    geom_text(data    = nc_ann,
              aes(x = X_LIMITS[1] + 0.1, y = Inf,
                  label = label),
              inherit.aes = FALSE,
              hjust = 0, vjust = 1.4,
              size = 2.6, colour = "#C45C30", fontface = "bold") +
    # MW floor label on first tier only
    geom_text(
      data = dplyr::filter(nc_ann,
                           Wage_group == TIER_LEVELS[1],
                           snapshot   == SNAP_LABELS[1]),
      aes(x = 0.05, y = Inf, label = "MW\nfloor"),
      inherit.aes = FALSE,
      hjust = 0, vjust = 1.3, size = 2.4, colour = "grey30"
    ) +
    # Facet: tier (rows) × snapshot (columns)
    facet_grid(Wage_group ~ snapshot, scales = "free_y") +
    # Use actual fill values computed per bar
    scale_fill_identity() +
    scale_x_continuous(breaks = X_BREAKS, labels = X_LABELS,
                       limits = X_LIMITS) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title    = paste0("Formal Worker Income Distribution vs MW \u2014 ",
                        CONCEPT_SHORT[[cpt]]),
      subtitle = paste0(
        CONCEPT_SUBTITLES[[cpt]], "\n",
        "Bars show proportion of workers within each tier-snapshot cell. ",
        "Red = non-compliance zone (x < 0). NC% = share of workers below MW floor."
      ),
      x = "Income relative to tier MW  (log\u2082 scale: +1 = double the MW)",
      y = "Proportion of workers",
      caption = paste(
        "Formal private-sector employees. Weights normalised within snapshot \u00d7 tier.",
        "Bin width = 0.25 log\u2082 units (\u224819% income range per bar).",
        "Government and Electricity & Water excluded.",
        "Income concept: real salary + commissions (wage_compliance_primary).",
        SRC, sep = "\n"
      )
    ) +
    theme_surveytools() +
    theme(
      strip.text.x     = element_text(face = "bold", size = 10),
      strip.text.y     = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "grey93", colour = "grey70"),
      axis.text.x      = element_text(size = 8),
      panel.spacing.x  = unit(0.6, "lines"),
      panel.spacing.y  = unit(0.4, "lines")
    )
  
  save_fig(fig,
           paste0("figA1_dist_hist_", cpt),
           w = config$fig_defaults$width  * 2,
           h = config$fig_defaults$height * 2.4)
  
  cat("    Saved: figA1_dist_hist_", cpt, "\n", sep = "")
}


#===============================================================================
# FIGURE D — Histogram scope comparison: formal vs formal+informal
#
# Replicates figD1 (KDE overlay) as a histogram.
#
# DESIGN
#   Layout:    4 rows (one per tier) × 2 columns (snapshot)
#   Within each panel: TWO sets of bars, DODGED side by side.
#     - Tier colour bars     = formal workers only (primary sample)
#     - Muted/light bars     = all private workers (formal + informal)
#   Non-compliance zone shaded red as before.
#   NC% annotated for each scope separately.
#
# PURPOSE
#   The KDE version overlays two lines per panel, which requires the reader
#   to mentally compare curve heights. Dodged bars make the comparison
#   direct: for each income bin you can see whether formal or all-private
#   workers are more concentrated there.
#
#   Key reading: bins to the LEFT of the floor (x<0) show how non-compliance
#   differs across the two scopes. If the all-private bars are taller in the
#   non-compliance zone, informal workers are pulling the aggregate distribution
#   further below the floor — direct evidence of spillover effects on the
#   income distribution.
#
# One figure per income concept, same three concepts as figA1.
#===============================================================================

cat("\n[3] Building figD histogram (scope comparison)...\n")

# Muted versions of tier colours for the all-private bars
# (lighter/more transparent to distinguish from the formal solid colour)
TIER_FILL_MUTED <- c(
  Micro  = "#8FC89A",   # lighter green
  Small  = "#A98EC4",   # lighter purple
  Medium = "#F0BE7A",   # lighter orange
  Large  = "#7AB3D4"    # lighter blue
)
NC_FILL_MUTED <- "#E8A898"   # lighter red for all-private NC bars

SCOPE_LABELS_SHORT <- c(
  "formal"      = "Formal only",
  "all_private" = "Formal + Informal"
)

for (cpt in concepts) {
  
  cat("  Building figD histogram for concept:", cpt, "\n")
  
  # Pull both scopes from dist_data
  d_both <- dist_data %>%
    filter(concept == cpt, !is.na(log2_ratio), is.finite(log2_ratio),
           !sparse) %>%
    mutate(
      Wage_group  = factor(Wage_group, levels = TIER_LEVELS),
      snapshot    = factor(year_quarter,
                           levels = names(SNAP_LABELS),
                           labels = SNAP_LABELS),
      scope_label = factor(SCOPE_LABELS_SHORT[scope],
                           levels = c("Formal only", "Formal + Informal"))
    )
  
  # Compute weighted histogram proportions per scope
  # Normalise within snapshot × Wage_group × scope so each scope sums to 1
  hist_d <- d_both %>%
    mutate(
      bin_mid = BIN_WIDTH * floor(log2_ratio / BIN_WIDTH) + BIN_WIDTH / 2,
      bin_mid = dplyr::if_else(bin_mid < X_LIMITS[1] | bin_mid > X_LIMITS[2],
                               NA_real_, bin_mid),
      compliant = bin_mid >= 0
    ) %>%
    filter(!is.na(bin_mid)) %>%
    # Use w_norm (economy-level normalisation within snapshot × tier × scope)
    group_by(snapshot, Wage_group, scope, scope_label, bin_mid, compliant) %>%
    summarise(wt = sum(w_norm, na.rm = TRUE), .groups = "drop") %>%
    group_by(snapshot, Wage_group, scope) %>%
    mutate(prop = wt / sum(wt)) %>%
    ungroup() %>%
    mutate(
      Wage_group = factor(Wage_group, levels = TIER_LEVELS),
      # Fill colour: solid tier colour for formal, muted for all-private
      fill_colour = dplyr::case_when(
        scope == "formal"      &  compliant ~ as.character(TIER_FILL[as.character(Wage_group)]),
        scope == "formal"      & !compliant ~ NC_FILL,
        scope == "all_private" &  compliant ~ as.character(TIER_FILL_MUTED[as.character(Wage_group)]),
        scope == "all_private" & !compliant ~ NC_FILL_MUTED
      )
    )
  
  # NC annotations per scope × tier × snapshot
  nc_d <- hist_d %>%
    group_by(snapshot, Wage_group, scope, scope_label) %>%
    summarise(nc_rate = sum(prop[!compliant], na.rm = TRUE), .groups = "drop") %>%
    mutate(
      label  = paste0(scope_label, " NC: ", round(nc_rate * 100, 1), "%"),
      y_pos  = dplyr::if_else(scope == "formal", Inf, Inf),
      vjust  = dplyr::if_else(scope == "formal", 1.4, 2.8),
      colour = dplyr::if_else(scope == "formal", "#C45C30", NC_FILL_MUTED)
    )
  
  fig_D <- ggplot(hist_d,
                  aes(x    = bin_mid,
                      y    = prop,
                      fill = fill_colour,
                      group = scope_label)) +
    
    # Non-compliance zone shading
    annotate("rect",
             xmin = X_LIMITS[1], xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "#C45C30", alpha = 0.06) +
    
    # MW floor line
    geom_vline(xintercept = 0, colour = "grey20",
               linewidth = 0.6, linetype = "solid") +
    
    # Dodged histogram bars
    geom_col(
      position = position_dodge(width = BIN_WIDTH * 0.85),
      width    = BIN_WIDTH * 0.42,   # half of dodge width so bars fit neatly
      colour   = "white",
      linewidth = 0.08
    ) +
    
    # NC annotations — formal (top line)
    geom_text(
      data = dplyr::filter(nc_d, scope == "formal"),
      aes(x = X_LIMITS[1] + 0.1, y = Inf,
          label = label, colour = colour),
      inherit.aes = FALSE,
      hjust = 0, vjust = 1.4,
      size = 2.4, fontface = "bold"
    ) +
    
    # NC annotations — all-private (second line)
    geom_text(
      data = dplyr::filter(nc_d, scope == "all_private"),
      aes(x = X_LIMITS[1] + 0.1, y = Inf,
          label = label, colour = colour),
      inherit.aes = FALSE,
      hjust = 0, vjust = 2.9,
      size = 2.4, fontface = "bold"
    ) +
    
    facet_grid(Wage_group ~ snapshot, scales = "free_y") +
    
    scale_fill_identity() +
    scale_colour_identity() +
    scale_x_continuous(breaks = X_BREAKS, labels = X_LABELS,
                       limits = X_LIMITS) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    
    # Manual legend using annotate (since fill_identity suppresses auto legend)
    annotate("rect", xmin = 2.5, xmax = 2.8, ymin = -Inf, ymax = -Inf,
             fill = NA) +  # placeholder — legend built via caption text
    
    labs(
      title    = paste0("Income Distribution vs MW: Formal vs All Private \u2014 ",
                        CONCEPT_SHORT[[cpt]]),
      subtitle = paste0(
        CONCEPT_SUBTITLES[[cpt]], "\n",
        "Solid bars = formal workers only.  Light bars = formal + informal workers.\n",
        "If light bars taller in non-compliance zone: informal workers below the floor (spillover evidence)."
      ),
      x = "Income relative to tier MW  (log\u2082 scale: +1 = double the MW)",
      y = "Proportion of workers (within scope)",
      caption = paste(
        "Solid bars: formal private-sector employees (primary sample).",
        "Light bars: all private-sector employees (formal + informal).",
        "Bars normalised within each scope \u00d7 snapshot \u00d7 tier so each sums to 100%.",
        "Government and Electricity & Water excluded.",
        SRC, sep = "\n"
      )
    ) +
    theme_surveytools() +
    theme(
      strip.text.x     = element_text(face = "bold", size = 10),
      strip.text.y     = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "grey93", colour = "grey70"),
      axis.text.x      = element_text(size = 8),
      panel.spacing.x  = unit(0.6, "lines"),
      panel.spacing.y  = unit(0.4, "lines")
    )
  
  save_fig(fig_D,
           paste0("figD1_dist_hist_scope_", cpt),
           w = config$fig_defaults$width  * 2,
           h = config$fig_defaults$height * 2.4)
  
  cat("    Saved: figD1_dist_hist_scope_", cpt, "\n", sep = "")
}


#===============================================================================
# DONE
#===============================================================================

cat("\n=== 05d complete ===\n")
cat("Saved to:", save_path, "\n")
cat("figA1 (formal, faceted by tier):\n")
cat("  figA1_dist_hist_monthly\n")
cat("  figA1_dist_hist_hourly\n")
cat("  figA1_dist_hist_hourly_eff\n")
cat("figD1 (formal vs formal+informal, faceted by tier):\n")
cat("  figD1_dist_hist_scope_monthly\n")
cat("  figD1_dist_hist_scope_hourly\n")
cat("  figD1_dist_hist_scope_hourly_eff\n")