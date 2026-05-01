#===============================================================================
#
# Script: 05b_MW_Bindingness_Figures.R
#
# Purpose: Produce all exploratory figures on MW evolution and bindingness.
#          Reads pre-computed RDS objects from 05a_MW_Bindingness_Compute.R.
#          No survey-weighted calculations here — pure ggplot2.
#
# FIGURE STRUCTURE
#
#   The compute script produces two scopes × three income concepts.
#   This script generates figures at THREE levels of aggregation:
#
#   LEVEL 1 — PRIMARY (formal workers, main paper figures)
#     Figure A1_{concept}   Economy-wide KDE, 2016Q2 vs 2025Q2
#     Figure A2_{concept}   Sector-level KDE, 2016Q2 vs 2025Q2
#     Figure B1_{concept}   Economy-wide log Kaitz time series
#     Figure B2_{concept}   Sector-level log Kaitz time series
#     fig_combined_{concept} A1 + B1 stacked (presentation slide)
#
#   LEVEL 2 — SCOPE COMPARISON (formal vs formal+informal, spillover check)
#     Figure D1_{concept}   Economy-wide KDE overlaying formal & all-private
#                           distributions in the same panel (2016Q2 + 2025Q2)
#     Figure D2_{concept}   Economy-wide Kaitz: formal vs all-private overlaid
#
#   LEVEL 3 — CROSS-CONCEPT COMPARISON (economy-wide, formal scope only)
#     Figure C1             Kaitz by concept, faceted — formal scope
#     Figure C2             Kaitz by concept, faceted — all-private scope
#
#   OUTPUT NAMING
#     figA1_dist_econ_{concept}
#     figA2_dist_sector_{concept}
#     figB1_kaitz_econ_{concept}
#     figB2_kaitz_sector_{concept}
#     fig_combined_{concept}
#     figD1_dist_scope_compare_{concept}
#     figD2_kaitz_scope_compare_{concept}
#     figC1_kaitz_concept_compare_formal
#     figC2_kaitz_concept_compare_allprivate
#
#===============================================================================

source("Code/R/setup/00_setup.R")
library(patchwork)
library(scales)


#===============================================================================
# STEP 0.  Output path and save helper
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
    filename = file.path(save_path,
                         paste0(name, ".", config$fig_defaults$format)),
    plot     = fig,
    width    = w,
    height   = h,
    dpi      = config$fig_defaults$dpi
  )
  message("Saved: ", name)
}


#===============================================================================
# STEP 1.  Load pre-computed objects
#===============================================================================

cat("[1] Loading pre-computed data...\n")

pd         <- config$paths$processed_data
dist_data  <- readRDS(file.path(pd, "mw_bindingness_dist_data.rds"))
kaitz_econ <- readRDS(file.path(pd, "mw_bindingness_kaitz_econ.rds"))
kaitz_sec  <- readRDS(file.path(pd, "mw_bindingness_kaitz_sector.rds"))

cat("    dist_data rows:    ", nrow(dist_data), "\n")
cat("    kaitz_econ rows:   ", nrow(kaitz_econ), "\n")
cat("    kaitz_sec rows:    ", nrow(kaitz_sec), "\n")
cat("    Scopes available:  ", paste(unique(dist_data$scope), collapse = ", "), "\n")
cat("    Concepts available:", paste(unique(dist_data$concept), collapse = ", "), "\n")


#===============================================================================
# STEP 2.  Shared aesthetics and helpers
#===============================================================================

TIER_LEVELS <- c("Micro", "Small", "Medium", "Large")
TIER_COLORS <- c(
  "Micro"  = "#2C8C3C",
  "Small"  = "#5B3A8E",
  "Medium" = "#E07B00",
  "Large"  = "#1B7BB4"
)

# Scope colours for overlaid scope-comparison figures (D1, D2)
SCOPE_COLORS <- c(
  "formal"      = "#2C5F8A",   # muted blue — formal (primary)
  "all_private" = "#C45C30"    # burnt orange — all private (secondary/robustness)
)
SCOPE_LTYPES <- c("formal" = "solid", "all_private" = "dashed")

MW_EVENT_QTR <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")

SRC      <- "Sources: ENCFT 2014Q3\u20132025Q2; Central Bank of Dominican Republic."
REG_NOTE <- "Base sample: private-sector employees, positive compliance income, known firm size, Govt & Elec/Water excluded."

SNAP_LABELS <- c("2016Q2" = "2016 Q2 (baseline)", "2025Q2" = "2025 Q2 (recent)")

SCOPE_LABELS <- c(
  "formal"      = "Formal private-sector employees (primary)",
  "all_private" = "All private-sector employees: formal + informal (spillover check)"
)

CONCEPT_SUBTITLES <- c(
  "monthly"    = "Monthly income vs monthly MW floor | Measure 1 \u2014 upper bound; part-timers may appear non-compliant",
  "hourly"     = "Hourly income vs hourly MW floor | Measure 2 \u2014 primary; removes part-time bias",
  "hourly_eff" = "OT-adjusted hourly income vs hourly MW floor | Measure 3 \u2014 robustness; hours > 44 inflated by legal premia"
)

CONCEPT_SHORT <- c(
  "monthly"    = "Monthly",
  "hourly"     = "Hourly",
  "hourly_eff" = "Hourly (OT-adjusted)"
)

# COVID rectangle helper
covid_rect <- function(all_qtrs) {
  xmin <- which(all_qtrs == "2020Q1")
  xmax <- which(all_qtrs == "2020Q4")
  if (length(xmin) == 0 || length(xmax) == 0) return(NULL)
  annotate("rect",
           xmin = xmin - 0.5, xmax = xmax + 0.5,
           ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.6)
}

event_pos <- function(all_qtrs) which(all_qtrs %in% MW_EVENT_QTR)

# X-axis for distribution charts
dist_x_scale <- scale_x_continuous(
  breaks = c(log2(0.25), log2(0.5), 0, 1, 2, 3),
  labels = c("\u00bc MW", "\u00bd MW", "= MW", "2\u00d7", "4\u00d7", "8\u00d7"),
  limits = c(log2(0.15), log2(10))
)

dist_x_scale_narrow <- scale_x_continuous(
  breaks = c(log2(0.25), log2(0.5), 0, 1, 2),
  labels = c("\u00bc", "\u00bd", "MW", "2\u00d7", "4\u00d7"),
  limits = c(log2(0.15), log2(8))
)

# Sector ordering by employment size (2016Q2, formal scope, monthly concept)
sector_order <- dist_data %>%
  filter(year_quarter == "2016Q2", concept == "monthly", scope == "formal") %>%
  count(Employment_Sector, wt = FACTOR_EXPANSION, sort = TRUE) %>%
  pull(Employment_Sector)

concepts <- c("monthly", "hourly", "hourly_eff")
scopes   <- c("formal", "all_private")


#===============================================================================
# LEVEL 1 — PRIMARY FIGURES (formal scope only)
#===============================================================================

cat("\n[A/B] Building primary figures (formal scope)...\n")

for (cpt in concepts) {
  
  cat("  Concept:", cpt, "\n")
  cpt_label    <- CONCEPT_SHORT[[cpt]]
  cpt_subtitle <- CONCEPT_SUBTITLES[[cpt]]
  
  # ── A1: Economy-wide KDE ────────────────────────────────────────────────────
  
  d_econ <- dist_data %>%
    filter(concept == cpt, scope == "formal", !sparse) %>%
    mutate(
      Wage_group = factor(Wage_group, levels = TIER_LEVELS),
      snapshot   = factor(year_quarter, levels = names(SNAP_LABELS),
                          labels = SNAP_LABELS)
    )
  
  bw_econ <- d_econ %>%
    group_by(snapshot, Wage_group) %>% filter(n() >= 20) %>%
    summarise(bw = bw.nrd0(log2_ratio), .groups = "drop") %>%
    pull(bw) %>% median(na.rm = TRUE)
  
  fig_A1 <- ggplot(d_econ,
                   aes(x = log2_ratio, weight = w_norm,
                       colour = Wage_group, fill = Wage_group)) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "grey80", alpha = 0.4) +
    geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.5) +
    geom_density(bw = bw_econ, adjust = 1, alpha = 0, linewidth = 0.7) +
    geom_text(
      data = data.frame(snapshot = factor(SNAP_LABELS[1], levels = SNAP_LABELS)),
      aes(x = 0.06, y = Inf, label = "MW\nfloor"),
      inherit.aes = FALSE,
      vjust = 1.3, hjust = 0, size = 2.6, colour = "grey30"
    ) +
    facet_wrap(~snapshot) +
    scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
    scale_fill_manual(values = TIER_COLORS, name = "Firm size tier") +
    dist_x_scale +
    labs(
      title    = paste0("Formal Worker Income Distribution vs MW \u2014 ", cpt_label),
      subtitle = cpt_subtitle,
      x        = "Income relative to tier MW  (log\u2082 scale: +1 = double the MW)",
      y        = "Density (integrates to 1 per tier; values >1 = tight concentration)",
      caption  = paste(
        "Formal private-sector employees only. Weights normalised within snapshot \u00d7 tier.",
        "Grey fill: non-compliance zone (income < MW floor).",
        REG_NOTE, SRC, sep = "\n"
      )
    ) +
    theme_surveytools() +
    theme(strip.text       = element_text(face = "bold", size = 11),
          strip.background = element_rect(fill = "grey93", colour = "grey70"),
          axis.text.x      = element_text(size = 8))
  
  save_fig(fig_A1, paste0("figA1_dist_econ_", cpt),
           w = config$fig_defaults$width * 2,
           h = config$fig_defaults$height * 1.4)
  
  
  # ── A2: Sector KDE ──────────────────────────────────────────────────────────
  
  d_sec <- dist_data %>%
    filter(concept == cpt, scope == "formal", !sparse) %>%
    mutate(
      Wage_group        = factor(Wage_group, levels = TIER_LEVELS),
      snapshot          = factor(year_quarter, levels = names(SNAP_LABELS),
                                 labels = SNAP_LABELS),
      Employment_Sector = factor(Employment_Sector, levels = sector_order)
    )
  
  # Keep only sectors with ≥2 tiers in both snapshots
  viable_sec <- d_sec %>%
    group_by(snapshot, Employment_Sector) %>%
    summarise(n_tiers = n_distinct(Wage_group), .groups = "drop") %>%
    group_by(Employment_Sector) %>%
    summarise(all_ok = all(n_tiers >= 2), .groups = "drop") %>%
    filter(all_ok) %>% pull(Employment_Sector)
  
  d_sec <- d_sec %>% filter(Employment_Sector %in% viable_sec)
  n_sec <- length(viable_sec)
  
  bw_sec <- d_sec %>%
    group_by(Employment_Sector, snapshot, Wage_group) %>%
    filter(n() >= 15) %>%
    summarise(bw = bw.nrd0(log2_ratio), .groups = "drop") %>%
    group_by(Employment_Sector) %>%
    summarise(bw = median(bw, na.rm = TRUE), .groups = "drop")
  
  d_sec <- d_sec %>% left_join(bw_sec, by = "Employment_Sector") %>% filter(!is.na(bw))
  
  fig_A2 <- ggplot(d_sec,
                   aes(x = log2_ratio, weight = w_norm_sec, colour = Wage_group)) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "grey80", alpha = 0.35) +
    geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.35) +
    geom_density(aes(bw = bw), adjust = 1, alpha = 0, linewidth = 0.55) +
    facet_grid(Employment_Sector ~ snapshot, scales = "free_y") +
    scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
    dist_x_scale_narrow +
    labs(
      title    = paste0("Formal Worker Income by Sector & Firm Size \u2014 ", cpt_label),
      subtitle = paste0(cpt_subtitle, "\nFormal private-sector employees. Free y-axis."),
      x        = "Income relative to tier MW  (log\u2082 scale)",
      y        = "Density",
      caption  = paste(
        "Sector\u00d7tier cells < 50 obs suppressed. Sectors with < 2 tiers omitted.",
        REG_NOTE, SRC, sep = "\n"
      )
    ) +
    theme_surveytools(legend_position = "bottom") +
    theme(strip.text.x     = element_text(face = "bold", size = 9),
          strip.text.y     = element_text(face = "bold", size = 7, angle = 0, hjust = 0),
          strip.background = element_rect(fill = "grey93", colour = "grey70"),
          axis.text.x      = element_text(size = 7),
          axis.text.y      = element_text(size = 6),
          panel.spacing.y  = unit(0.25, "lines"),
          panel.spacing.x  = unit(0.5, "lines"))
  
  save_fig(fig_A2, paste0("figA2_dist_sector_", cpt),
           w = config$fig_defaults$width  * 2.2,
           h = config$fig_defaults$height * n_sec * 0.6)
  
  
  # ── B1: Economy-wide Kaitz (formal) ─────────────────────────────────────────
  
  k_econ <- kaitz_econ %>%
    filter(concept == cpt, scope == "formal", !sparse) %>%
    mutate(Wage_group = factor(Wage_group, levels = TIER_LEVELS))
  
  all_qtrs_e <- sort(unique(k_econ$year_quarter))
  epos_e     <- event_pos(all_qtrs_e)
  
  fig_B1 <- ggplot(k_econ,
                   aes(x = year_quarter, y = log_kaitz,
                       colour = Wage_group, group = Wage_group)) +
    covid_rect(all_qtrs_e) +
    geom_vline(xintercept = epos_e, linetype = "dashed",
               colour = "red", linewidth = 0.4) +
    geom_hline(yintercept = 0, linetype = "dotted",
               colour = "grey50", linewidth = 0.35) +
    geom_line(linewidth = 0.75) +
    scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
    scale_x_discrete(breaks = all_qtrs_e[seq(1, length(all_qtrs_e), by = 4)]) +
    labs(
      title    = paste0("Log Kaitz Index by Firm Size Tier \u2014 ", cpt_label,
                        " | Formal workers"),
      subtitle = paste0(
        "log(real tier MW) \u2212 log(median formal ", tolower(cpt_label),
        " compliance income within tier)\n", cpt_subtitle
      ),
      y        = "Log Kaitz Index",
      x        = NULL,
      caption  = paste(
        "Kaitz > 0: MW exceeds median (binding). Formal private-sector employees only.",
        paste0("Red dashed: MW events (", paste(MW_EVENT_QTR, collapse = ", "), ")."),
        "Grey band: COVID-19 (2020Q1\u20132020Q4).",
        REG_NOTE, SRC, sep = "\n"
      )
    ) +
    theme_surveytools() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))
  
  save_fig(fig_B1, paste0("figB1_kaitz_econ_", cpt),
           w = config$fig_defaults$width * 1.7,
           h = config$fig_defaults$height)
  
  
  # ── B2: Sector Kaitz (formal) ───────────────────────────────────────────────
  
  k_sec <- kaitz_sec %>%
    filter(concept == cpt, scope == "formal", !sparse) %>%
    mutate(Wage_group        = factor(Wage_group, levels = TIER_LEVELS),
           Employment_Sector = factor(Employment_Sector, levels = sector_order))
  
  viable_sec_k <- k_sec %>%
    group_by(Employment_Sector) %>%
    summarise(n_tiers = n_distinct(Wage_group), .groups = "drop") %>%
    filter(n_tiers >= 2) %>% pull(Employment_Sector)
  
  k_sec <- k_sec %>% filter(Employment_Sector %in% viable_sec_k)
  n_sec_k    <- length(viable_sec_k)
  all_qtrs_s <- sort(unique(k_sec$year_quarter))
  epos_s     <- event_pos(all_qtrs_s)
  
  fig_B2 <- ggplot(k_sec,
                   aes(x = year_quarter, y = log_kaitz,
                       colour = Wage_group, group = Wage_group)) +
    covid_rect(all_qtrs_s) +
    geom_vline(xintercept = epos_s, linetype = "dashed",
               colour = "red", linewidth = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotted",
               colour = "grey50", linewidth = 0.3) +
    geom_line(linewidth = 0.6, na.rm = TRUE) +
    facet_wrap(~Employment_Sector, ncol = 2, scales = "free_y") +
    scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
    scale_x_discrete(breaks = all_qtrs_s[seq(1, length(all_qtrs_s), by = 8)]) +
    labs(
      title    = paste0("Log Kaitz by Sector & Firm Size \u2014 ", cpt_label,
                        " | Formal workers"),
      subtitle = paste0(
        "log(tier MW) \u2212 log(median formal compliance income, sector\u00d7tier). ",
        "Free y-axis. Formal private-sector employees."
      ),
      y = "Log Kaitz Index", x = NULL,
      caption  = paste(
        "Cells < 50 obs suppressed. Sectors with < 2 tiers omitted.",
        paste0("Red dashed: MW events (", paste(MW_EVENT_QTR, collapse = ", "), ")."),
        REG_NOTE, SRC, sep = "\n"
      )
    ) +
    theme_surveytools(legend_position = "bottom") +
    theme(axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
          strip.text       = element_text(face = "bold", size = 8),
          strip.background = element_rect(fill = "grey93", colour = "grey70"),
          panel.spacing    = unit(0.5, "lines"))
  
  save_fig(fig_B2, paste0("figB2_kaitz_sector_", cpt),
           w = config$fig_defaults$width  * 2,
           h = config$fig_defaults$height * ceiling(n_sec_k / 2) * 0.9)
  
  
  # ── Combined: A1 + B1 stacked (presentation slide) ──────────────────────────
  
  fig_comb <- fig_A1 / fig_B1 +
    plot_annotation(
      title    = paste0("MW Bindingness \u2014 ", cpt_label,
                        " | Formal private-sector employees"),
      subtitle = cpt_subtitle,
      theme    = theme(
        plot.title    = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9, colour = "grey30")
      )
    )
  
  save_fig(fig_comb, paste0("fig_combined_", cpt),
           w = config$fig_defaults$width  * 2,
           h = config$fig_defaults$height * 2.8)
  
  cat("    A1 + A2 + B1 + B2 + combined saved for", cpt, "\n")
}


#===============================================================================
# LEVEL 2 — SCOPE COMPARISON (formal vs formal+informal, spillover check)
#
#  D1: Distribution KDE overlaying formal (solid) and all-private (dashed)
#      in the same panel, coloured by scope rather than by tier, so the
#      two distributions are directly comparable.
#      Show all four tiers separately via facet_wrap on Wage_group.
#
#  D2: Kaitz time series overlaying formal (solid) and all-private (dashed)
#      for each tier, coloured by scope.
#
#  Reading guide for D2:
#    - If formal and all-private Kaitz lines track each other closely, the
#      informality margin is not large or the informal wage distribution
#      closely mirrors the formal one.
#    - If all-private Kaitz is substantially lower (less binding) than formal,
#      informal workers pull the median down — the floor is much less binding
#      for the private sector as a whole than for formal workers alone.
#    - A widening gap over time would indicate growing formal/informal wage
#      divergence — consistent with the Parente spillover mechanism.
#===============================================================================

cat("\n[D] Building scope-comparison figures...\n")

for (cpt in concepts) {
  
  cat("  Concept:", cpt, "\n")
  cpt_label <- CONCEPT_SHORT[[cpt]]
  
  # ── D1: Distribution comparison by scope ────────────────────────────────────
  
  d_scope <- dist_data %>%
    filter(concept == cpt, !sparse) %>%
    mutate(
      Wage_group = factor(Wage_group, levels = TIER_LEVELS),
      snapshot   = factor(year_quarter, levels = names(SNAP_LABELS),
                          labels = SNAP_LABELS),
      scope      = factor(scope, levels = c("formal", "all_private"),
                          labels = c("Formal only", "Formal + Informal"))
    )
  
  # Common bandwidth across scopes for fair comparison
  bw_d <- d_scope %>%
    group_by(snapshot, Wage_group, scope) %>% filter(n() >= 20) %>%
    summarise(bw = bw.nrd0(log2_ratio), .groups = "drop") %>%
    pull(bw) %>% median(na.rm = TRUE)
  
  fig_D1 <- ggplot(d_scope,
                   aes(x = log2_ratio, weight = w_norm,
                       colour = scope, linetype = scope)) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "grey80", alpha = 0.35) +
    geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
    geom_density(bw = bw_d, adjust = 1, alpha = 0, linewidth = 0.7) +
    facet_grid(Wage_group ~ snapshot, scales = "free_y") +
    scale_colour_manual(
      values = c("Formal only" = SCOPE_COLORS[["formal"]],
                 "Formal + Informal" = SCOPE_COLORS[["all_private"]]),
      name = "Worker scope"
    ) +
    scale_linetype_manual(
      values = c("Formal only" = "solid", "Formal + Informal" = "dashed"),
      name = "Worker scope"
    ) +
    dist_x_scale +
    labs(
      title    = paste0("Income Distribution: Formal vs All Private Workers \u2014 ", cpt_label),
      subtitle = paste0(
        "Overlaid KDEs. Solid = formal only (primary). Dashed = formal + informal (spillover check).\n",
        CONCEPT_SUBTITLES[[cpt]]
      ),
      x        = "Income relative to tier MW  (log\u2082 scale)",
      y        = "Density (normalised within scope\u00d7snapshot\u00d7tier)",
      caption  = paste(
        "If informal distribution bunches near x = 0, this is evidence of MW spillovers to the informal sector.",
        "Free y-axis across tier rows.",
        REG_NOTE, SRC, sep = "\n"
      )
    ) +
    theme_surveytools(legend_position = "bottom") +
    theme(strip.text.x     = element_text(face = "bold", size = 9),
          strip.text.y     = element_text(face = "bold", size = 9),
          strip.background = element_rect(fill = "grey93", colour = "grey70"),
          axis.text.x      = element_text(size = 7),
          panel.spacing    = unit(0.4, "lines"))
  
  save_fig(fig_D1, paste0("figD1_dist_scope_compare_", cpt),
           w = config$fig_defaults$width  * 2,
           h = config$fig_defaults$height * 2.2)
  
  
  # ── D2: Kaitz comparison by scope ───────────────────────────────────────────
  
  k_scope <- kaitz_econ %>%
    filter(concept == cpt, !sparse) %>%
    mutate(
      Wage_group = factor(Wage_group, levels = TIER_LEVELS),
      scope      = factor(scope, levels = c("formal", "all_private"),
                          labels = c("Formal only", "Formal + Informal"))
    )
  
  all_qtrs_d <- sort(unique(k_scope$year_quarter))
  epos_d     <- event_pos(all_qtrs_d)
  
  fig_D2 <- ggplot(k_scope,
                   aes(x = year_quarter, y = log_kaitz,
                       colour = scope, linetype = scope,
                       group = interaction(Wage_group, scope))) +
    covid_rect(all_qtrs_d) +
    geom_vline(xintercept = epos_d, linetype = "dashed",
               colour = "red", linewidth = 0.35, alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dotted",
               colour = "grey50", linewidth = 0.3) +
    geom_line(linewidth = 0.65) +
    facet_wrap(~Wage_group, ncol = 2, scales = "free_y") +
    scale_colour_manual(
      values = c("Formal only" = SCOPE_COLORS[["formal"]],
                 "Formal + Informal" = SCOPE_COLORS[["all_private"]]),
      name = "Worker scope"
    ) +
    scale_linetype_manual(
      values = c("Formal only" = "solid", "Formal + Informal" = "dashed"),
      name = "Worker scope"
    ) +
    scale_x_discrete(breaks = all_qtrs_d[seq(1, length(all_qtrs_d), by = 4)]) +
    labs(
      title    = paste0("Kaitz Index: Formal vs All Private Workers \u2014 ", cpt_label),
      subtitle = paste0(
        "Solid = formal only (standard). Dashed = formal + informal (robustness).\n",
        "A lower all-private Kaitz means informality pulls the median below the formal median,\n",
        "making the floor appear less binding when evaluated on the full private sector."
      ),
      y        = "Log Kaitz Index",
      x        = NULL,
      caption  = paste(
        paste0("Red dashed: MW events (", paste(MW_EVENT_QTR, collapse = ", "), ")."),
        "Grey band: COVID-19. Free y-axis across tier panels.",
        REG_NOTE, SRC, sep = "\n"
      )
    ) +
    theme_surveytools(legend_position = "bottom") +
    theme(axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
          strip.text       = element_text(face = "bold", size = 9),
          strip.background = element_rect(fill = "grey93", colour = "grey70"),
          panel.spacing    = unit(0.5, "lines"))
  
  save_fig(fig_D2, paste0("figD2_kaitz_scope_compare_", cpt),
           w = config$fig_defaults$width  * 2,
           h = config$fig_defaults$height * 1.6)
  
  cat("    D1 + D2 saved for", cpt, "\n")
}


#===============================================================================
# LEVEL 3 — CROSS-CONCEPT COMPARISON (one chart per scope)
#===============================================================================

cat("\n[C] Building cross-concept comparison figures...\n")

CONCEPT_ORDER <- c(
  "Monthly (Measure 1)",
  "Hourly (Measure 2 \u2014 Primary)",
  "Hourly OT-adjusted (Measure 3)"
)

for (scp in scopes) {
  
  k_all <- kaitz_econ %>%
    filter(scope == scp, !sparse) %>%
    mutate(
      Wage_group    = factor(Wage_group, levels = TIER_LEVELS),
      concept_label = factor(concept_label, levels = CONCEPT_ORDER)
    )
  
  all_qtrs_c <- sort(unique(k_all$year_quarter))
  epos_c     <- event_pos(all_qtrs_c)
  scope_lbl  <- SCOPE_LABELS[[scp]]
  
  fig_C <- ggplot(k_all,
                  aes(x = year_quarter, y = log_kaitz,
                      colour = Wage_group, group = Wage_group)) +
    covid_rect(all_qtrs_c) +
    geom_vline(xintercept = epos_c, linetype = "dashed",
               colour = "red", linewidth = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotted",
               colour = "grey50", linewidth = 0.3) +
    geom_line(linewidth = 0.65) +
    facet_wrap(~concept_label, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
    scale_x_discrete(breaks = all_qtrs_c[seq(1, length(all_qtrs_c), by = 4)]) +
    labs(
      title    = paste0("Log Kaitz \u2014 Concept Comparison | ",
                        ifelse(scp == "formal", "Formal workers", "All private workers")),
      subtitle = paste0(
        scope_lbl, "\n",
        "Each panel uses a different income/MW definition. Free y-axis."
      ),
      y        = "Log Kaitz Index",
      x        = NULL,
      caption  = paste(
        paste0("Red dashed: MW events (", paste(MW_EVENT_QTR, collapse = ", "), ")."),
        "Grey band: COVID-19.",
        REG_NOTE, SRC, sep = "\n"
      )
    ) +
    theme_surveytools(legend_position = "bottom") +
    theme(axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
          strip.text       = element_text(face = "bold", size = 9),
          strip.background = element_rect(fill = "grey93", colour = "grey70"),
          panel.spacing    = unit(0.5, "lines"))
  
  fig_name <- paste0("figC_kaitz_concept_compare_", scp)
  save_fig(fig_C, fig_name,
           w = config$fig_defaults$width  * 1.7,
           h = config$fig_defaults$height * 3)
  
  cat("    Saved:", fig_name, "\n")
}


#===============================================================================
# DONE
#===============================================================================

cat("\n=== 05b_MW_Bindingness_Figures.R complete ===\n")
cat("Figures saved to:\n  ", save_path, "\n\n")

cat("PRIMARY (formal scope):\n")
for (cpt in concepts) {
  for (fig in c("figA1_dist_econ", "figA2_dist_sector",
                "figB1_kaitz_econ", "figB2_kaitz_sector", "fig_combined")) {
    cat(sprintf("  %s_%s\n", fig, cpt))
  }
}
cat("SCOPE COMPARISON (formal vs formal+informal):\n")
for (cpt in concepts) {
  cat(sprintf("  figD1_dist_scope_compare_%s\n", cpt))
  cat(sprintf("  figD2_kaitz_scope_compare_%s\n", cpt))
}
cat("CROSS-CONCEPT:\n")
for (scp in scopes) cat(sprintf("  figC_kaitz_concept_compare_%s\n", scp))
