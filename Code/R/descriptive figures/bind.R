#===============================================================================
#
# Script: 05b_MW_Bindingness_Figures.R
#
# Purpose: All exploratory figures on MW bindingness and compliance.
#          Pure ggplot2 — reads pre-computed RDS from 05a.
#
# FIGURE INVENTORY
#
#   SECTION A — Wage Distribution vs MW (snapshot: 2016Q2 vs 2025Q2)
#     figA1_dist_econ_{concept}          Economy-wide KDE by firm size tier
#     figA2_dist_sector_{concept}        Sector-level KDE by firm size tier
#
#   SECTION B — Log Kaitz Index (full quarterly time series)
#     figB1_kaitz_econ_{concept}         Economy-wide by tier
#     figB2_kaitz_sector_{concept}       By sector, faceted
#     fig_combined_{concept}             A1 + B1 stacked (slide)
#
#   SECTION C — Cross-concept Kaitz comparison
#     figC_kaitz_concept_compare_{scope} All three concepts in one chart
#
#   SECTION D — Scope comparison (formal vs formal+informal)
#     figD1_dist_scope_compare_{concept} KDE overlaid by scope
#     figD2_kaitz_scope_compare_{concept} Kaitz overlaid by scope
#
#   SECTION E — Compliance time series (NEW)
#     figE1_compliance_econ              Economy-wide NC rate, all 3 concepts,
#                                        formal scope. One panel per concept,
#                                        stacked. Formal vs all-private overlaid.
#     figE2_compliance_tier              NC rate by firm size tier, one panel
#                                        per tier. Three lines = three concepts.
#                                        Formal scope primary.
#     figE3_compliance_sector            NC rate by sector (faceted), formal scope,
#                                        one line per concept.
#     figE4_compliance_concept_compare   All three concepts + both scopes in
#                                        a single comparison chart (economy-wide).
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
    plot = fig, width = w, height = h, dpi = config$fig_defaults$dpi
  )
  message("Saved: ", name)
}


#===============================================================================
# STEP 1.  Load pre-computed objects
#===============================================================================

cat("[1] Loading pre-computed data...\n")

pd           <- config$paths$processed_data
dist_data    <- readRDS(file.path(pd, "mw_bindingness_dist_data.rds"))
kaitz_econ   <- readRDS(file.path(pd, "mw_bindingness_kaitz_econ.rds"))
kaitz_sec    <- readRDS(file.path(pd, "mw_bindingness_kaitz_sector.rds"))
comp_econ    <- readRDS(file.path(pd, "mw_bindingness_compliance_econ.rds"))
comp_tier    <- readRDS(file.path(pd, "mw_bindingness_compliance_tier.rds"))
comp_sec     <- readRDS(file.path(pd, "mw_bindingness_compliance_sector.rds"))

cat("    dist_data rows:    ", nrow(dist_data), "\n")
cat("    kaitz_econ rows:   ", nrow(kaitz_econ), "\n")
cat("    comp_econ rows:    ", nrow(comp_econ), "\n")
cat("    comp_tier rows:    ", nrow(comp_tier), "\n")
cat("    comp_sec rows:     ", nrow(comp_sec), "\n")


#===============================================================================
# STEP 2.  Shared aesthetics
#===============================================================================

TIER_LEVELS <- c("Micro", "Small", "Medium", "Large")
TIER_COLORS <- c(Micro = "#2C8C3C", Small = "#5B3A8E",
                 Medium = "#E07B00", Large = "#1B7BB4")

# Three concepts: line type differentiates them on the same panel
CONCEPT_COLORS <- c(
  "monthly"    = "#1B7BB4",   # blue
  "hourly"     = "#2C8C3C",   # green
  "hourly_eff" = "#C45C30"    # burnt orange
)
CONCEPT_LTYPES <- c(
  "monthly"    = "solid",
  "hourly"     = "dashed",
  "hourly_eff" = "dotted"
)
CONCEPT_SHORT <- c(
  "monthly"    = "Monthly",
  "hourly"     = "Hourly",
  "hourly_eff" = "Hourly (OT-adj.)"
)
CONCEPT_LABELS_LONG <- c(
  "monthly"    = "Monthly (Measure 1)",
  "hourly"     = "Hourly (Measure 2 \u2014 Primary)",
  "hourly_eff" = "Hourly OT-adjusted (Measure 3)"
)
CONCEPT_SUBTITLES <- c(
  "monthly"    = "Monthly income vs monthly MW floor | Measure 1: upper bound; part-timers may appear non-compliant",
  "hourly"     = "Hourly income vs hourly MW floor | Measure 2: primary; removes part-time bias",
  "hourly_eff" = "OT-adjusted hourly income vs hourly MW floor | Measure 3: robustness; hours >44 inflated by legal premia"
)

SCOPE_COLORS <- c("formal" = "#2C5F8A", "all_private" = "#C45C30")
SCOPE_LTYPES <- c("formal" = "solid",   "all_private" = "dashed")
SCOPE_LABELS_SHORT <- c(
  "formal"      = "Formal only",
  "all_private" = "Formal + Informal"
)

MW_EVENT_QTR <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")
SRC          <- "Sources: ENCFT 2014Q3\u20132025Q2; Central Bank of Dominican Republic."
REG_NOTE     <- "Base: private-sector employees, positive compliance income. Govt & Elec/Water excl. for Kaitz."
SNAP_LABELS  <- c("2016Q2" = "2016 Q2 (baseline)", "2025Q2" = "2025 Q2 (recent)")

CONCEPT_ORDER <- c(
  "Monthly (Measure 1)",
  "Hourly (Measure 2 \u2014 Primary)",
  "Hourly OT-adjusted (Measure 3)"
)

covid_rect <- function(all_qtrs) {
  xmin <- which(all_qtrs == "2020Q1"); xmax <- which(all_qtrs == "2020Q4")
  if (!length(xmin) || !length(xmax)) return(NULL)
  annotate("rect", xmin = xmin - 0.5, xmax = xmax + 0.5,
           ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.6)
}
event_pos <- function(all_qtrs) which(all_qtrs %in% MW_EVENT_QTR)

dist_x_scale <- scale_x_continuous(
  breaks = c(log2(0.25), log2(0.5), 0, 1, 2, 3),
  labels = c("\u00bc MW", "\u00bd MW", "= MW", "2\u00d7", "4\u00d7", "8\u00d7"),
  limits = c(log2(0.15), log2(10))
)
dist_x_narrow <- scale_x_continuous(
  breaks = c(log2(0.25), log2(0.5), 0, 1, 2),
  labels = c("\u00bc", "\u00bd", "MW", "2\u00d7", "4\u00d7"),
  limits = c(log2(0.15), log2(8))
)

sector_order <- dist_data %>%
  filter(year_quarter == "2016Q2", concept == "monthly", scope == "formal") %>%
  count(Employment_Sector, wt = FACTOR_EXPANSION, sort = TRUE) %>%
  pull(Employment_Sector)

concepts <- c("monthly", "hourly", "hourly_eff")
scopes   <- c("formal", "all_private")


#===============================================================================
# SECTIONS A & B — Distribution and Kaitz figures (one set per concept)
#===============================================================================

cat("\n[A/B] Distribution and Kaitz figures...\n")

for (cpt in concepts) {
  cat("  Concept:", cpt, "\n")
  
  # ── A1: Economy-wide KDE ────────────────────────────────────────────────────
  d <- dist_data %>%
    filter(concept == cpt, scope == "formal", !sparse) %>%
    mutate(Wage_group = factor(Wage_group, levels = TIER_LEVELS),
           snapshot   = factor(year_quarter, levels = names(SNAP_LABELS),
                               labels = SNAP_LABELS))
  
  bw <- d %>% group_by(snapshot, Wage_group) %>% filter(n() >= 15) %>%
    summarise(bw = bw.nrd0(log2_ratio), .groups = "drop") %>%
    pull(bw) %>% median(na.rm = TRUE)
  
  fig_A1 <- ggplot(d, aes(x = log2_ratio, weight = w_norm,
                          colour = Wage_group, fill = Wage_group)) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "grey80", alpha = 0.4) +
    geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.5) +
    geom_density(bw = bw, adjust = 1, alpha = 0, linewidth = 0.7) +
    geom_text(data = data.frame(snapshot = factor(SNAP_LABELS[1], levels = SNAP_LABELS)),
              aes(x = 0.06, y = Inf, label = "MW\nfloor"), inherit.aes = FALSE,
              vjust = 1.3, hjust = 0, size = 2.6, colour = "grey30") +
    facet_wrap(~snapshot) +
    scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
    scale_fill_manual(values = TIER_COLORS, name = "Firm size tier") +
    dist_x_scale +
    labs(title    = paste0("Formal Worker Income Distribution vs MW \u2014 ", CONCEPT_SHORT[[cpt]]),
         subtitle = CONCEPT_SUBTITLES[[cpt]],
         x        = "Income relative to tier MW  (log\u2082 scale: +1 = double the MW)",
         y        = "Density",
         caption  = paste("Formal private-sector employees. Weights normalised within snapshot\u00d7tier.",
                          "Grey fill: non-compliance zone.", REG_NOTE, SRC, sep = "\n")) +
    theme_surveytools() +
    theme(strip.text = element_text(face = "bold", size = 11),
          strip.background = element_rect(fill = "grey93", colour = "grey70"),
          axis.text.x = element_text(size = 8))
  
  save_fig(fig_A1, paste0("figA1_dist_econ_", cpt),
           w = config$fig_defaults$width * 2, h = config$fig_defaults$height * 1.4)
  
  # ── A2: Sector KDE ──────────────────────────────────────────────────────────
  d_sec <- dist_data %>%
    filter(concept == cpt, scope == "formal", !sparse) %>%
    mutate(Wage_group = factor(Wage_group, levels = TIER_LEVELS),
           snapshot   = factor(year_quarter, levels = names(SNAP_LABELS),
                               labels = SNAP_LABELS),
           Employment_Sector = factor(Employment_Sector, levels = sector_order))
  
  viable_sec <- d_sec %>%
    group_by(snapshot, Employment_Sector) %>%
    summarise(n_tiers = n_distinct(Wage_group), .groups = "drop") %>%
    group_by(Employment_Sector) %>%
    summarise(all_ok = all(n_tiers >= 2), .groups = "drop") %>%
    filter(all_ok) %>% pull(Employment_Sector)
  
  d_sec <- d_sec %>% filter(Employment_Sector %in% viable_sec)
  n_sec <- length(viable_sec)
  
  bw_sec <- d_sec %>%
    group_by(Employment_Sector, snapshot, Wage_group) %>% filter(n() >= 10) %>%
    summarise(bw = bw.nrd0(log2_ratio), .groups = "drop") %>%
    group_by(Employment_Sector) %>%
    summarise(bw = median(bw, na.rm = TRUE), .groups = "drop")
  
  d_sec <- d_sec %>% left_join(bw_sec, by = "Employment_Sector") %>% filter(!is.na(bw))
  
  fig_A2 <- ggplot(d_sec, aes(x = log2_ratio, weight = w_norm_sec, colour = Wage_group)) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "grey80", alpha = 0.35) +
    geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.35) +
    geom_density(aes(bw = bw), adjust = 1, alpha = 0, linewidth = 0.55) +
    facet_grid(Employment_Sector ~ snapshot, scales = "free_y") +
    scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
    dist_x_narrow +
    labs(title = paste0("Formal Worker Income by Sector & Firm Size \u2014 ", CONCEPT_SHORT[[cpt]]),
         subtitle = paste0("Formal private-sector employees. Free y-axis."),
         x = "Income relative to tier MW  (log\u2082 scale)", y = "Density",
         caption = paste("Cells <30 obs suppressed. Sectors with <2 tiers omitted.",
                         REG_NOTE, SRC, sep = "\n")) +
    theme_surveytools(legend_position = "bottom") +
    theme(strip.text.x = element_text(face = "bold", size = 9),
          strip.text.y = element_text(face = "bold", size = 7, angle = 0, hjust = 0),
          strip.background = element_rect(fill = "grey93", colour = "grey70"),
          axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 6),
          panel.spacing.y = unit(0.25, "lines"), panel.spacing.x = unit(0.5, "lines"))
  
  save_fig(fig_A2, paste0("figA2_dist_sector_", cpt),
           w = config$fig_defaults$width * 2.2,
           h = config$fig_defaults$height * n_sec * 0.6)
  
  # ── B1: Economy-wide Kaitz ───────────────────────────────────────────────────
  k <- kaitz_econ %>%
    filter(concept == cpt, scope == "formal", !sparse) %>%
    mutate(Wage_group = factor(Wage_group, levels = TIER_LEVELS))
  
  qtrs <- sort(unique(k$year_quarter))
  fig_B1 <- ggplot(k, aes(x = year_quarter, y = log_kaitz,
                          colour = Wage_group, group = Wage_group)) +
    covid_rect(qtrs) +
    geom_vline(xintercept = event_pos(qtrs), linetype = "dashed",
               colour = "red", linewidth = 0.4) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50", linewidth = 0.35) +
    geom_line(linewidth = 0.75) +
    scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
    scale_x_discrete(breaks = qtrs[seq(1, length(qtrs), by = 4)]) +
    labs(title    = paste0("Log Kaitz Index by Tier \u2014 ", CONCEPT_SHORT[[cpt]],
                           " | Formal workers"),
         subtitle = CONCEPT_SUBTITLES[[cpt]],
         y = "Log Kaitz Index", x = NULL,
         caption  = paste("Kaitz > 0: MW exceeds median (binding).",
                          paste0("Red dashed: MW events (", paste(MW_EVENT_QTR, collapse=", "), ")."),
                          "Grey band: COVID-19.", REG_NOTE, SRC, sep = "\n")) +
    theme_surveytools() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))
  
  save_fig(fig_B1, paste0("figB1_kaitz_econ_", cpt),
           w = config$fig_defaults$width * 1.7, h = config$fig_defaults$height)
  
  # ── B2: Sector Kaitz ────────────────────────────────────────────────────────
  k_sec <- kaitz_sec %>%
    filter(concept == cpt, scope == "formal", !sparse) %>%
    mutate(Wage_group = factor(Wage_group, levels = TIER_LEVELS),
           Employment_Sector = factor(Employment_Sector, levels = sector_order))
  
  viable_k <- k_sec %>%
    group_by(Employment_Sector) %>%
    summarise(n_tiers = n_distinct(Wage_group), .groups = "drop") %>%
    filter(n_tiers >= 2) %>% pull(Employment_Sector)
  
  k_sec <- k_sec %>% filter(Employment_Sector %in% viable_k)
  n_k   <- length(viable_k)
  qtrs_s <- sort(unique(k_sec$year_quarter))
  
  fig_B2 <- ggplot(k_sec, aes(x = year_quarter, y = log_kaitz,
                              colour = Wage_group, group = Wage_group)) +
    covid_rect(qtrs_s) +
    geom_vline(xintercept = event_pos(qtrs_s), linetype = "dashed",
               colour = "red", linewidth = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50", linewidth = 0.3) +
    geom_line(linewidth = 0.6, na.rm = TRUE) +
    facet_wrap(~Employment_Sector, ncol = 2, scales = "free_y") +
    scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
    scale_x_discrete(breaks = qtrs_s[seq(1, length(qtrs_s), by = 8)]) +
    labs(title    = paste0("Log Kaitz by Sector & Tier \u2014 ", CONCEPT_SHORT[[cpt]]),
         subtitle = "Formal private-sector employees. Free y-axis.",
         y = "Log Kaitz Index", x = NULL,
         caption  = paste("Cells <30 obs suppressed. Sectors with <2 tiers omitted.",
                          REG_NOTE, SRC, sep = "\n")) +
    theme_surveytools(legend_position = "bottom") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
          strip.text = element_text(face = "bold", size = 8),
          strip.background = element_rect(fill = "grey93", colour = "grey70"),
          panel.spacing = unit(0.5, "lines"))
  
  save_fig(fig_B2, paste0("figB2_kaitz_sector_", cpt),
           w = config$fig_defaults$width * 2,
           h = config$fig_defaults$height * ceiling(n_k / 2) * 0.9)
  
  # ── Combined: A1 + B1 ───────────────────────────────────────────────────────
  fig_comb <- fig_A1 / fig_B1 +
    plot_annotation(
      title    = paste0("MW Bindingness \u2014 ", CONCEPT_SHORT[[cpt]],
                        " | Formal private-sector employees"),
      subtitle = CONCEPT_SUBTITLES[[cpt]],
      theme    = theme(plot.title    = element_text(size = 13, face = "bold"),
                       plot.subtitle = element_text(size = 9, colour = "grey30"))
    )
  save_fig(fig_comb, paste0("fig_combined_", cpt),
           w = config$fig_defaults$width * 2, h = config$fig_defaults$height * 2.8)
  
  cat("    A1+A2+B1+B2+combined saved for", cpt, "\n")
}


#===============================================================================
# SECTION C — Cross-concept Kaitz comparison
#===============================================================================

cat("\n[C] Cross-concept Kaitz comparison...\n")

for (scp in scopes) {
  k_all <- kaitz_econ %>%
    filter(scope == scp, !sparse) %>%
    mutate(Wage_group    = factor(Wage_group, levels = TIER_LEVELS),
           concept_label = factor(concept_label, levels = CONCEPT_ORDER))
  
  qtrs_c <- sort(unique(k_all$year_quarter))
  fig_C <- ggplot(k_all, aes(x = year_quarter, y = log_kaitz,
                             colour = Wage_group, group = Wage_group)) +
    covid_rect(qtrs_c) +
    geom_vline(xintercept = event_pos(qtrs_c), linetype = "dashed",
               colour = "red", linewidth = 0.35) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50", linewidth = 0.3) +
    geom_line(linewidth = 0.65) +
    facet_wrap(~concept_label, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
    scale_x_discrete(breaks = qtrs_c[seq(1, length(qtrs_c), by = 4)]) +
    labs(title    = paste0("Log Kaitz \u2014 Concept Comparison | ",
                           ifelse(scp == "formal", "Formal workers", "All private workers")),
         subtitle = "Free y-axis. Each panel: different income/MW definition.",
         y = "Log Kaitz Index", x = NULL,
         caption  = paste(REG_NOTE, SRC, sep = "\n")) +
    theme_surveytools(legend_position = "bottom") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
          strip.text = element_text(face = "bold", size = 9),
          strip.background = element_rect(fill = "grey93", colour = "grey70"),
          panel.spacing = unit(0.5, "lines"))
  
  save_fig(fig_C, paste0("figC_kaitz_concept_compare_", scp),
           w = config$fig_defaults$width * 1.7, h = config$fig_defaults$height * 3)
  cat("    Saved figC for scope:", scp, "\n")
}


#===============================================================================
# SECTION D — Scope comparison figures (formal vs formal+informal)
#===============================================================================

cat("\n[D] Scope comparison figures...\n")

for (cpt in concepts) {
  # D1: KDE by scope
  d_scope <- dist_data %>%
    filter(concept == cpt, !sparse) %>%
    mutate(Wage_group = factor(Wage_group, levels = TIER_LEVELS),
           snapshot   = factor(year_quarter, levels = names(SNAP_LABELS),
                               labels = SNAP_LABELS),
           scope_lbl  = factor(SCOPE_LABELS_SHORT[scope],
                               levels = c("Formal only", "Formal + Informal")))
  
  bw_d <- d_scope %>% group_by(snapshot, Wage_group, scope) %>%
    filter(n() >= 15) %>%
    summarise(bw = bw.nrd0(log2_ratio), .groups = "drop") %>%
    pull(bw) %>% median(na.rm = TRUE)
  
  fig_D1 <- ggplot(d_scope, aes(x = log2_ratio, weight = w_norm,
                                colour = scope_lbl, linetype = scope_lbl)) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = "grey80", alpha = 0.35) +
    geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
    geom_density(bw = bw_d, adjust = 1, alpha = 0, linewidth = 0.7) +
    facet_grid(Wage_group ~ snapshot, scales = "free_y") +
    scale_colour_manual(values = c("Formal only" = SCOPE_COLORS[["formal"]],
                                   "Formal + Informal" = SCOPE_COLORS[["all_private"]]),
                        name = "Worker scope") +
    scale_linetype_manual(values = c("Formal only" = "solid",
                                     "Formal + Informal" = "dashed"),
                          name = "Worker scope") +
    dist_x_scale +
    labs(title    = paste0("Distribution Scope Comparison \u2014 ", CONCEPT_SHORT[[cpt]]),
         subtitle = "Solid = formal only. Dashed = formal + informal. If similar, spillovers are present.",
         x = "Income relative to tier MW  (log\u2082 scale)", y = "Density",
         caption  = paste(REG_NOTE, SRC, sep = "\n")) +
    theme_surveytools(legend_position = "bottom") +
    theme(strip.text = element_text(face = "bold", size = 9),
          strip.background = element_rect(fill = "grey93", colour = "grey70"),
          panel.spacing = unit(0.4, "lines"))
  
  save_fig(fig_D1, paste0("figD1_dist_scope_compare_", cpt),
           w = config$fig_defaults$width * 2, h = config$fig_defaults$height * 2.2)
  
  # D2: Kaitz by scope
  k_scope <- kaitz_econ %>%
    filter(concept == cpt, !sparse) %>%
    mutate(Wage_group = factor(Wage_group, levels = TIER_LEVELS),
           scope_lbl  = factor(SCOPE_LABELS_SHORT[scope],
                               levels = c("Formal only", "Formal + Informal")))
  
  qtrs_d <- sort(unique(k_scope$year_quarter))
  fig_D2 <- ggplot(k_scope, aes(x = year_quarter, y = log_kaitz,
                                colour = scope_lbl, linetype = scope_lbl,
                                group = interaction(Wage_group, scope_lbl))) +
    covid_rect(qtrs_d) +
    geom_vline(xintercept = event_pos(qtrs_d), linetype = "dashed",
               colour = "red", linewidth = 0.35, alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50", linewidth = 0.3) +
    geom_line(linewidth = 0.65) +
    facet_wrap(~Wage_group, ncol = 2, scales = "free_y") +
    scale_colour_manual(values = c("Formal only" = SCOPE_COLORS[["formal"]],
                                   "Formal + Informal" = SCOPE_COLORS[["all_private"]]),
                        name = "Worker scope") +
    scale_linetype_manual(values = c("Formal only" = "solid",
                                     "Formal + Informal" = "dashed"),
                          name = "Worker scope") +
    scale_x_discrete(breaks = qtrs_d[seq(1, length(qtrs_d), by = 4)]) +
    labs(title    = paste0("Kaitz Scope Comparison \u2014 ", CONCEPT_SHORT[[cpt]]),
         subtitle = "Solid = formal. Dashed = all private. Gap = informality pulls median down.",
         y = "Log Kaitz Index", x = NULL,
         caption  = paste(REG_NOTE, SRC, sep = "\n")) +
    theme_surveytools(legend_position = "bottom") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
          strip.text = element_text(face = "bold", size = 9),
          strip.background = element_rect(fill = "grey93", colour = "grey70"))
  
  save_fig(fig_D2, paste0("figD2_kaitz_scope_compare_", cpt),
           w = config$fig_defaults$width * 2, h = config$fig_defaults$height * 1.6)
  
  cat("    D1+D2 saved for", cpt, "\n")
}


#===============================================================================
# SECTION E — Compliance time series (NEW)
#===============================================================================

cat("\n[E] Compliance figures...\n")

COMP_NOTE <- paste(
  "Non-compliance rate = share of workers earning below the tier-specific MW floor.",
  "Measure 1 (monthly): direct comparison, no hours adjustment \u2014 upper bound.",
  "Measure 2 (hourly): corrects for part-time status \u2014 primary measure.",
  "Measure 3 (OT-adjusted): additionally adjusts for overtime non-compliance."
)

# ── E1: Economy-wide NC rate — formal vs all-private, all three concepts ──────
cat("    E1: Economy-wide compliance...\n")

comp_e1 <- comp_econ %>%
  mutate(
    concept_label = factor(concept_label, levels = CONCEPT_ORDER),
    scope_lbl     = factor(SCOPE_LABELS_SHORT[scope],
                           levels = c("Formal only", "Formal + Informal"))
  )

qtrs_e1 <- sort(unique(comp_e1$year_quarter))

fig_E1 <- ggplot(comp_e1,
                 aes(x = year_quarter, y = nc_rate,
                     colour = scope_lbl, linetype = scope_lbl,
                     group = interaction(concept_label, scope_lbl))) +
  covid_rect(qtrs_e1) +
  geom_vline(xintercept = event_pos(qtrs_e1), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~concept_label, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_colour_manual(values = c("Formal only" = SCOPE_COLORS[["formal"]],
                                 "Formal + Informal" = SCOPE_COLORS[["all_private"]]),
                      name = "Worker scope") +
  scale_linetype_manual(values = c("Formal only" = "solid",
                                   "Formal + Informal" = "dashed"),
                        name = "Worker scope") +
  scale_x_discrete(breaks = qtrs_e1[seq(1, length(qtrs_e1), by = 4)]) +
  labs(
    title    = "Non-Compliance Rate Over Time \u2014 Economy-Wide",
    subtitle = "Share of private-sector employees earning below the MW floor. All three measures.",
    y        = "Non-compliance rate",
    x        = NULL,
    caption  = paste(COMP_NOTE,
                     paste0("Red dashed: MW events (", paste(MW_EVENT_QTR, collapse = ", "), ")."),
                     "All private-sector employees, positive compliance income.",
                     SRC, sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
        strip.text = element_text(face = "bold", size = 9),
        strip.background = element_rect(fill = "grey93", colour = "grey70"),
        panel.spacing = unit(0.5, "lines"))

save_fig(fig_E1, "figE1_compliance_econ",
         w = config$fig_defaults$width * 1.7, h = config$fig_defaults$height * 3)


# ── E2: By firm size tier — formal scope, all three concepts ──────────────────
cat("    E2: Tier-level compliance...\n")

comp_e2 <- comp_tier %>%
  filter(scope == "formal", !sparse) %>%
  mutate(
    Wage_group    = factor(Wage_group, levels = TIER_LEVELS),
    concept_label = factor(concept_label, levels = CONCEPT_ORDER),
    concept_short = CONCEPT_SHORT[concept]
  )

qtrs_e2 <- sort(unique(comp_e2$year_quarter))

fig_E2 <- ggplot(comp_e2,
                 aes(x = year_quarter, y = nc_rate,
                     colour = concept, linetype = concept,
                     group = concept)) +
  covid_rect(qtrs_e2) +
  geom_vline(xintercept = event_pos(qtrs_e2), linetype = "dashed",
             colour = "red", linewidth = 0.35) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~Wage_group, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_colour_manual(values = CONCEPT_COLORS,
                      labels = CONCEPT_SHORT,
                      name   = "Income measure") +
  scale_linetype_manual(values = CONCEPT_LTYPES,
                        labels = CONCEPT_SHORT,
                        name   = "Income measure") +
  scale_x_discrete(breaks = qtrs_e2[seq(1, length(qtrs_e2), by = 4)]) +
  labs(
    title    = "Non-Compliance Rate by Firm Size Tier \u2014 Formal Workers",
    subtitle = "Share of formal workers earning below tier-specific MW. Three income concepts shown.",
    y        = "Non-compliance rate",
    x        = NULL,
    caption  = paste(COMP_NOTE,
                     "Formal private-sector employees with known firm size.",
                     SRC, sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
        strip.text = element_text(face = "bold", size = 10),
        strip.background = element_rect(fill = "grey93", colour = "grey70"),
        panel.spacing = unit(0.5, "lines"))

save_fig(fig_E2, "figE2_compliance_tier",
         w = config$fig_defaults$width * 2, h = config$fig_defaults$height * 1.8)


# ── E3: By sector — formal scope, all three concepts ──────────────────────────
cat("    E3: Sector-level compliance...\n")

comp_e3 <- comp_sec %>%
  filter(scope == "formal", !sparse) %>%
  mutate(
    Employment_Sector = factor(Employment_Sector, levels = sector_order),
    concept_label     = factor(concept_label, levels = CONCEPT_ORDER)
  )

viable_sec_e3 <- comp_e3 %>%
  group_by(Employment_Sector) %>%
  summarise(n_concepts = n_distinct(concept), .groups = "drop") %>%
  filter(n_concepts == 3) %>% pull(Employment_Sector)

comp_e3 <- comp_e3 %>% filter(Employment_Sector %in% viable_sec_e3)
n_sec_e3 <- length(viable_sec_e3)
qtrs_e3  <- sort(unique(comp_e3$year_quarter))

fig_E3 <- ggplot(comp_e3,
                 aes(x = year_quarter, y = nc_rate,
                     colour = concept, linetype = concept,
                     group = concept)) +
  covid_rect(qtrs_e3) +
  geom_vline(xintercept = event_pos(qtrs_e3), linetype = "dashed",
             colour = "red", linewidth = 0.35) +
  geom_line(linewidth = 0.6, na.rm = TRUE) +
  facet_wrap(~Employment_Sector, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_colour_manual(values = CONCEPT_COLORS,
                      labels = CONCEPT_SHORT, name = "Income measure") +
  scale_linetype_manual(values = CONCEPT_LTYPES,
                        labels = CONCEPT_SHORT, name = "Income measure") +
  scale_x_discrete(breaks = qtrs_e3[seq(1, length(qtrs_e3), by = 8)]) +
  labs(
    title    = "Non-Compliance Rate by Sector \u2014 Formal Workers",
    subtitle = "Share below tier MW. Three income measures. Free y-axis.",
    y        = "Non-compliance rate",
    x        = NULL,
    caption  = paste("Formal private-sector employees with known firm size.",
                     "Cells <30 obs suppressed. Sectors without all 3 measures omitted.",
                     SRC, sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
        strip.text = element_text(face = "bold", size = 8),
        strip.background = element_rect(fill = "grey93", colour = "grey70"),
        panel.spacing = unit(0.5, "lines"))

save_fig(fig_E3, "figE3_compliance_sector",
         w = config$fig_defaults$width * 2,
         h = config$fig_defaults$height * ceiling(n_sec_e3 / 2) * 0.9)


# ── E4: Economy-wide — all concepts × both scopes on one chart ────────────────
cat("    E4: Cross-concept × scope compliance...\n")

comp_e4 <- comp_econ %>%
  mutate(
    concept_label = factor(concept_label, levels = CONCEPT_ORDER),
    scope_lbl     = factor(SCOPE_LABELS_SHORT[scope],
                           levels = c("Formal only", "Formal + Informal")),
    series        = interaction(scope_lbl, concept, sep = "__")
  )

qtrs_e4 <- sort(unique(comp_e4$year_quarter))

fig_E4 <- ggplot(comp_e4,
                 aes(x = year_quarter, y = nc_rate,
                     colour = scope_lbl, linetype = concept,
                     group = series)) +
  covid_rect(qtrs_e4) +
  geom_vline(xintercept = event_pos(qtrs_e4), linetype = "dashed",
             colour = "red", linewidth = 0.4, alpha = 0.5) +
  geom_line(linewidth = 0.65) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_colour_manual(values = c("Formal only" = SCOPE_COLORS[["formal"]],
                                 "Formal + Informal" = SCOPE_COLORS[["all_private"]]),
                      name = "Scope") +
  scale_linetype_manual(values = c("monthly" = "solid",
                                   "hourly" = "dashed",
                                   "hourly_eff" = "dotted"),
                        labels = CONCEPT_SHORT,
                        name = "Measure") +
  scale_x_discrete(breaks = qtrs_e4[seq(1, length(qtrs_e4), by = 4)]) +
  labs(
    title    = "Non-Compliance Rate \u2014 All Concepts & Scopes",
    subtitle = paste0(
      "Colour = worker scope (formal vs all private). ",
      "Line type = income measure (monthly/hourly/OT-adj.).\n",
      "Gap between formal and all-private = informality contributes to aggregate non-compliance."
    ),
    y        = "Non-compliance rate",
    x        = NULL,
    caption  = paste(COMP_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))

save_fig(fig_E4, "figE4_compliance_concept_scope_compare",
         w = config$fig_defaults$width * 1.7, h = config$fig_defaults$height * 1.4)

cat("    E1-E4 saved.\n")


#===============================================================================
# DONE
#===============================================================================

cat("\n=== 05b_MW_Bindingness_Figures.R complete ===\n")
cat("All figures saved to:\n  ", save_path, "\n\n")

all_figs <- c(
  paste0(c("figA1_dist_econ_", "figA2_dist_sector_",
           "figB1_kaitz_econ_", "figB2_kaitz_sector_",
           "fig_combined_",
           "figD1_dist_scope_compare_", "figD2_kaitz_scope_compare_"),
         rep(concepts, each = 1), recycle0 = FALSE) |>
    {\(x) unlist(lapply(concepts, function(c) gsub("$", c, x, fixed = TRUE)))}(),
  paste0("figC_kaitz_concept_compare_", scopes),
  paste0("figE", 1:4, "_compliance_", c("econ", "tier", "sector", "concept_scope_compare"))
)
cat("Total figures:", length(all_figs), "\n")