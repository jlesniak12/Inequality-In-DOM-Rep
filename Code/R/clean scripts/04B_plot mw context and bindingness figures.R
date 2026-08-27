#===============================================================================
#
# Script: 04B_Plot_MW_Context_and_Bindingness.R
#
# Purpose: Build the MW context and bindingness figures from the RDS objects
#          produced by 04A. No survey computation here.
#
# FIGURES
#   fig_MW1_minwage_levels          Real MW levels by LEGAL tier
#   fig_MW2_growth_decomp           MW growth split into real gain vs inflation
#   fig_MW3_firmsize_shares_*       Employment share by OBSERVED tier
#   fig_MW4_kaitz                   Log Kaitz by OBSERVED tier (all tiers)
#   fig_MW4b_kaitz_micro            Micro vs Small since the micro tier began
#   fig_MW5a_noncompliance_econ     Non-compliance, economy-wide
#   fig_MW5a_noncompliance_measures Monthly vs hourly measure gap
#   fig_MW5b_noncompliance_tier     Non-compliance by tier
#   fig_MW6_bunching_kde            Density vs own floor at 4 focal moments
#   fig_MW6_bunching_hist           Bunching histogram, Micro/Small/Rest
#   fig_MW6b_micro_bite             Share below the incoming micro floor
#
# CONVENTIONS: MW_EVENT_QTR, SRC, MW_NOTE, event_pos(), covid_rect(),
# qtr_breaks() all come from 00_setup.R and are derived from config. Do NOT
# redefine them here — that is how the event list silently desynced from the
# data in the previous version.
#===============================================================================

source(here::here("Code", "R", "clean scripts", "00_setup.R"))

cat("=== 04B_Plot_MW_Context_and_Bindingness.R ===\n\n")


#===============================================================================
# CONSTANTS
#===============================================================================

TIER_LEVELS  <- config$TIER_LEVELS
TIER_COLORS  <- config$figures$tier_colors
BUNCH_COLORS <- c("Micro" = "#1b7837", "Small" = "#762a83", "Rest" = "#1f78b4")

HEADLINE  <- config$figures$headline_concept              # "monthly" | "hourly"
OTHER     <- setdiff(c("monthly", "hourly"), HEADLINE)
RATIO_VAR <- paste0("log2_ratio_", HEADLINE)
MICRO_START <- config$figures$micro_tier_start_qtr

CONCEPT_BLURB <- c(
  monthly = "Monthly earnings vs the monthly floor. Part-time workers paid above the hourly floor are counted as below the monthly floor.",
  hourly  = "Hourly rate (hours capped at 44) vs the hourly floor."
)

in_dir   <- config$data_dirs$minwage
read_obj <- function(name) readRDS(file.path(in_dir, paste0(name, ".rds")))

save_path <- file.path(config$paths$outputs, config$output_stage,
                       config$out_subdirs$desc_fig, "Min Wage")
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)

save_fig <- function(p, name,
                     w = config$fig_defaults$width,
                     h = config$fig_defaults$height) {
  ggsave(file.path(save_path, paste0(name, ".", config$fig_defaults$format)),
         plot = p, width = w, height = h, dpi = config$fig_defaults$dpi)
  message("Saved: ", name)
}

MICRO_NOTE <- sprintf(
  paste("The Micro tier was created in %s. Before then, sub-10-worker firms",
        "were legally bound by the Small floor."), MICRO_START)


#===============================================================================
# FIGURE MW-1: Real MW levels by LEGAL tier
#
# Keyed on wage_group_legal, so the pre-micro period shows ONE line for the
# Small floor rather than two identical lines labelled Micro and Small. The
# Micro line begins at the quarter the tier was created — and begins BELOW the
# Small line, which is the substantive point: the reform gave those firms a
# lower floor, it did not raise anything.
#===============================================================================

cat("[MW-1] Real MW levels by legal tier...\n")

mw_levels <- read_obj("mw_context_levels") %>%
  dplyr::mutate(time = as.character(year_quarter))

qtrs1 <- mw_levels$time

fig_MW1 <- ggplot(mw_levels,
                  aes(x = time, y = real_minwage_harmonized,
                      colour = wage_group_legal, group = wage_group_legal)) +
  covid_rect(qtrs1) +
  geom_vline(xintercept = event_pos(qtrs1), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = TIER_COLORS, name = "Legal MW tier",
                      drop = FALSE) +
  scale_x_discrete(breaks = qtr_breaks(qtrs1)) +
  labs(
    title    = "Real Minimum Wage by Legal Tier",
    subtitle = sprintf("CPI-deflated Dominican pesos (%s base)", DEFLATOR_LABEL),
    x = NULL, y = sprintf("Real minimum wage (%s DOP)", DEFLATOR_LABEL),
    caption = paste(
      "Series are keyed on the floor that legally applied, not on observed firm size.",
      MICRO_NOTE,
      "The Micro line starts below Small: the 2021 reform set a LOWER floor for those firms.",
      MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_MW1, "fig_MW1_minwage_levels")


#===============================================================================
# FIGURE MW-2: MW growth decomposition
#
# Each bar is the CUMULATIVE change since the previous announcement and so spans
# any phase-in tranche in between. It will not match a single resolucion's
# headline percentage; the subtitle says so explicitly.
#
# Because the panel is keyed on legal tier, the pre-micro panels have three bars
# and the post-micro panels four. That asymmetry is correct and self-explaining.
#===============================================================================

cat("[MW-2] MW growth decomposition...\n")

decomp <- read_obj("mw_context_growth_decomp")

base_map <- decomp %>%
  dplyr::distinct(year_quarter, base_quarter) %>%
  dplyr::mutate(lab = paste0(year_quarter, "\n(vs ", base_quarter, ")"))

decomp_long <- decomp %>%
  dplyr::left_join(base_map, by = c("year_quarter", "base_quarter")) %>%
  dplyr::select(lab, year_quarter, wage_group_legal,
                real_pct_chg, inflation_component) %>%
  tidyr::pivot_longer(c(real_pct_chg, inflation_component),
                      names_to = "component", values_to = "pct") %>%
  dplyr::mutate(
    component = dplyr::recode(component,
                              real_pct_chg        = "Real gain",
                              inflation_component = "Eroded by inflation"),
    component = factor(component, levels = c("Eroded by inflation", "Real gain")),
    lab = factor(lab, levels = base_map$lab[order(base_map$year_quarter)])
  ) %>%
  tidyr::drop_na(pct)

fig_MW2 <- ggplot(decomp_long,
                  aes(x = wage_group_legal, y = pct, fill = component)) +
  geom_col(width = 0.7, colour = "white") +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  facet_wrap(~lab, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = c("Real gain" = "#1b7837",
                               "Eroded by inflation" = "grey70"), name = NULL) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
  labs(
    title    = "Decomposing Each Minimum Wage Increase",
    subtitle = "Cumulative nominal change since the previous announcement, split into real gain and inflation erosion",
    x = "Legal MW tier", y = "% change since previous announcement",
    caption = paste(
      "Bars are CUMULATIVE over the full policy cycle and therefore include any",
      "phase-in tranche falling between the two announcements. They will not match",
      "the headline percentage of a single resolucion.",
      "Real gain + inflation component = nominal change, by construction.",
      MICRO_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

save_fig(fig_MW2, "fig_MW2_growth_decomp",
         w = config$fig_defaults$width * 1.7, h = config$fig_defaults$height)


#===============================================================================
# FIGURE MW-3: Employment share by OBSERVED tier
#===============================================================================

cat("[MW-3] Employment share by tier...\n")

fs_shares <- read_obj("mw_context_firmsize_shares") %>%
  dplyr::mutate(time = as.character(year_quarter))
qtrs3 <- fs_shares$time

fs_caption <- paste(
  "All employed (OCUPADO == 1), conditional on reporting a firm size.",
  "Keyed on OBSERVED firm size, which is measured identically in every quarter,",
  "so the 2021 creation of the Micro tier does not shift these series.",
  MW_NOTE, SRC, sep = "\n")

fig_MW3a <- ggplot(fs_shares,
                   aes(x = time, y = share_of_known,
                       colour = wage_group, group = wage_group)) +
  covid_rect(qtrs3) +
  geom_vline(xintercept = event_pos(qtrs3), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = TIER_COLORS, name = "Firm size (observed)") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(breaks = qtr_breaks(qtrs3)) +
  labs(title = "Employment Share by Firm Size Tier",
       subtitle = "Share of employed workers with known firm size",
       x = NULL, y = "Share of employed (known firm size)",
       caption = fs_caption) +
  theme_surveytools()

save_fig(fig_MW3a, "fig_MW3_firmsize_shares_lines")

fig_MW3b <- ggplot(fs_shares,
                   aes(x = time, y = share_of_known,
                       fill = wage_group, group = wage_group)) +
  geom_area(position = "stack", alpha = 0.85) +
  geom_vline(xintercept = event_pos(qtrs3), linetype = "dashed",
             colour = "white", linewidth = 0.4) +
  scale_fill_manual(values = TIER_COLORS, name = "Firm size (observed)") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(breaks = qtr_breaks(qtrs3)) +
  labs(title = "Employment Composition by Firm Size",
       subtitle = "Cumulative share adds to 100% within each quarter (known firm size only)",
       x = NULL, y = "Cumulative share", caption = fs_caption) +
  theme_surveytools(legend_position = "right")

save_fig(fig_MW3b, "fig_MW3_firmsize_shares_stacked")


#===============================================================================
# FIGURE MW-4: Log Kaitz by OBSERVED tier
#
# Numerator is the floor that legally applied; denominator is the median among
# workers actually in that observed size class. Pre-2021Q3 Micro is therefore a
# genuine series (Small floor over micro-firm median), not a copy of Small, and
# it is drawn dashed to flag the imputed floor.
#===============================================================================

cat("[MW-4] Log Kaitz index...\n")

kaitz <- read_obj("mw_bind_kaitz") %>%
  dplyr::filter(!sparse) %>%
  dplyr::mutate(time = as.character(year_quarter),
                wage_group = factor(wage_group, levels = TIER_LEVELS))
qtrs4 <- kaitz$time

fig_MW4 <- ggplot(kaitz,
                  aes(x = time, y = log_kaitz, colour = wage_group)) +
  covid_rect(qtrs4) +
  geom_vline(xintercept = event_pos(qtrs4), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50",
             linewidth = 0.3) +
  geom_line(aes(linetype = mw_floor_imputed,
                group    = interaction(wage_group, mw_floor_imputed)),
            linewidth = 0.7) +
  scale_colour_manual(values = TIER_COLORS, name = "Firm size (observed)") +
  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "22"),
                        labels = c("FALSE" = "Own-tier floor",
                                   "TRUE"  = "Small floor (pre-Micro)"),
                        name = "Floor applied") +
  scale_x_discrete(breaks = qtr_breaks(qtrs4)) +
  labs(
    title    = "Log Kaitz Index by Firm Size Tier",
    subtitle = "log(real floor applying to the tier) - log(median formal monthly salary within the tier)",
    x = NULL, y = "Log Kaitz index",
    caption = paste(
      "Closer to 0 = floor nearer the median = more binding.",
      "Grouped on OBSERVED firm size; numerator is the floor that legally applied.",
      "Dashed Micro segment: Small floor over the micro-firm median, a distinct",
      "series from Small (different denominator), not a duplicate of it.",
      sprintf("Sparse cells (n < %d) omitted.", config$figures$min_cell_n),
      MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_MW4, "fig_MW4_kaitz")

# ── MW-4b: Micro vs Small since the micro tier began ─────────────────────────
# The requested "micro firms since 2021Q3" view. Restricting to the post-reform
# window makes the level gap between the two floors legible; it is squashed on
# the full-sample panel above.
kaitz_micro <- kaitz %>%
  dplyr::filter(wage_group %in% c("Micro", "Small"),
                as.character(year_quarter) >= MICRO_START)
qtrs4b <- kaitz_micro$time

fig_MW4b <- ggplot(kaitz_micro,
                   aes(x = time, y = log_kaitz,
                       colour = wage_group, group = wage_group)) +
  geom_vline(xintercept = event_pos(qtrs4b), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50",
             linewidth = 0.3) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2) +
  scale_colour_manual(values = TIER_COLORS, name = "Firm size (observed)") +
  scale_x_discrete(breaks = qtr_breaks(qtrs4b, every = 2)) +
  labs(
    title    = sprintf("Log Kaitz: Micro vs Small Firms Since %s", MICRO_START),
    subtitle = "How binding each floor is, from the quarter the Micro tier was created",
    x = NULL, y = "Log Kaitz index",
    caption = paste(
      "Micro firms moved from the Small floor to a new, lower Micro floor at",
      sprintf("%s. A lower Kaitz means the floor sits further below the median.",
              MICRO_START),
      MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_MW4b, "fig_MW4b_kaitz_micro")


#===============================================================================
# FIGURE MW-5a: Non-compliance, economy-wide
#===============================================================================

cat("[MW-5a] Non-compliance, economy-wide...\n")

nc_econ_all <- read_obj("mw_bind_noncompliance_econ")

nc_scope <- nc_econ_all %>%
  dplyr::filter(concept == HEADLINE,
                scope %in% c("formal", "formal_ex_large")) %>%
  dplyr::mutate(time = as.character(year_quarter),
                scope_label = dplyr::recode(scope,
                                            formal          = "All tiers",
                                            formal_ex_large = "Excluding Large"))
qtrs5 <- nc_scope$time

fig_MW5a <- ggplot(nc_scope,
                   aes(x = time, y = nc_rate,
                       colour = scope_label, group = scope_label)) +
  covid_rect(qtrs5) +
  geom_vline(xintercept = event_pos(qtrs5), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("All tiers" = "#1f78b4",
                                 "Excluding Large" = "#33a02c"), name = "Scope") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(breaks = qtr_breaks(qtrs5)) +
  labs(
    title    = "Minimum Wage Non-Compliance Over Time",
    subtitle = sprintf("Share of formal private employees below the tier floor (%s measure)",
                       HEADLINE),
    x = NULL, y = "Non-compliance rate",
    caption = paste(
      CONCEPT_BLURB[[HEADLINE]],
      "'Excluding Large' drops the contaminated 100+ survey bin; if the trend",
      "survives, it is not an artifact of large-firm misclassification.",
      MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_MW5a, "fig_MW5a_noncompliance_econ")

# ── Monthly vs hourly measure gap (kept regardless of which is headline) ─────
nc_measures <- nc_econ_all %>%
  dplyr::filter(scope == "formal") %>%
  dplyr::mutate(time = as.character(year_quarter))
qtrs5m <- nc_measures$time

fig_MW5a_meas <- ggplot(nc_measures,
                        aes(x = time, y = nc_rate,
                            colour = concept_label, group = concept_label)) +
  covid_rect(qtrs5m) +
  geom_vline(xintercept = event_pos(qtrs5m), linetype = "dashed",
             colour = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("#1f78b4", "#e08214"), name = "Income measure") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(breaks = qtr_breaks(qtrs5m)) +
  labs(
    title    = "Non-Compliance: Monthly vs Hourly Measure",
    subtitle = "Share of formal private employees below the tier floor (all tiers)",
    x = NULL, y = "Non-compliance rate",
    caption = paste(
      CONCEPT_BLURB[["monthly"]], CONCEPT_BLURB[["hourly"]],
      "The gap is sub-full-time workers who clear the hourly but not the monthly",
      "floor. Its width moves with the part-time share, which is itself an outcome.",
      MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_MW5a_meas, "fig_MW5a_noncompliance_measures")


#===============================================================================
# FIGURE MW-5b: Non-compliance by OBSERVED tier
#===============================================================================

cat("[MW-5b] Non-compliance by tier...\n")

nc_tier <- read_obj("mw_bind_noncompliance_tier") %>%
  dplyr::filter(scope == "formal", concept == HEADLINE, !sparse) %>%
  dplyr::mutate(time = as.character(year_quarter),
                wage_group = factor(wage_group, levels = TIER_LEVELS))
qtrs5b <- nc_tier$time

fig_MW5b <- ggplot(nc_tier,
                   aes(x = time, y = nc_rate,
                       colour = wage_group, group = wage_group)) +
  covid_rect(qtrs5b) +
  geom_vline(xintercept = event_pos(qtrs5b), linetype = "dashed",
             colour = "red", linewidth = 0.35) +
  geom_line(aes(linetype = mw_floor_imputed,
                group    = interaction(wage_group, mw_floor_imputed)),
            linewidth = 0.7) +
  facet_wrap(~wage_group, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = TIER_COLORS, guide = "none") +
  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "22"),
                        guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(breaks = qtr_breaks(qtrs5b)) +
  labs(
    title    = "Non-Compliance by Firm Size Tier (Formal Workers)",
    subtitle = sprintf("Share below the applicable floor, %s measure (free y-axis)",
                       HEADLINE),
    x = NULL, y = "Non-compliance rate",
    caption = paste(
      CONCEPT_BLURB[[HEADLINE]],
      "Grouped on OBSERVED firm size. Dashed Micro segment: compliance measured",
      "against the Small floor, which is what legally applied then.",
      "Large-tier non-compliance partly reflects the 100+ survey bin containing",
      "legal mediums judged against the higher large floor.",
      MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools() +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey93", colour = "grey70"))

save_fig(fig_MW5b, "fig_MW5b_noncompliance_tier",
         w = config$fig_defaults$width * 1.4,
         h = config$fig_defaults$height * 1.3)


#===============================================================================
# FIGURE MW-6: Bunching at four focal moments, Micro / Small / Rest
#
# x = log2(earnings / own applicable floor), computed row-by-row against each
# worker's own-quarter floor. No floor averaging, so no smearing at quarters
# that straddle a MW change. Note the ratio is deflator-invariant.
#===============================================================================

cat("[MW-6] Bunching at focal moments...\n")

dist <- read_obj("mw_bind_dist_formal") %>%
  dplyr::filter(!sparse) %>%
  dplyr::rename(ratio = dplyr::all_of(RATIO_VAR)) %>%
  dplyr::filter(is.finite(ratio), ratio > -3, ratio < 4) %>%
  # Re-normalise after trimming so each density integrates to 1.
  dplyr::group_by(focal_qtr, bunch_group) %>%
  dplyr::mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
  dplyr::ungroup()

x_breaks <- c(-2, -1, 0, 1, 2, 3)
x_labels <- c("\u00bc\u00d7", "\u00bd\u00d7", "= floor", "2\u00d7", "4\u00d7", "8\u00d7")

MOMENT_NOTE <- paste(
  "Moments: 2019Q4 pre-COVID; 2021Q2 immediately before the Micro tier;",
  "2023Q1 after the Micro tier and before the 2023Q2 increase; 2025Q4 latest.",
  "Each moment pools only quarters sharing its nominal floor.")

fig_MW6_kde <- ggplot(dist,
                      aes(x = ratio, weight = w_norm, colour = bunch_group)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "grey92", alpha = 0.7) +
  geom_vline(xintercept = 0, colour = "grey25", linewidth = 0.8) +
  stat_density(aes(linetype = bunch_group), geom = "line",
               bw = 0.15 / log(2), linewidth = 1.0, alpha = 0.9) +
  facet_wrap(~focal_qtr, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = BUNCH_COLORS, name = "Firm size group") +
  scale_linetype_manual(values = c("Micro" = "solid", "Small" = "dashed",
                                   "Rest" = "dotdash"),
                        name = "Firm size group") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  labs(
    title    = "Earnings Distribution Relative to the Applicable Minimum Wage",
    subtitle = sprintf("Formal workers, %s earnings, own-quarter floor. x = 0 is the floor that applied to that worker.",
                       HEADLINE),
    x = "Earnings relative to own applicable floor (log\u2082)", y = "Density",
    caption = paste(
      MOMENT_NOTE,
      "Grey = below the floor. Weights normalised within moment x group.",
      MICRO_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()

save_fig(fig_MW6_kde, "fig_MW6_bunching_kde",
         w = config$fig_defaults$width * 2.0, h = config$fig_defaults$height)

# ── Histogram ────────────────────────────────────────────────────────────────
BIN_W <- 0.25

hist_df <- dist %>%
  dplyr::mutate(bin_mid   = BIN_W * floor(ratio / BIN_W) + BIN_W / 2,
                compliant = bin_mid >= 0) %>%
  dplyr::group_by(focal_qtr, bunch_group, bin_mid, compliant) %>%
  dplyr::summarise(wt = sum(w_norm, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(focal_qtr, bunch_group) %>%
  dplyr::mutate(prop = wt / sum(wt)) %>%
  dplyr::ungroup()

fig_MW6_hist <- ggplot(hist_df, aes(x = bin_mid, y = prop, fill = compliant)) +
  geom_col(width = BIN_W * 0.95) +
  geom_vline(xintercept = 0, colour = "grey25", linewidth = 0.6) +
  facet_grid(bunch_group ~ focal_qtr, scales = "free_y") +
  scale_fill_manual(values = c("FALSE" = "#C45C30", "TRUE" = "#1f78b4"),
                    labels = c("FALSE" = "Below floor", "TRUE" = "At/above floor"),
                    name = NULL) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(-3, 4)) +
  labs(
    title    = "Bunching at the Minimum Wage, Before and After the Micro Reform",
    subtitle = sprintf("Weighted share of formal workers by %s earnings relative to their applicable floor",
                       HEADLINE),
    x = "Earnings relative to own applicable floor (log\u2082)",
    y = "Share of workers",
    caption = paste(
      MOMENT_NOTE,
      sprintf("Bin width %.2f log\u2082 units. Red = below the floor.", BIN_W),
      MICRO_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools() +
  theme(strip.text = element_text(face = "bold", size = 8),
        strip.background = element_rect(fill = "grey93", colour = "grey70"))

save_fig(fig_MW6_hist, "fig_MW6_bunching_hist",
         w = config$fig_defaults$width * 1.9,
         h = config$fig_defaults$height * 1.5)


#===============================================================================
# FIGURE MW-6b: Bite against the incoming micro floor
#
# Share of workers below the Micro floor at each moment. For pre-2021Q3 moments
# the threshold is the real 2021Q3 Micro floor in constant base pesos — the
# counterfactual "incoming" floor — so the Micro bars at 2019Q4 and 2021Q2 are
# the pre-reform bite. Hollow bars mark the counterfactual threshold.
#===============================================================================

cat("[MW-6b] Bite vs the incoming micro floor...\n")

bite <- read_obj("mw_bind_micro_bite") %>%
  dplyr::filter(!sparse) %>%
  dplyr::mutate(focal_qtr = factor(focal_qtr,
                                   levels = config$figures$dist_focal_qtrs))

fig_MW6b <- ggplot(bite,
                   aes(x = focal_qtr, y = bite,
                       fill = bunch_group, alpha = counterfactual)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           colour = "white") +
  geom_text(aes(label = percent(bite, accuracy = 1)),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 2.6, alpha = 1) +
  scale_fill_manual(values = BUNCH_COLORS, name = "Firm size group") +
  scale_alpha_manual(values = c("FALSE" = 1, "TRUE" = 0.55),
                     labels = c("FALSE" = "Micro floor in force",
                                "TRUE"  = "Counterfactual (incoming) floor"),
                     name = "Threshold") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Bite of the Micro Floor at Each Focal Moment",
    subtitle = "Share of formal workers earning below the Micro minimum wage",
    x = NULL, y = "Share below the Micro floor",
    caption = paste(
      sprintf("Before %s the Micro tier did not exist; the threshold is the real %s",
              MICRO_START, MICRO_START),
      "Micro floor expressed in constant base pesos, i.e. the floor that was about to arrive.",
      "Micro bars at the pre-reform moments are the pre-reform bite. Small and Rest",
      "bars show what the same threshold implies for firms it never applied to.",
      MOMENT_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom")

save_fig(fig_MW6b, "fig_MW6b_micro_bite",
         w = config$fig_defaults$width * 1.3, h = config$fig_defaults$height)


#===============================================================================

cat("\n=== 04B complete. Figures ->", save_path, "===\n\n")