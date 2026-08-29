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

source(here::here("Code","R","clean scripts","00_setup.R"))



cat("=== 05B_Plot_Inequality.R ===\n\n")


#===============================================================================
# SHARED HELPERS AND CONSTANTS
#
# event_pos(), covid_rect(), qtr_breaks(), MW_EVENT_QTR, MW_NOTE, SRC,
# DEFLATOR_LABEL all come from fun_plotting.R via 00_setup.R. Do NOT redefine.
#===============================================================================

DIST_FOCAL_YEARS <- c(2016L, 2024L)

# Income concepts — must match 05A's INCOME_CONCEPTS tags
INCOME_CONCEPTS <- list(
  monthly = list(
    tag   = "monthly",
    label = "monthly real earnings",
    axis  = "monthly earnings",
    pop   = "Formal private wage earners, positive real monthly earnings."
  ),
  hourly = list(
    tag   = "hourly",
    label = "hourly real earnings (44h cap)",
    axis  = "hourly earnings",
    pop   = "Formal private wage earners, positive real hourly earnings (44h cap)."
  )
)

in_dir <- config$data_dirs$desc_fig
read_obj <- function(name) readRDS(file.path(in_dir, paste0(name, ".rds")))

save_path <- file.path(config$out_dirs$desc_fig, "Inequality")
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

# COVID-spike handling: compute a y-axis cap so normal-time variation is
# legible. The cap is `mult` x the maximum value OUTSIDE the COVID+recovery
# window (2020Q1-2021Q1). The COVID spike then runs off the top of the panel
# via coord_cartesian (zooms without deleting data).
CAP_EXCLUDE_QTRS <- c("2020Q1", "2020Q2", "2020Q3", "2020Q4", "2021Q1")
COVID_QTRS       <- config$events$covid_qtrs

covid_cap <- function(df, valcol, mult = 2, exclude = CAP_EXCLUDE_QTRS) {
  normal_time <- df[!df$time %in% exclude, valcol, drop = TRUE]
  spike_vals  <- df[df$time %in% COVID_QTRS, valcol, drop = TRUE]
  list(
    cap  = max(normal_time, na.rm = TRUE) * mult,
    peak = if (length(spike_vals)) max(spike_vals, na.rm = TRUE) else NA_real_
  )
}

covid_label_x <- function(qtrs) {
  pos <- which(sort(unique(qtrs)) == "2020Q2")
  if (length(pos)) pos else which(sort(unique(qtrs)) == "2020Q1")
}


#===============================================================================
# FIGURES INEQ-1, INEQ-2, INEQ-3 — looped over income concepts
# Each figure saved with _{tag} suffix (e.g. fig_INEQ1_var_log_monthly.png)
#===============================================================================

for (ic_name in names(INCOME_CONCEPTS)) {
  ic <- INCOME_CONCEPTS[[ic_name]]
  POP_NOTE <- ic$pop
  cat(sprintf("\n--- Headline figures [%s] ---\n", ic$tag))
  
  # ── INEQ-1: Variance of log earnings ───────────────────────────────────────
  cat(sprintf("  [INEQ-1-%s] Variance of log %s...\n", ic$tag, ic$axis))
  
  vlog <- read_obj(paste0("ineq_var_log_", ic$tag)) %>%
    dplyr::filter(!sparse) %>%
    dplyr::mutate(time = as.character(year_quarter))
  qtrs1 <- vlog$time
  cap1 <- covid_cap(vlog, "estimate")
  cap1$cap <- 0.6
  
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
      subtitle = sprintf("Variance of log real %s, formal private wage earners", ic$axis),
      x = NULL, y = sprintf("Variance of log real %s", ic$axis),
      caption = paste("Shaded band: 95% CI. y-axis capped; 2020 COVID spike runs off scale.",
                      POP_NOTE, MW_NOTE, SRC, sep = "\n")
    ) +
    theme_surveytools()
  save_fig(fig_INEQ1, paste0("fig_INEQ1_var_log_", ic$tag))
  
  # ── INEQ-2: Percentile ratios ──────────────────────────────────────────────
  cat(sprintf("  [INEQ-2-%s] Percentile ratios...\n", ic$tag))
  
  ratios <- read_obj(paste0("ineq_pctile_ratios_", ic$tag)) %>%
    dplyr::filter(!sparse) %>%
    dplyr::select(year_quarter, `p90/p10`, `p50/p10`, `p90/p50`) %>%
    tidyr::pivot_longer(-year_quarter, names_to = "ratio", values_to = "value") %>%
    dplyr::mutate(time = as.character(year_quarter),
                  ratio = factor(ratio, levels = c("p90/p10", "p50/p10", "p90/p50")))
  qtrs2 <- ratios$time
  
  fig_INEQ2 <- ggplot(ratios,
                      aes(x = time, y = value, colour = ratio, group = ratio)) +
    covid_rect(qtrs2) +
    geom_vline(xintercept = event_pos(qtrs2), linetype = "dashed",
               colour = "red", linewidth = 0.4) +
    geom_line(linewidth = 0.8) +
    coord_cartesian(ylim = c(0, 5)) +
    scale_colour_manual(
      values = c("p90/p10" = "#1b1b1b", "p50/p10" = "#1f78b4", "p90/p50" = "#e08214"),
      name = "Percentile ratio") +
    scale_x_discrete(breaks = qtr_breaks(qtrs2)) +
    labs(
      title    = "Wage Inequality Has Narrowed Over Time",
      subtitle = sprintf("Percentile ratios of real %s, formal private wage earners", ic$axis),
      x = NULL, y = "Ratio",
      caption = paste(
        "y-axis capped at 5; COVID (2020) spike runs off scale.",
        POP_NOTE, MW_NOTE, SRC, sep = "\n")
    ) +
    theme_surveytools()
  save_fig(fig_INEQ2, paste0("fig_INEQ2_pctile_ratios_", ic$tag))
  
  # ── INEQ-3: Density overlay ────────────────────────────────────────────────
  cat(sprintf("  [INEQ-3-%s] Density overlay...\n", ic$tag))
  
  dens <- read_obj(paste0("ineq_density_extract_", ic$tag)) %>%
    dplyr::mutate(
      year_lab = factor(
        paste0(year, ifelse(year == min(year), " (baseline)", " (recent)")),
        levels = c(paste0(min(year), " (baseline)"),
                   paste0(max(year), " (recent)"))))
  
  if (requireNamespace("matrixStats", quietly = TRUE)) {
    med_lines <- dens %>%
      dplyr::group_by(year_lab) %>%
      dplyr::summarise(
        med = matrixStats::weightedMedian(log_real_earn, w = w_norm, na.rm = TRUE),
        .groups = "drop")
  } else {
    med_lines <- NULL
  }
  
  YEAR_COLS <- c("#9ecae1", "#08519c")
  names(YEAR_COLS) <- levels(dens$year_lab)
  
  fig_INEQ3 <- ggplot(dens,
                      aes(x = log_real_earn, weight = w_norm,
                          colour = year_lab, fill = year_lab)) +
    stat_density(geom = "area", position = "identity",
                 alpha = 0.25, bw = 0.15, colour = NA) +
    stat_density(geom = "line", position = "identity",
                 bw = 0.15, linewidth = 0.9) +
    {if (!is.null(med_lines))
      geom_vline(data = med_lines,
                 aes(xintercept = med, colour = year_lab),
                 linetype = "dashed", linewidth = 0.5, show.legend = FALSE)} +
    scale_colour_manual(values = YEAR_COLS, name = NULL) +
    scale_fill_manual(values = YEAR_COLS, name = NULL) +
    labs(
      title    = "The Wage Distribution Compressed Toward the Middle",
      subtitle = sprintf("Density of log real %s, formal private wage earners", ic$axis),
      x = sprintf("Log real %s (%s DOP)", ic$axis, DEFLATOR_LABEL), y = "Density",
      caption = paste("Weights normalised within year.", POP_NOTE, SRC, sep = "\n")
    ) +
    theme_surveytools(legend_position = "bottom")
  save_fig(fig_INEQ3, paste0("fig_INEQ3_density_overlay_", ic$tag),
           w = config$fig_defaults$width * 1.2)
}

#===============================================================================
# ============================  PARENTE SET  ==================================
# Monthly earnings, all wage earners, Overall / Formal / Informal.
# Matches the established project palette (SERIES_COLORS_3_VAR).
#===============================================================================

# Shared three-series styling
SERIES_COLORS_3 <- c("Overall" = "black", "Formal" = "#1f78b4",
                     "Informal" = "#e08214")
SERIES_LTY_3    <- c("Overall" = "solid", "Formal" = "solid",
                     "Informal" = "longdash")

DECOMP_COLORS <- c("Total" = "black",
                   "Within groups" = "#1f78b4",
                   "Between groups" = "#e08214")
DECOMP_LTY    <- c("Total" = "solid",
                   "Within groups" = "solid",
                   "Between groups" = "longdash")

for (ic_name in names(INCOME_CONCEPTS)) {
  ic <- INCOME_CONCEPTS[[ic_name]]
  POP_NOTE_M <- sprintf("All wage earners (salaried private + public), positive real %s.", ic$axis)
  cat(sprintf("\n--- Parente figures [%s] ---\n", ic$tag))
  
  # ── INEQ-4: Variance of log by formality (two-panel patchwork) ─────────────
  cat(sprintf("  [INEQ-4-%s] Var(log) by formality...\n", ic$tag))
  
  vlogf <- read_obj(paste0("ineq_var_log_formality_", ic$tag)) %>%
    dplyr::filter(!sparse) %>%
    dplyr::mutate(time = as.character(year_quarter),
                  group = factor(group, levels = names(SERIES_COLORS_3)))
  qtrs4 <- vlogf$time
  MAIN_CAP4 <- 1.0
  
  build_INEQ4 <- function() {
    ggplot(vlogf, aes(x = time, y = estimate,
                      colour = group, linetype = group, group = group)) +
      covid_rect(qtrs4) +
      geom_vline(xintercept = event_pos(qtrs4), linetype = "dashed",
                 colour = "red", linewidth = 0.4) +
      geom_line(linewidth = 0.8) +
      scale_colour_manual(values = SERIES_COLORS_3, name = NULL) +
      scale_linetype_manual(values = SERIES_LTY_3, name = NULL) +
      scale_x_discrete(breaks = qtr_breaks(qtrs4))
  }
  
  fig_INEQ4_capped <- build_INEQ4() +
    coord_cartesian(ylim = c(0, MAIN_CAP4)) +
    labs(subtitle = "Normal times (y-axis capped at 1.0)",
         x = NULL, y = sprintf("Variance of log %s", ic$axis)) +
    theme_surveytools(legend_position = "bottom")
  
  fig_INEQ4_full <- build_INEQ4() +
    labs(subtitle = "Full range (incl. COVID spike)",
         x = NULL, y = NULL) +
    theme_surveytools(legend_position = "none")
  
  fig_INEQ4 <- (fig_INEQ4_capped | fig_INEQ4_full) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title    = sprintf("Variance of Log Earnings by Formality (%s)", ic$tag),
      subtitle = sprintf("Real %s, wage earners (Overall, Formal, Informal)", ic$axis),
      caption = paste(
        "Replicates Parente (2024) Fig 1 for the Dominican Republic.",
        POP_NOTE_M, MW_NOTE, SRC, sep = "\n"),
      theme = theme(legend.position = "bottom",
                    plot.caption = element_text(hjust = 0, size = 8, colour = "grey40")))
  save_fig(fig_INEQ4, paste0("fig_INEQ4_var_log_formality_", ic$tag),
           w = config$fig_defaults$width * 1.8)
  
  # ── INEQ-5: Percentile ratios by formality ─────────────────────────────────
  cat(sprintf("  [INEQ-5-%s] Percentile ratios by formality...\n", ic$tag))
  
  ratiosf <- read_obj(paste0("ineq_pctile_ratios_formality_", ic$tag)) %>%
    dplyr::filter(!sparse) %>%
    dplyr::select(year_quarter, group, `p90/p10`, `p50/p10`, `p90/p50`) %>%
    tidyr::pivot_longer(c(`p90/p10`, `p50/p10`, `p90/p50`),
                        names_to = "ratio", values_to = "value") %>%
    dplyr::mutate(time = as.character(year_quarter),
                  ratio = factor(ratio, levels = c("p90/p10", "p50/p10", "p90/p50")),
                  group = factor(group, levels = names(SERIES_COLORS_3)))
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
      title    = sprintf("Percentile Ratios by Formality (%s)", ic$tag),
      subtitle = sprintf("Real %s, wage earners", ic$axis),
      x = NULL, y = "Ratio",
      caption = paste(POP_NOTE_M, MW_NOTE, SRC, sep = "\n")
    ) +
    theme_surveytools() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))
  save_fig(fig_INEQ5, paste0("fig_INEQ5_pctile_ratios_formality_", ic$tag),
           w = config$fig_defaults$width * 1.7)
  
  # ── INEQ-6: Density overlay by formality ───────────────────────────────────
  cat(sprintf("  [INEQ-6-%s] Density overlay by formality...\n", ic$tag))
  
  densf <- read_obj(paste0("ineq_density_formality_", ic$tag)) %>%
    dplyr::mutate(
      year_lab = factor(
        paste0(year, ifelse(year == min(year), " (baseline)", " (recent)")),
        levels = c(paste0(min(year), " (baseline)"),
                   paste0(max(year), " (recent)"))),
      Employment_Status = factor(Employment_Status, levels = c("Formal", "Informal")))
  
  fig_INEQ6 <- ggplot(densf,
                      aes(x = log_real_earn, weight = w_norm,
                          colour = year_lab, fill = year_lab)) +
    stat_density(geom = "area", position = "identity",
                 alpha = 0.25, bw = 0.15, colour = NA) +
    stat_density(geom = "line", position = "identity",
                 bw = 0.15, linewidth = 0.9) +
    facet_wrap(~Employment_Status, nrow = 1) +
    scale_colour_manual(values = c("#9ecae1", "#08519c"), name = NULL) +
    scale_fill_manual(values = c("#9ecae1", "#08519c"), name = NULL) +
    labs(
      title    = sprintf("Earnings Distribution by Formality, 2016 vs 2024 (%s)", ic$tag),
      subtitle = sprintf("Density of log real %s, wage earners", ic$axis),
      x = sprintf("Log real %s (%s DOP)", ic$axis, DEFLATOR_LABEL), y = "Density",
      caption = paste("Weights normalised within year x formality.",
                      POP_NOTE_M, SRC, sep = "\n")
    ) +
    theme_surveytools(legend_position = "bottom")
  save_fig(fig_INEQ6, paste0("fig_INEQ6_density_formality_", ic$tag),
           w = config$fig_defaults$width * 1.5)
  
  # ── INEQ-7: Variance decomposition ─────────────────────────────────────────
  cat(sprintf("  [INEQ-7-%s] Variance decomposition...\n", ic$tag))
  
  decomp <- read_obj(paste0("ineq_variance_decomp_", ic$tag)) %>%
    dplyr::filter(!sparse) %>%
    dplyr::mutate(time = as.character(year_quarter))
  
  decomp_long <- decomp %>%
    dplyr::select(year_quarter, time, partition, total, within, between) %>%
    tidyr::pivot_longer(c(total, within, between),
                        names_to = "component", values_to = "value") %>%
    dplyr::mutate(
      component = factor(component,
                         levels = c("total", "within", "between"),
                         labels = c("Total", "Within groups", "Between groups")))
  qtrs7 <- sort(unique(decomp$time))
  DECOMP_CAP <- 0.7
  
  fig_INEQ7 <- ggplot(decomp_long,
                      aes(x = time, y = value,
                          colour = component, linetype = component,
                          group = component)) +
    covid_rect(qtrs7) +
    geom_vline(xintercept = event_pos(qtrs7), linetype = "dashed",
               colour = "red", linewidth = 0.4) +
    geom_line(linewidth = 0.7) +
    coord_cartesian(ylim = c(0, DECOMP_CAP)) +
    facet_wrap(~partition, nrow = 1) +
    scale_colour_manual(values = DECOMP_COLORS, name = NULL) +
    scale_linetype_manual(values = DECOMP_LTY, name = NULL) +
    scale_x_discrete(breaks = qtr_breaks(qtrs7)) +
    labs(
      title    = sprintf("Variance Decomposition of Log Earnings (%s)", ic$tag),
      subtitle = sprintf("Between-group and within-group components, real %s", ic$axis),
      x = NULL, y = "Variance of log earnings",
      caption = paste(
        "Total = within + between, by construction.",
        sprintf("y-axis capped at %.1f; COVID (2020) spike runs off scale.", DECOMP_CAP),
        POP_NOTE_M, MW_NOTE, SRC, sep = "\n")
    ) +
    theme_surveytools(legend_position = "bottom") +
    theme(strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey93", colour = "grey70"))
  save_fig(fig_INEQ7, paste0("fig_INEQ7_variance_decomp_", ic$tag),
           w = config$fig_defaults$width * 1.6)
}


cat("\n=== 05B_Plot_Inequality.R complete ===\n")
cat("Figures saved to:", save_path, "\n\n")