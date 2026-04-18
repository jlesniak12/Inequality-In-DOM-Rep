#===============================================================================
#
# Script: 04a_Figures_Context.R
#
# Scope:  Full-economy context figures for the presentation and paper.
#         Loads pre-computed RDS objects from 03_Compute_Descriptive_Stats
#         and Min_Wage.rds. No survey computation here.
#
# Follows project figure conventions:
#   - theme_surveytools() from 00_functions_plotting.R
#   - config$fig_defaults for width/height/dpi/format
#   - year_quarter character x-axis with 90-degree rotated labels
#   - Saves via ggsave to config$out_subdirs$inequality folder
#   - All-black lines with varying linetypes (Parente/academic style)
#   - Vertical dashed lines at min wage event quarters
#
# Figures:
#   fig_01_minwage           Real min wage levels by firm size tier
#   fig_02_kaitz             Kaitz ratio (min wage / median formal wage) by tier
#   fig_03_variance          Var(log wage) — Overall, Formal, Informal, Self-emp
#   fig_04_variance_decomp   Within/between decomposition
#   fig_05_percentile_ratios log(p90/p10) and log(p50/p10)
#   fig_06_emp_shares        Informal share + self-employed share
#
# Reads:
#   Min_Wage.rds
#   desc_variance_by_status.rds
#   desc_variance_overall.rds
#   desc_variance_decomp.rds
#   desc_percentile_ratios.rds
#   desc_employment_shares.rds
#   desc_selfemployed_variance.rds
#
#===============================================================================

source("Code/R/00_setup.R")
library(patchwork)
library(scales)
 
 
#===============================================================================
# STEP 1. Load Data
#===============================================================================
 
pd  <- config$paths$processed_data
 
min_wage           <- readRDS(file.path(pd, "Min_Wage.rds"))
formal_median_tier <- readRDS(file.path(pd, "desc_formal_median_by_tier.rds"))
var_status         <- readRDS(file.path(pd, "desc_variance_by_status.rds"))
var_overall        <- readRDS(file.path(pd, "desc_variance_overall.rds"))
var_decomp    <- readRDS(file.path(pd, "desc_variance_decomp.rds"))
pct_ratios    <- readRDS(file.path(pd, "desc_percentile_ratios.rds"))
emp_shares    <- readRDS(file.path(pd, "desc_employment_shares.rds"))
var_selfempl  <- readRDS(file.path(pd, "desc_selfemployed_variance.rds"))
 
 
#===============================================================================
# STEP 2. Shared Helpers
#===============================================================================
 
# Min wage event quarter labels — used to draw reference lines
# NOTE: 2019 event is 2019Q3 (not Q2) — nominal wage did not change in 2019Q2,
# the increase was implemented in 2019Q3. Verify against Min_Wage.rds if unsure.
MW_EVENT_QTR <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")
 
# Add vertical event lines — works with character x-axis
mw_events_layer <- function(data_with_time_col) {
  all_qtrs  <- sort(unique(data_with_time_col$time))
  event_pos <- which(all_qtrs %in% MW_EVENT_QTR)
  if (length(event_pos) == 0) return(NULL)
  geom_vline(xintercept = event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4)
}
 
# Source/note strings
SRC <- "Sources: ENCFT 2014Q3-2025Q2; Central Bank of Dominican Republic."
MW_NOTE <- paste(
  "Red dashed vertical lines: min wage announcement quarters",
  "(2017Q2, 2019Q3, 2021Q3, 2023Q2).",
  "Grey shading: 2020Q1-2020Q4 (COVID-19 disruption)."
)
 
# Consistent save helper using project config
save_path <- file.path(
  config$paths$outputs,
  config$output_stage,
  config$out_subdirs$inequality
)
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
 
save_fig <- function(p, name) {
  ggsave(
    filename = file.path(save_path, paste0(name, ".", config$fig_defaults$format)),
    plot     = p,
    width    = config$fig_defaults$width,
    height   = config$fig_defaults$height,
    dpi      = config$fig_defaults$dpi
  )
  message("Saved: ", name)
}
 
# ── Color palettes (all solid lines; red reserved for event lines) ──────────────
 
# Tier palette: 4 distinct colors, no red
TIER_COLORS <- c(
  "Micro"  = "#1b7837",   # dark green
  "Small"  = "#762a83",   # purple
  "Medium" = "#e08214",   # orange
  "Large"  = "#1f78b4"    # blue
)
 
# Multi-series palette for non-tier figures: distinct, no red
# Overall/Formal/Informal/Self-employed
SERIES_COLORS_4 <- c(
  "Overall"       = "black",
  "Formal"        = "#1f78b4",   # blue
  "Informal"      = "#e08214",   # orange
  "Self-employed" = "#762a83"    # purple
)
 
SERIES_COLORS_3 <- c(
  "Overall" = "black",
  "Within"  = "#1f78b4",   # blue
  "Between" = "#e08214"    # orange
)
 
SERIES_COLORS_2_RATIO <- c(
  "log(p90/p10)" = "black",
  "log(p50/p10)" = "#1f78b4",
  "log(p90/p50)" = "#e08214"
)
 
SERIES_COLORS_2_EMP <- c(
  "Informal"      = "black",
  "Self-employed" = "#1f78b4"   # blue
)
 
# ── Covid shading helper ─────────────────────────────────────────────────────────
# Adds a light grey rectangle covering 2020Q1-2020Q4.
# Drawn BEFORE geom_line() so it sits behind the data series.
# xmin/xmax are integer positions of 2020Q1 and 2020Q4 on the character x-axis.
covid_rect <- function(all_qtrs) {
  xmin <- which(all_qtrs == "2020Q1")
  xmax <- which(all_qtrs == "2020Q4")
  if (length(xmin) == 0 || length(xmax) == 0) return(NULL)
  annotate(
    "rect",
    xmin  = xmin - 0.5,
    xmax  = xmax + 0.5,
    ymin  = -Inf,
    ymax  = Inf,
    fill  = "grey85",
    alpha = 0.6
  )
}
 


#===============================================================================
# FIGURE 1: Real Minimum Wage Levels by Firm Size Tier
#===============================================================================

mw_data <- min_wage %>%
  filter(Wage_group %in% c("Micro", "Small", "Medium", "Large")) %>%
  mutate(
    time       = paste0(year, "Q", quarter),
    Wage_group = factor(Wage_group, levels = c("Micro", "Small", "Medium", "Large"))
  ) %>%
  arrange(Wage_group, time)

# Filter to quarters present in the survey period
survey_qtrs <- sort(unique(var_overall$time))
mw_data <- mw_data %>% filter(time %in% survey_qtrs)

# Event positions for this data
mw_event_pos <- which(sort(unique(mw_data$time)) %in% MW_EVENT_QTR)

fig_01 <- ggplot(mw_data,
                 aes(x = time, y = real_minwage_harmonized,
                     color = Wage_group, linetype = Wage_group, group = Wage_group)) +
  covid_rect(sort(unique(mw_data$time))) +
  geom_vline(xintercept = mw_event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = TIER_COLORS) +
  scale_linetype_manual(values = setNames(rep("solid", length(TIER_COLORS)), names(TIER_COLORS)))  +
  labs(
    title    = "Real Minimum Wage by Firm Size Tier",
    subtitle = "CPI Deflated Dominican Pesos",
    y        = "Real min wage (real DOP (CPI-deflated)",
    caption  = paste(MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()


#===============================================================================
# FIGURE 2: Log Kaitz Index by Firm Size Tier
#
# Kaitz = log(real min wage / median formal wage within that tier).
# This is the canonical definition from Lee (1999), Autor et al. (2016),
# Parente (2024). A higher value means the floor is more binding.
# Denominator is the formal sector median within each tier — computed in
# 03_Compute_Descriptive_Stats.R from all formal salaried workers with
# known firm size. No regression sample restriction needed.
#===============================================================================

kaitz_data <- mw_data %>%
  left_join(
    formal_median_tier %>% rename(time = time),
    by = c("time", "Wage_group")
  ) %>%
  mutate(
    log_kaitz = log(real_minwage_harmonized) - log(p50_formal)
  ) %>%
  filter(!is.na(log_kaitz))

fig_02 <- ggplot(kaitz_data,
                 aes(x = time, y = log_kaitz,
                     color = Wage_group, group = Wage_group)) +
  covid_rect(sort(unique(kaitz_data$time))) +
  geom_vline(xintercept = mw_event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = TIER_COLORS) +
  scale_linetype_manual(values = setNames(rep("solid", length(TIER_COLORS)), names(TIER_COLORS))) +
  labs(
    title    = "Log Kaitz Index by Firm Size Tier",
    subtitle = "log(real min wage) - log(median formal wage within tier)",
    y        = "Log Kaitz Index",
    caption  = paste(
      "Kaitz = log(tier min wage / median formal salary within that tier).",
      "Positive values = min wage exceeds median; more negative = less binding.",
      "Denominator: formal salaried workers (private + public) with positive earnings.",
      MW_NOTE, SRC, sep = "\n"
    )
  ) +
  theme_surveytools()


#===============================================================================
  # FIGURE 3 (REVISED): Variance of Log Earnings — Overall, Formal, Informal
  #
  # Self-employed dropped from this figure for two reasons:
  #   1. Their income concept (profit) differs from wages — mixing them
  #      obscures the wage inequality story.
  #   2. The self-employed series starts ~0.92 and dominates the y-axis,
  #      compressing the formal/informal comparison that is the focus here.
  # Self-employed variance remains saved in desc_selfemployed_variance.rds
  # and can be shown separately if needed.
  #
  # NOTE ON THE TWO "OVERALL" SERIES (Figures 3 and 4):
  #   Figure 3 Overall = var(log wage) on all salaried workers (formal +
  #     informal combined), computed directly via design_wage_ineq.
  #   Figure 4 Overall = within + between from the decomposition, computed
  #     on the same design but after splitting by Employment_Status.
  #   Minor sample differences (workers with missing Employment_Status are
  #   excluded from the decomposition but not from Figure 3 Overall) mean
  #   the two black lines are close but not identical.
  #===============================================================================

# Combine overall + formal/informal, excluding self-employed
var_all_series_no_se <- bind_rows(
  desc_variance_overall %>% mutate(group = "Overall"),
  desc_variance_by_status        # already contains only Formal and Informal
) %>%
  mutate(group = factor(group, levels = c("Overall", "Formal", "Informal")))

# Colour palette: subset of SERIES_COLORS_4 (no Self-employed entry)
SERIES_COLORS_3_VAR <- c(
  "Overall"  = "black",
  "Formal"   = "#1f78b4",   # blue
  "Informal" = "#e08214"    # orange
)

var_event_pos <- which(sort(unique(var_all_series_no_se$time)) %in% MW_EVENT_QTR)

fig_03 <- ggplot(var_all_series_no_se,
                 aes(x = time, y = variance,
                     color = group, linetype = group, group = group)) +
  covid_rect(sort(unique(var_all_series_no_se$time))) +
  geom_vline(xintercept = var_event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = SERIES_COLORS_3_VAR, name = "group") +
  scale_linetype_manual(
    values = c("Overall"  = "solid",
               "Formal"   = "solid",
               "Informal" = "longdash"),
    name = "group"
  ) +
  labs(
    title    = "Variance of Log Earnings",
    subtitle = paste0(
      "Salaried workers (private + public employees) with positive earnings. "
    ),
    y       = "Variance of log earnings",
    caption = paste(MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()



#===============================================================================
# FIGURE 3b. Variance of Log Total Labor Income — Formal vs Informal
#
# Companion to Figure 3a (variance of log salary income, wage earners only).
# Uses total primary labor income (salary + independent) so that all informal
# workers with any positive earnings are included — not just the ~40% who
# receive a salary wage.
#
# READING THE TWO FIGURES TOGETHER:
#   Figure 3a (salary):  formal vs informal among wage earners only.
#                        Clean minimum wage comparison — salary is what
#                        the minimum wage governs.
#   Figure 3b (total):   formal vs informal among all earners.
#                        Fuller picture of earnings inequality including
#                        self-employment income in the informal sector.
#
#   If informal variance is HIGHER in 3b than 3a, it means self-employment
#   income adds inequality within the informal sector on top of wage
#   inequality — i.e. there is a wide spread between low-earning own-account
#   workers and higher-earning informal entrepreneurs.
#
#   If the formal series looks similar across both figures, it confirms that
#   formal workers earn primarily through wages (as expected) and independent
#   income is a minor addition.
#
# Reads (produced by Step 10 of 03_Compute_Descriptive_Stats.R):
#   desc_variance_total_by_status.rds
#   desc_variance_total_overall.rds
#
#===============================================================================

# ── Load data ----------------------------------------------------------------
var_total_status  <- readRDS(file.path(pd,
                                       "desc_variance_total_by_status.rds"))
var_total_overall <- readRDS(file.path(pd,
                                       "desc_variance_total_overall.rds"))

# ── Combine series -----------------------------------------------------------
var_total_all <- bind_rows(
  var_total_overall,                     # already has group = "Overall"
  var_total_status                       # Formal and Informal
) %>%
  mutate(group = factor(group, levels = c("Overall", "Formal", "Informal")))

# ── Colour and linetype: same palette as Fig 3a for easy comparison ----------
# Reuse SERIES_COLORS_3_VAR defined in the Fig 3a block above
var_total_event_pos <- which(
  sort(unique(var_total_all$time)) %in% MW_EVENT_QTR
)

fig_03b <- ggplot(var_total_all,
                  aes(x = time, y = variance,
                      color = group, linetype = group, group = group)) +
  covid_rect(sort(unique(var_total_all$time))) +
  geom_vline(xintercept = var_total_event_pos,
             linetype = "dashed", color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = SERIES_COLORS_3_VAR, name = "group") +
  scale_linetype_manual(
    values = c("Overall"  = "solid",
               "Formal"   = "solid",
               "Informal" = "longdash"),
    name = "group"
  ) +
  labs(
    title    = "Variance of Log Total Labor Income",
    subtitle = paste0(
      "All employed workers with positive total primary income ",
      "(salary + independent)."
    ),
    y       = "Variance of log total labor income",
    caption = paste(
      "Total primary labor income = real_salary_income_primary + real_independent_income_primary (real DOP (CPI-deflated).",
      "All employed (OCUPADO == 1) with positive total income and known formality status.",
      "Unlike Figure 3a, informal workers with zero salary but positive independent income are included.",
      "Compare with Figure 3a (salary income only) to isolate the contribution of self-employment income.",
      MW_NOTE, SRC,
      sep = "\n"
    )
  ) +
  theme_surveytools()


# ── Combined panel: 3a and 3b side by side -----------------------------------
# Allows direct visual comparison of how the informal series changes
# when self-employment income is included

fig_03_combined <- fig_03 | fig_03b +
  plot_annotation(
    title    = "Variance of Log Earnings: Salary vs Total Labor Income",
    subtitle = paste0(
      "Left: salary income (wage earners only — minimum wage sample).  ",
      "Right: total income (all earners including self-employed informal workers)."
    ),
    theme = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, colour = "grey30")
    )
  )


# ── Save --------------------------------------------------------------------
save_fig(fig_03b, "fig_03b_variance_total_income")

ggsave(
  filename = file.path(save_path,
                       paste0("fig_03_combined_salary_total.",
                              config$fig_defaults$format)),
  plot   = fig_03_combined,
  width  = config$fig_defaults$width * 2,
  height = config$fig_defaults$height,
  dpi    = config$fig_defaults$dpi
)
message("Saved: fig_03_combined_salary_total")

#===============================================================================
# FIGURE 4: Variance Decomposition — Within and Between
#===============================================================================

decomp_long <- var_decomp %>%
  select(time, total_var, within, between) %>%
  pivot_longer(c("total_var", "within", "between"),
               names_to = "component", values_to = "value") %>%
  mutate(
    component = recode(component,
                       "total_var" = "Overall",
                       "within"    = "Within",
                       "between"   = "Between"),
    component = factor(component, levels = c("Overall", "Within", "Between"))
  )

decomp_event_pos <- which(sort(unique(decomp_long$time)) %in% MW_EVENT_QTR)

fig_04 <- ggplot(decomp_long,
                 aes(x = time, y = value,
                     color = component, linetype = component, group = component)) +
  covid_rect(sort(unique(kaitz_data$time))) +
  geom_vline(xintercept = decomp_event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = SERIES_COLORS_3) +
  scale_linetype_manual(values = setNames(rep("solid", length(SERIES_COLORS_3)), names(SERIES_COLORS_3))) +
  labs(
    title    = "Decomposition of Variance of Log Earnings",
    subtitle = "V = Within (formal/informal) + Between (formal/informal) components",
    y        = "Variance of log earnings",
    caption  = paste(
      "Within = sum_j s_jt * Var_j(log w).  Between = sum_j s_jt * (mean_j - mean)^2.",
      "j in {Formal, Informal}. Salaried workers with positive earnings.",
      MW_NOTE, SRC, sep = "\n"
    )
  ) +
  theme_surveytools()


#===============================================================================
# FIGURE 4b. Decomposition of Variance of Log Total Labor Income
#
# Companion to Figure 4 (decomposition of log salary variance).
# Shows within/between decomposition for total primary labor income
# (salary + independent), covering all employed workers with positive
# total income including self-employed informal workers.
#
# READING FIGURES 4 AND 4b TOGETHER:
#
#   Figure 4  (salary):  within = weighted avg of formal/informal SALARY
#                        variance among wage earners.
#                        between = formal/informal SALARY GAP contribution.
#
#   Figure 4b (total):   within = weighted avg of formal/informal TOTAL
#                        income variance among all earners.
#                        between = formal/informal TOTAL EARNINGS GAP
#                        contribution.
#
#   Key questions the comparison answers:
#   - Does including self-employment income increase the within component
#     (suggesting self-employment adds inequality within the informal sector)?
#   - Does the between component change (suggesting the formal/informal
#     earnings gap is different when self-employment is counted)?
#   - Does the overall story — most inequality is within-sector, not
#     between sectors — hold when using a broader income concept?
#
# Reads (produced by Step 11 of 03_Compute_Descriptive_Stats.R):
#   desc_variance_total_decomp.rds
#
#===============================================================================

# ── Load data ----------------------------------------------------------------
var_total_decomp <- readRDS(
  file.path(pd, "desc_variance_total_decomp.rds")
)

# ── Reshape to long for plotting --------------------------------------------
decomp_total_long <- var_total_decomp %>%
  select(time, total_var, within, between) %>%
  tidyr::pivot_longer(
    cols      = c("total_var", "within", "between"),
    names_to  = "component",
    values_to = "value"
  ) %>%
  mutate(
    component = dplyr::recode(component,
                              "total_var" = "Overall",
                              "within"    = "Within",
                              "between"   = "Between"),
    component = factor(component, levels = c("Overall", "Within", "Between"))
  )

decomp_total_event_pos <- which(
  sort(unique(decomp_total_long$time)) %in% MW_EVENT_QTR
)

# ── Figure 4b ---------------------------------------------------------------
fig_04b <- ggplot(decomp_total_long,
                  aes(x = time, y = value,
                      color = component, linetype = component,
                      group = component)) +
  covid_rect(sort(unique(decomp_total_long$time))) +
  geom_vline(xintercept = decomp_total_event_pos,
             linetype = "dashed", color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = SERIES_COLORS_3, name = "component") +
  scale_linetype_manual(
    values = setNames(rep("solid", length(SERIES_COLORS_3)),
                      names(SERIES_COLORS_3)),
    name = "component"
  ) +
  labs(
    title    = "Decomposition of Variance of Log Total Labor Income",
    subtitle = "V = Within (formal/informal) + Between (formal/informal) components",
    y        = "Variance of log total labor income",
    caption  = paste(
      "Within = sum_j s_jt * Var_j(log total income).",
      "Between = sum_j s_jt * (mean_j - mean)^2.",
      "j in {Formal, Informal}.",
      "Total primary labor income = salary + independent income (real DOP (CPI-deflated).",
      "All employed (OCUPADO == 1) with positive total income — includes",
      "self-employed and own-account informal workers excluded from Figure 4.",
      "Compare with Figure 4 (salary income only) to assess contribution of",
      "self-employment income to within- and between-sector inequality.",
      MW_NOTE, SRC,
      sep = "\n"
    )
  ) +
  theme_surveytools()


# ── Combined panel: Fig 4 and Fig 4b side by side ---------------------------
fig_04_combined <- fig_04 | fig_04b +
  plot_annotation(
    title    = "Variance Decomposition: Salary vs Total Labor Income",
    subtitle = paste0(
      "Left: salary income (wage earners only).  ",
      "Right: total labor income (all earners including self-employed).  ",
      "Within = inequality inside each sector. Between = gap between sectors."
    ),
    theme = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, colour = "grey30")
    )
  )


# ── Save --------------------------------------------------------------------
save_fig(fig_04b, "fig_04b_variance_total_decomp")

ggsave(
  filename = file.path(save_path,
                       paste0("fig_04_combined_salary_total.",
                              config$fig_defaults$format)),
  plot   = fig_04_combined,
  width  = config$fig_defaults$width * 2,
  height = config$fig_defaults$height,
  dpi    = config$fig_defaults$dpi
)
message("Saved: fig_04_combined_salary_total")


#===============================================================================
# FIGURE 5: Percentile Ratios — log(p90/p10) and log(p50/p10)
#===============================================================================

pct_long <- pct_ratios %>%
  select(time, log_p90_p10, log_p50_p10, log_p90_p50) %>%
  pivot_longer(c("log_p90_p10", "log_p50_p10", "log_p90_p50"),
               names_to = "ratio", values_to = "value") %>%
  mutate(
    ratio = recode(ratio,
                   "log_p90_p10" = "log(p90/p10)",
                   "log_p50_p10" = "log(p50/p10)",
                   "log_p90_p50" = "log(p90/p50)"),
    ratio = factor(ratio, levels = c("log(p90/p10)", "log(p50/p10)", "log(p90/p50)"))
  )

pct_event_pos <- which(sort(unique(pct_long$time)) %in% MW_EVENT_QTR)

fig_05 <- ggplot(pct_long,
                 aes(x = time, y = value,
                     color = ratio, linetype = ratio, group = ratio)) +
  covid_rect(sort(unique(kaitz_data$time))) +
  geom_vline(xintercept = pct_event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = SERIES_COLORS_2_RATIO) +
  scale_linetype_manual(values = setNames(rep("solid", length(SERIES_COLORS_2_RATIO)), names(SERIES_COLORS_2_RATIO))) +
  labs(
    title    = "Earnings Inequality: Log Percentile Ratios",
    subtitle = "Salaried workers (private + public) with positive earnings",
    y        = "Log Percentile Ratio",
    caption  = paste(MW_NOTE, SRC, sep = "\n")
  ) +
  theme_surveytools()


#===============================================================================
# FIGURE 6: Informal and Self-Employed Shares Over Time
#===============================================================================

shares_long <- emp_shares %>%
  pivot_longer(c("informal_share", "selfempl_share"),
               names_to = "series", values_to = "share") %>%
  mutate(
    series = recode(series,
                    "informal_share" = "Informal",
                    "selfempl_share" = "Self-employed"),
    series = factor(series, levels = c("Informal", "Self-employed"))
  )

shares_event_pos <- which(sort(unique(shares_long$time)) %in% MW_EVENT_QTR)

fig_06 <- ggplot(shares_long,
                 aes(x = time, y = share,
                     color = series, linetype = series, group = series)) +
  covid_rect(sort(unique(kaitz_data$time))) +
  geom_vline(xintercept = shares_event_pos, linetype = "dashed",
             color = "red", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = SERIES_COLORS_2_EMP) +
  scale_linetype_manual(values = setNames(rep("solid", length(SERIES_COLORS_2_EMP)), names(SERIES_COLORS_2_EMP))) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Informality and Self-Employment",
    subtitle = "Share of all employed workers",
    y        = "Share of employed workers",
    caption  = paste(MW_NOTE, SRC, sep = "\n"
    )
  ) +
  theme_surveytools()


#===============================================================================
# STEP 3. Save Individual Figures
#===============================================================================

save_fig(fig_01, "fig_01_minwage_levels")
save_fig(fig_02, "fig_02_kaitz_ratio")
save_fig(fig_03, "fig_03_variance_by_status")
save_fig(fig_04, "fig_04_variance_decomp")
save_fig(fig_05, "fig_05_percentile_ratios")
save_fig(fig_06, "fig_06_emp_shares")


#===============================================================================
# STEP 4. Combined Presentation Layouts
#===============================================================================

# 2x2: the four inequality figures together
fig_combined_ineq <- (fig_03 | fig_06) / (fig_05 | fig_04) +
  plot_annotation(
    title    = "Labor Market Context: Dominican Republic 2014-2025",
    subtitle = "Salaried worker inequality and employment structure",
    theme    = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  filename = file.path(save_path,
                       paste0("fig_combined_context.",
                              config$fig_defaults$format)),
  plot     = fig_combined_ineq,
  width    = config$fig_defaults$width * 2,
  height   = config$fig_defaults$height * 2,
  dpi      = config$fig_defaults$dpi
)
message("Saved: fig_combined_context")

# Min wage context: levels + Kaitz side by side
fig_combined_mw <- (fig_01 | fig_02) +
  plot_annotation(
    title = "The Minimum Wage in Dominican Republic, 2014-2025",
    theme = theme(plot.title = element_text(size = 13, face = "bold"))
  )

ggsave(
  filename = file.path(save_path,
                       paste0("fig_combined_minwage.",
                              config$fig_defaults$format)),
  plot     = fig_combined_mw,
  width    = config$fig_defaults$width * 2,
  height   = config$fig_defaults$height,
  dpi      = config$fig_defaults$dpi
)
message("Saved: fig_combined_minwage")

cat("\n=== 04a_Figures_Context.R complete ===\n")
#===============================================================================
# FIGURES 7 & 8. Wage Distribution and Minimum Wage Bunching
#
# Figure 7 — Formal workers (private sector, formal only):
#   Two-column panel (2016 | 2024).  Four overlaid survey-weighted kernel
#   densities by firm size tier (Micro, Small, Medium, Large).
#   X-axis: log base-2 of (income / own tier minimum wage).
#     x =  0  → earns exactly the minimum wage
#     x =  1  → earns double the minimum wage
#     x = -1  → earns half the minimum wage (non-compliance)
#
# Figure 8 — Informal workers (all, pooled):
#   Two-column panel (2016 | 2024).  Single pooled density per year.
#   Two orange dashed vertical lines mark lower and upper bounds of the
#   formal MW range.  No fill, no individual tier lines.
#
# BUG FIXES vs previous version:
#   1. Local save_fig_dist() defined here with w/h args — avoids inheriting
#      the no-arg version from 04_Descriptive Figures.R when sourced together.
#   2. linetype aesthetic moved inside stat_density() aes() — fixes the
#      "no shared levels" warning from scale_linetype_manual when using
#      stat_density(..., geom = "line") instead of geom_density().
#
#===============================================================================

cat("\n[Figures 7 & 8] Wage distribution bunching figures...\n")

library(patchwork)
library(scales)

# ── Load data -----------------------------------------------------------------
pd <- config$paths$processed_data

dist_formal   <- readRDS(file.path(pd, "desc_wage_dist_formal.rds"))
dist_informal <- readRDS(file.path(pd, "desc_wage_dist_informal.rds"))
mw_avg        <- readRDS(file.path(pd, "desc_mw_annual_avg.rds"))
mw_band       <- readRDS(file.path(pd, "desc_mw_band_informal.rds"))

# ── Local save helper ---------------------------------------------------------
# Defined locally to avoid inheriting the no-w/h version from
# 04_Descriptive Figures.R. Saves to the inequality_minwage output folder.
dist_save_path <- file.path(
  config$paths$outputs,
  config$output_stage,
  config$out_subdirs$inequality_minwage
)
dir.create(dist_save_path, recursive = TRUE, showWarnings = FALSE)

save_fig_dist <- function(p, name,
                          w = config$fig_defaults$width,
                          h = config$fig_defaults$height) {
  ggsave(
    filename = file.path(dist_save_path,
                         paste0(name, ".", config$fig_defaults$format)),
    plot     = p,
    width    = w,
    height   = h,
    dpi      = config$fig_defaults$dpi
  )
  message("Saved: ", name)
}

# ── Constants -----------------------------------------------------------------
# Bandwidth on log2 scale: divide natural-log bw by log(2) to keep the same
# physical smoothing window as bw = 0.15 on the natural log scale.
DIST_BW_LOG2 <- 0.15 / log(2)
DIST_YEARS   <- c(2016L, 2024L)
DIST_TIERS   <- c("Micro", "Small", "Medium", "Large")

TIER_COLORS <- c(
  "Micro"  = "#1b7837",
  "Small"  = "#762a83",
  "Medium" = "#e08214",
  "Large"  = "#1f78b4"
)

SRC <- "Sources: ENCFT 2014Q3-2025Q2; Central Bank of Dominican Republic."

year_labeller <- as_labeller(c(
  "2016" = "2016 (baseline)",
  "2024" = "2024 (recent)"
))


#===============================================================================
# FIGURE 7: Formal Worker Distribution — log2 normalised x-axis
#===============================================================================

cat("  Building Figure 7 (formal, log2 normalised axis)...\n")

# ── Join each worker to their tier x year minimum wage -----------------------
mw_for_join <- mw_avg %>%
  select(year, Wage_group, real_mw) %>%
  mutate(year = as.integer(year))

dist_formal_norm <- dist_formal %>%
  mutate(
    year       = as.integer(as.character(year)),
    Wage_group = factor(Wage_group, levels = DIST_TIERS)
  ) %>%
  left_join(mw_for_join, by = c("year", "Wage_group")) %>%
  mutate(
    log2_ratio = log2(real_total_income_primary / real_mw),
    year       = factor(year, levels = DIST_YEARS)
  ) %>%
  filter(!is.na(log2_ratio), log2_ratio > -3, log2_ratio < 4)

# Re-normalise weights after row trimming
dist_formal_norm <- dist_formal_norm %>%
  group_by(year, Wage_group) %>%
  mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
  ungroup()

# ── 2016 micro note -----------------------------------------------------------
micro_note <- data.frame(
  year  = factor(2016L, levels = DIST_YEARS),
  label = "Note: Micro used Small\nrate pre-2021"
)

# ── x-axis: intuitive doubling labels ----------------------------------------
x_breaks <- c(-2, -1, 0, 1, 2, 3)
x_labels <- c("\u00bc\u00d7 MW", "\u00bd\u00d7 MW", "= MW",
              "2\u00d7 MW",     "4\u00d7 MW",      "8\u00d7 MW")

fig_07 <- ggplot(
  dist_formal_norm,
  aes(x      = log2_ratio,
      colour = Wage_group,
      weight = w_norm)
) +
  
  # ── non-compliance shading ------------------------------------------------
annotate("rect",
         xmin = -Inf, xmax = 0,
         ymin = -Inf, ymax = Inf,
         fill = "grey92", alpha = 0.7) +
  
  # ── minimum wage reference line ------------------------------------------
geom_vline(xintercept = 0,
           colour     = "grey25",
           linewidth  = 0.85,
           linetype   = "solid") +
  
  # ── density curves --------------------------------------------------------
# FIX: linetype aesthetic placed INSIDE stat_density aes(), not inherited
# from the top-level ggplot aes(). This is required when using
# stat_density(..., geom = "line") — the stat does not forward aesthetics
# from the parent mapping the same way geom_density does.
stat_density(
  aes(linetype = Wage_group),
  geom      = "line",
  bw        = DIST_BW_LOG2,
  linewidth = 1.15,
  alpha     = 0.92
) +
  
  # ── zero-line label -------------------------------------------------------
annotate("text",
         x = 0.08, y = Inf,
         label = "Minimum\nwage floor",
         hjust = 0, vjust = 1.5,
         size = 2.7, colour = "grey35") +
  
  # ── 2016 micro note -------------------------------------------------------
geom_text(
  data        = micro_note,
  aes(x = -Inf, y = Inf, label = label),
  hjust = -0.05, vjust = 1.4,
  size = 2.5, colour = "grey45",
  inherit.aes = FALSE
) +
  
  # ── facet by year ---------------------------------------------------------
facet_wrap(~year,
           ncol     = 2,
           labeller = year_labeller,
           scales   = "free_y") +
  
  # ── scales ---------------------------------------------------------------
scale_colour_manual(values = TIER_COLORS, name = "Firm size tier") +
  scale_linetype_manual(
    values = c("Micro"  = "solid",
               "Small"  = "solid",
               "Medium" = "longdash",
               "Large"  = "dotdash"),
    name   = "Firm size tier"
  ) +
  scale_x_continuous(
    name   = "Income relative to tier minimum wage  (log\u2082 scale: +1 = double the minimum wage)",
    breaks = x_breaks,
    labels = x_labels
  ) +
  scale_y_continuous(
    name = "Density (integrates to 1 over area; values >1 reflect tight concentration)"
  ) +
  
  # ── labels ----------------------------------------------------------------
labs(
  title    = "Formal Worker Income Distribution Relative to Minimum Wage",
  subtitle = paste0(
    "Survey-weighted kernel density by firm size tier.  ",
    "x\u00a0=\u00a00 is each tier\u2019s annual-average real minimum wage.  ",
    "Grey = non-compliance zone (income < minimum wage)."
  ),
  caption  = paste(
    "Formal private-sector employees only (Employment_Status == Formal,",
    "Employment_Type == private employee), positive total primary labor income.",
    "Government and Electricity & Water excluded (regression sample).",
    "Income = real salary + real independent income (real DOP (CPI-deflated).",
    "Weights normalised within year x tier. Pre-2021 Micro used Small-firm rate.",
    "Large-firm non-compliance partly reflects self-reported firm size measurement error.",
    SRC,
    sep = "\n"
  )
) +
  
  # ── theme ----------------------------------------------------------------
theme_surveytools(legend_position = "bottom") +
  theme(
    axis.text.x      = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.ticks.x     = element_line(),
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey93", colour = "grey70"),
    legend.key.width = unit(1.8, "cm")
  )

cat("  Figure 7 built.\n")


#===============================================================================
# FIGURE 8: Informal Worker Distribution — clean, two boundary lines only
#===============================================================================

cat("  Building Figure 8 (informal, clean version)...\n")

dist_informal <- dist_informal %>%
  mutate(year = factor(year, levels = DIST_YEARS))

mw_band <- mw_band %>%
  mutate(year = factor(year, levels = DIST_YEARS))

# DOP reference breaks
DOP_REFS <- c(5000, 10000, 20000, 40000, 80000)
log_dops <- log(DOP_REFS)
dop_labs <- paste0(DOP_REFS / 1000, "k")

# Labels for the two boundary lines
mw_band_labels <- mw_band %>%
  tidyr::pivot_longer(cols      = c(log_mw_min, log_mw_max),
                      names_to  = "bound",
                      values_to = "x") %>%
  mutate(
    label = if_else(bound == "log_mw_min",
                    "Lower MW\n(Micro/Small)",
                    "Upper MW\n(Large)")
  )

fig_08 <- ggplot(dist_informal,
                 aes(x = log_income, weight = w_norm)) +
  
  # ── light DOP reference grid ----------------------------------------------
geom_vline(xintercept = log_dops,
           linetype   = "dotted",
           colour     = "grey80",
           linewidth  = 0.3) +
  
  # ── informal density: bold blue line, no fill -----------------------------
stat_density(
  colour    = "#2C5F8A",
  geom = "line",
  linewidth = 1.4,
  bw        = 0.15
) +
  
  # ── formal MW band boundary lines ----------------------------------------
geom_vline(
  data      = mw_band,
  aes(xintercept = log_mw_min),
  colour    = "#e08214",
  linetype  = "dashed",
  linewidth = 0.9,
  inherit.aes = FALSE
) +
  geom_vline(
    data      = mw_band,
    aes(xintercept = log_mw_max),
    colour    = "#e08214",
    linetype  = "dashed",
    linewidth = 0.9,
    inherit.aes = FALSE
  ) +
  
  # ── boundary labels -------------------------------------------------------
geom_text(
  data        = mw_band_labels,
  aes(x = x, y = Inf, label = label),
  hjust       = -0.08,
  vjust       = 1.4,
  size        = 2.6,
  colour      = "#8B4500",
  fontface    = "italic",
  inherit.aes = FALSE
) +
  
  # ── facet by year ---------------------------------------------------------
facet_wrap(~year,
           ncol     = 2,
           labeller = year_labeller,
           scales   = "free_y") +
  
  # ── scales ---------------------------------------------------------------
scale_x_continuous(
  name   = "Log real total labor income (real DOP (CPI-deflated), primary job)",
  breaks = log_dops,
  labels = dop_labs
) +
  scale_y_continuous(name = "Density") +
  
  # ── labels ----------------------------------------------------------------
labs(
  title    = "Informal Worker Income Distribution Relative to Formal Minimum Wages",
  subtitle = paste0(
    "Survey-weighted kernel density, all informal workers (pooled).  ",
    "Orange dashed lines = lower and upper bounds of formal minimum wage range across tiers."
  ),
  caption  = paste(
    "All informal workers with positive total primary labor income (no firm size filter).",
    "Firm size not used to split density: ~35% of informal workers have unknown firm size.",
    "Income = real salary + real independent income (real DOP (CPI-deflated).",
    "Weights normalised within year.",
    "MW lines: annual-average real values (real DOP (CPI-deflated)). Lower = Micro/Small rate; Upper = Large rate.",
    "The lines show where formal workers are legally required to be paid,",
    "not a binding threshold for informal workers. Spillover = mass concentrated near the lines.",
    SRC,
    sep = "\n"
  )
) +
  
  # ── theme ----------------------------------------------------------------
theme_surveytools(legend_position = "none") +
  theme(
    axis.text.x      = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.ticks.x     = element_line(),
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey93", colour = "grey70")
  )

cat("  Figure 8 built.\n")


#===============================================================================
# Save
#===============================================================================

save_fig_dist(fig_07, "fig_07_formal_wage_dist_bunching",
              w = config$fig_defaults$width  * 2,
              h = config$fig_defaults$height * 1.4)

save_fig_dist(fig_08, "fig_08_informal_wage_dist_mw_ref",
              w = config$fig_defaults$width  * 2,
              h = config$fig_defaults$height * 1.4)

# Combined stacked panel
fig_dist_combined <- fig_07 / fig_08 +
  plot_annotation(
    title    = "Income Distributions and Minimum Wage \u2014 Formal vs Informal Sector",
    subtitle = "Dominican Republic 2016 vs 2024 | Real real DOP (CPI-deflated) | Survey-weighted",
    theme    = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, colour = "grey30")
    )
  )

save_fig_dist(fig_dist_combined, "fig_dist_combined",
              w = config$fig_defaults$width  * 2,
              h = config$fig_defaults$height * 3)

cat("\n=== Figures 7 & 8 complete ===\n")

#===============================================================================
# FIGURES 9 & 10. Formal vs Informal Income Distribution — 2016 vs 2024
#
# Both figures show survey-weighted kernel densities of log real income,
# overlaying Formal and Informal workers within each year facet.
# Two-column panel: 2016 (baseline) | 2024 (recent).
#
# WEIGHT CONVENTION — within-group normalisation:
#   Weights are normalised within year × Employment_Status, so each curve
#   integrates to 1 independently of group size. This is the correct choice
#   when the goal is to compare distributional *shape* (location, spread,
#   skewness) between formal and informal workers and across years.
#
#   Using raw weights would make the taller curve simply the larger group,
#   obscuring the shape differences that are the analytical focus here.
#   Group size is instead shown as a % of total employment annotation on
#   each panel so the information is not lost.
#
#   Contrast with Figures 3-6 (time-series variance/share figures) where
#   raw weights are appropriate since the level of each series matters.
#
# Figure 9 — Salary income (real_salary_income_total):
#   Workers with positive salary income only.
#   ~60% of informal workers (zero salary) are excluded — this distribution
#   represents wage-earning informal workers only.
#
# Figure 10 — Total labor income (real_total_income_total):
#   All workers with positive total earnings (salary + independent).
#   Includes the full informal workforce; mixes wages and profit income.
#
#===============================================================================

cat("\n[Figures 9 & 10] Formal vs Informal income distribution...\n")

library(patchwork)
library(scales)

# ── Local save helper --------------------------------------------------------
dist_save_path <- file.path(
  config$paths$outputs,
  config$output_stage,
  config$out_subdirs$inequality
)
dir.create(dist_save_path, recursive = TRUE, showWarnings = FALSE)

save_fig_ineq <- function(p, name,
                          w = config$fig_defaults$width,
                          h = config$fig_defaults$height) {
  ggsave(
    filename = file.path(dist_save_path,
                         paste0(name, ".", config$fig_defaults$format)),
    plot     = p,
    width    = w,
    height   = h,
    dpi      = config$fig_defaults$dpi
  )
  message("Saved: ", name)
}

# ── Constants ----------------------------------------------------------------
FOCAL_YEARS <- c(2016L, 2024L)
DIST_BW     <- 0.2

STATUS_COLORS <- c("Formal" = "#1f78b4", "Informal" = "#e08214")
SRC <- "Sources: ENCFT 2014Q3-2025Q2; Central Bank of Dominican Republic."

DOP_REFS <- c(5000, 10000, 20000, 40000, 80000, 160000)
log_dops <- log(DOP_REFS)
dop_labs <- paste0(DOP_REFS / 1000, "k")

year_labeller <- as_labeller(c(
  "2016" = "2016 (baseline)",
  "2024" = "2024 (recent)"
))

# ── Load microdata -----------------------------------------------------------
pd      <- config$paths$processed_data
full_df <- readRDS(file.path(pd, "Full_ENCFT_clean.rds"))

base_df <- full_df %>%
  filter(
    OCUPADO           == 1,
    !is.na(Employment_Status),
    Employment_Status %in% c("Formal", "Informal"),
    year              %in% FOCAL_YEARS
  ) %>%
  mutate(
    year              = factor(year,              levels = FOCAL_YEARS),
    Employment_Status = factor(Employment_Status, levels = c("Formal", "Informal"))
  )

rm(full_df); gc()

# ── Employment share annotation data ----------------------------------------
# Weighted share of Formal vs Informal within each focal year.
# Used to annotate each density curve so group size is visible despite
# within-group weight normalisation.
emp_shares_ann <- base_df %>%
  group_by(year, Employment_Status) %>%
  summarise(wt = sum(FACTOR_EXPANSION, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = wt / sum(wt)) %>%
  ungroup() %>%
  mutate(
    share_label = paste0(Employment_Status, "\n(",
                         round(share * 100, 1), "% of employed)")
  )


#===============================================================================
# Helper: build one distribution figure given an income variable
#===============================================================================

build_dist_fig <- function(df,
                           income_var,    # column name (unquoted string)
                           emp_shares,    # emp_shares_ann data frame
                           informal_note, # TRUE/FALSE: add exclusion note
                           title,
                           subtitle_income_desc,
                           caption_lines) {
  
  # Filter to positive income and log-transform
  plot_df <- df %>%
    filter(.data[[income_var]] > 0) %>%
    mutate(log_income = log(.data[[income_var]]))
  
  # Normalise weights within year x group — each curve integrates to 1
  plot_df <- plot_df %>%
    group_by(year, Employment_Status) %>%
    mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
    ungroup()
  
  # Unweighted sample counts
  n_ann <- plot_df %>%
    group_by(year, Employment_Status) %>%
    summarise(n_obs = n(), .groups = "drop")
  
  # Combine n and share into a single per-panel label
  label_df <- emp_shares %>%
    left_join(n_ann, by = c("year", "Employment_Status")) %>%
    group_by(year) %>%
    summarise(
      label = paste(
        paste0(Employment_Status, ": ",
               round(share * 100, 1), "% of employed",
               "  (n=", formatC(n_obs, format = "d", big.mark = ","), ")"),
        collapse = "\n"
      ),
      .groups = "drop"
    )
  
  # Informal exclusion note — only when requested, placed in 2016 panel
  note_df <- if (informal_note) {
    data.frame(
      year  = factor(FOCAL_YEARS[1], levels = FOCAL_YEARS),
      label = "~60% of informal workers\nhave zero salary income\nand are excluded here"
    )
  } else NULL
  
  p <- ggplot(
    plot_df,
    aes(x = log_income, colour = Employment_Status, weight = w_norm)
  ) +
    
    # ── DOP reference grid --------------------------------------------------
  geom_vline(xintercept = log_dops,
             linetype = "dotted", colour = "grey80", linewidth = 0.3) +
    
    # ── density curves (linetype mapped inside stat_density) ----------------
  stat_density(
    aes(linetype = Employment_Status),
    geom      = "line",
    bw        = DIST_BW,
    linewidth = 1.1
  ) +
    
    # ── n + share annotation (top-left of each facet) -----------------------
  geom_text(
    data        = label_df,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.04, vjust = 1.35,
    size = 2.35, colour = "grey35",
    inherit.aes = FALSE
  ) +
    
    # ── facets --------------------------------------------------------------
  facet_wrap(~year, ncol = 2, labeller = year_labeller, scales = "free_y") +
    
    # ── scales --------------------------------------------------------------
  scale_colour_manual(values = STATUS_COLORS, name = NULL) +
    scale_linetype_manual(
      values = c("Formal" = "solid", "Informal" = "longdash"),
      name   = NULL
    ) +
    scale_x_continuous(
      name   = paste0("Log real ", subtitle_income_desc, " (real DOP (CPI-deflated))"),
      breaks = log_dops,
      labels = dop_labs
    ) +
    scale_y_continuous(name = "Density") +
    
    # ── labels --------------------------------------------------------------
  labs(
    title    = title,
    subtitle = paste0(
      "Survey-weighted kernel density.  ",
      "Each curve normalised to integrate to 1 (shape comparison, not size).  ",
      "Full-year pooled (Q1\u2013Q4).  ",
      "Bandwidth = ", DIST_BW, "."
    ),
    caption = paste(caption_lines, collapse = "\n")
  ) +
    
    # ── theme ---------------------------------------------------------------
  theme_surveytools(legend_position = "bottom") +
    theme(
      axis.text.x      = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.ticks.x     = element_line(),
      strip.text       = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "grey93", colour = "grey70"),
      legend.key.width = unit(1.5, "cm")
    )
  
  # Add informal exclusion note if requested
  if (!is.null(note_df)) {
    p <- p +
      geom_text(
        data        = note_df,
        aes(x = Inf, y = Inf, label = label),
        hjust = 1.05, vjust = 1.35,
        size = 2.2, colour = "#8B4500", fontface = "italic",
        inherit.aes = FALSE
      )
  }
  
  p
}


#===============================================================================
# Figure 9: Salary income
#===============================================================================

cat("  Building Figure 9 (salary income)...\n")

fig_09 <- build_dist_fig(
  df                   = base_df,
  income_var           = "real_salary_income_total",
  emp_shares           = emp_shares_ann,
  informal_note        = TRUE,
  title                = "Formal vs Informal Wage Distribution \u2014 2016 and 2024",
  subtitle_income_desc = "salary income \u2014 primary + secondary jobs",
  caption_lines        = c(
    "Employed workers (OCUPADO == 1) with positive salary income",
    "(real_salary_income_total = primary + secondary salary, real DOP (CPI-deflated)).",
    "~60% of informal workers have zero salary income and are excluded;",
    "the informal distribution represents wage-earning informal workers only.",
    "Curves normalised to integrate to 1; % of employed shown in annotation.",
    "See Figure 10 for total labor income including all informal workers.",
    SRC
  )
)

cat("  Figure 9 built.\n")


#===============================================================================
# Figure 10: Total labor income
#===============================================================================

cat("  Building Figure 10 (total income)...\n")

fig_10 <- build_dist_fig(
  df                   = base_df,
  income_var           = "real_total_income_total",
  emp_shares           = emp_shares_ann,
  informal_note        = FALSE,
  title                = "Formal vs Informal Total Income Distribution \u2014 2016 and 2024",
  subtitle_income_desc = "total labor income \u2014 all jobs (salary + independent)",
  caption_lines        = c(
    "Employed workers (OCUPADO == 1) with positive total labor income",
    "(real_total_income_total = salary + independent, all jobs, real DOP (CPI-deflated)).",
    "All informal workers with any positive earnings are included.",
    "Informal distribution mixes wage and self-employment profit income.",
    "Curves normalised to integrate to 1; % of employed shown in annotation.",
    "See Figure 9 for salary income only (excludes zero-salary informal workers).",
    SRC
  )
)

cat("  Figure 10 built.\n")


#===============================================================================
# Save
#===============================================================================

save_fig_ineq(fig_09, "fig_09_salary_dist_formal_informal",
              w = config$fig_defaults$width  * 2,
              h = config$fig_defaults$height * 1.35)

save_fig_ineq(fig_10, "fig_10_total_income_dist_formal_informal",
              w = config$fig_defaults$width  * 2,
              h = config$fig_defaults$height * 1.35)

fig_ineq_combined <- fig_09 / fig_10 +
  plot_annotation(
    title    = "Formal vs Informal Income Distributions \u2014 2016 vs 2024",
    subtitle = paste0(
      "Top: salary income (wage earners only).  ",
      "Bottom: total labor income (all workers with positive earnings).  ",
      "Dominican Republic | Real real DOP (CPI-deflated) | Curves normalised within group."
    ),
    theme = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, colour = "grey30")
    )
  )

save_fig_ineq(fig_ineq_combined, "fig_ineq_combined_salary_total",
              w = config$fig_defaults$width  * 2,
              h = config$fig_defaults$height * 2.8)

cat("\n=== Figures 9 & 10 complete ===\n")
