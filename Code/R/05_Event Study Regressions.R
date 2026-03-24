#===============================================================================
#
# Script: 05_Event_Study_Regressions_revised.R
#
# PURPOSE:
#   Event study regressions of minimum wage exposure on labor market outcomes.
#   Revised version incorporating four changes from code review:
#
#   CHANGE 1 — UNIT SCALING (p.p. interpretation)
#     exposure_sf_val and exposure_baseline_val multiplied by 100.
#     below_min and informal multiplied by 100.
#     Coefficients now read as: change in outcome (p.p. or log units) per
#     1 percentage point increase in baseline exposure.
#     log_var_wage is NOT scaled — it is already unit-free.
#
#   CHANGE 2 — YEAR×QUARTER FIXED EFFECTS
#     Time FE now uses `time` (year-quarter character, e.g. "2017Q3") instead
#     of `year_fe` (integer year). This absorbs 44 quarter-specific common
#     shocks rather than 11 annual shocks, eliminating within-year seasonal
#     confounding (e.g. regalía pascual Q4 spikes).
#     The event study interaction still uses year_num so each beta_k summarises
#     one full year — the interaction and FE are different variables so there
#     is no collinearity.
#     Reference year for interaction remains 2016. Reference quarter for the
#     time FE is absorbed implicitly by fixest (it omits one level).
#
#   CHANGE 3 — FOUR-WINDOW COLLAPSED REGRESSION
#     Previous: pre / mid_2017_2019 / post_2021
#     Revised:  pre / mid_2017_2019 / post_2021_2022 / post_2023
#     The post_2023 window (2023Q3–2025Q2) isolates Event D with a clean
#     post-COVID pre-period. It is the most credibly identified event.
#     post_2021_2022 covers 2021Q4–2023Q1 (post-Event C, pre-Event D).
#     Treatment quarters and gaps remain excluded via NA assignment.
#
#   CHANGE 4 — COVID ROBUSTNESS SPECIFICATION
#     Additional collapsed regression including COVID quarters (2020Q1–2021Q2)
#     with a covid_period dummy and a covid_period × exposure interaction.
#     This allows COVID to shift both the intercept and the exposure slope.
#     Run for both main (sector × firm size) and robustness (sector) panels.
#     Point estimates compared to main spec to assess 2021Q3 sensitivity.
#
# SPECIFICATIONS (unchanged from original):
#
#   MAIN SPEC — sector × firm size × quarter panel
#     Y_sft = alpha_sf + delta_t + sum_{k!=2016} beta_k*(E_sf x 1[year=k]) + e
#     alpha_sf = cell FE (sector × firm size)
#     delta_t  = year-QUARTER FE  [CHANGE 2]
#     E_sf     = 2016 baseline exposure × 100 (p.p. scale)  [CHANGE 1]
#
#   ROBUSTNESS — sector × quarter panel
#     Y_st = alpha_s + delta_t + sum_{k!=2016} beta_k*(E_s x 1[year=k]) + e
#
# OUTCOMES:
#   log_var_wage  — log variance of log wages (log units; NOT rescaled)
#   below_min     — share below minimum wage × 100 (p.p.)  [CHANGE 1]
#   informal      — informality share × 100 (p.p.)         [CHANGE 1]
#
#===============================================================================

source("Code/R/00_setup.R")

library(fixest)
library(modelsummary)
library(fwildclusterboot)
library(glue)


#===============================================================================
# STEP 0. Load Data
#===============================================================================

pd <- config$paths$processed_data

panel_sf <- readRDS(file.path(pd, "panel_sf_clean.rds")) %>%
  mutate(time = as.character(time))

exposure_s <- readRDS(file.path(pd, "sector_mw_exposure_baseline.rds"))

panel_s <- panel_sf %>%
  filter(!is.na(exposure_sf_val)) %>%
  group_by(Employment_Sector, time, time_index) %>%
  summarise(
    log_var_wage = weighted.mean(log_var_wage, pi, na.rm = TRUE),
    below_min    = weighted.mean(below_min,    pi, na.rm = TRUE),
    informal     = weighted.mean(informal,     pi, na.rm = TRUE),
    pi_sector    = sum(pi, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  left_join(exposure_s %>% select(Employment_Sector, exposure_baseline_val),
            by = "Employment_Sector") %>%
  mutate(
    group     = Employment_Sector,
    sector_fe = factor(Employment_Sector)
  )

COVID_QTRS <- c("2020Q1", "2020Q2", "2020Q3", "2020Q4", "2021Q1", "2021Q2")

OUTCOMES <- c(
  log_var_wage = "log_var_wage",
  below_min    = "below_min",
  informal     = "informal"
)

OUTCOME_LABELS <- c(
  log_var_wage = "Log wage variance",
  below_min    = "Non-compliance rate (p.p.)",
  informal     = "Informality share (p.p.)"
)


#===============================================================================
# STEP 1. Scale Variables to Percentage Points  [CHANGE 1]
#
# Exposure: multiply by 100 so 1 unit = 1 p.p. of workers near min wage.
# below_min, informal: multiply by 100 so coefficients are in p.p.
# log_var_wage: no change — already a log, unit-free.
#
# NOTE: This rescaling does not affect t-statistics, p-values, or R².
#       It only rescales coefficients and SEs by the same factor.
#       Exposure and proportion outcomes are scaled independently — the
#       coefficient on a proportion outcome is then "p.p. change in outcome
#       per 1 p.p. increase in exposure", a clean comparable number.
#===============================================================================

panel_sf <- panel_sf %>%
  mutate(
    exposure_sf_val = exposure_sf_val * 100,   # 0-100 scale
    below_min       = below_min       * 100,   # 0-100 scale
    informal        = informal        * 100    # 0-100 scale
    # log_var_wage: NOT rescaled
  )

panel_s <- panel_s %>%
  mutate(
    exposure_baseline_val = exposure_baseline_val * 100,
    below_min             = below_min             * 100,
    informal              = informal              * 100
  )

cat("=== UNIT SCALING APPLIED ===\n")
cat("exposure_sf_val range:      ", round(range(panel_sf$exposure_sf_val, na.rm=TRUE), 1), "\n")
cat("exposure_baseline_val range:", round(range(panel_s$exposure_baseline_val, na.rm=TRUE), 1), "\n")
cat("below_min range (sf):       ", round(range(panel_sf$below_min, na.rm=TRUE), 1), "\n")
cat("informal range (sf):        ", round(range(panel_sf$informal, na.rm=TRUE), 1), "\n\n")


#===============================================================================
# STEP 2. Prepare Regression Datasets  [CHANGE 2: year×quarter FE]
#
# Key change: time FE uses factor(time) — the year-quarter string — not
# factor(year_num). year_num is retained separately for the interaction term
# so the event study produces one beta per year rather than per quarter.
#
# sector_fe added explicitly to reg_s mutate to avoid relying on carry-over
# from panel_s (defensive coding fix from review).
#===============================================================================

# ── Main spec: sector × firm size × quarter ───────────────────────────────────
reg_sf <- panel_sf %>%
  filter(
    !time %in% COVID_QTRS,
    !is.na(exposure_sf_val)
  ) %>%
  mutate(
    cell_fe  = factor(cell_id),
    year_num = as.integer(substr(time, 1, 4)),         # for interaction term
    time_fe  = factor(time),                           # year×quarter FE [CHANGE 2]
    year_fe  = relevel(factor(year_num), ref = "2016"),# kept for bootstrap compat
    sector   = factor(Employment_Sector)
  )

# ── Robustness spec: sector × quarter ─────────────────────────────────────────
reg_s <- panel_s %>%
  filter(!time %in% COVID_QTRS,
         !is.na(exposure_baseline_val)) %>%
  mutate(
    year_num  = as.integer(substr(time, 1, 4)),
    time_fe   = factor(time),                          # year×quarter FE [CHANGE 2]
    year_fe   = relevel(factor(year_num), ref = "2016"),
    sector    = factor(Employment_Sector),
    sector_fe = factor(Employment_Sector)              # explicit (defensive fix)
  )

# ── Diagnostics ───────────────────────────────────────────────────────────────
cat("=== MAIN SPEC (sector × firm size × quarter) ===\n")
cat("Cells:", n_distinct(reg_sf$cell_fe), "\n")
cat("Sectors (clusters):", n_distinct(reg_sf$sector), "\n")
cat("Quarters (time FE levels):", n_distinct(reg_sf$time_fe), "\n")
cat("Years (interaction levels):", n_distinct(reg_sf$year_num), "\n")
cat("Observations:", nrow(reg_sf), "\n\n")

cat("Non-missing per outcome (main spec):\n")
reg_sf %>%
  summarise(across(all_of(unname(OUTCOMES)), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "n_nonmissing") %>%
  print()

cat("\n=== ROBUSTNESS SPEC (sector × quarter) ===\n")
cat("Sectors:", n_distinct(reg_s$sector), "\n")
cat("Quarters:", n_distinct(reg_s$time_fe), "\n")
cat("Observations:", nrow(reg_s), "\n\n")


#===============================================================================
# STEP 3. Event Study Helper Functions  [CHANGE 2: time_fe replaces year_fe in FE]
#
# The formula now separates:
#   - interaction: i(year_num, exposure, ref=2016) — one beta per year
#   - FE:          cell_fe + time_fe              — absorbs year×quarter shocks
#
# This is valid: the interaction slopes and the time intercepts are different
# objects. fixest handles this without collinearity issues.
#===============================================================================

run_es_main <- function(outcome, data = reg_sf, ref = 2016) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) {
    message("Skipping '", outcome, "' — insufficient observations.")
    return(NULL)
  }
  feols(
    as.formula(glue(
      "{outcome} ~ i(year_num, exposure_sf_val, ref = {ref}) | cell_fe + time_fe"
    )),
    data    = d,
    weights = ~pi,
    vcov    = ~sector
  )
}

run_es_robust <- function(outcome, data = reg_s, ref = 2016) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) {
    message("Skipping '", outcome, "' — insufficient observations.")
    return(NULL)
  }
  feols(
    as.formula(glue(
      "{outcome} ~ i(year_num, exposure_baseline_val, ref = {ref}) | sector_fe + time_fe"
    )),
    data    = d,
    weights = ~pi_sector,
    vcov    = ~sector
  )
}


#===============================================================================
# STEP 4. Run Event Studies
#===============================================================================

cat(strrep("=", 70), "\n")
cat("EVENT STUDIES — MAIN SPEC\n")
cat(strrep("=", 70), "\n\n")

es_main <- map(OUTCOMES, run_es_main) %>% compact()
cat("Estimated:", paste(names(es_main), collapse = ", "), "\n\n")

cat("Cells and clusters contributing per outcome:\n")
map_dfr(names(OUTCOMES), function(col) {
  reg_sf %>%
    filter(!is.na(.data[[col]])) %>%
    summarise(
      n_obs     = n(),
      n_cells   = n_distinct(cell_id),
      n_sectors = n_distinct(Employment_Sector),
      .groups   = "drop"
    ) %>%
    mutate(outcome = col)
}) %>%
  select(outcome, n_obs, n_cells, n_sectors) %>%
  print()

cat("\nNOTE: n_sectors < 30 — wild cluster bootstrap provides reliable inference.\n\n")

for (nm in names(es_main)) {
  cat("---", nm, "---\n")
  print(summary(es_main[[nm]]))
  cat("\n")
}

cat(strrep("=", 70), "\n")
cat("EVENT STUDIES — ROBUSTNESS SPEC\n")
cat(strrep("=", 70), "\n\n")

es_robust <- map(OUTCOMES, run_es_robust) %>% compact()
cat("Estimated:", paste(names(es_robust), collapse = ", "), "\n\n")


#===============================================================================
# STEP 5. Wild Cluster Bootstrap
#
# fwildclusterboot does not support weights — unweighted refits used here.
# Point estimates from weighted models (Step 4).
# CIs and p-values from unweighted bootstrap refits.
# Webb weights recommended for <= 10 clusters.
#
# NOTE on CHANGE 2: unweighted refits now also use time_fe (year×quarter).
# The formula uses year_num for interaction and time_fe for the FE, consistent
# with the main models.
#===============================================================================

refit_unweighted <- function(outcome, data, fe_spec) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  fit <- feols(
    as.formula(glue(
      "{outcome} ~ i(year_num, exposure_sf_val, ref = 2016) | {fe_spec}"
    )),
    data = d,
    vcov = ~sector
  )
  attr(fit, "fit_data") <- d
  fit
}

refit_unweighted_robust <- function(outcome, data, fe_spec) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  fit <- feols(
    as.formula(glue(
      "{outcome} ~ i(year_num, exposure_baseline_val, ref = 2016) | {fe_spec}"
    )),
    data = d,
    vcov = ~sector
  )
  attr(fit, "fit_data") <- d
  fit
}

bootstrap_ci <- function(fit, B = 9999, seed = 42, conf_level = 0.95) {
  coef_names <- names(coef(fit))
  es_coefs   <- coef_names[grepl("year_num::", coef_names, fixed = TRUE)]
  if (length(es_coefs) == 0) return(NULL)
  
  fit_data <- attr(fit, "fit_data")
  if (is.null(fit_data)) stop("fit_data attribute missing.")
  
  set.seed(seed)
  
  map_dfr(es_coefs, function(cn) {
    bt <- tryCatch(
      boottest(
        object     = fit,
        param      = cn,
        B          = B,
        clustid    = "sector",
        data       = fit_data,
        type       = "webb",
        sign_level = 1 - conf_level
      ),
      error = function(e) {
        warning("Bootstrap failed for ", cn, ": ", e$message)
        NULL
      }
    )
    if (is.null(bt)) {
      return(tibble(term = cn, estimate = coef(fit)[cn],
                    conf.low = NA_real_, conf.high = NA_real_,
                    p.value = NA_real_))
    }
    tidy_bt <- generics::tidy(bt)
    tibble(
      term      = cn,
      estimate  = coef(fit)[cn],
      conf.low  = tidy_bt$conf.low,
      conf.high = tidy_bt$conf.high,
      p.value   = tidy_bt$p.value
    )
  })
}

cat("Refitting without weights for bootstrap...\n")
es_main_unw <- map(names(OUTCOMES), refit_unweighted,
                   data    = reg_sf,
                   fe_spec = "cell_fe + time_fe") %>%   # [CHANGE 2]
  setNames(names(OUTCOMES)) %>%
  compact()

es_robust_unw <- map(names(OUTCOMES), refit_unweighted_robust,
                     data    = reg_s,
                     fe_spec = "sector_fe + time_fe") %>%  # [CHANGE 2]
  setNames(names(OUTCOMES)) %>%
  compact()

cat("Running bootstrap (B=9999)...\n\n")

set.seed(42)
boot_ci_main <- map(es_main_unw, bootstrap_ci, B = 9999, seed = 42) %>%
  compact()

set.seed(42)
boot_ci_robust <- map(es_robust_unw, bootstrap_ci, B = 9999, seed = 42) %>%
  compact()

walk(names(boot_ci_main), function(nm) {
  n_na <- sum(is.na(boot_ci_main[[nm]]$conf.low))
  if (n_na > 0) warning(nm, ": ", n_na, " bootstrap CIs are NA.")
})

saveRDS(boot_ci_main,   file.path(pd, "bootstrap_ci_main.rds"))
saveRDS(boot_ci_robust, file.path(pd, "bootstrap_ci_robust.rds"))
cat("Bootstrap CIs saved.\n\n")


#===============================================================================
# STEP 6. Event Study Plots
#===============================================================================

out_path_sf <- file.path(config$paths$outputs, config$output_stage,
                         config$out_subdirs$reg_sector_firmsize)
out_path_s  <- file.path(config$paths$outputs, config$output_stage,
                         config$out_subdirs$reg_sector)

dir.create(out_path_sf, recursive = TRUE, showWarnings = FALSE)
dir.create(out_path_s,  recursive = TRUE, showWarnings = FALSE)

MW_EVENT_YEARS <- c(2017.5, 2019.75, 2021.75, 2023.5)

parse_year <- function(term) {
  as.integer(regmatches(term, regexpr("\\d{4}", term)))
}

plot_event_study <- function(boot_tbl,
                             title    = NULL,
                             subtitle = NULL,
                             ref_year = 2016,
                             y_label  = "Coefficient on Exposure (p.p.) × Year") {
  if (is.null(boot_tbl) || nrow(boot_tbl) == 0) return(NULL)
  
  plot_data <- boot_tbl %>%
    mutate(year = parse_year(term)) %>%
    bind_rows(tibble(year = ref_year, estimate = 0,
                     conf.low = 0, conf.high = 0,
                     p.value = NA_real_)) %>%
    arrange(year) %>%
    mutate(
      sig = case_when(
        p.value < 0.01 ~ "p < 0.01",
        p.value < 0.05 ~ "p < 0.05",
        p.value < 0.10 ~ "p < 0.10",
        TRUE           ~ "n.s."
      ),
      sig = factor(sig, levels = c("p < 0.01", "p < 0.05", "p < 0.10", "n.s."))
    )
  
  ggplot(plot_data, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey50", linewidth = 0.4) +
    annotate("rect",
             xmin = ref_year - 0.5, xmax = ref_year + 0.5,
             ymin = -Inf, ymax = Inf,
             alpha = 0.08, fill = "grey50") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.15, fill = "#2166ac") +
    geom_line(colour = "#2166ac", linewidth = 0.7) +
    geom_point(aes(colour = sig, shape = sig), size = 2.5) +
    geom_vline(xintercept = MW_EVENT_YEARS,
               linetype = "dotted", colour = "red", alpha = 0.6,
               linewidth = 0.5) +
    scale_colour_manual(
      values = c("p < 0.01" = "#d73027", "p < 0.05" = "#fc8d59",
                 "p < 0.10" = "#fee090", "n.s."     = "grey60"),
      drop = FALSE
    ) +
    scale_shape_manual(
      values = c("p < 0.01" = 16, "p < 0.05" = 16,
                 "p < 0.10" = 17, "n.s."     = 1),
      drop = FALSE
    ) +
    scale_x_continuous(breaks = seq(2014, 2025, 1)) +
    labs(title    = title,
         subtitle = subtitle,
         x        = NULL,
         y        = y_label,
         colour   = "Significance",
         shape    = "Significance") +
    theme_surveytools() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.ticks.x = element_line())
}

subtitle_main <- paste(
  "Main spec: sector × firm size × quarter. 2016 base year.",
  "Exposure and proportion outcomes in p.p. (×100). Coefficients: outcome change per 1 p.p. exposure.",
  "Shaded band = wild cluster bootstrap 95% CI (B=9999, Webb weights, unweighted refit).",
  "Clustered at sector level (10 clusters). COVID 2020Q1-2021Q2 excluded.",
  "Year×quarter time FE. Red dotted lines = min wage events (2017Q2, 2019Q3, 2021Q3, 2023Q2).",
  sep = "\n"
)

es_plots <- imap(OUTCOME_LABELS, function(label, nm) {
  if (!nm %in% names(boot_ci_main)) return(NULL)
  plot_event_study(
    boot_ci_main[[nm]],
    title    = paste("Minimum Wage Exposure:", label),
    subtitle = subtitle_main,
    y_label  = case_when(
      nm == "log_var_wage" ~ "Coefficient on Exposure (p.p.) × Year [log units]",
      TRUE                 ~ "Coefficient on Exposure (p.p.) × Year [p.p.]"
    )
  )
}) %>% compact()

walk2(es_plots, names(es_plots), function(p, nm) {
  path <- file.path(out_path, paste0("es_plot_", nm, ".", config$fig_defaults$format))
  ggsave(path, p,
         width  = config$fig_defaults$width,
         height = config$fig_defaults$height,
         dpi    = config$fig_defaults$dpi)
  cat("Saved:", path, "\n")
})


#===============================================================================
# STEP 7. Collapsed Window Regressions — Four Windows  [CHANGE 3]
#
# REVISED WINDOWS (from three to four):
#   pre            = 2014Q3–2016Q4  (reference)
#   mid_2017_2019  = 2017Q1–2019Q2  (post-Event A; Event B treatment Q excluded)
#   post_2021_2022 = 2021Q4–2023Q1  (post-Event C; Event C treatment Q excluded)
#   post_2023      = 2023Q3–2025Q2  (post-Event D; Event D treatment Q excluded)
#
# Excluded from all windows (mapped to NA):
#   2019Q3        — Event B treatment quarter
#   2019Q4–2021Q3 — gap between Event B post and Event C
#                   (includes COVID 2020Q1–2021Q2 and Event C treatment Q)
#   2023Q2        — Event D treatment quarter
#
# The post_2023 window is the cleanest event estimate: it has a well-defined
# post-COVID pre-period (post_2021_2022) and no COVID contamination.
# The mid_2017_2019 window is the second cleanest: pre-COVID, two clean years.
# post_2021_2022 is the weakest: first post-COVID quarters, limited pre-period.
#
# time_idx formula: year + (quarter-1)/4
#   2017Q1 → 2017.00,  2019Q2 → 2019.25,  2021Q4 → 2021.75
#   2023Q1 → 2023.00,  2023Q2 → 2023.25,  2023Q3 → 2023.50
#===============================================================================

add_window_4 <- function(data) {
  data %>%
    mutate(
      year_num_w    = as.integer(substr(time, 1, 4)),
      quarter_num_w = as.integer(substr(time, 6, 6)),
      time_idx      = year_num_w + (quarter_num_w - 1) / 4,
      window = case_when(
        # Reference period
        time_idx <  2017.00                              ~ "pre",
        # Post-Event A: 2017Q1 to 2019Q2 (2019Q2 = 2019.25)
        time_idx >= 2017.00 & time_idx <= 2019.25        ~ "mid_2017_2019",
        # Post-Event C: 2021Q4 (2021.75) to 2023Q1 (2023.00)
        time_idx >= 2021.75 & time_idx <= 2023.00        ~ "post_2021_2022",
        # Post-Event D: 2023Q3 (2023.50) onwards
        time_idx >= 2023.50                              ~ "post_2023",
        # Everything else excluded: Event B Q (2019Q3), gap, COVID, Event C Q,
        # Event D Q (2023Q2 = 2023.25)
        TRUE                                             ~ NA_character_
      )
    ) %>%
    filter(!is.na(window)) %>%
    mutate(window = factor(window,
                           levels = c("pre", "mid_2017_2019",
                                      "post_2021_2022", "post_2023")))
}

run_collapsed_main_4 <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(
    as.formula(glue(
      "{outcome} ~ i(window, exposure_sf_val, ref = 'pre') | cell_fe + time_fe"
    )),
    data    = d,
    weights = ~pi,
    vcov    = ~sector
  )
}

run_collapsed_robust_4 <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(
    as.formula(glue(
      "{outcome} ~ i(window, exposure_baseline_val, ref = 'pre') | sector_fe + time_fe"
    )),
    data    = d,
    weights = ~pi_sector,
    vcov    = ~sector
  )
}

cat(strrep("=", 70), "\n")
cat("COLLAPSED WINDOW REGRESSIONS — FOUR WINDOWS\n")
cat(strrep("=", 70), "\n\n")

# Apply COVID exclusion before windowing (COVID already excluded in reg_sf/reg_s)
reg_sf_win4 <- add_window_4(reg_sf)
reg_s_win4  <- add_window_4(reg_s)

cat("Observations per window (main spec):\n")
reg_sf_win4 %>% count(window) %>% print()
cat("\n")

collapsed_main_4 <- map(names(OUTCOMES), run_collapsed_main_4,
                        data = reg_sf_win4) %>%
  setNames(names(OUTCOMES)) %>% compact()

collapsed_robust_4 <- map(names(OUTCOMES), run_collapsed_robust_4,
                          data = reg_s_win4) %>%
  setNames(names(OUTCOMES)) %>% compact()

cat("Main collapsed (4-window):", paste(names(collapsed_main_4), collapse = ", "), "\n")
cat("Robust collapsed (4-window):", paste(names(collapsed_robust_4), collapse = ", "), "\n\n")

for (nm in names(collapsed_main_4)) {
  cat("---", nm, "(4-window collapsed, main) ---\n")
  print(summary(collapsed_main_4[[nm]]))
  cat("\n")
}


#===============================================================================
# STEP 8. Summary Tables — Four-Window  [CHANGE 3]
#===============================================================================

coef_map_4 <- c(
  "window::mid_2017_2019:exposure_sf_val"       = "Exposure × Post-2017",
  "window::post_2021_2022:exposure_sf_val"       = "Exposure × Post-2021",
  "window::post_2023:exposure_sf_val"            = "Exposure × Post-2023",
  "window::mid_2017_2019:exposure_baseline_val"  = "Exposure × Post-2017",
  "window::post_2021_2022:exposure_baseline_val" = "Exposure × Post-2021",
  "window::post_2023:exposure_baseline_val"      = "Exposure × Post-2023"
)

gof_map <- tribble(
  ~raw,            ~clean,        ~fmt,
  "nobs",          "Observations",  0,
  "r.squared",     "R²",            3,
  "adj.r.squared", "Adj. R²",       3
)

notes_main_4 <- list(
  "Main spec: sector × firm size × quarter. Four-window collapsed event study.",
  paste0("Exposure = share of formal workers within 10% above min wage, ",
         "2016 baseline, scaled ×100 (percentage points)."),
  "below_min and informal also scaled ×100 (p.p.). log_var_wage in log units.",
  "Cell (sector × firm size) and year×quarter fixed effects. Weighted by baseline employment share (pi).",
  "Standard errors clustered at sector level (10 clusters). * p<0.10, ** p<0.05, *** p<0.01.",
  "COVID (2020Q1-2021Q2) excluded. Treatment quarters excluded.",
  "Windows: pre=2014Q3-2016Q4; mid=2017Q1-2019Q2; post21=2021Q4-2023Q1; post23=2023Q3-2025Q2.",
  "post_2023 is the most cleanly identified event: post-COVID pre-period, no pandemic overlap."
)

modelsummary(
  collapsed_main_4,
  coef_map  = coef_map_4,
  gof_map   = gof_map,
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  vcov      = ~sector,
  title     = "Minimum Wage Exposure and Labor Market Outcomes (Four Windows)",
  notes     = notes_main_4,
  output    = file.path(out_path, "table_main_collapsed_4window.html")
)

modelsummary(
  collapsed_main_4,
  coef_map  = coef_map_4,
  gof_map   = gof_map,
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  vcov      = ~sector,
  title     = "Minimum Wage Exposure and Labor Market Outcomes (Four Windows)",
  notes     = list(
    "Four-window collapsed event study. Cell and year×quarter FE.",
    "Exposure and proportion outcomes in p.p. (×100).",
    "Clustered by sector (10 clusters). * p$<$0.10, ** p$<$0.05, *** p$<$0.01."
  ),
  output    = file.path(out_path, "table_main_collapsed_4window.tex")
)

modelsummary(
  collapsed_robust_4,
  coef_map  = coef_map_4,
  gof_map   = gof_map,
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  vcov      = ~sector,
  title     = "Robustness: Sector × Quarter Panel (Four Windows)",
  notes     = list(
    "Robustness spec: sector × quarter. Exposure aggregated to sector level.",
    "Sector and year×quarter FE. Clustered by sector (10 clusters).",
    "* p<0.10, ** p<0.05, *** p<0.01."
  ),
  output    = file.path(out_path, "table_robustness_sector_4window.html")
)

cat("Four-window tables saved.\n\n")


#===============================================================================
# STEP 9. COVID Robustness Specification  [CHANGE 4]
#
# Re-runs the collapsed regression INCLUDING the six COVID quarters
# (2020Q1–2021Q2) with two additional terms:
#
#   covid_period            — dummy = 1 for 2020Q1–2021Q2 (level shift)
#   covid_period:exposure   — interaction = COVID dummy × exposure (p.p.)
#                             allows COVID to differentially affect high-
#                             vs. low-exposure cells
#
# The window definitions are extended to include COVID quarters in the gap:
#   pre            = 2014Q3–2016Q4        (unchanged)
#   mid_2017_2019  = 2017Q1–2019Q2        (unchanged)
#   covid          = 2019Q4–2021Q3        (gap + COVID + Event C treatment Q,
#                                          absorbed by covid_period dummy)
#   post_2021_2022 = 2021Q4–2023Q1        (unchanged)
#   post_2023      = 2023Q3–2025Q2        (unchanged)
#
# NOTE: The covid window rows enter the regression but their window indicator
# is NOT an interaction term — the covid_period dummy handles them. They are
# included so the panel is balanced around the COVID period.
#
# Interpretation: If the post_2021 and post_2023 window coefficients are
# stable relative to the main spec (Step 7), COVID composition effects are
# not driving results. If they change substantially, flag in paper.
#
# Run for both main (cell-level) and robustness (sector-level) panels.
#===============================================================================

cat(strrep("=", 70), "\n")
cat("COVID ROBUSTNESS — WITH COVID QUARTERS INCLUDED  [CHANGE 4]\n")
cat(strrep("=", 70), "\n\n")

# Helper: add window including COVID quarters
add_window_covid <- function(data) {
  data %>%
    mutate(
      year_num_w    = as.integer(substr(time, 1, 4)),
      quarter_num_w = as.integer(substr(time, 6, 6)),
      time_idx      = year_num_w + (quarter_num_w - 1) / 4,
      # COVID dummy: 1 for 2020Q1–2021Q2
      covid_period  = as.integer(time %in% COVID_QTRS),
      window = case_when(
        time_idx <  2017.00                              ~ "pre",
        time_idx >= 2017.00 & time_idx <= 2019.25        ~ "mid_2017_2019",
        # COVID gap: 2019Q3 (Event B) through 2021Q3 (Event C treatment Q)
        # These are included but absorbed by covid_period dummy
        time_idx >  2019.25 & time_idx <= 2021.50        ~ "covid_gap",
        time_idx >= 2021.75 & time_idx <= 2023.00        ~ "post_2021_2022",
        time_idx >= 2023.50                              ~ "post_2023",
        # 2023Q2 = Event D treatment quarter, excluded
        TRUE                                             ~ NA_character_
      )
    ) %>%
    filter(!is.na(window)) %>%
    mutate(window = factor(window,
                           levels = c("pre", "mid_2017_2019", "covid_gap",
                                      "post_2021_2022", "post_2023")))
}

# COVID robustness: collapsed regression with covid_period dummy + interaction
# The window interaction terms are only for pre/mid/post_2021/post_2023.
# covid_gap rows are controlled for by the covid_period dummy.
# We use i(window, exposure, ref='pre') which will produce coefficients for
# mid, covid_gap, post_2021_2022, and post_2023 — but covid_gap is a
# nuisance coefficient, not of primary interest.

run_collapsed_covid_main <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(
    as.formula(glue(
      "{outcome} ~ i(window, exposure_sf_val, ref = 'pre') +
                   covid_period +
                   covid_period:exposure_sf_val |
                   cell_fe + time_fe"
    )),
    data    = d,
    weights = ~pi,
    vcov    = ~sector
  )
}

run_collapsed_covid_robust <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(
    as.formula(glue(
      "{outcome} ~ i(window, exposure_baseline_val, ref = 'pre') +
                   covid_period +
                   covid_period:exposure_baseline_val |
                   sector_fe + time_fe"
    )),
    data    = d,
    weights = ~pi_sector,
    vcov    = ~sector
  )
}

# Build COVID-inclusive datasets (no COVID exclusion filter)
reg_sf_covid <- panel_sf %>%
  filter(!is.na(exposure_sf_val)) %>%
  mutate(
    cell_fe  = factor(cell_id),
    year_num = as.integer(substr(time, 1, 4)),
    time_fe  = factor(time),
    sector   = factor(Employment_Sector)
  )

reg_s_covid <- panel_s %>%
  filter(!is.na(exposure_baseline_val)) %>%
  mutate(
    year_num  = as.integer(substr(time, 1, 4)),
    time_fe   = factor(time),
    sector    = factor(Employment_Sector),
    sector_fe = factor(Employment_Sector)
  )

# Apply COVID window helper
reg_sf_covid_win <- add_window_covid(reg_sf_covid)
reg_s_covid_win  <- add_window_covid(reg_s_covid)

cat("Observations per window (COVID robustness, main spec):\n")
reg_sf_covid_win %>% count(window, covid_period) %>% print()
cat("\n")

collapsed_covid_main <- map(names(OUTCOMES), run_collapsed_covid_main,
                            data = reg_sf_covid_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

collapsed_covid_robust <- map(names(OUTCOMES), run_collapsed_covid_robust,
                              data = reg_s_covid_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

cat("COVID robustness main:", paste(names(collapsed_covid_main), collapse = ", "), "\n")
cat("COVID robustness robust:", paste(names(collapsed_covid_robust), collapse = ", "), "\n\n")

for (nm in names(collapsed_covid_main)) {
  cat("---", nm, "(COVID robustness, main) ---\n")
  print(summary(collapsed_covid_main[[nm]]))
  cat("\n")
}

# COVID robustness coefficient map
# We report only the post-2021 and post-2023 window terms — the covid_gap
# term is a nuisance parameter and the covid_period interaction is diagnostic.
coef_map_covid <- c(
  "window::mid_2017_2019:exposure_sf_val"       = "Exposure × Post-2017",
  "window::post_2021_2022:exposure_sf_val"       = "Exposure × Post-2021",
  "window::post_2023:exposure_sf_val"            = "Exposure × Post-2023",
  "covid_period"                                 = "COVID period (level)",
  "covid_period:exposure_sf_val"                 = "COVID × Exposure (p.p.)",
  "window::mid_2017_2019:exposure_baseline_val"  = "Exposure × Post-2017",
  "window::post_2021_2022:exposure_baseline_val" = "Exposure × Post-2021",
  "window::post_2023:exposure_baseline_val"      = "Exposure × Post-2023",
  "covid_period:exposure_baseline_val"           = "COVID × Exposure (p.p.)"
)

notes_covid <- list(
  "COVID robustness: COVID quarters (2020Q1-2021Q2) included with covid_period dummy.",
  "covid_period = 1 for 2020Q1-2021Q2. COVID x Exposure interaction allows differential COVID effect.",
  "Compare post-2021 and post-2023 coefficients to main spec (table_main_collapsed_4window).",
  "Stable coefficients = COVID composition effects not driving main results.",
  "Cell and year×quarter FE. Clustered by sector. * p<0.10, ** p<0.05, *** p<0.01."
)

modelsummary(
  collapsed_covid_main,
  coef_map  = coef_map_covid,
  gof_map   = gof_map,
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  vcov      = ~sector,
  title     = "COVID Robustness: Including COVID Quarters with COVID Dummy",
  notes     = notes_covid,
  output    = file.path(out_path, "table_covid_robustness_main.html")
)

modelsummary(
  collapsed_covid_robust,
  coef_map  = coef_map_covid,
  gof_map   = gof_map,
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  vcov      = ~sector,
  title     = "COVID Robustness: Sector × Quarter Panel",
  notes     = c(notes_covid,
                list("Robustness spec: sector × quarter, exposure at sector level.")),
  output    = file.path(out_path, "table_covid_robustness_sector.html")
)

cat("COVID robustness tables saved.\n\n")


#===============================================================================
# STEP 10. Robustness — Drop REVIEW Cells
# (unchanged from original; uses four-window windowed data)
#===============================================================================

cat(strrep("=", 70), "\n")
cat("ROBUSTNESS: DROP REVIEW CELLS\n")
cat(strrep("=", 70), "\n\n")

review_cells <- readRDS(file.path(pd, "validation_decision_table.rds")) %>%
  filter(recommendation == "Review")

if (nrow(review_cells) == 0) {
  cat("No Review cells — robustness check not needed.\n\n")
} else {
  cat("Review cells being dropped:\n")
  print(review_cells %>% select(outcome, Employment_Sector, Wage_group))
  cat("\n")
  
  reg_sf_strict <- reg_sf_win4   # use four-window data
  
  for (out_name in unique(review_cells$outcome)) {
    if (!out_name %in% names(OUTCOMES)) next
    drop_cells <- review_cells %>%
      filter(outcome == out_name) %>%
      mutate(cell_id = paste0(Employment_Sector, "__", Wage_group)) %>%
      pull(cell_id)
    reg_sf_strict <- reg_sf_strict %>%
      mutate(!!out_name := if_else(cell_id %in% drop_cells,
                                   NA_real_, .data[[out_name]]))
  }
  
  collapsed_strict <- map(names(OUTCOMES), run_collapsed_main_4,
                          data = reg_sf_strict) %>%
    setNames(names(OUTCOMES)) %>% compact()
  
  modelsummary(
    collapsed_strict,
    coef_map = coef_map_4,
    gof_map  = gof_map,
    stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    vcov     = ~sector,
    title    = "Robustness: Drop Review Cells (Four Windows)",
    notes    = list("Review cells excluded. Four-window spec. All other settings identical."),
    output   = file.path(out_path, "table_robustness_strict_4window.html")
  )
  
  cat("Strict robustness table saved.\n\n")
}

cat("=== 05_Event_Study_Regressions_revised.R complete ===\n")