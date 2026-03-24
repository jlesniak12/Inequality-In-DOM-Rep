#===============================================================================
#
# Script: 04_Regressions.R
#
# PURPOSE:
#   Event study regressions of minimum wage exposure on labor market outcomes.
#   Uses panel_sf_clean.rds produced by 02_Cell_Validation_For_Regression_Data.R
#   — outcome columns are already set to NA for cells that fail validation.
#
# SPECIFICATIONS:
#
#   MAIN SPEC — sector × firm size × quarter panel
#     Y_sft = alpha_sf + delta_t + sum_{k!=2016} beta_k*(E_sf x 1[year=k]) + e_sft
#     alpha_sf = cell FE (sector × firm size)
#     delta_t  = year-quarter FE
#     E_sf     = 2016 baseline exposure at sector × firm size level (fixed)
#     Cluster  = Employment_Sector (10 clusters, wild bootstrap for inference)
#     Weights  = pi (firm size employment share within sector, from baseline)
#
#   ROBUSTNESS — sector × quarter panel
#     Y_st = alpha_s + delta_t + sum_{k!=2016} beta_k*(E_s x 1[year=k]) + e_st
#     alpha_s = sector FE
#     E_s     = 2016 baseline exposure aggregated to sector level
#
# OUTCOMES (log_50_10 dropped — too few reliable cells from validation):
#   log_var_wage      — log variance of log wages (primary inequality measure)
#   below_min         — share of workers below minimum wage (non-compliance)
#   informal          — informality share (restricted subsample)
#
# NOTES:
#   - Wild bootstrap uses unweighted refit (fwildclusterboot limitation).
#     Point estimates from weighted models; CIs from unweighted refits.
#   - COVID quarters (2020Q1-2021Q2) excluded from all regressions.
#   - Reference year = 2016 (baseline exposure year).
#   - informal outcome covers only cells that pass the near-zero screen
#     (~15 cells, Micro/Small in sectors with meaningful informality).
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

# Main panel — outcome columns already NA-masked by cell validation
panel_sf <- readRDS(file.path(pd, "panel_sf_clean.rds")) %>%
  mutate(time = as.character(time))

# Sector-level exposure for robustness spec
exposure_s <- readRDS(file.path(pd, "sector_mw_exposure_baseline.rds"))

# Construct sector × quarter panel for robustness by collapsing main panel
# Weight outcomes by pi within each sector × quarter
panel_s <- panel_sf %>%
  filter(!is.na(exposure_sf_val)) %>%
  group_by(Employment_Sector, time, time_index) %>%
  summarise(
    log_var_wage = weighted.mean(log_var_wage, pi, na.rm = TRUE),
    below_min    = weighted.mean(below_min,    pi, na.rm = TRUE),
    informal     = weighted.mean(informal,     pi, na.rm = TRUE),
    pi_sector    = sum(pi, na.rm = TRUE),   # total employment share in sector
    .groups      = "drop"
  ) %>%
  left_join(exposure_s %>% select(Employment_Sector, exposure_baseline_val),
            by = "Employment_Sector") %>%
  mutate(
    group     = Employment_Sector,
    sector_fe = factor(Employment_Sector)
  )

# COVID quarters to exclude
COVID_QTRS <- c("2020Q1", "2020Q2", "2020Q3", "2020Q4", "2021Q1", "2021Q2")

# Outcomes used in regressions
# log_50_10 dropped: only 13 KEEP cells from validation — too few for regression
OUTCOMES <- c(
  log_var_wage = "log_var_wage",
  below_min    = "below_min",
  informal     = "informal"
)

OUTCOME_LABELS <- c(
  log_var_wage = "Log wage variance",
  below_min    = "Non-compliance rate",
  informal     = "Informality share"
)


#===============================================================================
# STEP 1. Prepare Regression Datasets
#===============================================================================

# ── Main spec: sector × firm size × quarter ───────────────────────────────────
reg_sf <- panel_sf %>%
  filter(
    !time %in% COVID_QTRS,
    !is.na(exposure_sf_val)
  ) %>%
  mutate(
    cell_fe  = factor(cell_id),
    year_num = as.integer(substr(time, 1, 4)),
    year_fe  = relevel(factor(year_num), ref = "2016"),
    sector   = factor(Employment_Sector)
  )

# ── Robustness spec: sector × quarter ─────────────────────────────────────────
reg_s <- panel_s %>%
  filter(!time %in% COVID_QTRS,
         !is.na(exposure_baseline_val)) %>%
  mutate(
    year_num  = as.integer(substr(time, 1, 4)),
    year_fe   = relevel(factor(year_num), ref = "2016"),
    sector    = factor(Employment_Sector)
  )

# ── Diagnostics ───────────────────────────────────────────────────────────────
cat("=== MAIN SPEC (sector × firm size × quarter) ===\n")
cat("Reference year:", levels(reg_sf$year_fe)[1], "\n")
cat("Cells:", n_distinct(reg_sf$cell_fe), "\n")
cat("Sectors (clusters):", n_distinct(reg_sf$sector), "\n")
cat("Quarters:", n_distinct(reg_sf$time), "\n")
cat("Observations:", nrow(reg_sf), "\n\n")

cat("Non-missing per outcome (main spec):\n")
reg_sf %>%
  summarise(across(all_of(unname(OUTCOMES)), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "n_nonmissing") %>%
  print()

cat("\n=== ROBUSTNESS SPEC (sector × quarter) ===\n")
cat("Sectors:", n_distinct(reg_s$sector), "\n")
cat("Quarters:", n_distinct(reg_s$time), "\n")
cat("Observations:", nrow(reg_s), "\n\n")


#===============================================================================
# STEP 2. Event Study Helper Functions
#===============================================================================

# Run event study — main spec (cell FE + year FE)
run_es_main <- function(outcome, data = reg_sf, ref = "2016") {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) {
    message("Skipping '", outcome, "' — insufficient observations.")
    return(NULL)
  }
  feols(
    as.formula(glue(
      "{outcome} ~ i(year_fe, exposure_sf_val, ref = '{ref}') | cell_fe + year_fe"
    )),
    data    = d,
    weights = ~pi,       # weight by baseline firm size employment share
    vcov    = ~sector    # cluster at sector level
  )
}

# Run event study — robustness spec (sector FE + year FE)
run_es_robust <- function(outcome, data = reg_s, ref = "2016") {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) {
    message("Skipping '", outcome, "' — insufficient observations.")
    return(NULL)
  }
  feols(
    as.formula(glue(
      "{outcome} ~ i(year_fe, exposure_baseline_val, ref = '{ref}') | sector_fe + year_fe"
    )),
    data    = d,
    weights = ~pi_sector,
    vcov    = ~sector
  )
}


#===============================================================================
# STEP 3. Run Event Studies
#===============================================================================

cat(strrep("=", 70), "\n")
cat("EVENT STUDIES — MAIN SPEC\n")
cat(strrep("=", 70), "\n\n")

es_main <- map(OUTCOMES, run_es_main) %>% compact()
cat("Estimated:", paste(names(es_main), collapse = ", "), "\n\n")

# Diagnostic: cells and clusters per outcome
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

cat("\nNOTE: n_sectors < 30 for all outcomes — cluster SEs are approximations.\n")
cat("      Wild cluster bootstrap (Section 4) provides reliable inference.\n\n")

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
# STEP 4. Wild Cluster Bootstrap
#
# fwildclusterboot does not support survey weights — unweighted refits used.
# Point estimates from weighted models (Step 3).
# CIs and p-values from unweighted bootstrap refits here.
#
# Webb weights used (recommended for <= 10 clusters).
# Flag any outcomes where bootstrap returns NA CIs.
#===============================================================================

cat(strrep("=", 70), "\n")
cat("WILD CLUSTER BOOTSTRAP\n")
cat(strrep("=", 70), "\n\n")

# Unweighted refit — stores the fitted data as attribute for bootstrap
refit_unweighted <- function(outcome, data, fe_spec) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  fit <- feols(
    as.formula(glue(
      "{outcome} ~ i(year_fe, exposure_sf_val, ref = '2016') | {fe_spec}"
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
      "{outcome} ~ i(year_fe, exposure_baseline_val, ref = '2016') | {fe_spec}"
    )),
    data = d,
    vcov = ~sector
  )
  attr(fit, "fit_data") <- d
  fit
}

# Bootstrap CI function
bootstrap_ci <- function(fit, B = 9999, seed = 42, conf_level = 0.95) {
  coef_names <- names(coef(fit))
  es_coefs   <- coef_names[grepl("year_fe::", coef_names, fixed = TRUE)]
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
        clustid    = "sector",    # variable name string, not vector
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

# Unweighted refits
cat("Refitting without weights for bootstrap...\n")
es_main_unw <- map(names(OUTCOMES), refit_unweighted,
                   data    = reg_sf,
                   fe_spec = "cell_fe + year_fe") %>%
  setNames(names(OUTCOMES)) %>%
  compact()

es_robust_unw <- map(names(OUTCOMES), refit_unweighted_robust,
                     data    = reg_s,
                     fe_spec = "sector_fe + year_fe") %>%
  setNames(names(OUTCOMES)) %>%
  compact()

cat("Main unweighted:      ", paste(names(es_main_unw),   collapse = ", "), "\n")
cat("Robustness unweighted:", paste(names(es_robust_unw), collapse = ", "), "\n\n")

# Check coefficient structure before bootstrapping
cat("Event study coefficients per outcome (main spec):\n")
map_dfr(es_main_unw, function(fit) {
  cns <- names(coef(fit))
  es  <- cns[grepl("year_fe::", cns, fixed = TRUE)]
  tibble(n_coefs = length(es),
         years   = paste(gsub(".*::(\\d{4}).*", "\\1", es), collapse = ", "))
}, .id = "outcome") %>%
  print()
cat("\n")

# Run bootstrap
cat("Running bootstrap (B=9999) — may take several minutes...\n\n")

set.seed(42)
boot_ci_main <- map(es_main_unw, bootstrap_ci, B = 9999, seed = 42) %>%
  compact()
cat("Main bootstrap complete:", paste(names(boot_ci_main), collapse = ", "), "\n\n")

set.seed(42)
boot_ci_robust <- map(es_robust_unw, bootstrap_ci, B = 9999, seed = 42) %>%
  compact()
cat("Robustness bootstrap complete:", paste(names(boot_ci_robust), collapse = ", "), "\n\n")

# Flag any NA CIs
walk(names(boot_ci_main), function(nm) {
  n_na <- sum(is.na(boot_ci_main[[nm]]$conf.low))
  if (n_na > 0) warning(nm, ": ", n_na, " bootstrap CIs are NA.")
})

saveRDS(boot_ci_main,   file.path(pd, "bootstrap_ci_main.rds"))
saveRDS(boot_ci_robust, file.path(pd, "bootstrap_ci_robust.rds"))
cat("Bootstrap CIs saved.\n\n")


#===============================================================================
# STEP 5. Event Study Plots
#===============================================================================

out_path <- file.path(config$paths$outputs, config$output_stage,
                      config$out_subdirs$inequality_minwage)
dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

# MW event lines — position on numeric year axis
MW_EVENT_YEARS <- c(2017.5, 2019.75, 2021.75, 2023.5)

parse_year <- function(term) as.integer(regmatches(term, regexpr("\\d{4}", term)))

plot_event_study <- function(boot_tbl,
                             title    = NULL,
                             subtitle = NULL,
                             ref_year = 2016,
                             y_label  = "Coefficient on Exposure × Year") {
  if (is.null(boot_tbl) || nrow(boot_tbl) == 0) return(NULL)
  
  plot_data <- boot_tbl %>%
    mutate(year = parse_year(term)) %>%
    bind_rows(tibble(year = ref_year, estimate = 0,
                     conf.low = 0, conf.high = 0,
                     p.value = NA_real_)) %>%
    arrange(year) %>%
    mutate(
      sig = case_when(
        p.value < 0.01  ~ "p < 0.01",
        p.value < 0.05  ~ "p < 0.05",
        p.value < 0.10  ~ "p < 0.10",
        TRUE            ~ "n.s."
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
  "Shaded band = wild cluster bootstrap 95% CI (B=9999, Webb weights, unweighted refit).",
  "Clustered at sector level (10 clusters). COVID 2020Q1-2021Q2 excluded.",
  "Red dotted lines = minimum wage reform episodes (2017Q2, 2019Q3, 2021Q3, 2023Q2).",
  sep = "\n"
)

es_plots <- imap(OUTCOME_LABELS, function(label, nm) {
  if (!nm %in% names(boot_ci_main)) return(NULL)
  plot_event_study(
    boot_ci_main[[nm]],
    title    = paste("Minimum Wage Exposure:", label),
    subtitle = subtitle_main,
    y_label  = "Coefficient on Exposure × Year"
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
# STEP 6. Collapsed Window Regressions
#
# Pooling quarters into three windows:
#   pre          = 2014Q3-2016Q4   (baseline period, reference)
#   mid_2017_2019 = 2017Q1-2019Q2  (post-Event A, pre-Event B)
#   post_2021    = 2021Q1-2025Q2   (post-Event C; COVID excluded separately)
#
# NOTE: 2019Q3-2020Q4 is excluded — 2019Q3 is the Event B treatment quarter
# and 2020 is COVID. This gives clean pre/post windows for Events A and C.
# Event B's post period partially overlaps with Event C's pre period so it
# is absorbed into the mid window and should be interpreted accordingly.
#===============================================================================

add_window <- function(data) {
  data %>%
    mutate(
      year_num = as.integer(substr(time, 1, 4)),
      quarter_num = as.integer(substr(time, 6, 6)),
      time_idx = year_num + (quarter_num - 1) / 4,
      window = case_when(
        time_idx <  2017.25                          ~ "pre",
        time_idx >= 2017.25 & time_idx <  2019.50   ~ "mid_2017_2019",
        time_idx >= 2021.50                          ~ "post_2021",
        TRUE                                         ~ NA_character_   # excludes treatment Qs + COVID
      )
    ) %>%
    filter(!is.na(window)) %>%
    mutate(window = factor(window,
                           levels = c("pre", "mid_2017_2019", "post_2021")))
}

run_collapsed_main <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(
    as.formula(glue(
      "{outcome} ~ i(window, exposure_sf_val, ref = 'pre') | cell_fe + year_fe"
    )),
    data    = d,
    weights = ~pi,
    vcov    = ~sector
  )
}

run_collapsed_robust <- function(outcome, data) {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(d) < 20) return(NULL)
  feols(
    as.formula(glue(
      "{outcome} ~ i(window, exposure_baseline_val, ref = 'pre') | sector_fe + year_fe"
    )),
    data    = d,
    weights = ~pi_sector,
    vcov    = ~sector
  )
}

cat(strrep("=", 70), "\n")
cat("COLLAPSED WINDOW REGRESSIONS\n")
cat(strrep("=", 70), "\n\n")

reg_sf_win <- add_window(reg_sf)
reg_s_win  <- add_window(reg_s)

collapsed_main   <- map(names(OUTCOMES), run_collapsed_main, data = reg_sf_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

collapsed_robust <- map(names(OUTCOMES), run_collapsed_robust, data = reg_s_win) %>%
  setNames(names(OUTCOMES)) %>% compact()

cat("Main collapsed:", paste(names(collapsed_main), collapse = ", "), "\n")
cat("Robust collapsed:", paste(names(collapsed_robust), collapse = ", "), "\n\n")

for (nm in names(collapsed_main)) {
  cat("---", nm, "(collapsed, main) ---\n")
  print(summary(collapsed_main[[nm]]))
  cat("\n")
}


#===============================================================================
# STEP 7. Summary Tables
#===============================================================================

coef_map <- c(
  "window::mid_2017_2019:exposure_sf_val"   = "Exposure × Post-2017",
  "window::post_2021:exposure_sf_val"       = "Exposure × Post-2021",
  "window::mid_2017_2019:exposure_baseline_val" = "Exposure × Post-2017",
  "window::post_2021:exposure_baseline_val"     = "Exposure × Post-2021"
)

gof_map <- tribble(
  ~raw,            ~clean,        ~fmt,
  "nobs",          "Observations",  0,
  "r.squared",     "R²",            3,
  "adj.r.squared", "Adj. R²",       3
)

notes_main <- list(
  "Main spec: sector × firm size × quarter. Collapsed event study.",
  paste0("Exposure = share of formal workers within 10% above minimum wage, ",
         "2016 baseline (cell level, fixed over time)."),
  "Cell (sector × firm size) and year fixed effects. Weighted by baseline employment share (pi).",
  "Standard errors clustered at sector level (10 clusters). * p<0.10, ** p<0.05, *** p<0.01.",
  "COVID (2020Q1-2021Q2) excluded. Windows: pre = 2014Q3-2016Q4; post-2021 = 2021Q1-2025Q2.",
  "Event B treatment quarter (2019Q3) and event window gap excluded from mid window.",
  "Wild cluster bootstrap CIs available in event study plots (Webb weights, unweighted refit)."
)

modelsummary(
  collapsed_main,
  coef_map  = coef_map,
  gof_map   = gof_map,
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  vcov      = ~sector,
  title     = "Minimum Wage Exposure and Labor Market Outcomes",
  notes     = notes_main,
  output    = file.path(out_path, "table_main_collapsed.html")
)

modelsummary(
  collapsed_main,
  coef_map  = coef_map,
  gof_map   = gof_map,
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  vcov      = ~sector,
  title     = "Minimum Wage Exposure and Labor Market Outcomes",
  notes     = list("Collapsed event study. Cell and year FE.",
                   "Clustered by sector (10 clusters).",
                   "* p$<$0.10, ** p$<$0.05, *** p$<$0.01."),
  output    = file.path(out_path, "table_main_collapsed.tex")
)

modelsummary(
  collapsed_robust,
  coef_map  = coef_map,
  gof_map   = gof_map,
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  vcov      = ~sector,
  title     = "Robustness: Sector × Quarter Panel",
  notes     = list(
    "Robustness spec: sector × quarter. Exposure aggregated to sector level.",
    "Sector and year FE. Clustered by sector (10 clusters).",
    "* p<0.10, ** p<0.05, *** p<0.01."
  ),
  output    = file.path(out_path, "table_robustness_sector.html")
)

cat("Tables saved.\n\n")


#===============================================================================
# STEP 8. Robustness — Drop REVIEW Cells
#
# Re-runs collapsed regression with REVIEW cells also set to NA.
# Compares to main results — if similar, Review cells are not driving findings.
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
  
  reg_sf_strict <- reg_sf_win
  
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
  
  collapsed_strict <- map(names(OUTCOMES), run_collapsed_main,
                          data = reg_sf_strict) %>%
    setNames(names(OUTCOMES)) %>% compact()
  
  modelsummary(
    collapsed_strict,
    coef_map = coef_map,
    gof_map  = gof_map,
    stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    vcov     = ~sector,
    title    = "Robustness: Drop Review Cells",
    notes    = list("Review cells (one reliability concern) excluded.",
                    "All other settings identical to main spec."),
    output   = file.path(out_path, "table_robustness_strict.html")
  )
  
  cat("Strict robustness table saved.\n\n")
}

cat("=== 04_Regressions.R complete ===\n")

