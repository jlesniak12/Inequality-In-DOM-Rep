# ==============================================================================
# 04_regressions.R
#
# PURPOSE:
#   Event study regressions of minimum wage exposure on labor market outcomes.
#
# SPECIFICATIONS:
#
#   MAIN SPEC — sector × firm size × quarter
#     Y_sft = alpha_sf + delta_t + sum_{k!=2016} beta_k*(E_sf x 1[year=k]) + e_sft
#     alpha_sf = cell FE (sector x firm size)
#     delta_t  = year FE
#     E_sf     = 2016 baseline exposure at sector x firm size level
#     Cluster  = Employment_Sector (10 clusters, wild bootstrap for inference)
#     Weights  = sector_employment (survey-weighted employment by sector x quarter)
#
#   ROBUSTNESS — sector × quarter
#     Y_st = alpha_s + delta_t + sum_{k!=2016} beta_k*(E_s x 1[year=k]) + e_st
#     alpha_s = sector FE
#     E_s     = 2016 baseline exposure aggregated to sector level
#     Same clustering and weights as main spec
#
# OUTPUTS:
#   - bootstrap_ci_main.rds          (wild cluster bootstrap CIs, main spec)
#   - bootstrap_ci_robust.rds        (wild cluster bootstrap CIs, robustness)
#   - es_plot_[outcome].pdf          (event study plots)
#   - table_main_collapsed.html/.tex
#   - table_robustness_sector.html   (sector x quarter robustness)
#   - table_robustness_strict.html   (drops Review cells)
#
# NOTES:
#   - Wild bootstrap uses unweighted refit (fwildclusterboot does not support
#     weights). Point estimates from weighted models; CIs from unweighted.
#     Note this distinction when presenting results.
#   - informal outcome uses restricted subsample (Micro/Small, 6 sectors).
#     Exposure range 0.055-0.189. Not directly comparable to wage outcomes.
# ==============================================================================

source("Code/R/00_setup.R")

library(fixest)
library(modelsummary)
library(fwildclusterboot)
library(tidyverse)
library(glue)
library(kableExtra)

# ==============================================================================
# 0. LOAD PANELS
# ==============================================================================

# Main spec — load masked sf panel
panel_sf <- readRDS(
  file.path(config$paths$processed_data, "restricted_sector_time_panel.rds")
)


# Robustness spec — load aggregated sector panel
panel_s <- readRDS(
  file.path(config$paths$processed_data, "sector_time_panel.rds")
)


# Outcomes — same column names in both panels
outcomes <- c(
  log_var_wage     = "log_var_wage",
  log_50_10        = "log_50_10",
  log_90_10        = "log_90_10",
  below_min_formal = "below_min_formal",
  informal         = "informal"
)




# ==============================================================================
# 1. PREPARE REGRESSION DATASETS
# ==============================================================================

# -- main spec
reg_data_sf <- panel_sf %>%
  filter(!covid_flag) %>%
  mutate(
    cell_fe  = factor(cell_id),
    year_num = as.integer(str_sub(time, 1, 4)),
    year_fe  = relevel(factor(year_num), ref = "2016"),
    exposure = exposure_sf_val,
    sector   = factor(Employment_Sector)
  ) %>%
  filter(
    !is.na(exposure_sf_val),
    !is.na(year_fe),
    !is.na(cell_fe)
  )

#--- robustness
reg_data_s <- panel_s %>%
  filter(!covid_flag) %>%
  mutate(
    sector_fe = factor(group),
    year_num  = as.integer(str_sub(time, 1, 4)),
    year_fe   = relevel(factor(year_num), ref = "2016"),
    exposure  = exposure_baseline_val,
    sector    = factor(group)
  ) %>%
  filter(
    !is.na(exposure_baseline_val),
    !is.na(year_fe),
    !is.na(sector)
  )

cat("=== MAIN SPEC (sector x firmsize x quarter) ===\n")
cat("Reference year (must be 2016):", levels(reg_data_sf$year_fe)[1], "\n")
cat("Cells:", n_distinct(reg_data_sf$cell_fe), "\n")
cat("Sectors (clusters):", n_distinct(reg_data_sf$sector), "\n")
cat("Quarters:", n_distinct(reg_data_sf$time), "\n")
cat("Observations:", nrow(reg_data_sf), "\n\n")

cat("Non-missing per outcome (main spec):\n")
reg_data_sf %>%
  summarise(across(all_of(unname(outcomes)), ~ sum(!is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "n") %>%
  print()

cat("\n=== ROBUSTNESS SPEC (sector x quarter) ===\n")
cat("Sectors:", n_distinct(reg_data_s$sector), "\n")
cat("Quarters:", n_distinct(reg_data_s$time), "\n")
cat("Observations:", nrow(reg_data_s), "\n\n")


# ==============================================================================
# 2. EVENT STUDY HELPERS
# ==============================================================================

# Main spec: cell FE + year FE, exposure_sf_val
run_event_study_main <- function(outcome,
                                 data     = reg_data_sf,
                                 ref_year = "2016") {
  
  if (!outcome %in% names(data)) {
    message("'", outcome, "' not found — skipping.")
    return(NULL)
  }
  if (sum(!is.na(data[[outcome]])) < 20) {
    message("'", outcome, "' has too few non-NA obs — skipping.")
    return(NULL)
  }
  
  fml <- as.formula(glue(
    "{outcome} ~ i(year_fe, exposure, ref = '{ref_year}') | cell_fe + year_fe"
  ))
  
  feols(fml,
        data    = data,
        weights = ~sector_employment,
        vcov    = ~sector)            # cluster at sector level (10 clusters)
}

# Robustness spec: sector FE + year FE, exposure_baseline_val
run_event_study_robust <- function(outcome,
                                   data     = reg_data_s,
                                   ref_year = "2016") {
  
  if (!outcome %in% names(data)) {
    message("'", outcome, "' not found — skipping.")
    return(NULL)
  }
  if (sum(!is.na(data[[outcome]])) < 20) {
    message("'", outcome, "' has too few non-NA obs — skipping.")
    return(NULL)
  }
  
  fml <- as.formula(glue(
    "{outcome} ~ i(year_fe, exposure, ref = '{ref_year}') | sector_fe + year_fe"
  ))
  
  feols(fml,
        data    = data,
        weights = ~sector_employment,
        vcov    = ~sector)
}

# ==============================================================================
# 3. RUN EVENT STUDIES
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("RUNNING EVENT STUDIES — MAIN SPEC\n")
cat(strrep("=", 70), "\n\n")

es_main <- map(outcomes, run_event_study_main, data = reg_data_sf)
es_main <- compact(es_main)
cat("Estimated:", paste(names(es_main), collapse = ", "), "\n\n")

# Diagnostic: FE params vs effective clusters per outcome
cat("FE parameters vs effective clusters (main spec):\n")
map(outcomes, function(col) {
  reg_data_sf %>%
    filter(!is.na(.data[[col]])) %>%
    summarise(
      n_obs       = n(),
      n_cells     = n_distinct(cell_id),
      n_sectors   = n_distinct(Employment_Sector),
      n_fe_params = n_distinct(cell_id) + n_distinct(year_fe) - 1
    ) %>%
    mutate(outcome = col)
}) %>%
  bind_rows() %>%
  select(outcome, n_obs, n_cells, n_sectors, n_fe_params) %>%
  print()
cat("NOTE: n_sectors < n_fe_params for all outcomes — cluster SEs unreliable.\n")
cat("      Inference will be based on wild cluster bootstrap (section 4).\n\n")

# Print weighted model summaries for reference
# (point estimates are valid; SEs are approximate due to non-PSD VCOV)
for (nm in names(es_main)) {
  cat("---", nm, "---\n")
  print(summary(es_main[[nm]]))
  cat("\n")
}

cat(strrep("=", 70), "\n")
cat("RUNNING EVENT STUDIES — ROBUSTNESS SPEC\n")
cat(strrep("=", 70), "\n\n")

es_robust <- map(outcomes, run_event_study_robust, data = reg_data_s)
es_robust <- compact(es_robust)
cat("Estimated:", paste(names(es_robust), collapse = ", "), "\n\n")


# Check cluster lengths match for one model
fit <- es_main_unw[["log_var_wage"]]
cat("Model nobs:", nobs(fit), "\n")
cat("reg_data_sf nrows:", nrow(reg_data_sf), "\n")
cat("Filtered nrows:", nrow(reg_data_sf %>% filter(!is.na(log_var_wage))), "\n")

# ==============================================================================
# 4. WILD CLUSTER BOOTSTRAP
#
# fwildclusterboot does not support weights — refit without weights.
# Point estimates come from weighted models (section 3).
# CIs and p-values come from unweighted bootstrap refits here.
# Note this distinction when presenting results.
#
# Webb weights used throughout (recommended for <= 10 clusters).
# log_50_10, log_90_10, and informal have only 6 effective clusters —
# right at the Webb minimum. CIs for these outcomes should be interpreted
# cautiously. Bootstrap may produce NA for some early pre-period coefficients
# if those years have very few contributing cells.
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("WILD CLUSTER BOOTSTRAP\n")
cat(strrep("=", 70), "\n\n")

refit_unweighted <- function(outcome, data, fe_spec) {
  if (!outcome %in% names(data)) return(NULL)
  fit_data <- data %>% filter(!is.na(.data[[outcome]]))
  if (nrow(fit_data) < 20) return(NULL)
  
  fit <- feols(
    as.formula(glue("{outcome} ~ i(year_fe, exposure, ref = '2016') | {fe_spec}")),
    data = fit_data,
    vcov = ~sector
  )
  
  # Attach fitted data so bootstrap_ci can pass correct rows to boottest()
  attr(fit, "fit_data") <- fit_data
  fit
}

bootstrap_ci <- function(fit,
                         B          = 9999,
                         seed       = 42,
                         conf_level = 0.95) {
  
  coef_names <- names(coef(fit))
  es_coefs   <- coef_names[str_detect(coef_names, "year_fe::")]
  if (length(es_coefs) == 0) return(NULL)
  
  # Extract cluster vector from the fitted data stored as attribute
  # This ensures length matches the model's actual observations
  fit_data <- attr(fit, "fit_data")
  if (is.null(fit_data)) {
    stop("fit_data attribute missing — refit using refit_unweighted()")
  }
  cluster_vec <- as.character(fit_data[["sector"]])
  
  set.seed(seed)
  dqrng::dqset.seed(seed)
  
  results <- map(es_coefs, function(cn) {
    bt <- tryCatch(
      fwildclusterboot::boottest(
        object      = fit,
        param       = cn,
        B           = B,
        bootcluster = cluster_vec,   # pass vector directly, not string
        type        = "webb",
        sign_level  = 1 - conf_level
      ),
      error = function(e) {
        warning("Bootstrap failed for ", cn, ": ", e$message)
        NULL
      }
    )
    if (is.null(bt)) return(NULL)
    tidy_bt <- generics::tidy(bt)
    tibble(
      term      = cn,
      estimate  = coef(fit)[cn],
      conf.low  = tidy_bt$conf.low,
      conf.high = tidy_bt$conf.high,
      p.value   = tidy_bt$p.value
    )
  })
  
  result <- bind_rows(results)
  
  n_failed <- sum(is.na(result$conf.low) | is.na(result$conf.high))
  if (n_failed > 0) {
    warning(n_failed, " coefficients returned NA CIs.")
  }
  
  result
}
# --- Unweighted refits ---
cat("Refitting models without weights for bootstrap...\n")

es_main_unw <- map(outcomes, refit_unweighted,
                   data    = reg_data_sf,
                   fe_spec = "cell_fe + year_fe")
es_main_unw <- compact(es_main_unw)
cat("Main spec unweighted models:", paste(names(es_main_unw), collapse = ", "), "\n\n")

es_robust_unw <- map(outcomes, refit_unweighted,
                     data    = reg_data_s,
                     fe_spec = "sector_fe + year_fe")
es_robust_unw <- compact(es_robust_unw)
cat("Robustness unweighted models:", paste(names(es_robust_unw), collapse = ", "), "\n\n")

# --- Verify coefficient names before bootstrapping ---
cat("Checking event study coefficients per outcome:\n")
map(es_main_unw, function(fit) {
  coef_names <- names(coef(fit))
  es_coefs   <- coef_names[str_detect(coef_names, "year_fe::")]
  tibble(n_coefs = length(es_coefs),
         first   = first(es_coefs),
         last    = last(es_coefs))
}) %>%
  bind_rows(.id = "outcome") %>%
  print()
cat("\n")

# --- Run bootstrap ---
cat("Running bootstrap (B=9999) — this will take several minutes...\n\n")

set.seed(42); dqrng::dqset.seed(42)
boot_ci_main <- map(es_main_unw, bootstrap_ci, B = 9999, seed = 42)
boot_ci_main <- compact(boot_ci_main)
cat("Main spec bootstrap complete:",
    paste(names(boot_ci_main), collapse = ", "), "\n\n")

set.seed(42); dqrng::dqset.seed(42)
boot_ci_robust <- map(es_robust_unw, bootstrap_ci, B = 9999, seed = 42)
boot_ci_robust <- compact(boot_ci_robust)
cat("Robustness bootstrap complete:",
    paste(names(boot_ci_robust), collapse = ", "), "\n\n")

saveRDS(boot_ci_main,
        file.path(config$paths$processed_data, "bootstrap_ci_main.rds"))
saveRDS(boot_ci_robust,
        file.path(config$paths$processed_data, "bootstrap_ci_robust.rds"))

cat("Bootstrap CIs saved.\n\n")

# ==============================================================================
# 5. EVENT STUDY PLOTS
# ==============================================================================

parse_year <- function(term) as.integer(str_extract(term, "\\d{4}"))

plot_event_study <- function(boot_tbl,
                             title    = NULL,
                             subtitle = NULL,
                             ref_year = 2016,
                             y_label  = "Coefficient") {
  
  if (is.null(boot_tbl) || nrow(boot_tbl) == 0) return(NULL)
  
  plot_data <- boot_tbl %>%
    mutate(year = parse_year(term)) %>%
    bind_rows(tibble(year = ref_year, estimate = 0,
                     conf.low = 0, conf.high = 0, p.value = NA_real_)) %>%
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
             ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey50") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.15, fill = "#2166ac") +
    geom_line(colour = "#2166ac", linewidth = 0.7) +
    geom_point(aes(colour = sig, shape = sig), size = 2.5) +
    geom_vline(xintercept = c(2017.5, 2019.75, 2021.75, 2023.5),
               linetype = "dotted", colour = "red", alpha = 0.5) +
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
    labs(title = title, subtitle = subtitle,
         x = NULL, y = y_label,
         colour = "Significance", shape = "Significance") +
    theme_surveytools() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

subtitle_main <- paste0(
  "Main spec: sector x firm size x quarter. 2016 base period.\n",
  "Wild cluster bootstrap 95% CI (B=9999, Webb weights, unweighted refit).\n",
  "Clustered at sector level (10 clusters). COVID excluded.\n",
  "Red dotted lines = minimum wage reform episodes."
)

plot_titles <- list(
  log_var_wage     = "Log Variance of Wages",
  log_50_10        = "Log(p50/p10) Wage Ratio",
  log_90_10        = "Log(p90/p10) Wage Ratio",
  below_min_formal = "Formal Sector Non-Compliance Rate",
  informal         = "Informality Share (Micro/Small, 6 sectors)"
)

es_plots <- imap(plot_titles, function(title, nm) {
  if (!nm %in% names(boot_ci_main)) return(NULL)
  plot_event_study(
    boot_ci_main[[nm]],
    title    = paste("Minimum Wage Exposure:", title),
    subtitle = subtitle_main,
    y_label  = "Coefficient on Exposure x Year"
  )
})
es_plots <- compact(es_plots)

walk2(es_plots, names(es_plots), function(p, nm) {
  path <- file.path(config$paths$output, paste0("es_plot_", nm, ".pdf"))
  ggsave(path, p, width = 9, height = 5.5)
  cat("Saved:", path, "\n")
})

config$out

# ==============================================================================
# 6. COLLAPSED WINDOW REGRESSIONS
#
# Three windows: pre-2017 (ref), 2017-2019, post-2021.
# Run for both main spec and robustness spec.
# ==============================================================================

add_window <- function(data) {
  data %>%
    mutate(
      year_num = as.integer(as.character(year_fe)),
      window   = case_when(
        year_num < 2017                     ~ "pre",
        year_num >= 2017 & year_num <= 2019 ~ "mid_2017_2019",
        year_num >= 2021                    ~ "post_2021",
        TRUE                                ~ NA_character_
      )
    ) %>%
    filter(!is.na(window)) %>%
    mutate(window = factor(window,
                           levels = c("pre", "mid_2017_2019", "post_2021")))
}

reg_collapsed_sf <- add_window(reg_data_sf)
reg_collapsed_s  <- add_window(reg_data_s)

run_collapsed <- function(outcome, data, fe_spec) {
  if (!outcome %in% names(data)) return(NULL)
  if (sum(!is.na(data[[outcome]])) < 20) return(NULL)
  feols(
    as.formula(glue(
      "{outcome} ~ i(window, exposure, ref = 'pre') | {fe_spec}"
    )),
    data    = data,
    weights = ~sector_employment,
    vcov    = ~sector
  )
}

cat(strrep("=", 70), "\n")
cat("COLLAPSED WINDOW REGRESSIONS\n")
cat(strrep("=", 70), "\n\n")

collapsed_main <- map(outcomes, run_collapsed,
                      data    = reg_collapsed_sf,
                      fe_spec = "cell_fe + year_fe")
collapsed_main <- compact(collapsed_main)

collapsed_robust <- map(outcomes, run_collapsed,
                        data    = reg_collapsed_s,
                        fe_spec = "sector_fe + year_fe")
collapsed_robust <- compact(collapsed_robust)

cat("Main spec collapsed — estimated:", paste(names(collapsed_main), collapse = ", "), "\n")
cat("Robustness collapsed — estimated:", paste(names(collapsed_robust), collapse = ", "), "\n\n")

# ==============================================================================
# 7. SUMMARY TABLES
# ==============================================================================

coef_map <- c(
  "window::mid_2017_2019:exposure" = "Exposure x Post-2017",
  "window::post_2021:exposure"     = "Exposure x Post-2021"
)

gof_map <- tribble(
  ~raw,            ~clean,         ~fmt,
  "nobs",          "Observations",  0,
  "r.squared",     "R2",            3,
  "adj.r.squared", "Adj. R2",       3
)

primary_outcomes <- c("log_var_wage", "log_50_10", "below_min_formal", "informal")

# --- Main table ---
primary_main <- collapsed_main[intersect(primary_outcomes, names(collapsed_main))]

names(primary_main) <- c(
  "Log wage\\\\variance",
  "Log(p50/p10)",
  "Non-compliance\\\\rate",
  "Informality\\\\share"
)[seq_along(primary_main)]

notes_main <- list(
  "Main spec: sector x firm size x quarter. Collapsed event study.",
  "Exposure = share of formal workers within 10% above minimum wage, 2016 baseline (cell level).",
  "Cell (sector x firm size) and year fixed effects in all specifications.",
  "Standard errors clustered at sector level (10 clusters). * p<0.10, ** p<0.05, *** p<0.01.",
  "COVID (2020) excluded. Pre = 2014Q3-2016Q4; Post-2021 = 2021Q1-2025Q2.",
  "Informality: Micro/Small tiers, 6 sectors, exposure range 0.055-0.189.",
  "Bootstrap CIs (Webb weights, unweighted refit) available in event study plots."
)

modelsummary(primary_main, coef_map = coef_map, gof_map = gof_map,
             stars  = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
             vcov   = ~sector,
             title  = "Minimum Wage Exposure and Labor Market Outcomes",
             notes  = notes_main,
             output = file.path(config$paths$output, "table_main_collapsed.html"))

modelsummary(primary_main, coef_map = coef_map, gof_map = gof_map,
             stars  = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
             vcov   = ~sector,
             title  = "Minimum Wage Exposure and Labor Market Outcomes",
             notes  = list("Collapsed event study, cell and year FE.",
                           "Clustered by sector (10 clusters).",
                           "* p$<$0.10, ** p$<$0.05, *** p$<$0.01."),
             output = file.path(config$paths$output, "table_main_collapsed.tex"))

# --- Robustness: sector x quarter ---
primary_robust <- collapsed_robust[intersect(primary_outcomes, names(collapsed_robust))]

names(primary_robust) <- c(
  "Log wage\\\\variance",
  "Log(p50/p10)",
  "Non-compliance\\\\rate",
  "Informality\\\\share"
)[seq_along(primary_robust)]

modelsummary(primary_robust, coef_map = coef_map, gof_map = gof_map,
             stars  = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
             vcov   = ~sector,
             title  = "Robustness: Sector x Quarter Panel",
             notes  = list(
               "Robustness spec: sector x quarter.",
               "Exposure aggregated to sector level using baseline pi weights.",
               "Sector and year FE. Clustered by sector (10 clusters).",
               "* p<0.10, ** p<0.05, *** p<0.01."
             ),
             output = file.path(config$paths$output, "table_robustness_sector.html"))

cat("Tables saved.\n\n")

# ==============================================================================
# 8. ROBUSTNESS — DROP REVIEW CELLS
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("ROBUSTNESS: DROP REVIEW CELLS\n")
cat(strrep("=", 70), "\n\n")

review_cells <- readRDS(
  file.path(config$paths$processed_data, "validation_decision_table.rds")
) %>%
  filter(str_detect(recommendation, "Review"))

if (nrow(review_cells) == 0) {
  cat("No Review cells — robustness check not needed.\n")
} else {
  
  reg_data_strict <- reg_data_sf
  
  for (out_name in unique(review_cells$outcome)) {
    col <- unlist(outcomes[out_name])
    if (is.null(col) || !col %in% names(reg_data_strict)) next
    drop_cells <- review_cells %>%
      filter(outcome == out_name) %>%
      mutate(cell_id = paste0(Employment_Sector, "__", Wage_group)) %>%
      pull(cell_id)
    reg_data_strict <- reg_data_strict %>%
      mutate(!!col := if_else(cell_id %in% drop_cells, NA_real_, .data[[col]]))
  }
  
  collapsed_strict <- map(outcomes, run_collapsed,
                          data    = add_window(reg_data_strict),
                          fe_spec = "cell_fe + year_fe")
  collapsed_strict <- compact(collapsed_strict)
  
  modelsummary(
    collapsed_strict[intersect(primary_outcomes, names(collapsed_strict))],
    coef_map = coef_map, gof_map = gof_map,
    stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    vcov     = ~sector,
    title    = "Robustness: Drop Review Cells",
    output   = file.path(config$paths$output, "table_robustness_strict.html")
  )
  
  cat("Robustness (strict) table saved.\n")
}

