
install.packages("kableExtra")
install.packages('fwildclusterboot', repos = 'https://s3alfisc.r-universe.dev')

install.packages("JuliaConnectoR")
install.packages("collapse")
install.packages("dqrng")
install.packages("gtools")


source("Code/R/00_setup.R")

library(fixest)
library(modelsummary)
library(openxlsx)
library(kableExtra)
library(fwildclusterboot)   # wild cluster bootstrap for few clusters




# 0. Load panel

panel <- readRDS(file.path(config$paths$processed_data, "sector_time_panel.rds"))


# 1. Prepare regression dataset

reg_data <-panel %>%
  # Drop COVID period from main specification — add back for robustness
  filter(!time %in% c("2020Q1", "2020Q2", "2020Q3", "2020Q4")) %>%
  mutate(
    # Sector FE identifier
    sector = factor(group),
    
    # Time FE — year_fct already has 2016 as reference level
    # Confirm: levels(year_fct)[1] should be "2016"
    year_fe = year_fct,
    
    # Continuous treatment: demeaned exposure (aids interpretation of intercept)
    exposure = exposure_baseline_val,
    exposure_dm = exposure_baseline_val - mean(exposure_baseline_val, na.rm = TRUE),
    
    # Interaction terms: exposure x year dummy
    # fixest i() syntax handles this cleanly with ref period
    
    # Quarter FE for within-year seasonality (optional — include in robustness)
    quarter_fe = factor(quarter_num)
  ) %>%
  filter(
    !is.na(exposure_baseline_val),
    !is.na(year_fct),
    !is.na(sector)
  )

# Verify base period
cat("Reference year (should be 2016):", levels(reg_data$year_fct)[1], "\n")
cat("Sectors in regression:", n_distinct(reg_data$sector), "\n")
cat("Quarters in regression:", n_distinct(reg_data$time), "\n")
cat("Observations:", nrow(reg_data), "\n")



# -----------------------------------------------------------------------------
# 2. Event study specification — fixest::feols
#
# Model: Y_st = alpha_s + delta_t + sum_{k != 2016} beta_k*(E_s x 1[year=k]) + e
#
# fixest i(year_fe, exposure, ref = "2016") creates the full set of
# year x exposure interactions with 2016 as the omitted base period.
# sector and year_fe absorbed as high-dimensional FE.
# -----------------------------------------------------------------------------

# --- Helper: run one event study and return fixest object ---
run_event_study <- function(outcome,
                            data      = reg_data,
                            ref_year  = "2016",
                            weights   = "sector_employment",
                            covid_excl = TRUE) {
  
  fml <- as.formula(
    glue::glue(
      "{outcome} ~ i(year_fe, exposure, ref = '{ref_year}') |
       sector + year_fe"
    )
  )
  
  feols(
    fml,
    data    = data,
    weights = ~ sector_employment,
    # Cluster SE by sector — note: only 10 clusters, use wild bootstrap
    # vcov supplied separately via fwildclusterboot; here use standard cluster
    # for initial estimates and replace SEs via boottest() below
    vcov    = ~ sector
  )
}



# -----------------------------------------------------------------------------
# 3. Run regressions for each outcome
# -----------------------------------------------------------------------------

outcomes <- list(
  # Wage inequality
  var_formal    = "var_logwage_formal",
  var_informal  = "var_logwage_informal",
  var_overall   = "var_logwage",
  log_50_10     = "log_50_10",
  log_90_10     = "log_90_10",
  # Employment margins
  informal      = "informal",
  below_min     = "below_min",
  below_min_f   = "below_min_formal"
)

# Run all event studies
event_studies <- map(outcomes, run_event_study, data = reg_data)

# Quick check — print first model
summary(event_studies$var_formal)



#-----------------------------------------------------------------------------
  # 4. Wild cluster bootstrap SEs
  #
  # With only 10 clusters (sectors), conventional cluster-robust SEs are
  # unreliable. Wild cluster bootstrap (Webb weights recommended for < 10
  # clusters; here use Rademacher for 10 clusters per MacKinnon & Webb 2018).
  #
  # fwildclusterboot::boottest() resamples residuals at cluster level.
  # We run bootstrap for each coefficient separately (one hypothesis at a time).
  # For the full event study plot we extract bootstrap CIs for each year x
  # exposure interaction term.
  # -----------------------------------------------------------------------------

# --- Helper: extract bootstrap CIs for all event study coefficients ---
bootstrap_event_study_ci <- function(fit,
                                     B          = 9999,
                                     seed       = 42,
                                     conf_level = 0.95) {
  
  coef_names <- names(coef(fit))
  es_coefs   <- coef_names[str_detect(coef_names, "year_fe::")]
  
  # Set BOTH seeds as required by fwildclusterboot
  set.seed(seed)
  dqrng::dqset.seed(seed)
  
  boot_results <- map(es_coefs, function(cn) {
    
    bt <- tryCatch(
      fwildclusterboot::boottest(
        object     = fit,
        param      = cn,
        B          = B,
        clustid    = "sector",
        type       = "webb",
        sign_level = 1 - conf_level
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
  
  bind_rows(boot_results)
}




# Refit models WITHOUT weights — for bootstrap only
event_studies_unw <- map(
  outcomes[c("var_formal", "var_informal", "informal", "below_min_f")],
  function(outcome) {
    feols(
      as.formula(glue::glue(
        "{outcome} ~ i(year_fe, exposure, ref = '2016') | sector + year_fe"
      )),
      data = reg_data %>% 
        filter(!is.na(.data[[outcome]])),  # pre-filter NAs per outcome
      vcov = ~ sector
      # NO weights argument
    )
  }
)

# Run bootstrap on unweighted models
set.seed(42)
dqrng::dqset.seed(42)

boot_ci <- map(
  event_studies_unw,
  bootstrap_event_study_ci,
  B    = 9999,
  seed = 42
)


saveRDS(boot_ci,
        file.path(config$paths$processed_data, "bootstrap_ci.rds"))



# To reload: boot_ci <- readRDS(file.path(config$paths$processed_data, "bootstrap_ci.rds"))


# -----------------------------------------------------------------------------
# 5. Event study plots
# -----------------------------------------------------------------------------

# --- Helper: parse year from fixest interaction coefficient name ---
parse_year <- function(term) {
  # term looks like "year_fe::2017:exposure" or "year_fe::2015:exposure"
  as.integer(str_extract(term, "\\d{4}"))
}

# --- Helper: plot one event study with bootstrap CIs ---
plot_event_study <- function(boot_tbl,
                             title    = NULL,
                             subtitle = NULL,
                             ref_year = 2016,
                             y_label  = "Coefficient") {
  
  plot_data <- boot_tbl %>%
    mutate(
      year     = parse_year(term),
      # Add reference year at zero
    ) %>%
    bind_rows(
      tibble(year = ref_year, estimate = 0,
             conf.low = 0, conf.high = 0, p.value = NA)
    ) %>%
    arrange(year) %>%
    mutate(
      sig = case_when(
        p.value < 0.01 ~ "p < 0.01",
        p.value < 0.05 ~ "p < 0.05",
        p.value < 0.10 ~ "p < 0.10",
        TRUE           ~ "n.s."
      ),
      sig = factor(sig, levels = c("p < 0.01", "p < 0.05",
                                   "p < 0.10", "n.s."))
    )
  
  ggplot(plot_data, aes(x = year, y = estimate)) +
    # Zero reference line
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50",
               linewidth = 0.4) +
    # Reference year shading
    annotate("rect",
             xmin = ref_year - 0.5, xmax = ref_year + 0.5,
             ymin = -Inf, ymax = Inf,
             alpha = 0.08, fill = "grey50") +
    # COVID shading — add back if not dropping COVID
    # annotate("rect", xmin = 2020, xmax = 2021,
    #          ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "steelblue") +
    # Confidence intervals
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.15, fill = "#2166ac") +
    geom_line(colour = "#2166ac", linewidth = 0.7) +
    geom_point(aes(colour = sig, shape = sig), size = 2.5) +
    # Reform vertical lines
    geom_vline(
      xintercept = c(2017.5, 2019.75, 2021.75, 2023.5),
      linetype = "dotted", colour = "red", alpha = 0.5
    ) +
    scale_colour_manual(
      values = c("p < 0.01" = "#d73027",
                 "p < 0.05" = "#fc8d59",
                 "p < 0.10" = "#fee090",
                 "n.s."     = "grey60"),
      drop = FALSE
    ) +
    scale_shape_manual(
      values = c("p < 0.01" = 16, "p < 0.05" = 16,
                 "p < 0.10" = 17, "n.s." = 1),
      drop = FALSE
    ) +
    scale_x_continuous(breaks = seq(2014, 2025, 1)) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = NULL,
      y        = y_label,
      colour   = "Significance",
      shape    = "Significance"
    ) +
    theme_surveytools() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generate plots
p_es_var_formal <- plot_event_study(
  boot_ci$var_formal,
  title    = "Effect of Minimum Wage Exposure on Formal Wage Variance",
  subtitle = "Event study coefficients, 2016 base period. Wild cluster bootstrap 95% CI (B=9999).\nCovid period (2020) excluded. Reform episodes marked with red dotted lines.",
  y_label  = "Coefficient on Exposure × Year"
)

p_es_var_informal <- plot_event_study(
  boot_ci$var_informal,
  title    = "Effect of Minimum Wage Exposure on Informal Wage Variance",
  subtitle = "Event study coefficients, 2016 base period. Wild cluster bootstrap 95% CI (B=9999).\nRestricted to sectors with reliable informal estimates.",
  y_label  = "Coefficient on Exposure × Year"
)

p_es_informal <- plot_event_study(
  boot_ci$informal,
  title    = "Effect of Minimum Wage Exposure on Informality Share",
  subtitle = "Event study coefficients, 2016 base period. Wild cluster bootstrap 95% CI (B=9999).",
  y_label  = "Coefficient on Exposure × Year"
)

p_es_compliance <- plot_event_study(
  boot_ci$below_min_f,
  title    = "Effect of Minimum Wage Exposure on Formal Non-Compliance Rate",
  subtitle = "Event study coefficients, 2016 base period. Wild cluster bootstrap 95% CI (B=9999).",
  y_label  = "Coefficient on Exposure × Year"
)




# -----------------------------------------------------------------------------
# 6. Summary table — aggregate pre/post estimates
#
# Rather than showing all ~44 event study coefficients in a table,
# collapse to three windows: pre-2017, 2017-2019, post-2021.
# This gives a cleaner summary table alongside the event study plots.
# -----------------------------------------------------------------------------

# --- Helper: run collapsed window regression ---
# Adds three dummy variables: pre (pre-2017), mid (2017-2019), post (post-2021)
# interacted with exposure. This gives a single coefficient per window.

reg_data_collapsed <- reg_data %>%
  mutate(
    window = case_when(
      year_num < 2017               ~ "pre",
      year_num >= 2017 & year_num <= 2019 ~ "mid_2017_2019",
      year_num >= 2021              ~ "post_2021",
      TRUE                          ~ NA_character_   # 2020 excluded
    )
  ) %>%
  filter(!is.na(window)) %>%
  mutate(window = factor(window,
                         levels = c("pre", "mid_2017_2019", "post_2021")))

run_collapsed <- function(outcome, data = reg_data_collapsed) {
  fml <- as.formula(
    glue::glue(
      "{outcome} ~ i(window, exposure, ref = 'pre') | sector + year_fe"
    )
  )
  feols(fml, data = data,
        weights = ~ sector_employment,
        vcov    = ~ sector)
}

collapsed_models <- map(outcomes, run_collapsed, data = reg_data_collapsed)




# -----------------------------------------------------------------------------
# 7. Journal-style table — modelsummary
# -----------------------------------------------------------------------------

# Select primary outcomes for main table
primary_outcomes <- c("var_formal", "var_informal",
                      "informal", "below_min_f")

primary_models <- collapsed_models[primary_outcomes]

# Rename models for column headers
names(primary_models) <- c(
  "Formal wage\\\\variance",
  "Informal wage\\\\variance",
  "Informality\\\\share",
  "Non-compliance\\\\rate"
)

# Coefficient map — rename interaction terms
coef_map <- c(
  "window::mid_2017_2019:exposure" = "Exposure × Post-2017",
  "window::post_2021:exposure"     = "Exposure × Post-2021"
)

# Custom statistics rows
gof_map <- tribble(
  ~raw,             ~clean,              ~fmt,
  "nobs",           "Observations",       0,
  "r.squared",      "R²",                 3,
  "adj.r.squared",  "Adj. R²",            3,
  "FE: sector",     "Sector FE",          0,
  "FE: year_fe",    "Year FE",            0
)

# Generate table
tbl_main <- modelsummary(
  primary_models,
  coef_map    = coef_map,
  gof_map     = gof_map,
  stars       = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  vcov        = ~ sector,          # cluster SE by sector
  title       = "Minimum Wage Exposure and Labor Market Outcomes",
  notes       = list(
    "Notes: Collapsed event study estimates. Unit of observation is sector × quarter.",
    "Exposure measure is share of formal workers within 10% above applicable minimum wage in 2016 baseline.",
    "All specifications include sector and year fixed effects.",
    "Standard errors clustered by sector (10 clusters). * p<0.10, ** p<0.05, *** p<0.01.",
    "COVID period (2020) excluded. Pre-period = 2014Q3–2016Q4; Post-2021 = 2021Q1–2025Q2."
  ),
  output      = "kableExtra"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width        = FALSE,
    font_size         = 11
  ) %>%
  add_header_above(
    c(" " = 1,
      "Wage Inequality" = 2,
      "Employment Margins" = 2)
  ) %>%
  row_spec(0, bold = TRUE) %>%
  pack_rows("Panel A: Reform effects", 1, 2) %>%
  pack_rows("Model statistics", 3, 7)

# Print table
tbl_main

# Save as HTML for inspection
modelsummary(
  primary_models,
  coef_map  = coef_map,
  gof_map   = gof_map,
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  vcov      = ~ sector,
  title     = "Minimum Wage Exposure and Labor Market Outcomes",
  notes     = list(
    "Collapsed event study estimates. Unit of observation is sector × quarter.",
    "Exposure = share of formal workers within 10% above applicable minimum wage, 2016 baseline.",
    "All specifications include sector and year fixed effects.",
    "Standard errors clustered by sector (10 clusters). * p<0.10, ** p<0.05, *** p<0.01.",
    "COVID period (2020) excluded."
  ),
  output    = file.path(config$paths$output, "table_main_collapsed.html")
)



# Save as LaTeX for paper
modelsummary(
  primary_models,
  coef_map  = coef_map,
  gof_map   = gof_map,
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  vcov      = ~ sector,
  title     = "Minimum Wage Exposure and Labor Market Outcomes",
  notes     = list(
    "Collapsed event study estimates.",
    "Standard errors clustered by sector (10 clusters).",
    "* p$<$0.10, ** p$<$0.05, *** p$<$0.01."
  ),
  output    = file.path(config$paths$output, "table_main_collapsed.tex")
)

