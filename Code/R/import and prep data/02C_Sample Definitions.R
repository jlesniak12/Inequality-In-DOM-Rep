#===============================================================================
#
#
# Purpose: Define subsamples and their corresponding survey
#          designs in one place. Every downstream script sources this file
#          (after sourcing 00_setup.R) instead of re-filtering independently.
#
#          This eliminates filter drift — the risk that two scripts computing
#          "the same" sample apply subtly different conditions and produce
#          incomparable estimates.
#
# USAGE IN DOWNSTREAM SCRIPTS:
#
#   source("Code/R/00_setup.R")
#   source("Code/R/02C_Sample Definitions.R")
#
#   # Use the data frame directly (e.g. for KDE, dplyr operations)
#   samples$employed$data
#
#   # Use the survey design for weighted estimates
#   svymean(~real_salary_income_primary, samples$wage_earners$design)
#
#   # Subset a design further without re-loading
#   subset(samples$private_employees$design, Employment_Status == "Formal")
#
# OUTPUT OBJECT: `samples`
#   A named list where each element is itself a list with two slots:
#     $data    — filtered data frame (use for non-survey operations)
#     $design  — svydesign object built from $data (use for survey estimates)
#
# SAMPLES Available
#
#   1.  all_individuals        All survey respondents
#   2.  working_age            Age 15-64 (standard WAP threshold)
#   3.  active_pop             Economically active (PEA == 1): employed + unemployed
#   4.  employed               All employed (OCUPADO == 1)
#   5.  wage_earners           Employed salaried workers (private + public),
#                              positive real salary income.
#                              Use for: log-wage distributions, inequality, Kaitz denominator.
#   6.  income_earners         All employed with positive real TOTAL primary labor income
#                              (salary + independent). Includes informal self-employed.
#                              Use for: income inequality incl. informal sector.
#   7.  private_employees      Employed private-sector employees (not self-employed,
#                              not government). No income filter.
#                              Use for: employment shares, formality rates by sector.
#   8.  private_employees_inc  Private employees with positive real compliance income.
#                              Use for: compliance analysis, distribution figures.
#   9.  regression_sample      Private employees, positive compliance income, known
#                              firm size (Wage_group), Government and Electricity &
#                              Water excluded. Hours > 0 required for hourly measures.
#                              This is the CAUSAL IDENTIFICATION SAMPLE.
#
# WEIGHT CONVENTION:
#   All designs use FACTOR_EXPANSION (quarterly weight) throughout.
#   An annual weight (FACTOR_EXPANSION / 4) is available as weight_annual
#   in the data if annual pooling is needed, but quarterly is the primary unit.
#
# SURVEY DESIGN NOTES:
#   id      = ~psu_unique      (unique PSU ID created by check_and_fix_survey_ids)
#   strata  = ~strata_unique   (unique stratum ID, same function)
#   weights = ~FACTOR_EXPANSION
#   nest    = TRUE             (PSUs nested within strata — required for ENCFT)
#
# SECTORS EXCLUDED FROM REGRESSION SAMPLE:
#   Government         — different MW schedule; not bound by private MW tiers
#   Electricity & Water — extreme sparsity in sector × firm size × quarter cells
#
#===============================================================================



source("Code/R/setup/00_setup.R")



#===============================================================================
# STEP 1.  Load clean survey panel
#===============================================================================

Full_ENCFT_clean <- readRDS(
  file.path(config$paths$processed_data, "Full_ENCFT_clean.rds")
)



#===============================================================================
# STEP 2.  Define sample filters
#
#  Each filter is a named list with:
#    $label   — human-readable description (for messages and documentation)
#    $filter  — a dplyr filter expression applied to Full_ENCFT_clean
#
#  Sectors excluded from the regression sample
#===============================================================================

EXCL_SECTORS_REGRESSION <- c("Government", "Electricity and Water")

#age filter 
Full_ENCFT_clean <- Full_ENCFT_clean %>%
  mutate(age_band = EDAD >= 15 & EDAD <=64)

# --- Now your SAMPLE_SPECS will work perfectly ---
# because filter() will find the column 'age_band' inside the data.
# Wage_group values that represent a known firm size tier
TIER_LEVELS <- c("Micro", "Small", "Medium", "Large")

SAMPLE_SPECS <- list(
  
  # ── 1. All survey respondents ───────────────────────────────────────────────
  all_individuals = list(
    label  = "All survey respondents",
    filter = quote(TRUE)
  ),
  
  # ── 2. Working-age population (15 -64) ─────────────────────────────────────────
  working_age = list(
    label  = "Working-age population (age 15 - 64)",
    filter = quote(age_band)
  ),
  
  # ── 3. Economically active population ───────────────────────────────────────
  # PEA == 1: employed or actively seeking work (ILO definition as coded in the
  # ENCFT). Use as denominator for unemployment rates.
  active_pop = list(
    label  = "Economically active population (PEA == 1, Age 15-64)",
    filter = quote(PEA == 1 &
                   age_band)
  ),
  
  # ── 4. All employed ─────────────────────────────────────────────────────────
  # OCUPADO == 1: has a job in the reference week. No income filter — income
  # conditioning introduces selection bias when computing employment shares,
  # informality rates, and sector composition.
  employed = list(
    label  = "All employed (OCUPADO == 1, Age 15-64)",
    filter = quote(OCUPADO == 1 &
                   age_band)
  ),
  
  # ── 5. Wage earners ─────────────────────────────────────────────────────────
  # Salaried workers (private + public employee) with positive real salary
  # income from primary job. Self-employed excluded because their income
  # concept is profit, not a wage, and the minimum wage does not formally
  # govern them. Public employees included here (distinct from the regression
  # sample which excludes them via sector).
  #
  # Use for: log-wage inequality, percentile ratios, Kaitz denominator
  # (formal-only subset), variance decomposition.
  wage_earners = list(
    label  = paste(
      "Salaried workers (private + public employee), Age 15-64,",
      "positive real salary income, primary job"
    ),
    filter = quote(
      OCUPADO == 1 &
        Employment_Type %in% c("private employee", "public employee") &
        age_band &
        !is.na(real_salary_income_primary) &
        real_salary_income_primary > 0
    )
  ),
  
  # ── 6. Income earners ───────────────────────────────────────────────────────
  # All employed with positive real TOTAL primary labor income
  # (salary + independent income). Includes informal self-employed and
  # own-account workers who have zero salary but positive independent income
  # (~60% of informal workers in this survey).
  #
  # Use for: income inequality analysis that must cover the full informal
  # sector (e.g. variance decomposition incl. self-employed, income
  # distribution comparisons, summary statistics for all earners).
  income_earners = list(
    label  = paste(
      "All employed with positive real total primary labor income",
      "(salary + independent). Includes self-employed."
    ),
    filter = quote(
      OCUPADO == 1 &
        age_band &
        !is.na(Employment_Status) &
        !is.na(real_total_income_primary) &
        real_total_income_primary > 0
    )
  ),
  
  # ── 7. Private-sector employees ─────────────────────────────────────────────
  # Employed workers whose Employment_Type is "private employee". Excludes
  # self-employed, business owners, non-remunerated relatives, and public
  # employees. No income filter — income conditioning is not appropriate for
  # computing employment shares, formality rates, or sector composition.
  #
  # Use for: private-sector employment shares, formality rates by sector/tier,
  # firm size distribution of employment.
  private_employees = list(
    label  = "Private-sector employees (no self-employed, no public sector). No income filter.",
    filter = quote(
      OCUPADO == 1 &
        age_band &
        Employment_Type == "private employee"
    )
  ),
  
  # ── 8. Private employees with positive salary income ────────────────────
  # Private employees who have positive real wage compliance income
  #
  # Hours > 0 required so that hourly measures are well-defined for all
  # workers in the sample.
  #
  # Use for: compliance analysis, wage distribution figures (A1, A2),
  # non-compliance share computation, any figure where income is the unit
  # of analysis for private workers.
  private_employees_inc = list(
    label  = paste(
      "Private-sector employees, positive real compliance income",
      "(salary + commissions), hours > 0. All three income concepts valid."
    ),
    filter = quote(
      OCUPADO == 1 &
        age_band &
        Employment_Type == "private employee" &
        !is.na(real_salary_primary) &
        real_salary_primary > 0 &
        !is.na(real_salary_primary_hourly) &
        real_wage_salary_hourly > 0 &
        !is.na(real_salary_primary_hourly_eff) &
        real_salary_primary_hourly_eff > 0 &
        !is.na(hours_worked_primary) &
        hours_worked_primary > 0
    )
  ),
  
  # ── 9. Regression / causal identification sample ────────────────────────────
  # This is the most restrictive sample. It is the one used for all event study
  # regressions and any figure that must be comparable with regression results.
  #
  # Conditions over and above private_employees_inc:
  #   (a) Wage_group known: firm size must be in {Micro, Small, Medium, Large}
  #       so the worker can be matched to their correct MW tier.
  #   (b) Government excluded: government workers face a distinct MW schedule.
  #   (c) Electricity & Water excluded: extreme cell sparsity in quarter × tier.
  #   (d) Employment_Status non-missing: needed to split formal/informal.
  #
  # Use for: Kaitz index computation, MW compliance regression, event study
  # estimation, any analysis where MW tier assignment is critical.
  regression_sample = list(
    label  = paste(
      "Private-sector employees, positive compliance income, known firm size",
      "(Wage_group in {Micro, Small, Medium, Large}),",
      "Government and Electricity & Water excluded, Employment_Status known."
    ),
    filter = quote(
      OCUPADO == 1 &
        age_band &
        Employment_Type == "private employee" &
        !is.na(real_wage_compliance_primary) &
        real_wage_compliance_primary > 0 &
        !is.na(real_wage_compliance_primary_hourly) &
        real_wage_compliance_primary_hourly > 0 &
        !is.na(real_wage_compliance_primary_hourly_eff) &
        real_wage_compliance_primary_hourly_eff > 0 &
        !is.na(hours_worked_primary) &
        hours_worked_primary > 0 &
        Wage_group %in% c("Micro", "Small", "Medium", "Large") &
        !Employment_Sector %in% c("Government", "Electricity and Water") &
        !is.na(Employment_Status)
    )
  )
)


#===============================================================================
# STEP 4.  Build samples list
#
#  For each spec: filter the data, build the svydesign, store both in a list.
#  A helper make_sample() does the work so the loop stays clean.
#===============================================================================

cat("[3] Building samples and survey designs...\n")

make_sample <- function(spec, data) {
  
  # Filter
  df <- dplyr::filter(data, !!spec$filter)
  
  # Build survey design
  design <- svydesign(
    id      = ~psu_unique,
    strata  = ~strata_unique,
    weights = ~FACTOR_EXPANSION,
    data    = df,
    nest    = TRUE
  )
  
  list(
    label  = spec$label,
    data   = df,
    design = design,
    n_rows = nrow(df)   # unweighted count, useful for quick checks
  )
}

samples <- lapply(SAMPLE_SPECS, make_sample, data = Full_ENCFT_clean)


#===============================================================================
# STEP 5.  Print sample size summary
#===============================================================================

cat("\n  Sample size summary (unweighted rows):\n")
cat("  ", strrep("-", 60), "\n", sep = "")
for (nm in names(samples)) {
  cat(sprintf("  %-26s  %8d rows  |  %12.0f (weighted) | %s\n",
              nm,
              samples[[nm]]$n_rows,
              sum(weights(samples[[nm]]$design, "sampling")),
              substr(samples[[nm]]$label, 1, 40)))
}
cat("  ", strrep("-", 60), "\n\n", sep = "")


#===============================================================================
# STEP 6.  Convenience accessors
#
#  Expose the nine data frames and designs directly in the environment so
#  downstream scripts can write either:
#    samples$employed$design          (explicit, recommended)
#    design_employed                  (shorthand, for quick interactive use)
#
#  The shorthand names follow the project's existing naming convention
#  (design_* prefix) for backwards compatibility with older scripts that
#  already reference e.g. design_wage_ineq.
#
#  BACKWARDS-COMPATIBILITY ALIASES:
#    design_wage_ineq  ->  samples$wage_earners$design
#    design_shares     ->  samples$employed$design
#
#  These aliases point to the SAME objects — no data is duplicated.
#===============================================================================

# Primary access pattern (recommended)
for (nm in names(samples)) {
  assign(paste0("df_",     nm), samples[[nm]]$data)
  assign(paste0("design_", nm), samples[[nm]]$design)
}

# Backwards-compatibility aliases
design_wage_ineq <- design_wage_earners   # used in 03_Compute Descriptive Stats
design_shares    <- design_employed       # used in 03_Compute Descriptive Stats

cat("[4] Environment objects created:\n")
cat("    samples$<name>$data    — filtered data frames\n")
cat("    samples$<name>$design  — svydesign objects\n")
cat("    df_<name>              — shorthand data frame access\n")
cat("    design_<name>          — shorthand design access\n")
cat("    Backwards-compatible aliases: design_wage_ineq, design_shares\n\n")


#===============================================================================
# STEP 7.  Export sample metadata as a tidy tibble
#
#  Useful for including in robustness appendices and for asserting in tests
#  that sample sizes haven't changed unexpectedly between runs.
#===============================================================================

sample_metadata <- tibble::tibble(
  sample_id    = names(samples),
  label        = purrr::map_chr(samples, "label"),
  n_unweighted = purrr::map_int(samples, "n_rows")
)

saveRDS(
  sample_metadata,
  file.path(config$paths$processed_data, "sample_metadata.rds")
)

cat("[5] Sample metadata saved to processed_data/sample_metadata.rds\n")
cat("\n=== 01_Sample_Definitions.R complete ===\n\n")
