
source(here::here("Code", "R", "clean scripts", "00_setup.R"))

cat("=== 03_sample definitions.R ===\n\n")

# Domains will empty out some strata down to a single PSU. Without this the
# variance estimator errors instead of returning a conservative answer.
options(survey.lonely.psu = "adjust")

# Name of the year-quarter variable. Adjust here if the project renames it.
TIME_VAR <- "year_quarter"


#===============================================================================
# STEP 1.  Load clean survey panel
#===============================================================================

cat("[1] Loading Full_ENCFT_clean...\n")

Full_ENCFT_clean <- readRDS(
  file.path(config$paths$processed_data, "Full_ENCFT_clean.rds")
)

stopifnot(TIME_VAR %in% names(Full_ENCFT_clean))


#===============================================================================
# STEP 2.  Pre-design variable prep and constants
#
#  NOTE: age_band is variable construction and belongs in 02, not here. It is
#  left in place for now so this refactor is a clean diff, but it must be
#  computed BEFORE the design is built (the design is now built on the full
#  panel, so any column used in a filter has to already exist).
#
#  EDAD missing => age_band NA => the record is dropped by every sample that
#  conditions on age. The count is reported in STEP 7.
#===============================================================================


TIER_LEVELS             <- c("Micro", "Small", "Medium", "Large")
EXCL_SECTORS_REGRESSION <- c("Government", "Electricity and Water")

EXCL_WORKERS <- c("Domestic Worker", "Free Trade Zone")
EXCL_SELF    <- c("Self Employed")

#age filter 
all_ENCFT_clean <- all_ENCFT_clean %>%
  mutate(age_band = EDAD >= 18 & EDAD <=60)


n_missing_age <- sum(is.na(Full_ENCFT_clean$age_band))


#===============================================================================
# STEP 3.  Build the ONE full-panel design
#
#  Everything downstream is a domain of this object. This is the single most
#  important line in the script.
#===============================================================================

cat("[2] Building full-panel survey design...\n")

design_full <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = Full_ENCFT_clean,
  nest    = TRUE
)


#===============================================================================
# STEP 4.  Define sample filters
#
#  Each spec is a named list with:
#    $label   — human-readable description
#    $parent  — sample_id this sample must nest inside (NA for the root).
#               Asserted in STEP 6.
#    $filter  — a quoted expression evaluated in the design's variables
#===============================================================================

# ── Regression sample, defined as an ORDERED sequence of conditions ───────────
#
#  Single-sourcing the definition this way means the sample and the attrition
#  waterfall table can never diverge: the table is built by applying these in
#  order, the sample is their conjunction. Do not restate the filter elsewhere.

REGRESSION_STEPS <- list(
  "All person-quarter records"   = quote(TRUE),
  "Age 18 - 60"                  = quote(age_band),
  "Employed"                     = quote(OCUPADO == 1),
  "Private-sector employee"      = quote(Employment_Type == "private employee"),
  "Positive compliance earnings" = quote(!is.na(real_salary_income_wage_primary) &
                                           real_salary_income_wage_primary > 0),
  "Hours worked > 0"             = quote(!is.na(hours_worked_primary) &
                                           hours_worked_primary > 0),
  "Firm size tier known"         = quote(Wage_group %in% TIER_LEVELS),
  "Excl. govt and utilities"     = quote(!Employment_Sector %in% EXCL_SECTORS_REGRESSION),
  "Excl. Domestic and Free Zone" = quote(!Principal_Category %in% EXCL_WORKERS),
  "Excl. Self Emp"               = quote(!Principal_Category %in% EXCL_SELF),
  "Formality status known"       = quote(!is.na(Employment_Status))
)

# Conjoin a list of quoted expressions into one quoted expression.
and_all <- function(exprs) {
  Reduce(function(a, b) rlang::expr(!!a & !!b), exprs)
}

REGRESSION_FILTER <- and_all(REGRESSION_STEPS)


SAMPLE_SPECS <- list(
  
  # ── 1. All survey respondents ───────────────────────────────────────────────
  all_individuals = list(
    label  = "All survey respondents",
    parent = NA_character_,
    filter = quote(TRUE)
  ),
  
  # ── 2. Working-age population (15-64) ───────────────────────────────────────
  working_age = list(
    label  = "Working-age population (age 15-64)",
    parent = "all_individuals",
    filter = quote(age_band)
  ),
  
  # ── 3. Economically active population ───────────────────────────────────────
  # PEA == 1: employed or actively seeking work (ILO definition as coded in the
  # ENCFT). Denominator for unemployment rates.
  active_pop = list(
    label  = "Economically active population (PEA == 1), age 15-64",
    parent = "working_age",
    filter = quote(age_band & PEA == 1)
  ),
  
  # ── 4. Employed ─────────────────────────────────────────────────────────────
  employed = list(
    label  = "All employed (OCUPADO == 1), age 15-64",
    parent = "active_pop",
    filter = quote(age_band & OCUPADO == 1)
  ),
  
  # ── 5. Wage earners ─────────────────────────────────────────────────────────
  # Salaried workers, public and private, with positive real salary income.
  # Use for: log-wage distributions, inequality, Kaitz denominator.
  wage_earners = list(
    label  = "Employed salaried workers (private + public), positive real salary income",
    parent = "employed",
    filter = quote(
      OCUPADO == 1 &
        age_band &
        !is.na(Employment_Status) &
        !is.na(real_salary_income_wage_primary) &
        real_salary_income_wage_primary > 0
    )
  ),
  
  # ── 6. Income earners ───────────────────────────────────────────────────────
  # Positive real TOTAL primary labor income (salary + independent). Includes
  # informal self-employed and own-account workers who have zero salary but
  # positive independent income.
  # Use for: income inequality covering the full informal sector.
  income_earners = list(
    label  = paste(
      "All employed with positive real total primary labor income",
      "(salary + independent). Includes self-employed."
    ),
    parent = "employed",
    filter = quote(
      OCUPADO == 1 &
        age_band &
        !is.na(Employment_Status) &
        !is.na(real_total_income_all_primary) &
        real_total_income_all_primary > 0
    )
  ),
  
  # ── 7. Private-sector employees ─────────────────────────────────────────────
  # No income filter: income conditioning is not appropriate for employment
  # shares, formality rates, or sector composition.
  private_employees = list(
    label  = "Private-sector employees (no self-employed, no public sector). No income filter.",
    parent = "employed",
    filter = quote(
      OCUPADO == 1 &
        age_band &
        Employment_Type == "private employee"
    )
  ),
  
  # ── 8. Private employees with positive compliance income ────────────────────
  # Hours > 0 required so hourly measures are well defined for every worker.
  # Use for: compliance analysis, wage distribution figures.
  private_employees_inc = list(
    label  = paste(
      "Private-sector employees, positive real compliance income",
      "(salary + commissions), hours > 0."
    ),
    parent = "private_employees",
    filter = quote(
      OCUPADO == 1 &
        age_band &
        Employment_Type == "private employee" &
        !is.na(real_salary_income_wage_primary) &
        real_salary_income_wage_primary > 0 &
        !is.na(hours_worked_primary) &
        hours_worked_primary > 0
    )
  ),
  
  # ── 9. Regression / causal identification sample ────────────────────────────
  # Conditions over and above private_employees_inc:
  #   (a) Wage_group known — worker can be matched to their MW tier
  #   (b) Government excluded — distinct MW schedule
  #   (c) Electricity & Water excluded — extreme cell sparsity in quarter x tier
  #   (d) Employment_Status non-missing — needed to split formal/informal
  # Filter is built from REGRESSION_STEPS above; do not edit it here.
  regression_sample = list(
    label  = paste(
      "Private-sector employees, positive compliance income, known firm size",
      "(Wage_group in {Micro, Small, Medium, Large}),",
      "Government and Electricity & Water excluded, Employment_Status known."
    ),
    parent = "private_employees_inc",
    filter = REGRESSION_FILTER
  )
)


#===============================================================================
# STEP 5.  Build samples as DOMAINS of design_full
#
#  make_sample() reproduces subset.survey.design() exactly — evaluate the
#  condition in the design's variables, coerce NA to FALSE, index the design —
#  but takes a pre-quoted expression instead of relying on substitute().
#  Using subset(design_full, spec$filter) directly would NOT work: subset()
#  substitutes its argument, so it would try to use the literal symbol
#  `spec$filter` as the condition.
#
#  $data is assigned from $variables rather than filtered separately, so there
#  is no second copy of the rows.
#===============================================================================

cat("[3] Building sample domains...\n")

make_sample <- function(spec, base_design) {
  
  idx <- eval(spec$filter, base_design$variables, parent.frame())
  
  # quote(TRUE) evaluates to a length-1 logical; expand it explicitly rather
  # than relying on recycling.
  if (length(idx) == 1L) idx <- rep(idx, nrow(base_design$variables))
  
  idx <- idx & !is.na(idx)
  
  d <- base_design[idx, ]
  
  list(
    label  = spec$label,
    parent = spec$parent,
    data   = d$variables,
    design = d,
    n_rows = nrow(d$variables)
  )
}

samples <- lapply(SAMPLE_SPECS, make_sample, base_design = design_full)
names(samples) <- names(SAMPLE_SPECS)


#===============================================================================
# STEP 6.  Assert the nesting structure
#
#  Each sample must be a strict subset of its declared parent. This catches the
#  failure mode where an upstream variable is renamed and, say,
#  regression_sample stops being contained in private_employees_inc — which
#  would silently invalidate every "we lose X% at this step" statement.
#
#  Comparison is on row position in Full_ENCFT_clean, so it does not depend on
#  ID_PERSONA being a stable panel key.
#===============================================================================

row_key <- function(s) {
  paste(s$data$psu_unique, s$data$ID_HOGAR, s$data$MIEMBRO,
        s$data[[TIME_VAR]], sep = "|")
}

sample_keys <- lapply(samples, row_key)

for (nm in names(samples)) {
  p <- samples[[nm]]$parent
  if (is.na(p)) next
  if (!all(sample_keys[[nm]] %in% sample_keys[[p]])) {
    stop(sprintf("Sample '%s' is not nested inside its declared parent '%s'.", nm, p))
  }
}

cat("[4] Nesting assertions passed.\n")


#===============================================================================
# STEP 7.  Sample metadata and summary
#
#  pop_mean_qtr is the number to quote as a population figure. sum(weights) is
#  n_quarters x population and should never appear in a table.
#===============================================================================

sample_metadata <- purrr::imap_dfr(samples, function(s, id) {
  
  w  <- weights(s$design, "sampling")
  v  <- s$data
  nq <- dplyr::n_distinct(v[[TIME_VAR]])
  
  tibble::tibble(
    sample_id     = id,
    label         = s$label,
    parent        = s$parent,
    n_rows        = s$n_rows,
    n_psu         = dplyr::n_distinct(v$psu_unique),
    n_strata      = dplyr::n_distinct(v$strata_unique),
    degf          = survey::degf(s$design),
    n_quarters    = nq,
    first_quarter = min(v[[TIME_VAR]]),
    last_quarter  = max(v[[TIME_VAR]]),
    pop_mean_qtr  = sum(w) / nq,
    min_n_qtr     = min(table(v[[TIME_VAR]])),
    filter_text   = paste(deparse(SAMPLE_SPECS[[id]]$filter), collapse = " "),
    filter_hash   = rlang::hash(SAMPLE_SPECS[[id]]$filter),
    built_at      = Sys.time()
  )
})

# Share of the parent sample's population retained.
sample_metadata <- sample_metadata %>%
  left_join(
    sample_metadata %>% select(parent = sample_id, parent_pop = pop_mean_qtr),
    by = "parent"
  ) %>%
  mutate(share_of_parent = pop_mean_qtr / parent_pop) %>%
  select(-parent_pop)

saveRDS(
  sample_metadata,
  file.path(config$paths$processed_data, "sample_metadata.rds")
)

cat("\n  Records with missing EDAD (dropped by all age-conditioned samples): ",
    n_missing_age, "\n", sep = "")

cat("\n  Sample summary:\n")
cat("  ", strrep("-", 78), "\n", sep = "")
cat(sprintf("  %-24s %9s %14s %8s %8s\n",
            "sample_id", "rows", "pop/quarter", "degf", "min n/q"))
for (i in seq_len(nrow(sample_metadata))) {
  r <- sample_metadata[i, ]
  cat(sprintf("  %-24s %9d %14.0f %8d %8d\n",
              r$sample_id, r$n_rows, r$pop_mean_qtr, r$degf, r$min_n_qtr))
}
cat("  ", strrep("-", 78), "\n\n", sep = "")


#===============================================================================
# STEP 8.  Convenience accessors (unchanged interface)
#===============================================================================

for (nm in names(samples)) {
  assign(paste0("df_",     nm), samples[[nm]]$data)
  assign(paste0("design_", nm), samples[[nm]]$design)
}

# Backwards-compatibility aliases — same objects, no duplication.
design_wage_ineq <- design_wage_earners
design_shares    <- design_employed

cat("[5] Environment objects created:\n")
cat("    design_full         — full-panel design (parent of every domain)\n")
cat("    samples$<id>$data   — data frames\n")
cat("    samples$<id>$design — domain designs\n")
cat("    df_<id> / design_<id>, plus design_wage_ineq, design_shares\n\n")


#===============================================================================
# STEP 9.  Optional: quantify the impact of the refactor
#
#  Run this interactively before updating any published estimate. It computes
#  the same statistic two ways — as a domain of design_full (new, correct) and
#  from a design rebuilt on the filtered rows (old) — and reports the ratio of
#  standard errors.
#
#  Expectation: se_prefilter / se_subset >= 1, i.e. the old approach was
#  conservative, with the gap widening for the smaller samples. Verify rather
#  than assume — the direction depends on how the retained fpc$sampsize
#  interacts with each stratum's realised PSU count.
#
#  Example:
#    compare_se_methods("regression_sample", ~real_salary_income_wage_primary)
#    compare_se_methods("employed",          ~I(Employment_Status == "Formal"))
#===============================================================================

compare_se_methods <- function(sample_id, formula) {
  
  d_new <- samples[[sample_id]]$design
  
  d_old <- svydesign(
    id      = ~psu_unique,
    strata  = ~strata_unique,
    weights = ~FACTOR_EXPANSION,
    data    = samples[[sample_id]]$data,
    nest    = TRUE
  )
  
  a <- svymean(formula, d_new, na.rm = TRUE)
  b <- svymean(formula, d_old, na.rm = TRUE)
  
  tibble::tibble(
    sample_id      = sample_id,
    term           = names(coef(a)),
    estimate       = as.numeric(coef(a)),
    est_matches    = isTRUE(all.equal(as.numeric(coef(a)), as.numeric(coef(b)))),
    se_subset      = as.numeric(SE(a)),
    se_prefilter   = as.numeric(SE(b)),
    se_ratio       = as.numeric(SE(b)) / as.numeric(SE(a)),
    degf_subset    = survey::degf(d_new),
    degf_prefilter = survey::degf(d_old)
  )
}

cat("=== 03_sample definitions.R complete ===\n\n")
