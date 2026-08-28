
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


stopifnot(all(c("has_tier","firm_size_dk") %in% names(Full_ENCFT_clean)))

#===============================================================================
# STEP 2.  Pre-design variable prep and constants
#
#
#===============================================================================



#age filters
work_AGE_MIN <- config$age$working_age$min
work_AGE_MAX <- config$age$working_age$max
reg_AGE_MIN  <- config$age$regression$min
reg_AGE_MAX  <- config$age$regression$max

reg_age_label = sprintf("Population Age (age %d-%d)", reg_AGE_MIN, reg_AGE_MAX)
work_age_label = sprintf("Population Age (age %d-%d)", work_AGE_MIN, work_AGE_MAX)

Full_ENCFT_clean <- Full_ENCFT_clean %>%
  mutate(working_age_band = EDAD >= work_AGE_MIN & EDAD <=work_AGE_MAX,
        reg_age_band = EDAD >= reg_AGE_MIN & EDAD <= reg_AGE_MAX)


n_missing_age <- sum(is.na(Full_ENCFT_clean$working_age_band))


# Which age band the regression sample uses. Switch to "reg_age_band" when
# ready; 18-60 nests inside 15-64 so the sample hierarchy stays valid.
REG_AGE_BAND  <- if (config$age$active_band == "regression") "reg_age_band" else "working_age_band"
REG_AGE_LABEL <- if (config$age$active_band == "regression") reg_age_label else work_age_label

#===============================================================================
# STEP 3.  Build the ONE full-panel design
#===============================================================================

cat("[2] Building full-panel survey design...\n")

design_full <- svydesign(
  id      = ~psu_unique,
  strata  = ~strata_unique,
  weights = ~FACTOR_EXPANSION,
  data    = Full_ENCFT_clean,
  nest    = TRUE
)

# Restrict to analysis window — everything downstream inherits this
design_full <- design_full[
  design_full$variables[[TIME_VAR]] >= config$sample$start_qtr &
    design_full$variables[[TIME_VAR]] <= config$sample$end_qtr, ]


design_full <- update(design_full, one = 1)

#===============================================================================
# STEP 4.  Regression sample as an ORDERED sequence of conditions
#
#===============================================================================

REGRESSION_STEPS <- rlang::list2(
  "All person-quarter records"   = quote(TRUE),
  !!REG_AGE_LABEL               := rlang::expr(!!rlang::sym(REG_AGE_BAND)),
  "Economically active"          = quote(PEA == 1),
  "Employed"                     = quote(OCUPADO == 1),
  "Private-sector employee"      = quote(Employment_Type == "private employee"),
  "Excl. domestic workers"       = quote(!Principal_Category %in% "Domestic Worker"),
  "Excl. free trade zone"        = quote(!Principal_Category %in% "Free Trade Zone"),
  "Excl. electricity and water"  = quote(!Employment_Sector %in% "Electricity and Water"),
  "Positive salary"              = quote(!is.na(real_salary_income_wage_primary) &
                                           real_salary_income_wage_primary > 0),
  "Hours worked > 0"             = quote(!is.na(hours_worked_primary) &
                                           hours_worked_primary > 0),
  "Firm size tier known"         = quote(has_tier),
  "Formality status known"       = quote(!is.na(Employment_Status))
)

# Conjoin a list of quoted expressions into one quoted expression.
and_all <- function(exprs) {
  Reduce(function(a, b) rlang::expr(!!a & !!b), exprs)
}



STEP_TYPE <- c(
  "All person-quarter records"  = NA_character_,
  "<AGE>"                       = "Population definition",
  "Economically active"         = "Population definition",
  "Employed"                    = "Population definition",
  "Private-sector employee"     = "Population definition",
  "Excl. domestic workers"      = "Legal scope of the MW schedule",
  "Excl. free trade zone"       = "Legal scope of the MW schedule",
  "Excl. electricity and water" = "Legal scope of the MW schedule",
  "Positive salary"             = "Data availability",
  "Hours worked > 0"            = "Data availability",
  "Firm size tier known"        = "Data availability",
  "Formality status known"      = "Data availability"
)
names(STEP_TYPE)[names(STEP_TYPE) == "<AGE>"] <- REG_AGE_LABEL

stopifnot(setequal(names(STEP_TYPE), names(REGRESSION_STEPS)))
STEP_TYPE <- STEP_TYPE[names(REGRESSION_STEPS)]   # reorder by name, not position

TYPE_LEVELS <- c("Population definition",
                 "Legal scope of the MW schedule",
                 "Data availability")

# Announcement quarters. Phase-in completion quarters live in 01B; a selection
# break would align with announcement, which is why these are the ones plotted.
MW_EVENTS <- config$events$event_qtrs


#-------------------------------------------------------------------------------
# Where each estimation frame cuts the ladder.
#
#  The ladder still runs to the bottom for Tables 1-2. The tier row documents
#  what that restriction WOULD cost, which is the argument for not imposing it;
#  it is not applied to the frames used for the share or variance outcomes.
#-------------------------------------------------------------------------------

FRAME_CUTS <- c(
  "Employed"             = "reg_shares",     # log informal / self-employed share
  "Hours worked > 0"     = "reg_variance",   # variance-of-log-wage outcomes
  "Firm size tier known" = "reg_tier"        # compliance, bunching, exposure base
)
stopifnot(all(names(FRAME_CUTS) %in% names(REGRESSION_STEPS)))





#===============================================================================
# STEP 5.  Sample specs
#
#  $label   — human-readable description
#  $parent  — sample_id this sample nests inside (NA for the root)
#  $filter  — conditions ADDED beyond the parent (do not restate the parent)
#===============================================================================

SAMPLE_SPECS <- list(
  
  analysis_window = list(
    label  = sprintf("Analysis window %s to %s",
                     config$sample$start_qtr, config$sample$end_qtr),
    parent = NA_character_,
    filter = rlang::expr(!!rlang::sym(TIME_VAR) >= !!config$sample$start_qtr &
                           !!rlang::sym(TIME_VAR) <= !!config$sample$end_qtr)
  ),
  
  all_individuals = list(
    label  = "All survey respondents",
    parent = "analysis_window",
    filter = quote(TRUE)
  ),
  
  working_age = list(
    label  = paste("Working-age population,", REG_AGE_LABEL),
    parent = "all_individuals",
    filter = rlang::expr(!!rlang::sym(REG_AGE_BAND))
  ),
  
  # PEA == 1: employed or actively seeking work. Denominator for unemployment.
  active_pop = list(
    label  = paste("Economically active population (PEA == 1),", REG_AGE_LABEL),
    parent = "working_age",
    filter = quote(PEA == 1)
  ),
  
  employed = list(
    label  = paste("All employed (OCUPADO == 1),", REG_AGE_LABEL),
    parent = "active_pop",
    filter = quote(OCUPADO == 1)
  ),
  
  # Salaried workers, private and public, with positive real salary income.
  # Employment_Type stated explicitly: relying on positive salary income alone
  # to exclude the self-employed is an implicit assumption about how 02 codes
  # salary for own-account workers.
  # Use for: log-wage distributions, inequality, Kaitz denominator.
  wage_earners = list(
    label  = "Salaried workers (private + public), positive real salary income",
    parent = "employed",
    filter = quote(
      Employment_Type %in% c("private employee", "public employee") &
        !is.na(Employment_Status) &
        !is.na(real_salary_income_wage_primary) &
        real_salary_income_wage_primary > 0
    )
  ),
  
  # Positive real TOTAL primary labor income (salary + independent). Includes
  # informal self-employed with zero salary but positive independent income.
  # Use for: inequality covering the full informal sector.
  income_earners = list(
    label  = paste("All employed with positive real total primary labor income",
                   "(salary + independent); includes self-employed"),
    parent = "employed",
    filter = quote(
      !is.na(Employment_Status) &
        !is.na(real_total_income_all_primary) &
        real_total_income_all_primary > 0
    )
  ),
  
  # No income filter: income conditioning is not appropriate for employment
  # shares, formality rates, or sector composition.
  
  private_employees = list(
    label  = "Private-sector employees (no self-employed, no public sector); no income filter",
    parent = "employed",
    filter = quote(Employment_Type == "private employee")
  ),
  
  mw_covered = list(
    label  = "Private employees covered by the private MW schedule (excl. domestic, FTZ, utilities)",
    parent = "private_employees",
    filter = quote(
      !Principal_Category %in% "Domestic Worker" &
        !Principal_Category %in% "Free Trade Zone" &
        !Employment_Sector %in% "Electricity and Water"
    )
  ),
  

  # Hours > 0 so hourly measures are defined for every worker.
  # Use for: compliance analysis, wage distribution figures.
  private_employees_inc = list(
    label  = "Private-sector employees, positive real salary income, hours > 0",
    parent = "mw_covered",
    filter = quote(
      !is.na(real_salary_income_wage_primary) &
        real_salary_income_wage_primary > 0 &
        !is.na(hours_worked_primary) &
        hours_worked_primary > 0
    )
  ),
  
  # Frame 2. Share outcomes have headcount denominators, so no data-availability
  # filter is defensible: every person dropped changes the outcome mechanically.
  # No legal-scope exclusions either — the MW-informality channel is movement
  # across those boundaries.
  reg_shares = list(
    label  = "Employed, working age. Regression frame for share outcomes.",
    parent = "employed",
    filter = quote(TRUE)
  ),
  
  # Frame 3. `positive salary` is not a chosen filter: it is part of the
  # definition of "dispersion among those with observed positive earnings".
  # Firm-size DK RETAINED — tier is not needed, since exposure is a
  # province-level scalar and the worker's own tier enters nothing.
  reg_variance = list(
    label  = "MW-covered private employees, positive salary, hours > 0. Variance outcomes.",
    parent = "private_employees_inc",
    filter = quote(!is.na(Employment_Status))
  ),
  
  # Frame 3T. The only frame that drops DK.
  reg_tier = list(
    label  = "reg_variance with known firm-size tier. Compliance, bunching, exposure base.",
    parent = "reg_variance",
    filter = quote(has_tier)
  )
)




#===============================================================================
# STEP 6.  Compose each sample's full filter from its parent chain
#===============================================================================

bad_parents <- setdiff(
  na.omit(vapply(SAMPLE_SPECS, function(s) s$parent, character(1))),
  names(SAMPLE_SPECS)
)
if (length(bad_parents)) {
  stop("Unknown parent sample_id(s): ", paste(bad_parents, collapse = ", "))
}

full_filter <- function(id, specs) {
  chain <- character(0)
  cur   <- id
  while (!is.na(cur)) {
    if (cur %in% chain) stop("Cycle in parent chain at '", cur, "'.")
    chain <- c(cur, chain)
    cur   <- specs[[cur]]$parent
  }
  and_all(lapply(chain, function(nm) specs[[nm]]$filter))
}

SAMPLE_FILTERS <- lapply(names(SAMPLE_SPECS), full_filter, specs = SAMPLE_SPECS)
names(SAMPLE_FILTERS) <- names(SAMPLE_SPECS)




#===============================================================================
# STEP 7.  Build samples as DOMAINS of design_full
#
#  Reproduces subset.survey.design(): evaluate the condition in the design's
#  variables, coerce NA to FALSE, index the design. subset() cannot be used
#  directly because it substitutes its argument.
#
#  $data is assigned from $variables (copy-on-write), so there is no second
#  copy of the rows.
#===============================================================================

cat("[3] Building sample domains...\n")

make_sample <- function(id, base_design, env = parent.frame()) {
  
  spec <- SAMPLE_SPECS[[id]]
  idx  <- eval(SAMPLE_FILTERS[[id]], base_design$variables, env)
  
  # quote(TRUE) evaluates to length 1; expand explicitly.
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

samples <- lapply(names(SAMPLE_SPECS), make_sample,
                  base_design = design_full, env = environment())
names(samples) <- names(SAMPLE_SPECS)





#===============================================================================
# STEP 8.  Assert the nesting structure
#
#  Nesting now holds by construction, so this is a cheap guard against a
#  mistyped parent or a spec edited in a way that breaks the chain. Keep it:
#  it is the assertion that must halt the pipeline, not a diagnostic.
#===============================================================================

row_key <- function(s) {
  paste(s$data$psu_unique, s$data$ID_HOGAR, s$data$MIEMBRO,
        s$data[[TIME_VAR]], sep = "|")
}

sample_keys <- lapply(samples, row_key)

if (anyDuplicated(sample_keys$all_individuals)) {
  stop("Row key is not unique — the nesting assertion below would be invalid.")
}

for (nm in names(samples)) {
  p <- samples[[nm]]$parent
  if (is.na(p)) next
  if (!all(sample_keys[[nm]] %in% sample_keys[[p]])) {
    stop(sprintf("Sample '%s' is not nested inside its declared parent '%s'.", nm, p))
  }
}

cat("[4] Nesting assertions passed.\n")




#===============================================================================
# STEP 9.  Convenience accessors and save
#===============================================================================

for (nm in names(samples)) {
  assign(paste0("df_",     nm), samples[[nm]]$data)
  assign(paste0("design_", nm), samples[[nm]]$design)
}

# Backwards-compatibility aliases — same objects, no duplication.
design_wage_ineq <- design_wage_earners
design_shares    <- design_employed

saveRDS(
  list(
    samples          = samples,
    design_full      = design_full,
    specs            = SAMPLE_SPECS,
    filters          = SAMPLE_FILTERS,
    regression_steps = REGRESSION_STEPS,
    n_missing_age    = n_missing_age,
    built_at         = Sys.time()
  ),
  file.path(config$paths$processed_data, "samples.rds")
)
frame_ns <- vapply(FRAME_CUTS, function(id) samples[[id]]$n_rows, numeric(1))
cat("[5] Built ", length(samples), " samples. Estimation frames: ",
    paste(sprintf("%s = %s", FRAME_CUTS, format(frame_ns, big.mark = ",")),
          collapse = " | "), "\n", sep = "")

cat("=== 03A_sample definitions.R complete ===\n\n")



