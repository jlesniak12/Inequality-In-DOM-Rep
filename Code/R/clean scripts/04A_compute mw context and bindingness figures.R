#===============================================================================
#
# Script: 04A_Compute_MW_Context_and_Bindingness.R
#
# Purpose: Compute all statistics needed for the minimum wage context and
#          bindingness descriptive figures. Saves results as RDS objects for
#          the companion plot script (04B_Plot_MW_Context_and_Bindingness.R).
#          No ggplot or figure code lives here.
#
# SURVEY CONVENTION:
#   This script uses the `survey` package directly (svydesign objects come
#   from 03_Sample_Definitions.R; estimation via svyby / svymean / svyquantile).
#   All survey calls are funneled through the small set of tidy helpers defined
#   in the HELPERS section below so that every output object shares the same
#   schema: a tibble with year_quarter, (wage_group), estimate, se, n_obs.
#   Do not call svyby/svymean/svyquantile outside those helpers without good
#   reason — keeping them centralised is what makes the output consistent.
#
# FIGURES THIS SCRIPT FEEDS:
#
#   MW CONTEXT
#     Fig MW-1  Real minimum wage levels by tier over time
#               -> mw_context_levels.rds
#     Fig MW-2  Nominal MW growth decomposed into real gain vs. inflation
#               at each MW event, by tier
#               -> mw_context_growth_decomp.rds
#     Fig MW-3  Employment share by firm size tier over time
#               (among workers with known firm size)
#               -> mw_context_firmsize_shares.rds
#
#   MW BINDINGNESS
#     Fig MW-4  Log Kaitz index by tier over time
#               log(real tier MW / median real salary within tier, formal)
#               -> mw_bind_kaitz.rds
#     Fig MW-5  Non-compliance rate over time (econ-wide + by tier x scope)
#               -> mw_bind_noncompliance_econ.rds
#               -> mw_bind_noncompliance_tier.rds
#     Fig MW-6  Wage distribution relative to MW — microdata extract for
#               kernel density / bunching histogram (formal workers)
#               -> mw_bind_dist_formal.rds
#
# DESIGNS USED (all from 03_Sample_Definitions.R — no ad-hoc svydesign here):
#   samples$employed$design                all employed (firm-size shares)
#   samples$private_employees_inc$design   private employees, +income, hrs>0
#                                          (compliance, Kaitz, distribution)
#   samples$regression_sample$data         microdata extract for distribution
#
# INCOME / COMPLIANCE CONCEPTS:
#   monthly  real_salary_income_wage_primary  vs real_minwage_harmonized
#            (Measure 1 — monthly, no hours adjustment)
#   hourly   real_salary_primary_hourly_base  vs real_minwage_hourly
#            (Measure 2 — hourly rate capped at 44hrs, PRIMARY)
#   The overtime-adjusted measure (Measure 3) is left to a robustness script.
#
# OUTPUTS -> config$paths$processed_data / "MW Context and Bindingness":
#   mw_context_levels.rds
#   mw_context_growth_decomp.rds
#   mw_context_firmsize_shares.rds
#   mw_bind_kaitz.rds
#   mw_bind_noncompliance_econ.rds
#   mw_bind_noncompliance_tier.rds
#   mw_bind_dist_formal.rds, mw_bind_dist_mw_ref.rds, mw_bind_mw_annual_avg.rds
#
# READS:
#   Min_Wage.rds, CPI.rds   (from 01B)
#   samples object          (from 03_Sample_Definitions.R)
#
#===============================================================================

source(here::here("Code","R","clean scripts","00_setup.R"))

source(here::here("Code", "R", "clean scripts", "03_sample definitions.R"))



cat("=== 04A_Compute_MW_Context_and_Bindingness.R ===\n\n")



#===============================================================================
# CONSTANTS — all resolved from config
#===============================================================================

TIER_LEVELS     <- config$TIER_LEVELS
MW_EVENT_QTR    <- config$events$event_qtrs
STANDARD_WEEK   <- config$hours$standard_week
WEEKS_PER_MONTH <- config$hours$weeks_per_month
MIN_CELL_N      <- config$figures$min_cell_n
MICRO_START     <- config$figures$micro_tier_start_qtr


# Output subfolder
out_dir <- config$data_dirs$minwage
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

save_rds <- function(obj, name) {
  path <- file.path(out_dir, paste0(name, ".rds"))
  saveRDS(obj, path)
  cat("  Saved:", path, "\n")
}


#===============================================================================
  # SAMPLE ACCESSORS
  #
  # `samples` element structure has changed; resolve the microdata frame through
  # one accessor so a future change is a one-function edit rather than a
  # find-and-replace. Fails loudly with the actual available names instead of
  # handing NULL to dplyr (which throws an opaque "no applicable method for
  # 'filter' applied to an object of class NULL" several lines later).
  #===============================================================================

samp_df <- function(id) {
  s <- samples[[id]]
  if (is.null(s)) {
    stop("Sample '", id, "' not found. Available: ",
         paste(names(samples), collapse = ", "), call. = FALSE)
  }
  if (!is.null(s$data))               return(s$data)
  if (!is.null(s$design$variables))   return(s$design$variables)
  stop("Sample '", id, "' has neither $data nor $design$variables. Slots: ",
       paste(names(s), collapse = ", "), call. = FALSE)
}

samp_design <- function(id) {
  s <- samples[[id]]
  if (is.null(s) || is.null(s$design)) {
    stop("Design for sample '", id, "' not found. Available: ",
         paste(names(samples), collapse = ", "), call. = FALSE)
  }
  s$design
}

invisible(lapply(c("employed", "private_employees_inc", "reg_tier"),
                 function(id) samp_design(id)))

#===============================================================================
# HELPERS — tidy wrappers around the survey package
#
# Every helper returns the same schema so 04B can treat all objects uniformly:
#   year_quarter chr | <group_var> chr | estimate dbl | se dbl | n_obs int |
#   sparse lgl
#===============================================================================

.n_by <- function(design, time_var, group_var = NULL) {
  grp <- c(time_var, group_var)
  design$variables %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(n_obs = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
}

.standardise_se <- function(df, keys) {
  leftover <- setdiff(names(df), c(keys, "estimate"))
  se_col <- NULL
  if ("se" %in% leftover) {
    se_col <- "se"
  } else {
    cand <- leftover[grepl("(^|\\.)se(\\.|$)|se$", leftover, ignore.case = TRUE)]
    cand <- setdiff(cand, c("ci_l", "ci_u", "ci.l", "ci.u"))
    if (length(cand)) se_col <- cand[[1]]
  }
  if (is.null(se_col)) df$se <- NA_real_ else names(df)[names(df) == se_col] <- "se"
  df[, c(keys, "estimate", "se"), drop = FALSE]
}

svy_mean_by <- function(design, var, time_var, group_var = NULL, na_rm = TRUE) {
  if (isTRUE(na_rm)) design <- design[!is.na(design$variables[[var]]), ]
  
  grp <- c(time_var, group_var)
  est <- svyby(stats::as.formula(paste0("~", var)),
               stats::as.formula(paste0("~", paste(grp, collapse = "+"))),
               design, svymean, na.rm = na_rm, vartype = "se",
               keep.names = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(estimate = !!var) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
  est <- .standardise_se(est, keys = grp)
  
  est %>%
    dplyr::left_join(.n_by(design, time_var, group_var), by = grp) %>%
    dplyr::rename(year_quarter = !!time_var) %>%
    dplyr::mutate(sparse = n_obs < MIN_CELL_N)
}

svy_quantile_by <- function(design, var, time_var, group_var = NULL,
                            prob = 0.5, na_rm = TRUE) {
  if (isTRUE(na_rm)) design <- design[!is.na(design$variables[[var]]), ]
  
  grp <- c(time_var, group_var)
  est <- svyby(stats::as.formula(paste0("~", var)),
               stats::as.formula(paste0("~", paste(grp, collapse = "+"))),
               design, FUN = svyquantile, quantiles = prob, ci = TRUE,
               vartype = "se", keep.names = FALSE, na.rm = na_rm) %>%
    tibble::as_tibble()
  
  # svyby(svyquantile) names the estimate column `var` or `var.0.5` depending
  # on the survey version. Find it as the non-key, non-SE/CI column.
  non_key <- setdiff(names(est), grp)
  se_ci   <- non_key[grepl("se|ci", non_key, ignore.case = TRUE)]
  est_col <- setdiff(non_key, se_ci)
  if (length(est_col) != 1) est_col <- if (var %in% non_key) var else est_col[[1]]
  names(est)[names(est) == est_col] <- "estimate"
  
  est <- est %>% dplyr::mutate(dplyr::across(dplyr::all_of(grp), as.character))
  est <- .standardise_se(est, keys = grp)
  
  est %>%
    dplyr::left_join(.n_by(design, time_var, group_var), by = grp) %>%
    dplyr::rename(year_quarter = !!time_var) %>%
    dplyr::mutate(sparse = n_obs < MIN_CELL_N)
}

# Survey-weighted share of x below a threshold, without needing a pre-built
# indicator column. Used for the incoming-micro-floor bite (Step 6b), where the
# threshold is counterfactual and so cannot exist in 02.
wtd_share_below <- function(x, thresh, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0 & is.finite(thresh)
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * (x[ok] < thresh[ok])) / sum(w[ok])
}


#===============================================================================
# STEP 0. Minimum wage schedule — sourced from 02's output
#
# Floors come from Full_ENCFT_clean.rds, NOT Min_Wage.rds. 02 is the single
# deflation point in the project: it rebases both worker incomes and MW floors
# onto config$CPI_base. The Min_Wage.rds "real" columns are still on the
# workbook's own index base (Oct 2019-Sep 2020 = 100), so reading floors from
# there puts wages and floors in different price units and every wage/floor
# ratio is off by CPI_base/100.
#===============================================================================

cat("[0] Loading MW schedule from 02 output...\n")

encft <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))

stopifnot(all(c("wage_group", "wage_group_legal", "year_quarter",
                "nom_minwage_harmonized", "real_minwage_harmonized")
              %in% names(encft)))

# Worker-facing floor lookup, keyed on OBSERVED tier.
mw_by_observed <- encft %>%
  dplyr::filter(wage_group %in% TIER_LEVELS) %>%
  dplyr::distinct(year, quarter, year_quarter, wage_group, wage_group_legal,
                  nom_minwage_harmonized, real_minwage_harmonized) %>%
  dplyr::mutate(mw_floor_imputed = wage_group == "Micro" &
                  wage_group_legal == "Small")

# distinct() on the worker file only yields a tier x quarter cell if some
# sampled worker sits in it. Assert the grid is complete rather than letting a
# thin Medium quarter vanish from the figure.
grid_n <- dplyr::n_distinct(mw_by_observed$year_quarter) * length(TIER_LEVELS)
if (nrow(mw_by_observed) != grid_n) {
  miss <- tidyr::expand_grid(
    year_quarter = unique(mw_by_observed$year_quarter),
    wage_group   = TIER_LEVELS
  ) %>% dplyr::anti_join(mw_by_observed, by = c("year_quarter", "wage_group"))
  stop("Empty tier x quarter cells in the MW lookup: ",
       paste(sprintf("%s/%s", miss$year_quarter, miss$wage_group),
             collapse = ", "))
}

# One floor value per tier x quarter, or the distinct() has split on a numeric
# discrepancy somewhere upstream.
stopifnot(!any(duplicated(mw_by_observed[c("year_quarter", "wage_group")])))

save_rds(mw_by_observed, "mw_by_observed_tier")

#===============================================================================
# STEP 1. MW-1: Real MW levels — keyed on the LEGAL tier
#
# Grouping on wage_group_legal collapses the pre-2021Q3 Micro row into Small,
# so the figure draws one line per distinct floor rather than two identical
# lines. mw_floor_imputed flags rows where an observed-Micro firm was on the
# Small floor, for captions and for filtering elsewhere.
#===============================================================================

cat("[1] MW levels by legal tier...\n")

mw_context_levels <- mw_by_observed %>%
  dplyr::distinct(year, quarter, year_quarter, wage_group_legal,
                  nom_minwage_harmonized, real_minwage_harmonized) %>%
  dplyr::mutate(wage_group_legal = factor(wage_group_legal, levels = TIER_LEVELS))

# One row per legal tier per quarter — if not, the collapse above failed.
dup <- mw_context_levels %>%
  dplyr::count(year_quarter, wage_group_legal) %>%
  dplyr::filter(n > 1)
if (nrow(dup)) {
  stop("mw_context_levels has duplicate legal-tier x quarter rows. ",
       "Micro and Small floors disagree in the pre-micro period: ",
       paste(utils::head(dup$year_quarter), collapse = ", "))
}

save_rds(mw_context_levels, "mw_context_levels")
cat("  Quarters:", dplyr::n_distinct(mw_context_levels$year_quarter),
    "| Rows:", nrow(mw_context_levels), "\n\n")

# Worker-facing floor lookup, keyed on OBSERVED tier. Everything from Step 4
# onward joins on this, so the value is the floor that legally applied while
# the grouping stays on observed firm size.
mw_by_observed <- min_wage %>%
  dplyr::filter(wage_group %in% TIER_LEVELS) %>%
  dplyr::transmute(year, quarter, year_quarter,
                   wage_group, wage_group_legal,
                   nom_minwage_harmonized, real_minwage_harmonized,
                   mw_floor_imputed = wage_group == "Micro" &
                     wage_group_legal == "Small")

save_rds(mw_by_observed, "mw_by_observed_tier")

#===============================================================================
# STEP 2. MW-2: MW growth decomposition at each event
#
# Base quarter is DERIVED from the event list by lag(), not maintained as a
# parallel vector — the previous hand-kept MW_EVENT_PREV desynced from
# MW_EVENT_QTR and map2() failed on a length mismatch.
#
# MW_CHANGE_QTR includes 2015Q2. It is a genuine MW change but not a treatment
# event (it is folded into the 2016 exposure baseline), so it appears only as
# the base for the first panel.
#
# Each bar is the CUMULATIVE change since the previous announcement, so it
# spans any phase-in tranche in between (2017Q4, 2022Q1, 2024Q1) and will not
# match the headline resolucion percentage for a single event. That is the
# intended "policy cycle" reading; 04B labels it as such.
#===============================================================================

cat("[2] MW growth decomposition at events...\n")

MW_CHANGE_QTR <- unique(c("2015Q2", MW_EVENT_QTR))

events_tbl <- tibble::tibble(event = MW_CHANGE_QTR,
                             base  = dplyr::lag(MW_CHANGE_QTR)) %>%
  dplyr::filter(!is.na(base))

missing_q <- setdiff(c(events_tbl$event, events_tbl$base),
                     unique(as.character(mw_context_levels$year_quarter)))
if (length(missing_q)) {
  stop("Event/base quarters absent from the MW schedule: ",
       paste(missing_q, collapse = ", "),
       ". Either the workbook is short or config$events$event_qtrs is wrong.")
}

event_levels <- purrr::pmap_dfr(events_tbl, function(event, base) {
  cur  <- mw_context_levels %>% dplyr::filter(year_quarter == event)
  prev <- mw_context_levels %>%
    dplyr::filter(year_quarter == base) %>%
    dplyr::select(wage_group_legal,
                  nom_prev  = nom_minwage_harmonized,
                  real_prev = real_minwage_harmonized)
  cur %>%
    dplyr::left_join(prev, by = "wage_group_legal") %>%
    dplyr::mutate(
      base_quarter        = base,
      nom_pct_chg         = (nom_minwage_harmonized  / nom_prev  - 1) * 100,
      real_pct_chg        = (real_minwage_harmonized / real_prev - 1) * 100,
      inflation_component = nom_pct_chg - real_pct_chg
    )
})

mw_context_growth_decomp <- event_levels %>%
  dplyr::select(year_quarter, base_quarter, wage_group_legal,
                nom_pct_chg, real_pct_chg, inflation_component,
                nom_minwage_harmonized, real_minwage_harmonized) %>%
  dplyr::mutate(
    wage_group_legal = factor(wage_group_legal, levels = TIER_LEVELS),
    year_quarter     = factor(year_quarter, levels = events_tbl$event)
  )

save_rds(mw_context_growth_decomp, "mw_context_growth_decomp")
cat(sprintf("  Rows: %d (%d events x up to %d legal tiers)\n\n",
            nrow(mw_context_growth_decomp), nrow(events_tbl),
            length(TIER_LEVELS)))

#===============================================================================
# STEP 3. MW-3: Employment share by OBSERVED firm size tier
#
# Observed tier, necessarily: on the legal key, Micro would appear from zero at
# 2021Q3 and Small would drop by the same amount — a definitional shock landing
# exactly on an event quarter, in the one figure whose job is to document
# genuine composition drift as a confounder.
#===============================================================================

cat("[3] Employment share by observed firm size tier...\n")

design_employed <- update(
  samp_design("employed"),
  has_known_size = as.integer(wage_group %in% TIER_LEVELS)
)

known_size_share <- svy_mean_by(design_employed, "has_known_size",
                                "year_quarter") %>%
  dplyr::select(year_quarter, known_size_share = estimate)

cat(sprintf("  Known firm size share - min %.1f%%, max %.1f%%\n",
            min(known_size_share$known_size_share) * 100,
            max(known_size_share$known_size_share) * 100))

design_known_fs <- subset(design_employed, has_known_size == 1)

tier_shares <- purrr::map_dfr(TIER_LEVELS, function(tier) {
  d <- update(design_known_fs, .tier_ind = as.integer(wage_group == tier))
  svy_mean_by(d, ".tier_ind", "year_quarter") %>%
    dplyr::transmute(year_quarter, wage_group = tier,
                     share_of_known = estimate, se)
})

tier_n <- design_known_fs$variables %>%
  dplyr::count(year_quarter, wage_group, name = "n_obs") %>%
  dplyr::mutate(dplyr::across(c(year_quarter, wage_group), as.character))

mw_context_firmsize_shares <- tier_shares %>%
  dplyr::left_join(tier_n, by = c("year_quarter", "wage_group")) %>%
  dplyr::left_join(known_size_share, by = "year_quarter") %>%
  dplyr::mutate(wage_group = factor(wage_group, levels = TIER_LEVELS),
                sparse     = dplyr::coalesce(n_obs, 0L) < MIN_CELL_N)

share_sums <- mw_context_firmsize_shares %>%
  dplyr::group_by(year_quarter) %>%
  dplyr::summarise(total = sum(share_of_known), .groups = "drop")
cat(sprintf("  Conditional share sum - min %.4f, max %.4f (should be ~1)\n",
            min(share_sums$total), max(share_sums$total)))

save_rds(mw_context_firmsize_shares, "mw_context_firmsize_shares")
cat("  Rows:", nrow(mw_context_firmsize_shares), "\n\n")


#===============================================================================
# STEP 4. MW-4: Log Kaitz by OBSERVED tier
#
#   Kaitz_{t,g} = log(real floor applying to tier g at t)
#                 - log(p50 real monthly salary among FORMAL workers in tier g)
#
# Numerator: real_minwage_harmonized joined on OBSERVED tier, so pre-2021Q3
# Micro correctly carries the Small floor. Denominator: median among
# micro-firm workers specifically. The two together make the pre-2021Q3 Micro
# series a real, non-duplicate object, and its break at 2021Q3 (floor falls
# 12,400 -> 11,500 for those firms) is the Method 2 first stage in picture form.
#===============================================================================

cat("[4] Kaitz index by observed tier...\n")

# reg_tier already imposes has_tier, hours > 0, positive salary, formality
# known, and the legal-scope exclusions. Only the Formal restriction is left
# to add here — Kaitz measures how binding the floor is for workers it legally
# binds.
design_formal_tier <- subset(samp_design("reg_tier"),
                             Employment_Status == "Formal")

formal_median_tier <- svy_quantile_by(
  design    = design_formal_tier,
  var       = "real_salary_income_wage_primary",
  time_var  = "year_quarter",
  group_var = "wage_group",
  prob      = 0.50
) %>%
  dplyr::select(year_quarter, wage_group,
                p50_formal = estimate, p50_se = se, n_obs, sparse)

mw_bind_kaitz <- mw_by_observed %>%
  dplyr::inner_join(formal_median_tier, by = c("year_quarter", "wage_group")) %>%
  dplyr::mutate(
    log_kaitz  = log(real_minwage_harmonized) - log(p50_formal),
    wage_group = factor(wage_group, levels = TIER_LEVELS)
  ) %>%
  dplyr::select(year, quarter, year_quarter, wage_group, wage_group_legal,
                mw_floor_imputed, real_minwage_harmonized,
                p50_formal, p50_se, log_kaitz, n_obs, sparse)

cat(sprintf("  Cells: %d | sparse: %d | log Kaitz range [%.3f, %.3f]\n",
            nrow(mw_bind_kaitz), sum(mw_bind_kaitz$sparse, na.rm = TRUE),
            min(mw_bind_kaitz$log_kaitz, na.rm = TRUE),
            max(mw_bind_kaitz$log_kaitz, na.rm = TRUE)))

save_rds(mw_bind_kaitz, "mw_bind_kaitz")
cat("\n")


#===============================================================================
# STEP 5. MW-5: Non-compliance rates
#
# Both concepts are computed and saved; config$figures$headline_concept decides
# which 04B plots as headline and which becomes the robustness panel.
#===============================================================================

cat("[5] Non-compliance rates...\n")

COMPLIANCE_VARS <- tibble::tribble(
  ~concept,  ~col,                           ~label,
  "monthly", "below_min_monthly_salary",     "Monthly (Measure 1)",
  "hourly",  "below_min_hourly_base_salary", "Hourly (Measure 2, 44h cap)"
)

base_inc <- samp_design("private_employees_inc")

cat("  Economy-wide...\n")
mw_bind_noncompliance_econ <- purrr::map_dfr(
  c("formal", "formal_ex_large", "all_private"),
  function(scope) {
    d <- switch(
      scope,
      formal          = subset(base_inc, Employment_Status == "Formal"),
      formal_ex_large = subset(base_inc, Employment_Status == "Formal" &
                                 wage_group != "Large"),
      all_private     = base_inc
    )
    purrr::pmap_dfr(COMPLIANCE_VARS, function(concept, col, label) {
      svy_mean_by(d, var = col, time_var = "year_quarter") %>%
        dplyr::rename(nc_rate = estimate) %>%
        dplyr::mutate(concept = concept, concept_label = label, scope = scope)
    })
  }
)
save_rds(mw_bind_noncompliance_econ, "mw_bind_noncompliance_econ")

cat("  By observed tier...\n")
mw_bind_noncompliance_tier <- purrr::map_dfr(
  c("formal", "all_private"),
  function(scope) {
    d <- if (scope == "formal") {
      subset(samp_design("reg_tier"), Employment_Status == "Formal")
    } else {
      samp_design("reg_tier")
    }
    purrr::pmap_dfr(COMPLIANCE_VARS, function(concept, col, label) {
      svy_mean_by(d, var = col, time_var = "year_quarter",
                  group_var = "wage_group") %>%
        dplyr::rename(nc_rate = estimate) %>%
        dplyr::mutate(concept = concept, concept_label = label, scope = scope)
    })
  }
) %>%
  dplyr::mutate(wage_group = factor(wage_group, levels = TIER_LEVELS)) %>%
  # Carry the imputation flag so 04B can annotate pre-2021Q3 Micro rather than
  # silently presenting it as compliance against a Micro floor.
  dplyr::left_join(
    dplyr::distinct(mw_by_observed, year_quarter, wage_group, mw_floor_imputed),
    by = c("year_quarter", "wage_group")
  )

save_rds(mw_bind_noncompliance_tier, "mw_bind_noncompliance_tier")
cat("  Rows:", nrow(mw_bind_noncompliance_tier), "\n\n")


#===============================================================================
# STEP 6. MW-6: Bunching microdata at FOCAL QUARTERS
#
# Changed from focal YEARS with an annual-average floor. Two reasons:
#
#  (1) A year containing a MW change has no single floor. The 2025 annual mean
#      blends a pre- and post-2025Q2 regime, so normalising by it splits one
#      bunching spike into two smeared humps either side of zero and reads as
#      "bunching weakened" when nothing of the sort happened.
#
#  (2) The ratio is computed against each row's OWN-quarter floor, so there is
#      no averaging step at all. Note this makes the ratio invariant to the
#      deflator: real wage / real floor == nominal wage / nominal floor.
#
# Pooling: +/- dist_pool_halfwidth quarters, but ONLY quarters sharing the focal
# quarter's NOMINAL floor. A regime change therefore cannot leak into a window.
#
# Tiers collapsed to Micro / Small / Rest.
#===============================================================================

cat("[6] Bunching microdata at focal quarters...\n")

FOCAL_QTRS <- config$figures$dist_focal_qtrs
POOL_K     <- config$figures$dist_pool_halfwidth

# Nominal floor signature per quarter: pooling requires an exact match.
floor_sig <- mw_by_observed %>%
  dplyr::group_by(year_quarter) %>%
  dplyr::summarise(sig = paste(nom_minwage_harmonized[order(wage_group)],
                               collapse = "|"), .groups = "drop")

pool_map <- purrr::map_dfr(FOCAL_QTRS, function(fq) {
  cand    <- qshift(fq, -POOL_K:POOL_K)
  sig_fq  <- floor_sig$sig[floor_sig$year_quarter == fq]
  keep    <- floor_sig %>%
    dplyr::filter(year_quarter %in% cand, sig == sig_fq) %>%
    dplyr::pull(year_quarter)
  tibble::tibble(focal_qtr = fq, year_quarter = keep)
})

cat("  Pooling windows (same-nominal-floor quarters only):\n")
pool_map %>%
  dplyr::group_by(focal_qtr) %>%
  dplyr::summarise(qtrs = paste(sort(year_quarter), collapse = ", "),
                   .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)

dist_raw <- samp_df("reg_tier") %>%
  dplyr::filter(year_quarter %in% pool_map$year_quarter,
                wage_group %in% TIER_LEVELS,
                Employment_Status == "Formal") %>%
  dplyr::select(year, quarter, year_quarter, wage_group, Employment_Status,
                real_salary_income_wage_primary,
                real_salary_primary_hourly_base,
                hours_worked_primary, FACTOR_EXPANSION,
                psu_unique, strata_unique) %>%
  dplyr::inner_join(pool_map, by = "year_quarter") %>%
  dplyr::left_join(
    dplyr::select(mw_by_observed, year_quarter, wage_group,
                  real_minwage_harmonized, mw_floor_imputed),
    by = c("year_quarter", "wage_group")
  )

mw_bind_dist_formal <- dist_raw %>%
  dplyr::mutate(
    bunch_group = dplyr::case_when(
      wage_group == "Micro" ~ "Micro",
      wage_group == "Small" ~ "Small",
      TRUE                  ~ "Rest"
    ),
    bunch_group = factor(bunch_group, levels = config$figures$bunch_groups),
    focal_qtr   = factor(focal_qtr, levels = FOCAL_QTRS),
    mw_hourly   = real_minwage_harmonized / (WEEKS_PER_MONTH * STANDARD_WEEK),
    log2_ratio_monthly = log2(real_salary_income_wage_primary /
                                real_minwage_harmonized),
    log2_ratio_hourly  = log2(real_salary_primary_hourly_base / mw_hourly)
  ) %>%
  dplyr::filter(is.finite(log2_ratio_monthly) | is.finite(log2_ratio_hourly))

cell_counts <- mw_bind_dist_formal %>%
  dplyr::count(focal_qtr, bunch_group, name = "n_obs") %>%
  dplyr::mutate(sparse = n_obs < MIN_CELL_N)

mw_bind_dist_formal <- mw_bind_dist_formal %>%
  dplyr::left_join(cell_counts, by = c("focal_qtr", "bunch_group")) %>%
  # Normalise weights within focal moment x group so each density integrates
  # to 1. 04B re-normalises after trimming the tails.
  dplyr::group_by(focal_qtr, bunch_group) %>%
  dplyr::mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm = TRUE)) %>%
  dplyr::ungroup()

cat("  Cell counts (focal quarter x group):\n")
print(as.data.frame(cell_counts), row.names = FALSE)
if (any(cell_counts$sparse)) {
  warning("Sparse bunching cells (n < ", MIN_CELL_N,
          "); raise dist_pool_halfwidth or drop the group.")
}

save_rds(mw_bind_dist_formal, "mw_bind_dist_formal")
cat("\n")


#===============================================================================
# STEP 6b. Bite against the INCOMING micro floor
#
# Share of workers earning below the micro floor, evaluated at each focal
# moment. This is the pre-reform bite measure: it asks what fraction of
# micro-firm workers were already below the floor that was about to be created.
#
# Reference floor, in constant base pesos:
#   post-2021Q3 quarters -> that quarter's own real micro floor
#   pre-2021Q3  quarters -> the real 2021Q3 micro floor (a single constant in
#                           base pesos; the counterfactual "incoming" floor)
#
# Reported for all three bunch groups so the micro series has a comparison.
# Only the Micro group's pre-reform value is the bite proper; the others show
# what the same threshold implies for firms it never applied to.
#===============================================================================

cat("[6b] Bite vs the incoming micro floor...\n")

micro_floor_at_start <- mw_by_observed %>%
  dplyr::filter(wage_group == "Micro", year_quarter == MICRO_START) %>%
  dplyr::pull(real_minwage_harmonized)
stopifnot(length(micro_floor_at_start) == 1)

cat(sprintf("  Micro floor at %s: %.0f (base-year pesos)\n",
            MICRO_START, micro_floor_at_start))

micro_floor_by_qtr <- mw_by_observed %>%
  dplyr::filter(wage_group == "Micro") %>%
  dplyr::transmute(
    year_quarter,
    micro_floor_ref = dplyr::if_else(year_quarter >= MICRO_START,
                                     real_minwage_harmonized,
                                     micro_floor_at_start),
    micro_floor_is_counterfactual = year_quarter < MICRO_START
  )

mw_bind_micro_bite <- mw_bind_dist_formal %>%
  dplyr::left_join(micro_floor_by_qtr, by = "year_quarter") %>%
  dplyr::group_by(focal_qtr, bunch_group) %>%
  dplyr::summarise(
    bite  = wtd_share_below(real_salary_income_wage_primary,
                            micro_floor_ref, FACTOR_EXPANSION),
    floor_ref = dplyr::first(micro_floor_ref),
    counterfactual = any(micro_floor_is_counterfactual),
    n_obs = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(sparse = n_obs < MIN_CELL_N)

print(as.data.frame(mw_bind_micro_bite), row.names = FALSE)
save_rds(mw_bind_micro_bite, "mw_bind_micro_bite")
cat("\n")


#===============================================================================
# STEP 7. Validation
#===============================================================================

cat("[7] Validation...\n")

decomp_ok <- mw_context_growth_decomp %>%
  dplyr::mutate(chk = round(real_pct_chg + inflation_component - nom_pct_chg, 6))
if (any(decomp_ok$chk != 0, na.rm = TRUE)) {
  warning("  MW-2 decomposition does not sum to nominal.")
} else {
  cat("  MW-2 decomposition sums correctly.\n")
}

cat(sprintf("  MW-4 Kaitz: %d cells, %d sparse\n",
            nrow(mw_bind_kaitz), sum(mw_bind_kaitz$sparse, na.rm = TRUE)))

cat(sprintf("  MW-5 concepts saved: %s | headline = %s\n",
            paste(unique(mw_bind_noncompliance_econ$concept), collapse = ", "),
            config$figures$headline_concept))

cat(sprintf("  MW-6: %d rows across %d focal moments\n",
            nrow(mw_bind_dist_formal),
            dplyr::n_distinct(mw_bind_dist_formal$focal_qtr)))

cat("\n=== 04A complete. Outputs ->", out_dir, "===\n\n")

