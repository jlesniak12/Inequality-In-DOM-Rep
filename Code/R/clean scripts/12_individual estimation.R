#===============================================================================
#
# Script: 12_individual_estimation.R
#
# Purpose: DiD regressions on the individual panel from 10.
#
# Model (pooled DiD):
#   Y_it = alpha_i + gamma_t + beta * (treat_i x post_t) + X'delta + e_it
#
# Model (event study):
#   Y_it = alpha_i + gamma_t + sum_k beta_k * (treat_i x 1[t=k]) + X'delta + e_it
#
# Pipeline: 01A -> 01B -> 02 -> 10 -> 11 -> [12]
#
# Reads:
#   individual_panel.rds from
#   <processed>/Panel Regressions/<event>/<treatment>/<control>/
#   <baseline_rule>/<balance>/<win_tag>/
#
# Writes:
#   <outputs>/.../<treatment>/<control>/<baseline_rule>/<balance>/<win_tag>/
#     tbl_M2_headline_nocontrols.{html,png}   <- main text table
#     tbl_M2_headline_controls.{html,png}     <- appendix twin
#     tbl_M2_full_nocontrols.{html,png}       <- all outcomes
#     tbl_M2_full_controls.{html,png}
#   <outputs>/.../<balance>/<win_tag>/Regression Results/
#     per-outcome modelsummary tables, event-study figures,
#     tbl_M2_pretrend_test.{html,png}   formal pre-trend test (STEP 3b) --
#       per window/balance, since the underlying event-study fit is too
#   <outputs>/.../<treatment>/<control>/<baseline_rule>/
#     manifest_M2.csv, fits_M2_<event>.rds, tbl_M2_pretrend_test.csv (all
#     windows/balances combined, for cross-window comparison)
#
#-------------------------------------------------------------------------------
# CHANGES vs previous version
#
#  1. Headline tables now live in the WINDOW folder (one per window x balance),
#     with specifications as columns. Replaces the old windows-as-columns table,
#     which had a latent bug: distinct(window, n_obs) returns several rows per
#     window whenever N varies across outcomes (i.e. always, on the intensive
#     margin), so pivot_wider silently produced list-columns.
#
#  2. N is reported per outcome per specification, not as a single bottom row.
#     Rows are separate regressions with different samples, so a shared N is
#     not well defined. Reported as "observations / distinct individuals" —
#     the second number is the cluster count that inference actually rests on.
#
#  3. Pre-period means of the dependent variable, for treated and control,
#     added to make coefficients interpretable in relative terms. Displayed
#     for column (1) only (see note in table); stored per spec in the manifest.
#
#  4. Standard errors are two-way clustered on individual + survey PSU
#     (ESTRATO x UPM) in one number, rather than reporting an individual-
#     clustered SE and a separately-recomputed PSU-clustered SE side by side
#     for the reader to choose between -- fixest's multiway clustering
#     computes the statistically correct single SE for both sources of
#     correlation at once. Falls back to individual-only clustering if PSU
#     is unavailable in a given estimation frame.
#
#  5. Control-variable specs get a separate, identically-structured table
#     rather than extra columns. Control coefficients are never displayed.
#
#  6. Outcomes carry metadata (block, primary flag, decimals) in one place:
#     edit OUTCOME_META to change what appears in the headline table.
#
#  7. build_did_sample() now also drops rows with missing controls, so the
#     manifest N for the controls arms matches the estimation frame exactly
#     and is directly comparable to the no-controls arms.
#
#  8. Formal joint pre-trend test added (STEP 3b), reusing the base-arm
#     event-study fit already computed for the ES figures rather than
#     estimating anything new. Tests whether pre-period lead coefficients
#     are jointly zero, distinct from the event-study figure which shows
#     each lead's own point estimate without a combined significance
#     statement.
#
#===============================================================================

# Skip re-sourcing if already loaded (e.g., by the runner script)
if (!exists("config")) {
  source(here::here("Code", "R", "clean scripts", "00_setup.R"))
}

if (!requireNamespace("fixest", quietly = TRUE)) install.packages("fixest")
library(fixest)

cat("=== 12_individual_estimation.R ===\n\n")


#===============================================================================
# STEP 0. Parameters
#===============================================================================

M2_EVENT   <- config$method2$events$event_2021q3_micro
M2_WINDOWS <- config$method2$windows

M2_CONTROL_BW   <- config$method2$control_bandwidth
M2_TREAT_MIN_FS <- config$method2$treatment_min_firmsize

# Must match script 10's rule (same fallback logic)
M2_BASELINE_RULE <- config$method2$baseline_rule %||% "first_qtr_only"

# Two-way cluster (individual + survey PSU) when TRUE, individual-only
# clustering when FALSE. ESTRATO x UPM because UPM is not guaranteed unique
# across strata. CHECK this against your sample design before relying on it.
M2_PSU_SE <- TRUE

# Windows ruled out for the balanced panel by the survey design rather than
# by the data: a household's ENCFT tenure is 5 consecutive quarters, so any
# window whose pre+event+post span exceeds 5 calendar quarters can never
# yield a balanced panel, regardless of what's in the data. Computed here
# (rather than a hardcoded name list) so it updates automatically for any
# window added to config -- mirrors the same check in 10B_panel_attrition.R.
span_len <- function(win) {
  qtrs <- sort(unique(c(win$pre_qtrs, M2_EVENT$event_qtr, win$post_qtrs)))
  as.integer(round(
    (as.numeric(substr(max(qtrs), 1, 4)) - as.numeric(substr(min(qtrs), 1, 4))) * 4 +
      (as.numeric(substr(max(qtrs), 6, 6)) - as.numeric(substr(min(qtrs), 6, 6)))
  )) + 1L
}
M2_SKIP_BALANCED <- names(M2_WINDOWS)[vapply(M2_WINDOWS, function(w) span_len(w) > 5, logical(1))]
if (length(M2_SKIP_BALANCED) > 0) {
  cat(sprintf("  Balanced panel infeasible for: %s (span > 5 quarters)\n",
              paste(M2_SKIP_BALANCED, collapse = ", ")))
}

# Build sample tag (must match scripts 10/11)
M2_TREATMENT_TAG <- if (!is.null(M2_TREAT_MIN_FS) && M2_TREAT_MIN_FS > 1) {
  sprintf("micro%dplus", M2_TREAT_MIN_FS)
} else {
  "micro_all"
}
M2_CONTROL_TAG  <- M2_CONTROL_BW
M2_BASELINE_TAG <- M2_BASELINE_RULE

BALANCE_MODES <- config$method2$active_balance

m2_data_root <- file.path(
  config$paths$processed_data, "Panel Regressions",
  M2_EVENT$event_tag, M2_TREATMENT_TAG, M2_CONTROL_TAG, M2_BASELINE_TAG
)
m2_out_root <- file.path(
  config$paths$outputs, config$output_stage, "Panel Regressions",
  M2_EVENT$event_tag, M2_TREATMENT_TAG, M2_CONTROL_TAG, M2_BASELINE_TAG
)

TREAT_LABEL   <- config$m2_labels$treatment
CONTROL_LABEL <- config$m2_labels$control
CTRL_LABEL    <- CONTROL_LABEL

cat(sprintf("  Treatment: %s | Control: %s | Baseline rule: %s\n",
            M2_TREATMENT_TAG, M2_CONTROL_TAG, M2_BASELINE_TAG))
cat(sprintf("  Data from: %s\n", m2_data_root))
cat(sprintf("  Output to: %s\n\n", m2_out_root))

SRC <- "Source: ENCFT (Banco Central de la Rep\u00fablica Dominicana)."

# Baseline sample restrictions imposed in script 10 (lines 164-184). Stated in
# the table note so the reader can see the FTZ / domestic / utilities cuts.
BASELINE_DESC <- paste(
  "private-sector employees at baseline in micro or small firms, excluding",
  "domestic workers and free-trade-zone and utilities workers, with positive",
  "salary and hours and a known firm-size tier"
)

# Time-varying controls.
# NOTE: with individual AND quarter fixed effects, EDAD is f(individual) +
# g(calendar time) up to birthday timing, so it is close to collinear and its
# coefficient is not interpretable. is_sec_complete / is_tert_complete rarely
# change for adults over four quarters. Expect the controls table to be close
# to identical to the no-controls table; if it is not, compare n_obs first.
TV_CONTROLS <- c("EDAD", "is_sec_complete", "is_tert_complete")


#===============================================================================
# STEP 0b. Outcome metadata
#
# One place to control what appears where. `primary` drives the headline
# table; `block` drives the row groups; `dec` drives decimal places.
#===============================================================================

OUTCOME_META <- tibble::tribble(
  ~outcome,                       ~label,                        ~block,                                           ~margin_type, ~primary, ~dec,
  "is_employed",                  "Employed",                    "A. Labour force status",                          "EXT",  TRUE,  3,
  "is_unemployed",                "Unemployed",                  "A. Labour force status",                          "EXT",  TRUE,  3,
  "is_out_of_lf",                 "Out of labour force",         "A. Labour force status",                          "EXT",  TRUE,  3,
  
  "is_private_employee",          "Private employee",            "B. Employment type",                              "EXT",  TRUE,  3,
  "is_independent_now",           "Independent (SE + employer)", "B. Employment type",                              "EXT",  TRUE,  3,
  "is_selfemp_now",               "Self-employed",               "B. Employment type",                              "EXT",  FALSE, 3,
  "is_owner_now",                 "Employer / patron",           "B. Employment type",                              "EXT",  FALSE, 3,
  
  "is_formal_private",            "Formal private employee",     "C. Formality",                                    "EXT",  TRUE,  3,
  "is_informal_now",              "Informal",                    "C. Formality",                                    "EXT",  TRUE,  3,
  
  "log_real_hwage",               "Log hourly wage",             "D. Wages and hours (conditional on wage)",        "INT",  TRUE,  3,
  "log_real_mwage",               "Log monthly wage",            "D. Wages and hours (conditional on wage)",        "INT",  FALSE, 3,
  "hours_worked_primary",         "Weekly hours",                "D. Wages and hours (conditional on wage)",        "INT",  TRUE,  2,
  "below_min_hourly_base_salary", "Below hourly MW floor",       "D. Wages and hours (conditional on wage)",        "INT",  TRUE,  3,
  "log_real_total_income",        "Log total income",            "D. Wages and hours (conditional on wage)",        "INT",  FALSE, 3
) %>%
  dplyr::mutate(ord = dplyr::row_number())

# Back-compat vectors used by the per-outcome tables and ES figure captions
ALL_OUTCOMES <- setNames(OUTCOME_META$label, OUTCOME_META$outcome)
EXTENSIVE_VARS <- OUTCOME_META$outcome[OUTCOME_META$margin_type == "EXT"]


#===============================================================================
# STEP 1. Estimation helpers
#===============================================================================

# Build the estimation frame. Dropping NA controls here means the manifest N
# matches what feols actually uses, so N is comparable across arms.
build_did_sample <- function(df, yvar, controls = NULL,
                             drop_switchers = FALSE, subset_col = NULL) {
  
  d <- df[!is.na(df[[yvar]]), , drop = FALSE]
  
  if (!is.null(controls) && length(controls) > 0) {
    ok <- stats::complete.cases(d[, controls, drop = FALSE])
    d <- d[ok, , drop = FALSE]
  }
  if (drop_switchers) d <- d[!d$tier_switcher, , drop = FALSE]
  if (!is.null(subset_col)) {
    keep <- !is.na(d[[subset_col]]) & d[[subset_col]] == 1L
    d <- d[keep, , drop = FALSE]
  }
  d
}


run_did <- function(d, yvar, controls = NULL, weights = NULL) {
  
  if (nrow(d) < 20) return(NULL)
  
  ctrl_str <- if (!is.null(controls) && length(controls) > 0) {
    paste("+", paste(controls, collapse = " + "))
  } else ""
  
  fml <- as.formula(sprintf(
    "%s ~ treat:post %s | ID_PERSONA + year_quarter", yvar, ctrl_str
  ))
  
  w <- if (!is.null(weights)) d[[weights]] else NULL
  
  tryCatch(
    fixest::feols(fml, data = d, weights = w,
                  vcov = m2_vcov_formula(d), warn = FALSE, notes = FALSE),
    error = function(e) { message("    feols error: ", e$message); NULL }
  )
}


run_event_study <- function(d, yvar, ref_time = -1L, controls = NULL,
                            weights = NULL) {
  
  d <- d[!is.na(d$event_time), , drop = FALSE]
  d$event_time_f <- factor(d$event_time)
  
  if (nrow(d) < 20 || length(unique(d$event_time)) < 3) return(NULL)
  
  ctrl_str <- if (!is.null(controls) && length(controls) > 0) {
    paste("+", paste(controls, collapse = " + "))
  } else ""
  
  fml <- as.formula(sprintf(
    "%s ~ i(event_time_f, treat, ref = '%d') %s | ID_PERSONA + year_quarter",
    yvar, ref_time, ctrl_str
  ))
  
  w <- if (!is.null(weights)) d[[weights]] else NULL
  
  tryCatch(
    fixest::feols(fml, data = d, weights = w,
                  vcov = m2_vcov_formula(d), warn = FALSE, notes = FALSE),
    error = function(e) { message("    ES error: ", e$message); NULL }
  )
}


# Two-way clustering (individual + survey PSU) when PSU is available and
# M2_PSU_SE is on; individual-only otherwise. Replaces the previous design
# of fitting with vcov = ~ID_PERSONA only and then separately re-summarizing
# with cluster = ~psu_id to get a SECOND standard error shown alongside the
# first -- fixest's multiway clustering computes the single, correct SE for
# both sources of correlation at once, in the fit itself, rather than
# reporting two univariate-clustered numbers side by side for the reader to
# choose between.
m2_vcov_formula <- function(d) {
  if (isTRUE(M2_PSU_SE) && "psu_id" %in% names(d)) {
    stats::as.formula("~ID_PERSONA + psu_id")
  } else {
    stats::as.formula("~ID_PERSONA")
  }
}


# Pull everything the tables need out of one fit + its estimation frame.
summarise_did <- function(fit, d, yvar, weight_col = NULL) {
  
  if (is.null(fit)) return(NULL)
  
  ct <- fixest::coeftable(fit)
  rn <- grep("treat.*post|post.*treat", rownames(ct), value = TRUE)
  if (length(rn) == 0) return(NULL)
  rn <- rn[1]
  
  se_is_twoway <- isTRUE(M2_PSU_SE) && "psu_id" %in% names(d)
  
  # feols drops zero / missing weights: mirror that so N and the cluster
  # count describe the estimation frame
  if (!is.null(weight_col)) {
    keep_w <- !is.na(d[[weight_col]]) & d[[weight_col]] > 0
    d <- d[keep_w, , drop = FALSE]
  }
  
  # Pre-period means of the dependent variable, weighted if the spec is
  dpre <- d[!is.na(d$period) & d$period == "pre", , drop = FALSE]
  wpre <- if (!is.null(weight_col)) dpre[[weight_col]] else rep(1, nrow(dpre))
  
  mean_at <- function(sel) {
    if (!any(sel)) return(NA_real_)
    stats::weighted.mean(dpre[[yvar]][sel], wpre[sel], na.rm = TRUE)
  }
  
  tibble::tibble(
    beta           = unname(ct[rn, "Estimate"]),
    se             = unname(ct[rn, "Std. Error"]),
    pvalue         = unname(ct[rn, "Pr(>|t|)"]),
    se_twoway      = se_is_twoway,
    n_obs          = nobs(fit),
    n_indiv        = dplyr::n_distinct(d$ID_PERSONA),
    n_psu          = if ("psu_id" %in% names(d)) dplyr::n_distinct(d$psu_id) else NA_integer_,
    mean_treat_pre = mean_at(dpre$treat == 1),
    mean_ctrl_pre  = mean_at(dpre$treat == 0)
  )
}


extract_es_coefs <- function(fit, ref_time = -1L) {
  if (is.null(fit)) return(NULL)
  
  es_coefs <- fixest::coeftable(fit) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    filter(grepl("event_time_f", term)) %>%
    mutate(
      event_time = as.integer(gsub(".*::(-?\\d+):.*", "\\1", term)),
      ci_lo = Estimate - 1.96 * `Std. Error`,
      ci_hi = Estimate + 1.96 * `Std. Error`
    )
  
  bind_rows(
    es_coefs,
    tibble::tibble(term = "ref", Estimate = 0, `Std. Error` = 0,
                   event_time = ref_time, ci_lo = 0, ci_hi = 0)
  ) %>% arrange(event_time)
}


#===============================================================================
# STEP 2. Run estimations
#===============================================================================

manifest_rows <- list()
pretrend_rows <- list()
all_fits <- list()
missing_panels <- character(0)

for (win_name in names(M2_WINDOWS)) {
  
  win <- M2_WINDOWS[[win_name]]
  
  for (balance_label in BALANCE_MODES) {
    
    cat(sprintf("\n========== Window: %s | %s ==========\n\n",
                win$label, balance_label))
    
    if (balance_label == "balanced" && win_name %in% M2_SKIP_BALANCED) {
      cat("  Skipped by design: the 5-quarter ENCFT rotation cannot produce a\n")
      cat("  balanced panel for this window. Unbalanced runs only.\n\n")
      next
    }
    
    win_data_dir <- file.path(m2_data_root, balance_label, win$tag)
    win_dir      <- file.path(m2_out_root, balance_label, win$tag)
    win_out_dir  <- file.path(win_dir, "Regression Results")
    dir.create(win_out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Accumulated per this (window, balance) only, so the pre-trend test
    # table can be saved right here in this window's own Regression Results
    # folder -- see STEP 3b-note after the outcome loop below for why it
    # lives per-window rather than as one cross-window table.
    pretrend_rows_local <- list()
    
    save_fig <- function(p, name,
                         w = config$fig_defaults$width,
                         h = config$fig_defaults$height) {
      fp <- file.path(win_out_dir,
                      paste0(name, ".", config$fig_defaults$format))
      ggsave(fp, plot = p, width = w, height = h, dpi = config$fig_defaults$dpi)
      message("  Saved: ", fp)
    }
    
    panel_file <- file.path(win_data_dir, "individual_panel.rds")
    if (!file.exists(panel_file)) {
      cat(sprintf("  Panel not found: %s — skipping.\n\n", panel_file))
      missing_panels <- c(missing_panels, panel_file)
      next
    }
    
    panel <- readRDS(panel_file) %>%
      mutate(ID_PERSONA   = factor(ID_PERSONA),
             year_quarter = factor(year_quarter),
             event_time_f = factor(event_time))
    
    if (nrow(panel) == 0) {
      cat("  Panel is empty (0 obs) — skipping.\n\n")
      next
    }
    
    # PSU identifier for two-way clustering. UPM may repeat across strata.
    if (all(c("ESTRATO", "UPM") %in% names(panel))) {
      panel$psu_id <- factor(paste(panel$ESTRATO, panel$UPM, sep = "_"))
    } else if ("UPM" %in% names(panel)) {
      panel$psu_id <- factor(panel$UPM)
    }
    
    cat(sprintf("  %s obs | %d persons | %d quarters\n",
                format(nrow(panel), big.mark = ","),
                n_distinct(panel$ID_PERSONA),
                n_distinct(panel$year_quarter)))
    
    fig_sub <- sprintf("Micro vs Small | %s | %s", win$label, balance_label)
    
    # --- Estimation arms ---
    # Columns (1)-(3) of the headline table are base_unw / formal_unw / base_wt.
    # The controls twin uses ctrl_unw / formal_ctrl / ctrl_wt.
    # nosw_unw is estimated but not tabulated: dropping tier switchers conditions
    # on a post-treatment outcome, so it belongs in the text with a caveat.
    #
    # formal_wt / formal_wt_ctrl added so the formal-at-baseline subset has
    # the same full {weighted x controls} 2x2 the baseline subset already
    # had -- without these, a "compare all specifications" table would be
    # asymmetric (4 baseline variants vs only 2 formal variants).
    ARMS <- list(
      list(arm_tag = "base_unw",    weight_col = NULL,
           drop_switch = FALSE, controls = NULL,        subset_col = NULL,
           arm_label = "Unweighted"),
      list(arm_tag = "ctrl_unw",    weight_col = NULL,
           drop_switch = FALSE, controls = TV_CONTROLS, subset_col = NULL,
           arm_label = "Unweighted + controls"),
      list(arm_tag = "base_wt",     weight_col = "FACTOR_EXPANSION",
           drop_switch = FALSE, controls = NULL,        subset_col = NULL,
           arm_label = "Weighted"),
      list(arm_tag = "ctrl_wt",     weight_col = "FACTOR_EXPANSION",
           drop_switch = FALSE, controls = TV_CONTROLS, subset_col = NULL,
           arm_label = "Weighted + controls"),
      list(arm_tag = "nosw_unw",    weight_col = NULL,
           drop_switch = TRUE,  controls = NULL,        subset_col = NULL,
           arm_label = "Excl. switchers"),
      list(arm_tag = "formal_unw",  weight_col = NULL,
           drop_switch = FALSE, controls = NULL,        subset_col = "baseline_formal",
           arm_label = "Formal at baseline"),
      list(arm_tag = "formal_ctrl", weight_col = NULL,
           drop_switch = FALSE, controls = TV_CONTROLS, subset_col = "baseline_formal",
           arm_label = "Formal at baseline + controls"),
      list(arm_tag = "formal_wt",   weight_col = "FACTOR_EXPANSION",
           drop_switch = FALSE, controls = NULL,        subset_col = "baseline_formal",
           arm_label = "Formal at baseline, weighted"),
      list(arm_tag = "formal_wt_ctrl", weight_col = "FACTOR_EXPANSION",
           drop_switch = FALSE, controls = TV_CONTROLS, subset_col = "baseline_formal",
           arm_label = "Formal at baseline, weighted + controls")
    )
    
    outcomes_present <- intersect(OUTCOME_META$outcome, names(panel))
    
    for (yvar in outcomes_present) {
      
      ylabel <- ALL_OUTCOMES[[yvar]]
      is_extensive <- yvar %in% EXTENSIVE_VARS
      margin_label <- if (is_extensive) "EXT" else "INT"
      
      cat(sprintf("  [%s] %s (%s)\n", margin_label, ylabel, yvar))
      
      fits_outcome <- list()
      
      for (spec in ARMS) {
        
        d <- build_did_sample(panel, yvar,
                              controls = spec$controls,
                              drop_switchers = spec$drop_switch,
                              subset_col = spec$subset_col)
        
        fit <- run_did(d, yvar,
                       controls = spec$controls,
                       weights = spec$weight_col)
        if (is.null(fit)) next
        
        fits_outcome[[spec$arm_tag]] <- fit
        
        st <- summarise_did(fit, d, yvar, weight_col = spec$weight_col)
        if (is.null(st)) next
        
        manifest_rows[[length(manifest_rows) + 1]] <- dplyr::bind_cols(
          tibble::tibble(
            window = win_name, window_tag = win$tag,
            balance = balance_label,
            baseline_rule = M2_BASELINE_RULE,
            outcome = yvar, outcome_label = ylabel,
            margin = margin_label,
            arm = spec$arm_tag, arm_label = spec$arm_label
          ),
          st
        )
        
        stars <- if (st$pvalue < 0.01) "***" else if (st$pvalue < 0.05) "**" else
          if (st$pvalue < 0.10) "*" else ""
        
        cat(sprintf("    [%s] b=%.4f (%.4f)%s  n=%d  indiv=%d\n",
                    spec$arm_tag, st$beta, st$se, stars, st$n_obs, st$n_indiv))
      }
      
      # --- Per-outcome regression table (kept, in Regression Results) ---
      headline_fits <- fits_outcome[c("base_unw", "ctrl_unw",
                                      "base_wt", "ctrl_wt",
                                      "formal_unw", "formal_ctrl",
                                      "formal_wt", "formal_wt_ctrl")]
      headline_fits <- headline_fits[!vapply(headline_fits, is.null,
                                             logical(1))]
      
      if (length(headline_fits) >= 2) {
        
        tbl_notes <- list(
          sprintf("Treatment: %s | Control: %s", TREAT_LABEL, CONTROL_LABEL),
          "Individual + quarter FE. SEs two-way clustered on individual and survey PSU where available.",
          if (is_extensive) "Extensive margin: all panel members."
          else "Intensive margin: conditional on positive wage and hours.",
          sprintf("Panel: %s.", balance_label)
        )
        
        tryCatch({
          tbl_gt <- modelsummary::msummary(
            headline_fits,
            stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
            coef_omit = paste(TV_CONTROLS, collapse = "|"),
            title = sprintf("DiD: %s (%s, %s)", ylabel, win$label, balance_label),
            notes = tbl_notes,
            output = "gt"
          )
          tbl_stem <- file.path(win_out_dir, sprintf("tbl_M2_did_%s", yvar))
          gt::gtsave(tbl_gt, paste0(tbl_stem, ".html"))
          gt::gtsave(tbl_gt, paste0(tbl_stem, ".png"), expand = 10)
        }, error = function(e) {
          cat(sprintf("    Table error: %s\n", e$message))
        })
      }
      
      # --- Event study ---
      es_arms <- list(
        list(tag = "base",   controls = NULL,        subset_col = NULL,
             label = "No controls"),
        list(tag = "ctrl",   controls = TV_CONTROLS, subset_col = NULL,
             label = "With controls"),
        list(tag = "formal", controls = NULL,        subset_col = "baseline_formal",
             label = "Formal at baseline")
      )
      
      for (es_spec in es_arms) {
        
        d_es <- build_did_sample(panel, yvar,
                                 controls = es_spec$controls,
                                 subset_col = es_spec$subset_col)
        
        es_fit <- run_event_study(d_es, yvar, ref_time = -1L,
                                  controls = es_spec$controls)
        if (is.null(es_fit)) next
        
        fits_outcome[[paste0("es_", es_spec$tag)]] <- es_fit
        
        # --- Formal joint pre-trend test (base arm only) ---
        # Tests whether the pre-period lead coefficients (everything before
        # the reference quarter, t=-1) are JOINTLY zero, using the SAME
        # fitted event-study model as the figure above -- not a new
        # regression. This is a different question than the event-study
        # plot answers: the plot shows each lead's own point estimate and CI
        # for eyeballing; this collapses them into one p-value.
        #
        # With only one pre-period lead (e.g. the symmetric_2_2 window,
        # which has 2 pre quarters so only one lead survives after the
        # reference), a "joint" test on a single coefficient is just that
        # coefficient's own t-test -- already visible as the one pre-period
        # point on the event-study figure. This test only adds genuinely
        # new information for windows with >=3 pre quarters (e.g. a 3-pre
        # window), where it can jointly test 2+ leads at once.
        if (es_spec$tag == "base") {
          lead_names <- grep("^event_time_f::-", names(coef(es_fit)), value = TRUE)
          # Excludes the reference level (t=-1 never gets its own coefficient)
          # and any post-period lags (event_time_f::0, ::1, ... have no "-").
          
          pt <- tryCatch({
            if (length(lead_names) == 0) {
              NULL
            } else if (length(lead_names) == 1) {
              # Single lead: joint test degenerates to that lead's own
              # t-test. Report it directly rather than calling wald() on a
              # length-1 hypothesis, which some fixest versions reject.
              ct <- fixest::coeftable(es_fit)
              tibble::tibble(n_leads = 1L, stat = ct[lead_names, "t value"]^2,
                             df1 = 1L, df2 = NA_real_,
                             pvalue = ct[lead_names, "Pr(>|t|)"])
            } else {
              w <- fixest::wald(es_fit, keep = lead_names)
              tibble::tibble(n_leads = length(lead_names), stat = w$stat,
                             df1 = w$df1, df2 = w$df2, pvalue = w$p)
            }
          }, error = function(e) {
            message("    Pre-trend test error: ", e$message)
            NULL
          })
          
          if (!is.null(pt)) {
            row <- dplyr::bind_cols(
              tibble::tibble(window = win_name, window_tag = win$tag,
                             balance = balance_label, outcome = yvar,
                             outcome_label = ylabel, margin = margin_label),
              pt
            )
            pretrend_rows[[length(pretrend_rows) + 1]] <- row       # global, for cross-window CSV
            pretrend_rows_local[[length(pretrend_rows_local) + 1]] <- row  # this window/balance only
          }
        }
        
        es_coefs <- extract_es_coefs(es_fit, ref_time = -1L)
        if (is.null(es_coefs) || nrow(es_coefs) <= 1) next
        
        fig_es <- ggplot(es_coefs, aes(x = event_time, y = Estimate)) +
          geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
          geom_vline(xintercept = -0.5, linetype = "dashed",
                     colour = "red", linewidth = 0.4) +
          geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
                      alpha = 0.15, fill = "#4575b4") +
          geom_point(size = 2.5, colour = "#4575b4") +
          geom_line(linewidth = 0.6, colour = "#4575b4") +
          scale_x_continuous(breaks = sort(unique(es_coefs$event_time))) +
          labs(
            title = sprintf("Event Study: %s (%s)", ylabel, margin_label),
            subtitle = sprintf("%s | %s | Ref: t=-1 (2021Q2)",
                               fig_sub, es_spec$label),
            x = "Quarters relative to 2021Q3",
            y = "Coefficient (Micro x quarter)",
            caption = paste(
              "Individual + quarter FE. 95% CI.",
              "SEs two-way clustered on individual and survey PSU where available.",
              if (is_extensive) "Extensive margin: all panel members."
              else "Intensive margin: conditional on positive wage.",
              SRC)
          ) +
          theme_surveytools()
        
        save_fig(fig_es, sprintf("fig_M2_es_%s_%s", yvar, es_spec$tag))
      }
      
      all_fits[[paste(win_name, balance_label, yvar, sep = "__")]] <-
        fits_outcome
    }
    
    
    #---------------------------------------------------------------------------
    # Formal pre-trend test table, THIS window/balance only
    #
    # Saved here rather than as one cross-window table, since the test is
    # only non-degenerate (n_leads >= 2) for windows with 3+ pre quarters --
    # keeping it inside each window's own Regression Results folder means a
    # reader looking at one window's results sees immediately whether the
    # test applies there, rather than hunting through a combined table for
    # the rows that matter. A combined CSV across all windows is still
    # written once at the end (STEP 3b) for convenience/comparison.
    #---------------------------------------------------------------------------
    
    pretrend_win <- dplyr::bind_rows(pretrend_rows_local)
    
    if (nrow(pretrend_win) > 0) {
      
      n_leads_here <- unique(pretrend_win$n_leads)
      degenerate <- length(n_leads_here) > 0 && all(n_leads_here <= 1)
      
      tbl_pretrend_win <- pretrend_win %>%
        mutate(
          stat_fmt = sprintf("%.2f", stat),
          pvalue_fmt = sprintf("%.3f", pvalue),
          reject = ifelse(pvalue < 0.05, "Yes", "No")
        ) %>%
        select(margin, outcome_label, n_leads, stat_fmt, pvalue_fmt, reject) %>%
        arrange(margin, outcome_label) %>%
        gt::gt(groupname_col = "margin") %>%
        gt::cols_label(outcome_label = "Outcome", n_leads = "Leads tested",
                       stat_fmt = "Stat", pvalue_fmt = "p-value",
                       reject = "Reject H0 (p<.05)?") %>%
        gt::tab_header(
          title = "Formal test of parallel pre-trends",
          subtitle = sprintf("%s | %s panel | base (no-controls) arm",
                             win$label, balance_label)) %>%
        gt::tab_source_note(paste(
          "H0: pre-period event-study lead coefficients (before the",
          "reference quarter t=-1) are jointly zero, from the same",
          "event-study model plotted in fig_M2_es_*_base.")) %>%
        {if (degenerate) gt::tab_source_note(., paste(
          "This window has only 1 pre-period lead after the reference",
          "quarter, so the 'joint' test here is exactly that lead's own",
          "t-test -- it adds nothing beyond the single pre-period point",
          "already shown on the event-study figure. A genuine joint test",
          "needs a window with >= 3 pre quarters.")) else .} %>%
        gt::tab_source_note(paste(
          "'Reject H0' = Yes means evidence AGAINST parallel pre-trends for",
          "that outcome -- treat the corresponding DiD estimate with",
          "caution. Not corrected for multiple comparisons across outcomes.")) %>%
        gt::tab_source_note(SRC)
      
      tryCatch({
        gt::gtsave(tbl_pretrend_win, file.path(win_out_dir, "tbl_M2_pretrend_test.html"))
        gt::gtsave(tbl_pretrend_win, file.path(win_out_dir, "tbl_M2_pretrend_test.png"),
                   expand = 10)
        cat(sprintf("    Pre-trend test (%s, %s): %d outcomes%s\n",
                    win$label, balance_label, nrow(pretrend_win),
                    if (degenerate) " -- single lead, see note in table" else ""))
      }, error = function(e) {
        cat(sprintf("    Pre-trend test table failed (%s, %s): %s\n",
                    win$label, balance_label, e$message))
      })
    }
    
  } # end balance loop
  
} # end window loop


#===============================================================================
# STEP 3. Save manifest and fits
#===============================================================================

manifest <- dplyr::bind_rows(manifest_rows)

if (nrow(manifest) == 0) {
  cat("\n[!] No regressions were estimated. Nothing to tabulate.\n")
  if (length(missing_panels) > 0) {
    cat("    No panel file was found at:\n")
    cat(paste0("      ", missing_panels, collapse = "\n"), "\n")
    cat(sprintf(
      paste0("    The sample folders are treatment='%s', control='%s',",
             " baseline_rule='%s' (control_bandwidth='%s', treatment_min_firmsize=%s).\n",
             "    Re-run scripts 10 and 11 with the current config, or point",
             " the config back\n    at a sample that has already been",
             " built.\n"),
      M2_TREATMENT_TAG, M2_CONTROL_TAG, M2_BASELINE_TAG, M2_CONTROL_BW,
      if (is.null(M2_TREAT_MIN_FS)) "NULL" else M2_TREAT_MIN_FS))
  }
  cat("\n=== 12_individual_estimation.R stopped (empty manifest) ===\n")
  # Leave any previous manifest on disk rather than overwriting it with nothing
} else {
  
  manifest_file <- file.path(m2_out_root, "manifest_M2.csv")
  readr::write_csv(manifest, manifest_file)
  cat(sprintf("\n[3] Manifest: %s (%d rows)\n",
              basename(manifest_file), nrow(manifest)))
  
  fits_file <- file.path(m2_out_root,
                         sprintf("fits_M2_%s.rds", M2_EVENT$event_tag))
  saveRDS(all_fits, fits_file)
  cat(sprintf("    Fits: %s\n", basename(fits_file)))
  
  
  #===============================================================================
  # STEP 3b. Consolidated pre-trend test CSV (all windows/balances)
  #
  # The per-window/balance HTML/PNG tables (saved above, inside each
  # window's Regression Results folder, right after that window's outcome
  # loop) are the primary artifact -- see the comment there for why the test
  # lives per-window rather than as one combined table. This CSV is kept as
  # a convenience copy for cross-window comparison (e.g. feeding
  # 13_regression_comparisons.R), not a reader-facing table.
  #===============================================================================
  
  pretrend <- dplyr::bind_rows(pretrend_rows)
  
  if (nrow(pretrend) > 0) {
    readr::write_csv(pretrend, file.path(m2_out_root, "tbl_M2_pretrend_test.csv"))
    n_reject <- sum(pretrend$pvalue < 0.05, na.rm = TRUE)
    cat(sprintf("    Pre-trend test CSV: tbl_M2_pretrend_test.csv (%d rows, %d reject at p<.05)\n",
                nrow(pretrend), n_reject))
  }
  
  
  #===============================================================================
  # STEP 4. Headline tables — one per window x balance x control-variant
  #
  # Layout:
  #   rows    = outcomes, grouped into blocks, three lines each
  #             (coefficient / two-way-clustered SE / observations per
  #              individuals)
  #   columns = specifications, plus two leading pre-period mean columns
  #
  # Saved in the window folder, above Regression Results.
  #===============================================================================
  
  cat("\n[4] Building headline tables...\n\n")
  
  MAIN_ARMS <- tibble::tribble(
    ~arm,          ~col_id, ~col_label,
    "base_unw",    "c1",    "(1) Baseline",
    "formal_unw",  "c2",    "(2) Formal at baseline",
    "base_wt",     "c3",    "(3) Survey weighted"
  )
  
  CTRL_ARMS <- tibble::tribble(
    ~arm,          ~col_id, ~col_label,
    "ctrl_unw",    "c1",    "(1) Baseline",
    "formal_ctrl", "c2",    "(2) Formal at baseline",
    "ctrl_wt",     "c3",    "(3) Survey weighted"
  )
  
  # formatC's digits is not reliably vectorised
  fmt_vec <- function(x, d) {
    purrr::map2_chr(x, d, function(v, dd) {
      if (is.na(v)) NA_character_ else formatC(v, format = "f", digits = dd)
    })
  }
  
  fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")
  
  
  build_spec_table <- function(mf, win_name, win_label, bal, arms,
                               primary_only, variant_label) {
    
    meta <- OUTCOME_META %>%
      select(outcome, label, block, dec, primary, ord)
    
    d <- mf %>%
      filter(window == win_name, balance == bal, arm %in% arms$arm) %>%
      inner_join(meta, by = "outcome") %>%
      left_join(arms, by = "arm")
    
    if (primary_only) d <- d %>% filter(primary)
    if (nrow(d) == 0) return(NULL)
    
    # One row per outcome per column, estimate/SE/N stacked in a single cell
    # via <br> (a bare "\n" is a markdown soft break and commonmark renders
    # it as a space, not a line break -- <br> is raw HTML and passes through
    # untouched, which is what actually stacks the lines).
    d <- d %>%
      mutate(
        stars  = case_when(pvalue < 0.01 ~ "***", pvalue < 0.05 ~ "**",
                           pvalue < 0.10 ~ "*", TRUE ~ ""),
        cell = paste0(
          fmt_vec(beta, dec), stars,
          "<br><span style='font-size:0.82em;color:#444'>(",
          fmt_vec(se, dec), ")</span>",
          "<br><span style='font-size:0.72em;color:#888'>",
          fmt_int(n_obs), " / ", fmt_int(n_indiv), "</span>"
        )
      )
    
    col_ids <- arms$col_id
    
    body <- d %>%
      select(ord, block, label, col_id, cell) %>%
      tidyr::pivot_wider(names_from = col_id, values_from = cell) %>%
      arrange(ord)
    
    # Specs that produced nothing for an outcome
    for (cc in col_ids) if (!cc %in% names(body)) body[[cc]] <- NA_character_
    body <- body %>%
      mutate(across(all_of(col_ids), ~ ifelse(is.na(.x), "\u2014", .x)))
    
    # Pre-period means, from column (1) only
    base_arm <- arms$arm[1]
    means <- d %>%
      filter(arm == base_arm) %>%
      distinct(ord, dec, mean_treat_pre, mean_ctrl_pre) %>%
      mutate(mean_treat = fmt_vec(mean_treat_pre, dec),
             mean_ctrl  = fmt_vec(mean_ctrl_pre, dec)) %>%
      select(ord, mean_treat, mean_ctrl)
    
    body <- body %>%
      left_join(means, by = "ord") %>%
      select(block, label, mean_treat, mean_ctrl, all_of(col_ids))
    
    lab_list <- as.list(c("Outcome", "Micro", "Small", arms$col_label))
    names(lab_list) <- c("label", "mean_treat", "mean_ctrl", col_ids)
    
    tbl <- body %>%
      gt::gt(groupname_col = "block") %>%
      gt::row_group_order(groups = unique(body$block))
    
    tbl <- do.call(gt::cols_label, c(list(tbl), lab_list))
    
    tbl <- tbl %>%
      gt::fmt_markdown(columns = all_of(col_ids)) %>%
      gt::tab_spanner(label = "Pre-period mean",
                      columns = c("mean_treat", "mean_ctrl")) %>%
      gt::tab_header(
        title = "Effect of the 2021Q3 micro-firm minimum wage carve-out",
        subtitle = sprintf("%s | %s | %s panel | %s",
                           win_label, CTRL_LABEL, bal, variant_label)
      ) %>%
      gt::cols_align(align = "center",
                     columns = c("mean_treat", "mean_ctrl", col_ids)) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = "label")
      ) %>%
      gt::tab_source_note(sprintf(
        paste("Each row is a separate regression of the outcome on Micro x Post",
              "with individual and quarter fixed effects. Treatment: %s.",
              "Control: %s. Baseline sample: %s."),
        TREAT_LABEL, CONTROL_LABEL, BASELINE_DESC)) %>%
      gt::tab_source_note(paste(
        "Each cell: DiD coefficient with stars, the standard error",
        "(in parentheses) on the second line, and observations / distinct",
        "individuals on the third line. SEs are two-way clustered on",
        "individual and survey PSU (ESTRATO x UPM) where PSU is available",
        "in the estimation frame, individual-only otherwise. The number of",
        "individuals is the cluster count on which inference rests.")) %>%
      gt::tab_source_note(paste(
        "Pre-period means are computed on the column (1) estimation sample and",
        "are informative for column (1) only: columns (2) and (3) restrict the",
        "sample or apply survey weights, so these means do not describe them.",
        "Specification-specific means are in manifest_M2.csv.")) %>%
      gt::tab_source_note(paste(
        "Wages and hours (Panel D) are conditional on an observed positive",
        "wage, so are subject to selection: Panel B's estimate on private",
        "employee status is the closest reported check on whether that",
        "selection differs by treatment (it does not capture selection from",
        "having a private-employee job but a zero/missing reported wage",
        "specifically, which is not separately reported as of this table).",
        "Where the Panel B effect is non-zero, Panel D estimates should not",
        "be read as effects on wages alone.")) %>%
      gt::tab_source_note(
        "* p<0.10, ** p<0.05, *** p<0.01") %>%
      gt::tab_source_note(SRC) %>%
      gt::tab_options(
        table.font.size = gt::px(12),
        heading.title.font.size = gt::px(14),
        heading.subtitle.font.size = gt::px(11),
        column_labels.font.weight = "bold",
        row_group.font.weight = "bold",
        source_notes.font.size = gt::px(10),
        data_row.padding = gt::px(3)
      )
    
    tbl
  }
  
  
  TABLE_VARIANTS <- list(
    list(tag = "nocontrols", arms = MAIN_ARMS, label = "No controls"),
    list(tag = "controls",   arms = CTRL_ARMS,
         label = "With time-varying controls")
  )
  
  for (win_name in names(M2_WINDOWS)) {
    
    win <- M2_WINDOWS[[win_name]]
    
    for (bal in BALANCE_MODES) {
      
      if (!any(manifest$window == win_name & manifest$balance == bal)) next
      
      win_dir <- file.path(m2_out_root, bal, win$tag)
      dir.create(win_dir, recursive = TRUE, showWarnings = FALSE)
      
      for (v in TABLE_VARIANTS) {
        for (scope in c("headline", "full")) {
          
          tbl <- build_spec_table(
            manifest, win_name, win$label, bal, v$arms,
            primary_only = (scope == "headline"),
            variant_label = v$label
          )
          if (is.null(tbl)) next
          
          stem <- file.path(win_dir, sprintf("tbl_M2_%s_%s", scope, v$tag))
          tryCatch({
            gt::gtsave(tbl, paste0(stem, ".html"))
            gt::gtsave(tbl, paste0(stem, ".png"), expand = 10)
            cat(sprintf("    %s / %s / %s_%s: saved\n",
                        win$tag, bal, scope, v$tag))
          }, error = function(e) {
            cat(sprintf("    %s / %s / %s_%s: error — %s\n",
                        win$tag, bal, scope, v$tag, e$message))
          })
        }
      }
    }
  }
  
  
  #===============================================================================
  # STEP 5. Console summary
  #===============================================================================
  
  for (bal in BALANCE_MODES) {
    
    if (!any(manifest$balance == bal & manifest$arm == "base_unw")) next
    
    cat(sprintf("\n  Console summary (base_unw, %s):\n", bal))
    
    console <- manifest %>%
      filter(arm == "base_unw", balance == bal) %>%
      mutate(stars = case_when(pvalue < 0.01 ~ "***", pvalue < 0.05 ~ "**",
                               pvalue < 0.10 ~ "*", TRUE ~ ""),
             result = sprintf("%.3f (%.3f)%s [n=%d]", beta, se, stars, n_indiv))
    
    if (nrow(console) == 0) next
    
    for (m in c("EXT", "INT")) {
      cat(sprintf("  -- %s --\n", if (m == "EXT") "Extensive" else "Intensive"))
      tbl_c <- console %>%
        filter(margin == m) %>%
        select(window, outcome_label, result) %>%
        tidyr::pivot_wider(names_from = window, values_from = result)
      print(tbl_c, n = 20)
      cat("\n")
    }
  }
  
  cat("\n=== 12_individual_estimation.R complete ===\n")
  
} # end of: if (nrow(manifest) == 0) ... else