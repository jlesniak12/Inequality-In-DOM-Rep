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
#   <processed>/Panel Regressions/<event>/<sample_tag>/<balance>/<win_tag>/
#
# Writes:
#   <outputs>/.../<balance>/<win_tag>/
#     tbl_M2_headline_nocontrols.{html,png}   <- main text table
#     tbl_M2_headline_controls.{html,png}     <- appendix twin
#     tbl_M2_full_nocontrols.{html,png}       <- all outcomes
#     tbl_M2_full_controls.{html,png}
#   <outputs>/.../<balance>/<win_tag>/Regression Results/
#     per-outcome modelsummary tables, event-study figures
#   <outputs>/.../manifest_M2.csv, fits_M2_<event>.rds
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
#  4. Second standard error clustered on survey PSU (ESTRATO x UPM), reported
#     in brackets below the individual-clustered SE. Clustering is an inference
#     choice, not a specification, so it does not get its own column.
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

# Second SE clustered on survey PSU. ESTRATO x UPM because UPM is not
# guaranteed unique across strata. CHECK this against your sample design
# before relying on it.
M2_PSU_SE <- TRUE

# Window x balance combinations ruled out by the survey design rather than by
# the data. The ENCFT rotation is 5 quarters, so max_window (6 non-contiguous
# quarters once the event quarter is excluded) can never yield a balanced
# panel. Skipped up front so a genuinely missing file is not mistaken for this.
M2_SKIP_BALANCED <- c("max_window")

# Build sample tag (must match scripts 10/11)
sample_tag <- M2_CONTROL_BW
if (!is.null(M2_TREAT_MIN_FS) && M2_TREAT_MIN_FS > 1) {
  sample_tag <- paste0(sample_tag, "_micro", M2_TREAT_MIN_FS, "plus")
}

BALANCE_MODES <- config$method2$active_balance

m2_data_root <- file.path(
  config$paths$processed_data, "Panel Regressions",
  M2_EVENT$event_tag, sample_tag
)
m2_out_root <- file.path(
  config$paths$outputs, config$output_stage, "Panel Regressions",
  M2_EVENT$event_tag, sample_tag
)

TREAT_LABEL   <- config$m2_labels$treatment
CONTROL_LABEL <- config$m2_labels$control
CTRL_LABEL    <- CONTROL_LABEL

cat(sprintf("  Sample: %s\n", sample_tag))
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
  
  "has_wage",                     "Wage observed",               "D. Selection into panel E",                       "EXT",  TRUE,  3,
  
  "log_real_hwage",               "Log hourly wage",             "E. Wages and hours (conditional on wage)",        "INT",  TRUE,  3,
  "log_real_mwage",               "Log monthly wage",            "E. Wages and hours (conditional on wage)",        "INT",  FALSE, 3,
  "hours_worked_primary",         "Weekly hours",                "E. Wages and hours (conditional on wage)",        "INT",  TRUE,  2,
  "below_min_hourly_base_salary", "Below hourly MW floor",       "E. Wages and hours (conditional on wage)",        "INT",  TRUE,  3,
  
  "log_real_total_income",        "Log total income",            "F. Diagnostics",                                  "INT",  FALSE, 3,
  "is_same_tier",                 "Same firm-size tier",         "F. Diagnostics",                                  "EXT",  FALSE, 3
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
                  vcov = ~ID_PERSONA, warn = FALSE, notes = FALSE),
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
                  vcov = ~ID_PERSONA, warn = FALSE, notes = FALSE),
    error = function(e) { message("    ES error: ", e$message); NULL }
  )
}


# Pull everything the tables need out of one fit + its estimation frame.
summarise_did <- function(fit, d, yvar, weight_col = NULL) {
  
  if (is.null(fit)) return(NULL)
  
  ct <- fixest::coeftable(fit)
  rn <- grep("treat.*post|post.*treat", rownames(ct), value = TRUE)
  if (length(rn) == 0) return(NULL)
  rn <- rn[1]
  
  # Second SE clustered on survey PSU
  se_psu <- NA_real_
  if (isTRUE(M2_PSU_SE) && "psu_id" %in% names(d)) {
    se_psu <- tryCatch({
      ct2 <- fixest::coeftable(summary(fit, cluster = ~psu_id))
      ct2[rn, "Std. Error"]
    }, error = function(e) NA_real_)
  }
  
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
    se_psu         = unname(se_psu),
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
    
    # PSU identifier for the second SE. UPM may repeat across strata.
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
           arm_label = "Formal at baseline + controls")
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
                                      "formal_unw", "formal_ctrl")]
      headline_fits <- headline_fits[!vapply(headline_fits, is.null,
                                             logical(1))]
      
      if (length(headline_fits) >= 2) {
        
        tbl_notes <- list(
          sprintf("Treatment: %s | Control: %s", TREAT_LABEL, CONTROL_LABEL),
          "Individual + quarter FE. SEs clustered at individual level.",
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
              "Clustered at individual level.",
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
      paste0("    The sample tag is '%s', built from control_bandwidth = '%s'",
             " and treatment_min_firmsize = %s.\n",
             "    Re-run scripts 10 and 11 with the current config, or point",
             " the config back\n    at a sample tag that has already been",
             " built.\n"),
      sample_tag, M2_CONTROL_BW,
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
  # STEP 4. Headline tables — one per window x balance x control-variant
  #
  # Layout:
  #   rows    = outcomes, grouped into blocks, four lines each
  #             (coefficient / individual-clustered SE / PSU-clustered SE /
  #              observations per individuals)
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
    
    d <- d %>%
      mutate(
        stars = case_when(pvalue < 0.01 ~ "***", pvalue < 0.05 ~ "**",
                          pvalue < 0.10 ~ "*", TRUE ~ ""),
        est  = paste0(fmt_vec(beta, dec), stars),
        se_i = paste0("(", fmt_vec(se, dec), ")"),
        se_p = ifelse(is.na(se_psu), "", paste0("[", fmt_vec(se_psu, dec), "]")),
        nn   = paste0(fmt_int(n_obs), " / ", fmt_int(n_indiv))
      )
    
    col_ids <- arms$col_id
    
    body <- d %>%
      select(ord, block, label, col_id, est, se_i, se_p, nn) %>%
      tidyr::pivot_longer(c(est, se_i, se_p, nn),
                          names_to = "row_type", values_to = "val") %>%
      mutate(row_type = factor(row_type,
                               levels = c("est", "se_i", "se_p", "nn"))) %>%
      tidyr::pivot_wider(names_from = col_id, values_from = val) %>%
      arrange(ord, row_type)
    
    # Specs that produced nothing for an outcome
    for (cc in col_ids) if (!cc %in% names(body)) body[[cc]] <- NA_character_
    body <- body %>%
      mutate(across(all_of(col_ids),
                    ~ ifelse(is.na(.x) & row_type == "est", "\u2014",
                             ifelse(is.na(.x), "", .x))))
    
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
      mutate(across(c(mean_treat, mean_ctrl),
                    ~ ifelse(row_type == "est" & !is.na(.x), .x, ""))) %>%
      mutate(label = ifelse(row_type == "est", label, "")) %>%
      select(block, label, row_type, mean_treat, mean_ctrl, all_of(col_ids))
    
    lab_list <- as.list(c("Outcome", "Micro", "Small", arms$col_label))
    names(lab_list) <- c("label", "mean_treat", "mean_ctrl", col_ids)
    
    tbl <- body %>%
      gt::gt(groupname_col = "block") %>%
      gt::row_group_order(groups = unique(body$block))
    
    tbl <- do.call(gt::cols_label, c(list(tbl), lab_list))
    
    tbl <- tbl %>%
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
        locations = gt::cells_body(columns = "label", rows = row_type == "est")
      ) %>%
      gt::tab_style(
        style = gt::cell_text(size = gt::px(10), color = "#555555"),
        locations = gt::cells_body(rows = row_type == "nn")
      ) %>%
      gt::tab_source_note(sprintf(
        paste("Each row is a separate regression of the outcome on Micro x Post",
              "with individual and quarter fixed effects. Treatment: %s.",
              "Control: %s. Baseline sample: %s."),
        TREAT_LABEL, CONTROL_LABEL, BASELINE_DESC)) %>%
      gt::tab_source_note(paste(
        "Cells report the DiD coefficient, the standard error clustered on the",
        "individual in parentheses, the standard error clustered on the survey",
        "PSU (ESTRATO x UPM) in brackets, and observations / distinct",
        "individuals. The number of individuals is the cluster count on which",
        "inference rests.")) %>%
      gt::tab_source_note(paste(
        "Pre-period means are computed on the column (1) estimation sample and",
        "are informative for column (1) only: columns (2) and (3) restrict the",
        "sample or apply survey weights, so these means do not describe them.",
        "Specification-specific means are in manifest_M2.csv.")) %>%
      gt::tab_source_note(paste(
        "Panel E is conditional on an observed positive wage. The treatment",
        "effect on wage observation is reported in Panel D; where that effect is",
        "non-zero, Panel E estimates are subject to differential selection and",
        "should not be read as effects on wages.")) %>%
      gt::tab_source_note(
        "* p<0.10, ** p<0.05, *** p<0.01") %>%
      gt::tab_source_note(SRC) %>%
      gt::tab_options(
        table.font.size = gt::px(12),
        heading.title.font.size = gt::px(14),
        heading.subtitle.font.size = gt::px(11),
        column_labels.font.weight = "bold",
        row_group.font.weight = "bold",
        source_notes.font.size = gt::px(10)
      ) %>%
      gt::cols_hide(columns = "row_type")
    
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



p <- readRDS(file.path(m2_data_root, "balanced", "sym2_2", "individual_panel.rds"))
p %>% distinct(ID_PERSONA, treat, baseline_qtr) %>% count(treat, baseline_qtr)
p %>% group_by(baseline_qtr, year_quarter) %>% summarise(emp = mean(is_employed))

p %>% group_by(treat, baseline_qtr, year_quarter) %>%
  summarise(emp = mean(is_employed), n = n(), .groups = "drop")
