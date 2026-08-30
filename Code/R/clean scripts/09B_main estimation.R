#===============================================================================
# Script: 09B_main_estimation.R  [REVISED]
#
# Purpose: Run every DiD specification for the CURRENT (active_income x
#          active_baseline) combination. Writes tables, event-study figures,
#          fitted-model rds files, and appends manifest rows to a master CSV.
#
# Parameterized by config$active_baseline and config$active_income (same axes
# as 07A / 08). The runner (run_all_estimations.R) loops over the grid,
# overwriting these two config fields before each source().
#
# Output tree (built under out_dir):
#   <income>/<baseline>/<design>/
#     tbl_A_group_<geo>_<weight>_<ctrl>.{html,tex}
#     tbl_B_cont_<geo>_<weight>_<ctrl>.{html,tex}
#     fits/A_group_<geo>_<weight>_<ctrl>.rds
#     fits/B_cont_<geo>_<weight>_<ctrl>.rds
#   <income>/<baseline>/event_study/
#     fig_es_<outcome>_<geo>.png
#     fits/es_<outcome>_<geo>.rds
#
# Manifest: manifest.csv at out_dir root. Append mode; run_all_estimations.R
# truncates it at driver start so a full grid run produces a fresh master.
#
# Designs (baseline-specific):
#   base2016_all_tiers  -> pooled | windows | event_study
#   base2021q2_micro    -> pooled | pooled_long_pre | event_study
#
# Reads:  panel_geo_quarter__<income>__<baseline>__<geo>.rds  (08, via mw_file)
# Writes: to <out_dir>/<income>/<baseline>/...
#===============================================================================

if (!exists("config", envir = .GlobalEnv, inherits = FALSE)) {
  source(here::here("Code","R","clean scripts","00_setup.R"))
} else {
  cat("[09] Reusing existing `config` (00_setup not re-sourced)\n")
}

# 09A is cheap; sourcing it fresh each iteration is fine (function definitions).
source(file.path(config$paths$scripts, "09A_estimation helpers.R"))

cat("=== 09B_main_estimation.R ===\n")


#===============================================================================
# STEP 0. Parameters, paths, arms, outcomes
#===============================================================================

BL <- config$baselines[[config$active_baseline]]
IS <- config$income_specs[[config$active_income]]

in_dir  <- config$data_dirs$regression
out_dir <- config$out_dirs$reg_results

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

GEO           <- config$exposure$construct_geo
COVID_QTRS    <- config$events$covid_qtrs
INFERENCE_GEO <- config$regression$inference_geo   # only used in table notes

BOOT_B    <- 9999
BOOT_SEED <- 42

cat(sprintf("  income=%s | baseline=%s | geo=%s\n",
            config$active_income, config$active_baseline, GEO))
cat(sprintf("  in_dir  = %s\n", in_dir))
cat(sprintf("  out_dir = %s\n", out_dir))


# --- Outcomes for THIS income concept -----------------------------------------
# Variance column names differ by income (hourly: log_var_hwage_*, monthly:
# log_var_mwage_*). Share outcomes are income-agnostic.

var_cols <- paste0(IS$log_var_prefix, c("_total", "_formal", "_informal"))
OUTCOMES <- c(setNames(var_cols, var_cols),
              log_informal_share = "log_informal_share",
              log_selfemp_share  = "log_selfemp_share")

OUTCOME_LABELS <- c(setNames(c("log(VAll)", "log(VF)", "log(VI)"), var_cols),
                    log_informal_share = "log(Inf. Share)",
                    log_selfemp_share  = "log(Self-emp.)")


# --- Control sets ------------------------------------------------------------
# Variance outcomes get baseline median wage x linear time trend as a
# heterogeneous-trends control (07C flagged r = -0.53 with exposure).

COMP_CONTROLS <- c("share_female", "share_sec_complete",
                   "share_tert_complete", "mean_age")

CONTROLS_BY_OUTCOME <- setNames(
  c(rep(list(c(COMP_CONTROLS, "baseline_median_lhw_formal:qidx")), 3),
    list(COMP_CONTROLS),
    list(COMP_CONTROLS)),
  names(OUTCOMES)
)


# --- Arms (weight x ctrl combinations) ---------------------------------------
# Encoded in the FILENAME, not the folder tree. Order matters for the file
# naming convention.

ARMS <- tibble::tribble(
  ~weight,           ~weight_tag,   ~use_controls, ~ctrl_tag,
  NA_character_,     "unw",         TRUE,          "ctrl",
  NA_character_,     "unw",         FALSE,         "noctrl",
  "baseline_emp",    "bemp",        TRUE,          "ctrl",
  "baseline_emp",    "bemp",        FALSE,         "noctrl"
)

ctrl_for <- function(nm, use_controls) {
  if (isTRUE(use_controls)) CONTROLS_BY_OUTCOME[[nm]] else NULL
}

CTRL_NOTE_ON <- paste(
  "Controls: share female, share secondary complete, share tertiary complete,",
  "mean age. Variance outcomes additionally include baseline formal median log",
  "wage x linear time trend.")
CTRL_NOTE_OFF <- "No compositional controls."


#===============================================================================
# STEP 1. Load panel and add estimation-side columns
#===============================================================================

panel_file <- mw_file("panel_geo_quarter", dir = in_dir)
if (!file.exists(panel_file)) {
  stop("Panel file not found: ", panel_file,
       "\nRun 08 for this (baseline, income) first.")
}
panel_gq <- readRDS(panel_file)

if (!"baseline_emp" %in% names(panel_gq)) {
  stop("baseline_emp not on panel - re-run 08 with the current config.")
}

reg <- panel_gq %>%
  filter(!is_treatment_qtr, !time %in% COVID_QTRS) %>%
  mutate(region_int = as.integer(factor(.data[[GEO]])),
         year       = as.integer(substr(time, 1, 4))) %>%
  add_ti()


cat(sprintf("[09] panel: %d obs | %d unique %s\n",
            nrow(reg), dplyr::n_distinct(reg[[GEO]]), GEO))


#===============================================================================
# STEP 2. Build specifications for the ACTIVE baseline
#
# Each SPEC is baseline-specific but takes the same shape:
#   design         : subfolder under <income>/<baseline>/
#   panel_tag      : A_group or B_cont (goes in filename)
#   data           : zero-arg fn returning the analysis frame for this spec
#   rhs            : treatment RHS string for run_did()
#   coef_map       : reported coefficients (fixest name -> pretty label)
#   title, notes   : table metadata
#===============================================================================

grp_levels <- other_levels(reg, "exposure_group", "Low exposure")

SPECS <- list()

# --- base2016 -----------------------------------------------------------------
# Sample restricted to 2016Q1+ so add_event_windows()'s open-ended "pre"
# doesn't reach into the 2015Q2 event.

if (config$active_baseline == "base2016_all_tiers") {
  
  reg_full <- reg %>% restrict_sample(from = 2016.00)
  reg_win  <- add_event_windows(reg_full)
  
  # --- pooled (single Post covering all events) ---
  SPECS$pooled_A_group <- list(
    design    = "pooled",
    panel_tag = "A_group",
    data      = function() reg_full,
    rhs       = "i(exposure_group, post_any, ref = 'Low exposure')",
    coef_map  = setNames(
      paste0("beta_", seq_along(grp_levels), " (", grp_levels, " x Post)"),
      nm_grp_post("exposure_group", grp_levels, "post_any")),
    title = "Pooled Post, exposure groups",
    notes = list(
      "Reference: Low exposure group.",
      paste0(GEO, " + quarter FE. COVID and MW event quarters excluded."),
      "Post = 1 from 2017Q3 onward (all events pooled).")
  )
  
  SPECS$pooled_B_cont <- list(
    design    = "pooled",
    panel_tag = "B_cont",
    data      = function() reg_full,
    rhs       = "exposure_geo_val:post_any",
    coef_map  = setNames("Exposure x Post",
                         nm_cont_post("exposure_geo_val", "post_any")),
    title = "Pooled Post, continuous exposure",
    notes = list(paste0(GEO, " + quarter FE. COVID and MW event quarters excluded."),
                 "Post = 1 from 2017Q3 onward (all events pooled).")
  )
  
  # --- windows (per-event windows in one regression) ---
  win_levels <- c("post_2017", "post_2021", "post_2023")
  win_note <- paste("Windows: pre (<2017Q2), post_2017 (2017Q3-2019Q2),",
                    "post_2021 (2021Q4-2023Q1), post_2023 (2023Q3-2025Q1).")
  
  SPECS$windows_A_group <- list(
    design    = "windows",
    panel_tag = "A_group",
    data      = function() reg_win,
    rhs       = "i(window, i.exposure_group, ref = 'pre', ref2 = 'Low exposure')",
    coef_map  = setNames(
      as.vector(outer(win_levels, grp_levels,
                      function(w, g) paste0(g, " x ", w))),
      as.vector(outer(win_levels, grp_levels,
                      function(w, g) nm_win_grp("window", w, "exposure_group", g)))),
    title = "Per-event windows, exposure groups",
    notes = list("Reference: Low exposure x pre.",
                 paste0(GEO, " + quarter FE."), win_note)
  )
  
  SPECS$windows_B_cont <- list(
    design    = "windows",
    panel_tag = "B_cont",
    data      = function() reg_win,
    rhs       = "i(window, exposure_geo_val, ref = 'pre')",
    coef_map  = setNames(paste0("Exposure x ", win_levels),
                         nm_win_cont("window", win_levels, "exposure_geo_val")),
    title = "Per-event windows, continuous exposure",
    notes = list("Reference: pre.", paste0(GEO, " + quarter FE."), win_note)
  )
  
  ES_REF_YEAR    <- 2016
  ES_SAMPLE_FN   <- function() reg_full
  ES_EVENT_YEARS <- qtr_to_ti(config$events$event_qtrs)
  ES_NOTE_EXTRA  <- "Red dotted lines = MW event quarters."
  
  # --- base2021q2 --------------------------------------------------------------
  # Both windows END at 2023Q1 (before the 2023Q2 event); running Post through
  # 2025 would put the 2023Q2 and 2025Q2 events inside Post, no longer
  # identifying the 2021Q3 micro floor.
  
} else if (config$active_baseline == "base2021q2_micro") {
  
  reg_tight <- reg %>%
    restrict_sample(from = 2021.00, to = 2023.00) %>%
    add_post(2021.50, "post")
  
  reg_long  <- reg %>%
    restrict_sample(from = 2016.00, to = 2023.00) %>%
    add_post(2021.50, "post")
  
  win_note_tight <- paste("Sample 2021Q1-2023Q1. Post = 1 from 2021Q3.",
                          "Base quarter 2021Q2. Window ends before the 2023Q2 event.")
  win_note_long  <- paste("Sample 2016Q1-2023Q1. Post = 1 from 2021Q3.",
                          "Longer pre period (contains the 2017Q2 and 2019Q3 events -",
                          "less clean than pooled_tight; robustness).")
  
  # --- pooled (tight window, headline for this baseline) ---
  SPECS$pooled_A_group <- list(
    design    = "pooled",
    panel_tag = "A_group",
    data      = function() reg_tight,
    rhs       = "i(exposure_group, post, ref = 'Low exposure')",
    coef_map  = setNames(
      paste0("beta_", seq_along(grp_levels), " (", grp_levels, " x Post)"),
      nm_grp_post("exposure_group", grp_levels, "post")),
    title = "Micro floor 2021Q3, exposure groups (tight window)",
    notes = list("Reference: Low exposure group.",
                 paste0(GEO, " + quarter FE."), win_note_tight)
  )
  
  SPECS$pooled_B_cont <- list(
    design    = "pooled",
    panel_tag = "B_cont",
    data      = function() reg_tight,
    rhs       = "exposure_geo_val:post",
    coef_map  = setNames("Exposure x Post",
                         nm_cont_post("exposure_geo_val", "post")),
    title = "Micro floor 2021Q3, continuous exposure (tight window)",
    notes = list(paste0(GEO, " + quarter FE."), win_note_tight)
  )
  
  # --- pooled_long_pre (extended pre, robustness) ---
  SPECS$pooled_long_pre_A_group <- list(
    design    = "pooled_long_pre",
    panel_tag = "A_group",
    data      = function() reg_long,
    rhs       = "i(exposure_group, post, ref = 'Low exposure')",
    coef_map  = setNames(
      paste0("beta_", seq_along(grp_levels), " (", grp_levels, " x Post)"),
      nm_grp_post("exposure_group", grp_levels, "post")),
    title = "Micro floor 2021Q3, exposure groups (extended pre)",
    notes = list("Reference: Low exposure group.",
                 paste0(GEO, " + quarter FE."), win_note_long)
  )
  
  SPECS$pooled_long_pre_B_cont <- list(
    design    = "pooled_long_pre",
    panel_tag = "B_cont",
    data      = function() reg_long,
    rhs       = "exposure_geo_val:post",
    coef_map  = setNames("Exposure x Post",
                         nm_cont_post("exposure_geo_val", "post")),
    title = "Micro floor 2021Q3, continuous exposure (extended pre)",
    notes = list(paste0(GEO, " + quarter FE."), win_note_long)
  )
  
  ES_REF_YEAR    <- 2021
  ES_SAMPLE_FN   <- function() reg %>% restrict_sample(from = 2016.00, to = 2023.00)
  # All events in-sample (ES sample ends 2023Q1, so drops later events).
  ES_EVENT_YEARS <- qtr_to_ti(config$events$event_qtrs) |>
    (\(x) x[x >= 2016 & x <= 2023])()
  ES_NOTE_EXTRA  <- "Reference year 2021. Sample ends 2023Q1 (before the 2023Q2 event)."
  
} else {
  stop("Unknown active_baseline: ", config$active_baseline)
}

cat(sprintf("[09] %d table specs x %d arms = %d tables\n",
            length(SPECS), nrow(ARMS), length(SPECS) * nrow(ARMS)))


#===============================================================================
# STEP 3. Driver: one call per (spec x arm), collect manifest rows
#===============================================================================

run_one <- function(spec, arm) {
  
  wt   <- if (is.na(arm$weight)) NULL else arm$weight
  dat  <- spec$data()
  dir_ <- spec_path(out_dir, config$active_income, config$active_baseline,
                    GEO, spec$design)
  
  file_base <- glue::glue("{spec$panel_tag}_{GEO}_{arm$weight_tag}_{arm$ctrl_tag}")
  
  cat(sprintf("[09] %s / %s / %s\n",
              config$active_baseline, spec$design, file_base))
  
  fits <- purrr::imap(OUTCOMES, function(v, nm)
    run_did(v, data = dat, rhs = spec$rhs, weights = wt,
            controls = ctrl_for(nm, arm$use_controls),
            fe = "region_int + time"))
  
  pvals <- save_table_boot(
    fits, coef_map = spec$coef_map,
    title = glue::glue("{spec$title} ({arm$weight_tag}, {arm$ctrl_tag})"),
    notes = c(spec$notes,
              list(if (arm$use_controls) CTRL_NOTE_ON else CTRL_NOTE_OFF)),
    file_base = file_base, path = dir_,
    outcome_labels = OUTCOME_LABELS, geo = GEO,
    has_controls = arm$use_controls,
    B = BOOT_B, seed = BOOT_SEED)
  
  manifest_rows(
    fits, pvals %||% list(),
    spec_meta = list(
      income   = config$active_income,
      baseline = config$active_baseline,
      design   = spec$design,
      panel_tag = spec$panel_tag,
      geo      = GEO,
      weight   = arm$weight_tag,
      ctrl     = arm$ctrl_tag
    ),
    coef_map = spec$coef_map,
    out_dir  = dir_)
}

manifest <- purrr::map_dfr(SPECS, function(spec)
  purrr::map_dfr(seq_len(nrow(ARMS)), function(i)
    tryCatch(run_one(spec, ARMS[i, ]),
             error = function(e) {
               msg <- sprintf("[FAILED: %s / %s / arm %d (%s, %s)] %s",
                              spec$design, spec$panel_tag, i,
                              ARMS$weight_tag[i], ARMS$ctrl_tag[i],
                              e$message)
               cat("!!!", msg, "\n")                # print inline
               warning(msg, call. = FALSE)
               tibble(income = config$active_income,
                      baseline = config$active_baseline,
                      design = spec$design, panel_tag = spec$panel_tag,
                      geo = GEO, weight = ARMS$weight_tag[i],
                      ctrl = ARMS$ctrl_tag[i],
                      outcome = NA_character_, term = NA_character_,
                      estimate = NA_real_, p_boot = NA_real_,
                      nobs = NA_integer_, r2 = NA_real_,
                      formula = NA_character_, out_dir = NA_character_,
                      error = e$message)
             })))


#===============================================================================
# STEP 4. Event studies (unweighted, with controls)
#
# One figure per outcome; single arm (unweighted + controls) since these are
# diagnostic rather than exhaustive. Manifest picks up per-term ES estimates
# so cross-baseline comparison of pre-trends is a CSV filter away.
#===============================================================================

cat("\n[09] Event studies (unweighted, with controls)...\n")

es_dir <- spec_path(out_dir, config$active_income, config$active_baseline,
                    GEO, "event_study")

es_sample <- ES_SAMPLE_FN()

es_subtitle <- paste(
  sprintf("Coefficient on Exposure x Year, reference year %d.", ES_REF_YEAR),
  sprintf("%s + time FE. Controls included. COVID quarters excluded.", GEO),
  sprintf("Wild cluster bootstrap p-values (Webb, B=%d, cluster = %s).",
          BOOT_B, GEO),
  ES_NOTE_EXTRA,
  sep = "\n")

es_manifest <- purrr::imap_dfr(OUTCOMES, function(v, nm) {
  
  rhs <- glue::glue("i(year, exposure_geo_val, ref = {ES_REF_YEAR})")
  fit <- run_did(v, data = es_sample, rhs = rhs, weights = NULL,
                 controls = CONTROLS_BY_OUTCOME[[nm]],
                 fe = "region_int + time")
  
  if (is.null(fit)) {
    message("  skipping ES for ", nm)
    return(tibble())
  }
  
  es_terms <- grep("year::", names(coef(fit)), value = TRUE, fixed = TRUE)
  boot_tbl <- bootstrap_ci(fit, B = BOOT_B, seed = BOOT_SEED,
                           terms = es_terms)
  
  # Figure
  p <- plot_event_study(boot_tbl, ref_year = ES_REF_YEAR,
                        title    = paste("Event study:", OUTCOME_LABELS[nm]),
                        subtitle = es_subtitle,
                        y_label  = "Coefficient x Year",
                        event_years = ES_EVENT_YEARS)
  save_plot(p, glue::glue("fig_es_{nm}_{GEO}"), es_dir,
            w = config$fig_defaults$width  * 1.2,
            h = config$fig_defaults$height * 0.9)
  
  # Fit
  fp <- fits_path(es_dir)
  saveRDS(fit, file.path(fp, glue::glue("es_{nm}_{GEO}.rds")))
  
  # Manifest rows: every reported year-coefficient becomes a row
  tibble(
    income   = config$active_income,
    baseline = config$active_baseline,
    design   = "event_study",
    panel_tag = "es_cont",
    geo      = GEO,
    weight   = "unw",
    ctrl     = "ctrl",
    outcome  = nm,
    term     = boot_tbl$term,
    estimate = boot_tbl$estimate,
    p_boot   = boot_tbl$p.value,
    nobs     = nobs(fit),
    r2       = unname(fixest::r2(fit)["r2"]),
    formula  = paste(deparse(formula(fit)), collapse = " "),
    out_dir  = es_dir,
    error    = NA_character_
  )
})

manifest <- bind_rows(manifest, es_manifest)


#===============================================================================
# STEP 5. Append to master manifest.csv
#
# Append mode: run_all_estimations.R truncates the file at driver start, so a
# full-grid run produces one fresh master CSV. Standalone runs append to
# whatever's already there (user can delete manually for a clean start).
#===============================================================================

mf_path <- file.path(out_dir, "manifest.csv")
append_mode <- file.exists(mf_path)
readr::write_csv(manifest, mf_path, append = append_mode)

cat(sprintf("\n[09] Wrote %d manifest rows (%s: %s)\n",
            nrow(manifest),
            if (append_mode) "appended" else "created",
            mf_path))

cat(sprintf("[09] Done. income=%s baseline=%s\n",
            config$active_income, config$active_baseline))




