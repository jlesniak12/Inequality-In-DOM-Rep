#===============================================================================
#
# Script: 13_regression_comparisons.R
#
# Purpose: Comparison tables built directly from manifest_M2.csv (12), for
#          the current sample (treatment x control x baseline_rule):
#
#   Table A: ACROSS WINDOWS, single arm. Fixes one arm and one balance mode;
#            rows = outcomes, columns = windows. Generalizes the
#            console-only summary that used to sit at the end of 12 (STEP 5).
#
#   Table B: ALL SPECIFICATIONS, within a window/balance. Rows = outcomes,
#            columns = the full baseline x formal x {unweighted, weighted} x
#            {no controls, controls} factorial (8 arms), plus "excl.
#            switchers" as a ninth column. Answers "how much does the
#            result move with subset/weighting/control choices".
#
#   Table D: BASELINE + FORMAL RESULTS ACROSS WINDOWS. Fixes balance mode;
#            columns = window x {Baseline+Ctrl, Formal+Ctrl}. Answers "is
#            the effect robust to the pre/post window choice", for the two
#            specs most likely to appear in the paper.
#
#   Table E: BASELINE + FORMAL RESULTS ACROSS BALANCE MODES. Fixes window;
#            columns = balance x {Baseline+Ctrl, Formal+Ctrl}. Answers "is
#            the effect robust to the balanced/unbalanced panel choice".
#
#   NOT covered here: comparison ACROSS baseline_rule (first_qtr_only vs
#   any_pre_first vs all_pre_qtrs) or ACROSS treatment/control definitions
#   (e.g. the different SPEC_GRID rows in run_panel_regs.R). Both would
#   require reading manifest_M2.csv from MULTIPLE <baseline_rule> or
#   <treatment>/<control> folders and stacking them, since this script only
#   ever reads one manifest (the current config's). Worth a follow-up
#   script (e.g. 14_cross_spec_comparisons.R) if useful -- not built here.
#
# Pipeline: 01A -> 01B -> 02 -> 10 -> 11 -> 12 -> [13]
#           Reads manifest_M2.csv, written by 12 for the CURRENT config's
#           treatment/control/baseline_rule combination. Must be run after
#           12 for that combination (or as part of run_panel_regs.R,
#           immediately after 12 in each spec's loop).
#
# Reads:  <outputs>/.../Panel Regressions/<event>/<treatment>/<control>/
#           <baseline_rule>/manifest_M2.csv
#
# Writes: <outputs>/.../Panel Regressions/<event>/<treatment>/<control>/
#           <baseline_rule>/Comparison Tables/
#             tbl_across_windows_<arm>_<balance>.{html,png}        Table A
#             tbl_across_arms_<window_tag>_<balance>.{html,png}    Table B
#             tbl_across_windows_paired_<balance>.{html,png}       Table D
#             tbl_across_balance_paired_<window_tag>.{html,png}    Table E
#
#===============================================================================

if (!exists("config")) {
  source(here::here("Code", "R", "clean scripts", "00_setup.R"))
}

cat("=== 13_regression_comparisons.R ===\n\n")

#===============================================================================
# STEP 0. Parameters (mirrors 12)
#===============================================================================

M2_EVENT   <- config$method2$events$event_2021q3_micro
M2_WINDOWS <- config$method2$windows

M2_CONTROL_BW   <- config$method2$control_bandwidth
M2_TREAT_MIN_FS <- config$method2$treatment_min_firmsize

# Must match script 10's rule (same fallback logic)
M2_BASELINE_RULE <- config$method2$baseline_rule %||% "first_qtr_only"

# Folder tags (must match scripts 10/10B/11/11B/12)
M2_TREATMENT_TAG <- if (!is.null(M2_TREAT_MIN_FS) && M2_TREAT_MIN_FS > 1) {
  sprintf("micro%dplus", M2_TREAT_MIN_FS)
} else {
  "micro_all"
}
M2_CONTROL_TAG  <- M2_CONTROL_BW
M2_BASELINE_TAG <- M2_BASELINE_RULE

TREAT_LABEL   <- config$m2_labels$treatment
CONTROL_LABEL <- config$m2_labels$control

m2_out_root <- file.path(
  config$paths$outputs, config$output_stage, "Panel Regressions",
  M2_EVENT$event_tag, M2_TREATMENT_TAG, M2_CONTROL_TAG, M2_BASELINE_TAG
)
cmp_dir <- file.path(m2_out_root, "Comparison Tables")
dir.create(cmp_dir, recursive = TRUE, showWarnings = FALSE)

SRC <- "Source: ENCFT (Banco Central de la Rep\u00fablica Dominicana)."

manifest_file <- file.path(m2_out_root, "manifest_M2.csv")
if (!file.exists(manifest_file)) {
  stop("manifest_M2.csv not found at ", manifest_file,
       " -- run script 12 for this treatment/control/baseline_rule first.")
}
manifest <- readr::read_csv(manifest_file, show_col_types = FALSE)

cat(sprintf("  Treatment: %s | Control: %s | Baseline rule: %s\n",
            M2_TREATMENT_TAG, M2_CONTROL_TAG, M2_BASELINE_TAG))
cat(sprintf("  Manifest: %d rows | windows: %s | balance: %s | arms: %s\n\n",
            nrow(manifest),
            paste(unique(manifest$window), collapse = ", "),
            paste(unique(manifest$balance), collapse = ", "),
            paste(unique(manifest$arm), collapse = ", ")))

fmt_cell <- function(beta, se, pvalue, dec = 3) {
  stars <- dplyr::case_when(pvalue < 0.01 ~ "***", pvalue < 0.05 ~ "**",
                            pvalue < 0.10 ~ "*", TRUE ~ "")
  # <br>, not \n: a bare newline is a markdown "soft break" and commonmark
  # renders it as a single space, not a line break, so fmt_markdown() would
  # silently collapse this back onto one line. <br> is raw HTML and passes
  # through commonmark untouched, which is what actually stacks the SE
  # beneath the estimate.
  sprintf("%s%s<br><span style='font-size:0.85em;color:#555'>(%s)</span>",
          formatC(beta, format = "f", digits = dec), stars,
          formatC(se, format = "f", digits = dec))
}

# One "N (observations / individuals)" row per margin block, rather than
# repeating N on every outcome row. This relies on N being constant across
# outcomes WITHIN a margin for a given column (all extensive-margin outcomes
# share one estimation sample; all intensive-margin outcomes share a
# different, smaller, wage-conditional sample) -- true by construction here,
# but the function warns rather than silently hides it if that assumption
# ever breaks for some outcome (e.g. an outcome-specific missingness pattern
# introduced later).
#
# col_var: the column in `d` (already long-format, pre-pivot) identifying
#   each output column -- "arm" for Table A/B, "col_id" for the paired
#   group x arm tables (D/E/F/G).
# Relies on gt preserving each group's row order as given in the input data
# (not re-sorting alphabetically): binding the N row ahead of the real
# outcome rows places it first within each margin block once gt groups by
# margin. Worth a visual check on first render rather than assumed blind.
add_n_row <- function(wide, d, col_var, col_cols) {
  
  n_long <- d %>%
    dplyr::distinct(margin, .data[[col_var]], n_obs, n_indiv) %>%
    dplyr::group_by(margin, .data[[col_var]]) %>%
    dplyr::summarise(
      n_cell   = sprintf("%s / %s", format(dplyr::first(n_obs), big.mark = ","),
                         format(dplyr::first(n_indiv), big.mark = ",")),
      n_varies = dplyr::n_distinct(n_obs) > 1,
      .groups = "drop")
  
  if (any(n_long$n_varies)) {
    message(paste(
      "  Note: N varies across outcomes within a margin for at least one",
      "column in this table -- showing the first outcome's N; check",
      "manually if that matters for your outcome set."))
  }
  
  n_wide <- n_long %>%
    dplyr::select(margin, dplyr::all_of(col_var), n_cell) %>%
    tidyr::pivot_wider(names_from = dplyr::all_of(col_var), values_from = n_cell)
  
  for (cc in col_cols) if (!cc %in% names(n_wide)) n_wide[[cc]] <- NA_character_
  
  n_wide <- n_wide %>%
    dplyr::mutate(outcome_label = "N (observations / individuals)") %>%
    dplyr::select(margin, outcome_label, dplyr::all_of(col_cols))
  
  dplyr::bind_rows(n_wide, wide)
}

#===============================================================================
# Table A: across windows
#===============================================================================

build_across_windows <- function(mf, arm_tag, bal, windows_present) {
  
  d <- mf %>%
    dplyr::filter(arm == arm_tag, balance == bal) %>%
    dplyr::mutate(cell = fmt_cell(beta, se, pvalue))
  
  if (nrow(d) == 0) return(NULL)
  
  wide <- d %>%
    dplyr::select(margin, outcome_label, window, cell) %>%
    tidyr::pivot_wider(names_from = window, values_from = cell) %>%
    dplyr::arrange(margin, outcome_label)
  
  win_cols <- intersect(windows_present, names(wide))
  wide <- wide %>% dplyr::select(margin, outcome_label, dplyr::all_of(win_cols))
  wide <- add_n_row(wide, d, col_var = "window", col_cols = win_cols)
  
  wide %>%
    gt::gt(groupname_col = "margin", rowname_col = "outcome_label") %>%
    gt::tab_header(
      title = "Table A. Coefficient comparison across windows",
      subtitle = sprintf("Arm: %s | %s panel | Treatment: %s | Control: %s",
                         arm_tag, bal, TREAT_LABEL, CONTROL_LABEL)) %>%
    gt::fmt_markdown(columns = dplyr::all_of(win_cols)) %>%
    gt::sub_missing(everything(), missing_text = "\u2014") %>%
    gt::tab_source_note(paste(
      "Each cell: DiD coefficient with stars, the standard error in",
      "parentheses (two-way clustered on individual and survey PSU where",
      "available, individual-only otherwise).",
      "* p<0.10, ** p<0.05, *** p<0.01. N is observations / distinct",
      "individuals, constant across outcomes within a margin block.")) %>%
    gt::tab_source_note(paste(
      "Same arm and balance mode held fixed across columns -- differences",
      "reflect only the pre/post window choice. Windows are NOT nested",
      "samples of the same people (see project notes on rotation cohorts);",
      "differences across columns can reflect a different underlying",
      "entry cohort, not just a longer/shorter post period.")) %>%
    gt::tab_source_note(SRC)
}

# One table per balance mode, for the headline unweighted arm. Add more
# arm_tag values here (e.g. "base_wt") if you want additional across-window
# comparisons saved by default.
for (bal in unique(manifest$balance)) {
  for (arm_tag in c("base_unw")) {
    
    windows_present <- names(M2_WINDOWS)[names(M2_WINDOWS) %in% unique(manifest$window)]
    tbl <- build_across_windows(manifest, arm_tag, bal, windows_present)
    if (is.null(tbl)) next
    
    stem <- file.path(cmp_dir, sprintf("tbl_across_windows_%s_%s", arm_tag, bal))
    gt::gtsave(tbl, paste0(stem, ".html"))
    gt::gtsave(tbl, paste0(stem, ".png"), expand = 10)
    cat(sprintf("  Wrote %s\n", basename(stem)))
  }
}


#===============================================================================
# Table B: all specifications, within a window/balance
#
# Full baseline x formal x {unweighted, weighted} x {no controls, controls}
# factorial (8 arms) plus "excl. switchers" as a ninth, unspannered column
# (descriptive only -- see its own footnote). This is what request #1 in the
# comparison-tables discussion asked for: "baseline unweighted / weighted /
# formal unweighted / weighted, each with and without controls."
#===============================================================================

ARM_ORDER <- c("base_unw", "ctrl_unw", "base_wt", "ctrl_wt",
               "formal_unw", "formal_ctrl", "formal_wt", "formal_wt_ctrl",
               "nosw_unw")
ARM_LABELS <- c(base_unw = "Unweighted", ctrl_unw = "+ controls",
                base_wt = "Weighted", ctrl_wt = "Weighted + controls",
                formal_unw = "Unweighted", formal_ctrl = "+ controls",
                formal_wt = "Weighted", formal_wt_ctrl = "Weighted + controls",
                nosw_unw = "Excl. switchers")
# Which spanner (if any) each arm sits under. Arms not listed get no spanner.
ARM_SPANNER <- c(base_unw = "Baseline", ctrl_unw = "Baseline",
                 base_wt = "Baseline", ctrl_wt = "Baseline",
                 formal_unw = "Formal at baseline", formal_ctrl = "Formal at baseline",
                 formal_wt = "Formal at baseline", formal_wt_ctrl = "Formal at baseline")

build_across_arms <- function(mf, win_name, bal) {
  
  d <- mf %>%
    dplyr::filter(window == win_name, balance == bal,
                  arm %in% ARM_ORDER) %>%
    dplyr::mutate(cell = fmt_cell(beta, se, pvalue))
  
  if (nrow(d) == 0) return(NULL)
  
  # NOTE: select only margin/outcome_label/arm/cell here. Including a
  # per-arm column like arm_label before pivot_wider would join it into the
  # implicit id_cols (everything not in names_from/values_from), which
  # varies by arm -- that was the bug that split every outcome across one
  # row per arm instead of collapsing to one row with 8-9 arm columns.
  wide <- d %>%
    dplyr::select(margin, outcome_label, arm, cell) %>%
    tidyr::pivot_wider(names_from = arm, values_from = cell) %>%
    dplyr::arrange(margin, outcome_label)
  
  arm_cols <- intersect(ARM_ORDER, names(wide))
  wide <- wide %>% dplyr::select(margin, outcome_label, dplyr::all_of(arm_cols))
  wide <- add_n_row(wide, d, col_var = "arm", col_cols = arm_cols)
  
  tbl <- wide %>%
    gt::gt(groupname_col = "margin", rowname_col = "outcome_label") %>%
    gt::tab_header(
      title = "Table B. All specifications",
      subtitle = sprintf("%s | %s panel | Treatment: %s | Control: %s",
                         win_name, bal, TREAT_LABEL, CONTROL_LABEL)) %>%
    gt::fmt_markdown(columns = dplyr::all_of(arm_cols)) %>%
    gt::sub_missing(everything(), missing_text = "\u2014") %>%
    gt::tab_source_note(paste(
      "Each cell: DiD coefficient with stars, the standard error in",
      "parentheses (two-way clustered on individual and survey PSU where",
      "available, individual-only otherwise).",
      "* p<0.10, ** p<0.05, *** p<0.01. N is observations / distinct",
      "individuals, constant across outcomes within a margin block.")) %>%
    gt::tab_source_note(paste(
      "Same window and balance mode held fixed across columns --",
      "differences reflect only subset/weighting/control choices.",
      "'Excl. switchers' conditions on a post-treatment outcome (never",
      "changing firm-size tier) and should be read as descriptive, not a",
      "preferred spec.")) %>%
    gt::tab_source_note(SRC)
  
  tbl <- do.call(gt::cols_label, c(list(tbl),
                                   setNames(as.list(ARM_LABELS[arm_cols]), arm_cols)))
  
  for (spn in unique(ARM_SPANNER)) {
    spn_cols <- intersect(arm_cols, names(ARM_SPANNER)[ARM_SPANNER == spn])
    if (length(spn_cols) > 0) {
      tbl <- tbl %>% gt::tab_spanner(label = spn, columns = dplyr::all_of(spn_cols))
    }
  }
  tbl
}

for (win_name in names(M2_WINDOWS)) {
  win <- M2_WINDOWS[[win_name]]
  for (bal in unique(manifest$balance)) {
    
    tbl <- build_across_arms(manifest, win_name, bal)
    if (is.null(tbl)) next
    
    stem <- file.path(cmp_dir, sprintf("tbl_across_arms_%s_%s", win$tag, bal))
    gt::gtsave(tbl, paste0(stem, ".html"))
    gt::gtsave(tbl, paste0(stem, ".png"), expand = 10)
    cat(sprintf("  Wrote %s\n", basename(stem)))
  }
}


#===============================================================================
# Shared helper for Tables D and E: a "paired arm" comparison, where the
# outer grouping (window, or balance) varies across column spanners and a
# FIXED pair of arms (baseline+controls, formal+controls) sits under each
# spanner. Both requests #2 and #3 from the comparison-tables discussion are
# this same shape, just with a different outer grouping variable.
#===============================================================================

FOCUS_ARMS <- c(ctrl_unw = "Baseline+Ctrl", formal_ctrl = "Formal+Ctrl")

build_paired_table <- function(mf, group_var, group_vals, group_labels,
                               title, subtitle, arm_on_top = FALSE) {
  # mf should already be filtered to the fixed dimension (e.g. one balance
  # mode, for Table D; one window, for Table E) by the caller -- simpler and
  # less error-prone than passing a filter condition through as a quosure.
  #
  # arm_on_top = FALSE (default): columns = group x {Baseline+Ctrl,
  #   Formal+Ctrl}, spanners are the GROUP labels (Table E's layout --
  #   unchanged from before).
  # arm_on_top = TRUE: columns = arm x group, spanners are {Baseline+Ctrl,
  #   Formal+Ctrl} with each group (e.g. window) nested underneath -- same
  #   shape as Table B's {Baseline, Formal} spanners on top. Used for
  #   Table D (across windows) per request.
  
  d <- mf %>%
    dplyr::filter(arm %in% names(FOCUS_ARMS),
                  .data[[group_var]] %in% group_vals) %>%
    dplyr::mutate(cell = fmt_cell(beta, se, pvalue),
                  col_id = if (arm_on_top) paste(arm, .data[[group_var]], sep = "__")
                  else paste(.data[[group_var]], arm, sep = "__"))
  
  if (nrow(d) == 0) return(NULL)
  
  wide <- d %>%
    dplyr::select(margin, outcome_label, col_id, cell) %>%
    tidyr::pivot_wider(names_from = col_id, values_from = cell) %>%
    dplyr::arrange(margin, outcome_label)
  
  # NOTE: build column order explicitly (not via outer()/as.vector(), which
  # flattens column-major and would put all of the OUTER dimension's first
  # level before any of its second level -- the opposite of the intended
  # "each outer-level's inner values sit together" layout).
  col_order <- if (arm_on_top) {
    unlist(lapply(names(FOCUS_ARMS), function(a) paste(a, group_vals, sep = "__")))
  } else {
    unlist(lapply(group_vals, function(g) paste(g, names(FOCUS_ARMS), sep = "__")))
  }
  col_cols  <- intersect(col_order, names(wide))
  if (length(col_cols) == 0) return(NULL)
  wide <- wide %>% dplyr::select(margin, outcome_label, dplyr::all_of(col_cols))
  wide <- add_n_row(wide, d, col_var = "col_id", col_cols = col_cols)
  
  tbl <- wide %>%
    gt::gt(groupname_col = "margin", rowname_col = "outcome_label") %>%
    gt::tab_header(title = title, subtitle = subtitle) %>%
    gt::fmt_markdown(columns = dplyr::all_of(col_cols)) %>%
    gt::sub_missing(everything(), missing_text = "\u2014") %>%
    gt::tab_source_note(paste(
      "Each cell: DiD coefficient with stars, the standard error in",
      "parentheses (two-way clustered on individual and survey PSU where",
      "available, individual-only otherwise).",
      "* p<0.10, ** p<0.05, *** p<0.01. 'Baseline+Ctrl' = ctrl_unw (full",
      "sample, unweighted, with time-varying controls). 'Formal+Ctrl' =",
      "formal_ctrl (formal-at-baseline subset, unweighted, with controls).",
      "N is observations / distinct individuals, constant across outcomes",
      "within a margin block.")) %>%
    gt::tab_source_note(SRC)
  
  if (arm_on_top) {
    # Column headers show the group label (e.g. window label); the arm
    # distinction is carried by the spanner instead.
    # NOTE: derive present_groups by checking membership directly rather
    # than parsing col_id strings -- a naive strip-the-prefix regex breaks
    # here because arm tags themselves contain underscores (ctrl_unw,
    # formal_ctrl), so a generic "up to the first __" pattern doesn't
    # isolate the arm cleanly.
    present_groups <- group_vals[vapply(group_vals, function(g) {
      any(paste(names(FOCUS_ARMS), g, sep = "__") %in% col_cols)
    }, logical(1))]
    col_labels_here <- setNames(rep(unname(group_labels[present_groups]), length(FOCUS_ARMS)),
                                col_cols)
    tbl <- do.call(gt::cols_label, c(list(tbl), as.list(col_labels_here)))
    
    for (a in names(FOCUS_ARMS)) {
      a_cols <- intersect(paste(a, group_vals, sep = "__"), col_cols)
      if (length(a_cols) > 0) {
        tbl <- tbl %>% gt::tab_spanner(label = FOCUS_ARMS[[a]], columns = dplyr::all_of(a_cols))
      }
    }
  } else {
    present_groups <- group_vals[vapply(group_vals, function(g) {
      any(paste(g, names(FOCUS_ARMS), sep = "__") %in% col_cols)
    }, logical(1))]
    arm_labels_here <- setNames(rep(unname(FOCUS_ARMS), length(present_groups)), col_cols)
    tbl <- do.call(gt::cols_label, c(list(tbl), as.list(arm_labels_here)))
    
    for (g in present_groups) {
      g_cols <- intersect(paste(g, names(FOCUS_ARMS), sep = "__"), col_cols)
      if (length(g_cols) > 0) {
        tbl <- tbl %>% gt::tab_spanner(label = group_labels[[g]], columns = dplyr::all_of(g_cols))
      }
    }
  }
  tbl
}


#===============================================================================
# Table D: across windows (baseline+ctrl and formal+ctrl, per window)
#
# Request #2: fixes baseline_rule (already fixed -- one manifest per run)
# and balance mode; columns = window x {Baseline+Ctrl, Formal+Ctrl}.
# arm_on_top = TRUE: {Baseline+Ctrl, Formal+Ctrl} spanners on top (same
# layout as Table B), windows nested underneath -- per request, since this
# reads more naturally as "here's the baseline spec across windows, here's
# the formal spec across windows" than the reverse.
#===============================================================================

windows_present <- names(M2_WINDOWS)[names(M2_WINDOWS) %in% unique(manifest$window)]
window_labels <- setNames(vapply(windows_present, function(w) M2_WINDOWS[[w]]$label,
                                 character(1)), windows_present)

for (bal in unique(manifest$balance)) {
  
  tbl <- build_paired_table(
    manifest %>% dplyr::filter(balance == bal),
    group_var = "window", group_vals = windows_present,
    group_labels = window_labels,
    title = "Table D. Baseline and formal-subset results across windows",
    subtitle = sprintf("%s panel | baseline_rule: %s | Treatment: %s | Control: %s",
                       bal, M2_BASELINE_TAG, TREAT_LABEL, CONTROL_LABEL),
    arm_on_top = TRUE)
  if (is.null(tbl)) next
  
  stem <- file.path(cmp_dir, sprintf("tbl_across_windows_paired_%s", bal))
  gt::gtsave(tbl, paste0(stem, ".html"))
  gt::gtsave(tbl, paste0(stem, ".png"), expand = 10)
  cat(sprintf("  Wrote %s\n", basename(stem)))
}


#===============================================================================
# Table E: across balance modes (baseline+ctrl and formal+ctrl, per balance)
#
# Request #3: fixes baseline_rule and window; columns = balance x
# {Baseline+Ctrl, Formal+Ctrl}.
#===============================================================================

balance_present <- unique(manifest$balance)
balance_labels <- setNames(balance_present, balance_present)

for (win_name in names(M2_WINDOWS)) {
  win <- M2_WINDOWS[[win_name]]
  
  tbl <- build_paired_table(
    manifest %>% dplyr::filter(window == win_name),
    group_var = "balance", group_vals = balance_present,
    group_labels = balance_labels,
    title = "Table E. Baseline and formal-subset results across balance modes",
    subtitle = sprintf("%s | baseline_rule: %s | Treatment: %s | Control: %s",
                       win$label, M2_BASELINE_TAG, TREAT_LABEL, CONTROL_LABEL))
  if (is.null(tbl)) next
  
  stem <- file.path(cmp_dir, sprintf("tbl_across_balance_paired_%s", win$tag))
  gt::gtsave(tbl, paste0(stem, ".html"))
  gt::gtsave(tbl, paste0(stem, ".png"), expand = 10)
  cat(sprintf("  Wrote %s\n", basename(stem)))
}

cat(sprintf("\n[Done] Comparison tables written to: %s\n", cmp_dir))
cat("\n=== 13_regression_comparisons.R complete ===\n")