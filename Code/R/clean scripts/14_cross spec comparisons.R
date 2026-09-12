#===============================================================================
#
# Script: 14_cross_spec_comparisons.R
#
# Purpose: The two comparison dimensions 13_regression_comparisons.R
#          couldn't cover, because they require reading manifest_M2.csv
#          from MULTIPLE <treatment>/<control>/<baseline_rule> folders and
#          stacking them, rather than the single current-config manifest 13
#          reads:
#
#   Table F: ACROSS BASELINE RULE. Fixes treatment + control; columns =
#            baseline_rule x {Baseline+Ctrl, Formal+Ctrl}. Answers "does the
#            estimate itself move across first_qtr_only / any_pre_first /
#            all_pre_qtrs" -- the regression-estimate counterpart to 10B's
#            sample-size sensitivity table (which only measures how many
#            people survive under each rule, not what the effect is).
#
#   Table G: ACROSS SAMPLE DEFINITIONS. Fixes baseline_rule; columns =
#            treatment/control combination (i.e. each SPEC_GRID row in
#            run_panel_regs.R -- headline, wide_bw, micro3plus, micro_all)
#            x {Baseline+Ctrl, Formal+Ctrl}. Answers "does the headline
#            result survive the sample-definition robustness checks",
#            without opening each spec's folder separately.
#
# HOW IT FINDS MANIFESTS: this script does NOT hardcode which
# treatment/control/baseline_rule combinations exist -- it scans the
# filesystem under <event>/ for whichever combinations actually have a
# manifest_M2.csv (i.e. whatever run_panel_regs.R has actually built so
# far). This avoids a second copy of run_panel_regs.R's SPEC_GRID that
# would need to be kept in sync by hand; if you add a new spec there and
# run it, this script picks it up automatically next time it's sourced.
#
# Pipeline: 01A -> 01B -> 02 -> 10 -> 11 -> 12 -> 13 -> [14]
#           Run any time after at least two treatment/control/baseline_rule
#           combinations have been built by 12 (e.g. after a full
#           run_panel_regs.R sweep). Independent of 13 -- reads manifests
#           directly, not 13's comparison tables.
#
# Reads:  <outputs>/.../Panel Regressions/<event>/*/*/*/manifest_M2.csv
#           (every manifest found under the event, via filesystem scan)
#
# Writes: <outputs>/.../Panel Regressions/<event>/<treatment>/<control>/
#           Comparison Tables/tbl_across_baseline_rule_<window_tag>_<balance>.{html,png}
#             (Table F -- one per treatment/control combo that has >=2
#              baseline_rule manifests, and per window/balance)
#         <outputs>/.../Panel Regressions/<event>/
#           Comparison Tables/tbl_across_sample_defs_<baseline_rule>_<window_tag>_<balance>.{html,png}
#             (Table G -- one per baseline_rule that has >=2
#              treatment/control manifests, and per window/balance)
#
#===============================================================================

if (!exists("config")) {
  source(here::here("Code", "R", "clean scripts", "00_setup.R"))
}

cat("=== 14_cross_spec_comparisons.R ===\n\n")


#===============================================================================
# STEP 0. Parameters
#===============================================================================

M2_EVENT   <- config$method2$events$event_2021q3_micro
M2_WINDOWS <- config$method2$windows

event_root <- file.path(
  config$paths$outputs, config$output_stage, "Panel Regressions",
  M2_EVENT$event_tag
)

SRC <- "Source: ENCFT (Banco Central de la Rep\u00fablica Dominicana)."

WINDOW_LABELS <- setNames(vapply(M2_WINDOWS, function(w) w$label, character(1)),
                          names(M2_WINDOWS))
WINDOW_TAGS   <- setNames(vapply(M2_WINDOWS, function(w) w$tag, character(1)),
                          names(M2_WINDOWS))

fmt_cell <- function(beta, se, pvalue, dec = 3) {
  stars <- dplyr::case_when(pvalue < 0.01 ~ "***", pvalue < 0.05 ~ "**",
                            pvalue < 0.10 ~ "*", TRUE ~ "")
  # <br>, not \n -- see 13_regression_comparisons.R's fmt_cell for why a bare
  # newline silently fails to stack lines under fmt_markdown().
  sprintf("%s%s<br><span style='font-size:0.85em;color:#555'>(%s)</span>",
          formatC(beta, format = "f", digits = dec), stars,
          formatC(se, format = "f", digits = dec))
}

# Same helper as 13_regression_comparisons.R's add_n_row() -- duplicated
# here rather than sourced, since this script is meant to be runnable
# standalone (see header). One "N (observations / individuals)" row per
# margin block; relies on N being constant across outcomes within a margin
# for a given column, which the function checks and warns about rather than
# silently assuming.
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

FOCUS_ARMS <- c(ctrl_unw = "Baseline+Ctrl", formal_ctrl = "Formal+Ctrl")


#===============================================================================
# STEP 1. Discover every manifest_M2.csv under this event, tagging each row
#         with which treatment/control/baseline_rule folder it came from.
#===============================================================================

discover_manifests <- function(event_root) {
  
  if (!dir.exists(event_root)) {
    cat(sprintf("  Event root not found: %s\n", event_root))
    return(tibble::tibble())
  }
  
  treat_dirs <- list.dirs(event_root, recursive = FALSE, full.names = TRUE)
  
  purrr::map_dfr(treat_dirs, function(td) {
    treat_tag <- basename(td)
    ctrl_dirs <- list.dirs(td, recursive = FALSE, full.names = TRUE)
    
    purrr::map_dfr(ctrl_dirs, function(cd) {
      ctrl_tag <- basename(cd)
      rule_dirs <- list.dirs(cd, recursive = FALSE, full.names = TRUE)
      
      purrr::map_dfr(rule_dirs, function(rd) {
        rule_tag <- basename(rd)
        # Skip non-baseline_rule subfolders if any ever appear at this level
        if (rule_tag == "Comparison Tables") return(NULL)
        
        mf_path <- file.path(rd, "manifest_M2.csv")
        if (!file.exists(mf_path)) return(NULL)
        
        readr::read_csv(mf_path, show_col_types = FALSE) %>%
          dplyr::mutate(treatment_tag = treat_tag, control_tag = ctrl_tag,
                        spec_tag = paste(control_tag, treatment_tag, sep = "_"))
      })
    })
  })
}

cat(sprintf("  Scanning: %s\n", event_root))
all_manifests <- discover_manifests(event_root)

if (nrow(all_manifests) == 0) {
  cat("  No manifest_M2.csv files found under this event. Nothing to compare.\n")
  cat("  Run scripts 10-12 (or run_panel_regs.R) for at least one spec first.\n")
  cat("\n=== 14_cross_spec_comparisons.R complete ===\n")
} else {
  
  cat(sprintf("  Found %d manifest rows across:\n", nrow(all_manifests)))
  cat(sprintf("    treatment/control combos: %s\n",
              paste(unique(all_manifests$spec_tag), collapse = ", ")))
  cat(sprintf("    baseline rules: %s\n\n",
              paste(unique(all_manifests$baseline_rule), collapse = ", ")))
  
  
  #=============================================================================
  # Shared paired-table builder (same shape as 13's build_paired_table:
  # columns = group x {Baseline+Ctrl, Formal+Ctrl}, rows = outcomes)
  #=============================================================================
  
  build_paired_table <- function(mf, group_var, group_vals, group_labels,
                                 title, subtitle, arm_on_top = FALSE) {
    # arm_on_top = FALSE (default): columns = group x {Baseline+Ctrl,
    #   Formal+Ctrl}, spanners are the GROUP labels.
    # arm_on_top = TRUE: columns = arm x group, spanners are {Baseline+Ctrl,
    #   Formal+Ctrl} with each group nested underneath -- same shape as
    #   13_regression_comparisons.R's Table B/D. Used for Table F (across
    #   baseline rules), per request, matching Table D's layout.
    
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
    
    # NOTE: present_groups derived via direct membership check, not by
    # parsing col_id strings -- see 13_regression_comparisons.R's identical
    # comment for why a naive "up to the first __" regex is unsafe here
    # (arm tags themselves contain underscores).
    if (arm_on_top) {
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
  
  
  #=============================================================================
  # Table F: across baseline_rule, one treatment/control combo at a time.
  # Only built for combos with >= 2 baseline_rule manifests -- nothing to
  # compare with just one.
  #=============================================================================
  
  cat("[Table F] Across baseline_rule...\n")
  
  specs_with_multi_rule <- all_manifests %>%
    dplyr::distinct(treatment_tag, control_tag, baseline_rule) %>%
    dplyr::count(treatment_tag, control_tag) %>%
    dplyr::filter(n >= 2)
  
  if (nrow(specs_with_multi_rule) == 0) {
    cat("  Skipped: no treatment/control combo has more than one baseline_rule built yet.\n")
    cat("  (Build e.g. the 'bl_anyfirst' or 'bl_allpre' rows in run_panel_regs.R's SPEC_GRID.)\n\n")
  } else {
    
    for (i in seq_len(nrow(specs_with_multi_rule))) {
      tt <- specs_with_multi_rule$treatment_tag[i]
      ct <- specs_with_multi_rule$control_tag[i]
      
      mf_spec <- all_manifests %>%
        dplyr::filter(treatment_tag == tt, control_tag == ct)
      rules_present <- sort(unique(mf_spec$baseline_rule))
      
      cmp_dir_f <- file.path(event_root, tt, ct, "Comparison Tables")
      dir.create(cmp_dir_f, recursive = TRUE, showWarnings = FALSE)
      
      for (win_name in unique(mf_spec$window)) {
        for (bal in unique(mf_spec$balance[mf_spec$window == win_name])) {
          
          tbl <- build_paired_table(
            mf_spec %>% dplyr::filter(window == win_name, balance == bal),
            group_var = "baseline_rule", group_vals = rules_present,
            group_labels = setNames(rules_present, rules_present),
            title = "Table F. Baseline and formal-subset results across baseline rules",
            subtitle = sprintf("Treatment: %s | Control: %s | %s | %s panel",
                               tt, ct, WINDOW_LABELS[win_name] %||% win_name, bal),
            arm_on_top = TRUE)
          if (is.null(tbl)) next
          
          win_tag <- WINDOW_TAGS[win_name] %||% win_name
          stem <- file.path(cmp_dir_f, sprintf("tbl_across_baseline_rule_%s_%s", win_tag, bal))
          gt::gtsave(tbl, paste0(stem, ".html"))
          gt::gtsave(tbl, paste0(stem, ".png"), expand = 10)
          cat(sprintf("  Wrote %s/%s\n", basename(cmp_dir_f), basename(stem)))
        }
      }
    }
    cat("\n")
  }
  
  
  #=============================================================================
  # Table G: across sample definitions (treatment/control combos), one
  # baseline_rule at a time. Only built for rules with >= 2 treatment/control
  # combos present.
  #=============================================================================
  
  cat("[Table G] Across sample definitions...\n")
  
  rules_with_multi_spec <- all_manifests %>%
    dplyr::distinct(baseline_rule, spec_tag) %>%
    dplyr::count(baseline_rule) %>%
    dplyr::filter(n >= 2)
  
  if (nrow(rules_with_multi_spec) == 0) {
    cat("  Skipped: no baseline_rule has more than one treatment/control combo built yet.\n")
    cat("  (Build more than one row of run_panel_regs.R's SPEC_GRID under the same rule.)\n\n")
  } else {
    
    cmp_dir_g <- file.path(event_root, "Comparison Tables")
    dir.create(cmp_dir_g, recursive = TRUE, showWarnings = FALSE)
    
    for (rule in rules_with_multi_spec$baseline_rule) {
      
      mf_rule <- all_manifests %>% dplyr::filter(baseline_rule == rule)
      specs_present <- sort(unique(mf_rule$spec_tag))
      
      for (win_name in unique(mf_rule$window)) {
        for (bal in unique(mf_rule$balance[mf_rule$window == win_name])) {
          
          tbl <- build_paired_table(
            mf_rule %>% dplyr::filter(window == win_name, balance == bal),
            group_var = "spec_tag", group_vals = specs_present,
            group_labels = setNames(specs_present, specs_present),
            title = "Table G. Baseline and formal-subset results across sample definitions",
            subtitle = sprintf("baseline_rule: %s | %s | %s panel",
                               rule, WINDOW_LABELS[win_name] %||% win_name, bal))
          if (is.null(tbl)) next
          
          win_tag <- WINDOW_TAGS[win_name] %||% win_name
          stem <- file.path(cmp_dir_g, sprintf("tbl_across_sample_defs_%s_%s_%s", rule, win_tag, bal))
          gt::gtsave(tbl, paste0(stem, ".html"))
          gt::gtsave(tbl, paste0(stem, ".png"), expand = 10)
          cat(sprintf("  Wrote Comparison Tables/%s\n", basename(stem)))
        }
      }
    }
    cat("\n")
  }
  
  cat(sprintf("[Done] Cross-spec comparison tables written under: %s\n", event_root))
  cat("\n=== 14_cross_spec_comparisons.R complete ===\n")
}