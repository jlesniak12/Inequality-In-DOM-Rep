#===============================================================================
#
# Script: 11B_baseline_cohort_diagnostics.R
#
# Purpose: Diagnose the sample-construction problems that arise under the
#          any_pre_first baseline rule (each person's baseline assigned from
#          their own first eligible pre quarter, so the conditioning quarter
#          is person-specific). Script 10's default is now first_qtr_only
#          (config$method2$baseline_rule), which does not have this problem.
#
#          Checks remaining (after simplification -- see NOTE below):
#            B  Constraint audit: asserts that outcomes expected to be
#               mechanically pinned at baseline (by script 10's eligibility
#               filter) are in fact exactly 0/1. A console warning, not a
#               saved table -- this is a correctness check on script 10's
#               filters, not something that needs a human to read it every
#               run unless it fires.
#            D  The unconstrained pre observation: for people baselined
#               after the earliest pre quarter (only possible under
#               any_pre_first), shows their pre-baseline values. Self-skips
#               with a one-line console note under a single cohort.
#            E  Switcher audit: within-person 0/1 variation per outcome,
#               needed to judge whether individual fixed effects can
#               identify anything for that outcome. Not cohort-dependent;
#               always relevant.
#
# NOTE ON WHAT WAS REMOVED AND WHY:
#   Earlier versions of this script also had a cohort-composition table, a
#   cohort-trends figure, a cohort-specific-DiD table, and a "cost of a
#   fixed baseline quarter" table (A, the figure, C, and F). All four
#   compared MULTIPLE baseline cohorts against each other -- exactly the
#   problem this script exists to catch. Now that first_qtr_only is the
#   default, every person shares one baseline quarter, so all four
#   degenerated to a single trivial row/line with nothing to compare.
#   Their genuinely informative content is already covered elsewhere:
#     - per-cohort headcounts -> 10B_panel_attrition.R's Row 1 ("Assigned to
#       panel") already reports this, with more context (retention, balance).
#     - "cost of choosing a different baseline quarter" -> 10B's
#       tbl_10B_baseline_rule_sensitivity already compares first_qtr_only /
#       any_pre_first / all_pre_qtrs directly, which is a more complete
#       version of the same question than picking a single alternate
#       quarter within one rule.
#   If you deliberately re-run this script against an any_pre_first-built
#   panel (config$method2$baseline_rule <- "any_pre_first") and want the
#   cohort-comparison views back, they're straightforward to re-add -- ask
#   for them rather than reconstructing from memory, since the exact
#   pivot/plot code had a few fiddly bits worth getting right again.
#
# Pipeline: 01A -> 01B -> 02 -> 10 -> 11 -> [11B] -> 12
#
# Reads:  individual_panel.rds from
#         <processed>/Panel Regressions/<event>/<treatment>/<control>/
#         <baseline_rule>/<balance>/<win_tag>/
#
# Writes: <outputs>/.../<treatment>/<control>/<baseline_rule>/<balance>/
#         <win_tag>/Sample Evaluation/
#           tbl_11B_switcher_audit     E. outcomes with too little variation
#           tbl_11B_free_quarter       D. the unconstrained pre observation
#                                         (only produced if >1 cohort)
#         <outputs>/.../tbl_11B_window_summary   across all windows
#
#===============================================================================

if (!exists("config")) {
  source(here::here("Code", "R", "clean scripts", "00_setup.R"))
}

cat("=== 11B_baseline_cohort_diagnostics.R ===\n\n")


#===============================================================================
# STEP 0. Parameters — mirrors scripts 10/11/12
#===============================================================================

M2_EVENT   <- config$method2$events$event_2021q3_micro
M2_WINDOWS <- config$method2$windows

M2_CONTROL_BW   <- config$method2$control_bandwidth
M2_TREAT_MIN_FS <- config$method2$treatment_min_firmsize

# Must match script 10's rule (same fallback logic)
M2_BASELINE_RULE <- config$method2$baseline_rule %||% "first_qtr_only"

# Folder tags (must match scripts 10/10B/11/12/13)
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

GRP_COLS <- c("#4575b4", "#d73027")
names(GRP_COLS) <- c(CONTROL_LABEL, TREAT_LABEL)

SRC <- "Source: ENCFT (Banco Central de la Rep\u00fablica Dominicana)."

# Binary outcomes to audit. Those marked below are expected to be mechanically
# constrained in a person's own baseline quarter, because script 10 requires
# employment as a private employee with positive wage, positive hours and a
# known firm-size tier for baseline eligibility.
BIN_OUTCOMES <- c(
  "is_employed", "is_private_employee", "has_wage", "is_tier_observed",
  "is_selfemp_now", "is_owner_now", "is_independent_now",
  "is_informal_now", "is_formal_private", "is_same_tier"
)

# Outcomes expected to be mechanically pinned to a specific value at baseline
# by script 10's eligibility filter (Check B asserts against this list).
# is_informal_now / is_formal_private are deliberately NOT here: formality
# only has to be KNOWN at baseline, not equal to any particular value, so
# it's allowed to differ genuinely across groups.
EXPECTED_CONSTRAINED <- c(
  is_employed = 1, is_private_employee = 1, has_wage = 1,
  is_tier_observed = 1, is_selfemp_now = 0, is_owner_now = 0,
  is_independent_now = 0, is_same_tier = 1
)

# Below this many within-person switches, an outcome cannot be meaningfully
# estimated with individual fixed effects
MIN_SWITCHERS <- 10

cat(sprintf("  Event: %s | Treatment: %s | Control: %s | Baseline rule: %s\n",
            M2_EVENT$event_tag, M2_TREATMENT_TAG, M2_CONTROL_TAG, M2_BASELINE_TAG))
cat(sprintf("  Data from: %s\n\n", m2_data_root))

window_summary <- list()


#===============================================================================
# LOOP OVER WINDOWS x BALANCE MODES
#===============================================================================

for (win_name in names(M2_WINDOWS)) {
  
  win <- M2_WINDOWS[[win_name]]
  
  for (balance_label in BALANCE_MODES) {
    
    cat(sprintf("\n========== Window: %s | %s ==========\n\n",
                win$label, balance_label))
    
    win_data_dir <- file.path(m2_data_root, balance_label, win$tag)
    win_out_dir  <- file.path(m2_out_root, balance_label, win$tag,
                              "Sample Evaluation")
    dir.create(win_out_dir, recursive = TRUE, showWarnings = FALSE)
    
    save_tbl <- function(tb, name) {
      tryCatch({
        gt::gtsave(tb, file.path(win_out_dir, paste0(name, ".html")))
        gt::gtsave(tb, file.path(win_out_dir, paste0(name, ".png")),
                   expand = 10)
        message("  Saved: ", name)
      }, error = function(e) cat(sprintf("  %s failed: %s\n", name, e$message)))
    }
    
    panel_file <- file.path(win_data_dir, "individual_panel.rds")
    if (!file.exists(panel_file)) {
      cat(sprintf("  Panel not found: %s — skipping.\n\n", panel_file))
      next
    }
    
    panel <- readRDS(panel_file)
    if (nrow(panel) == 0) {
      cat("  Panel is empty — skipping.\n\n")
      next
    }
    
    # Firm size reported, conditional on employment. Distinct from has_wage:
    # this is the "don't know firm size" margin, which matters because tier
    # assigns treatment.
    if (all(c("OCUPADO", "has_tier") %in% names(panel))) {
      panel$is_tier_observed <- as.integer(
        panel$OCUPADO == 1 & !is.na(panel$has_tier) & panel$has_tier)
    }
    
    panel <- panel %>%
      mutate(
        cohort = as.character(baseline_qtr),
        group_label = factor(ifelse(treat == 1, TREAT_LABEL, CONTROL_LABEL),
                             levels = c(CONTROL_LABEL, TREAT_LABEL))
      )
    
    outs <- intersect(BIN_OUTCOMES, names(panel))
    
    sub_txt <- sprintf("%s | %s | Treatment: %s | Control: %s",
                       win$label, balance_label, TREAT_LABEL, CONTROL_LABEL)
    
    long <- panel %>%
      select(ID_PERSONA, treat, group_label, cohort, period, year_quarter,
             all_of(outs)) %>%
      tidyr::pivot_longer(all_of(outs), names_to = "outcome",
                          values_to = "y")
    
    n_cohorts <- n_distinct(panel$cohort)
    cat(sprintf("  Baseline cohorts: %d (rule = %s)%s\n",
                n_cohorts, M2_BASELINE_RULE,
                if (n_cohorts == 1) sprintf(" -- all at %s, as expected under this rule",
                                            unique(panel$cohort))
                else " -- multiple cohorts present, see Check D below"))
    
    
    #-------------------------------------------------------------------------
    # B. Constraint audit (assertion, not a saved table)
    #
    # Confirms that outcomes script 10's baseline-eligibility filter should
    # have mechanically pinned to a specific value in the baseline quarter
    # are, in fact, exactly that value. This is a correctness check on
    # script 10's filters (would catch e.g. a join silently admitting
    # ineligible rows), not a descriptive result -- so it's a console
    # warning that only speaks up if something is wrong, not a table saved
    # every run regardless of outcome.
    #-------------------------------------------------------------------------
    
    baseline_means <- long %>%
      filter(year_quarter == cohort, outcome %in% names(EXPECTED_CONSTRAINED)) %>%
      group_by(outcome) %>%
      summarise(m = mean(y, na.rm = TRUE), .groups = "drop")
    
    constraint_violations <- baseline_means %>%
      mutate(expected = EXPECTED_CONSTRAINED[outcome]) %>%
      filter(abs(m - expected) > 1e-9)
    
    if (nrow(constraint_violations) > 0) {
      cat("  B. *** CONSTRAINT VIOLATION *** the following outcomes should be\n")
      cat("     mechanically pinned at baseline by script 10's eligibility filter\n")
      cat("     but are not -- check script 10's baseline_eligible construction:\n")
      print(as.data.frame(constraint_violations), row.names = FALSE)
    } else {
      cat(sprintf("  B. constraint audit passed: all %d expected-mechanical outcomes are exactly pinned at baseline.\n",
                  nrow(baseline_means)))
    }
    
    
    #-------------------------------------------------------------------------
    # D. The free quarter
    #
    # For people baselined after the first quarter (only possible under
    # any_pre_first), the earlier pre observation is unconstrained -- and is
    # where they were before qualifying. If treatment and control differ
    # there, the mechanical distortion enters the DiD. Self-skips under a
    # single cohort (nothing before the one shared baseline quarter).
    #-------------------------------------------------------------------------
    
    free_q <- long %>%
      filter(period == "pre", year_quarter < cohort) %>%
      group_by(outcome, cohort, year_quarter, group_label) %>%
      summarise(m = mean(y, na.rm = TRUE), n = dplyr::n(), .groups = "drop")
    
    if (nrow(free_q) > 0) {
      tbl_free <- free_q %>%
        tidyr::pivot_wider(names_from = group_label,
                           values_from = c(m, n)) %>%
        gt::gt(groupname_col = "outcome") %>%
        gt::tab_header(
          title = "D. Unconstrained pre observations, by group",
          subtitle = sub_txt) %>%
        gt::tab_source_note(paste(
          "Pre-period observations that fall BEFORE the person's own baseline",
          "quarter, so are not fixed by the eligibility rule. These people had",
          "not yet qualified, so low values here are expected. A difference",
          "between treatment and control is what injects the mechanical",
          "distortion into the DiD; its size is roughly the cohort share",
          "times that difference.")) %>%
        gt::tab_source_note(SRC)
      save_tbl(tbl_free, "tbl_11B_free_quarter")
      cat(sprintf("  D. unconstrained pre cells: %d\n", nrow(free_q)))
    } else {
      cat("  D. no unconstrained pre observations (single-cohort window)\n")
    }
    
    
    #-------------------------------------------------------------------------
    # E. Switcher audit
    #
    # Under individual fixed effects only within-person switches identify
    # anything. Outcomes with almost no switchers should not be reported.
    #-------------------------------------------------------------------------
    
    sw <- long %>%
      group_by(outcome, group_label, ID_PERSONA) %>%
      summarise(ever1 = any(y == 1, na.rm = TRUE),
                ever0 = any(y == 0, na.rm = TRUE), .groups = "drop") %>%
      group_by(outcome, group_label) %>%
      summarise(n_indiv = dplyr::n(),
                n_ever1 = sum(ever1),
                n_switch = sum(ever1 & ever0), .groups = "drop")
    
    sw_min <- sw %>%
      group_by(outcome) %>%
      summarise(min_switch = min(n_switch, na.rm = TRUE), .groups = "drop")
    
    sw_wide <- sw %>%
      tidyr::pivot_wider(names_from = group_label,
                         values_from = c(n_indiv, n_ever1, n_switch)) %>%
      left_join(sw_min, by = "outcome") %>%
      mutate(flag = ifelse(min_switch < MIN_SWITCHERS, "TOO FEW", "")) %>%
      arrange(min_switch)
    
    tbl_sw <- sw_wide %>%
      gt::gt() %>%
      gt::tab_header(title = "E. Within-person variation by outcome",
                     subtitle = sub_txt) %>%
      gt::tab_source_note(sprintf(paste(
        "n_switch is the number of individuals observed with both values, the",
        "only people who contribute to a fixed-effects estimate. Outcomes",
        "flagged TOO FEW have under %d switchers in at least one group and",
        "should not be reported as results."), MIN_SWITCHERS)) %>%
      gt::tab_source_note(SRC)
    
    save_tbl(tbl_sw, "tbl_11B_switcher_audit")
    cat(sprintf("  E. outcomes with too few switchers: %s\n",
                paste(sw_wide$outcome[sw_wide$flag != ""], collapse = ", ")))
    
    
    #-------------------------------------------------------------------------
    # Accumulate cross-window summary
    #-------------------------------------------------------------------------
    
    window_summary[[paste(win_name, balance_label, sep = "_")]] <-
      tibble::tibble(
        window       = win$label,
        balance      = balance_label,
        n_indiv      = n_distinct(panel$ID_PERSONA),
        n_treat      = n_distinct(panel$ID_PERSONA[panel$treat == 1]),
        n_ctrl       = n_distinct(panel$ID_PERSONA[panel$treat == 0]),
        n_cohorts    = n_cohorts,
        n_flagged_outcomes = sum(sw_wide$flag != "")
      )
    
  } # end balance loop
} # end window loop


#===============================================================================
# Cross-window summary
#===============================================================================

ws <- dplyr::bind_rows(window_summary)

if (nrow(ws) > 0) {
  
  tbl_ws <- ws %>%
    gt::gt() %>%
    gt::cols_label(
      window = "Window", balance = "Balance", n_indiv = "Individuals",
      n_treat = "T", n_ctrl = "C", n_cohorts = "Cohorts",
      n_flagged_outcomes = "Outcomes flagged") %>%
    gt::tab_header(
      title = "Sample diagnostics across windows",
      subtitle = sprintf("Event: %s | Treatment: %s | Control: %s | Baseline rule: %s",
                         M2_EVENT$event_tag, M2_TREATMENT_TAG, M2_CONTROL_TAG, M2_BASELINE_TAG)) %>%
    gt::tab_source_note(paste(
      "Cohorts = distinct baseline quarters present in this panel; should be",
      "1 under the default first_qtr_only rule. Outcomes flagged = count of",
      "outcomes with fewer than MIN_SWITCHERS within-person switches in",
      "either group (see tbl_11B_switcher_audit).")) %>%
    gt::tab_source_note(SRC)
  
  tryCatch({
    gt::gtsave(tbl_ws, file.path(m2_out_root, "tbl_11B_window_summary.html"))
    gt::gtsave(tbl_ws, file.path(m2_out_root, "tbl_11B_window_summary.png"),
               expand = 10)
  }, error = function(e) cat("  Summary table failed: ", e$message, "\n"))
  
  readr::write_csv(ws, file.path(m2_out_root, "tbl_11B_window_summary.csv"))
  
  cat("\n  Cross-window summary:\n")
  print(as.data.frame(ws))
}

cat("\n=== 11B_baseline_cohort_diagnostics.R complete ===\n")
