#===============================================================================
#
# Script: 11B_baseline_cohort_diagnostics.R
#
# Purpose: Diagnose the sample-construction problems that surfaced when reading
#          the Method 2 results, all of which trace to one design choice in
#          script 10 (lines 340-344): baseline is assigned from the FIRST
#          eligible pre observation via slice(1), so the conditioning quarter
#          is person-specific rather than fixed.
#
#          Consequences this script measures:
#            - outcomes are mechanically equal to 1 (or 0) in each person's own
#              baseline quarter, so pre-period means and pre-trends are partly
#              definitional rather than behavioural;
#            - the panel is a mixture of baseline cohorts that can give
#              different DiD answers, with the pooled estimate depending on the
#              mixture rather than on a common effect;
#            - some binary outcomes have too few within-person switches to
#              identify anything under individual fixed effects.
#
# Pipeline: 01A -> 01B -> 02 -> 10 -> 11 -> [11B] -> 12
#
# Reads:  individual_panel.rds from
#         <processed>/Panel Regressions/<event>/<sample_tag>/<balance>/<win_tag>/
#
# Writes: <outputs>/.../<balance>/<win_tag>/Sample Evaluation/
#           tbl_11B_cohort_composition   A. who is in which baseline cohort
#           tbl_11B_constraint_audit     B. which cells are definitional
#           tbl_11B_cohort_did           C. does the DiD agree across cohorts
#           tbl_11B_free_quarter         D. the unconstrained pre observation
#           tbl_11B_switcher_audit       E. outcomes with too little variation
#           tbl_11B_fixed_baseline       F. cost of fixing the baseline quarter
#           fig_11B_cohort_trends        the picture behind A-D
#         <outputs>/.../tbl_11B_window_summary   across all windows
#
# NOTE: this script only measures the problem. Fixing it means changing the
#       baseline rule in script 10 to a fixed quarter and excluding that
#       quarter from the estimation sample.
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

# Shown in the trends figure
FIG_OUTCOMES <- c("is_employed", "is_private_employee", "has_wage",
                  "is_informal_now")

# Below this many within-person switches, an outcome cannot be meaningfully
# estimated with individual fixed effects
MIN_SWITCHERS <- 10

cat(sprintf("  Event: %s | Sample: %s\n", M2_EVENT$event_tag, sample_tag))
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
    
    save_fig <- function(p, name,
                         w = config$fig_defaults$width,
                         h = config$fig_defaults$height) {
      fp <- file.path(win_out_dir,
                      paste0(name, ".", config$fig_defaults$format))
      ggsave(fp, plot = p, width = w, height = h,
             dpi = config$fig_defaults$dpi)
      message("  Saved: ", fp)
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
    qtrs <- sort(unique(panel$year_quarter))
    
    sub_txt <- sprintf("%s | %s | Treatment: %s | Control: %s",
                       win$label, balance_label, TREAT_LABEL, CONTROL_LABEL)
    
    long <- panel %>%
      select(ID_PERSONA, treat, group_label, cohort, period, year_quarter,
             all_of(outs)) %>%
      tidyr::pivot_longer(all_of(outs), names_to = "outcome",
                          values_to = "y")
    
    
    #-------------------------------------------------------------------------
    # A. Baseline cohort composition
    #
    # If the cohort shares differ across treatment and control, the mechanical
    # distortion does not difference out of the DiD.
    #-------------------------------------------------------------------------
    
    comp <- panel %>%
      distinct(ID_PERSONA, treat, group_label, cohort) %>%
      count(group_label, cohort, name = "n_indiv") %>%
      group_by(group_label) %>%
      mutate(share = 100 * n_indiv / sum(n_indiv)) %>%
      ungroup()
    
    share_gap <- comp %>%
      group_by(cohort) %>%
      summarise(gap = diff(range(share)), .groups = "drop") %>%
      summarise(max_gap = max(gap, na.rm = TRUE)) %>%
      pull(max_gap)
    
    tbl_comp <- comp %>%
      mutate(share = sprintf("%.1f%%", share)) %>%
      gt::gt(groupname_col = "group_label") %>%
      gt::cols_label(cohort = "Baseline quarter", n_indiv = "Individuals",
                     share = "Share of group") %>%
      gt::tab_header(
        title = "A. Baseline cohort composition",
        subtitle = sub_txt) %>%
      gt::tab_source_note(paste(
        "Script 10 assigns baseline from each person's FIRST eligible pre",
        "observation, so the conditioning quarter varies across people.",
        "Where cohort shares are similar across groups the resulting",
        "distortion largely differences out of the DiD; where they differ it",
        "does not.")) %>%
      gt::tab_source_note(sprintf(
        "Largest across-group difference in cohort share: %.1f percentage points.",
        share_gap)) %>%
      gt::tab_source_note(SRC)
    
    save_tbl(tbl_comp, "tbl_11B_cohort_composition")
    cat(sprintf("  A. cohorts: %d | max across-group share gap %.1f pp\n",
                n_distinct(comp$cohort), share_gap))
    
    
    #-------------------------------------------------------------------------
    # B. Constraint audit
    #
    # Flags cells whose mean is exactly 0 or 1. Those are definitional, not
    # behavioural, and any pre-trend drawn through them is an artefact.
    #-------------------------------------------------------------------------
    
    cellmeans <- long %>%
      group_by(outcome, cohort, year_quarter) %>%
      summarise(m = mean(y, na.rm = TRUE), n = dplyr::n(), .groups = "drop")
    
    constrained <- cellmeans %>%
      filter(abs(m - 1) < 1e-9 | abs(m) < 1e-9) %>%
      mutate(value = ifelse(abs(m - 1) < 1e-9, "1.000", "0.000"),
             own_baseline = ifelse(cohort == year_quarter, "yes", "no")) %>%
      arrange(outcome, cohort, year_quarter)
    
    if (nrow(constrained) > 0) {
      tbl_con <- constrained %>%
        select(outcome, cohort, year_quarter, value, n, own_baseline) %>%
        gt::gt() %>%
        gt::cols_label(outcome = "Outcome", cohort = "Baseline cohort",
                       year_quarter = "Quarter", value = "Mean",
                       n = "Obs", own_baseline = "Own baseline qtr?") %>%
        gt::tab_header(title = "B. Definitional cells (mean exactly 0 or 1)",
                       subtitle = sub_txt) %>%
        gt::tab_source_note(paste(
          "Cells where the outcome is fixed by the baseline eligibility rule",
          "rather than observed. 'Own baseline qtr = yes' means the value is",
          "imposed by construction. Any outcome appearing here has a",
          "pre-period mean and a pre-trend that are partly definitional.")) %>%
        gt::tab_source_note(SRC)
      save_tbl(tbl_con, "tbl_11B_constraint_audit")
    }
    
    cat(sprintf("  B. definitional cells: %d (%d in own baseline quarter)\n",
                nrow(constrained), sum(constrained$own_baseline == "yes")))
    
    
    #-------------------------------------------------------------------------
    # C. Cohort-specific DiD decomposition
    #
    # The pooled DiD is a mixture of within-cohort DiDs. If they disagree, the
    # pooled number reflects the cohort mixture, not a common effect.
    #-------------------------------------------------------------------------
    
    did_cells <- function(dat, coh_label) {
      dat %>%
        group_by(outcome, treat, period) %>%
        summarise(m = mean(y, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = c(period, treat), values_from = m,
                           names_glue = "{period}_t{treat}") %>%
        mutate(cohort = coh_label)
    }
    
    did_tab <- bind_rows(
      long %>% group_split(cohort) %>%
        purrr::map_dfr(~ did_cells(.x, unique(.x$cohort))),
      did_cells(long, "All (pooled)")
    )
    
    need <- c("pre_t0", "pre_t1", "post_t0", "post_t1")
    for (cc in need) if (!cc %in% names(did_tab)) did_tab[[cc]] <- NA_real_
    
    did_tab <- did_tab %>%
      mutate(d_treat = post_t1 - pre_t1,
             d_ctrl  = post_t0 - pre_t0,
             did     = d_treat - d_ctrl) %>%
      select(outcome, cohort, pre_t1, post_t1, pre_t0, post_t0,
             d_treat, d_ctrl, did) %>%
      arrange(outcome, cohort)
    
    # How far apart are the within-cohort answers?
    spread <- did_tab %>%
      filter(cohort != "All (pooled)") %>%
      group_by(outcome) %>%
      summarise(did_spread = diff(range(did, na.rm = TRUE)), .groups = "drop")
    
    tbl_did <- did_tab %>%
      left_join(spread, by = "outcome") %>%
      mutate(across(c(pre_t1, post_t1, pre_t0, post_t0, d_treat, d_ctrl,
                      did, did_spread),
                    ~ ifelse(is.na(.x), "", sprintf("%.3f", .x)))) %>%
      mutate(did_spread = ifelse(cohort == "All (pooled)", did_spread, "")) %>%
      gt::gt(groupname_col = "outcome") %>%
      gt::cols_label(cohort = "Cohort", pre_t1 = "Pre", post_t1 = "Post",
                     pre_t0 = "Pre", post_t0 = "Post", d_treat = "Delta T",
                     d_ctrl = "Delta C", did = "DiD",
                     did_spread = "Spread") %>%
      gt::tab_header(title = "C. DiD by baseline cohort (unadjusted means)",
                     subtitle = sub_txt) %>%
      gt::tab_source_note(paste(
        "Raw group means, no fixed effects, so these will not equal the",
        "regression estimates; they show whether the cohorts point the same",
        "way. 'Spread' is the range of the within-cohort DiDs. A large spread",
        "means the pooled estimate depends on the cohort mixture.")) %>%
      gt::tab_source_note(SRC)
    
    save_tbl(tbl_did, "tbl_11B_cohort_did")
    
    emp_spread <- spread %>% filter(outcome == "is_employed") %>% pull(did_spread)
    if (length(emp_spread) == 0) emp_spread <- NA_real_
    cat(sprintf("  C. is_employed within-cohort DiD spread: %.3f\n", emp_spread))
    
    
    #-------------------------------------------------------------------------
    # D. The free quarter
    #
    # For people baselined after the first quarter, the earlier pre observation
    # is unconstrained — and is where they were before qualifying. If treatment
    # and control differ there, the mechanical distortion enters the DiD.
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
    # F. Cost of fixing the baseline quarter
    #
    # How many individuals survive if baseline is conditioned on one fixed
    # quarter and that quarter is then dropped from estimation?
    #-------------------------------------------------------------------------
    
    elig_cols <- intersect(c("is_private_employee", "has_wage",
                             "is_tier_observed"), names(panel))
    
    if (length(elig_cols) == 0) {
      panel$is_elig_approx <- NA_integer_
    } else {
      elig_mat <- vapply(elig_cols, function(cn) panel[[cn]] %in% 1,
                         logical(nrow(panel)))
      panel$is_elig_approx <- as.integer(
        rowSums(elig_mat) == length(elig_cols))
    }
    
    feas <- purrr::map_dfr(qtrs, function(q) {
      
      ids <- panel %>%
        filter(year_quarter == q, is_elig_approx == 1) %>%
        distinct(ID_PERSONA, treat)
      
      usable <- panel %>%
        filter(ID_PERSONA %in% ids$ID_PERSONA, year_quarter != q) %>%
        group_by(ID_PERSONA) %>%
        summarise(ok = any(period == "pre") && any(period == "post"),
                  .groups = "drop") %>%
        filter(ok) %>%
        inner_join(ids, by = "ID_PERSONA")
      
      tibble::tibble(
        baseline_qtr = as.character(q),
        elig_treat   = sum(ids$treat == 1),
        elig_ctrl    = sum(ids$treat == 0),
        usable_treat = sum(usable$treat == 1),
        usable_ctrl  = sum(usable$treat == 0),
        valid_choice = ifelse(q == min(qtrs), "yes", "no")
      )
    })
    
    tbl_feas <- feas %>%
      gt::gt() %>%
      gt::cols_label(baseline_qtr = "Fixed baseline quarter",
                     elig_treat = "Eligible T", elig_ctrl = "Eligible C",
                     usable_treat = "Usable T", usable_ctrl = "Usable C",
                     valid_choice = "Valid?") %>%
      gt::tab_header(title = "F. Sample under a fixed baseline quarter",
                     subtitle = sub_txt) %>%
      gt::tab_source_note(paste(
        "Usable = eligible in the fixed quarter and observed in at least one",
        "other pre and one post quarter, with the conditioning quarter",
        "dropped from estimation.")) %>%
      gt::tab_source_note(paste(
        "Only the EARLIEST quarter in the window is a valid choice. Choosing a",
        "later quarter selects the retained earlier observations on being",
        "eligible in the future, which creates the same artefact inside the",
        "pre-period.")) %>%
      gt::tab_source_note(paste(
        "Eligibility is approximated from the panel as private employee with",
        "a positive wage and a reported firm-size tier; it omits the",
        "minimum-wage coverage exclusions applied upstream, so counts are a",
        "slight upper bound.")) %>%
      gt::tab_source_note(SRC)
    
    save_tbl(tbl_feas, "tbl_11B_fixed_baseline")
    
    first_q <- feas %>% filter(valid_choice == "yes")
    cat(sprintf("  F. fixing baseline at %s leaves %d T / %d C usable\n",
                first_q$baseline_qtr, first_q$usable_treat,
                first_q$usable_ctrl))
    
    
    #-------------------------------------------------------------------------
    # Figure: cohort trends
    #-------------------------------------------------------------------------
    
    fig_dat <- long %>%
      filter(outcome %in% intersect(FIG_OUTCOMES, outs)) %>%
      group_by(outcome, cohort, group_label, year_quarter) %>%
      summarise(m = mean(y, na.rm = TRUE), .groups = "drop")
    
    if (nrow(fig_dat) > 0 && n_distinct(fig_dat$cohort) > 0) {
      
      fig_coh <- ggplot(
        fig_dat,
        aes(x = year_quarter, y = m, colour = group_label,
            linetype = cohort,
            group = interaction(group_label, cohort))) +
        geom_hline(yintercept = 1, linetype = "dotted", colour = "grey60") +
        geom_line(linewidth = 0.6) +
        geom_point(size = 1.8) +
        facet_wrap(~ outcome, scales = "free_y") +
        scale_colour_manual(values = GRP_COLS) +
        labs(
          title = "Outcome means by baseline cohort",
          subtitle = sub_txt,
          x = NULL, y = "Mean", colour = "Group",
          linetype = "Baseline cohort",
          caption = paste(
            "Each cohort is pinned to 1 in its own baseline quarter by the",
            "eligibility rule, so cohort-specific pre-trends are partly",
            "definitional.", SRC)
        ) +
        theme_surveytools() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
      
      save_fig(fig_coh, "fig_11B_cohort_trends")
    }
    
    
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
        n_cohorts    = n_distinct(panel$cohort),
        max_share_gap = share_gap,
        emp_did_spread = emp_spread,
        n_flagged_outcomes = sum(sw_wide$flag != ""),
        usable_fixed_T = first_q$usable_treat,
        usable_fixed_C = first_q$usable_ctrl
      )
    
  } # end balance loop
} # end window loop


#===============================================================================
# Cross-window summary
#===============================================================================

ws <- dplyr::bind_rows(window_summary)

if (nrow(ws) > 0) {
  
  tbl_ws <- ws %>%
    mutate(across(c(max_share_gap, emp_did_spread),
                  ~ sprintf("%.3f", .x))) %>%
    gt::gt() %>%
    gt::cols_label(
      window = "Window", balance = "Balance", n_indiv = "Individuals",
      n_treat = "T", n_ctrl = "C", n_cohorts = "Cohorts",
      max_share_gap = "Max share gap (pp)",
      emp_did_spread = "Employment DiD spread",
      n_flagged_outcomes = "Outcomes flagged",
      usable_fixed_T = "Fixed-baseline T",
      usable_fixed_C = "Fixed-baseline C") %>%
    gt::tab_header(
      title = "Sample diagnostics across windows",
      subtitle = sprintf("Event: %s | Sample: %s",
                         M2_EVENT$event_tag, sample_tag)) %>%
    gt::tab_source_note(paste(
      "Cohorts = distinct person-specific baseline quarters. Share gap and",
      "DiD spread both measure how much the pooled estimate depends on the",
      "cohort mixture; near zero is good. The last two columns give the",
      "sample that survives if the baseline quarter is fixed at the earliest",
      "quarter in the window and dropped from estimation.")) %>%
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
