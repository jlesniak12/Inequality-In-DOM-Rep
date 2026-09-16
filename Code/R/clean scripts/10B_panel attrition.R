#===============================================================================
#
# Script: 10B_panel_attrition.R
#
# Purpose: Waterfall attrition tables for the Method 2 (2021Q3 micro MW tier)
#          individual panel. Tracks, separately for treatment (Micro) and
#          control (Small), and separately for the pre- and post-event
#          period, how many individuals survive:
#
#            Row 1  Assigned to panel (baseline treatment/control identified)
#            Row 2  Retained under the panel's balance rule (this is where
#                   "balanced" vs "unbalanced" diverge -- see run_window())
#            Row 3  Employed
#            Row 4  Private employee (excl. self-employed / owner)
#            Row 5  Formal private employee
#
#          Loops over EVERY window in config$method2$windows and produces,
#          for each, a balanced and unbalanced waterfall table (balanced is
#          skipped automatically when the window can't fit inside the ENCFT
#          5-quarter rotation -- see the feasibility check in run_window()),
#          plus one baseline-rule sensitivity table per window.
#
# ON THE BASELINE-QUARTER CHOICE:
#   Script 10 assigns baseline per config$method2$baseline_rule, defaulting
#   to first_qtr_only (fixed calendar baseline -- the earliest pre quarter in
#   each window). The old person-specific any_pre_first rule (each person's
#   own first eligible pre quarter) is still available in config for
#   comparison, but is no longer the default: 11B_baseline_cohort_check.R
#   documents why it mixes baseline cohorts and mechanically pins each
#   person's own-baseline-quarter outcome at 0/1. This script's headline
#   waterfall follows whatever M2_BASELINE_RULE resolves to (STEP 0), so it
#   always describes the same panel script 10 actually builds; the
#   sensitivity table separately compares all three rules regardless of the
#   active config value.
#
# Pipeline: 01A -> 01B -> 02 -> 10 -> [10B] -> 11 -> 11B -> 12
#           Mirrors 10's STEP 1-3 independently (does not read 10's saved
#           .rds panels), so M2_EVENT / M2_CONTROL_BW / M2_TREAT_MIN_FS /
#           M2_BASELINE_RULE must match 10's config for the counts to
#           reconcile with 11/12.
#
# Reads:  Full_ENCFT_clean.rds (02)
#
# Writes: <outputs>/.../Panel Regressions/<event>/<treatment>/<control>/
#           <baseline_rule>/<balance>/<window>/
#           Panel Attrition/tbl_10B_waterfall.{html,png}
#           Panel Attrition/tbl_10B_baseline_rule_sensitivity.{html,png}
#             (only under "unbalanced" -- see run_window() for why)
#
#   NOTE ON FOLDER ORDER: event -> treatment -> control -> baseline_rule ->
#   balance -> window -> <type folder>. Matches scripts 11/12's convention.
#
#===============================================================================

if (!exists("config")) {
  source(here::here("Code", "R", "clean scripts", "00_setup.R"))
}

cat("=== 10B_panel_attrition.R ===\n\n")

#===============================================================================
# STEP 0. Parameters that don't vary by window
#===============================================================================

M2_EVENT   <- config$method2$events$event_2021q3_micro
M2_WINDOWS <- config$method2$windows

M2_CONTROL_BW   <- config$method2$control_bandwidth
M2_TREAT_MIN_FS <- config$method2$treatment_min_firmsize

M2_AGE_MIN <- config$age$regression$min
M2_AGE_MAX <- config$age$regression$max

TREAT_LABEL   <- config$m2_labels$treatment
CONTROL_LABEL <- config$m2_labels$control

# Must match script 10's rule for the headline waterfall to describe the
# same panel script 10 actually builds. Falls back to "first_qtr_only" if
# not yet set in 00_config.R -- same fallback used in scripts 10/11/11B/12.
M2_BASELINE_RULE <- config$method2$baseline_rule %||% "first_qtr_only"

M2_TREATMENT_TAG <- if (!is.null(M2_TREAT_MIN_FS) && M2_TREAT_MIN_FS > 1) {
  sprintf("micro%dplus", M2_TREAT_MIN_FS)
} else {
  "micro_all"
}
M2_CONTROL_TAG  <- M2_CONTROL_BW
M2_BASELINE_TAG <- M2_BASELINE_RULE

out_root <- file.path(
  config$paths$outputs, config$output_stage, "Panel Regressions",
  M2_EVENT$event_tag, M2_TREATMENT_TAG, M2_CONTROL_TAG, M2_BASELINE_TAG
)

SRC <- "Source: ENCFT (Banco Central de la Rep\u00fablica Dominicana)."

# WINDOWS_TO_RUN: defaults to every window in config -- add a window there
# (e.g. a 3-pre/1-post variant) and it's picked up here automatically, no
# script edit needed. Narrow this vector to re-run a subset during dev.
WINDOWS_TO_RUN <- names(M2_WINDOWS)

cat(sprintf("  Control: %s | Treatment: %s\n", CONTROL_LABEL, TREAT_LABEL))
cat(sprintf("  Windows: %s\n", paste(WINDOWS_TO_RUN, collapse = ", ")))
cat(sprintf("  Output root: %s\n\n", out_root))

cat("[1] Loading Full_ENCFT_clean...\n")
full_data <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))
cat(sprintf("  %s rows x %d cols\n\n", format(nrow(full_data), big.mark = ","), ncol(full_data)))


#===============================================================================
# STEP 1. Baseline (treatment-assignment) rules -- same three rules for
#         every window; parameterized on (elig_pool, pre_qtrs) so run_window()
#         can reuse it with each window's own elig_pool.
#
#   any_pre_first   CURRENT SCRIPT-10 LOGIC. Baseline = the first pre quarter,
#                   in calendar order, in which the person happens to satisfy
#                   every elig_pool condition. PERSON-SPECIFIC conditioning
#                   quarter. 11B_baseline_cohort_check.R shows this mixes
#                   baseline cohorts and mechanically pins each person's
#                   own-baseline-quarter outcomes at 0/1. Shown in the
#                   sensitivity table only, not the headline.
#
#   first_qtr_only  Fixed calendar baseline: everyone assigned from the same
#                   quarter, the earliest pre quarter in the window. One
#                   cohort, no mixing. RECOMMENDED / headline rule.
#
#   all_pre_qtrs    Eligible in EVERY pre quarter of the window, same tier
#                   throughout. Screens recall noise in the firm-size
#                   question; costs sample and selects on stable pre-period
#                   employment. Robustness, not headline.
#===============================================================================

assign_treatment <- function(elig_pool, pre_qtrs,
                             rule = c("any_pre_first", "first_qtr_only", "all_pre_qtrs")) {
  rule          <- match.arg(rule)
  first_pre_qtr <- sort(pre_qtrs)[1]
  
  out <- switch(rule,
                any_pre_first = elig_pool %>%
                  arrange(ID_PERSONA, year_quarter) %>%
                  group_by(ID_PERSONA) %>%
                  slice(1) %>%
                  ungroup(),
                
                first_qtr_only = elig_pool %>%
                  filter(year_quarter == first_pre_qtr),
                
                all_pre_qtrs = elig_pool %>%
                  group_by(ID_PERSONA) %>%
                  filter(n_distinct(year_quarter) == length(pre_qtrs),
                         n_distinct(wage_group) == 1) %>%
                  slice(1) %>%
                  ungroup()
  )
  
  out %>%
    transmute(ID_PERSONA, ID_HOGAR, baseline_qtr = year_quarter,
              baseline_tier = wage_group,
              treat = as.integer(wage_group == M2_EVENT$treatment$tier))
}

WATERFALL_STEPS <- c(
  "3. Employed"                                 = "OCUPADO == 1",
  "4. Private employee (excl. self-emp./owner)" = "OCUPADO == 1 & Employment_Type == 'private employee'",
  "5. Formal private employee"                  = "OCUPADO == 1 & Employment_Type == 'private employee' & !is.na(Employment_Status) & Employment_Status == 'Formal'"
)


#===============================================================================
# STEP 2. Waterfall engine (unchanged in logic from the previous version;
#         now takes pre_qtrs/post_qtrs/keep_qtrs/tracking_pool as arguments
#         instead of reading them from the global environment, so it's safe
#         to call once per window inside the loop)
#===============================================================================

panel_waterfall <- function(baseline_assign, tracking_pool, keep_qtrs,
                            pre_qtrs, post_qtrs,
                            balance = c("balanced", "unbalanced")) {
  
  balance   <- match.arg(balance)
  panel_ids <- baseline_assign$ID_PERSONA
  
  row1_base <- baseline_assign %>%
    mutate(group = ifelse(treat == 1, "Treatment", "Control")) %>%
    group_by(group) %>%
    summarise(n_indiv = n_distinct(ID_PERSONA), .groups = "drop")
  row1 <- tidyr::crossing(row1_base, period = c("Pre", "Post")) %>%
    mutate(step = "1. Assigned to panel (treat/control identified)")
  
  pool_all <- tracking_pool %>%
    filter(ID_PERSONA %in% panel_ids, year_quarter %in% keep_qtrs) %>%
    left_join(baseline_assign %>% select(ID_PERSONA, treat), by = "ID_PERSONA") %>%
    mutate(period = dplyr::case_when(
      year_quarter %in% pre_qtrs  ~ "Pre",
      year_quarter %in% post_qtrs ~ "Post",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(period)) %>%
    mutate(group = ifelse(treat == 1, "Treatment", "Control"))
  
  person_periods <- pool_all %>%
    group_by(ID_PERSONA) %>%
    summarise(n_pre  = n_distinct(year_quarter[period == "Pre"]),
              n_post = n_distinct(year_quarter[period == "Post"]),
              .groups = "drop")
  
  retained_ids <- if (balance == "balanced") {
    person_periods %>%
      filter(n_pre == length(pre_qtrs), n_post == length(post_qtrs)) %>%
      pull(ID_PERSONA)
  } else {
    person_periods %>%
      filter(n_pre >= 1, n_post >= 1) %>%
      pull(ID_PERSONA)
  }
  
  pool_ret <- pool_all %>% filter(ID_PERSONA %in% retained_ids)
  
  row2_base <- pool_ret %>%
    distinct(ID_PERSONA, group) %>%
    group_by(group) %>%
    summarise(n_indiv = n_distinct(ID_PERSONA), .groups = "drop")
  row2 <- tidyr::crossing(row2_base, period = c("Pre", "Post")) %>%
    mutate(step = sprintf("2. Retained (%s panel)", balance))
  
  rows_emp <- purrr::imap_dfr(WATERFALL_STEPS, function(expr_str, lbl) {
    idx <- with(pool_ret, eval(parse(text = expr_str)))
    idx[is.na(idx)] <- FALSE
    pool_ret[idx, ] %>%
      group_by(group, period) %>%
      summarise(n_indiv = n_distinct(ID_PERSONA), .groups = "drop") %>%
      mutate(step = lbl)
  })
  
  step_levels <- c("1. Assigned to panel (treat/control identified)",
                   sprintf("2. Retained (%s panel)", balance),
                   names(WATERFALL_STEPS))
  
  bind_rows(row1, row2, rows_emp) %>%
    mutate(step   = factor(step, levels = step_levels),
           group  = factor(group, levels = c("Treatment", "Control")),
           period = factor(period, levels = c("Pre", "Post"))) %>%
    arrange(step, group, period)
}


#===============================================================================
# STEP 3. gt renderer (unchanged in logic; takes first_pre_qtr/pre_qtrs/
#         post_qtrs as arguments rather than globals)
#===============================================================================

render_waterfall_gt <- function(dat, title, subtitle, balance_label,
                                first_pre_qtr, pre_qtrs, post_qtrs) {
  
  wide <- dat %>%
    select(step, group, period, n_indiv) %>%
    tidyr::pivot_wider(
      names_from = c(group, period),
      values_from = n_indiv,
      names_glue = "{group}_{period}"
    ) %>%
    select(step, Treatment_Pre, Treatment_Post, Control_Pre, Control_Post)
  
  row1_lbl <- "1. Assigned to panel (treat/control identified)"
  row2_lbl <- sprintf("2. Retained (%s panel)", balance_label)
  
  row2_note <- if (balance_label == "unbalanced") {
    sprintf(paste(
      "Retention requires at least 1 pre quarter AND at least 1 post quarter",
      "observed in the survey. Baseline eligibility (Row 1) already requires",
      "a record in %s, a pre quarter, so the pre-quarter condition holds",
      "mechanically for everyone in Row 1 -- all attrition in this row comes",
      "from the post-period condition."), first_pre_qtr)
  } else {
    sprintf(paste(
      "Retention requires presence in ALL planned pre quarters (%s) AND ALL",
      "planned post quarters (%s)."),
      paste(pre_qtrs, collapse = ", "), paste(post_qtrs, collapse = ", "))
  }
  
  wide %>%
    gt::gt(rowname_col = "step") %>%
    gt::tab_header(title = title, subtitle = subtitle) %>%
    gt::tab_spanner(label = "Treatment", columns = c(Treatment_Pre, Treatment_Post)) %>%
    gt::tab_spanner(label = "Control", columns = c(Control_Pre, Control_Post)) %>%
    gt::cols_label(Treatment_Pre = "Pre", Treatment_Post = "Post",
                   Control_Pre = "Pre", Control_Post = "Post") %>%
    gt::fmt_number(c(Treatment_Pre, Treatment_Post, Control_Pre, Control_Post),
                   decimals = 0) %>%
    gt::sub_missing(everything(), missing_text = "\u2014") %>%
    gt::tab_source_note(paste(
      "Rows 1-2 are person-level counts (assignment/retention are not",
      "period-specific); the same value is shown in the Pre and Post",
      "columns. Rows 3-5 re-measure employment status separately in each",
      "period on the retained sample -- that is where real employment",
      "transitions appear.")) %>%
    gt::tab_footnote(sprintf(paste(
      "These are individuals who satisfied the full baseline eligibility",
      "stack (employed, private-sector, MW-covered, positive salary &",
      "hours, known firm-size tier) specifically in the fixed baseline",
      "quarter, %s -- not people who satisfied it in any quarter of the",
      "window."), first_pre_qtr),
      locations = gt::cells_stub(rows = row1_lbl)) %>%
    gt::tab_footnote(row2_note,
                     locations = gt::cells_stub(rows = row2_lbl)) %>%
    gt::tab_source_note(sprintf(
      "Treatment = %s. Control = %s. Baseline quarter fixed at %s (%s rule).",
      TREAT_LABEL, CONTROL_LABEL, first_pre_qtr, M2_BASELINE_RULE)) %>%
    gt::tab_source_note(paste(
      "Counts are unweighted individuals, not survey-weighted population.")) %>%
    gt::tab_source_note(SRC)
}


#===============================================================================
# STEP 4. run_window(): everything that depends on a single window's
#         pre_qtrs/post_qtrs. Called once per entry in WINDOWS_TO_RUN.
#===============================================================================

run_window <- function(win_name) {
  
  win <- M2_WINDOWS[[win_name]]
  cat(sprintf("\n--- Window: %s (%s) ---\n", win_name, win$label))
  
  pre_qtrs      <- win$pre_qtrs
  post_qtrs     <- win$post_qtrs
  event_qtr     <- M2_EVENT$event_qtr
  first_pre_qtr <- sort(pre_qtrs)[1]
  
  keep_qtrs <- c(pre_qtrs, post_qtrs)
  if (!isTRUE(win$exclude_event)) keep_qtrs <- c(keep_qtrs, event_qtr)
  
  # Dynamic balanced-panel feasibility check: a household's tenure is 5
  # consecutive quarters, so a window needing more than 5 non-... wait, the
  # correct count is simply the number of DISTINCT calendar quarters this
  # window spans end-to-end (pre through post, inclusive of the event
  # quarter whether or not it's excluded from the panel, since a household
  # present pre AND post was necessarily still in-sample during the event
  # quarter too). If that span exceeds 5, no household can cover the full
  # window, so "balanced" is guaranteed empty -- this replaces script 12's
  # hardcoded M2_SKIP_BALANCED list with a computed check that updates
  # automatically for any new window added to config.
  full_span <- sort(unique(c(pre_qtrs, event_qtr, post_qtrs)))
  span_len  <- as.integer(round(
    (as.numeric(substr(max(full_span), 1, 4)) - as.numeric(substr(min(full_span), 1, 4))) * 4 +
      (as.numeric(substr(max(full_span), 6, 6)) - as.numeric(substr(min(full_span), 6, 6)))
  )) + 1L
  balanced_feasible <- span_len <= 5
  
  cat(sprintf("  Pre: %s | Post: %s | Calendar span: %d qtrs | Balanced feasible: %s\n",
              paste(pre_qtrs, collapse = ","), paste(post_qtrs, collapse = ","),
              span_len, balanced_feasible))
  
  # --- Build this window's eligibility / tracking pools ---
  all_records <- full_data %>%
    filter(year_quarter %in% c(pre_qtrs, post_qtrs),
           EDAD >= M2_AGE_MIN, EDAD <= M2_AGE_MAX)
  
  elig_pool <- all_records %>%
    filter(year_quarter %in% pre_qtrs) %>%
    filter(OCUPADO == 1) %>%
    filter(Employment_Type == "private employee") %>%
    filter(!Principal_Category %in% "Domestic Worker",
           !Principal_Category %in% "Free Trade Zone",
           !Employment_Sector %in% "Electricity and Water") %>%
    filter(!is.na(real_salary_income_wage_primary),
           real_salary_income_wage_primary > 0,
           !is.na(hours_worked_primary),
           hours_worked_primary > 0) %>%
    filter(has_tier) %>%
    filter(!is.na(Employment_Status)) %>%
    filter(wage_group %in% c(M2_EVENT$treatment$tier, M2_EVENT$control$tier))
  
  if (M2_CONTROL_BW == "narrow") {
    elig_pool <- elig_pool %>%
      filter(wage_group == M2_EVENT$treatment$tier | Firm_size == "11-20")
  }
  # The != 98 guard must match script 10's identical filter -- 98 is the
  # survey's "don't know" sentinel on the follow-up integer headcount, not a
  # count, so without it every DK respondent passes as a 98-person firm.
  # See script 10's fuller comment on this block.
  if (!is.null(M2_TREAT_MIN_FS) && M2_TREAT_MIN_FS > 1) {
    elig_pool <- elig_pool %>%
      filter(wage_group != M2_EVENT$treatment$tier |
               (CANTIDAD_PERSONAS_TRABAJAN_EMP >= M2_TREAT_MIN_FS &
                  CANTIDAD_PERSONAS_TRABAJAN_EMP != 98))
  }
  
  tracking_pool <- all_records %>%
    filter(ID_PERSONA %in% unique(elig_pool$ID_PERSONA))
  
  cat(sprintf("  elig_pool obs: %s | candidates: %s\n",
              format(nrow(elig_pool), big.mark = ","),
              format(n_distinct(elig_pool$ID_PERSONA), big.mark = ",")))
  
  # --- Headline waterfalls (M2_BASELINE_RULE), one subfolder per balance ---
  balance_modes <- if (balanced_feasible) c("balanced", "unbalanced") else "unbalanced"
  if (!balanced_feasible) {
    cat("  Skipping balanced panel: window spans >5 calendar quarters, no household can cover it.\n")
  }
  
  ba <- assign_treatment(elig_pool, pre_qtrs, M2_BASELINE_RULE)
  
  bal_dirs <- list()  # keep a handle on each balance's "Panel Attrition" dir
  
  for (bal in balance_modes) {
    # order matches 11/12's actual convention:
    # event/treatment/control/baseline_rule / balance / window / <type folder>
    bal_dir <- file.path(out_root, bal, win$tag, "Panel Attrition")
    dir.create(bal_dir, recursive = TRUE, showWarnings = FALSE)
    bal_dirs[[bal]] <- bal_dir
    
    dat <- panel_waterfall(ba, tracking_pool, keep_qtrs, pre_qtrs, post_qtrs, balance = bal)
    
    tbl <- render_waterfall_gt(
      dat,
      title = sprintf("Table 10B. Panel attrition around the 2021Q3 event (%s panel)", bal),
      subtitle = sprintf("%s | Pre: %s | Post: %s", win$label,
                         paste(pre_qtrs, collapse = ", "), paste(post_qtrs, collapse = ", ")),
      balance_label = bal, first_pre_qtr = first_pre_qtr,
      pre_qtrs = pre_qtrs, post_qtrs = post_qtrs
    )
    
    gt::gtsave(tbl, file.path(bal_dir, "tbl_10B_waterfall.html"))
    gt::gtsave(tbl, file.path(bal_dir, "tbl_10B_waterfall.png"), expand = 10)
    cat(sprintf("  Wrote %s waterfall to %s\n", bal, bal_dir))
  }
  
  # --- Baseline-rule sensitivity table ---
  # This is inherently an "unbalanced" computation (see panel_waterfall call
  # below), so it's saved alongside the unbalanced waterfall's Panel
  # Attrition folder rather than at a separate window-level location.
  sens <- purrr::map_dfr(c("any_pre_first", "first_qtr_only", "all_pre_qtrs"),
                         function(rule) {
                           ba_r <- assign_treatment(elig_pool, pre_qtrs, rule)
                           panel_waterfall(ba_r, tracking_pool, keep_qtrs, pre_qtrs, post_qtrs,
                                           balance = "unbalanced") %>%
                             filter(period == "Pre") %>%
                             mutate(rule = rule) %>%
                             filter(grepl("^1\\.|^2\\.", step))
                         })
  
  tbl_sens <- sens %>%
    select(rule, step, group, n_indiv) %>%
    tidyr::pivot_wider(names_from = group, values_from = n_indiv) %>%
    select(rule, step, Treatment, Control) %>%
    gt::gt(groupname_col = "rule") %>%
    gt::tab_header(
      title = "Table 10B-sens. Sensitivity of panel size to the baseline-assignment rule",
      subtitle = sprintf("%s | unbalanced panel", win$label)) %>%
    gt::tab_source_note(paste(
      "any_pre_first assigns baseline from each person's first eligible pre",
      "quarter (person-specific) -- the current script-10 rule, shown here",
      "for comparison only. 11B_baseline_cohort_check.R documents that this",
      "mixes baseline cohorts and pins outcomes in each person's own baseline",
      "quarter.", "first_qtr_only fixes baseline at", first_pre_qtr,
      "for everyone (RECOMMENDED). all_pre_qtrs additionally requires the",
      "same tier in every pre quarter (robustness, not headline).")) %>%
    gt::tab_source_note(SRC)
  
  sens_dir <- bal_dirs[["unbalanced"]]  # "unbalanced" always present (balance_modes always includes it)
  gt::gtsave(tbl_sens, file.path(sens_dir, "tbl_10B_baseline_rule_sensitivity.html"))
  gt::gtsave(tbl_sens, file.path(sens_dir, "tbl_10B_baseline_rule_sensitivity.png"), expand = 10)
  cat(sprintf("  Wrote sensitivity table to %s\n", sens_dir))
  
  invisible(NULL)
}


#===============================================================================
# STEP 5. Run every window
#===============================================================================

for (w in WINDOWS_TO_RUN) run_window(w)

cat(sprintf("\n[Done] All windows written under: %s\n", out_root))