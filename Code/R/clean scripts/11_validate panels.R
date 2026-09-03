#===============================================================================
#
# Script: 11_method2_diagnostics.R
#
# Purpose: Pre-regression validation of the individual panel built in 10.
#
#   Checks run for each window x balance combination:
#     A  Panel observation counts by quarter x treatment
#     B  Baseline covariate balance table
#     C  Intensive margin pre-trends (wages, hours)
#     D  Extensive margin pre-trends (employment transitions)
#     E  Transition matrix (first pre -> last post)
#     F  Sample summary counts
#     G  Tier switching direction (reads tier_switch_detail.rds from 10)
#     H  Compliance trends (reads compliance_subset.rds from 10)
#     I  Wage density pre vs post by treatment
#     J  Geographic composition check (treatment/control overlap)
#
# Output tree:
#   <outputs>/Panel Regressions/<event>/<sample_tag>/<win_tag>/<balance>/
#     Sample Evaluation/
#
# Reads from:
#   <processed>/Panel Regressions/<event>/<sample_tag>/<win_tag>/<balance>/
#     individual_panel.rds
#     tier_switch_detail.rds
#     compliance_subset.rds
#
# Pipeline: 01A -> 01B -> 02 -> 10 -> [11] -> 12
#
#===============================================================================

# Skip re-sourcing if already loaded (e.g., by the runner script)
if (!exists("config")) {
  source(here::here("Code", "R", "clean scripts", "00_setup.R"))
}

cat("=== 11_method2_diagnostics.R ===\n\n")


#===============================================================================
# STEP 0. Parameters — read directly from config (no fallbacks)
#===============================================================================

M2_EVENT   <- config$method2$events$event_2021q3_micro
M2_WINDOWS <- config$method2$windows

M2_CONTROL_BW   <- config$method2$control_bandwidth
M2_TREAT_MIN_FS <- config$method2$treatment_min_firmsize

# Build sample tag (must match script 10)
sample_tag <- M2_CONTROL_BW
if (!is.null(M2_TREAT_MIN_FS) && M2_TREAT_MIN_FS > 1) {
  sample_tag <- paste0(sample_tag, "_micro", M2_TREAT_MIN_FS, "plus")
}

# Balance mode — script 10 builds both; diagnostics runs the active one only.
# Set active_balance in config to "balanced" or "unbalanced" (or both).
BALANCE_MODES <- config$method2$active_balance

# --- Directories ---
m2_data_root <- file.path(
  config$paths$processed_data, "Panel Regressions",
  M2_EVENT$event_tag, sample_tag
)
m2_out_root <- file.path(
  config$paths$outputs, config$output_stage, "Panel Regressions",
  M2_EVENT$event_tag, sample_tag
)

# Control group label (used in subtitles)
CTRL_LABEL <- config$m2_labels$control

# Dynamic group labels reflecting actual bandwidth/firmsize restrictions
TREAT_LABEL <- config$m2_labels$treatment
CONTROL_LABEL <- config$m2_labels$control

cat(sprintf("  Event: %s | Sample: %s\n", M2_EVENT$event_tag, sample_tag))
cat(sprintf("  Labels: Treatment = %s | Control = %s\n",
            TREAT_LABEL, CONTROL_LABEL))
cat(sprintf("  Data from: %s\n", m2_data_root))
cat(sprintf("  Output to: %s\n\n", m2_out_root))

SRC <- "Source: ENCFT (Banco Central de la Rep\u00fablica Dominicana)."

# Group colours used throughout
GRP_COLS <- c("#4575b4", "#d73027")
names(GRP_COLS) <- c(CONTROL_LABEL, TREAT_LABEL)


#===============================================================================
# LOOP OVER WINDOWS x BALANCE MODES
#===============================================================================

for (win_name in names(M2_WINDOWS)) {
  
  win <- M2_WINDOWS[[win_name]]
  
  for (balance_label in BALANCE_MODES) {
    
    cat(sprintf("\n========== Window: %s | %s ==========\n\n",
                win$label, balance_label))
    
    # --- Per-window/balance directories ---
    win_data_dir <- file.path(m2_data_root, balance_label, win$tag)
    win_out_dir  <- file.path(m2_out_root, balance_label, win$tag,
                              "Sample Evaluation")
    dir.create(win_out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # save_fig scoped to this output folder
    save_fig <- function(p, name,
                         w = config$fig_defaults$width,
                         h = config$fig_defaults$height) {
      fp <- file.path(win_out_dir, paste0(name, ".", config$fig_defaults$format))
      ggsave(fp, plot = p, width = w, height = h, dpi = config$fig_defaults$dpi)
      message("  Saved: ", fp)
    }
    
    # --- Load panel ---
    panel_file <- file.path(win_data_dir, "individual_panel.rds")
    if (!file.exists(panel_file)) {
      cat(sprintf("  Panel not found: %s — skipping.\n\n", panel_file))
      next
    }
    
    panel <- readRDS(panel_file) %>%
      mutate(group_label = factor(
        ifelse(treat == 1, TREAT_LABEL, CONTROL_LABEL),
        levels = c(CONTROL_LABEL, TREAT_LABEL)
      ))
    
    if (nrow(panel) == 0) {
      cat(sprintf("  Panel is empty (0 obs) — skipping.\n\n"))
      next
    }
    
    # Subtitle used on multiple figures
    fig_sub <- sprintf("%s | %s | Control: %s",
                       win$label, balance_label, CTRL_LABEL)
    
    
    #==========================================================================
    # CHECK A: Panel balance — obs per quarter x treatment
    #==========================================================================
    
    cat("[A] Panel balance by quarter x treatment...\n")
    
    balance_qtr <- panel %>% count(year_quarter, group_label, name = "n_obs")
    print(tidyr::pivot_wider(balance_qtr, names_from = group_label,
                             values_from = n_obs, values_fill = 0))
    cat("\n")
    
    fig_balance <- ggplot(balance_qtr,
                          aes(x = year_quarter, y = n_obs, fill = group_label)) +
      geom_col(position = "dodge", alpha = 0.85) +
      geom_vline(xintercept = M2_EVENT$event_qtr, linetype = "dashed",
                 colour = "red", linewidth = 0.5) +
      scale_fill_manual(values = GRP_COLS, name = "Group") +
      labs(title = "Panel Observations per Quarter",
           subtitle = fig_sub,
           x = NULL, y = "Number of observations", caption = SRC) +
      theme_surveytools()
    
    save_fig(fig_balance, "fig_M2_balance")
    
    
    #==========================================================================
    # CHECK B: Baseline balance table
    #==========================================================================
    
    cat("[B] Baseline balance table...\n")
    
    baseline_obs <- panel %>%
      filter(period == "pre") %>%
      arrange(ID_PERSONA, year_quarter) %>%
      group_by(ID_PERSONA) %>%
      slice(1) %>%
      ungroup()
    
    balance_vars <- c(
      "EDAD", "is_female", "is_sec_complete", "is_tert_complete",
      "hours_worked_primary", "real_salary_income_wage_primary",
      "real_salary_primary_hourly_base",
      "is_informal_now", "is_formal_private"
    )
    balance_vars <- intersect(balance_vars, names(baseline_obs))
    
    wm <- function(x, w) {
      ok <- !is.na(x) & !is.na(w)
      if (sum(ok) == 0) return(NA_real_)
      sum(x[ok] * w[ok]) / sum(w[ok])
    }
    
    balance_stats <- purrr::map_dfr(balance_vars, function(v) {
      t_vals <- baseline_obs %>% filter(treat == 1) %>% pull(!!sym(v))
      c_vals <- baseline_obs %>% filter(treat == 0) %>% pull(!!sym(v))
      t_w    <- baseline_obs %>% filter(treat == 1) %>% pull(FACTOR_EXPANSION)
      c_w    <- baseline_obs %>% filter(treat == 0) %>% pull(FACTOR_EXPANSION)
      m_t <- wm(t_vals, t_w); m_c <- wm(c_vals, c_w)
      sd_pool <- sqrt((var(t_vals, na.rm = TRUE) + var(c_vals, na.rm = TRUE)) / 2)
      std_diff <- if (sd_pool > 0) (m_t - m_c) / sd_pool else NA_real_
      tibble::tibble(variable = v, mean_treat = m_t, mean_control = m_c,
                     diff = m_t - m_c, std_diff = std_diff)
    })
    
    cat("  Baseline balance:\n")
    print(as.data.frame(balance_stats %>%
                          mutate(across(where(is.numeric), ~round(., 3)))),
          row.names = FALSE)
    cat("\n")
    
    tbl_balance <- balance_stats %>%
      mutate(across(c(mean_treat, mean_control, diff), ~round(., 2)),
             std_diff = round(std_diff, 3)) %>%
      gt::gt() %>%
      gt::tab_header(
        title = sprintf("Baseline Balance (%s, %s)", win$label, balance_label),
        subtitle = sprintf("Treatment = %s | Control = %s (%s)",
                           TREAT_LABEL, CONTROL_LABEL,
                           CTRL_LABEL)
      ) %>%
      gt::cols_label(variable = "Variable",
                     mean_treat = "Treatment", mean_control = "Control",
                     diff = "Diff", std_diff = "Std. Diff.") %>%
      gt::tab_source_note(SRC)
    
    gt::gtsave(tbl_balance, file.path(win_out_dir, "tbl_M2_balance.html"))
    gt::gtsave(tbl_balance, file.path(win_out_dir, "tbl_M2_balance.png"),
               expand = 10)
    
    
    #==========================================================================
    # CHECK C: Intensive margin pre-trends
    #==========================================================================
    
    cat("[C] Pre-trend figures (intensive margin)...\n")
    
    INTENSIVE_OUTCOMES <- c(
      log_real_hwage       = "Log real hourly wage",
      log_real_mwage       = "Log real monthly wage",
      hours_worked_primary = "Weekly hours worked"
    )
    INTENSIVE_OUTCOMES <- INTENSIVE_OUTCOMES[names(INTENSIVE_OUTCOMES) %in% names(panel)]
    
    trend_intensive <- purrr::map_dfr(names(INTENSIVE_OUTCOMES), function(v) {
      panel %>%
        filter(!is.na(.data[[v]])) %>%
        group_by(year_quarter, group_label) %>%
        summarise(mean_val = weighted.mean(.data[[v]], FACTOR_EXPANSION,
                                           na.rm = TRUE),
                  n_obs = n(), .groups = "drop") %>%
        mutate(outcome = v, outcome_label = INTENSIVE_OUTCOMES[[v]])
    })
    
    if (nrow(trend_intensive) > 0) {
      fig_int <- ggplot(trend_intensive,
                        aes(x = year_quarter, y = mean_val,
                            colour = group_label, group = group_label)) +
        geom_vline(xintercept = M2_EVENT$event_qtr, linetype = "dashed",
                   colour = "red", linewidth = 0.4) +
        geom_line(linewidth = 0.7) + geom_point(size = 1.5) +
        facet_wrap(~outcome_label, scales = "free_y", ncol = 2) +
        scale_colour_manual(values = GRP_COLS, name = "Group") +
        labs(title = "Intensive Margin Trends",
             subtitle = paste(fig_sub, "| Conditional on wage data"),
             x = NULL, y = NULL, caption = SRC) +
        theme_surveytools() +
        theme(strip.text = element_text(face = "bold"))
      save_fig(fig_int, "fig_M2_pretrends_intensive",
               w = config$fig_defaults$width * 1.4,
               h = config$fig_defaults$height * 1.1)
    }
    
    
    #==========================================================================
    # CHECK D: Extensive margin pre-trends
    #==========================================================================
    
    cat("[D] Pre-trend figures (extensive margin)...\n")
    
    EXTENSIVE_OUTCOMES <- c(
      is_employed          = "Employed",
      is_private_employee  = "Private employee",
      is_selfemp_now       = "Self-employed",
      is_independent_now   = "Independent (SE+owner)",
      is_informal_now      = "Informal",
      is_formal_private    = "Formal private emp.",
      has_wage             = "Has wage data"
    )
    EXTENSIVE_OUTCOMES <- EXTENSIVE_OUTCOMES[names(EXTENSIVE_OUTCOMES) %in% names(panel)]
    
    trend_ext <- purrr::map_dfr(names(EXTENSIVE_OUTCOMES), function(v) {
      panel %>%
        group_by(year_quarter, group_label) %>%
        summarise(mean_val = weighted.mean(.data[[v]], FACTOR_EXPANSION,
                                           na.rm = TRUE),
                  n_obs = n(), .groups = "drop") %>%
        mutate(outcome = v, outcome_label = EXTENSIVE_OUTCOMES[[v]])
    })
    
    if (nrow(trend_ext) > 0) {
      fig_ext <- ggplot(trend_ext,
                        aes(x = year_quarter, y = mean_val,
                            colour = group_label, group = group_label)) +
        geom_vline(xintercept = M2_EVENT$event_qtr, linetype = "dashed",
                   colour = "red", linewidth = 0.4) +
        geom_line(linewidth = 0.7) + geom_point(size = 1.5) +
        facet_wrap(~outcome_label, scales = "free_y", ncol = 3) +
        scale_colour_manual(values = GRP_COLS, name = "Group") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = "Extensive Margin Trends",
             subtitle = paste(fig_sub, "| All panel individuals"),
             x = NULL, y = NULL, caption = SRC) +
        theme_surveytools() +
        theme(strip.text = element_text(face = "bold"))
      save_fig(fig_ext, "fig_M2_pretrends_extensive",
               w = config$fig_defaults$width * 1.6,
               h = config$fig_defaults$height * 1.4)
    }
    
    
    #==========================================================================
    # CHECK E: Transition matrix (first pre -> last post)
    #==========================================================================
    
    cat("[E] Transition summary...\n")
    
    pre_status <- panel %>%
      filter(period == "pre") %>%
      arrange(ID_PERSONA, year_quarter) %>%
      group_by(ID_PERSONA) %>% slice(1) %>% ungroup() %>%
      transmute(ID_PERSONA, treat, group_label,
                pre_employed = is_employed, pre_private = is_private_employee,
                pre_informal = is_informal_now, pre_selfemp = is_selfemp_now)
    
    post_last <- panel %>%
      filter(period == "post") %>%
      arrange(ID_PERSONA, desc(year_quarter)) %>%
      group_by(ID_PERSONA) %>% slice(1) %>% ungroup() %>%
      transmute(ID_PERSONA,
                post_employed     = is_employed,
                post_private      = is_private_employee,
                post_informal     = is_informal_now,
                post_selfemp      = is_selfemp_now,
                post_independent  = is_independent_now,
                post_has_wage     = has_wage)
    
    transitions <- pre_status %>% inner_join(post_last, by = "ID_PERSONA")
    
    trans_summary <- transitions %>%
      group_by(group_label) %>%
      summarise(n = n(),
                stayed_employed    = 100 * mean(post_employed),
                stayed_private     = 100 * mean(post_private),
                to_selfemp         = 100 * mean(post_selfemp & !pre_selfemp),
                to_informal        = 100 * mean(post_informal & !pre_informal),
                lost_wage          = 100 * mean(post_has_wage == 0),
                .groups = "drop")
    
    cat("  Transitions (first pre -> last post):\n")
    print(as.data.frame(trans_summary %>%
                          mutate(across(where(is.numeric), ~round(., 1)))),
          row.names = FALSE)
    cat("\n")
    
    
    #==========================================================================
    # CHECK F: Sample summary counts
    #==========================================================================
    
    cat("[F] Sample summary...\n")
    
    n_t <- sum(baseline_obs$treat == 1)
    n_c <- sum(baseline_obs$treat == 0)
    n_switch <- sum(
      panel %>% distinct(ID_PERSONA, .keep_all = TRUE) %>% pull(tier_switcher)
    )
    n_wage <- sum(panel$has_wage, na.rm = TRUE)
    
    cat(sprintf("  Persons: %d treatment, %d control, %d total\n",
                n_t, n_c, n_t + n_c))
    cat(sprintf("  Tier switchers: %d (%.1f%%)\n",
                n_switch, 100 * n_switch / (n_t + n_c)))
    cat(sprintf("  Obs with wage: %d (%.0f%%)\n\n",
                n_wage, 100 * n_wage / nrow(panel)))
    
    
    #==========================================================================
    # CHECK G: Tier switching direction
    #
    # Reads the person-level tier_switch_detail.rds produced by script 10.
    # Shows where treatment and control workers ended up by tier, and
    # visualises the flow as a grouped bar chart.
    #==========================================================================
    
    cat("[G] Tier switching direction...\n")
    
    ts_file <- file.path(win_data_dir, "tier_switch_detail.rds")
    if (file.exists(ts_file)) {
      
      tier_switch <- readRDS(ts_file)
      
      # Summary table: direction counts by treatment group
      ts_summary <- tier_switch %>%
        count(treat, baseline_tier, switch_direction, name = "n_persons") %>%
        group_by(treat) %>%
        mutate(pct = round(100 * n_persons / sum(n_persons), 1)) %>%
        ungroup() %>%
        arrange(treat, desc(n_persons))
      
      cat("  Tier switching flows:\n")
      print(as.data.frame(ts_summary), row.names = FALSE)
      cat("\n")
      
      # Save table
      tbl_ts <- ts_summary %>%
        mutate(group = ifelse(treat == 1, TREAT_LABEL,
                              CONTROL_LABEL)) %>%
        select(group, baseline_tier, switch_direction, n_persons, pct) %>%
        gt::gt() %>%
        gt::tab_header(
          title = sprintf("Tier Switching (%s, %s)", win$label, balance_label),
          subtitle = "Baseline tier -> last observed post-period tier"
        ) %>%
        gt::cols_label(group = "Group", baseline_tier = "Baseline",
                       switch_direction = "Direction",
                       n_persons = "N", pct = "%") %>%
        gt::tab_source_note(SRC)
      
      gt::gtsave(tbl_ts, file.path(win_out_dir, "tbl_M2_tier_switching.html"))
      gt::gtsave(tbl_ts, file.path(win_out_dir, "tbl_M2_tier_switching.png"),
                 expand = 10)
      
      # Bar chart of switching direction
      ts_plot_data <- ts_summary %>%
        mutate(group_label = factor(
          ifelse(treat == 1, TREAT_LABEL, CONTROL_LABEL),
          levels = c(CONTROL_LABEL, TREAT_LABEL)
        ))
      
      fig_ts <- ggplot(ts_plot_data,
                       aes(x = reorder(switch_direction, -pct),
                           y = pct, fill = group_label)) +
        geom_col(position = "dodge", alpha = 0.85) +
        scale_fill_manual(values = GRP_COLS, name = "Group") +
        labs(title = "Tier Switching Direction",
             subtitle = fig_sub,
             x = NULL, y = "Percent of group", caption = SRC) +
        theme_surveytools() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
      save_fig(fig_ts, "fig_M2_tier_switching",
               w = config$fig_defaults$width * 1.3)
      
    } else {
      cat("  tier_switch_detail.rds not found — skipping.\n\n")
    }
    
    
    #==========================================================================
    # CHECK H: Compliance trends (continuously-employed subset)
    #
    # Reads compliance_subset.rds from script 10: quarter-level non-compliance
    # rates for workers employed with known tier and wage in ALL quarters.
    #==========================================================================
    
    cat("[H] Compliance trends (continuous employment subset)...\n")
    
    comp_file <- file.path(win_data_dir, "compliance_subset.rds")
    if (file.exists(comp_file)) {
      
      compliance <- readRDS(comp_file)
      
      if (nrow(compliance) > 0) {
        
        comp_long <- compliance %>%
          mutate(group_label = factor(
            ifelse(treat == 1, TREAT_LABEL, CONTROL_LABEL),
            levels = c(CONTROL_LABEL, TREAT_LABEL)
          )) %>%
          tidyr::pivot_longer(
            cols = c(pct_below_hourly, pct_below_monthly),
            names_to = "measure", values_to = "pct_below"
          ) %>%
          mutate(measure_label = ifelse(
            measure == "pct_below_hourly",
            "Below hourly floor", "Below monthly floor"
          ))
        
        fig_comp <- ggplot(comp_long,
                           aes(x = year_quarter, y = pct_below / 100,
                               colour = group_label, group = group_label)) +
          geom_vline(xintercept = M2_EVENT$event_qtr, linetype = "dashed",
                     colour = "red", linewidth = 0.4) +
          geom_line(linewidth = 0.7) + geom_point(size = 1.5) +
          facet_wrap(~measure_label, ncol = 2) +
          scale_colour_manual(values = GRP_COLS, name = "Group") +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
          labs(title = "Non-Compliance Rate (Continuous Employment Subset)",
               subtitle = paste(fig_sub,
                                sprintf("| N per qtr: treat %d, ctrl %d",
                                        min(compliance$n_obs[compliance$treat == 1]),
                                        min(compliance$n_obs[compliance$treat == 0]))),
               x = NULL, y = "Share below applicable floor",
               caption = paste(
                 "Subset: employed with known tier and wage in every quarter.",
                 SRC)) +
          theme_surveytools() +
          theme(strip.text = element_text(face = "bold"))
        
        save_fig(fig_comp, "fig_M2_compliance",
                 w = config$fig_defaults$width * 1.3)
        
        cat("  Compliance rates:\n")
        print(as.data.frame(compliance %>%
                              mutate(across(where(is.numeric), ~round(., 1)))),
              row.names = FALSE)
        cat("\n")
        
      } else {
        cat("  Compliance subset is empty.\n\n")
      }
    } else {
      cat("  compliance_subset.rds not found — skipping.\n\n")
    }
    
    
    #==========================================================================
    # CHECK I: Wage distribution — pre vs post by treatment
    #
    # Overlapping density plots of log hourly wages to visualise:
    #   - Pre-period distributional overlap between treatment and control
    #   - Post-period shift (compression, bunching, spillovers)
    #==========================================================================
    
    cat("[I] Wage density pre vs post...\n")
    
    wage_data <- panel %>%
      filter(has_wage == 1, !is.na(log_real_hwage)) %>%
      mutate(period_label = factor(period, levels = c("pre", "post"),
                                   labels = c("Pre-period", "Post-period")))
    
    if (nrow(wage_data) > 50) {
      
      fig_dens <- ggplot(wage_data,
                         aes(x = log_real_hwage, colour = group_label,
                             fill = group_label)) +
        geom_density(alpha = 0.15, linewidth = 0.6) +
        facet_wrap(~period_label, ncol = 2) +
        scale_colour_manual(values = GRP_COLS, name = "Group") +
        scale_fill_manual(values = GRP_COLS, name = "Group") +
        labs(title = "Log Hourly Wage Distribution",
             subtitle = paste(fig_sub, "| Conditional on wage data"),
             x = "Log real hourly wage", y = "Density",
             caption = paste(
               "Unweighted densities for visual comparison.",
               "Shift in post-period overlap indicates treatment effect.",
               SRC)) +
        theme_surveytools() +
        theme(strip.text = element_text(face = "bold"))
      
      save_fig(fig_dens, "fig_M2_wage_density",
               w = config$fig_defaults$width * 1.4)
    }
    
    
    #==========================================================================
    # CHECK J: Geographic composition
    #
    # Treatment and control should overlap geographically; if one group is
    # concentrated in a few provinces, the DiD may confound MW effects with
    # region-specific shocks. Shows the distribution by survey region.
    #==========================================================================
    
    cat("[J] Geographic composition...\n")
    
    geo_var <- if ("GRUPO_REGION" %in% names(baseline_obs)) "GRUPO_REGION"
    else if ("DES_PROVINCIA" %in% names(baseline_obs)) "DES_PROVINCIA"
    else NULL
    
    if (!is.null(geo_var)) {
      
      geo_comp <- baseline_obs %>%
        count(group_label, !!sym(geo_var), wt = FACTOR_EXPANSION,
              name = "pop") %>%
        group_by(group_label) %>%
        mutate(share = pop / sum(pop)) %>%
        ungroup()
      
      fig_geo <- ggplot(geo_comp,
                        aes(x = reorder(!!sym(geo_var), -share),
                            y = share, fill = group_label)) +
        geom_col(position = "dodge", alpha = 0.85) +
        scale_fill_manual(values = GRP_COLS, name = "Group") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = "Geographic Distribution of Treatment vs Control",
             subtitle = paste(fig_sub, sprintf("| By %s (weighted)", geo_var)),
             x = NULL, y = "Share of group",
             caption = paste(
               "Similar distributions support the identifying assumption",
               "that groups face common regional shocks.", SRC)) +
        theme_surveytools() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      save_fig(fig_geo, "fig_M2_geographic_composition",
               w = config$fig_defaults$width * 1.5)
      
      # Print top regions
      geo_wide <- geo_comp %>%
        tidyr::pivot_wider(names_from = group_label,
                           values_from = c(pop, share), values_fill = 0) %>%
        arrange(desc(rowSums(across(starts_with("pop_")))))
      cat(sprintf("  Geographic composition by %s (weighted):\n", geo_var))
      print(as.data.frame(geo_wide %>%
                            mutate(across(starts_with("share_"),
                                          ~round(. * 100, 1)))),
            row.names = FALSE)
      cat("\n")
    } else {
      cat("  No geography variable found — skipping.\n\n")
    }
    
    
  } # end balance loop
  
} # end window loop


cat("\n=== 11_method2_diagnostics.R complete ===\n")