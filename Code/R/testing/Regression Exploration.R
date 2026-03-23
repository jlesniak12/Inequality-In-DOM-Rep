source("Code/R/00_setup.R")


library(fixest)
library(modelsummary)
library(openxlsx)

# ---- 1. Load Data---- #

panel_Sector_time <- readRDS(file.path(config$paths$processed_data, "sector_time_panel.rds")) %>%
  rename(sector = group)


sector_time_panel <- sector_time_panel %>%
  mutate(exposure_pp = exposure_baseline_val * 100)

# ---- Shift Share Model at sector level ---- #

outcomes <- c("log_90_10", "p90_p10", "log_50", "informal", "below_min")

#try annual level shock
models <- lapply(outcomes, function(y) {
  feols(
    as.formula(paste0(y, " ~ exposure_pp * shock_yoy_nom | sector + time")),
    cluster = ~sector,
    data = sector_time_panel
  )
})

etable(models)


#try quarterly shock
models <- lapply(outcomes, function(y) {
  feols(
    as.formula(paste0(y, " ~ exposure_pp * shock_qoq_nom | sector + time")),
    cluster = ~sector,
    data = sector_time_panel
  )
})

etable(models)


# ---- Shift Share Model at sector x size level ---- #


sector_size_time_panel <- readRDS(file.path(config$paths$processed_data, "sector_size_time_panel.rds"))

sector_size_time_panel <- sector_size_time_panel %>%
  mutate(exposure_pp = exposure_baseline_val * 100)


sector_size_time_panel <- sector_size_time_panel %>%
  mutate(sector_size = interaction(Employment_Sector, Wage_group, drop = TRUE))



outcomes <- c("log_90_10", "p90_p10", "log_50", "informal", "below_min")


#try quarterly shock
models <- setNames(lapply(outcomes, function(y) {
  feols(
    as.formula(paste0(y, " ~ exposure_pp * shock_qoq_nom | sector_size + time")),
    cluster = ~sector_size,
    data = sector_size_time_panel
  )}),
  outcomes
)

etable(models)





# ----- Define Functions -----# 
make_es_df <- function(model, outcome_name, exposure_delta = 10) {
  
  ct <- as.data.frame(coeftable(model)) %>%
    rownames_to_column("term") %>%
    rename(estimate = Estimate,
           se = `Std. Error`) %>%
    filter(str_detect(term, "^event_time::")) %>%
    mutate(
      event_time = as.integer(str_match(term, "^event_time::(-?\\d+):")[,2]),
      est_scaled = estimate * exposure_delta,
      se_scaled  = se * exposure_delta,
      lo_scaled  = est_scaled - 1.96 * se_scaled,
      hi_scaled  = est_scaled + 1.96 * se_scaled
    ) %>%
    arrange(event_time)
  
  # Transform depending on outcome type
  if (str_starts(outcome_name, "log_")) {
    ct <- ct %>%
      mutate(
        y  = 100 * (exp(est_scaled) - 1),
        lo = 100 * (exp(lo_scaled) - 1),
        hi = 100 * (exp(hi_scaled) - 1),
        ylab = paste0("Percent effect (", exposure_delta, "pp exposure)")
      )
  } else if (outcome_name %in% c("informal", "below_min")) {
    ct <- ct %>%
      mutate(
        y  = 100 * est_scaled,
        lo = 100 * lo_scaled,
        hi = 100 * hi_scaled,
        ylab = paste0("Percentage point effect (", exposure_delta, "pp exposure)")
      )
  } else {
    ct <- ct %>%
      mutate(
        y  = est_scaled,
        lo = lo_scaled,
        hi = hi_scaled,
        ylab = paste0("Level effect (", exposure_delta, "pp exposure)")
      )
  }
  
  ct
}

make_es_plot <- function(df_plot, outcome_name) {
  
  ggplot(df_plot, aes(x = event_time, y = y)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15) +
    geom_point(size = 2) +
    geom_line() +
    labs(
      x = "Event time (quarters relative to reform)",
      y = unique(df_plot$ylab),
      title = paste0("Pooled stacked event study: ", outcome_name)
    ) +
    theme_minimal()
}


make_overlay_df_for_outcome <- function(dat, outcome, exposure_delta = 10) {
  
  mods <- dat %>%
    split(.$stack_id) %>%
    purrr::map(~ fixest::feols(
      as.formula(paste0(
        outcome, " ~ i(event_time, exposure_pp, ref = -1) | stack_sector_fe + stack_time_fe"
      )),
      cluster = ~stack_sector_fe,
      data = .x
    ))
  
  purrr::imap_dfr(mods, function(m, event) {
    make_es_df(m, outcome_name = outcome, exposure_delta = exposure_delta) %>%
      dplyr::mutate(stack_id = event, outcome = outcome)
  })
}




overlay_plot <- function(overlay_df) {
  ggplot(
    overlay_df,
    aes(x = event_time, y = y, color = stack_id, fill = stack_id, group = stack_id)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    labs(
      x = "Event time (quarters)",
      y = unique(overlay_df$ylab),
      title = paste0("Event-study by reform episode: ", unique(overlay_df$outcome))
    ) +
    theme_minimal() +
    theme(legend.title = element_blank())
}

facet_plot <- function(overlay_df) {
  ggplot(
    overlay_df,
    aes(x = event_time, y = y)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    facet_wrap(~ stack_id, nrow = 2) +
    labs(
      x = "Event time (quarters)",
      y = unique(overlay_df$ylab),
      title = paste0("Event-study by reform episode: ", unique(overlay_df$outcome))
    ) +
    theme_minimal()
}


write_es_blocks_sheet <- function(wb, sheet, es_df, drop_stack = NULL, start_row = 1) {
  if (!is.null(drop_stack)) {
    es_df <- dplyr::filter(es_df, !stack_id %in% drop_stack)
  }
  
  addWorksheet(wb, sheet)
  
  # basic styles
  titleStyle  <- createStyle(textDecoration = "bold", fontSize = 12)
  headerStyle <- createStyle(textDecoration = "bold", border = "Bottom")
  numStyle    <- createStyle(numFmt = "0.000")  # adjust if you want more/less
  intStyle    <- createStyle(numFmt = "0")
  
  r <- start_row
  stacks <- unique(es_df$stack_id)
  
  for (sid in stacks) {
    block <- es_df |>
      dplyr::filter(stack_id == sid) |>
      dplyr::arrange(event_time) |>
      dplyr::select(event_time, y, lo, hi)
    
    # Title row for the block
    writeData(wb, sheet, x = paste0("Event: ", sid), startRow = r, startCol = 1)
    addStyle(wb, sheet, titleStyle, rows = r, cols = 1, gridExpand = TRUE, stack = TRUE)
    r <- r + 1
    
    # write table
    writeData(wb, sheet, x = block, startRow = r, startCol = 1, withFilter = FALSE)
    addStyle(wb, sheet, headerStyle, rows = r, cols = 1:ncol(block), gridExpand = TRUE, stack = TRUE)
    
    # style columns
    addStyle(wb, sheet, intStyle, rows = (r+1):(r+nrow(block)), cols = 1, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, sheet, numStyle, rows = (r+1):(r+nrow(block)), cols = 2:ncol(block), gridExpand = TRUE, stack = TRUE)
    
    # blank space between blocks
    r <- r + nrow(block) + 2
  }
  
  # column widths
  setColWidths(wb, sheet, cols = 1:4, widths = c(12, 14, 14, 14))
}






# ---- Stacked model at Sector X Firm Size Level ---- #

stacked_panel_sector_size_time <- readRDS(file.path(config$paths$processed_data, "stacked_panel_sector_size_time.rds"))

stacked_panel_sector_size_time <- stacked_panel_sector_size_time %>%
  mutate(exposure_pp = exposure_baseline_val * 100)


# ---- Model stacked panel pooled averages ---- #

save_path <- file.path(config$paths$outputs, config$output_stage, "Regression", "Sector x size", "Pooled")

outcomes <- c("log_90_10", "log_50_10", "log_50", "log_10", "informal", "below_min")

#estimate models (pooled across all stack_id)
models_pooled <- setNames(
  map(outcomes, ~ feols(
    as.formula(paste0(.x, " ~ i(event_time, exposure_pp, ref = -1) | stack_sector_fe + stack_time_fe")),
    cluster = ~stack_sector_fe,
    data = stacked_panel_sector_size_time
  )),
  outcomes
)

etable(models_pooled)





modelsummary(
  models_pooled,
  stars = TRUE,
  estimate  = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  gof_map = c("nobs", "r.squared"),
  output = file.path(save_path, "Pooled_Event_Study.xlsx")
)




plot_dfs <- imap(models_pooled, ~make_es_df(.x, .y, exposure_delta = 10))
plots_pooled <- imap(plot_dfs, ~make_es_plot(.x, .y))


#save plots
save_plots_dir(plots_pooled, dir = save_path )



# ---- Model stacked panel seperate events ---- #
save_path <- file.path(config$paths$outputs, config$output_stage, "Regression", "Sector x size", "Individual Events")
outcomes <- c("log_90_10", "log_50_10", "log_50", "log_10", "informal", "below_min")


overlay_dfs <- setNames(
  purrr::map(outcomes, ~ make_overlay_df_for_outcome(stacked_panel_sector_size_time, .x, exposure_delta = 10)),
  outcomes
)

# drop the 2015 stack from every outcome df
overlay_dfs_no2015 <- purrr::map(
  overlay_dfs,
  ~ dplyr::filter(.x, stack_id != "2015")
)

# plots facet
overlay_plots <- purrr::map(overlay_dfs_no2015, facet_plot)


# save outputs
save_plots_dir(overlay_plots, dir = file.path(save_path ))



format_es_table <- function(model, exposure_term = "exposure_pp") {
  ct <- as.data.frame(coeftable(model)) |>
    rownames_to_column("term") |>
    rename(
      estimate = Estimate,
      se       = `Std. Error`,
      p        = `Pr(>|t|)`
    ) |>
    filter(str_detect(term, "^event_time::")) |>
    mutate(
      event_time = as.integer(str_match(term, "^event_time::(-?\\d+):")[, 2]),
      stars = case_when(
        p < 0.01 ~ "***",
        p < 0.05 ~ "**",
        p < 0.10 ~ "*",
        TRUE ~ ""
      ),
      est_star = sprintf("%.3f%s", estimate, stars),
      se_paren = sprintf("(%.3f)", se)
    ) |>
    arrange(event_time) |>
    select(event_time, est_star, se_paren, estimate, se, p, term)
  
  ct
}

  write_es_workbook <- function(all_models, out_file, drop_stack = NULL) {
    wb <- createWorkbook()
    
    titleStyle  <- createStyle(textDecoration = "bold", fontSize = 12)
    headerStyle <- createStyle(textDecoration = "bold", border = "Bottom")
    
    iwalk(all_models, function(models_by_stack, outcome) {
      
      sheet <- substr(gsub("[\\[\\]\\*\\?/\\\\:]", "_", outcome), 1, 31)
      addWorksheet(wb, sheet)
      
      r <- 1
      stacks <- names(models_by_stack)
      if (!is.null(drop_stack)) stacks <- setdiff(stacks, drop_stack)
      
      for (sid in stacks) {
        tab <- format_es_table(models_by_stack[[sid]]) |>
          select(event_time, est_star, se_paren)
        
        # block title
        writeData(wb, sheet, x = paste0("Event: ", sid), startRow = r, startCol = 1)
        addStyle(wb, sheet, titleStyle, rows = r, cols = 1, stack = TRUE)
        r <- r + 1
        
        # table
        writeData(wb, sheet, x = tab, startRow = r, startCol = 1)
        addStyle(wb, sheet, headerStyle, rows = r, cols = 1:ncol(tab), gridExpand = TRUE, stack = TRUE)
        
        r <- r + nrow(tab) + 2
      }
    })
    
    saveWorkbook(wb, out_file, overwrite = TRUE)
  }

  run_models_for_outcome <- function(dat, outcome) {
    split(dat, dat$stack_id) |>
      purrr::map(~ fixest::feols(
        as.formula(paste0(
          outcome, " ~ i(event_time, exposure_pp, ref = -1) | stack_sector_fe + stack_time_fe"
        )),
        cluster = ~ stack_sector_fe,
        data = .x
      ))
  }
  
  all_models <- setNames(
    purrr::map(outcomes, ~ run_models_for_outcome(stacked_panel_sector_size_time, .x)),
    outcomes
  )
  
  
  library(modelsummary)
  library(openxlsx)
  
  # optional: make sheet names Excel-safe
  sanitize_sheet <- function(x) {
    x <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", x)
    substr(x, 1, 31)
  }
  
  stars = c(
    "*"  = 0.10,
    "**"  = 0.05,
    "***" = 0.01
  )
  
  export_individual_es_tables <- function(all_models, wb_file) {
    
    wb <- createWorkbook()
    
    for (outcome in names(all_models)) {
      
      tab <- modelsummary(
        all_models[[outcome]],
        stars = stars,
        estimate  = "{estimate}{stars} ({std.error})",
        statistic = NULL,
        gof_map   = c("nobs", "r.squared"),
        output    = "data.frame"
      )
      
      sh <- sanitize_sheet(outcome)
      addWorksheet(wb, sh)
      
      # write the table; modelsummary returns a "data.frame" with rownames in the first col sometimes
      writeData(wb, sh, tab, startRow = 1, startCol = 1, rowNames = FALSE)
      
      
      # Add legend 2 rows below table
      legend_row <- nrow(tab) + 3
      
      legend_text <- "Notes: * p < 0.10, ** p < 0.05, *** p < 0.01"
      
      writeData(wb, sh, legend_text, startRow = legend_row, startCol = 1)
      
      # optional formatting
      setColWidths(wb, sh, cols = 1:ncol(tab), widths = "auto")
      
      
      
    }
    
    saveWorkbook(wb, wb_file, overwrite = TRUE)
  }
  
  # run it
  wb_file <- file.path(save_path, "Individual_Event_Study_Tables.xlsx")
  export_individual_es_tables(all_models, wb_file)
  
  
  
  write_es_workbook(
    all_models,
    out_file = file.path(save_path, "Individual_Event_Study_Tables1.xlsx"),
    drop_stack = NULL
  )
  
  
  
  
# 1) Run and store all models (outcome x stack_id)
run_models_for_outcome <- function(dat, outcome) {
  dat %>%
    split(.$stack_id) %>%
    purrr::map(~ fixest::feols(
      as.formula(paste0(outcome, " ~ i(event_time, exposure_pp, ref = -1) | stack_sector_fe + stack_time_fe")),
      cluster = ~stack_sector_fe,
      data = .x
    ))
}

all_models <- setNames(
  purrr::imap(outcomes, ~ run_models_for_outcome(stacked_panel_sector_size_time, .x)),
  outcomes)




# all_models[[outcome_name]][[stack_id]] is a fixest model

# 2) Tidy into a single table (coefficients + SE)
coef_table <- purrr::imap_dfr(all_models, function(mods_by_stack, outcome_name) {
  purrr::imap_dfr(mods_by_stack, function(m, stack_id) {
    broom::tidy(m) %>%
      dplyr::mutate(outcome = outcome_name, stack_id = stack_id, .before = 1)
  })
})























################################################################
m_log50 <- feols(
  log_50 ~ i(event_time, exposure_pp, ref = -1) |
    stack_sector_fe + stack_time_fe,
  cluster = ~stack_sector_fe,
  data = stacked_panel_sector_size_time
)

# Event-study plot (coefficients + CI)
iplot(
  m_log50,
  xlab = "Event time (quarters relative to reform)",
  ylab = "Effect per 1pp baseline exposure on log(p50)",
  ref.line = 0,
  main = "Stacked event study: MW exposure × event time"
)


m_log50 <- feols(
  log_50 ~ i(event_time, exposure_pp, ref = -1) |
    stack_sector_fe + stack_time_fe,
  cluster = ~stack_sector_fe,
  data = stacked_panel_sector_size_time
)

# Event-study plot (coefficients + CI)
iplot(
  m_log50,
  xlab = "Event time (quarters relative to reform)",
  ylab = "Effect per 1pp baseline exposure on log(p50)",
  ref.line = 0,
  main = "Stacked event study: MW exposure × event time"
)





models_by_event <- stacked_panel_sector_size_time %>%
  split(.$stack_id) %>%
  map(~ feols(
    log_50 ~ i(event_time, exposure_pp, ref = -1) |
      stack_sector_fe + stack_time_fe,
    cluster = ~stack_sector_fe,
    data = .x
  ))

names(models_by_event)


etable(models_by_event)


summary(quarter_panel$shock_qoq)
summary(quarter_panel$exposure_pp)
summary(quarter_panel$exposure_pp * quarter_panel$shock_qoq)







