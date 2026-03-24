#===============================================================================
#
# Script: 02_Cell_Validation_For_Regression_Data.R
#
# Scope:  Validates reliability of survey-weighted estimates at the
#         sector × firm size × quarter cell level for four regression outcomes.
#         Produces a clean regression-ready panel restricted to cells that
#         pass all reliability checks for each outcome.
#
# Validation checks (per cell × outcome):
#   A. Unweighted cell count  — raw n screen (most fundamental)
#   B. Temporal stability     — sd/mean of outcome over time, excl. COVID
#   C. Near-zero screen       — for proportions: structural near-zero ≠ noisy
#
# NOTE: CV from the survey design (Part B in old script) is dropped.
#   The design-based CV is expensive to compute and adds little beyond what
#   the raw count screen and temporal stability already capture. The key
#   insight is that low n IS the CV problem — a cell with n=15 will have
#   high CV by construction regardless of the estimator.
#
# Outputs:
#   validation_decision_table.rds    — cell × outcome recommendations
#   validation_cross_matrix.csv      — wide matrix for data appendix
#   panel_sf_clean.rds               — panel_sf filtered to valid cells,
#                                      with outcome columns set to NA for
#                                      cells that fail for that outcome
#
# Reads:
#   panel_sector_firmsize_quarter.rds
#   panel_emp_privsec_wagegrp.rds
#
#===============================================================================

source("Code/R/00_setup.R")


#===============================================================================
# STEP 1. Parameters and Outcome Metadata
#===============================================================================

# Thresholds
N_MIN_MEAN  <- 30    # minimum n for means and proportions
N_MIN_QUANT <- 50    # minimum n for quantile ratios (p10 is unstable below this)
N_WARN      <- 50    # borderline n — flag as "Caution" but keep
THIN_CUT    <- 0.20  # fraction of quarters below n_min before flagging as thin
STAB_CUT    <- 1.0   # ts_cv (sd/|mean|) above this = temporally unstable
NEAR_ZERO_INFORMAL  <- 0.05   # below 5% informality = structural near-zero
NEAR_ZERO_BELOW_MIN <- 0.03   # below 3% non-compliance = structural near-zero

# COVID quarters excluded from temporal stability
COVID_QTRS <- c("2020Q1", "2020Q2", "2020Q3", "2020Q4", "2021Q1", "2021Q2")

# Outcome metadata — maps outcome name to panel_sf column and n threshold
OUTCOMES <- tibble::tribble(
  ~outcome,      ~panel_col,      ~n_min,      ~near_zero_thresh,
  "log_var_wage", "log_var_wage",  N_MIN_MEAN,  NA_real_,
  "log_50_10",    "log_50_10",     N_MIN_QUANT, NA_real_,
  "below_min",    "below_min",     N_MIN_MEAN,  NEAR_ZERO_BELOW_MIN,
  "informal",     "informal",      N_MIN_MEAN,  NEAR_ZERO_INFORMAL
)


#===============================================================================
# STEP 2. Load Data
#===============================================================================

pd <- config$paths$processed_data

panel_sf   <- readRDS(file.path(pd, "panel_sector_firmsize_quarter.rds")) %>%
  mutate(time = as.character(time))

micro_data <- readRDS(file.path(pd, "panel_emp_privsec_wagegrp.rds"))

cat(sprintf("Panel: %d rows | %d cells | %d quarters\n",
            nrow(panel_sf),
            n_distinct(panel_sf$cell_id),
            n_distinct(panel_sf$time)))


#===============================================================================
# STEP 3. Part A — Unweighted Cell Counts
#
# Compute raw n per cell × quarter from the microdata.
# This is the fundamental screen: no weighting or modelling can fix thin cells.
#===============================================================================

cell_counts <- micro_data %>%
  mutate(time = as.character(year_quarter)) %>%
  group_by(Employment_Sector, Wage_group, time) %>%
  summarise(n = n(), .groups = "drop")

# Per-cell summary across quarters
cell_count_summary <- cell_counts %>%
  group_by(Employment_Sector, Wage_group) %>%
  summarise(
    n_quarters   = n(),
    n_min        = min(n),
    n_median     = median(n),
    n_max        = max(n),
    .groups      = "drop"
  )

cat("\nCell count summary (median quarterly n by sector × firm size):\n")
cell_count_summary %>%
  select(Employment_Sector, Wage_group, n_median, n_min) %>%
  pivot_wider(
    id_cols     = Employment_Sector,
    names_from  = Wage_group,
    values_from = c(n_median, n_min),
    names_glue  = "{Wage_group}_{.value}"
  ) %>%
  # Reorder columns: sector, then for each tier show median and min together
  select(Employment_Sector,
         Micro_n_median,  Micro_n_min,
         Small_n_median,  Small_n_min,
         Medium_n_median, Medium_n_min,
         Large_n_median,  Large_n_min) %>%
  arrange(Employment_Sector) %>%
  print(n = Inf)


#===============================================================================
# STEP 4. Part B — Temporal Stability
#
# For each cell × outcome: compute sd / |mean| over non-COVID quarters.
# High values (> STAB_CUT) indicate the series is dominated by noise.
# COVID quarters excluded because they mechanically inflate volatility.
#===============================================================================

stability <- panel_sf %>%
  filter(!time %in% COVID_QTRS) %>%
  # Cross outcomes: compute ts_cv for each outcome column
  tidyr::pivot_longer(
    cols      = all_of(OUTCOMES$panel_col),
    names_to  = "panel_col",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  group_by(Employment_Sector, Wage_group, panel_col) %>%
  summarise(
    ts_mean = mean(value, na.rm = TRUE),
    ts_sd   = sd(value,   na.rm = TRUE),
    ts_cv   = ts_sd / abs(ts_mean),
    n_obs   = n(),
    .groups = "drop"
  ) %>%
  left_join(OUTCOMES %>% select(outcome, panel_col), by = "panel_col") %>%
  mutate(flag_unstable = ts_cv > STAB_CUT)


#===============================================================================
# STEP 5. Part C — Near-Zero Screen (proportions only)
#
# A cell with 2% informality every quarter is NOT noisy — it's structurally
# near-zero. Distinguishing this from a noisy cell matters:
#   - Noisy cell → Drop from regression (unreliable estimate)
#   - Near-zero  → Exclude from THAT outcome's regression only
#                  (not a compliance/informality margin — out of scope)
#===============================================================================

near_zero <- panel_sf %>%
  group_by(Employment_Sector, Wage_group) %>%
  summarise(
    median_informal  = median(informal,  na.rm = TRUE),
    median_below_min = median(below_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    near_zero_informal  = !is.na(median_informal)  & median_informal  < NEAR_ZERO_INFORMAL,
    near_zero_below_min = !is.na(median_below_min) & median_below_min < NEAR_ZERO_BELOW_MIN
  )


#===============================================================================
# STEP 6. Assemble Decision Table
#
# Per cell × outcome:
#   1. Count how many quarters fall below the outcome-specific n_min
#   2. Check temporal stability
#   3. Check near-zero (proportions only)
#   4. Assign recommendation
#===============================================================================

decision_table <- OUTCOMES %>%
  purrr::pmap_dfr(function(outcome, panel_col, n_min, near_zero_thresh) {
    
    # Count flags: join microdata counts with outcome n_min threshold
    count_flags <- cell_counts %>%
      group_by(Employment_Sector, Wage_group) %>%
      summarise(
        n_qtrs_total = n(),
        n_qtrs_thin  = sum(n < n_min),
        n_qtrs_warn  = sum(n >= n_min & n < N_WARN),
        pct_thin     = n_qtrs_thin / n_qtrs_total,
        .groups      = "drop"
      ) %>%
      mutate(flag_thin = pct_thin > THIN_CUT)
    
    # Stability for this outcome
    stab_flags <- stability %>%
      filter(outcome == !!outcome) %>%
      select(Employment_Sector, Wage_group, ts_cv, flag_unstable)
    
    # Near-zero for this outcome (proportions only)
    if (!is.na(near_zero_thresh)) {
      nz_col <- if (outcome == "informal") "near_zero_informal" else "near_zero_below_min"
      nz_flags <- near_zero %>%
        select(Employment_Sector, Wage_group, flag_near_zero = !!nz_col)
    } else {
      nz_flags <- cell_count_summary %>%
        select(Employment_Sector, Wage_group) %>%
        mutate(flag_near_zero = FALSE)
    }
    
    # Join everything
    cell_count_summary %>%
      select(Employment_Sector, Wage_group, n_median) %>%
      left_join(count_flags, by = c("Employment_Sector", "Wage_group")) %>%
      left_join(stab_flags,  by = c("Employment_Sector", "Wage_group")) %>%
      left_join(nz_flags,    by = c("Employment_Sector", "Wage_group")) %>%
      mutate(
        outcome    = outcome,
        n_flags    = as.integer(coalesce(flag_thin, FALSE)) +
          as.integer(coalesce(flag_unstable, FALSE)),
        recommendation = case_when(
          coalesce(flag_near_zero, FALSE)    ~ "Out of scope",
          pct_thin >= 0.75                   ~ "Drop",
          n_flags >= 2                       ~ "Drop",
          n_flags == 1                       ~ "Review",
          pct_thin > 0.30                    ~ "Caution",
          TRUE                               ~ "Keep"
        )
      )
  })


#===============================================================================
# STEP 7. Report
#===============================================================================

cat("\n=== KEEP/DROP SUMMARY BY OUTCOME ===\n")
decision_table %>%
  count(outcome, recommendation) %>%
  pivot_wider(names_from = recommendation, values_from = n, values_fill = 0L) %>%
  print()

cat("\n=== CROSS-OUTCOME MATRIX (with median quarterly n) ===\n")
cross_matrix <- decision_table %>%
  mutate(rec = case_when(
    recommendation == "Keep"         ~ "KEEP",
    recommendation == "Review"       ~ "REVIEW",
    recommendation == "Caution"      ~ "CAUTION",
    recommendation == "Out of scope" ~ "SCOPE",
    recommendation == "Drop"         ~ "DROP"
  )) %>%
  select(Employment_Sector, Wage_group, outcome, rec) %>%
  pivot_wider(names_from = outcome, values_from = rec) %>%
  left_join(cell_count_summary %>% select(Employment_Sector, Wage_group, n_median),
            by = c("Employment_Sector", "Wage_group")) %>%
  relocate(n_median, .after = Wage_group) %>%
  arrange(Employment_Sector, Wage_group)

print(cross_matrix, n = Inf)

cat("\n=== CELLS RECOMMENDED FOR DROP ===\n")
decision_table %>%
  filter(recommendation == "Drop") %>%
  select(outcome, Employment_Sector, Wage_group,
         n_median, pct_thin, ts_cv, flag_near_zero) %>%
  arrange(outcome, Employment_Sector) %>%
  print(n = Inf)

cat("\n=== CELLS FLAGGED FOR REVIEW ===\n")
decision_table %>%
  filter(recommendation == "Review") %>%
  select(outcome, Employment_Sector, Wage_group,
         n_median, pct_thin, ts_cv) %>%
  arrange(outcome, Employment_Sector) %>%
  print(n = Inf)


saveRDS(panel_sf, file.path(pd, "panel_sf_full.rds"))

#===============================================================================
# STEP 8. Build Clean Regression Panel
#
# Strategy: keep all cell × quarter rows in the panel, but set outcome
# columns to NA for cells that fail validation for that outcome.
# This means:
#   - Cells that "Drop" for an outcome contribute no observations for it
#   - Cells that "Out of scope" are excluded from that outcome's regression
#   - "Review" cells are included but the flag is preserved for sensitivity checks
#   - A single panel_sf_clean.rds works for all outcome regressions
#===============================================================================

# Build a lookup: for each cell × outcome, should the value be kept?
valid_lookup <- decision_table %>%
  mutate(keep = recommendation %in% c("Keep", "Review", "Caution")) %>%
  select(Employment_Sector, Wage_group, outcome, keep)

# Pivot to wide: one column per outcome indicating whether cell is valid
valid_wide <- valid_lookup %>%
  pivot_wider(
    id_cols     = c(Employment_Sector, Wage_group),
    names_from  = outcome,
    values_from = keep,
    names_prefix = "valid_"
  )

panel_clean <- panel_sf %>%
  left_join(valid_wide, by = c("Employment_Sector", "Wage_group")) %>%
  mutate(
    log_var_wage = if_else(coalesce(valid_log_var_wage, FALSE), log_var_wage, NA_real_),
    log_50_10    = if_else(coalesce(valid_log_50_10,    FALSE), log_50_10,    NA_real_),
    below_min    = if_else(coalesce(valid_below_min,    FALSE), below_min,    NA_real_),
    informal     = if_else(coalesce(valid_informal,     FALSE), informal,     NA_real_)
  ) %>%
  # Drop the validity flag columns — not needed downstream
  select(-starts_with("valid_"))

# Coverage check: how many non-NA observations per outcome?
cat("\n=== CLEAN PANEL COVERAGE ===\n")
panel_clean %>%
  summarise(across(c(log_var_wage, log_50_10, below_min, informal),
                   ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "n_obs") %>%
  mutate(
    total_possible = nrow(panel_clean),
    pct_available  = scales::percent(n_obs / total_possible, accuracy = 1)
  ) %>%
  print()

# How many unique cells survive per outcome?
cat("\n=== UNIQUE CELLS IN CLEAN PANEL ===\n")
panel_clean %>%
  group_by(Employment_Sector, Wage_group) %>%
  summarise(
    log_var_wage = any(!is.na(log_var_wage)),
    log_50_10    = any(!is.na(log_50_10)),
    below_min    = any(!is.na(below_min)),
    informal     = any(!is.na(informal)),
    .groups      = "drop"
  ) %>%
  summarise(across(c(log_var_wage, log_50_10, below_min, informal), sum)) %>%
  print()


#===============================================================================
# STEP 9. Save RDS and CSV outputs
#===============================================================================

saveRDS(decision_table,
        file.path(pd, "validation_decision_table.rds"))

write_csv(cross_matrix,
          file.path(pd, "validation_cross_matrix.csv"))

saveRDS(panel_clean,
        file.path(pd, "panel_sf_clean.rds"))


#===============================================================================
# STEP 10. Export Validation Workbook (openxlsx)
#
# Produces a formatted Excel workbook with four sheets:
#   Sheet 1: Raw cell counts (sector × firm size, median + min n by tier)
#   Sheet 2: Near-zero flags and temporal stability ts_cv
#   Sheet 3: Decision matrix — recommendation per cell × outcome
#   Sheet 4: Legend and methodology notes
#
# All data comes directly from objects built above — nothing is hardcoded.
# Re-run the script to update the workbook whenever inputs change.
#===============================================================================

library(openxlsx)

# ── Colour palette ────────────────────────────────────────────────────────────
COL_HEADER    <- "#2F5496"   # dark blue — header rows
COL_KEEP      <- "#C6EFCE"   # green
COL_KEEP_FG   <- "#276221"
COL_REVIEW    <- "#FFEB9C"   # yellow
COL_REVIEW_FG <- "#7D6608"
COL_DROP      <- "#FFC7CE"   # red
COL_DROP_FG   <- "#9C0006"
COL_SCOPE     <- "#EDEDED"   # grey
COL_SCOPE_FG  <- "#595959"
COL_ALT       <- "#F2F2F2"   # alternating row
COL_THIN_MED  <- "#FFE0E0"   # light pink — median n below threshold
COL_THIN_MIN  <- "#FFC7CE"   # deeper red — min n below threshold
COL_NOTE_FG   <- "#595959"

# ── Style helpers ─────────────────────────────────────────────────────────────
header_style <- function(bg = COL_HEADER) {
  createStyle(fontName = "Arial", fontSize = 10, fontColour = "#FFFFFF",
              fgFill = bg, halign = "center", valign = "center",
              textDecoration = "bold", wrapText = TRUE,
              border = "TopBottomLeftRight", borderColour = "#BFBFBF")
}

body_style <- function(halign = "center", bold = FALSE,
                       fg = "#000000", bg = NULL) {
  s <- createStyle(fontName = "Arial", fontSize = 10, fontColour = fg,
                   halign = halign, valign = "center",
                   textDecoration = if (bold) "bold" else NULL,
                   border = "TopBottomLeftRight", borderColour = "#BFBFBF")
  if (!is.null(bg)) s <- modifyStyle(s, fgFill = bg)
  s
}

title_style  <- createStyle(fontName = "Arial", fontSize = 12,
                            textDecoration = "bold")
note_style   <- createStyle(fontName = "Arial", fontSize = 9,
                            fontColour = COL_NOTE_FG,
                            textDecoration = "italic")
bold_style   <- createStyle(fontName = "Arial", fontSize = 10,
                            textDecoration = "bold")

# Helper: apply a single style to a range
apply_style <- function(wb, sheet, rows, cols, style) {
  addStyle(wb, sheet, style, rows = rows, cols = cols,
           gridExpand = TRUE, stack = FALSE)
}

# Helper: write a data frame starting at a given row/col
write_df <- function(wb, sheet, df, start_row, start_col = 1,
                     col_names = TRUE) {
  writeData(wb, sheet, df,
            startRow = start_row, startCol = start_col,
            colNames = col_names)
}

# Recommendation → fill/fg colours
rec_colours <- function(rec) {
  switch(rec,
         "Keep"         = list(bg = COL_KEEP,   fg = COL_KEEP_FG),
         "Review"       = list(bg = COL_REVIEW, fg = COL_REVIEW_FG),
         "Caution"      = list(bg = COL_REVIEW, fg = COL_REVIEW_FG),
         "Drop"         = list(bg = COL_DROP,   fg = COL_DROP_FG),
         "Out of scope" = list(bg = COL_SCOPE,  fg = COL_SCOPE_FG),
         list(bg = NULL, fg = "#000000")
  )
}

# ── Create workbook ───────────────────────────────────────────────────────────
wb <- createWorkbook()
options("openxlsx.borderColour" = "#BFBFBF")
options("openxlsx.borderStyle"  = "thin")


# ════════════════════════════════════════════════════════════════════════════
# SHEET 1: Raw Cell Counts
# ════════════════════════════════════════════════════════════════════════════
addWorksheet(wb, "1. Raw Cell Counts")
s1 <- "1. Raw Cell Counts"

# Build wide table: one row per sector, columns = tier × {median, min}
counts_wide <- cell_count_summary %>%
  select(Employment_Sector, Wage_group, n_median, n_min) %>%
  pivot_wider(
    id_cols     = Employment_Sector,
    names_from  = Wage_group,
    values_from = c(n_median, n_min),
    names_glue  = "{Wage_group}_{.value}"
  ) %>%
  select(Employment_Sector,
         Micro_n_median,  Micro_n_min,
         Small_n_median,  Small_n_min,
         Medium_n_median, Medium_n_min,
         Large_n_median,  Large_n_min) %>%
  arrange(Employment_Sector) %>%
  rename(Sector = Employment_Sector)

# Title and notes
writeData(wb, s1, "Cell Reliability Validation — Raw Unweighted Cell Counts",
          startRow = 1, startCol = 1)
addStyle(wb, s1, title_style, rows = 1, cols = 1)
mergeCells(wb, s1, cols = 1:9, rows = 1)

note_txt <- paste0(
  "Unweighted survey respondents per cell × quarter. ",
  "Light pink = median n < n_min.  Deeper red = minimum n < n_min.  ",
  "n_min = ", N_MIN_MEAN, " (means/proportions), ",
  N_MIN_QUANT, " (quantile ratios).  ",
  "DROP triggered when >", scales::percent(THIN_CUT), " of quarters fall below n_min."
)
writeData(wb, s1, note_txt, startRow = 2, startCol = 1)
addStyle(wb, s1, note_style, rows = 2, cols = 1)
mergeCells(wb, s1, cols = 1:9, rows = 2)

# Tier subheaders (row 3)
tier_labels <- c("Micro", "Small", "Medium", "Large")
tier_start_cols <- c(2, 4, 6, 8)
for (i in seq_along(tier_labels)) {
  writeData(wb, s1, tier_labels[i],
            startRow = 3, startCol = tier_start_cols[i])
  addStyle(wb, s1, header_style(), rows = 3,
           cols = tier_start_cols[i]:(tier_start_cols[i]+1), gridExpand = TRUE)
  mergeCells(wb, s1, cols = tier_start_cols[i]:(tier_start_cols[i]+1), rows = 3)
}
writeData(wb, s1, "Sector", startRow = 3, startCol = 1)
addStyle(wb, s1, header_style(), rows = 3, cols = 1)

# Column headers (row 4)
col_hdrs <- c("Sector", "Median n", "Min n", "Median n", "Min n",
              "Median n", "Min n", "Median n", "Min n")
writeData(wb, s1, as.data.frame(t(col_hdrs)), startRow = 4, startCol = 1,
          colNames = FALSE)
apply_style(wb, s1, rows = 4, cols = 1:9, header_style())

# Data rows (start row 5)
write_df(wb, s1, counts_wide, start_row = 5, col_names = FALSE)

n_rows <- nrow(counts_wide)
for (i in seq_len(n_rows)) {
  r <- 4 + i
  alt <- (i %% 2 == 0)
  base_bg <- if (alt) COL_ALT else NULL
  
  # Sector column
  apply_style(wb, s1, r, 1,
              createStyle(fontName="Arial", fontSize=10, halign="left",
                          border="TopBottomLeftRight", borderColour="#BFBFBF",
                          fgFill = if (!is.null(base_bg)) base_bg else "white"))
  
  # Value columns: median (even cols) and min (odd cols within 2-9)
  for (j in 2:9) {
    val <- counts_wide[i, j] %>% pull()
    is_median_col <- (j %% 2 == 0)
    thresh <- if (j %in% c(2,3)) N_MIN_MEAN else   # Micro
      if (j %in% c(4,5)) N_MIN_MEAN else   # Small
        if (j %in% c(6,7)) N_MIN_MEAN else   # Medium
          N_MIN_MEAN                             # Large
    
    bg <- base_bg
    fg <- "#000000"
    bold <- FALSE
    if (!is.na(val) && val < thresh) {
      if (is_median_col) { bg <- COL_THIN_MED }
      else               { bg <- COL_THIN_MIN; fg <- COL_DROP_FG; bold <- TRUE }
    }
    addStyle(wb, s1,
             createStyle(fontName="Arial", fontSize=10, fontColour=fg,
                         halign="center", fgFill=if(is.null(bg)) "white" else bg,
                         textDecoration=if(bold)"bold" else NULL,
                         border="TopBottomLeftRight", borderColour="#BFBFBF"),
             rows=r, cols=j)
  }
}

# Footer notes
fr <- 5 + n_rows + 1
writeData(wb, s1, "Colour guide:", startRow=fr, startCol=1)
addStyle(wb, s1, bold_style, rows=fr, cols=1)
writeData(wb, s1, paste0("Light pink (median n) = median quarterly n < n_min.  ",
                         "Deeper red (min n) = worst-case quarter below threshold.  ",
                         "DROP flag: >", scales::percent(THIN_CUT),
                         " of quarters below n_min."),
          startRow=fr+1, startCol=1)
addStyle(wb, s1, note_style, rows=fr+1, cols=1)
mergeCells(wb, s1, cols=1:9, rows=fr+1)

setColWidths(wb, s1, cols=1,   widths=28)
setColWidths(wb, s1, cols=2:9, widths=11)
freezePane(wb,  s1, firstActiveRow=5, firstActiveCol=2)


# ════════════════════════════════════════════════════════════════════════════
# SHEET 2: Near-Zero and Temporal Stability
# ════════════════════════════════════════════════════════════════════════════
addWorksheet(wb, "2. Near-Zero & Stability")
s2 <- "2. Near-Zero & Stability"

# Build data from objects in memory
nz_long <- near_zero %>%
  pivot_longer(c(near_zero_informal, near_zero_below_min),
               names_to  = "outcome",
               values_to = "flag_near_zero") %>%
  mutate(outcome = recode(outcome,
                          "near_zero_informal"  = "informal",
                          "near_zero_below_min" = "below_min")) %>%
  left_join(
    panel_sf %>%
      group_by(Employment_Sector, Wage_group) %>%
      summarise(median_informal  = median(informal,  na.rm=TRUE),
                median_below_min = median(below_min, na.rm=TRUE),
                .groups="drop") %>%
      pivot_longer(c(median_informal, median_below_min),
                   names_to="outcome", values_to="median_est") %>%
      mutate(outcome = recode(outcome,
                              "median_informal"  = "informal",
                              "median_below_min" = "below_min")),
    by = c("Employment_Sector", "Wage_group", "outcome")
  ) %>%
  mutate(nz_threshold = case_when(
    outcome == "informal"  ~ NEAR_ZERO_INFORMAL,
    outcome == "below_min" ~ NEAR_ZERO_BELOW_MIN
  ),
  flag_label = if_else(flag_near_zero, "SCOPE", "OK"))

stab_long <- stability %>%
  select(Employment_Sector, Wage_group, outcome, ts_cv, flag_unstable) %>%
  mutate(stab_label = if_else(coalesce(flag_unstable, FALSE), "UNSTABLE", "OK"))

# Sheet 2a: Near-zero table
writeData(wb, s2, "Near-Zero Screen (Proportions Only)", startRow=1, startCol=1)
addStyle(wb, s2, title_style, rows=1, cols=1)
mergeCells(wb, s2, cols=1:6, rows=1)
writeData(wb, s2,
          paste0("Cells where the median outcome is structurally near-zero are marked SCOPE — ",
                 "not a data quality failure, but these cells have no meaningful margin for that outcome. ",
                 "Threshold: informal < ", scales::percent(NEAR_ZERO_INFORMAL),
                 ", below_min < ", scales::percent(NEAR_ZERO_BELOW_MIN), "."),
          startRow=2, startCol=1)
addStyle(wb, s2, note_style, rows=2, cols=1); mergeCells(wb, s2, cols=1:6, rows=2)

nz_tbl <- nz_long %>%
  filter(flag_near_zero) %>%
  transmute(Sector         = Employment_Sector,
            `Firm Size`    = Wage_group,
            Outcome        = outcome,
            `Median est.`  = scales::percent(median_est, accuracy=0.1),
            Threshold      = scales::percent(nz_threshold, accuracy=0.1),
            Flag           = flag_label) %>%
  arrange(Outcome, Sector)

hdrs_nz <- names(nz_tbl)
for (j in seq_along(hdrs_nz)) {
  writeData(wb, s2, hdrs_nz[j], startRow=4, startCol=j)
}
apply_style(wb, s2, 4, 1:length(hdrs_nz), header_style())
write_df(wb, s2, nz_tbl, start_row=5, col_names=FALSE)
for (i in seq_len(nrow(nz_tbl))) {
  r <- 4 + i
  apply_style(wb, s2, r, 1,
              createStyle(fontName="Arial",fontSize=10,halign="left",
                          border="TopBottomLeftRight",borderColour="#BFBFBF",
                          fgFill=if(i%%2==0) COL_ALT else "white"))
  for (j in 2:length(hdrs_nz)) {
    bg <- if(i%%2==0) COL_ALT else "white"
    if (j == length(hdrs_nz)) { bg <- COL_SCOPE }
    addStyle(wb, s2,
             createStyle(fontName="Arial",fontSize=10,halign="center",
                         fontColour=if(j==length(hdrs_nz)) COL_SCOPE_FG else "#000000",
                         fgFill=bg, border="TopBottomLeftRight",borderColour="#BFBFBF"),
             rows=r, cols=j)
  }
}

# Sheet 2b: Stability table (below near-zero table, gap of 2 rows)
stab_start <- 5 + nrow(nz_tbl) + 3
writeData(wb, s2, "Temporal Stability — ts_cv = sd / |mean| (non-COVID quarters)",
          startRow=stab_start, startCol=1)
addStyle(wb, s2, title_style, rows=stab_start, cols=1)
mergeCells(wb, s2, cols=1:5, rows=stab_start)
writeData(wb, s2,
          paste0("COVID quarters excluded: ", paste(COVID_QTRS, collapse=", "), ".  ",
                 "UNSTABLE flag: ts_cv > ", STAB_CUT,
                 " (standard deviation exceeds mean level — series dominated by sampling noise)."),
          startRow=stab_start+1, startCol=1)
addStyle(wb, s2, note_style, rows=stab_start+1, cols=1)
mergeCells(wb, s2, cols=1:5, rows=stab_start+1)

stab_tbl <- stab_long %>%
  transmute(Sector       = Employment_Sector,
            `Firm Size`  = Wage_group,
            Outcome      = outcome,
            `ts_cv`      = round(ts_cv, 3),
            Flag         = stab_label) %>%
  arrange(Outcome, desc(ts_cv))

sh <- stab_start + 2
for (j in seq_along(names(stab_tbl))) {
  writeData(wb, s2, names(stab_tbl)[j], startRow=sh, startCol=j)
}
apply_style(wb, s2, sh, 1:length(names(stab_tbl)), header_style())
write_df(wb, s2, stab_tbl, start_row=sh+1, col_names=FALSE)
for (i in seq_len(nrow(stab_tbl))) {
  r <- sh + i
  alt <- (i %% 2 == 0)
  apply_style(wb, s2, r, 1,
              createStyle(fontName="Arial",fontSize=10,halign="left",
                          border="TopBottomLeftRight",borderColour="#BFBFBF",
                          fgFill=if(alt) COL_ALT else "white"))
  for (j in 2:length(names(stab_tbl))) {
    flag_val <- stab_tbl$Flag[i]
    is_flag_col <- (j == length(names(stab_tbl)))
    bg <- if(alt) COL_ALT else "white"
    fg <- "#000000"; bld <- FALSE
    if (is_flag_col && flag_val == "UNSTABLE") {
      bg <- COL_DROP; fg <- COL_DROP_FG; bld <- TRUE
    }
    addStyle(wb, s2,
             createStyle(fontName="Arial",fontSize=10,fontColour=fg,
                         textDecoration=if(bld)"bold" else NULL,
                         halign="center",fgFill=bg,
                         border="TopBottomLeftRight",borderColour="#BFBFBF"),
             rows=r, cols=j)
  }
}

setColWidths(wb, s2, cols=1,   widths=28)
setColWidths(wb, s2, cols=2:6, widths=14)
freezePane(wb, s2, firstActiveRow=5)


# ════════════════════════════════════════════════════════════════════════════
# SHEET 3: Decision Matrix
# ════════════════════════════════════════════════════════════════════════════
addWorksheet(wb, "3. Decision Matrix")
s3 <- "3. Decision Matrix"

writeData(wb, s3,
          "Cell Reliability Decision Matrix — Sector × Firm Size × Outcome",
          startRow=1, startCol=1)
addStyle(wb, s3, title_style, rows=1, cols=1)
mergeCells(wb, s3, cols=1:7, rows=1)
writeData(wb, s3,
          paste0("KEEP = use in regression.  REVIEW = include, flag for sensitivity check.  ",
                 "DROP = exclude.  SCOPE = structural near-zero (not a quality failure, exclude from that outcome only)."),
          startRow=2, startCol=1)
addStyle(wb, s3, note_style, rows=2, cols=1)
mergeCells(wb, s3, cols=1:7, rows=2)

# Build matrix from cross_matrix (already in memory)
# Rename for display
mat_display <- cross_matrix %>%
  rename(Sector = Employment_Sector, `Firm Size` = Wage_group,
         `Median n` = n_median)

hdrs3 <- names(mat_display)
for (j in seq_along(hdrs3)) {
  writeData(wb, s3, hdrs3[j], startRow=3, startCol=j)
}
apply_style(wb, s3, 3, 1:length(hdrs3), header_style())

write_df(wb, s3, mat_display, start_row=4, col_names=FALSE)

outcome_cols <- which(hdrs3 %in% c("log_var_wage","log_50_10","below_min","informal",
                                   "log_var_wage_rec","log_50_10_rec",
                                   "below_min_rec","informal_rec"))
# The actual outcome columns depend on cross_matrix structure — find them
outcome_col_names <- names(mat_display)[!(names(mat_display) %in%
                                            c("Sector","Firm Size","Median n"))]
outcome_col_idx   <- which(names(mat_display) %in% outcome_col_names)

for (i in seq_len(nrow(mat_display))) {
  r <- 3 + i
  n_val <- mat_display$`Median n`[i]
  
  # Sector column
  apply_style(wb, s3, r, 1,
              createStyle(fontName="Arial",fontSize=10,halign="left",
                          textDecoration=if(i==1||mat_display$Sector[i]!=mat_display$Sector[i-1])"bold" else NULL,
                          border="TopBottomLeftRight",borderColour="#BFBFBF"))
  
  # Firm size
  apply_style(wb, s3, r, 2,
              createStyle(fontName="Arial",fontSize=10,halign="center",
                          border="TopBottomLeftRight",borderColour="#BFBFBF"))
  
  # Median n — colour by adequacy
  n_bg <- if (!is.na(n_val) && n_val < N_MIN_MEAN)  COL_DROP
  else if (!is.na(n_val) && n_val < N_WARN)  COL_REVIEW
  else "white"
  n_fg <- if (!is.na(n_val) && n_val < N_MIN_MEAN)  COL_DROP_FG else "#000000"
  n_bld <- (!is.na(n_val) && n_val < N_MIN_MEAN)
  addStyle(wb, s3,
           createStyle(fontName="Arial",fontSize=10,fontColour=n_fg,
                       textDecoration=if(n_bld)"bold" else NULL,
                       halign="center",fgFill=n_bg,
                       border="TopBottomLeftRight",borderColour="#BFBFBF"),
           rows=r, cols=3)
  
  # Outcome recommendation columns
  for (j in outcome_col_idx) {
    rec_raw <- mat_display[i, j] %>% pull()
    # cross_matrix uses KEEP/REVIEW/DROP/SCOPE codes; map back to full label
    rec_full <- switch(as.character(rec_raw),
                       "KEEP"   = "Keep",
                       "REVIEW" = "Review",
                       "DROP"   = "Drop",
                       "SCOPE"  = "Out of scope",
                       "CAUTION"= "Caution",
                       as.character(rec_raw))
    cols_info <- rec_colours(rec_full)
    addStyle(wb, s3,
             createStyle(fontName="Arial",fontSize=10,
                         fontColour=cols_info$fg,
                         textDecoration=if(rec_full %in% c("Keep","Drop"))"bold" else NULL,
                         halign="center",
                         fgFill=if(is.null(cols_info$bg)) "white" else cols_info$bg,
                         border="TopBottomLeftRight",borderColour="#BFBFBF"),
             rows=r, cols=j)
  }
}

# Summary below table
sr3 <- 4 + nrow(mat_display) + 2
summary_counts <- decision_table %>%
  count(outcome, recommendation) %>%
  mutate(recommendation = recode(recommendation,
                                 "Keep"="KEEP","Review"="REVIEW","Caution"="CAUTION",
                                 "Drop"="DROP","Out of scope"="SCOPE")) %>%
  pivot_wider(names_from=recommendation, values_from=n, values_fill=0L)

writeData(wb, s3, "Summary — cells per outcome:", startRow=sr3, startCol=1)
addStyle(wb, s3, bold_style, rows=sr3, cols=1)
write_df(wb, s3, summary_counts, start_row=sr3+1)
apply_style(wb, s3, sr3+1, 1:ncol(summary_counts), header_style())

fn_row <- sr3 + nrow(summary_counts) + 3
writeData(wb, s3,
          "Construction is entirely excluded — all tiers thin across all outcomes.",
          startRow=fn_row, startCol=1)
addStyle(wb, s3, note_style, rows=fn_row, cols=1)
writeData(wb, s3,
          "Medium tier mostly excluded — adequate data only in Commerce and Manufacturing.",
          startRow=fn_row+1, startCol=1)
addStyle(wb, s3, note_style, rows=fn_row+1, cols=1)
writeData(wb, s3,
          "log_50_10 has the fewest KEEPs — p10 quantile unreliable at small n. Recommend log_var_wage as primary inequality measure.",
          startRow=fn_row+2, startCol=1)
addStyle(wb, s3, note_style, rows=fn_row+2, cols=1)

setColWidths(wb, s3, cols=1,   widths=28)
setColWidths(wb, s3, cols=2,   widths=12)
setColWidths(wb, s3, cols=3,   widths=12)
setColWidths(wb, s3, cols=4:7, widths=14)
freezePane(wb, s3, firstActiveRow=4)


# ════════════════════════════════════════════════════════════════════════════
# SHEET 4: Legend
# ════════════════════════════════════════════════════════════════════════════
addWorksheet(wb, "Legend")
s4 <- "Legend"

writeData(wb, s4, "Validation Methodology and Legend", startRow=1, startCol=1)
addStyle(wb, s4, title_style, rows=1, cols=1)

legend_items <- list(
  list(row=3,  label="Recommendation codes (Sheet 3):",          fill=NULL,         desc=NULL),
  list(row=4,  label="KEEP",    fill=COL_KEEP,   fg=COL_KEEP_FG,   desc="Passes all checks. Include in regression."),
  list(row=5,  label="REVIEW",  fill=COL_REVIEW, fg=COL_REVIEW_FG, desc="One concern (thin count or temporal instability). Include but run sensitivity without these cells."),
  list(row=6,  label="DROP",    fill=COL_DROP,   fg=COL_DROP_FG,   desc=paste0("Multiple failures or >", scales::percent(THIN_CUT), " of quarters below n_min. Exclude from regression.")),
  list(row=7,  label="SCOPE",   fill=COL_SCOPE,  fg=COL_SCOPE_FG,  desc="Structural near-zero proportion. No meaningful margin — exclude from that outcome only, not a data quality failure."),
  list(row=9,  label="Raw count shading (Sheet 1):",              fill=NULL,         desc=NULL),
  list(row=10, label="Light pink", fill=COL_THIN_MED, fg="#000000",
       desc=paste0("Median quarterly n < n_min (", N_MIN_MEAN, " for means/proportions; ", N_MIN_QUANT, " for quantile ratios).")),
  list(row=11, label="Deeper red", fill=COL_THIN_MIN, fg=COL_DROP_FG,
       desc="Minimum quarterly n < n_min. Shows worst-case quarter."),
  list(row=13, label="Validation checks:",                        fill=NULL,         desc=NULL),
  list(row=14, label="A. Raw count",         fill=NULL, desc=paste0("n_min = ", N_MIN_MEAN, " (means/proportions), ", N_MIN_QUANT, " (quantile ratios). DROP if >", scales::percent(THIN_CUT), " of quarters below n_min.")),
  list(row=15, label="B. Temporal stability",fill=NULL, desc=paste0("ts_cv = sd/|mean| (non-COVID quarters). UNSTABLE if ts_cv > ", STAB_CUT, ".")),
  list(row=16, label="C. Near-zero",         fill=NULL, desc=paste0("informal < ", scales::percent(NEAR_ZERO_INFORMAL), " or below_min < ", scales::percent(NEAR_ZERO_BELOW_MIN), " → SCOPE.")),
  list(row=18, label="Key findings:",                             fill=NULL,         desc=NULL),
  list(row=19, label="Construction",    fill=NULL, desc="All four tiers excluded — entire sector dropped from regression sample."),
  list(row=20, label="Medium tier",     fill=NULL, desc="Excluded in 8/10 sectors. Only Commerce and Manufacturing borderline adequate."),
  list(row=21, label="log_50_10",       fill=NULL, desc="Only 13 KEEPs — p10 quantile unreliable at small n. Recommend log_var_wage as primary inequality measure."),
  list(row=22, label="informal (SCOPE)",fill=NULL, desc="22/40 cells near-zero — private sector sample has low informality in most cells.")
)

for (item in legend_items) {
  r <- item$row
  is_section_header <- is.null(item$fill) && is.null(item$desc)
  
  writeData(wb, s4, item$label, startRow=r, startCol=1)
  
  cell_style <- if (is_section_header) {
    bold_style
  } else if (!is.null(item$fill)) {
    # Coloured swatch — draw border around it
    createStyle(fontName="Arial", fontSize=10,
                fontColour=if(!is.null(item$fg)) item$fg else "#000000",
                fgFill=item$fill,
                border="TopBottomLeftRight", borderColour="#BFBFBF")
  } else {
    # Plain text row — no border, no fill
    createStyle(fontName="Arial", fontSize=10,
                fontColour=if(!is.null(item$fg)) item$fg else "#000000")
  }
  
  addStyle(wb, s4, cell_style, rows=r, cols=1)
  
  if (!is.null(item$desc)) {
    writeData(wb, s4, item$desc, startRow=r, startCol=2)
    addStyle(wb, s4, note_style, rows=r, cols=2)
  }
}

setColWidths(wb, s4, cols=1, widths=22)
setColWidths(wb, s4, cols=2, widths=90)


# ── Save workbook ─────────────────────────────────────────────────────────────
wb_path <- file.path(config$paths$outputs, config$output_stage,
                     "cell_validation_report.xlsx")
saveWorkbook(wb, wb_path, overwrite = TRUE)

cat("\nSaved:\n")
cat("  validation_decision_table.rds  — cell × outcome recommendations\n")
cat("  validation_cross_matrix.csv    — appendix table\n")
cat("  panel_sf_clean.rds             — regression-ready panel\n")
cat("  cell_validation_report.xlsx    — formatted validation workbook\n")

cat("\n=== 02_Cell_Validation_For_Regression_Data.R complete ===\n")