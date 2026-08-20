#===============================================================================
  # 03B_sample Attrition.R
  #
  # PURPOSE
  #   Describe and validate the samples built in 03A. Produces:
  #     sample_metadata.rds              sizes, degf, population per quarter
  #     table1_attrition.{html,png}      pooled sample construction
  #     table2_retention.{html,png}      retention by year
  #
  #   Nothing downstream depends on this file. Assertions that must HALT the
  #   pipeline belong in 03A; this file produces things a human reads.
  #
  #===============================================================================

source(here::here("Code", "R", "clean scripts", "00_setup.R"))
source(here::here("Code", "R", "clean scripts", "03_sample definitions.R"))

cat("=== 03B_sample diagnostics.R ===\n\n")

tbl_dir <- file.path(config$paths$outputs, config$output_stage,
                     config$out_subdirs$data_check, "reg sample")
stopifnot(!is.null(config$out_subdirs$data_check))
dir.create(tbl_dir, recursive = TRUE, showWarnings = FALSE)

tbl_path <- function(stem, ext) file.path(tbl_dir, paste0(stem, ".", ext))



#===============================================================================
# STEP 1.  attrition_table()
#
#  MOVE TO 00_setup.R once stable — it is a general helper, not a step in this
#  script. Kept here for now so 03B runs standalone.
#
#  Applies `steps` cumulatively within a time window and reports records and
#  weighted population at each stage. pop is divided by the number of quarters
#  in the window, so it is the AVERAGE POPULATION PER QUARTER, never a headcount.
#===============================================================================

attrition_table <- function(steps,
                            design,
                            period       = quote(TRUE),
                            period_label = "All quarters",
                            se           = FALSE,
                            anchors      = c(employed = "Employed",
                                             priv     = "Private-sector employee"),
                            env          = parent.frame()) {
  
  force(env)
  
  if (se && !"one" %in% names(design$variables)) design <- update(design, one = 1)
  
  dat <- design$variables
  n   <- nrow(dat)
  
  as_idx <- function(e) {
    x <- eval(e, dat, env)
    if (length(x) == 1L) x <- rep(x, n)
    x & !is.na(x)
  }
  
  base <- as_idx(period)
  if (!any(base)) stop("No records in period '", period_label, "'.")
  
  nq <- dplyr::n_distinct(dat[[TIME_VAR]][base])
  
  cum <- purrr::accumulate(steps, function(prev, e) prev & as_idx(e), .init = base)[-1]
  names(cum) <- names(steps)
  
  out <- purrr::imap_dfr(cum, function(idx, lbl) {
    d <- design[idx, ]
    if (se && sum(idx) > 0L) {
      tt  <- svytotal(~one, d)
      pop <- as.numeric(coef(tt)) / nq
      pse <- as.numeric(SE(tt))   / nq
    } else {
      pop <- sum(weights(d, "sampling")) / nq
      pse <- NA_real_
    }
    tibble::tibble(period = period_label, step = lbl,
                   n_rows = sum(idx), pop = pop, pop_se = pse)
  }) |>
    dplyr::mutate(
      n_dropped    = dplyr::lag(n_rows) - n_rows,
      pop_dropped  = dplyr::lag(pop)    - pop,
      pop_pct_prev = pop / dplyr::lag(pop),
      cv           = pop_se / pop
    )
  
  missing_anchors <- setdiff(anchors, out$step)
  if (length(missing_anchors)) {
    stop("Anchor step(s) not found: ", paste(missing_anchors, collapse = ", "),
         "\nAvailable steps: ", paste(out$step, collapse = " | "))
  }
  
  for (nm in names(anchors)) {
    i     <- match(anchors[[nm]], out$step)
    share <- out$pop / out$pop[i]
    share[seq_len(i - 1L)] <- NA_real_    # rows above the anchor are not shares
    out[[paste0("pct_of_", nm)]] <- share
  }
  
  out
}


#===============================================================================
# STEP 2.  Sample metadata
#
#  pop_mean_qtr is the number to quote as a population figure. sum(weights) is
#  n_quarters x population and must never appear in a table.
#===============================================================================

cat("[1] Building sample metadata...\n")

sample_metadata <- purrr::imap_dfr(samples, function(s, id) {
  
  w  <- weights(s$design, "sampling")
  v  <- s$data
  nq <- dplyr::n_distinct(v[[TIME_VAR]])
  
  tibble::tibble(
    sample_id     = id,
    label         = s$label,
    parent        = s$parent,
    n_rows        = s$n_rows,
    n_psu         = dplyr::n_distinct(v$psu_unique),
    n_strata      = dplyr::n_distinct(v$strata_unique),
    degf          = survey::degf(s$design),
    n_quarters    = nq,
    first_quarter = min(v[[TIME_VAR]]),
    last_quarter  = max(v[[TIME_VAR]]),
    pop_mean_qtr  = sum(w) / nq,
    min_n_qtr     = min(table(droplevels(factor(v[[TIME_VAR]])))),
    # Composed filter, not the incremental spec — this documents the sample.
    filter_text   = paste(deparse(SAMPLE_FILTERS[[id]]), collapse = " "),
    filter_hash   = rlang::hash(SAMPLE_FILTERS[[id]]),
    built_at      = Sys.time()
  )
})

sample_metadata <- sample_metadata %>%
  left_join(sample_metadata %>% select(parent = sample_id, parent_pop = pop_mean_qtr),
            by = "parent") %>%
  mutate(share_of_parent = pop_mean_qtr / parent_pop) %>%
  select(-parent_pop)

saveRDS(sample_metadata,
        file.path(config$paths$processed_data, "sample_metadata.rds"))

# Binding constraint for the regression design: quarter x firm-size tier cells.
min_cell_qtr_tier <- df_regression_sample %>%
  count(.data[[TIME_VAR]], wage_group) %>%
  summarise(min_cell = min(n), .groups = "drop") %>%
  pull(min_cell)

cat("    Records with missing EDAD: ", n_missing_age, "\n", sep = "")
cat("    Smallest quarter x tier cell in regression_sample: ",
    min_cell_qtr_tier, "\n\n", sep = "")

print(sample_metadata %>%
        select(sample_id, n_rows, pop_mean_qtr, degf, min_n_qtr, share_of_parent),
      n = Inf)



#===============================================================================
# STEP 3.  Table 1 — pooled attrition
#===============================================================================

cat("\n[2] Table 1: pooled attrition...\n")

x1 <- attrition_table(REGRESSION_STEPS, design_full, se = FALSE)

# The waterfall and the sample must agree. If this fires, REGRESSION_STEPS and
# the parent chain of regression_sample have diverged.
stopifnot(identical(dplyr::last(x1$n_rows), samples$regression_sample$n_rows))

qtr_range <- range(as.character(design_full$variables[[TIME_VAR]]))

table1 <- x1 %>%
  select(step, n_rows, n_dropped, pop, pop_dropped,
         pop_pct_prev, pct_of_employed, pct_of_priv) %>%
  gt() %>%
  tab_header(
    title    = "Table 1. Construction of the estimation sample",
    subtitle = sprintf("ENCFT, %s-%s. Conditions applied cumulatively.",
                       qtr_range[1], qtr_range[2])
  ) %>%
  cols_label(step = "Restriction",
             n_rows = "Retained", n_dropped = "Dropped",
             pop = "Retained",    pop_dropped = "Dropped",
             pop_pct_prev = "of previous row",
             pct_of_employed = "of employed",
             pct_of_priv = "of private employees") %>%
  tab_spanner("Person-quarters",                  columns = c(n_rows, n_dropped)) %>%
  tab_spanner("Population (average per quarter)", columns = c(pop, pop_dropped)) %>%
  tab_spanner("Share", columns = c(pop_pct_prev, pct_of_employed, pct_of_priv)) %>%
  fmt_number(c(n_rows, n_dropped, pop, pop_dropped), decimals = 0, use_seps = TRUE) %>%
  fmt_percent(c(pop_pct_prev, pct_of_employed, pct_of_priv), decimals = 1) %>%
  sub_missing(everything(), missing_text = "\u2014") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = step %in% c("Employed",
                                                      "Private-sector employee",
                                                      "Formality status known"))) %>%
  tab_style(style = cell_borders(sides = "top", weight = px(2)),
            locations = cells_body(rows = step == "Formality status known")) %>%
  tab_footnote(
    paste("Person-quarters, not persons: the ENCFT follows sampled dwellings for",
          "up to five consecutive quarters, so an individual may contribute up to",
          "five records."),
    locations = cells_column_spanners("Person-quarters")) %>%
  tab_footnote(
    paste("Sum of expansion factors divided by the number of quarters in the",
          "window: the average population represented in a typical quarter, not a",
          "count of distinct individuals."),
    locations = cells_column_spanners("Population (average per quarter)")) %>%
  tab_footnote(
    paste("Firm size is self-reported by the worker in bins (1-10, 11-20, 21-30,",
          "31-50, 51-99, 100+, don't know) and mapped to the legal minimum-wage",
          "tiers."),
    locations = cells_body(columns = step, rows = step == "Firm size tier known")) %>%
  tab_source_note(paste("Source: Authors' calculations using ENCFT (Banco Central",
                        "de la Republica Dominicana). Population figures use survey",
                        "expansion factors.")) %>%
  cols_align("left", columns = step) %>%
  cols_align("right", columns = -step) %>%
  tab_options(table.font.size = px(13), heading.title.font.size = px(15),
              heading.subtitle.font.size = px(12), column_labels.font.weight = "bold",
              table.border.top.style = "none", table_body.hlines.style = "none",
              data_row.padding = px(4))


stem <- "table1_Sample"

gtsave(table1, tbl_path(stem, "html"))
gtsave(table1, tbl_path(stem, "png"), expand = 10)






#===============================================================================
# ADD THIS TO 03_sample definitions.R, immediately after REGRESSION_STEPS.
#
# Defined by POSITION, not by label, so it cannot drift when REG_AGE_LABEL
# changes or a step is renamed. The stopifnot is the guard.
#===============================================================================

STEP_TYPE <- c(
  "All person-quarter records"  = "—",
  "<age band>"                  = "Population definition",
  "Economically active"         = "Population definition",
  "Employed"                    = "Population definition",
  "Private-sector employee"     = "Population definition",
  "Excl. domestic workers"      = "Legal scope of the MW schedule",
  "Excl. free trade zone"       = "Legal scope of the MW schedule",
  "Excl. electricity and water" = "Legal scope of the MW schedule",
  "Positive salary"             = "Data availability",
  "Hours worked > 0"            = "Data availability",
  "Firm size tier known"        = "Data availability",
  "Formality status known"      = "Data availability"
)
names(STEP_TYPE) <- names(REGRESSION_STEPS)[-1]   # 11 names onto a 12-element vector
stopifnot(length(STEP_TYPE) == length(REGRESSION_STEPS))   # tests 12 == 12
names(STEP_TYPE) <- names(REGRESSION_STEPS)                # overwrites the first line

TYPE_LEVELS <- c("Population definition",
                 "Legal scope of the MW schedule",
                 "Data availability")

# Minimum wage events. Phase-in completion quarters are in 01B; these are the
# announcement quarters, which is what a selection break would align with.
MW_EVENTS <- config$events$event_qtrs


#===============================================================================
# STEP 4a.  pass_rates_by()
#
#  Applies `steps` cumulatively and returns, for every (group x step), the
#  unweighted records and weighted population surviving, plus the pass rate
#  (share of the PREVIOUS step's population within the same group).
#
#  Pass rates are ratios of weighted sums within a group, so the per-quarter
#  normalisation used in Table 1 cancels. No design subsetting, no svytotal:
#  with se = FALSE those buy nothing and cost ~550 subsets of a 900k-row frame.
#===============================================================================

pass_rates_by <- function(steps, data, group_var, weight_var = "FACTOR_EXPANSION",
                          env = parent.frame()) {
  
  force(env)
  n  <- nrow(data)
  w  <- data[[weight_var]]
  gf <- factor(data[[group_var]])
  
  as_idx <- function(e) {
    x <- eval(e, data, env)
    if (length(x) == 1L) x <- rep(x, n)
    x & !is.na(x)
  }
  
  cum <- purrr::accumulate(steps, function(prev, e) prev & as_idx(e),
                           .init = rep(TRUE, n))[-1]
  names(cum) <- names(steps)
  
  purrr::imap_dfr(cum, function(ix, lbl) {
    tibble::tibble(
      group  = levels(gf),
      step   = lbl,
      n_rows = as.numeric(tapply(as.numeric(ix), gf, sum)),
      pop    = as.numeric(tapply(w * ix,         gf, sum))
    )
  }) %>%
    dplyr::mutate(step = factor(step, levels = names(steps))) %>%
    dplyr::arrange(group, step) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(pass = pop / dplyr::lag(pop)) %>%
    dplyr::ungroup()
}


#===============================================================================
# STEP 4b.  Annual pass rates -> Table 2
#
#  Full years only: a partial year is a noisier estimate and not comparable
#  across columns. This drops 2014 (two quarters observed).
#===============================================================================

cat("[3] Table 2: stability of the sample-construction rule...\n")

dat_full <- design_full$variables

full_years <- dat_full %>%
  dplyr::distinct(year, .data[[TIME_VAR]]) %>%
  dplyr::count(year, name = "n_quarters") %>%
  dplyr::filter(n_quarters == 4L) %>%
  dplyr::pull(year)

pass_year <- pass_rates_by(REGRESSION_STEPS,
                           dplyr::filter(dat_full, year %in% full_years),
                           group_var = "year") %>%
  dplyr::rename(year = group) %>%
  dplyr::mutate(year = as.integer(year))

# Keep the full grid for the online appendix / own checking.
saveRDS(pass_year, file.path(config$paths$processed_data, "pass_rates_by_year.rds"))

# Pooled pass rates come from Table 1, so the two tables are tied together
# rather than being two independent calculations of the same quantity.
pooled <- x1 %>% dplyr::select(step, pooled = pop_pct_prev)

#-------------------------------------------------------------------------------
# Cumulative rows.
#
#  Marginal pass rates isolate WHICH restriction moves, which is what a
#  diagnostic needs, but "share of the row above" is not how anyone thinks about
#  a sample. Two cumulative shares give the intuitive version.
#
#  Both are anchored BELOW the population-definition steps on purpose. Anchored
#  at "all records" they would move with participation and the private-employee
#  share, i.e. with the labour market rather than with our sample rule, and the
#  cumulative row would no longer mean anything about the data.
#    (a) anchored at private employee   -> cost of scope + data restrictions
#    (b) anchored at the last scope step -> cost of data availability alone
#-------------------------------------------------------------------------------

LAST_STEP   <- dplyr::last(names(REGRESSION_STEPS))
CUM_ANCHORS <- c(
  "Private-sector employee"     = "Share of private-sector employees retained",
  "Excl. electricity and water" = "Share retained by data-availability rules alone"
)

cum_year <- purrr::imap_dfr(CUM_ANCHORS, function(lbl, anchor) {
  pass_year %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(pass = pop[step == LAST_STEP] / pop[step == anchor],
                     .groups = "drop") %>%
    dplyr::mutate(step = lbl)
})

cum_pooled <- purrr::imap_dfr(CUM_ANCHORS, function(lbl, anchor) {
  tibble::tibble(
    step   = lbl,
    pooled = x1$pop[x1$step == LAST_STEP] / x1$pop[x1$step == anchor]
  )
})

series <- dplyr::bind_rows(
  pass_year %>%
    dplyr::filter(!is.na(pass)) %>%
    dplyr::transmute(year, step = as.character(step), pass),
  cum_year
)

STEP_ORDER     <- c(names(REGRESSION_STEPS), unname(CUM_ANCHORS))
SERIES_TYPE    <- c(STEP_TYPE,
                    setNames(rep("Cumulative", length(CUM_ANCHORS)),
                             unname(CUM_ANCHORS)))
TYPE_LEVELS_T2 <- c(TYPE_LEVELS, "Cumulative")

table2_data <- series %>%
  dplyr::group_by(step) %>%
  dplyr::summarise(
    min_yr     = min(pass),
    max_yr     = max(pass),
    range_pp   = 100 * (max(pass) - min(pass)),
    first_last = 100 * (pass[which.max(year)] - pass[which.min(year)]),
    # Trend on annual pass rates. Communicates direction and pace in one number;
    # not an inferential claim, so no SE reported.
    trend_pp   = 100 * unname(coef(lm(pass ~ year))[2]),
    .groups    = "drop"
  ) %>%
  dplyr::left_join(dplyr::bind_rows(pooled, cum_pooled), by = "step") %>%
  dplyr::mutate(
    type = factor(unname(SERIES_TYPE[step]), levels = TYPE_LEVELS_T2),
    step = factor(step, levels = STEP_ORDER)
  ) %>%
  dplyr::arrange(type, step) %>%
  dplyr::select(type, step, pooled, min_yr, max_yr, range_pp, first_last, trend_pp)

# Flag the unstable rows programmatically rather than hard-coding a label.
UNSTABLE_PP <- 3
flag_rows <- which(abs(table2_data$range_pp) > UNSTABLE_PP)

yr_first <- min(full_years); yr_last <- max(full_years)

table2 <- table2_data %>%
  gt(groupname_col = "type") %>%
  tab_header(
    title    = "Table 2. Stability of the sample-construction rule over time",
    subtitle = sprintf(paste("Share of the eligible population satisfying each",
                             "restriction (pass rate), %d-%d"),
                       yr_first, yr_last)
  ) %>%
  cols_label(
    step       = "Restriction",
    pooled     = "Pooled",
    min_yr     = "Min",
    max_yr     = "Max",
    range_pp   = "Range",
    first_last = sprintf("%d\u2192%d", yr_first, yr_last),
    trend_pp   = "Trend"
  ) %>%
  tab_spanner("Pass rate", columns = c(pooled, min_yr, max_yr)) %>%
  tab_spanner("Movement across years (pp)",
              columns = c(range_pp, first_last, trend_pp)) %>%
  fmt_percent(c(pooled, min_yr, max_yr), decimals = 1) %>%
  fmt_number(c(range_pp, first_last), decimals = 1) %>%
  fmt_number(trend_pp, decimals = 2) %>%
  sub_missing(everything(), missing_text = "\u2014") %>%
  tab_style(style = list(cell_fill(color = "#FDF3E7"), cell_text(weight = "bold")),
            locations = cells_body(rows = flag_rows)) %>%
  tab_footnote(
    paste("Pass rate is the population satisfying a restriction divided by the",
          "population satisfying all restrictions above it. Restrictions are",
          "cumulative, so pass rates are conditional and are not comparable",
          "across rows as standalone drop rates. This is NOT panel attrition:",
          "no individual leaves the sample here, and the ENCFT rotation is",
          "documented separately."),
    locations = cells_column_spanners("Pass rate")) %>%
  tab_footnote(
    paste("Range is max minus min across years. Trend is the OLS slope of the",
          "annual pass rate on year, in percentage points per year; it is a",
          "descriptive summary, not an inferential test."),
    locations = cells_column_spanners("Movement across years (pp)")) %>%
  tab_footnote(
    paste("Population-definition restrictions move with the labour market",
          "itself and are not indicators of data quality: the rise in the",
          "economically active share reflects rising participation."),
    locations = cells_row_groups(groups = "Population definition")) %>%
  tab_footnote(
    paste("Cumulative rows are the estimation sample as a share of a fixed",
          "anchor, not of the row above. The first is anchored at private-sector",
          "employees and so measures the combined cost of the scope and",
          "data-availability restrictions; the second is anchored after the",
          "scope exclusions and so isolates data availability. Both anchors sit",
          "below the population-definition steps, so neither moves with",
          "participation or with the private-employee share of employment."),
    locations = cells_row_groups(groups = "Cumulative")) %>%
  tab_source_note(sprintf(paste("Source: Authors' calculations using ENCFT.",
                                "Years with fewer than four observed quarters",
                                "are omitted (%d). Shaded rows move by more",
                                "than %d pp across years; see Figure A1 for",
                                "their quarterly path."),
                          min(dat_full$year), UNSTABLE_PP)) %>%
  cols_align("left", columns = step) %>%
  cols_align("right", columns = -c(step)) %>%
  tab_options(table.font.size = px(11), heading.title.font.size = px(14),
              heading.subtitle.font.size = px(11),
              column_labels.font.weight = "bold",
              row_group.font.weight = "bold",
              table.border.top.style = "none",
              table_body.hlines.style = "none",
              data_row.padding = px(3))

stem <- "table2_stability"

gtsave(table2, tbl_path(stem, "html"))
gtsave(table2, tbl_path(stem, "png"), expand = 10)



#===============================================================================
# REPLACEMENT FOR 03B STEP 4c (Figure A1)
#
# CHANGES
#   1. Non-binding restrictions are dropped from the figure. This is a DISPLAY
#      filter only - the cumulative chain and every pass rate are computed on
#      all 12 steps exactly as before, so the plotted series are unchanged.
#      Restrictions that never bind stay in Tables 1 and 2, where "this
#      condition costs nothing" is itself a documented result.
#   2. accuracy = 0.1 on the y-axis. At accuracy = 1 a panel spanning 63.4-64.7%
#      printed five labels all reading "64%".
#   3. Title corrected: two series move, not one.
#   4. Event quarters in the caption are built from MW_EVENTS rather than typed
#      out, so the caption cannot drift from config.
#===============================================================================

cat("[4] Figure A1: quarterly pass rates...\n")

if (!exists("theme_surveytools")) theme_surveytools <- function(...) theme_minimal()

pass_qtr <- pass_rates_by(REGRESSION_STEPS, dat_full, group_var = TIME_VAR) %>%
  dplyr::rename(!!TIME_VAR := group) %>%
  dplyr::filter(!is.na(pass)) %>%
  dplyr::mutate(
    # "2017Q2" -> 2017.25, so the x-axis is continuous and event lines land
    # between quarters rather than on category midpoints.
    t    = as.integer(substr(.data[[TIME_VAR]], 1, 4)) +
      (as.integer(substr(.data[[TIME_VAR]], 6, 6)) - 1) / 4,
    type = factor(unname(STEP_TYPE[as.character(step)]), levels = TYPE_LEVELS),
    step = factor(step, levels = names(REGRESSION_STEPS))
  )

stopifnot(!anyNA(pass_qtr$type))

#-------------------------------------------------------------------------------
# Display filter. Threshold, not a hard-coded list: if a restriction starts
# binding in a later data vintage its panel comes back without an edit here.
#-------------------------------------------------------------------------------

NONBINDING_PP <- 0.5

step_range <- pass_qtr %>%
  dplyr::group_by(step) %>%
  dplyr::summarise(rng_pp = 100 * (max(pass) - min(pass)), .groups = "drop")

keep_steps <- step_range$step[step_range$rng_pp >= NONBINDING_PP]
drop_steps <- setdiff(as.character(step_range$step), as.character(keep_steps))

pass_fig <- pass_qtr %>%
  dplyr::filter(step %in% keep_steps) %>%
  dplyr::mutate(step = droplevels(step))

cat("    Panels omitted as non-binding (<", NONBINDING_PP, " pp range): ",
    if (length(drop_steps)) paste(drop_steps, collapse = "; ") else "none",
    "\n", sep = "")

event_t <- as.integer(substr(MW_EVENTS, 1, 4)) +
  (as.integer(substr(MW_EVENTS, 6, 6)) - 1) / 4

TYPE_COLS <- c("Population definition"          = "#7F7F7F",
               "Legal scope of the MW schedule" = "#2C5F8A",
               "Data availability"              = "#C45C30")

drop_note <- if (length(drop_steps)) {
  paste0("Omitted as non-binding (range below ", NONBINDING_PP, " pp): ",
         paste(drop_steps, collapse = "; "),
         ". They are retained in Tables 1 and 2 and in the cumulative chain.")
} else NULL

fig_A1 <- ggplot(pass_fig, aes(t, pass, colour = type)) +
  annotate("rect", xmin = 2020.00, xmax = 2020.75, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.45) +
  geom_vline(xintercept = event_t, linetype = "dashed",
             colour = "grey55", linewidth = 0.35) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~step, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = TYPE_COLS, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(2015, 2025, 2)) +
  labs(
    title    = paste("Firm-size reporting and salary response deteriorate;",
                     "the rest of the sample rule is stable"),
    subtitle = paste("Share of the eligible population satisfying each",
                     "restriction, by quarter"),
    x = NULL, y = "Pass rate",
    caption = paste(c(
      paste0("Dashed verticals: minimum wage events (",
             paste(MW_EVENTS, collapse = ", "), "). ",
             "Shaded band: 2020 pandemic quarters."),
      paste("Restrictions are applied cumulatively in the order shown, so each",
            "series is conditional on those above it."),
      "Y-axis scales differ across panels.",
      drop_note,
      "Source: Authors' calculations using ENCFT."
    ), collapse = "\n")
  ) +
  theme_surveytools(legend_position = "bottom")

ggsave(file.path(tbl_dir, "figA1_pass_rates_quarterly.png"), fig_A1,
       width = 10, height = 7.5, dpi = 300)

cat("\n[5] Table 2 and Figure A1 written to: ", tbl_dir, "\n", sep = "")


cat("=== 03B_sample diagnostics.R complete ===\n\n")




