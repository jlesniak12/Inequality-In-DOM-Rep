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
# STEP 2.  Check Basic Sample Metadata
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



cell_diag <- df_reg_variance %>%
  count(.data[[TIME_VAR]], Region10, Employment_Status) %>%
  group_by(Employment_Status) %>%
  summarise(min = min(n), p10 = quantile(n, .10), median = median(n),
            share_under_30 = mean(n < 30), .groups = "drop")

cat("    Region x quarter x formality cells in reg_variance:\n")
print(cell_diag)
cat("    Smallest quarter x tier cell in reg_tier: ",
    df_reg_tier %>% count(.data[[TIME_VAR]], wage_group) %>% pull(n) %>% min(),
    "\n\n", sep = "")



#===============================================================================
# STEP 3.  Table 1 — pooled attrition
#===============================================================================


#===============================================================================
# 3A.  attrition_table()
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
# STEP 3B Construct Table 1
#===============================================================================


cat("\n[2] Table 1: pooled attrition...\n")

x1 <- attrition_table(REGRESSION_STEPS, design_full, se = FALSE)

# The waterfall and the sample must agree. If this fires, REGRESSION_STEPS and
# the parent chain of regression_sample have diverged.
stopifnot(identical(dplyr::last(x1$n_rows), samples$reg_tier$n_rows))


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
            locations = cells_body(rows = step %in% names(FRAME_CUTS))) %>%
  tab_style(style = cell_borders(sides = "bottom", weight = px(1.5),
                                 color = "#999999"),
            locations = cells_body(rows = step %in% names(FRAME_CUTS))) %>%
  tab_footnote(
    paste("Rules are shown to the bottom of the ladder, but the estimating",
          "samples cut earlier. Horizontal rules mark the three cut points:",
          "employed (share outcomes), hours worked > 0 (wage-dispersion",
          "outcomes), and firm size tier known (compliance and the base-year",
          "exposure weights). Firm-size tier is not imposed on the first two:",
          "exposure is a region-level scalar, so a worker's own tier enters",
          "neither treatment assignment nor those outcomes, and imposing it",
          "would drop a group that grows from 1.5% to 13.4% of the sample and",
          "is concentrated in Gran Santo Domingo."),
    locations = cells_column_labels(columns = step)) %>%
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
# STEP 4.  Table 2 — Sample Stability
#===============================================================================


#===============================================================================
# STEP 4A.  pass_rates_by()
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
CUM_ROWS <- tibble::tribble(
  ~label,                                            ~anchor,                   ~endpoint,
  "Share of private employees in the wage sample",   "Private-sector employee", "Hours worked > 0",
  "...if firm-size tier were also required",         "Private-sector employee", "Firm size tier known"
)

cum_year <- purrr::pmap_dfr(CUM_ROWS, function(label, anchor, endpoint) {
  pass_year %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(pass = pop[step == endpoint] / pop[step == anchor],
                     .groups = "drop") %>%
    dplyr::mutate(step = label)
})

cum_pooled <- purrr::pmap_dfr(CUM_ROWS, function(label, anchor, endpoint) {
  tibble::tibble(step   = label,
                 pooled = x1$pop[x1$step == endpoint] / x1$pop[x1$step == anchor])
})

series <- dplyr::bind_rows(
  pass_year %>%
    dplyr::filter(!is.na(pass)) %>%
    dplyr::transmute(year, step = as.character(step), pass),
  cum_year
)

# Downstream uses of unname(CUM_ANCHORS) become CUM_ROWS$label:
STEP_ORDER  <- c(names(REGRESSION_STEPS), CUM_ROWS$label)
SERIES_TYPE <- c(STEP_TYPE,
                 setNames(rep("Cumulative", nrow(CUM_ROWS)), CUM_ROWS$label))

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
flag_rows <- which(
  round(abs(table2_data$range_pp), 1) > UNSTABLE_PP &
    table2_data$type %in% c("Legal scope of the MW schedule",
                            "Data availability", "Cumulative")
)

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
          "anchor, not of the row above. Both are anchored at private-sector",
          "employees; they differ only in whether the firm-size tier is",
          "required, so the gap between them is the cost of that restriction.",
          "The anchor sits below the population-definition steps, so neither",
          "row moves with participation or with the private-employee share",
          "of employment."),
    locations = cells_row_groups(groups = "Cumulative")) %>%
  tab_source_note(sprintf(paste("Source: Authors' calculations using ENCFT.",
                                "Years with fewer than four observed quarters",
                                "are omitted (%d). Shaded rows are scoppe or",
                                "data-availability restrictions moving by more",
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
# STEP 5.  Table 3 — Sample Characteristics
#===============================================================================


#===============================================================================
# STEP 5A.  TABLE 3 — SAMPLE CHARACTERISTICS  (new; insert after Table 2)
#
#  PERIOD CHOICE
#    Pooled is primary: the table exists so a reader can compare the samples to
#    each other, and that comparison is stable over the decade. Population is
#    reported as AVERAGE PER QUARTER, never sum(weights), which is 46x the
#    population. 2025Q4 is written as a second panel because composition drifts
#    (formality and education both rise) and because it is the quarter where the
#    firm-size non-response is largest.
#
#  DENOMINATORS
#    Population shares use fixed anchors (all respondents; working-age) so the
#    column means the same thing in every row. Formality is computed over the
#    EMPLOYED within each sample, since it is undefined otherwise; every other
#    characteristic is over everyone in the sample.
#===============================================================================

cat("[5] Table 3: sample characteristics...\n")

#--- variable map. CHECK THESE AGAINST 02_variable_construction.R --------------
SEX_VAR      <- "SEXO"
MALE_CODE    <- 1L
AGE_VAR      <- "EDAD"
FORMAL_EXPR  <- quote(Employment_Status == "Formal")
HIGHER_EXPR  <- quote(GRUPO_EDUCACION %in% c("Universitario"))

# Which samples appear, in ladder order.
# NOTE: reg_shares is `employed` with filter TRUE, so the two rows are identical
# by construction. Both are listed on purpose - it documents that the share
# regressions impose nothing beyond `employed`. Drop one if you find it noisy.
TABLE3_SAMPLES <- c("all_individuals", "working_age", "active_pop", "employed",
                    "wage_earners", "income_earners", "private_employees",
                    "mw_covered", "reg_shares", "reg_variance", "reg_tier")

#-------------------------------------------------------------------------------
# Helper. svyquantile's return shape changed in survey 4.1; this handles both.
#-------------------------------------------------------------------------------

svy_median <- function(design, var) {
  if (nrow(design$variables) == 0L) return(NA_real_)
  q <- survey::svyquantile(stats::reformulate(var), design,
                           quantiles = 0.5, ci = FALSE, na.rm = TRUE)
  as.numeric(unlist(q))[1]
}

svy_share <- function(design, expr) {
  v <- eval(expr, design$variables)
  v[is.na(v)] <- FALSE
  if (!any(!is.na(v))) return(NA_real_)
  d <- update(design, .tmp = as.numeric(v))
  as.numeric(coef(svymean(~.tmp, d, na.rm = TRUE)))
}

#-------------------------------------------------------------------------------
# One row per sample.
#-------------------------------------------------------------------------------

sample_profile <- function(ids, period_expr = quote(TRUE), period_label = "Pooled") {
  
  subset_design <- function(d) {
    idx <- eval(period_expr, d$variables)
    if (length(idx) == 1L) idx <- rep(idx, nrow(d$variables))
    d[idx & !is.na(idx), ]
  }
  
  anchors <- lapply(c("all_individuals", "working_age"),
                    function(id) subset_design(samples[[id]]$design))
  anchor_pop <- vapply(anchors, function(d) sum(weights(d, "sampling")), numeric(1))
  
  purrr::map_dfr(ids, function(id) {
    
    d  <- subset_design(samples[[id]]$design)
    v  <- d$variables
    nq <- dplyr::n_distinct(v[[TIME_VAR]])
    w  <- sum(weights(d, "sampling"))
    
    # Formality is only defined for the employed, so it gets its own domain.
    emp <- v$OCUPADO == 1 & !is.na(v$OCUPADO)
    d_emp <- if (any(emp)) d[emp, ] else NULL
    
    tibble::tibble(
      period       = period_label,
      sample_id    = id,
      label        = samples[[id]]$label,
      n_rows       = nrow(v),
      pop_mean_qtr = w / nq,
      pct_of_total = w / anchor_pop[1],
      pct_of_wap   = if (id == "all_individuals") NA_real_ else w / anchor_pop[2],
      pct_formal   = if (is.null(d_emp)) NA_real_ else svy_share(d_emp, FORMAL_EXPR),
      pct_male     = svy_share(d, rlang::expr(!!rlang::sym(SEX_VAR) == !!MALE_CODE)),
      median_age   = svy_median(d, AGE_VAR),
      pct_higher   = svy_share(d, HIGHER_EXPR),
      # Not requested, but this is the live sample question and it is one line:
      # what share of each sample would the tier restriction remove.
      pct_dk       = svy_share(d, quote(firm_size_dk)),
      pct_not_asked = svy_share(d, quote(!has_tier & !firm_size_dk))
    )
  })
}

table3_pooled <- sample_profile(TABLE3_SAMPLES)

SNAPSHOT_QTR  <- "2025Q4"
table3_snap   <- sample_profile(
  TABLE3_SAMPLES,
  period_expr  = rlang::expr(!!rlang::sym(TIME_VAR) == !!SNAPSHOT_QTR),
  period_label = SNAPSHOT_QTR
)

table3_data <- dplyr::bind_rows(table3_pooled, table3_snap)

saveRDS(table3_data,
        file.path(config$paths$processed_data, "sample_characteristics.rds"))


#-------------------------------------------------------------------------------
# Render. One gt per period so each has its own population column meaning.
#-------------------------------------------------------------------------------

render_table3 <- function(dat, subtitle, number = "3") {
  
  dat %>%
    dplyr::select(-period, -label) %>%
    gt(rowname_col = "sample_id") %>%
    tab_header(title = sprintf("Table %s. Characteristics of the analysis samples", number),
               subtitle = subtitle) %>%
    cols_label(n_rows = "Person-quarters", pop_mean_qtr = "Population",
               pct_of_total = "of all respondents", pct_of_wap = "of working age",
               pct_formal = "Formal", pct_male = "Male",
               median_age = "Median age", pct_higher = "Higher education",
               pct_dk = "Asked, said don't know",
               pct_not_asked = "Never asked") %>%
    tab_spanner("Size",           columns = c(n_rows, pop_mean_qtr)) %>%
    tab_spanner("Population share", columns = c(pct_of_total, pct_of_wap)) %>%
    tab_spanner("Composition",    columns = c(pct_formal, pct_male,
                                              median_age, pct_higher)) %>%
    tab_spanner("Firm size reporting", columns = c(pct_dk, pct_not_asked)) %>%
    fmt_number(c(n_rows, pop_mean_qtr), decimals = 0, use_seps = TRUE) %>%
    fmt_percent(c(pct_of_total, pct_of_wap, pct_formal, pct_male,
                  pct_higher, pct_dk, pct_not_asked), decimals = 1) %>%
    fmt_number(median_age, decimals = 0) %>%
    sub_missing(everything(), missing_text = "\u2014") %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_stub(rows = sample_id %in% FRAME_CUTS)) %>%
    tab_footnote(
      paste("Average population represented in a typical quarter: expansion",
            "factors summed and divided by the number of quarters in the",
            "window. Not a count of distinct individuals."),
      locations = cells_column_labels(columns = pop_mean_qtr)) %>%
    tab_footnote(
      paste("Denominators are fixed across rows: all survey respondents, and",
            "the working-age population. Not the row above."),
      locations = cells_column_spanners("Population share")) %>%
    tab_footnote(
      paste("Formality is computed over the EMPLOYED within each sample, since",
            "it is undefined for the non-employed. All other characteristics",
            "are computed over everyone in the sample. Means are design-based."),
      locations = cells_column_labels(columns = pct_formal)) %>%
    tab_footnote(
      paste("The firm-size question is asked of every employed person except",
            "domestic workers. 'Never asked' is therefore mechanical - it is the",
            "share of the sample that is not employed, is self-employed, or is a",
            "domestic worker - and is shown only so the don't-know share can be",
            "read against the right denominator. 'Asked, said don't know' is the",
            "item non-response that the firm-size tier restriction would remove;",
            "it is imposed only on reg_tier."),
      locations = cells_column_spanners("Firm size reporting")) %>%
    tab_footnote(
      paste("Bold rows are the three regression frames. reg_shares is `employed`",
            "with no additional condition; the two rows are identical by",
            "construction and both are shown to make that explicit."),
      locations = cells_stub(rows = sample_id == "reg_shares")) %>%
    tab_source_note("Source: Authors' calculations using ENCFT.") %>%
    cols_align("right", columns = -sample_id) %>%
    tab_options(table.font.size = px(11), heading.title.font.size = px(14),
                heading.subtitle.font.size = px(11),
                column_labels.font.weight = "bold",
                table.border.top.style = "none",
                table_body.hlines.style = "none",
                data_row.padding = px(3))
}

table3 <- render_table3(
  table3_pooled, "Pooled",
  sprintf("ENCFT, %s-%s, pooled", qtr_range[1], qtr_range[2]))

table3b <- render_table3(
  table3_snap, SNAPSHOT_QTR,
  sprintf("ENCFT, %s", SNAPSHOT_QTR))

gtsave(table3,  tbl_path("table3_sample_characteristics", "html"))
gtsave(table3,  tbl_path("table3_sample_characteristics", "png"), expand = 10)
gtsave(table3b, tbl_path("table3b_sample_characteristics_2025Q4", "html"))
gtsave(table3b, tbl_path("table3b_sample_characteristics_2025Q4", "png"), expand = 10)



#===============================================================================
# STEP 6.  Table 4 — FRAME 2 OUTCOME-INPUT AUDIT
#===============================================================================

#===============================================================================
#  reg_shares imposes no data-availability filter. That is only defensible if
#  Employment_Status and Employment_Type are observed for every employed person
#  in every year, INCLUDING firm-size DK cases. Confirmed at the console; this
#  puts it in the output directory so the claim in the paper has a table behind
#  it and so a future data vintage cannot break it silently.
#===============================================================================

cat("[6] Table 4: Frame 2 outcome-input audit...\n")

table4_data <- df_reg_shares %>%
  group_by(year) %>%
  summarise(
    n              = n(),
    n_dk           = sum(firm_size_dk),
    miss_formality = sum(is.na(Employment_Status)),
    miss_type      = sum(is.na(Employment_Type)),
    dk_miss_form   = sum(firm_size_dk & is.na(Employment_Status)),
    .groups = "drop"
  )

# This is the claim. If it ever fails, reg_shares has a selected denominator and
# the share outcomes are not interpretable until it is resolved.
if (sum(table4_data$miss_formality) > 0 || sum(table4_data$miss_type) > 0) {
  warning("Frame 2 has missing outcome inputs. reg_shares denominator is ",
          "selected; do not run the share regressions until resolved.")
}

table4 <- table4_data %>%
  gt() %>%
  tab_header(
    title    = "Table 4. The share-outcome frame has no item non-response",
    subtitle = paste("Employed, working age. Outcome inputs by year,",
                     "including firm-size non-respondents")) %>%
  cols_label(year = "Year", n = "Employed", n_dk = "of which, firm size DK",
             miss_formality = "Formality", miss_type = "Employment type",
             dk_miss_form = "Formality, DK cases only") %>%
  tab_spanner("Records", columns = c(n, n_dk)) %>%
  tab_spanner("Missing outcome inputs",
              columns = c(miss_formality, miss_type, dk_miss_form)) %>%
  fmt_number(c(n, n_dk), decimals = 0, use_seps = TRUE) %>%
  fmt_number(c(miss_formality, miss_type, dk_miss_form), decimals = 0) %>%
  tab_footnote(
    paste("Formality and employment type are recorded independently of the",
          "firm-size question: formality follows RNC registration, not reported",
          "headcount. Firm-size non-respondents therefore retain a usable",
          "formality status, which is why they can be kept in this frame."),
    locations = cells_column_spanners("Missing outcome inputs")) %>%
  tab_source_note("Source: Authors' calculations using ENCFT.") %>%
  cols_align("right", columns = -year) %>%
  tab_options(table.font.size = px(11), heading.title.font.size = px(14),
              column_labels.font.weight = "bold",
              table.border.top.style = "none",
              table_body.hlines.style = "none", data_row.padding = px(3))

gtsave(table4, tbl_path("table4_frame2_audit", "html"))
gtsave(table4, tbl_path("table4_frame2_audit", "png"), expand = 10)

cat("\n[7] Tables 3, 3b and 4 written to: ", tbl_dir, "\n", sep = "")



#===============================================================================
# STEP 7.  Figure A1
#===============================================================================

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
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), n.breaks = 4) +
  scale_x_continuous(breaks = seq(2015, 2025, 2)) +
  labs(
    title    = paste("Firm-size reporting and salary response deteriorate; other movement reflects the labour market, not the data."),
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




