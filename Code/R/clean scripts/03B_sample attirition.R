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
source(here::here("Code", "R", "clean scripts", "03_sample_definitions.R"))

cat("=== 03B_sample diagnostics.R ===\n\n")

tbl_dir <- file.path(config$paths$outputs, config$output_stage,
                     config$out_subdirs$data_check)
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
    paste("Firm size is self-reported by the worker in bins (1-10, 11-19, 20-30,",
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

gtsave(table1, tbl_path("table1_attrition", "html"))


#===============================================================================
# STEP 4.  Table 2 — retention by year
#
#  Marginal retention only (share of the previous row). Columns are not
#  cumulative. Purpose: show the sample definition is stable over time, and
#  isolate the restrictions that are not.
#===============================================================================

cat("[3] Table 2: retention by year...\n")

# Only use years with all four quarters observed — a partial year is a noisier
# estimate and is not comparable across columns.
qtr_per_year <- design_full$variables %>%
  distinct(year, .data[[TIME_VAR]]) %>%
  count(year, name = "n_quarters")%>%
  tibble::as_tibble()

print(qtr_per_year, n = Inf)

years     <- qtr_per_year$year[qtr_per_year$n_quarters == 4]
year_cols <- as.character(years)

attr_by_year <- purrr::map_dfr(years, function(y) {
  attrition_table(REGRESSION_STEPS, design_full,
                  rlang::expr(year == !!y), as.character(y), se = FALSE)
})

table2_data <- attr_by_year %>%
  filter(step != "All person-quarter records") %>%
  select(period, step, pop_pct_prev) %>%
  tidyr::pivot_wider(names_from = period, values_from = pop_pct_prev)

m <- as.matrix(select(table2_data, all_of(year_cols)))
table2_data <- table2_data %>%
  mutate(change_pp = 100 * (m[, ncol(m)] - m[, 1]),
         range_pp  = 100 * (apply(m, 1, max, na.rm = TRUE) -
                              apply(m, 1, min, na.rm = TRUE)))

table2 <- table2_data %>%
  gt() %>%
  tab_header(
    title    = "Table 2. Stability of sample restrictions over time",
    subtitle = "Share of the previous row's population retained at each restriction, by year"
  ) %>%
  cols_label(step = "Restriction",
             change_pp = sprintf("%s\u2192%s", first(year_cols), last(year_cols)),
             range_pp = "Range") %>%
  tab_spanner("Retention rate", columns = all_of(year_cols)) %>%
  tab_spanner("Change (pp)",    columns = c(change_pp, range_pp)) %>%
  fmt_percent(all_of(year_cols), decimals = 1) %>%
  fmt_number(c(change_pp, range_pp), decimals = 1) %>%
  sub_missing(everything(), missing_text = "\u2014") %>%
  tab_style(style = list(cell_fill(color = "#FDF3E7"), cell_text(weight = "bold")),
            locations = cells_body(rows = step == "Firm size tier known")) %>%
  tab_footnote(
    paste("Each cell is the population retained at that restriction divided by",
          "the population retained at the restriction above it; columns are not",
          "cumulative. Population figures use survey expansion factors, averaged",
          "across the quarters in each year."),
    locations = cells_column_spanners("Retention rate")) %>%
  tab_footnote(
    "Percentage-point difference between first and last year, and between the maximum and minimum across years.",
    locations = cells_column_spanners("Change (pp)")) %>%
  tab_source_note(paste("Source: Authors' calculations using ENCFT. Years with",
                        "fewer than four observed quarters are omitted.")) %>%
  cols_align("left", columns = step) %>%
  cols_align("right", columns = -step) %>%
  tab_options(table.font.size = px(11), heading.title.font.size = px(14),
              heading.subtitle.font.size = px(11), column_labels.font.weight = "bold",
              table.border.top.style = "none", table_body.hlines.style = "none",
              data_row.padding = px(3))

gtsave(table2, tbl_path("table2_retention", "html"))

cat("\n[4] Tables written to: ", tbl_dir, "\n", sep = "")
cat("=== 03B_sample diagnostics.R complete ===\n\n")




