#===============================================================================
  # 09_summary tables.R
  #
  # PURPOSE
  #   Build the two descriptive tables for the paper:
  #
  #     TABLE 1  Summary statistics of employed workers, formal vs informal,
  #              at two reference periods (default 2016Q1 and 2025Q4).
  #
  #     TABLE 2  Evolution of earnings percentile ratios at four reference
  #              periods chosen around the MW/policy timeline.
  #
  #   Nothing downstream depends on this file. It is a paper deliverable.
  #
  # READS
  #   samples object (from 03_sample definitions.R)
  #
  # WRITES -> config$paths$processed_data / "Summary Tables":
  #   tab1_summary_stats.rds        long tibble, one row per statistic x cell
  #   tab1_memo_unemployed.rds      unemployment memo (not in the gt)
  #   tab2_pctiles.rds              long tibble of percentile levels + SEs
  #   tab2_ratios.rds               long tibble of percentile ratios
  #
  # WRITES -> config$paths$outputs / config$output_stage / desc_tables:
  #   table1_summary_stats.html
  #   table2_pctile_ratios.html
  #
  # DESIGN DECISIONS (see response/notes for fuller discussion)
  #   * "All workers" = samples$employed, i.e. employed, working-age, formality
  #     known. This is EMPLOYMENT, not the labor force: the unemployed have no
  #     formal/informal status. Unemployment is reported as a separate memo.
  #   * Two wage concepts are reported side by side. Salary-only is the MW-
  #     relevant concept but is defined on wage earners only, which drops the
  #     large share of informal workers whose earnings are independent income.
  #     Total primary labor income covers them. Reporting only one is misleading.
  #   * Education shares are HIGHEST LEVEL ATTENDED (GRUPO_EDUCACION), not
  #     completed, and the two rows are mutually exclusive categories.
  #   * Percentile ratios are scale-invariant, so real vs nominal is irrelevant
  #     for Table 2. Real is used anyway for consistency with 05A.
  #===============================================================================

source(here::here("Code", "R", "clean scripts", "00_setup.R"))

# NOTE: the filename is inconsistent across the repo — 03B sources
# "03_sample_definitions.R", 05A sources "03_Sample Definitions.R", and the
# file on disk is "03_sample definitions.R". This breaks on case-sensitive
# filesystems. Fix the callers; this script uses the on-disk name.
source(file.path(config$paths$scripts, "03_sample definitions.R"))

cat("=== 09_summary tables.R ===\n\n")


#===============================================================================
# STEP 0.  Parameters
#===============================================================================

# Each element: display label -> vector of year_quarter values to pool.
# Pooling >1 quarter averages out seasonality; population totals are divided
# by the number of quarters so they remain a per-quarter population level.
TAB1_PERIODS <- list(
  "2016Q4" = c("2016Q4"),
  "2025Q4" = c("2025Q4")
)

TAB2_PERIODS <- list(
  "2019Q4\n(pre-COVID)"                = c("2019Q4"),
  "2021Q2\n(pre-micro tier)"           = c("2021Q2"),
  "2023Q1\n(post-micro, pre-2023 MW)"  = c("2023Q1"),
  "2025Q4\n(latest)"                   = c("2025Q4")
)

# Percentiles needed to build every ratio below.
TAB2_PROBS <- c(0.10, 0.20, 0.40, 0.50, 0.80, 0.90)

# Ratio definitions: label -> c(numerator, denominator).
# NOTE: "p10/p40" is written as you specified it. It evaluates to a number
# BELOW 1 and moves in the opposite direction to every other row in the table,
# which will confuse a reader. If you meant p40/p10 (lower-tail compression,
# the usual MW diagnostic), flip the two entries.
TAB2_RATIOS <- list(
  "p90/p10" = c("p90", "p10"),
  "p90/p50" = c("p90", "p50"),
  "p50/p10" = c("p50", "p10"),
  "p40/p10" = c("p40", "p10"),
  "p80/p20" = c("p80", "p20")
)

# Earnings concept for Table 2. Monthly, consistent with 05A INEQ-5.
TAB2_WAGE_VAR <- "real_salary_income_wage_primary"

# Populations for Table 2. Names become row groups in the gt.
TAB2_POPULATIONS <- list(
  "All wage earners" = quote(TRUE),
  "Formal"           = quote(Employment_Status == "Formal"),
  "Informal"         = quote(Employment_Status == "Informal")
)

MIN_CELL_N <- 30   # flag cells thinner than this

out_data <- file.path(configdata_dirs$desc_tables)
dir.create(out_data, recursive = TRUE, showWarnings = FALSE)

out_tbl <- file.path(config$out_dirs$desc_tables)

dir.create(out_tbl, recursive = TRUE, showWarnings = FALSE)

save_rds <- function(obj, name) {
  saveRDS(obj, file.path(out_data, paste0(name, ".rds")))
  cat("  Saved:", file.path(out_data, paste0(name, ".rds")), "\n")
}


#===============================================================================
# STEP 1.  Helpers
#===============================================================================

# Attach a `period` factor to a design and drop everything outside the periods.
# Returns the design plus an attribute giving the number of quarters pooled per
# period (needed to rescale weighted totals).
add_period <- function(design, periods) {
  qmap <- unlist(lapply(names(periods), function(nm) {
    stats::setNames(rep(nm, length(periods[[nm]])), periods[[nm]])
  }))
  
  qq   <- as.character(design$variables$year_quarter)
  miss <- setdiff(unlist(periods), unique(qq))
  if (length(miss)) stop("Quarters not present in the data: ",
                         paste(miss, collapse = ", "))
  
  design$variables$period <- unname(qmap[qq])
  d <- design[!is.na(design$variables$period), ]
  d$variables$period <- factor(d$variables$period, levels = names(periods))
  
  attr(d, "n_qtr") <- vapply(periods, length, integer(1))
  d
}

# Apply `fun` to each cell of the design defined by `cell_vars`.
# `fun(d)` must return a tibble (>= 1 row). Cell keys and unweighted n are
# bound on. Cells with zero rows are skipped.
#
# design[idx, ] is the survey package's domain-estimation idiom: SEs are
# correct domain SEs, not naive subset SEs.
svy_cell <- function(design, cell_vars, fun) {
  dat  <- design$variables
  keys <- dplyr::distinct(dplyr::select(dat, dplyr::all_of(cell_vars)))
  keys <- dplyr::arrange(keys, dplyr::across(dplyr::all_of(cell_vars)))
  
  purrr::map_dfr(seq_len(nrow(keys)), function(i) {
    k   <- keys[i, , drop = FALSE]
    idx <- rep(TRUE, nrow(dat))
    for (v in cell_vars) idx <- idx & (as.character(dat[[v]]) == as.character(k[[v]]))
    idx[is.na(idx)] <- FALSE
    if (!any(idx)) return(NULL)
    
    res <- fun(design[idx, ])
    if (is.null(res) || nrow(res) == 0) return(NULL)
    dplyr::bind_cols(k[rep(1, nrow(res)), , drop = FALSE],
                     tibble::tibble(n_obs = sum(idx))[rep(1, nrow(res)), , drop = FALSE],
                     res)
  })
}

# --- single-statistic wrappers, all returning tibble(estimate, se) ---

f_total <- function(var) function(d) {
  x <- svytotal(stats::as.formula(paste0("~", var)), d, na.rm = TRUE)
  tibble::tibble(estimate = as.numeric(coef(x)), se = as.numeric(SE(x)))
}

f_mean <- function(var) function(d) {
  x <- svymean(stats::as.formula(paste0("~", var)), d, na.rm = TRUE)
  tibble::tibble(estimate = as.numeric(coef(x)), se = as.numeric(SE(x)))
}

f_median <- function(var) function(d) {
  keep <- !is.na(d$variables[[var]])
  if (sum(keep) < 2) return(NULL)
  x <- svyquantile(stats::as.formula(paste0("~", var)), d[keep, ],
                   quantiles = 0.5, ci = TRUE, na.rm = TRUE)
  tibble::tibble(estimate = as.numeric(coef(x)), se = as.numeric(SE(x)))
}

# Several quantiles at once -> long tibble(prob_label, estimate, se).
f_quantiles <- function(var, probs) function(d) {
  keep <- !is.na(d$variables[[var]]) & d$variables[[var]] > 0
  if (sum(keep) < 2) return(NULL)
  x <- svyquantile(stats::as.formula(paste0("~", var)), d[keep, ],
                   quantiles = probs, ci = TRUE, na.rm = TRUE)
  tibble::tibble(
    prob_label = paste0("p", as.integer(round(probs * 100))),
    estimate   = as.numeric(coef(x)),
    se         = as.numeric(SE(x))
  )
}


#===============================================================================
# STEP 2.  TABLE 1 — designs
#===============================================================================

cat("[1] Building Table 1 designs...\n")

# 2a. Characteristics design: all employed, formality known, no income filter.
#     Income conditioning here would bias composition statistics.
d1_emp <- add_period(samples$employed$design, TAB1_PERIODS)
d1_emp <- d1_emp[!is.na(d1_emp$variables$Employment_Status), ]

d1_emp <- update(
  d1_emp,
  one            = 1,
  age            = EDAD,
  is_male        = as.integer(Sex == "Male"),
  is_univ        = as.integer(edu4 == "Tertiary complete"),
  some_univ      = as.integer(edu4 == "Some tertiary"),
  is_secondary   = as.integer(edu4 == "Secondary complete"),
  less_secondary = as.integer(edu4 == "Less than secondary")
)

# 2b. Salary design: wage earners (salaried, positive real salary).
#     This is the MW-relevant earnings concept.
d1_wage <- add_period(samples$wage_earners$design, TAB1_PERIODS)

# 2c. Total-income design: all employed with positive total primary labor
#     income, including the self-employed. Covers informal own-account workers
#     who have no salary at all.
d1_inc <- add_period(samples$income_earners$design, TAB1_PERIODS)

n_qtr1 <- attr(d1_emp, "n_qtr")

cat(sprintf("    employed rows: %s | wage earners: %s | income earners: %s\n",
            format(nrow(d1_emp$variables), big.mark = ","),
            format(nrow(d1_wage$variables), big.mark = ","),
            format(nrow(d1_inc$variables), big.mark = ",")))


#===============================================================================
# STEP 3.  TABLE 1 — compute
#===============================================================================

cat("[2] Computing Table 1 statistics...\n")

CELLS <- c("period", "Employment_Status")

stat_row <- function(design, fun, block, label, fmt) {
  svy_cell(design, CELLS, fun) %>%
    dplyr::mutate(block = block, label = label, fmt = fmt)
}

tab1_long <- dplyr::bind_rows(
  
  # -- Size of employment ------------------------------------------------------
  stat_row(d1_emp, f_total("one"),
           "Employment", "Employment (persons)", "count"),
  
  # -- Earnings: salary concept ------------------------------------------------
  stat_row(d1_wage, f_mean(TAB2_WAGE_VAR),
           "Earnings: monthly salary, primary job (wage earners)",
           "Mean", "dop"),
  stat_row(d1_wage, f_median(TAB2_WAGE_VAR),
           "Earnings: monthly salary, primary job (wage earners)",
           "Median", "dop"),
  
  # -- Earnings: total labor income concept ------------------------------------
  stat_row(d1_inc, f_mean("real_total_income_all_primary"),
           "Earnings: monthly total labor income, primary job (all earners)",
           "Mean", "dop"),
  stat_row(d1_inc, f_median("real_total_income_all_primary"),
           "Earnings: monthly total labor income, primary job (all earners)",
           "Median", "dop"),
  
  # -- Composition -------------------------------------------------------------
  stat_row(d1_emp, f_mean("is_univ"),
           "Composition", "University (%)", "pct"),
  stat_row(d1_emp, f_mean("some_univ"),
           "Composition", "Some University (%)", "pct"),
  stat_row(d1_emp, f_mean("is_secondary"),
           "Composition", "Secondary (%)", "pct"),
  stat_row(d1_emp, f_mean("less_secondary"),
           "Composition", "Less than Secondary (%)", "pct"),
  stat_row(d1_emp, f_mean("age"),
           "Composition", "Age (mean years)", "num"),
  stat_row(d1_emp, f_mean("is_male"),
           "Composition", "Male (%)", "pct")
)

# Rescale weighted totals to a per-quarter population when periods pool
# multiple quarters. No-op when each period is a single quarter.
tab1_long <- tab1_long %>%
  dplyr::mutate(
    nq       = n_qtr1[as.character(period)],
    estimate = dplyr::if_else(fmt == "count", estimate / nq, estimate),
    se       = dplyr::if_else(fmt == "count", se / nq, se)
  ) %>%
  dplyr::select(-nq)

# Employment share: derived from the counts so it is internally consistent
# with the row above it (shares sum to 100 within period by construction).
emp_share <- tab1_long %>%
  dplyr::filter(label == "Employment (persons)") %>%
  dplyr::group_by(period) %>%
  dplyr::mutate(estimate = estimate / sum(estimate),
                se       = NA_real_,
                block    = "Employment",
                label    = "Employment share (%)",
                fmt      = "pct") %>%
  dplyr::ungroup()

tab1_long <- dplyr::bind_rows(tab1_long, emp_share) %>%
  dplyr::mutate(
    sparse = n_obs < MIN_CELL_N,
    label  = factor(label, levels = c(
      "Employment (persons)", "Employment share (%)",
      "Mean", "Median",
      "University (%)", "Some University (%)", "Secondary (%)", "Less than Secondary (%)", "Age (mean years)", "Male (%)"
    ))
  ) %>%
  dplyr::arrange(block, label, period, Employment_Status)

if (any(tab1_long$sparse)) {
  cat("  WARNING: cells with n <", MIN_CELL_N, "unweighted obs:\n")
  print(dplyr::filter(tab1_long, sparse) %>%
          dplyr::select(block, label, period, Employment_Status, n_obs))
}

save_rds(tab1_long, "tab1_summary_stats")


# -- Memo: unemployment, which has no formal/informal split --------------------
d1_act <- add_period(samples$active_pop$design, TAB1_PERIODS)
d1_act <- update(d1_act, is_unemp = as.integer(OCUPADO != 1))

tab1_memo <- dplyr::bind_rows(
  svy_cell(d1_act, "period", f_total("is_unemp")) %>%
    dplyr::mutate(label = "Unemployed (persons)"),
  svy_cell(d1_act, "period", f_mean("is_unemp")) %>%
    dplyr::mutate(label = "Unemployment rate (share of PEA)")
) %>%
  dplyr::mutate(nq = n_qtr1[as.character(period)],
                estimate = dplyr::if_else(label == "Unemployed (persons)",
                                          estimate / nq, estimate),
                se = dplyr::if_else(label == "Unemployed (persons)",
                                    se / nq, se)) %>%
  dplyr::select(-nq)

cat("\n  Memo — unemployment (not classified by formality):\n")
print(tab1_memo)
save_rds(tab1_memo, "tab1_memo_unemployed")


#===============================================================================
# STEP 4.  TABLE 1 — render
#===============================================================================

cat("\n[3] Rendering Table 1...\n")

tab1_wide <- tab1_long %>%
  dplyr::mutate(col = paste(period, Employment_Status, sep = "__")) %>%
  dplyr::select(block, label, fmt, col, estimate) %>%
  tidyr::pivot_wider(names_from = col, values_from = estimate) %>%
  dplyr::arrange(factor(block, levels = unique(tab1_long$block)), label)

val_cols <- setdiff(names(tab1_wide), c("block", "label", "fmt"))

table1 <- tab1_wide %>%
  gt(groupname_col = "block", rowname_col = "label") %>%
  tab_header(
    title    = "Table 1. Summary statistics, employed workers",
    subtitle = sprintf("Formal vs informal, %s",
                       paste(names(TAB1_PERIODS), collapse = " and "))
  ) %>%
  fmt_number(columns = all_of(val_cols), rows = fmt == "count",
             decimals = 0, use_seps = TRUE) %>%
  fmt_number(columns = all_of(val_cols), rows = fmt == "dop",
             decimals = 0, use_seps = TRUE) %>%
  fmt_percent(columns = all_of(val_cols), rows = fmt == "pct", decimals = 1) %>%
  fmt_number(columns = all_of(val_cols), rows = fmt == "num", decimals = 1) %>%
  cols_hide(columns = "fmt") %>%
  sub_missing(everything(), missing_text = "\u2014") %>%
  cols_align("right", columns = all_of(val_cols)) %>%
  tab_source_note(paste(
    "Source: Authors' calculations using ENCFT, survey-weighted",
    "(FACTOR_EXPANSION), stratified two-stage design.")) %>%
  tab_footnote(paste(
    "Population is EMPLOYED working-age persons with known formality status,",
    "not the labor force: the unemployed cannot be classified formal or",
    "informal. See the unemployment memo for the missing group.")) %>%
  tab_footnote(paste(
    "Salary rows cover salaried workers with positive salary only. Total labor",
    "income rows add independent income and cover self-employed workers, who",
    "are a large share of informal employment. The two informal columns are",
    "therefore computed on different populations and are not comparable to",
    "each other.")) %>%
  tab_footnote(paste(
    "Education refers to completed levels of education only.")) %>%
  tab_options(table.font.size = px(11), heading.title.font.size = px(14),
              heading.subtitle.font.size = px(11),
              column_labels.font.weight = "bold",
              table.border.top.style = "none",
              data_row.padding = px(3))

# Column spanners, one per period.
for (p in names(TAB1_PERIODS)) {
  cols_p <- val_cols[startsWith(val_cols, paste0(p, "__"))]
  table1 <- table1 %>%
    tab_spanner(label = p, columns = all_of(cols_p))
}
table1 <- table1 %>%
  cols_label(.list = stats::setNames(
    as.list(sub("^.*__", "", val_cols)), val_cols))

gtsave(table1, file.path(out_tbl, "table1_summary_stats.html"))
gtsave(table1, file.path(out_tbl, "table1_summary_stats.png"))

cat("  Saved:", file.path(out_tbl, "table1_summary_stats.html"), "\n")


#===============================================================================
# STEP 5.  TABLE 2 — compute
#===============================================================================

cat("\n[4] Computing Table 2 percentiles...\n")

d2 <- add_period(samples$wage_earners$design, TAB2_PERIODS)

tab2_pctiles <- purrr::imap_dfr(TAB2_POPULATIONS, function(cond, pop_name) {
  idx <- eval(cond, d2$variables)
  if (length(idx) == 1L) idx <- rep(idx, nrow(d2$variables))
  idx[is.na(idx)] <- FALSE
  
  svy_cell(d2[idx, ], "period", f_quantiles(TAB2_WAGE_VAR, TAB2_PROBS)) %>%
    dplyr::mutate(population = pop_name)
}) %>%
  dplyr::mutate(sparse = n_obs < MIN_CELL_N)

if (any(tab2_pctiles$sparse)) {
  cat("  WARNING: thin cells in Table 2:\n")
  print(dplyr::distinct(dplyr::filter(tab2_pctiles, sparse),
                        population, period, n_obs))
}

save_rds(tab2_pctiles, "tab2_pctiles")

# Form the ratios.
pct_wide <- tab2_pctiles %>%
  dplyr::select(population, period, prob_label, estimate) %>%
  tidyr::pivot_wider(names_from = prob_label, values_from = estimate)

tab2_ratios <- purrr::imap_dfr(TAB2_RATIOS, function(pair, lab) {
  pct_wide %>%
    dplyr::transmute(population, period, ratio = lab,
                     value = .data[[pair[1]]] / .data[[pair[2]]])
}) %>%
  dplyr::left_join(
    dplyr::distinct(tab2_pctiles, population, period, n_obs, sparse),
    by = c("population", "period")
  ) %>%
  dplyr::mutate(
    population = factor(population, levels = names(TAB2_POPULATIONS)),
    ratio      = factor(ratio, levels = names(TAB2_RATIOS))
  ) %>%
  dplyr::arrange(population, ratio, period)

save_rds(tab2_ratios, "tab2_ratios")

cat("  Ratios computed for", dplyr::n_distinct(tab2_ratios$population),
    "populations x", dplyr::n_distinct(tab2_ratios$period), "periods\n")


#===============================================================================
# STEP 6.  TABLE 2 — render
#===============================================================================

cat("\n[5] Rendering Table 2...\n")

tab2_wide <- tab2_ratios %>%
  dplyr::select(population, ratio, period, value) %>%
  tidyr::pivot_wider(names_from = period, values_from = value) %>%
  dplyr::arrange(population, ratio)

period_cols <- names(TAB2_PERIODS)

table2 <- tab2_wide %>%
  gt(groupname_col = "population", rowname_col = "ratio") %>%
  tab_header(
    title    = "Table 2. Evolution of earnings inequality ratios",
    subtitle = "Percentile ratios of real monthly salary, primary job, wage earners"
  ) %>%
  fmt_number(columns = all_of(period_cols), decimals = 2) %>%
  sub_missing(everything(), missing_text = "\u2014") %>%
  cols_align("right", columns = all_of(period_cols)) %>%
  tab_source_note(paste(
    "Source: Authors' calculations using ENCFT, survey-weighted",
    "(FACTOR_EXPANSION). Percentiles estimated with survey::svyquantile.")) %>%
  tab_footnote(paste(
    "Ratios are scale-invariant, so deflation does not affect them; real",
    "values are used for consistency with the rest of the analysis.")) %>%
  tab_footnote(paste(
    "Single-quarter estimates. Tail percentiles (p10, p90) carry the largest",
    "sampling error; see tab2_pctiles.rds for percentile-level standard",
    "errors. Ratio standard errors require replicate weights",
    "(survey::as.svrepdesign) and are not reported.")) %>%
  tab_options(table.font.size = px(11), heading.title.font.size = px(14),
              heading.subtitle.font.size = px(11),
              column_labels.font.weight = "bold",
              table.border.top.style = "none",
              data_row.padding = px(3))

gtsave(table2, file.path(out_tbl, "table2_pctile_ratios.html"))
gtsave(table2, file.path(out_tbl, "table2_pctile_ratios.png"))
cat("  Saved:", file.path(out_tbl, "table2_pctile_ratios.html"), "\n")

cat("\n=== 09_summary tables.R complete ===\n\n")


samples$wage_earners$data %>%
  group_by(year_quarter) %>%
  summarise(
    pct_round_1000 = weighted.mean(salary_income_primary %% 1000 == 0,
                                   FACTOR_EXPANSION) * 100,
    pct_round_5000 = weighted.mean(salary_income_primary %% 5000 == 0,
                                   FACTOR_EXPANSION) * 100
  ) %>% print(n = Inf)


samples$wage_earners$data %>%
  mutate(bin = cut(salary_income_primary,
                   breaks = c(10000, 15000, 20000, 30000, 50000))) %>%
  filter(!is.na(bin)) %>%
  group_by(year, bin) %>%
  summarise(
    pct_round_1000 = weighted.mean(salary_income_primary %% 1000 == 0,
                                   FACTOR_EXPANSION) * 100,
    n = n(), .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = bin, values_from = c(pct_round_1000, n)) %>%
  print(n = Inf)

jump_at <- function(x, w, p) {
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  v  <- x[which(cw >= p)[1]]
  sum(w[x == v]) / sum(w) * 100      # % of mass sitting on that value
}

samples$wage_earners$data %>%
  group_by(year_quarter) %>%
  summarise(across(everything(), ~NULL),
            p10_jump = jump_at(salary_income_primary, FACTOR_EXPANSION, .10),
            p50_jump = jump_at(salary_income_primary, FACTOR_EXPANSION, .50),
            p90_jump = jump_at(salary_income_primary, FACTOR_EXPANSION, .90))

