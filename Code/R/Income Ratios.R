source("Code/R/00_setup.R")




# ---- 1. Load Data and Set Basic Design ---- #
Full_ENCFT_clean <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))


#function to create unique PSU/STRATA
individual_level_unique_id <- check_and_fix_survey_ids(Full_ENCFT_clean, psu_var = "UPM", strata_var = "ESTRATO", time_var = "year_quarter")


#set overall design for individual level quarterly analysis
design_indiv_q <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = individual_level_unique_id, nest = TRUE)


# ---- Define Helper Functions ---- #
# This function is intended to be used as compute_fn inside run_specs().
# run_specs() will pass design, var, time_var, group_var. :contentReference[oaicite:2]{index=2}
compute_log_quantile_ratio <- function(design,
                                       var,
                                       time_var,
                                       group_var = NULL,
                                       p_hi,
                                       p_lo,
                                       quantile_base = "p",
                                       method = "default",
                                       na.rm = TRUE,
                                       fast = TRUE,
                                       return_se = FALSE,
                                       return_ci = FALSE,
                                       level = 0.95) {
  
  # Compute only the two quantiles needed for this ratio
  q <- compute_quantiles(
    design = design,
    var = var,
    time_var = time_var,
    group_var = group_var,
    probs = c(p_lo, p_hi),
    na.rm = na.rm,
    method = method,
    quantile_base = quantile_base,
    fast = fast,
    return_se = FALSE,  # ratio SE/CI not supported here (set NA below)
    return_ci = FALSE,
    level = level
  )
  
  # Expect measure labels like p10, p50, p90, etc.
  hi_lab <- paste0(quantile_base, as.integer(round(p_hi * 100)))
  lo_lab <- paste0(quantile_base, as.integer(round(p_lo * 100)))
  
  # Wide per (time, group) then compute log ratio
  id_cols <- c("time", if (!is.null(group_var)) "group")
  qw <- q %>%
    select(any_of(id_cols), measure, estimate) %>%
    tidyr::pivot_wider(names_from = measure, values_from = estimate)
  
  # Safe log ratio: require both positive and finite
  hi_val <- qw[[hi_lab]]
  lo_val <- qw[[lo_lab]]
  
  est <- ifelse(is.finite(hi_val) & is.finite(lo_val) & hi_val > 0 & lo_val > 0,
                log(hi_val / lo_val),
                NA_real_)
  
  out <- qw %>%
    transmute(
      time = .data$time,
      !!if (!is.null(group_var)) rlang::sym("group") else NULL,
      estimate = est,
      measure = paste0("log_", hi_lab, "_", lo_lab)
    )
  
  # Keep schema stable if caller asked for variance columns
  if (isTRUE(return_se) && !"se" %in% names(out)) out$se <- NA_real_
  if (isTRUE(return_ci)) {
    if (!"ci_l" %in% names(out)) out$ci_l <- NA_real_
    if (!"ci_u" %in% names(out)) out$ci_u <- NA_real_
  }
  
  out
}


# ---- 3. Set up Spec Table ---- #


# --each row of the table represents a population sub group for analysis -- #

outcomes <- c(
  salary = "real_salary_income_total",
  indep  = "real_independent_income_total",
  total  = "real_total_income_total"
)

outcome_labels <- c(
  salary = "Real salary income - All Jobs",
  indep  = "Real independent income - All Jobs",
  total  = "Real total labor income - All Jobs"
)

base_filters <- list(
  salary = quote(real_salary_income_total > 0),
  indep  = quote(real_independent_income_total > 0),
  total  = quote(real_total_income_total > 0)
)

pop <- list(
  employed = quote(OCUPADO == 1)
)

pop_labels <- c(
  employed = " – Employed Population"
)

log_ratio_specs <- make_spec_table(
  outcomes           = outcomes,
  outcome_labels     = outcome_labels,
  populations        = pop,
  population_labels  = pop_labels,
  base_filters       = base_filters
)



# ---- 4. Configurations for Specs Run ---- #

# Quantiles needed for all ratios you want to plot
quantile_probs <- c(0.05, 0.10, 0.20, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95)

# Define log-ratio "runs" (one ratio per run keeps plotting unchanged)
ratio_plan <- tibble::tribble(
  ~run_name,        ~p_hi,  ~p_lo,
  "Log p95_p50",    0.95,   0.50,
  "Log p90_p10",    0.90,   0.10,
  "Log p50_p10",    0.50,   0.10,
  "Log p50_p05",    0.50,   0.05,
  "Log p80_p20",    0.80,   0.20,
  "Log p80_p10",    0.80,   0.10,
  "Log p70_p10",    0.70,   0.10,
  "Log p60_p10",    0.60,   0.10
)

# Cell adequacy thresholds (recommended stricter for quantiles/ratios)
adequacy_on <- TRUE
adequacy_args <- list(
  min_cell_n = 50L,
  min_cell_neff = 25
)

group_plan <- c(
  None      = NA_character_,
  Sex       = "Sex",
  Education = "education",
  Regions   = "Region4",
  Formality = "Employment_Status",
  Size      = "Wage_group"
)

# ---- 5. Run Specs for Ratios x Groups ---- #


# Each ratio is a "run", each group is a "group"
jobs <- tidyr::expand_grid(
  run   = ratio_plan$run_name,
  group = names(group_plan)
) %>%
  mutate(
    p_hi     = ratio_plan$p_hi[match(run, ratio_plan$run_name)],
    p_lo     = ratio_plan$p_lo[match(run, ratio_plan$run_name)],
    group_var = unname(group_plan[group])
  )

run_one_ratio <- function(p_hi, p_lo, group_var) {
  run_specs(
    specs      = log_ratio_specs,
    design     = design_indiv_q,
    compute_fn = compute_log_quantile_ratio,
    time_var   = "year_quarter",
    group_var  = if (is.na(group_var)) NULL else group_var,
    adequacy   = adequacy_on,
    adequacy_args = adequacy_args,
    # forwarded to compute_fn via ...
    p_hi = p_hi,
    p_lo = p_lo,
    return_se = FALSE
  )
}

jobs <- jobs %>%
  mutate(
    data = pmap(list(p_hi, p_lo, group_var), run_one_ratio)
  )

# ---- 6. Plots Generation ---- #


# ---- build nested plots: ratio -> group -> plotlist ----
all_plots <- purrr::set_names(ratio_plan$run_name) %>%
  purrr::map(function(rn) {
    purrr::set_names(names(group_plan)) %>%
      purrr::map(function(gn) {
        plots_from_jobs(jobs, run_name = rn, group_name = gn)
      })
  })



# ---- 7. Saving Plots ---- #


# ---- base chart directory from config ----
charts_dir <- file.path(
  ".", config$paths$outputs, config$output_stage, config$out_subdirs$charts
)


ineq_dir      <- file.path(charts_dir, "Income Inequality Trends")
pdf_dir       <- ineq_dir
png_base_dir  <- file.path(ineq_dir, "Log Ratios")


# Put these PDFs under Charts/Income Inequality Trends/Log Ratios/pdf/
pdf_dir <- file.path(charts_dir, "Income Inequality Trends", "Log Ratios", "pdf")
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)

save_fmt <- config$fig_defaults$format

# ---- helpers ----
# "Log p90_p10" -> "p90_p10"
ratio_slug <- function(run_name) {
  x <- gsub("^Log\\s+", "", run_name)
  gsub("[^A-Za-z0-9_\\-]+", "_", x)
}


purrr::iwalk(all_plots, function(by_group, run_name) {
  
  # ---- 1) Save PNGs into ratio folder (no group subfolders) ----
  rdir <- file.path(png_base_dir, ratio_slug(run_name))
  dir.create(rdir, recursive = TRUE, showWarnings = FALSE)
  
  # Flatten all group plotlists into one list for saving PNGs
  # Prefix names with group to keep filenames unique/readable
  png_plots <- list()
  for (gn in names(by_group)) {
    pl <- by_group[[gn]]
    if (!length(pl)) next
    pl <- rlang::set_names(pl, paste0(gn, "__", names(pl)))
    png_plots <- c(png_plots, pl)
  }
  
  save_plots_dir(
    plot_list = png_plots,
    dir       = rdir,
    format    = save_fmt,
    width     = config$fig_defaults$width,
    height    = config$fig_defaults$height,
    dpi       = config$fig_defaults$dpi
  )
  
  # ---- 2) Save ONE PDF per ratio in Income Inequality Trends/ ----
  flat <- list()
  sections <- list()
  
  for (gn in names(by_group)) {
    pl <- by_group[[gn]]
    if (!length(pl)) next
    
    start <- length(flat) + 1L
    flat <- c(flat, pl)
    end <- length(flat)
    
    sections[[gn]] <- start:end   # section headers: None, Sex, Education, ...
  }
  
  pdf_file <- file.path(pdf_dir, paste0(ratio_slug(run_name), ".pdf"))
  
  save_plots_pdf_grid(
    plot_list         = flat,
    file              = pdf_file,
    ncol              = 1,
    nrow              = 2,
    break_sections    = TRUE,
    sections          = sections,
    add_page_numbers  = TRUE
  )
})


































plots_log_p95_p50 <- c(
  plots_from_jobs(jobs, run_name = "Log p95_p50", group_name = "None"),
  plots_from_jobs(jobs, run_name = "Log p95_p50", group_name = "Sex"),
  plots_from_jobs(jobs, run_name = "Log p95_p50", group_name = "Education"),
  plots_from_jobs(jobs, run_name = "Log p95_p50", group_name = "Regions"),
  plots_from_jobs(jobs, run_name = "Log p95_p50", group_name = "Formality"),
  plots_from_jobs(jobs, run_name = "Log p95_p50", group_name = "Size")
)


plots_log_p90_p10 <- c(
  plots_from_jobs(jobs, run_name = "Log p90_p10", group_name = "None"),
  plots_from_jobs(jobs, run_name = "Log p90_p10", group_name = "Sex"),
  plots_from_jobs(jobs, run_name = "Log p90_p10", group_name = "Education"),
  plots_from_jobs(jobs, run_name = "Log p90_p10", group_name = "Regions"),
  plots_from_jobs(jobs, run_name = "Log p90_p10", group_name = "Formality"),
  plots_from_jobs(jobs, run_name = "Log p90_p10", group_name = "Size")
)

plots_log_p80_p20 <- c(
  plots_from_jobs(jobs, run_name = "Log p80_p20", group_name = "None"),
  plots_from_jobs(jobs, run_name = "Log p80_p20", group_name = "Sex"),
  plots_from_jobs(jobs, run_name = "Log p80_p20", group_name = "Education"),
  plots_from_jobs(jobs, run_name = "Log p80_p20", group_name = "Regions"),
  plots_from_jobs(jobs, run_name = "Log p80_p20", group_name = "Formality"),
  plots_from_jobs(jobs, run_name = "Log p80_p20", group_name = "Size")
)

plots_log_p80_p10 <- c(
  plots_from_jobs(jobs, run_name = "Log p80_p10", group_name = "None"),
  plots_from_jobs(jobs, run_name = "Log p80_p10", group_name = "Sex"),
  plots_from_jobs(jobs, run_name = "Log p80_p10", group_name = "Education"),
  plots_from_jobs(jobs, run_name = "Log p80_p10", group_name = "Regions"),
  plots_from_jobs(jobs, run_name = "Log p80_p10", group_name = "Formality"),
  plots_from_jobs(jobs, run_name = "Log p80_p10", group_name = "Size")
)

plots_log_p70_p10 <- c(
  plots_from_jobs(jobs, run_name = "Log p70_p10", group_name = "None"),
  plots_from_jobs(jobs, run_name = "Log p70_p10", group_name = "Sex"),
  plots_from_jobs(jobs, run_name = "Log p70_p10", group_name = "Education"),
  plots_from_jobs(jobs, run_name = "Log p70_p10", group_name = "Regions"),
  plots_from_jobs(jobs, run_name = "Log p70_p10", group_name = "Formality"),
  plots_from_jobs(jobs, run_name = "Log p70_p10", group_name = "Size")
)

plots_log_p60_p10 <- c(
  plots_from_jobs(jobs, run_name = "Log p60_p10", group_name = "None"),
  plots_from_jobs(jobs, run_name = "Log p60_p10", group_name = "Sex"),
  plots_from_jobs(jobs, run_name = "Log p60_p10", group_name = "Education"),
  plots_from_jobs(jobs, run_name = "Log p60_p10", group_name = "Regions"),
  plots_from_jobs(jobs, run_name = "Log p60_p10", group_name = "Formality"),
  plots_from_jobs(jobs, run_name = "Log p60_p10", group_name = "Size")
)

plots_log_p50_p05 <- c(
  plots_from_jobs(jobs, run_name = "Log p50_p05", group_name = "None"),
  plots_from_jobs(jobs, run_name = "Log p50_p05", group_name = "Sex"),
  plots_from_jobs(jobs, run_name = "Log p50_p05", group_name = "Education"),
  plots_from_jobs(jobs, run_name = "Log p50_p05", group_name = "Regions"),
  plots_from_jobs(jobs, run_name = "Log p50_p05", group_name = "Formality"),
  plots_from_jobs(jobs, run_name = "Log p50_p05", group_name = "Size")
)

plots_log_p50_p10 <- c(
  plots_from_jobs(jobs, run_name = "Log p50_p10", group_name = "None"),
  plots_from_jobs(jobs, run_name = "Log p50_p10", group_name = "Sex"),
  plots_from_jobs(jobs, run_name = "Log p50_p10", group_name = "Education"),
  plots_from_jobs(jobs, run_name = "Log p50_p10", group_name = "Regions"),
  plots_from_jobs(jobs, run_name = "Log p50_p10", group_name = "Formality"),
  plots_from_jobs(jobs, run_name = "Log p50_p10", group_name = "Size")
)






all_plots <- purrr::map(
  ratio_plan$run_name,
  ~ plots_from_jobs(jobs, run_name = .x, group_name = "Sex")
)

names(all_plots) <- ratio_plan$run_name



# ---- 7. Savings Plots and PDF ---- #






