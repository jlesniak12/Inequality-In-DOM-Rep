source("Code/R/00_setup.R")



# ---- 1. Load Data and Fix Ids ---- #
Full_ENCFT_clean <- readRDS(file.path(config$paths$processed_data, "Full_ENCFT_clean.rds"))


table(Full_ENCFT_clean$Employment_Type)
table(Full_ENCFT_clean$CATEGORIA_PRINCIPAL)


Full_ENCFT_clean %>%
  filter(Employment_Type == "unclassified") %>%
  count(OCUPADO, Employment_Status) %>%  # or whatever your formality var is
  print()


Full_ENCFT_clean %>%
  filter(OCUPADO == 1, Employment_Type == "private employee") %>%
  count(CATEGORIA_PRINCIPAL) %>%
  mutate(share = n / sum(n))



# ---- 2. Simplify keeping needed variables and create sub samples ---- #

# -- simnplify dataset keeping only needed vars -- #

srvy_vars <- c("psu_unique", "strata_unique", "weight_annual", "weight_quarter", "FACTOR_EXPANSION", "DES_ESTRATO",
               "quarter", "month", "year", "year_quarter")

analysis_vars <- c(
  # -core income and min wage
  
  "real_salary_income_total",
  "real_salary_income_primary",
  "real_independent_income_primary",
  "real_total_income_total",
  "real_adj_income_primary",
  
  "Wage_group",             #defines firm size based on official buckets of min wage (micro small med large)
  "real_minwage_harmonized",
  
  #  -other traits
  
  "Employment_Sector",      #sector of work
  "Employment_Status",      #formal vs informal,
  "Employment_Type",        # employment class
  "OCUPADO"                 #binary for currently employed
)



#key variables
analysis_all <- individual_level_unique_id %>%
  select(
    all_of(srvy_vars),
    all_of(analysis_vars)
  )


#set overall design for individual level quarterly analysis
design <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = analysis_all,
                            nest = TRUE)



# ---- 3. Set up table of specs ---- #


outcomes <- c(
  "Primary Salary Income" = "real_salary_income_primary",
  "Adjusted Income" = "real_adj_income_primary",
  "Independent Primary Income" = "real_independent_income_primary",
  "Total Primary Income" = "real_total_income_total"
)

populations <- list(
  "Total Population"                                 = NULL,
  "Employed non Gov"                                 = quote(OCUPADO == 1 & !(Employment_Sector == "Government")),
  "Employed Private"                                 = quote(OCUPADO == 1 &  !(Employment_Sector == "Government") & Employment_Type == "private employee"),
  "Employed non Private"                             = quote(OCUPADO == 1 &  !(Employment_Sector == "Government") & !(Employment_Type == "private employee")),
  "Employed Private Min Wage Group"                  = quote(OCUPADO == 1 & !(Employment_Sector == "Government") & Employment_Type == "private employee"  
                                                          & !is.na(Wage_group) & Wage_group != "Don't know"),
  "Formal Private Min Wage Group"                    = quote(OCUPADO == 1 & Employment_Status == "Formal" & !(Employment_Sector == "Government") & Employment_Type == "private employee"  
                                                          & !is.na(Wage_group) & Wage_group != "Don't know")
)


for (pop in names(populations)) {

  print(pop)
  print(populations[[pop]])
  print(grepl("Formal", populations[[pop]] ))
  print(grepl("Formal", pop))

  # -- Basic Indicators No Income Variation -- #
  
  test_N_W <- compute_pop_total(
    design = design,
    subset_expr = populations[[pop]],
    time_var = "year_quarter",
    group_var = "Employment_Sector"
  ) %>%
    mutate(measure = "Obs Count, Survey Weighted")
  
  test_n_unw <- compute_n_unw(
    design = design,
    subset_expr = populations[[pop]],
    time_var = "year_quarter",
    group_var = "Employment_Sector"
  ) %>%
    mutate(measure = "Obs Count, Unweighted")
  
  
  # -- Formal and Informal (if both included in population subset) -- #
  
  #if not Formal do calculations
  if (!((grepl("Formal", pop)))) {
    test_shares <- compute_prop_factor(design = design,
                                       subset_expr = populations[[pop]],
                                       time_var = "year_quarter",
                                       var = "Employment_Status",
                                       group_var = "Employment_Sector"
    ) %>% filter(!is.na(estimate)) %>%
      mutate(measure = paste(measure, " ", level, sep ="")) %>%
      select(-level)
    
    formal_share <- test_shares %>%
      filter(measure == "share Formal")
    
    formal_rank <- ranking(
      df = filter(test_shares, measure == "share Formal"),
      rank_var = "estimate",
      time_var ="time",
      rank_col  = "rank"
    ) %>%
      mutate(estimate = rank,
             measure = "Ranking, Formal Worker Share") %>%
      select(-rank)
    
    informal_share <- test_shares %>%
      filter(measure == "share Informal")
    
    informal_rank <- ranking(
      df = filter(test_shares, measure == "share Informal"),
      rank_var = "estimate",
      time_var ="time",
      rank_col  = "rank"
    ) %>%
      mutate(estimate = rank,
             measure = "Ranking, Informal Worker Share") %>%
      select(-rank)
    
    df_status <- list(formal_share, formal_rank, informal_share, informal_rank)
    status_data <- bind_rows(df_status)
  }
  
  
  # ---- Calculations with Income Variation ---- #
  df_inc <- list()
  
  for (inc in names(outcomes)) {
    
    test_checks <- compute_checks(
      design = design,
      subset_expr = populations[[pop]],
      var = outcomes[[inc]],
      cut_var = "real_minwage_harmonized",
      time_var = "year_quarter",
      group_var = "Employment_Sector"
    )
    
    test_checks <- test_checks %>%
      mutate(measure = paste(measure, inc))
    
    df_inc[[inc]] <- test_checks
    
    # ---- Inequality: p90/p50 (values + ranking) ----
    ineq <- compute_ineq_q9050(
      design      = design,
      subset_expr = populations[[pop]],
      time_var    = "year_quarter",
      group_var   = "Employment_Sector",
      var         = outcomes[[inc]],
      probs       = c(0.5, 0.9),
      ratio_name  = "Inequality (p90/p50)"
    ) |>
      dplyr::mutate(measure = paste(.data$measure, inc))
    
    # add to the same list you bind later
    df_inc[[paste0(inc, "__ineq")]] <- ineq
    
    
    
  }
  
  inc_data <- bind_rows(df_inc)
  
  # ---- Process into Wide Tables list ---- #
  
  
  #remove formal/informal breakdown when not needed
  if (grepl("Formal", pop)) {
    
    tbls <- bind_rows(test_N_W, test_n_unw, inc_data)
  } else {
    
    tbls <- bind_rows(test_N_W, test_n_unw, status_data, inc_data)
  }
  
  
  wide_tbls <- build_table_wide(tbls, id_cols = c("measure", "group"), col_keys = "time")
  
  list_all_tables <- split_by_indicator(wide_tbls, group_var = "group", indicator_col = "measure")
  
 # ---- Create Sheet Mapping and Save ---- #
    
  sheets_map <- list(
    "Counts" = simplify(lapply(names(list_all_tables), grep, pattern = "Obs Count,", value = TRUE)),
    
    "Zeros" = simplify(lapply(names(list_all_tables), grep, pattern = "share_zero", value = TRUE)),
    
    "Below_Min" = simplify(lapply(names(list_all_tables), grep, pattern = "share_below_cut", value = TRUE)),
    
    "Inequality" = simplify(lapply(names(list_all_tables), grep, pattern = "Inequality \\(p90/p50\\)", value = TRUE)),
    
    "Formal" = simplify(lapply(names(list_all_tables), grep, pattern = "Formal", value = TRUE)),
    
    "Informal" = simplify(lapply(names(list_all_tables), grep, pattern = "Informal", value = TRUE))
  )
  
  
  sample_name <- pop
  group_var <- "sector"
  

  out_file <- file.path(config$path$outputs, config$output_stage, config$out_subdirs$data_checks, "Samples", paste0(sample_name, "__", group_var, "__tables.xlsx", sep=""))
  
  export_tables_xlsx(
    tables         = list_all_tables,
    sheets         = sheets_map,
    file           = out_file,
    include_titles = TRUE,
    gap_rows       = 3,
    with_filter    = FALSE,
    autofit        = TRUE,
  )
  
}



compute_ineq_q9050 <- function(design,
                               subset_expr = NULL,
                               time_var,
                               group_var,
                               var,
                               probs = c(0.5, 0.9),
                               ratio_name = "p90_p50") {
  
  q <- compute_quantiles(
    design      = design,
    subset_expr = subset_expr,
    time_var    = time_var,
    group_var   = group_var,
    var         = var,
    probs       = probs,
    fast        = TRUE
  ) %>%
    # --- FORCE consistent classes ---
    dplyr::mutate(
      time  = as.character(.data$time),
      group = as.character(.data$group)
    )
  
  q_wide <- q %>%
    dplyr::filter(.data$measure %in% c("p50","p90")) %>%
    dplyr::select(time, group, measure, estimate) %>%
    tidyr::pivot_wider(names_from = measure, values_from = estimate)
  
  ratio <- q_wide %>%
    dplyr::mutate(
      estimate = dplyr::if_else(!is.na(p50) & p50 > 0, p90 / p50, NA_real_),
      measure  = ratio_name
    ) %>%
    dplyr::select(time, group, measure, estimate)
  
  ratio_rank <- ranking(
    df       = ratio,
    rank_var = "estimate",
    time_var = "time",
    rank_col = "rank"
  ) %>%
    dplyr::mutate(
      estimate = rank,
      measure  = paste0("Ranking, ", ratio_name)
    ) %>%
    dplyr::select(time, group, measure, estimate)
  
  dplyr::bind_rows(ratio, ratio_rank) %>%
    # --- and again, to be safe if ranking changes types ---
    dplyr::mutate(
      time  = as.character(.data$time),
      group = as.character(.data$group)
    )
}







# ---- Checks ---- #

#set overall design for individual level quarterly analysis
design <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = analysis_all,
                    nest = TRUE)


des_up <- subset(design, (OCUPADO == 1 & Employment_Status == "Formal" & Employment_Type == "private employee" 
                               & !(Employment_Sector == "Government") & !(Wage_group == "Don't know") ))


data<-des_up$variables



# ---- 4. Calculations ---- #


#set overall design for individual level quarterly analysis
design <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = analysis_all,
                    nest = TRUE)


# -- Basic Indicators No Income Variation -- #

test_N_W <- compute_pop_total(
  design = design,
  subset_expr = populations$formal__all__priv__minwage,
  time_var = "year_quarter",
  group_var = "Employment_Sector"
) %>%
  mutate(measure = "Obs Count, Survey Weighted")

test_n_unw <- compute_n_unw(
  design = design,
  subset_expr = populations$employed__all__priv__minwage,
  time_var = "year_quarter",
  group_var = "Employment_Sector"
) %>%
  mutate(measure = "Obs Count, Unweighted")


# -- Formal and Informal -- #

test_shares <- compute_prop_factor(design = design,
                                   subset_expr = populations$employed__all__priv__minwage,,
                                   time_var = "year_quarter",
                                   var = "Employment_Status",
                                   group_var = "Employment_Sector"
) %>% filter(!is.na(estimate)) %>%
  mutate(measure = paste(measure, " ", level, sep ="")) %>%
  select(-level)


formal_rank <- ranking(
  df = filter(test_shares, measure == "share_Formal"),
  rank_var = "estimate",
  time_var ="time",
  rank_col  = "rank"
) %>%
  mutate(estimate = rank,
         measure = "Ranking, Formal Worker Share") %>%
  select(-rank)

informal_rank <- ranking(
  df = filter(test_shares, measure == "share_Informal"),
  rank_var = "estimate",
  time_var ="time",
  rank_col  = "rank"
) %>%
  mutate(estimate = rank,
         measure = "Ranking, Informal Worker Share") %>%
  select(-rank)


# ---- Calculations with Income Variation ---- #

test_checks <- compute_checks(
  design = design,
  subset_expr = populations$employed__all__priv__minwage,
  var = "real_salary_income_primary",
  cut_var = "real_minwage_harmonized",
  time_var = "year_quarter",
  group_var = "Employment_Sector"
)



# ---- Process into Wide Tables list ---- #

tbls <- bind_rows( test_N_W, test_n_unw, test_checks, formal_rank, informal_rank, test_shares)
wide_tbls <- build_table_wide(tbls, id_cols = c("measure", "group"), col_keys = "time")


list_all_tables <- split_by_indicator(wide_tbls, group_var = "group", indicator_col = "measure")


# ---- Create Sheet Mapping and Save ---- #

sheets_map <- list(
  "Counts" = simplify(lapply(names(list_all_tables), grep, pattern = "Obs Count,", value = TRUE)),
  
  "Zeros" = simplify(lapply(names(list_all_tables), grep, pattern = "share_zero", value = TRUE)),
  
  "Below_Min" = simplify(lapply(names(list_all_tables), grep, pattern = "share_below_cut", value = TRUE)),
  
  "Formal" = simplify(lapply(names(list_all_tables), grep, pattern = "Formal", value = TRUE)),
  
  "Informal" = simplify(lapply(names(list_all_tables), grep, pattern = "Formal", value = TRUE))
)


out_file <- file.path(config$path$outputs, config$output_stage, config$out_subdirs$tables, "Samples", paste0(sample_name, "_", group_var, "_tables.xlsx"))

sample_name <- "check"
group_var <- "sector"

export_tables_xlsx(
  tables         = list_all_tables,
  sheets         = sheets_map,
  file           = out_file,
  include_titles = TRUE,
  gap_rows       = 3,
  with_filter    = FALSE,
  autofit        = TRUE,
)

#########################



#set overall design for individual level quarterly analysis
design <- svydesign(id = ~psu_unique , weights = ~FACTOR_EXPANSION, strata = ~strata_unique, data = analysis_all,
                    nest = TRUE)

design <- update(design,
                 formal = case_when(Employment_Status == "Formal" ~ 1,
                                    TRUE ~ 0),
                 informal = case_when(Employment_Status == "Informal" ~ 1,
                                      TRUE ~ 0))


outcomes <- c(
  adj = "adj_income_primary",
  sal = "real_salary_income_primary",
  
  formal = "formal",
  informal = "informal"
)

populations <- list(
  employed__all                = quote(OCUPADO == 1),
  employed__all__priv          = quote(OCUPADO == 1 & Employment_Type == "private employee" & !(Employment_Sector == "Government")),
  employed__all__priv__minwage = quote(OCUPADO == 1 & Employment_Type == "private employee" & !(Employment_Sector == "Government") 
                                       & !is.na(Wage_group) & Wage_group != "Don't know")
)
  
spec_tbl <- make_spec_table(
  outcomes = outcomes,
  populations = populations
)




test <- run_specs(specs = spec_tbl, design = design,
          compute_fn = compute_prop_factor,
          time_var = "year_quarter",
          group_var = "Employment_Sector")




?run_specs

out_file <- file.path(out_dir, paste0(sample_name, "_", group_var, "_tables.xlsx"))

export_tables_xlsx(
  tables         = list_all_tables,
  sheets         = sheets_map,
  file           = out_file,
  include_titles = TRUE,
  gap_rows       = 3,
  with_filter    = FALSE,
  autofit        = TRUE,
)






# ---- Create Sheet Mapping and Save ---- #

x <- grep(pattern = "n_" , names(list_all_tables))


names(list_all_tables)[1]

lapply(names(list_all_tables), grep, pattern = "n_", value = TRUE)

y <- simplify(lapply(names(list_all_tables), grep, pattern = ("n_", value = TRUE)))


sheets <- list(
  "Counts" = c(simplify(lapply(names(list_all_tables), grep, pattern = "n_", value = TRUE)),
             simplify(lapply(names(list_all_tables), grep, pattern = "pop_", value = TRUE))
  ),
  
  "Zeros" = simplify(lapply(names(list_all_tables), grep, pattern = "share_zero", value = TRUE)),
  
  "Below_Min" = simplify(lapply(names(list_all_tables), grep, pattern = "share_below_cut", value = TRUE)),
  
  "Formal" = simplify(lapply(names(list_all_tables), grep, pattern = "Formal", value = TRUE)),
  
  "Informal" = simplify(lapply(names(list_all_tables), grep, pattern = "Formal", value = TRUE))
)






out_file <- file.path(out_dir, paste0(sample_name, "_", group_var, "_tables.xlsx"))

export_tables_xlsx(
  tables         = tables_list,
  sheets         = sheets_map,
  file           = out_file,
  include_titles = TRUE,
  gap_rows       = 3,
  with_filter    = FALSE,
  autofit        = TRUE,
)





share_tbls <-  bind_rows(test_shares , test_checks)

wide_shares <- build_table_wide(share_tbls, col_keys="time")


num_tbls <- bind_rows(test_N_W, test_n_unw, formal_rank, informal_rank)
wide_num <- build_table_wide(num_tbls, col_keys="time")



list_tables <- split_by_indicator(wide_shares, group_var = "group", indicator_col = "measure")

list_tables <- split_by_indicator(wide_num, group_var = "group", indicator_col = "measure")






##############################




names(per_var) <- list_inc

tables_long <- imap_dfr(per_var, function(ind_list, v) {
  tibble(
    var = v,
    indicator = names(ind_list),
    tbl = unname(ind_list)
  )
}) %>%
  # drop indicators you don't want / drop missing
  filter(!is.na(indicator)) %>%
  # keep only indicators you want (important!)
  filter(indicator %in% indicator_order) %>%
  mutate(
    indicator = factor(indicator, levels = indicator_order),
    var       = factor(var, levels = list_inc),
    table_name = paste(as.character(var), as.character(indicator), sep = " — ")
  ) %>%
  arrange(indicator, var)

tables_list <- setNames(tables_long$tbl, tables_long$table_name)

sheets_map <- tables_long %>%
  group_by(indicator) %>%
  summarise(tbls = list(table_name), .groups = "drop") %>%
  mutate(sheet = as.character(indicator)) %>%
  { setNames(.$tbls, .$sheet) }

out_file <- file.path(out_dir, paste0(sample_name, "_", group_var, "_tables.xlsx"))

export_tables_xlsx(
  tables         = tables_list,
  sheets         = sheets_map,
  file           = out_file,
  include_titles = TRUE,
  gap_rows       = 3,
  with_filter    = FALSE,
  autofit        = TRUE
)





?export_tables_xlsx


?build_table_wide()


all_long <- dplyr::bind_rows(test_N_W, test_n_unw, test_checks)

shares_tbl <- all_long |> dplyr::filter(measure %in% c("share_zero","share_na","share_below_cut"))

wide_shares <- build_table_wide(shares_tbl, col_keys="time", row_order=c("share_zero","share_below_cut","share_na"),
                                as_character=TRUE, scale=100, digits=1, suffix="%")












ranking <- function(df,
                    rank_var,
                    time_var  = NULL,
                    group_var = NULL,
                    desc      = TRUE,
                    ties      = c("min", "dense", "first"),
                    rank_col  = "rank") {
  stopifnot(is.data.frame(df))
  
  ties <- match.arg(ties)
  
  # accept bare names OR strings OR strings stored in variables
  to_name <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.character(x) && length(x) == 1) return(x)
    rlang::as_name(rlang::ensym(x))
  }
  
  rank_var  <- to_name(rank_var)
  time_var  <- to_name(time_var)
  group_var <- to_name(group_var)
  
  group_vars <- c(time_var, group_var)
  group_vars <- group_vars[!is.null(group_vars)]
  
  rank_fun <- switch(
    ties,
    min   = dplyr::min_rank,
    dense = dplyr::dense_rank,
    first = dplyr::row_number
  )
  
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::mutate(
      "{rank_col}" := rank_fun(if (isTRUE(desc)) dplyr::desc(.data[[rank_var]]) else .data[[rank_var]])
    ) %>%
    dplyr::ungroup()
}











# -- Inequality Measures -- #

#note this is calculated with data for workers who dont fit a min wage category
#INCLUDES FORMAL AND INFORMAL


q_sector <- compute_quantiles(
  design      = design,
  var         = "adj_income_primary",
  time_var    = "year",
  group_var   = "Employment_Sector",
  probs       = c(0.10, 0.50, 0.90),
  subset_expr = populations$employed__all__priv__minwage,
  na.rm       = TRUE,
  fast        = TRUE
)













  

# -- Basic Indicators -- #


all_long <- dplyr::bind_rows(test_N_W, test_n_unw, test_checks)

shares_tbl <- all_long |> dplyr::filter(measure %in% c("share_zero","share_na","share_below_cut"))

wide_shares <- build_table_wide(shares_tbl, col_keys="time", row_order=c("share_zero","share_below_cut","share_na"),
                                as_character=TRUE, scale=100, digits=1, suffix="%")








shares_list <- split_by_indicator(shares_tbl, group_var = "group", indicator_col = "measure")


counts_Nw <- run_specs(
  specs = spec_tbl,
  design = design,
  compute_fn = compute_pop_total,
  time_var = "year_quarter",
  group_var = "Employment_Sector",
  outcome_var_col = NULL,     # <- key
  measure = "N_w"
)




counts_n <- run_specs(
  specs = spec_tbl,
  design = design,
  compute_fn = compute_n_unw,
  time_var = "year_quarter",
  group_var = "Employment_Sector",
  outcome_var_col = NULL,
  measure = "n_unw"
)


checks <- run_specs(
  specs = spec_tbl,
  design = design,
  compute_fn = compute_checks,
  time_var = "year_quarter",
  group_var = "Employment_Sector",
  cut_var = "real_minwage_harmonized"
)



all_long <- dplyr::bind_rows(checks, counts_Nw, counts_n)

shares_tbl <- all_long |> dplyr::filter(measure %in% c("share_zero","share_na","share_below_cut"))

wide_shares <- build_table_wide(shares_tbl, col_keys="time", row_order=c("share_zero","share_below_cut","share_na"),
                                as_character=TRUE, scale=100, digits=1, suffix="%")



all_long <- dplyr::bind_rows(checks, counts_Nw, counts_n)


shares_tbl <- all_long |> dplyr::filter(measure %in% c("share_zero","share_na","share_below_cut"))
counts_tbl <- all_long |> dplyr::filter(measure %in% c("N_w","n_unw"))



shares_list <- split_by_indicator(shares_tbl, group_var = "group", indicator_col = "measure")
counts_list <-split_by_indicator(shares_tbl, group_var = "group", indicator_col = "measure")





wide_shares <- build_table_wide(shares_tbl, col_keys="time", row_order=c("share_zero","share_below_cut","share_na"),
                                as_character=TRUE, scale=100, digits=1, suffix="%")

wide_counts <- build_table_wide(counts_tbl, col_keys="time", row_order=c("n_unw","N_w"),
                                as_character=FALSE)




compute_pop_total(
  
  
)








#load a named list of RDS files
read_rds_list <- function(named_paths) {
  purrr::imap(named_paths, ~ readRDS(.x))
}

sample_paths <- c(
  analysis_all                      = file.path(config$paths$processed_data, "analysis_all.rds"),
  analysis_formal_emp               = file.path(config$paths$processed_data, "analysis_formal_emp.rds"),
  analysis_formal_emp_wagegroup     = file.path(config$paths$processed_data, "analysis_formal_emp_wagegroup.rds"),
  analysis_formal_emp_wagegroup_priv= file.path(config$paths$processed_data, "analysis_formal_emp_wagegroup_priv.rds"),
  analysis_all_emp_wagegroup_priv   = file.path(config$paths$processed_data, "analysis_all_emp_wagegroup_priv.rds")
)


list_samplesdf <- read_rds_list(sample_paths)

# create survey designs
make_design <- function(df,
                        id_var = "psu_unique",
                        strata_var = "strata_unique",
                        weight_var = "FACTOR_EXPANSION") {
  
  survey::svydesign(
    id      = as.formula(paste0("~", id_var)),
    strata  = as.formula(paste0("~", strata_var)),
    weights = as.formula(paste0("~", weight_var)),
    data    = df,
    nest    = TRUE
  )
}

list_designs <- purrr::map(list_samplesdf, make_design)



#define income variables
list_inc <- c(
  "adj_income_primary",
  "real_salary_income_primary",
  "real_independent_income_primary",
  "real_total_income_total"

)


out_dir <- file.path(config$paths$outputs, config$output_stage, config$out_subdir$tables, "Samples")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

time_var <- "year_quarter"
cut_var  <- "real_minwage_harmonized"
indicator_order <- c("n_unw", "N_w", "share_zero", "share_below_cut", "share_na")

group_var <- "Employment_Sector"

for (sample_name in names(list_designs)) {
  
  des_sample <- list_designs[[sample_name]]
  
  per_var <- map(list_inc, function(v) {
    wide_tbl <- sample_table_svy(
      design    = des_sample,
      var       = v,
      time_var  = time_var,
      cut_var   = cut_var,
      group_var = group_var,
      wide      = TRUE,
      wide_vars = time_var
    )
    split_by_indicator(wide_tbl, group_var = group_var)
  })
  

  names(per_var) <- list_inc
  
  tables_long <- imap_dfr(per_var, function(ind_list, v) {
    tibble(
      var = v,
      indicator = names(ind_list),
      tbl = unname(ind_list)
    )
  }) %>%
    # drop indicators you don't want / drop missing
    filter(!is.na(indicator)) %>%
    # keep only indicators you want (important!)
    filter(indicator %in% indicator_order) %>%
    mutate(
      indicator = factor(indicator, levels = indicator_order),
      var       = factor(var, levels = list_inc),
      table_name = paste(as.character(var), as.character(indicator), sep = " — ")
    ) %>%
    arrange(indicator, var)
  
  tables_list <- setNames(tables_long$tbl, tables_long$table_name)
  
  sheets_map <- tables_long %>%
    group_by(indicator) %>%
    summarise(tbls = list(table_name), .groups = "drop") %>%
    mutate(sheet = as.character(indicator)) %>%
    { setNames(.$tbls, .$sheet) }
  
  out_file <- file.path(out_dir, paste0(sample_name, "_", group_var, "_tables.xlsx"))
  
  export_tables_xlsx(
    tables         = tables_list,
    sheets         = sheets_map,
    file           = out_file,
    include_titles = TRUE,
    gap_rows       = 3,
    with_filter    = FALSE,
    autofit        = TRUE
  )
}
  
  


list_designs

group_var <- "Wage_group"

for (sample_name in names(list_designs)) {
  
  des_sample <- list_designs[[sample_name]]
  
  per_var <- map(list_inc, function(v) {
    wide_tbl <- sample_table_svy(
      design    = des_sample,
      var       = v,
      time_var  = time_var,
      cut_var   = cut_var,
      group_var = group_var,
      wide      = TRUE,
      wide_vars = time_var
    )
    split_by_indicator(wide_tbl, group_var = group_var)
  })
  
  
  names(per_var) <- list_inc
  
  tables_long <- imap_dfr(per_var, function(ind_list, v) {
    tibble(
      var = v,
      indicator = names(ind_list),
      tbl = unname(ind_list)
    )
  }) %>%
    # drop indicators you don't want / drop missing
    filter(!is.na(indicator)) %>%
    # keep only indicators you want (important!)
    filter(indicator %in% indicator_order) %>%
    mutate(
      indicator = factor(indicator, levels = indicator_order),
      var       = factor(var, levels = list_inc),
      table_name = paste(as.character(var), as.character(indicator), sep = " — ")
    ) %>%
    arrange(indicator, var)
  
  tables_list <- setNames(tables_long$tbl, tables_long$table_name)
  
  sheets_map <- tables_long %>%
    group_by(indicator) %>%
    summarise(tbls = list(table_name), .groups = "drop") %>%
    mutate(sheet = as.character(indicator)) %>%
    { setNames(.$tbls, .$sheet) }
  
  out_file <- file.path(out_dir, paste0(sample_name, "_", group_var, "_tables.xlsx"))
  
  export_tables_xlsx(
    tables         = tables_list,
    sheets         = sheets_map,
    file           = out_file,
    include_titles = TRUE,
    gap_rows       = 3,
    with_filter    = FALSE,
    autofit        = TRUE
  )
}












x <- list_designs[["analysis_all"]]



test <- sample_table_svy(
  design    = x,
  var       = "adj_income_primary",
  time_var  = time_var,
  cut_var   = cut_var,
  group_var = group_var,
  wide      = FALSE
)

glimpse(test)



x <- update(x,
            zero = case_when(adj_income_primary == 0 ~ 1,
                             TRUE ~ 0),
            na = case_when( is.na(adj_income_primary) ~ 1,
                            TRUE ~ 0),
            below_cut = case_when( adj_income_primary < cut_var ~ 1,
                                   TRUE ~ 0)
)


by_f <- as.formula(paste0("~", time_var, " + ", group_var))


out <- svyby(~zero + na, by_f, x, svymean, na.rm = FALSE, vartype = NULL)

glimpse(out)

# weighted populati
on total (sum of weights)
x <- update(x,
              N_w = 1)

Nw <- svyby(~N_w, by_f, x, svytotal, na.rm = TRUE, vartype = NULL)

out <- out %>%
  left_join(Nw, by = c(time_var, group_var))



wide = TRUE
wide_vars = NULL

if (!isTRUE(wide)) return(out)

#optional reshape
if (is.null(wide_vars)) wide_vars <- time_var

out <- z_na

value_cols <- intersect(c("n_unw","N_w","zero","share_below_cut","na"), names(out))

check <- out %>%
  tidyr::pivot_longer(
    cols = all_of(value_cols),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  tidyr::pivot_wider(
    id_cols = c(all_of(group_var), "indicator"),
    names_from = all_of(wide_vars),
    values_from = "value",
    values_fn = first
  ) %>%
  dplyr::arrange(.data[[group_var]], .data$indicator)



test2 <- split_by_indicator(check)

split_by_indicator <- function(wide_tbl,
                               group_var = "Employment_Sector",
                               indicator_col = "indicator") {
  
  stopifnot(all(c(group_var, indicator_col) %in% names(wide_tbl)))
  
  split(wide_tbl, wide_tbl[[indicator_col]]) |>
    lapply(function(d) {
      d |>
        dplyr::select(-dplyr::all_of(indicator_col)) |>
        dplyr::arrange(.data[[group_var]])
    })
}





jobs <- tidyr::expand_grid(
  sample = names(list_samplesdf),
  var    = list_inc
)

tables <- jobs %>%
  mutate(
    table = pmap(
      list(sample, var),
      ~ sample_table(
        df        = list_samplesdf[[.x]],
        var       = .y,
        time_var  = "year_quarter",
        cut_var   = "real_minwage_harmonized",
        group_var = "Employment_Sector",
        wide = TRUE,
        wide_vars = "year_quarter"
      )
    )
  )

#split tables by indicators
tables_ind <- tables %>%
  mutate(
    ind_tables = purrr::map(
      table,
      split_by_indicator,
      group_var = "Employment_Sector"
    )
  )


#Names list of tables for excel export
tables_list <- tables_ind %>%
  select(sample, var, ind_tables) %>%
  tidyr::unnest_longer(ind_tables, values_to = "tbl", indices_to = "indicator") %>%
  mutate(
    name = paste(sample, var, indicator, sep = " — ")
  ) %>%
  { stats::setNames(.$tbl, .$name) }


#map one sheet per indicator
indicator_order <- c("n", "share_zero", "share_neg", "share_na", "share_below_cut")

sheets_map <- tables_ind %>%
  select(sample, var, ind_tables) %>%
  tidyr::unnest_longer(ind_tables, values_to = "tbl", indices_to = "indicator") %>%
  mutate(
    indicator = factor(indicator, levels = indicator_order),
    table_name = paste(sample, var, as.character(indicator), sep = " — ")
  ) %>%
  arrange(sample, var, indicator) %>%
  dplyr::group_by(sample) %>%
  summarise(tbls = list(table_name), .groups = "drop") %>%
  { stats::setNames(.$tbls, .$sample) }




# 5) Export with your helper (one sheet per sample, stacked tables per sheet)
out_file <- file.path(config$paths$outputs, config$output_stage, config$out_subdir$tables, "Samples",  "sample_tables_by_sample.xlsx")

export_tables_xlsx(
  tables         = tables_list,
  sheets         = sheets_map,
  file           = out_file,
  include_titles = TRUE,
  gap_rows       = 2,
  with_filter    = FALSE,
  autofit        = TRUE
)


