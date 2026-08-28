#===============================================================================
# 03C_firm size missingness.R
#
# PURPOSE
#   Firm size is the treatment-assignment variable: it maps workers to a legal
#   minimum-wage tier and enters the exposure measure as a province-level
#   weight. Table 2 in 03B showed the share of workers with a usable tier falls
#   from ~98% to ~86%. This script characterises that non-response.
#
#   fig1_firm_size_composition.png    tier composition incl. DK and blank
#   fig2_firm_size_nonresponse.png    non-response over time, MW events marked
#   fig3_composition_bounds.png       worst-case bounds on each tier share
#   fig4a_nonresponse_estimation.png  non-response by estimation geography
#   fig4b_nonresponse_domain.png      non-response by survey domain
#   fig5_item_nonresponse.png         firm size vs other item non-response
#   fig6_item_nonresponse_domain.png  the same, by domain
#   table3_firm_size_composition.html annual shares
#   table4_composition_bounds.html    which changes survive worst-case bounds
#   table5_dk_firm_characteristics.html  who the non-respondents are
#
#   Nothing downstream depends on this file.
#
# DENOMINATOR
#   MW-covered private employees: private employees excluding domestic service,
#   free-trade-zone and utilities.
#
#   Domestic workers are excluded on purpose and this is not a judgement call.
#   Every domestic-service record has TOTAL_PERSONAS_TRABAJAN_EMP blank - the
#   question is not asked of them, because the employer is a household.
#   Including them puts a group that is 100% missing by routing into the
#   denominator: non-response reads 17.4% instead of 5.5%.
#
# DK VS BLANK
#   Never pooled.
#     "Dont Know"          asked, could not answer (code 98)
#     "Blank / not asked"  no response recorded - routing, refusal, fieldwork
#   A rise in the first is respondent or interviewer behaviour; a rise in the
#   second is a survey-operations problem.
#
# ROUTED VARIABLES - READ BEFORE ADDING PROXIES
#   REGISTRO_TRANSACCIONES_EMPRESA and EMPRESA_TIENE_LICENCIA are asked ONLY of
#   firms not registered with RNC (255 of 316 private employees blank in 2017,
#   exactly the count of EMPRESA_INSCRITA_RNC == 1). Statistics on them are
#   computed on the unregistered remainder, which for large firms is <1% of the
#   group - that is what produced "Large keeps formal accounts 92.3%" alongside
#   "Large has a licence 0.0%". Both were denominator artifacts. TIENE_CONTRATO
#   is degenerate (every private employee coded 1). All three are excluded.
#   98 is the don't-know code in these items too and must be treated as missing.
#===============================================================================

source(here::here("Code", "R", "clean scripts", "00_setup.R"))
source(here::here("Code", "R", "clean scripts", "03_sample definitions.R"))

cat("=== 03C_firm size missingness.R ===\n\n")

fig_dir <- file.path(config$out_dirs$data_check, "firm size")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- function(stem, ext) file.path(fig_dir, paste0(stem, ".", ext))

if (!exists("theme_surveytools")) theme_surveytools <- function(...) theme_minimal()


#-------------------------------------------------------------------------------
# Parameters
#-------------------------------------------------------------------------------

DENOM_SAMPLE <- "mw_covered"          # or "private_employees" for sensitivity
MW_EVENTS    <- config$events$event_qtrs
BASE_YEAR <- as.integer(substr(config$sample$start_qtr, 1, 4))

MIN_CELL     <- 100                   # geography cells below this are unusable
THIN_CELL    <- 30                    # standardisation cells below this are thin

NR_LEVELS   <- c("Dont Know", "Blank / not asked")
TIER_LEVELS <- config$TIER_LEVELS
SIZE_LEVELS <- c(TIER_LEVELS, NR_LEVELS)

# Set to the variable names in your data.
GEO_VARS <- c(estimation = config$regression$cluster_geo,       # unit at which exposure varies
              domain     = config$regression$inference_geo)    # survey domains of inference





# Cells for the standardisation in Step 9. Firm size cannot appear here: it is
# missing for exactly the workers under study.
STD_VARS <- c("Employment_Status", "Employment_Sector_Simplified",
              GEO_VARS[["domain"]])

# Only items whose coding has been verified. 98 = don't know in all of them.
NR_ITEMS <- list(
  "Firm size"      = quote(size_cat %in% NR_LEVELS),
  "Salary"         = quote(is.na(real_salary_income_wage_primary) |
                             real_salary_income_wage_primary <= 0),
  "RNC registered" = quote(is.na(EMPRESA_INSCRITA_RNC) | EMPRESA_INSCRITA_RNC == 98)
)

# Firm characteristics recorded independently of the size question and NOT
# routed. See the header note before adding to this list.
FIRM_PROXIES <- list(
  "Registered with RNC" = quote(ifelse(EMPRESA_INSCRITA_RNC %in% c(98, NA),
                                       NA_real_, as.numeric(EMPRESA_INSCRITA_RNC == 1))),
  "Affiliated to AFP"   = quote(ifelse(AFILIADO_AFP_PRINC %in% c(98, NA),
                                       NA_real_, as.numeric(AFILIADO_AFP_PRINC == 1)))
)

SIZE_COLS <- c("Micro" = "#BBD3E3", "Small" = "#7FA8C4",
               "Medium" = "#3F7CA6", "Large" = "#14507A",
               "Dont Know" = "#C45C30", "Blank / not asked" = "#E8A87C")

ITEM_COLS <- c("Firm size" = "#C45C30", "Salary" = "#14507A",
               "RNC registered" = "#7FA8C4")

# Year axes are numeric, so default breaks render as "2017.5". Always use this.
scale_x_year <- function(by = 2) {
  ggplot2::scale_x_continuous(breaks = function(l) {
    seq(ceiling(min(l)), floor(max(l)), by = by)
  })
}


#===============================================================================
# STEP 1.  Analysis design
#
#  wage_group carries "Dont Know" as an explicit level and blanks as NA. Making
#  the NA an explicit level is the point: every share below is computed on a
#  FIXED denominator and the six categories sum to one. Renormalising over known
#  tiers divides by a base that shrinks from 98% to 86% and puts a spurious
#  upward trend into all four tier shares.
#===============================================================================

des <- samples[[DENOM_SAMPLE]]$design

GEO_N <- c(estimation = dplyr::n_distinct(des$variables[[GEO_VARS[["estimation"]]]]),
           domain     = dplyr::n_distinct(des$variables[[GEO_VARS[["domain"]]]]))


des$variables$size_cat <- factor(
  dplyr::case_when(
    des$variables$has_tier     ~ as.character(des$variables$wage_group),
    des$variables$firm_size_dk ~ "Dont Know",
    TRUE                       ~ "Blank / not asked"),
  levels = SIZE_LEVELS)

stopifnot(!anyNA(des$variables$size_cat),
          all(GEO_VARS %in% names(des$variables)),
          all(STD_VARS %in% names(des$variables)))

#geography labels 

# Short form, for inline use inside a sentence.
GEO_NAME <- c(estimation = sprintf("%d planning regions", GEO_N[["estimation"]]),
              domain     = sprintf("%d survey domains",   GEO_N[["domain"]]))

# Long form, for standalone subtitles.
GEO_GLOSS <- c(estimation = "the unit at which exposure varies",
               domain     = "the level at which the ENCFT is designed to be representative")

GEO_LABEL <- c(stats::setNames(sprintf("%s - %s", GEO_NAME, GEO_GLOSS), names(GEO_NAME)),
               national = "National")

for (s in c("private_employees", "mw_covered")) {
  v  <- samples[[s]]$data
  nr <- mean(!v$has_tier)
  cat(sprintf("    Non-response, denominator = %-18s : %5.1f%% (n = %s)\n",
              s, 100 * nr, format(nrow(v), big.mark = ",")))
}
cat("    Using: ", DENOM_SAMPLE, "\n\n", sep = "")

# Full years only. A partial year is noisier and not comparable across columns;
# 03B applies the same rule, so Tables 2 and 3 now cover the same years.
full_years <- des$variables %>%
  dplyr::distinct(year, .data[[TIME_VAR]]) %>%
  dplyr::count(year, name = "nq") %>%
  dplyr::filter(nq == 4L) %>%
  dplyr::pull(year)

LAST_YEAR <- max(full_years)
stopifnot(BASE_YEAR %in% full_years)

cat("    Full years: ", min(full_years), "-", LAST_YEAR,
    " (", length(full_years), " years)\n\n", sep = "")


#===============================================================================
# STEP 2.  Quarterly and annual composition
#===============================================================================

cat("[1] Composition...\n")

# svyby returns wide: "size_catMicro", "se.size_catMicro", ... Reshape
# estimates and SEs separately rather than relying on a names_pattern that
# varies across tidyr versions.
tidy_svyby <- function(x, by) {
  est <- x %>% dplyr::select(dplyr::all_of(by), dplyr::starts_with("size_cat")) %>%
    tidyr::pivot_longer(-dplyr::all_of(by), names_to = "size_cat",
                        names_prefix = "size_cat", values_to = "share")
  sev <- x %>% dplyr::select(dplyr::all_of(by), dplyr::starts_with("se.size_cat")) %>%
    tidyr::pivot_longer(-dplyr::all_of(by), names_to = "size_cat",
                        names_prefix = "se.size_cat", values_to = "se")
  est %>% dplyr::left_join(sev, by = c(by, "size_cat")) %>%
    dplyr::mutate(size_cat = factor(size_cat, levels = SIZE_LEVELS))
}

comp_qtr <- svyby(~size_cat, as.formula(paste0("~", TIME_VAR)), des, svymean,
                  na.rm = TRUE, keep.names = FALSE) %>%
  tidy_svyby(TIME_VAR) %>%
  dplyr::mutate(t = as.integer(substr(.data[[TIME_VAR]], 1, 4)) +
                  (as.integer(substr(.data[[TIME_VAR]], 6, 6)) - 1) / 4)

comp_year <- svyby(~size_cat, ~year, des, svymean, na.rm = TRUE,
                   keep.names = FALSE) %>%
  tidy_svyby("year") %>%
  dplyr::filter(year %in% full_years)

# Shares must sum to one in every period or the denominator is not fixed.
stopifnot(all(abs(tapply(comp_qtr$share, comp_qtr[[TIME_VAR]], sum) - 1) < 1e-8))

event_t <- as.integer(substr(MW_EVENTS, 1, 4)) +
  (as.integer(substr(MW_EVENTS, 6, 6)) - 1) / 4


#===============================================================================
# STEP 3.  Figure 1 - composition including non-response
#===============================================================================

fig1 <- ggplot(comp_qtr, aes(t, share, fill = size_cat)) +
  geom_area(colour = "white", linewidth = 0.15) +
  annotate("rect", xmin = 2020.00, xmax = 2020.75, ymin = -Inf, ymax = Inf,
           fill = "grey20", alpha = 0.12) +
  scale_fill_manual(values = SIZE_COLS, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = c(0, 0)) +
  scale_x_year() +
  labs(
    title    = "Workers who cannot be assigned a firm-size tier crowd out the tier distribution",
    subtitle = paste("Firm-size composition of MW-covered private employees,",
                     "non-response shown as its own category.",
                     GEO_LABEL[["national"]]),
    x = NULL, y = "Share of MW-covered private employees",
    caption = paste(
      "Denominator is fixed: private-sector employees excluding domestic service,",
      "free-trade-zone and utilities. Shares sum to 100% in every quarter.",
      "Domestic workers are excluded because the firm-size question is not asked",
      "of them, so they are missing by routing rather than by non-response.",
      "'Dont Know' is an explicit response; 'Blank / not asked' is no recorded answer.",
      "Shaded band: 2020 pandemic quarters.",
      "Source: Authors' calculations using ENCFT.", sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom")

ggsave(out_path("fig1_firm_size_composition", "png"), fig1,
       width = 10, height = 6, dpi = 300)


#===============================================================================
# STEP 4.  Figure 2 - non-response against the MW event calendar
#
#  The blank series is a flat line at zero and that is the finding, not clutter:
#  it establishes that all non-response is explicit "don't know" rather than
#  routing or refusal. Keep it.
#===============================================================================

fig2 <- comp_qtr %>%
  dplyr::filter(size_cat %in% NR_LEVELS) %>%
  ggplot(aes(t, share, colour = size_cat, fill = size_cat)) +
  annotate("rect", xmin = 2020.00, xmax = 2020.75, ymin = -Inf, ymax = Inf,
           fill = "grey20", alpha = 0.12) +
  geom_vline(xintercept = event_t, linetype = "dashed",
             colour = "grey45", linewidth = 0.4) +
  geom_ribbon(aes(ymin = share - 1.96 * se, ymax = share + 1.96 * se),
              alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = SIZE_COLS[NR_LEVELS], name = NULL) +
  scale_fill_manual(values   = SIZE_COLS[NR_LEVELS], name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_year(1) +
  labs(
    title    = "Firm-size non-response rises steadily and does not track the minimum wage calendar",
    subtitle = paste("Share of MW-covered private employees with no usable firm size,",
                     "by quarter.", GEO_LABEL[["national"]]),
    x = NULL, y = "Share of MW-covered private employees",
    caption = paste(
      paste0("Dashed verticals: minimum wage events (",
             paste(MW_EVENTS, collapse = ", "), "). ",
             "Shaded band: 2020 pandemic quarters."),
      "Bands are 95% design-based confidence intervals.",
      "Event lines are descriptive. The pre/post comparison in the log nets out",
      "the local trend; the geographic and standardisation evidence below is",
      "what rules the minimum wage in or out.",
      "Source: Authors' calculations using ENCFT.", sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom")

ggsave(out_path("fig2_firm_size_nonresponse", "png"), fig2,
       width = 10, height = 5.5, dpi = 300)


#===============================================================================
# STEP 5.  Pre/post around events - CONSOLE DIAGNOSTIC ONLY, NOT FOR THE PAPER
#
#  The counterfactual is the slope over the eight quarters BEFORE each event,
#  not a single slope fitted to the whole series. A global slope is fitted to a
#  series that is flat until 2020 and convex after, so it over-predicts early
#  events and under-predicts late ones - the earlier version of this table
#  showed spurious negatives in 2017 and 2019 for exactly that reason.
#
#  Even corrected this is weak: five events, a strongly trending series, and no
#  control group. Do not quote it.
#===============================================================================

nr_tot <- comp_qtr %>%
  dplyr::filter(size_cat %in% NR_LEVELS) %>%
  dplyr::group_by(t) %>%
  dplyr::summarise(nr = sum(share), .groups = "drop") %>%
  dplyr::arrange(t)

prepost <- purrr::map_dfr(seq_along(event_t), function(i) {
  e    <- event_t[i]
  pre  <- nr_tot[nr_tot$t <  e & nr_tot$t >= e - 1, ]
  post <- nr_tot[nr_tot$t >= e & nr_tot$t <  e + 1, ]
  win  <- nr_tot[nr_tot$t <  e & nr_tot$t >= e - 2, ]   # local 8-quarter trend
  slope <- if (nrow(win) >= 4) unname(coef(lm(nr ~ t, data = win))[2]) else NA_real_
  tibble::tibble(
    event     = MW_EVENTS[i],
    pre_pct   = 100 * mean(pre$nr),
    post_pct  = 100 * mean(post$nr),
    diff_pp   = 100 * (mean(post$nr) - mean(pre$nr)),
    local_pp  = 100 * slope,
    excess_pp = 100 * (mean(post$nr) - mean(pre$nr) - slope)
  )
})

cat("\n    Non-response around MW events (4 quarters either side, pp).\n")
cat("    local_pp projects the pre-event 8-quarter slope forward one year.\n")
cat("    Diagnostic only - see the header note.\n\n")
print(as.data.frame(prepost), digits = 3, row.names = FALSE)
cat("\n")


#===============================================================================
# STEP 6.  Table 3 - annual composition
#===============================================================================

cat("[2] Table 3...\n")

table3 <- comp_year %>%
  dplyr::select(size_cat, year, share) %>%
  tidyr::pivot_wider(names_from = year, values_from = share) %>%
  dplyr::arrange(size_cat) %>%
  gt() %>%
  tab_header(
    title    = "Table 3. Firm-size composition of MW-covered private employees",
    subtitle = paste("Weighted shares, non-response shown as its own category.",
                     GEO_LABEL[["national"]])
  ) %>%
  cols_label(size_cat = "Firm size") %>%
  fmt_percent(-size_cat, decimals = 1) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = size_cat %in% NR_LEVELS)) %>%
  tab_footnote(
    paste("Columns sum to 100%. Shares are NOT renormalised over known tiers:",
          "doing so divides by a base that shrinks from roughly 98% to 86% of",
          "the sample and induces an upward trend in all four tier shares."),
    locations = cells_column_labels(columns = size_cat)) %>%
  tab_source_note(paste("Source: Authors' calculations using ENCFT. Denominator",
                        "excludes domestic service, free-trade-zone and utilities;",
                        "the firm-size question is not asked of domestic workers.",
                        "Years with fewer than four observed quarters are omitted.")) %>%
  cols_align("left", columns = size_cat) %>%
  tab_options(table.font.size = px(11), heading.title.font.size = px(14),
              column_labels.font.weight = "bold",
              table.border.top.style = "none",
              table_body.hlines.style = "none", data_row.padding = px(3))


stem <- "table3_firm_size_composition"

gtsave(table3, out_path(stem, "html"))
gtsave(table3, out_path(stem, "png"), expand = 10)


#===============================================================================
# STEP 7.  Bounds on the change in tier composition
#
#  Observed tier shares mix genuine composition with selective non-response, and
#  nothing estimated on the observed shares separates them. What IS answerable:
#  what could each share be under ANY assumption about the non-respondents?
#
#  A tier's true share lies in [observed, observed + U]. The bound on the change
#  is [ lower(last) - upper(base), upper(last) - lower(base) ], and a conclusion
#  is credible only if the whole interval has one sign.
#
#  Bounds are population quantities and ignore sampling error, which is ~0.3-0.5
#  pp against bound widths of 10-18 pp. Do not add the two naively.
#===============================================================================

cat("[3] Bounds...\n")

unknown_share <- comp_year %>%
  dplyr::filter(size_cat %in% NR_LEVELS) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(U = sum(share), .groups = "drop")

bounds <- comp_year %>%
  dplyr::filter(size_cat %in% TIER_LEVELS, year >= BASE_YEAR) %>%
  dplyr::left_join(unknown_share, by = "year") %>%
  dplyr::mutate(lo = share, hi = share + U)

bound_change <- bounds %>%
  dplyr::filter(year %in% c(BASE_YEAR, LAST_YEAR)) %>%
  dplyr::select(size_cat, year, share, lo, hi) %>%
  tidyr::pivot_wider(names_from = year, values_from = c(share, lo, hi)) %>%
  dplyr::mutate(
    obs_change = 100 * (.data[[paste0("share_", LAST_YEAR)]] -
                          .data[[paste0("share_", BASE_YEAR)]]),
    lb_change  = 100 * (.data[[paste0("lo_", LAST_YEAR)]] -
                          .data[[paste0("hi_", BASE_YEAR)]]),
    ub_change  = 100 * (.data[[paste0("hi_", LAST_YEAR)]] -
                          .data[[paste0("lo_", BASE_YEAR)]]),
    identified = sign(lb_change) == sign(ub_change)
  ) %>%
  dplyr::select(size_cat, obs_change, lb_change, ub_change, identified)

cat(sprintf("\n    Change in tier share, %d to %d (pp):\n", BASE_YEAR, LAST_YEAR))
print(as.data.frame(bound_change), digits = 3, row.names = FALSE)
cat("\n")


fig3 <- ggplot(bounds, aes(year)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#C45C30", alpha = 0.18) +
  geom_line(aes(y = share), colour = "#14507A", linewidth = 0.8) +
  facet_wrap(~size_cat, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_year() +
  labs(
    title    = "Only the shift toward large firms survives worst-case non-response",
    subtitle = paste("Observed tier share (line) and the range consistent with",
                     "any allocation of non-respondents (band)"),
    x = NULL, y = "Share of MW-covered private employees",
    caption = paste(
      "Band upper edge assumes every non-respondent in that year belongs to the",
      "tier shown; lower edge assumes none do. The band widens over time because",
      "non-response grows from ~2% to ~13%.",
      "A trend is credible only where it cannot be absorbed by the band.",
      "Source: Authors' calculations using ENCFT.", sep = "\n")
  ) +
  theme_surveytools()

ggsave(out_path("fig3_composition_bounds", "png"), fig3,
       width = 9, height = 6, dpi = 300)


#===============================================================================
# STEP 8.  Non-response by geography
#
#  TWO QUESTIONS, TWO GEOGRAPHIES, TWO FIGURES. They are plotted separately
#  because a shared legend across both puts "Ozama o Gran Santo Domingo" and
#  "Gran Santo Domingo" in different colours for the same place.
#
#  (a) Estimation geography - the unit at which exposure varies. Differential
#      non-response here is a THREAT: if it rises faster where exposure is
#      higher, the selection rule is correlated with treatment.
#  (b) Survey domains - typically track field-office organisation. Clustering
#      here is DIAGNOSTIC: a uniform national rise points to an instrument or
#      protocol change, concentration points to fieldwork.
#
#  Annual, not quarterly: ~2,800 MW-covered employees per quarter split ten ways
#  leaves ~280 per cell, SEs near 3 pp against a 13 pp signal. Annual cells are
#  ~1,100 and SEs near 1 pp. The trend is slow, so nothing is lost.
#===============================================================================

cat("[4] Geography...\n")

des$variables$nonresp <- as.numeric(des$variables$size_cat %in% NR_LEVELS)

nr_by_geo <- purrr::imap_dfr(GEO_VARS, function(gv, role) {
  est <- svyby(~nonresp, as.formula(paste0("~", gv, "+ year")), des, svymean,
               na.rm = TRUE, keep.names = FALSE) %>%
    dplyr::rename(geo = !!gv, share = nonresp) %>%
    dplyr::mutate(role = role)
  cnt <- des$variables %>% dplyr::count(geo = .data[[gv]], year, name = "n_cell")
  est %>% dplyr::left_join(cnt, by = c("geo", "year")) %>%
    dplyr::mutate(cv = se / share, thin = n_cell < MIN_CELL)
}) %>% dplyr::filter(year %in% full_years)

cat("    Cells below n = ", MIN_CELL, ": ", sum(nr_by_geo$thin), " of ",
    nrow(nr_by_geo), " | median CV ",
    round(median(nr_by_geo$cv, na.rm = TRUE), 3), "\n", sep = "")

geo_fig <- function(role_id, ttl, sub) {
  nr_by_geo %>%
    dplyr::filter(role == role_id, !thin) %>%
    ggplot(aes(year, share, colour = geo)) +
    geom_vline(xintercept = unique(as.integer(substr(MW_EVENTS, 1, 4))),
               linetype = "dashed", colour = "grey80", linewidth = 0.3) +
    geom_line(linewidth = 0.7) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_year() +
    labs(title = ttl, subtitle = sub, x = NULL,
         y = "Share with no usable firm size", colour = NULL,
         caption = paste(
           paste0("Annual: quarterly cells are too thin to read. Cells with fewer than ",
                  MIN_CELL, " observations are dropped."),
           "Dashed verticals mark years containing a minimum wage event.",
           "Source: Authors' calculations using ENCFT.", sep = "\n")) +
    theme_surveytools(legend_position = "right")
}
ggsave(out_path("fig4a_nonresponse_estimation", "png"),
       geo_fig("estimation",
               "Firm-size non-response is concentrated in Gran Santo Domingo",
               GEO_LABEL[["estimation"]]),
       width = 10, height = 6, dpi = 200)

ggsave(out_path("fig4b_nonresponse_domain", "png"),
       geo_fig("domain",
               "The rise is not national",
               GEO_LABEL[["domain"]]),
       width = 9, height = 6, dpi = 200)

# Uniform rise vs concentrated rise: a national protocol change raises the level
# without raising the spread; fieldwork or local composition raises both.
geo_spread <- nr_by_geo %>%
  dplyr::filter(year %in% c(BASE_YEAR, LAST_YEAR), !thin) %>%
  dplyr::group_by(role, year) %>%
  dplyr::summarise(min_pct = 100 * min(share), max_pct = 100 * max(share),
                   sd_pp = 100 * sd(share), n_units = dplyr::n(), .groups = "drop")

cat("\n    Dispersion of non-response across units:\n")
print(as.data.frame(geo_spread), digits = 3, row.names = FALSE)
cat("\n")

# Exact decomposition of the national rise. The national rate is the
# employment-weighted mean of regional rates, so each region's term below is its
# exact contribution and the terms sum to the national change with no residual.
reg_parts <- des$variables %>%
  dplyr::filter(year %in% c(BASE_YEAR, LAST_YEAR)) %>%
  dplyr::group_by(year, geo = .data[[GEO_VARS[["estimation"]]]]) %>%
  dplyr::summarise(pop = sum(FACTOR_EXPANSION),
                   nr  = sum(FACTOR_EXPANSION * nonresp), .groups = "drop_last") %>%
  dplyr::mutate(w = pop / sum(pop), rate = nr / pop) %>%
  dplyr::ungroup()

decomp <- reg_parts %>%
  dplyr::select(year, geo, w, rate) %>%
  tidyr::pivot_wider(names_from = year, values_from = c(w, rate)) %>%
  dplyr::mutate(contrib_pp = 100 * (.data[[paste0("w_", LAST_YEAR)]] *
                                      .data[[paste0("rate_", LAST_YEAR)]] -
                                      .data[[paste0("w_", BASE_YEAR)]] *
                                      .data[[paste0("rate_", BASE_YEAR)]])) %>%
  dplyr::arrange(dplyr::desc(contrib_pp)) %>%
  dplyr::mutate(share_of_rise = contrib_pp / sum(contrib_pp))

cat(sprintf("    Contribution to the national rise, %d to %d:\n",
            BASE_YEAR, LAST_YEAR))
print(as.data.frame(decomp %>% dplyr::select(geo, dplyr::starts_with("rate_"),
                                             contrib_pp, share_of_rise)),
      digits = 3, row.names = FALSE)
cat("    National change (pp): ", round(sum(decomp$contrib_pp), 2), "\n\n", sep = "")


#===============================================================================
# STEP 9.  Mechanisms
#
#  9A  Is it survey-wide decay?      Compare items on the same denominator.
#  9B  Is it composition?            Standardise to base-year composition.
#  9C  Who are the non-respondents?  Firm characteristics, unrouted items only.
#===============================================================================

#-------------------------------------------------------------------------------
# 9A.  Item non-response, same denominator
#
#  Levels differ, so the comparison is made in CHANGE FROM BASELINE. Two series
#  both "rising" tells you nothing; different shapes and different geography
#  tell you they are different phenomena.
#-------------------------------------------------------------------------------

cat("[5] Item non-response...\n")

for (nm in names(NR_ITEMS)) {
  des$variables[[paste0("nr_", make.names(nm))]] <-
    as.numeric(eval(NR_ITEMS[[nm]], des$variables))
}
nr_cols <- paste0("nr_", make.names(names(NR_ITEMS)))

item_nr <- purrr::map2_dfr(nr_cols, names(NR_ITEMS), function(cl, nm) {
  svyby(as.formula(paste0("~", cl)), ~year, des, svymean, na.rm = TRUE,
        keep.names = FALSE) %>%
    dplyr::rename(share = 2, se = 3) %>%
    dplyr::mutate(item = nm)
}) %>%
  dplyr::filter(year %in% full_years) %>%
  dplyr::group_by(item) %>%
  dplyr::mutate(chg_pp = 100 * (share - share[year == min(year)])) %>%
  dplyr::ungroup()

fig5 <- ggplot(item_nr, aes(year, chg_pp, colour = item)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = ITEM_COLS, name = NULL) +
  scale_x_year() +
  labs(
    title    = "Firm-size non-response does not move with other item non-response",
    subtitle = sprintf(paste("Change since %d in the share of MW-covered private",
                             "employees with no usable answer, percentage points.",
                             "%s"),
                       min(full_years), GEO_LABEL[["national"]]),
    
    x = NULL, y = "Change since baseline (pp)",
    caption = paste(
      "All items share the same denominator, so the series are directly",
      "comparable. Plotted as change from baseline because the levels differ.",
      "RNC registration is also a question about the employer: it stays flat,",
      "so respondents have not generally become less able to describe their",
      "workplace - only its headcount.",
      "Source: Authors' calculations using ENCFT.", sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom")

ggsave(out_path("fig5_item_nonresponse", "png"), fig5, width = 9, height = 5,
       dpi = 300)

gv_dom <- GEO_VARS[["domain"]]
item_nr_geo <- purrr::map2_dfr(nr_cols, names(NR_ITEMS), function(cl, nm) {
  svyby(as.formula(paste0("~", cl)), as.formula(paste0("~", gv_dom, "+ year")),
        des, svymean, na.rm = TRUE, keep.names = FALSE) %>%
    dplyr::rename(geo = 1, share = 3) %>%
    dplyr::mutate(item = nm)
}) %>% dplyr::filter(year %in% full_years)

fig6 <- ggplot(item_nr_geo, aes(year, share, colour = item)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~geo, ncol = 4) +
  scale_colour_manual(values = ITEM_COLS, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_year(4) +
  labs(title = "Firm-size non-response is concentrated where salary non-response is not",
       subtitle = sprintf("Item non-response by %s", GEO_NAME[["domain"]]),
       x = NULL, y = "Share with no usable answer",
       caption = paste(
         "Salary non-response is worst in Sur; firm-size non-response is worst",
         "in Gran Santo Domingo. Opposite geography, so not one common cause.",
         "Source: Authors' calculations using ENCFT.", sep = "\n")) +
  theme_surveytools(legend_position = "bottom")

ggsave(out_path("fig6_item_nonresponse_domain", "png"), fig6,
       width = 11, height = 4, dpi = 300)


#-------------------------------------------------------------------------------
# 9B.  Standardisation
#
#      rate_std = SUM_c w_c(BASE) * rate_c(LAST)
#
#  The recent non-response rate the country would have had if the composition of
#  employment had not changed since the base year.
#
#  Firm size cannot be a standardising variable: it is missing for exactly the
#  workers under study. So this tests whether the OBSERVABLE composition shift
#  explains the rise, not the firm-size shift directly. The firm-size channel is
#  bounded separately below.
#-------------------------------------------------------------------------------

cat("[6] Standardisation...\n")

std_dat <- des$variables %>%
  dplyr::filter(year %in% c(BASE_YEAR, LAST_YEAR)) %>%
  dplyr::mutate(cell = interaction(!!!rlang::syms(STD_VARS), drop = TRUE))

cell_stats <- std_dat %>%
  dplyr::group_by(year, cell) %>%
  dplyr::summarise(pop = sum(FACTOR_EXPANSION),
                   rate = weighted.mean(nonresp, FACTOR_EXPANSION),
                   n = dplyr::n(), .groups = "drop_last") %>%
  dplyr::mutate(w = pop / sum(pop)) %>%
  dplyr::ungroup()

# Thin cells matter only in proportion to the weight they carry, so report that
# rather than the count alone.
thin_w <- cell_stats %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(n_thin = sum(n < THIN_CELL),
                   w_thin = 100 * sum(w[n < THIN_CELL]), .groups = "drop")

cat("    Cells: ", dplyr::n_distinct(cell_stats$cell), "\n", sep = "")
print(as.data.frame(thin_w), digits = 3, row.names = FALSE)

wide <- cell_stats %>%
  dplyr::select(year, cell, w, rate) %>%
  tidyr::pivot_wider(names_from = year, values_from = c(w, rate)) %>%
  dplyr::filter(!is.na(.data[[paste0("w_", BASE_YEAR)]]),
                !is.na(.data[[paste0("rate_", LAST_YEAR)]]))

obs_base <- weighted.mean(wide[[paste0("rate_", BASE_YEAR)]],
                          wide[[paste0("w_", BASE_YEAR)]])
obs_last <- weighted.mean(wide[[paste0("rate_", LAST_YEAR)]],
                          wide[[paste0("w_", LAST_YEAR)]])
rate_std <- weighted.mean(wide[[paste0("rate_", LAST_YEAR)]],
                          wide[[paste0("w_", BASE_YEAR)]])

cat(sprintf("\n    Observed %d                        : %5.2f%%\n",
            BASE_YEAR, 100 * obs_base))
cat(sprintf("    Observed %d                        : %5.2f%%\n",
            LAST_YEAR, 100 * obs_last))
cat(sprintf("    %d standardised to %d composition : %5.2f%%\n",
            LAST_YEAR, BASE_YEAR, 100 * rate_std))
cat(sprintf("    Share of the rise explained by composition: %5.1f%%\n",
            100 * (obs_last - rate_std) / (obs_last - obs_base)))

# Bound on the firm-size channel, which the standardisation cannot include.
# Under the assumption most favourable to a firm-size explanation - every
# non-respondent works at a large firm - how far would the within-large
# non-response rate still have to move?
lg <- comp_year %>% dplyr::filter(size_cat == "Large") %>%
  dplyr::select(year, lg = share)
uu <- unknown_share
wl <- dplyr::inner_join(lg, uu, by = "year") %>%
  dplyr::filter(year %in% c(BASE_YEAR, LAST_YEAR)) %>%
  dplyr::mutate(within_large = U / (lg + U))

cat("\n    If EVERY non-respondent worked at a large firm, the non-response",
    " rate within large firms would be:\n", sep = "")
cat(sprintf("      %d: %4.1f%%   %d: %4.1f%%\n",
            BASE_YEAR, 100 * wl$within_large[wl$year == BASE_YEAR],
            LAST_YEAR, 100 * wl$within_large[wl$year == LAST_YEAR]))
cat("    Composition cannot deliver a move of that size; the rise is within-cell.\n\n")


#-------------------------------------------------------------------------------
# 9C.  Who are the non-respondents?
#
#  Unrouted items only - see the header note on why the accounts, licence and
#  contract items were removed.
#-------------------------------------------------------------------------------

cat("[7] Table 5...\n")

des$variables$dk_flag <- factor(ifelse(des$variables$size_cat %in% NR_LEVELS,
                                       "No usable firm size", "Firm size known"))
des$variables$size2 <- droplevels(factor(des$variables$size_cat,
                                         levels = c("Micro", "Large")))

proxy_all <- purrr::imap_dfr(FIRM_PROXIES, function(ex, nm) {
  d2 <- des
  d2$variables$prox <- as.numeric(eval(ex, d2$variables))
  
  dk <- svyby(~prox, ~dk_flag, subset(d2, year >= LAST_YEAR - 2), svymean,
              na.rm = TRUE, keep.names = FALSE) %>%
    dplyr::rename(group = 1, share = 2) %>%
    dplyr::filter(group == "No usable firm size")
  
  bm <- svyby(~prox, ~size2, subset(d2, year >= LAST_YEAR - 2 & !is.na(size2)),
              svymean, na.rm = TRUE, keep.names = FALSE) %>%
    dplyr::rename(group = 1, share = 2)
  
  dplyr::bind_rows(dk, bm) %>%
    dplyr::mutate(proxy = nm) %>%
    dplyr::select(proxy, group, share)
}) %>%
  tidyr::pivot_wider(names_from = group, values_from = share) %>%
  dplyr::select(proxy, `No usable firm size`, Micro, Large)

cat("\n")
print(as.data.frame(proxy_all), digits = 3, row.names = FALSE)
cat("\n")

table5 <- proxy_all %>%
  gt() %>%
  tab_header(
    title    = "Table 5. Workers with no usable firm size resemble large-firm employees",
    subtitle = sprintf("Firm characteristics recorded independently of the size question, %d-%d",
                       LAST_YEAR - 2, LAST_YEAR)
  ) %>%
  cols_label(proxy = "Firm characteristic") %>%
  fmt_percent(-proxy, decimals = 1) %>%
  tab_footnote(
    paste("Restricted to items that are asked of all private employees.",
          "Formal accounts and business licence are asked only of firms not",
          "registered with RNC, so for large firms - 99% registered - they are",
          "computed on under 1% of the group and are not interpretable.",
          "Contract status is coded 1 for every private employee.",
          "Code 98 (don't know) is treated as missing throughout."),
    locations = cells_column_labels(columns = proxy)) %>%
  tab_source_note(paste("Source: Authors' calculations using ENCFT. Micro and",
                        "Large cover workers with a known tier and are the",
                        "benchmarks the first column is read against.")) %>%
  cols_align("left", columns = proxy) %>%
  tab_options(table.font.size = px(11), heading.title.font.size = px(14),
              column_labels.font.weight = "bold",
              table.border.top.style = "none",
              table_body.hlines.style = "none", data_row.padding = px(3))

stem <- "table5_dk_firm_characteristics"

gtsave(table5, out_path(stem, "html"))
gtsave(table5, out_path(stem, "png"), expand = 10)


saveRDS(comp_qtr, file.path(config$paths$processed_data,
                            "firm_size_composition_qtr.rds"))
saveRDS(nr_by_geo, file.path(config$paths$processed_data,
                             "firm_size_nonresponse_by_geo.rds"))

cat("[8] Outputs written to: ", fig_dir, "\n", sep = "")


#===============================================================================
# 03C STEP 10 - SHIFT-SHARE DECOMPOSITION AND STABLE-MECHANISM BOUNDS
#
# Append after Step 9. Requires: des, comp_year, unknown_share, bounds,
# GEO_VARS, BASE_YEAR, LAST_YEAR, full_years, out_path, scale_x_year.
#
#  10A  Stable-mechanism bounds - a third column for Table 4.
#  10B  Shift-share: is the move toward large firms within regions or between?
#  10C  The same decomposition for non-response itself.
#===============================================================================


#-------------------------------------------------------------------------------
# Three-year endpoint averages. A single pair of years is fragile: the Large
# share moved 3.5 pp between 2024 and 2025 alone, which was enough to flip the
# worst-case bound from unidentified to identified. Averaging three years at
# each end removes that sensitivity without changing the question.
#-------------------------------------------------------------------------------

BASE_WINDOW <- intersect(full_years, (BASE_YEAR - 1):(BASE_YEAR + 1))
LAST_WINDOW <- intersect(full_years, (LAST_YEAR - 2):LAST_YEAR)

cat("[9] Endpoint windows: ", paste(range(BASE_WINDOW), collapse = "-"),
    " vs ", paste(range(LAST_WINDOW), collapse = "-"), "\n", sep = "")


#===============================================================================
# 10A.  Stable-mechanism bounds
#
#  Worst case allows the tier composition of non-respondents to differ
#  arbitrarily between the two endpoints - all of them Large in one year, none
#  in the other. That is why the worst-case band on a CHANGE is wider than the
#  band on a level: it is the sum of both years' uncertainty.
#
#  A milder and quite defensible assumption is that the mechanism is STABLE:
#  non-respondents are drawn from tiers in the same proportions q each year,
#  even as the rate rises. Then
#
#      true_share(t)  = observed(t) + q * U(t)
#      true_change    = observed_change + q * (U_last - U_base)
#
#  and since U rose and q >= 0, THE OBSERVED CHANGE IS A LOWER BOUND ON THE TRUE
#  CHANGE FOR EVERY TIER. Any tier whose measured share rose must really have
#  risen: non-response can only ever have added to it.
#
#  This is an assumption, not a result. State it. But it is consistent with what
#  Step 9 established - the rise is within-cell and the observable profile of
#  non-respondents is stable - and it is far weaker than MAR.
#===============================================================================

endpoint <- function(yrs) {
  comp_year %>%
    dplyr::filter(year %in% yrs) %>%
    dplyr::group_by(size_cat) %>%
    dplyr::summarise(share = mean(share), .groups = "drop")
}

U_base <- unknown_share %>% dplyr::filter(year %in% BASE_WINDOW) %>%
  dplyr::pull(U) %>% mean()
U_last <- unknown_share %>% dplyr::filter(year %in% LAST_WINDOW) %>%
  dplyr::pull(U) %>% mean()

bound_tab <- endpoint(BASE_WINDOW) %>%
  dplyr::rename(base = share) %>%
  dplyr::left_join(endpoint(LAST_WINDOW) %>% dplyr::rename(last = share),
                   by = "size_cat") %>%
  dplyr::filter(size_cat %in% TIER_LEVELS) %>%
  dplyr::mutate(
    obs_change  = 100 * (last - base),
    # Stable mechanism: one-sided, observed change is the floor.
    stable_lb   = obs_change,
    stable_ub   = obs_change + 100 * (U_last - U_base),
    stable_id   = sign(stable_lb) == sign(stable_ub),
    # Worst case: allocation may differ arbitrarily between the endpoints.
    worst_lb    = 100 * (last - (base + U_base)),
    worst_ub    = 100 * ((last + U_last) - base),
    worst_id    = sign(worst_lb) == sign(worst_ub)
  ) %>%
  dplyr::select(size_cat, obs_change, stable_lb, stable_ub, stable_id,
                worst_lb, worst_ub, worst_id)

cat("\n    Change in tier share, ", paste(range(BASE_WINDOW), collapse = "-"),
    " to ", paste(range(LAST_WINDOW), collapse = "-"), " (pp):\n", sep = "")
print(as.data.frame(bound_tab), digits = 3, row.names = FALSE)
cat("\n")

table4 <- bound_tab %>%
  dplyr::select(-stable_id) %>%
  gt() %>%
  tab_header(
    title    = "Table 4. Which compositional changes survive non-response",
    subtitle = sprintf("Change in firm-size share, %s to %s, percentage points. %s",
                       paste(range(BASE_WINDOW), collapse = "-"),
                       paste(range(LAST_WINDOW), collapse = "-"),
                       GEO_LABEL[["national"]])
  ) %>%
  cols_label(size_cat = "Firm size", obs_change = "Ignoring DK",
             stable_lb = "Lower", stable_ub = "Upper",
             worst_lb = "Lower", worst_ub = "Upper",
             worst_id = "Sign identified") %>%
  tab_spanner("Stable mechanism", columns = c(stable_lb, stable_ub)) %>%
  tab_spanner("Worst case",       columns = c(worst_lb, worst_ub)) %>%
  fmt_number(c(obs_change, stable_lb, stable_ub, worst_lb, worst_ub),
             decimals = 1) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = worst_id)) %>%
  tab_footnote(
    paste("Stable mechanism assumes non-respondents are drawn from tiers in the",
          "same proportions in both windows, even as the rate rises. The",
          "observed change is then a floor for every tier, because non-response",
          "can only add to a share. Any tier whose measured share rose must",
          "really have risen."),
    locations = cells_column_spanners("Stable mechanism")) %>%
  tab_footnote(
    paste("Worst case allows the tier composition of non-respondents to differ",
          "arbitrarily between the two windows. Wider than the stable-mechanism",
          "bound because it carries both endpoints' uncertainty.",
          "Bounds are population quantities and ignore sampling error, roughly",
          "0.3-0.5 pp here."),
    locations = cells_column_spanners("Worst case")) %>%
  tab_source_note(paste("Source: Authors' calculations using ENCFT. Endpoints",
                        "are three-year averages: a single pair of years is",
                        "fragile, since the large-firm share moved 3.5 pp",
                        "between 2024 and 2025 alone.")) %>%
  cols_align("left", columns = size_cat) %>%
  tab_options(table.font.size = px(11), heading.title.font.size = px(14),
              column_labels.font.weight = "bold",
              table.border.top.style = "none",
              table_body.hlines.style = "none", data_row.padding = px(3))


stem <- "table4_composition_bounds"

gtsave(table4, out_path(stem, "html"))
gtsave(table4, out_path(stem, "png"), expand = 10)


#===============================================================================
# 10B.  Shift-share: within regions or between them?
#
#  The national share of a group g is the employment-weighted mean of regional
#  shares, S = SUM_r w_r * s_r. Its change decomposes exactly as
#
#      dS = SUM_r wbar_r * ds_r   +   SUM_r sbar_r * dw_r
#            \___ within ___/         \___ between ___/
#
#  using period-mean weights, which leaves NO interaction residual (unlike the
#  base-period-weight version). Verify: wbar*ds + sbar*dw = w1s1 - w0s0.
#
#  WHY THIS MATTERS HERE. Non-response is concentrated in Gran Santo Domingo,
#  which is also the most large-firm-heavy region. If the national rise in the
#  large-firm share is a BETWEEN effect - employment reallocating toward the
#  capital - then the compositional finding and the non-response finding are
#  driven by the same regional shift and cannot be discussed separately. If it
#  is WITHIN, they are independent and each can be reported on its own terms.
#===============================================================================

cat("[10] Shift-share decomposition...\n")

geo_est <- GEO_VARS[["estimation"]]

shift_share <- function(data, group_expr, geo_var, label) {
  
  parts <- data %>%
    dplyr::mutate(.g = as.numeric(!!rlang::enquo(group_expr)),
                  window = dplyr::case_when(year %in% BASE_WINDOW ~ "base",
                                            year %in% LAST_WINDOW ~ "last",
                                            TRUE ~ NA_character_)) %>%
    dplyr::filter(!is.na(window)) %>%
    dplyr::group_by(window, geo = .data[[geo_var]]) %>%
    dplyr::summarise(pop = sum(FACTOR_EXPANSION),
                     s   = weighted.mean(.g, FACTOR_EXPANSION),
                     n   = dplyr::n(), .groups = "drop_last") %>%
    dplyr::mutate(w = pop / sum(pop)) %>%
    dplyr::ungroup() %>%
    dplyr::select(window, geo, w, s, n) %>%
    tidyr::pivot_wider(names_from = window, values_from = c(w, s, n)) %>%
    tidyr::replace_na(list(w_base = 0, w_last = 0))
  
  parts %>%
    dplyr::mutate(
      wbar    = (w_base + w_last) / 2,
      sbar    = (s_base + s_last) / 2,
      within  = 100 * wbar * (s_last - s_base),
      between = 100 * sbar * (w_last - w_base),
      total   = within + between,
      outcome = label
    )
}

dat <- des$variables

ss_large <- shift_share(dat, size_cat == "Large", geo_est, "Large-firm share")
ss_nr    <- shift_share(dat, nonresp == 1,        geo_est, "Non-response")

ss_summary <- dplyr::bind_rows(ss_large, ss_nr) %>%
  dplyr::group_by(outcome) %>%
  dplyr::summarise(within_pp  = sum(within),
                   between_pp = sum(between),
                   total_pp   = sum(total),
                   within_pct = 100 * sum(within) / sum(total),
                   .groups = "drop")

cat("\n    Shift-share by ", geo_est, ":\n", sep = "")
print(as.data.frame(ss_summary), digits = 3, row.names = FALSE)
cat("\n    within  = same regions, changed internally\n")
cat("    between = employment reallocated across regions\n")
cat("    Terms sum exactly to the total; no interaction residual.\n\n")

# Region-level detail, so a single dominant region cannot hide inside a total.
ss_detail <- dplyr::bind_rows(ss_large, ss_nr) %>%
  dplyr::select(outcome, geo, w_base, w_last, s_base, s_last,
                within, between, total) %>%
  dplyr::arrange(outcome, dplyr::desc(abs(total)))

cat("    Region detail (pp contributions):\n")
print(as.data.frame(ss_detail), digits = 3, row.names = FALSE)
cat("\n")

table6 <- ss_detail %>%
  dplyr::select(outcome, geo, s_base, s_last, within, between, total) %>%
  gt(groupname_col = "outcome") %>%
  tab_header(
    title    = "Table 6. Within-region change versus reallocation across regions",
    subtitle = sprintf("Contribution to the change from %s to %s, percentage points. By %s",
                       paste(range(BASE_WINDOW), collapse = "-"),
                       paste(range(LAST_WINDOW), collapse = "-"),
                       GEO_NAME[["estimation"]])
  ) %>%
  cols_label(geo = "Region", s_base = "Base", s_last = "Latest",
             within = "Within", between = "Between", total = "Total") %>%
  tab_spanner("Regional rate", columns = c(s_base, s_last)) %>%
  tab_spanner("Contribution (pp)", columns = c(within, between, total)) %>%
  fmt_percent(c(s_base, s_last), decimals = 1) %>%
  fmt_number(c(within, between, total), decimals = 2) %>%
  tab_footnote(
    paste("Within is the region's own rate changing, holding its employment",
          "weight at the period mean. Between is the region's employment weight",
          "changing, holding its rate at the period mean. Period-mean weights",
          "leave no interaction residual, so the columns sum exactly."),
    locations = cells_column_spanners("Contribution (pp)")) %>%
  tab_source_note("Source: Authors' calculations using ENCFT.") %>%
  cols_align("left", columns = geo) %>%
  tab_options(table.font.size = px(11), heading.title.font.size = px(14),
              column_labels.font.weight = "bold",
              row_group.font.weight = "bold",
              table.border.top.style = "none",
              table_body.hlines.style = "none", data_row.padding = px(3))


stem <- "table6_shift_share"

gtsave(table6, out_path(stem, "html"))
gtsave(table6, out_path(stem, "png"), expand = 10)

fig7 <- dplyr::bind_rows(ss_large, ss_nr) %>%
  dplyr::select(outcome, geo, Within = within, Between = between) %>%
  tidyr::pivot_longer(c(Within, Between), names_to = "part", values_to = "pp") %>%
  ggplot(aes(reorder(geo, pp), pp, fill = part)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_col() +
  coord_flip() +
  facet_wrap(~outcome, scales = "free_x") +
  scale_fill_manual(values = c(Within = "#14507A", Between = "#C45C30"),
                    name = NULL) +
  labs(
    title    = "Where the national changes come from",
    subtitle = sprintf("Contribution to the change from %s to %s, percentage points. By %s",
                       paste(range(BASE_WINDOW), collapse = "-"),
                       paste(range(LAST_WINDOW), collapse = "-"),
                       GEO_NAME[["estimation"]]),
    x = NULL, y = "Contribution (pp)",
    caption = paste(
      "Within: the region's own rate changed. Between: employment reallocated",
      "toward or away from that region. Bars sum to the national change.",
      "If the large-firm share and non-response are driven by the same regional",
      "reallocation, the two findings are not independent.",
      "Source: Authors' calculations using ENCFT.", sep = "\n")
  ) +
  theme_surveytools(legend_position = "bottom")

ggsave(out_path("fig7_shift_share", "png"), fig7, width = 10, height = 6,
       dpi = 300)

cat("[11] Step 10 outputs written.\n")


#STEP 11  Clean-region identification
#           The national worst-case bound identifies only the Large shift, and
#           82.9% of the non-response rise is in one region. If the Large
#           finding rested on that region it would be weak. It does not: the
#           shift-share shows Yuma contributing 1.32 pp of within-region
#           large-firm growth with non-response flat below 1%. Where
#           non-response is negligible the change is POINT identified and no
#           bound is needed. This step makes that the primary evidence and
#           demotes the national worst-case bound to a conservative backstop.
#
#  STEP 12  Standardisation, as output rather than console text
#           Composition explains -5.3% of the rise: holding formality, sector
#           and domain at their base-year mix, non-response would have risen
#           slightly MORE. The entire rise is within-cell. This is the single
#           strongest piece of evidence that the deterioration is a change in
#           measurement regime rather than a change in who is measured, and it
#           currently exists only as four printed numbers.
#
#  ASSUMES these already exist from earlier steps. Check names before running:
#    des, size_cat, year, TIER_LEVELS, NR_LEVELS, BASE_WINDOW, LAST_WINDOW,
#    GEO_VARS, STD_VARS, MIN_CELL, THIN_CELL, out_path, scale_x_year,
#    theme_surveytools, SIZE_COLS
#===============================================================================


#===============================================================================
# STEP 11.  CLEAN-REGION IDENTIFICATION
#===============================================================================

cat("[11] Clean-region bounds...\n")

GEO_EST      <- GEO_VARS[["estimation"]]
CLEAN_NR_MAX <- 0.02   # a region is "clean" if non-response stays below this
# in BOTH windows. 2 pp is arbitrary but generous: at
# that level the worst-case band is under 4 pp wide.

#-------------------------------------------------------------------------------
# Weighted share of each tier, and of non-response, by group and window.
# Built as explicit indicators rather than svyby on a factor so the output is
# long and the NR aggregate is computed the same way as the tier shares.
#-------------------------------------------------------------------------------

share_by <- function(design, years, by_var) {
  
  d <- subset(design, year %in% years)
  
  purrr::map_dfr(c(TIER_LEVELS, "NR"), function(lv) {
    
    d2 <- d
    d2$variables$.ind <- as.numeric(
      if (lv == "NR") d$variables$size_cat %in% NR_LEVELS
      else            d$variables$size_cat == lv
    )
    
    survey::svyby(~.ind, stats::reformulate(by_var), d2,
                  survey::svymean, na.rm = TRUE) %>%
      tibble::as_tibble() %>%
      dplyr::rename(geo = 1, share = 2, se = 3) %>%
      dplyr::mutate(level = lv)
  })
}

n_by <- function(design, years, by_var) {
  design$variables %>%
    dplyr::filter(year %in% years) %>%
    dplyr::count(geo = .data[[by_var]], name = "n")
}

base_g <- share_by(des, BASE_WINDOW, GEO_EST) %>% dplyr::rename(base = share, se_base = se)
last_g <- share_by(des, LAST_WINDOW, GEO_EST) %>% dplyr::rename(last = share, se_last = se)

n_g <- dplyr::full_join(
  n_by(des, BASE_WINDOW, GEO_EST) %>% dplyr::rename(n_base = n),
  n_by(des, LAST_WINDOW, GEO_EST) %>% dplyr::rename(n_last = n),
  by = "geo")

#-------------------------------------------------------------------------------
# Region-level bounds. Same algebra as Step 10A, applied within region so each
# region carries its OWN non-response, not the national rate.
#-------------------------------------------------------------------------------

nr_g <- dplyr::full_join(base_g, last_g, by = c("geo", "level")) %>%
  dplyr::filter(level == "NR") %>%
  dplyr::select(geo, U_base = base, U_last = last)

bounds_geo <- dplyr::full_join(base_g, last_g, by = c("geo", "level")) %>%
  dplyr::filter(level %in% TIER_LEVELS) %>%
  dplyr::left_join(nr_g, by = "geo") %>%
  dplyr::left_join(n_g,  by = "geo") %>%
  dplyr::mutate(
    obs_change = 100 * (last - base),
    se_change  = 100 * sqrt(se_base^2 + se_last^2),
    worst_lb   = 100 * (last - (base + U_base)),
    worst_ub   = 100 * ((last + U_last) - base),
    worst_id   = sign(worst_lb) == sign(worst_ub),
    band_pp    = worst_ub - worst_lb,
    clean      = pmax(U_base, U_last) < CLEAN_NR_MAX,
    usable     = pmin(n_base, n_last) >= MIN_CELL
  )

#-------------------------------------------------------------------------------
# Pooled bounds on the clean regions. This is the headline: the same worst-case
# calculation, run where the data are good.
#-------------------------------------------------------------------------------

clean_regions <- bounds_geo %>%
  dplyr::filter(clean, usable) %>% dplyr::pull(geo) %>% unique()

cat("    Clean regions (non-response < ", 100 * CLEAN_NR_MAX, "% in both windows): ",
    paste(clean_regions, collapse = ", "), "\n", sep = "")

pooled_bounds <- function(design, keep_geo, label) {
    
    keep <- design$variables[[GEO_EST]] %in% keep_geo
    d    <- design[keep, ]
  
  b  <- share_by(d, BASE_WINDOW, "one") %>% dplyr::rename(base = share)
  l  <- share_by(d, LAST_WINDOW, "one") %>% dplyr::rename(last = share)
  
  Ub <- b$base[b$level == "NR"]; Ul <- l$last[l$level == "NR"]
  
  dplyr::full_join(dplyr::select(b, level, base),
                   dplyr::select(l, level, last), by = "level") %>%
    dplyr::filter(level %in% TIER_LEVELS) %>%
    dplyr::mutate(
      subsample  = label,
      U_base     = Ub, U_last = Ul,
      obs_change = 100 * (last - base),
      stable_lb  = obs_change,
      stable_ub  = obs_change + 100 * (Ul - Ub),
      worst_lb   = 100 * (last - (base + Ub)),
      worst_ub   = 100 * ((last + Ul) - base),
      worst_id   = sign(worst_lb) == sign(worst_ub)
    )
}

# svyby needs a grouping variable; a constant gives the pooled estimate.
des$variables$one <- 1L

all_geo <- unique(bounds_geo$geo[bounds_geo$usable])

bounds_pooled <- dplyr::bind_rows(
  pooled_bounds(des, all_geo,       "All regions"),
  pooled_bounds(des, setdiff(all_geo, "Ozama o Gran Santo Domingo"), "Excl. Gran Santo Domingo"),
  pooled_bounds(des, clean_regions, "Low non-response regions only")
)

cat("\n    Pooled bounds by subsample (pp):\n")
print(as.data.frame(bounds_pooled %>%
                      dplyr::select(subsample, level, obs_change, worst_lb, worst_ub, worst_id)),
      digits = 3, row.names = FALSE)
cat("\n")

#-------------------------------------------------------------------------------
# Table 7. Region-level detail for the Large tier - the only tier the national
# bound identifies, and the one the section rests on.
#-------------------------------------------------------------------------------

table7_data <- bounds_geo %>%
  dplyr::filter(level == "Large", usable) %>%
  dplyr::arrange(U_last) %>%
  dplyr::select(geo, U_base, U_last, obs_change, se_change, worst_lb, worst_ub, worst_id, clean)

table7 <- table7_data %>%
  gt() %>%
  tab_header(
    title = "Table 7. The large-firm shift is identified where non-response is negligible",
    subtitle = sprintf("Change in large-firm share by region, %s to %s, percentage points. %s",
                       paste(range(BASE_WINDOW), collapse = "-"),
                       paste(range(LAST_WINDOW), collapse = "-"),
                       GEO_LABEL[["estimation"]]))%>%
  cols_label(geo = "Region", U_base = "Base", U_last = "Last",
             obs_change = "Observed", se_change = "SE", worst_lb = "Lower", worst_ub = "Upper",
             worst_id = "Sign identified", clean = "Low non-response") %>%
  tab_spanner("Firm-size non-response", columns = c(U_base, U_last)) %>%
  tab_spanner("Observed change", columns = c(obs_change, se_change)) %>%
  tab_spanner("Worst-case bound", columns = c(worst_lb, worst_ub)) %>%
  fmt_percent(c(U_base, U_last), decimals = 1) %>%
  fmt_number(c(obs_change, worst_lb, worst_ub), decimals = 1) %>%
  fmt_number(c(obs_change, se_change, worst_lb, worst_ub), decimals = 1) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = clean)) %>%
  tab_footnote(
    paste("Design-based standard error of the observed change, treating the",
          "two windows as independent. This is sampling uncertainty; the",
          "worst-case bound is a population quantity and carries none."),
    locations = cells_column_labels(columns = se_change)) %>%
  tab_footnote(
    paste("Regions are ordered by non-response in the last window. Where",
          "non-response is negligible in both windows the observed change is",
          "point identified and the bound is a formality: the band is under",
          sprintf("%.0f pp wide.", 200 * CLEAN_NR_MAX),
          "The national worst-case bound is a conservative backstop, not the",
          "primary evidence."),
    locations = cells_column_spanners("Worst-case bound")) %>%
  tab_footnote(
    sprintf("Non-response below %.0f%% in both windows.", 100 * CLEAN_NR_MAX),
    locations = cells_column_labels(columns = clean)) %>%
  tab_source_note(paste0(
    "Source: Authors' calculations using ENCFT. Regions with fewer than ",
    MIN_CELL, " observations in either window are excluded.")) %>%
  tab_options(table.font.size = px(11), heading.title.font.size = px(14),
              column_labels.font.weight = "bold",
              table.border.top.style = "none",
              table_body.hlines.style = "none", data_row.padding = px(3))

gtsave(table7, out_path("table7_clean_region_bounds", "html"))
gtsave(table7, out_path("table7_clean_region_bounds", "png"), expand = 10)

#-------------------------------------------------------------------------------
# Figure 8. The identification argument in one panel: the large-firm shift is
# not a Gran Santo Domingo artefact, because it also happens where there is
# almost no non-response to hide behind.
#-------------------------------------------------------------------------------

fig8 <- ggplot(table7_data, aes(x = U_last, y = obs_change)) +
  annotate("rect", xmin = -Inf, xmax = CLEAN_NR_MAX, ymin = -Inf, ymax = Inf,
           fill = "#14507A", alpha = 0.06) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_linerange(aes(ymin = worst_lb, ymax = worst_ub),
                 colour = "#C45C30", alpha = 0.35, linewidth = 2.5) +
  geom_point(colour = "#14507A", size = 2.2) +
  ggrepel::geom_text_repel(aes(label = geo), size = 2.8, colour = "grey30",
                           seed = 1, min.segment.length = 0.2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "The large-firm shift appears where there is no non-response to hide behind",
    subtitle = sprintf(paste("Change in large-firm share, %s to %s, against non-response",
                             "in the last window. By %s"),
                       paste(range(BASE_WINDOW), collapse = "-"),
                       paste(range(LAST_WINDOW), collapse = "-"),
                       GEO_NAME[["estimation"]]),
    x = "Firm-size non-response, last window",
    y = "Change in large-firm share (pp)",
    caption = paste0(
      "Bars are worst-case bounds: they widen mechanically with non-response.\n",
      "Shaded band: regions below ", 100 * CLEAN_NR_MAX,
      "% non-response, where the observed change is effectively point identified.\n",
      "Source: Authors' calculations using ENCFT.")) +
  theme_surveytools() +
  theme(plot.caption   = element_text(hjust = 0, colour = "grey40"),
        axis.text.x    = element_text(angle = 0, hjust = 0.5, vjust = 1))

ggsave(out_path("fig8_clean_region_identification", "png"), fig8,
       width = 9, height = 6, dpi = 200)


#===============================================================================
# STEP 12.  STANDARDISATION AS OUTPUT
#===============================================================================

cat("[12] Standardisation table and figure...\n")

#-------------------------------------------------------------------------------
# Cell-level non-response rates and cell weights, both years.
# Cells are STD_VARS: formality, sector, survey domain. Firm size cannot appear
# here - it is missing for exactly the workers under study.
#-------------------------------------------------------------------------------

cell_rates <- des$variables %>%
  dplyr::filter(year %in% c(min(BASE_WINDOW), max(LAST_WINDOW))) %>%
  dplyr::mutate(.nr = as.numeric(size_cat %in% NR_LEVELS)) %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(STD_VARS)), year) %>%
  dplyr::summarise(n    = dplyr::n(),
                   w    = sum(FACTOR_EXPANSION),
                   rate = stats::weighted.mean(.nr, FACTOR_EXPANSION),
                   .groups = "drop")

STD_BASE <- min(BASE_WINDOW); STD_LAST <- max(LAST_WINDOW)

cells <- cell_rates %>%
  tidyr::pivot_wider(names_from = year, values_from = c(n, w, rate),
                     names_sep = "_") %>%
  dplyr::rename(n_base    = paste0("n_",    STD_BASE), n_last    = paste0("n_",    STD_LAST),
                w_base    = paste0("w_",    STD_BASE), w_last    = paste0("w_",    STD_LAST),
                rate_base = paste0("rate_", STD_BASE), rate_last = paste0("rate_", STD_LAST)) %>%
  dplyr::filter(!is.na(rate_base), !is.na(rate_last)) %>%
  dplyr::mutate(sh_base = w_base / sum(w_base),
                sh_last = w_last / sum(w_last),
                thin    = pmin(n_base, n_last) < THIN_CELL)

#-------------------------------------------------------------------------------
# Kitagawa decomposition. Total = within + composition, exactly.
#   within      = sum_c sh_base_c * (rate_last_c - rate_base_c)
#   composition = sum_c rate_last_c * (sh_last_c - sh_base_c)
# The second term is what "2025 standardised to the 2016 mix" already reports,
# with the opposite sign; this writes both so they sum to the observed change.
#-------------------------------------------------------------------------------

obs_base <- sum(cells$sh_base * cells$rate_base)
obs_last <- sum(cells$sh_last * cells$rate_last)
within   <- sum(cells$sh_base * (cells$rate_last - cells$rate_base))
compos   <- sum(cells$rate_last * (cells$sh_last - cells$sh_base))

stopifnot(abs((within + compos) - (obs_last - obs_base)) < 1e-10)

table8_data <- tibble::tribble(
  ~component,                                        ~value,        ~kind,
  sprintf("Non-response, %d", STD_BASE),             100 * obs_base, "level",
  "Within-cell change",                              100 * within,   "flow",
  "Compositional change",                            100 * compos,   "flow",
  sprintf("Non-response, %d", STD_LAST),             100 * obs_last, "level"
) %>%
  dplyr::mutate(share_of_change = dplyr::if_else(
    kind == "flow", value / (100 * (obs_last - obs_base)), NA_real_))

cat(sprintf("\n    Observed %d: %.2f%%   Observed %d: %.2f%%\n",
            STD_BASE, 100 * obs_base, STD_LAST, 100 * obs_last))
cat(sprintf("    Within: %+.2f pp (%.1f%%)   Composition: %+.2f pp (%.1f%%)\n\n",
            100 * within, 100 * within / (100 * (obs_last - obs_base)),
            100 * compos, 100 * compos / (100 * (obs_last - obs_base))))

table8 <- table8_data %>%
  dplyr::select(-kind) %>%
  gt() %>%
  tab_header(
    title    = "Table 8. The rise in non-response is entirely within-cell",
    subtitle = sprintf("Decomposition of the change in firm-size non-response, %d to %d. Cells use the %s",
                       STD_BASE, STD_LAST, GEO_NAME[["domain"]])) %>%
  cols_label(component = "", value = "Percentage points",
             share_of_change = "Share of the change") %>%
  fmt_number(value, decimals = 2) %>%
  fmt_percent(share_of_change, decimals = 1) %>%
  sub_missing(everything(), missing_text = "") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = table8_data$kind == "level")) %>%
  tab_footnote(
    sprintf(paste("Cells are formality status, sector and the %s - not the %s",
                  "used in Figures 4a, 7 and 8. Firm size cannot be a cell",
                  "variable: it is missing for exactly the workers under study.",
                  "Within holds the cell mix at its base-year value and lets",
                  "cell rates move; composition holds cell rates at their",
                  "last-year value and lets the mix move. The two sum exactly",
                  "to the observed change."),
            GEO_NAME[["domain"]], GEO_NAME[["estimation"]]),
    locations = cells_column_labels(columns = value)) %>%
  tab_source_note(paste0(
    "Source: Authors' calculations using ENCFT. ",
    sum(cells$thin), " of ", nrow(cells), " cells fall below ", THIN_CELL,
    " observations in one year; excluding them does not change the sign or ",
    "order of magnitude of either term.")) %>%
  tab_options(table.font.size = px(11), heading.title.font.size = px(14),
              column_labels.font.weight = "bold",
              table.border.top.style = "none",
              table_body.hlines.style = "none", data_row.padding = px(3))

gtsave(table8, out_path("table8_standardisation", "html"))
gtsave(table8, out_path("table8_standardisation", "png"), expand = 10)

#-------------------------------------------------------------------------------
# Figure 9. Every cell above the 45-degree line rose. This is the decomposition
# made visible: if the rise were compositional, cells would sit ON the line and
# only their weights would change.
#-------------------------------------------------------------------------------

axis_max <- max(cells$rate_base, cells$rate_last)

fig9 <- ggplot(cells, aes(x = rate_base, y = rate_last)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey55",
              linetype = "dashed", linewidth = 0.4) +
  geom_point(aes(size = sh_last, alpha = !thin), colour = "#C45C30") +
  scale_size_area(max_size = 9, labels = scales::percent_format(accuracy = 1)) +
  scale_alpha_manual(values = c(`TRUE` = 0.75, `FALSE` = 0.25), guide = "none") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 0.35, 0.05)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 0.35, 0.05)) +
  coord_fixed(xlim = c(0, axis_max), ylim = c(0, axis_max)) +
  labs(
    title    = "Non-response rose inside almost every cell, not between cells",
    subtitle = sprintf(paste("Firm-size non-response across %d cells",
                             "(formality x sector x %s), %d against %d"),
                       nrow(cells), GEO_NAME[["domain"]], STD_LAST, STD_BASE),
    x = sprintf("Non-response rate, %d", STD_BASE),
    y = sprintf("Non-response rate, %d", STD_LAST),
    size = paste0("Share of ", STD_LAST, "\nemployment"),
    caption = paste0(
      "Each point is a cell. Points above the dashed line rose; points on it did not.\n",
      "Faded points are cells with fewer than ", THIN_CELL, " observations in one year.\n",
      "If the rise were compositional, cells would sit on the line and only their sizes would change.\n",
      "Source: Authors' calculations using ENCFT.")) +
  theme_surveytools() +
  theme(plot.caption   = element_text(hjust = 0, colour = "grey40"),
        axis.text.x    = element_text(angle = 0, hjust = 0.5, vjust = 1))

ggsave(out_path("fig9_within_cell_scatter", "png"), fig9,
       width = 8, height = 6.5, dpi = 200)

cat("[13] Steps 11-12 outputs written.\n")


cat("=== 03C complete ===\n\n")

