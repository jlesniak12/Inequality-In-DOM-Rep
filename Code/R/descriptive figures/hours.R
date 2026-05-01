#===============================================================================
#
# Script: 05c_Hours_and_Compliance_Context.R
#
# Purpose: Two things:
#   1. Fix the compliance figures from 05b: exclude Government sector,
#      which has its own MW schedule and inflates measured non-compliance
#      when compared against the private-sector tier MWs.
#
#   2. Hours worked analysis — motivated by the finding that hourly NC > monthly
#      NC, which implies overtime workers outnumber part-timers among the
#      non-compliant population. These figures explain WHY the three compliance
#      measures diverge and provide essential context for interpreting them.
#
# HOURS FIGURES
#
#   H1  Distribution of hours worked — snapshot 2016Q2 vs 2025Q2
#       Separate panels: formal vs informal, by firm size tier.
#       Key reference lines: 44hrs (standard week), 68hrs (OT tier boundary).
#       This directly shows whether the DR workforce skews toward long hours
#       (explaining hourly > monthly NC) or short hours (where we'd expect
#       hourly < monthly NC).
#
#   H2  Share of workers by hours band over time — national level
#       Bands: <20hrs (very PT), 20-43hrs (PT), 44hrs (standard), 45-68hrs
#       (OT tier 1), >68hrs (OT tier 2). Stacked area or overlaid lines.
#       Shows whether the hours distribution has shifted over time.
#
#   H3  Mean and median hours worked over time — by firm size tier
#       Formal workers only. Shows whether the MW events are associated
#       with hours changes (a classic margin of adjustment).
#
#   H4  Mean and median hours worked over time — by economic sector
#       Formal workers only, faceted. Reveals sector heterogeneity.
#
#   H5  Share working overtime (>44hrs) over time — formal vs informal
#       National level plus by sector. The overtime share is the direct
#       driver of the monthly/hourly NC divergence.
#
#   H6  Compliance rate by hours band — snapshot
#       Bar chart: for each hours band, what share are non-compliant on
#       monthly vs hourly measure? This directly shows:
#         - Part-timers: monthly NC >> hourly NC (part-time bias confirmed)
#         - Overtime workers: hourly NC >> monthly NC (hours extraction)
#         - Standard workers: measures should roughly agree
#       This is the most direct evidence on what is driving the divergence.
#
# ADDITIONAL SUGGESTION — H7
#   Hours × income scatter with MW reference lines.
#   Plot each worker as a point (hours, log income) with the MW compliance
#   boundary drawn as a line: income = MW_hourly × hours × weeks_per_month.
#   Workers below the line are hourly non-compliant; above the line are
#   compliant. Colour by monthly compliance status. This makes it immediately
#   visible that many overtime workers sit above the monthly MW floor
#   (monthly compliant) but below the hourly compliance line — they are the
#   key population driving the divergence.
#
# COMPLIANCE FIX
#   figE1_compliance_econ_fixed      — Government excluded
#   figE2_compliance_tier_fixed      — Government excluded
#   figE3_compliance_sector_fixed    — Government excluded (also removes it
#                                      from the sector panels since it's no
#                                      longer comparable)
#
#===============================================================================

source("Code/R/setup/00_setup.R")
source("Code/R/import and prep data/02_Sample Definitions.R")
library(patchwork)
library(scales)


#===============================================================================
# STEP 0.  Output paths and helpers
#===============================================================================

save_path <- file.path(
  config$paths$outputs,
  config$output_stage,
  config$out_subdirs$inequality_minwage,
  "Bindingness Exploratory"
)
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)

save_fig <- function(fig, name,
                     w = config$fig_defaults$width,
                     h = config$fig_defaults$height) {
  ggsave(
    file.path(save_path, paste0(name, ".", config$fig_defaults$format)),
    plot = fig, width = w, height = h, dpi = config$fig_defaults$dpi
  )
  message("Saved: ", name)
}

MW_EVENT_QTR <- c("2017Q2", "2019Q3", "2021Q3", "2023Q2")
SRC          <- "Sources: ENCFT 2014Q3\u20132025Q2; Central Bank of Dominican Republic."
TIER_LEVELS  <- c("Micro", "Small", "Medium", "Large")
TIER_COLORS  <- c(Micro="#2C8C3C", Small="#5B3A8E", Medium="#E07B00", Large="#1B7BB4")
SNAP_QTRS    <- c("2016Q2", "2025Q2")
WEEKS_PM     <- 52 / 12

covid_rect <- function(qtrs) {
  xmin <- which(qtrs == "2020Q1"); xmax <- which(qtrs == "2020Q4")
  if (!length(xmin) || !length(xmax)) return(NULL)
  annotate("rect", xmin=xmin-0.5, xmax=xmax+0.5, ymin=-Inf, ymax=Inf,
           fill="grey85", alpha=0.6)
}
event_pos <- function(qtrs) which(qtrs %in% MW_EVENT_QTR)

# Sector order by employment size
sector_order <- samples$private_employees_inc$data %>%
  filter(year_quarter == "2016Q2") %>%
  count(Employment_Sector, wt = FACTOR_EXPANSION, sort = TRUE) %>%
  pull(Employment_Sector)


#===============================================================================
# STEP 1.  Build analysis data frames
#
#  For compliance fix: exclude Government (different MW schedule).
#  For hours: use private_employees (broader — hours analysis not restricted
#  to positive income workers) but also show formal vs informal split.
#===============================================================================

cat("[1] Building data frames...\n")

# --- Compliance (fixed): private employees excl. Government, positive income ---
comp_base <- samples$private_employees_inc$data %>%
  filter(Employment_Sector != "Government") %>%
  mutate(Wage_group = factor(Wage_group, levels = TIER_LEVELS))

comp_formal <- comp_base %>% filter(Employment_Status == "Formal")

# --- Hours: all private employees with non-missing, plausible hours ---
# Cap at 98hrs (survey maximum) and floor at 1hr to remove data errors
hours_base <- samples$private_employees$data %>%
  filter(
    !is.na(hours_worked_primary),
    hours_worked_primary >= 1,
    hours_worked_primary <= 98
  ) %>%
  mutate(
    Wage_group        = factor(Wage_group, levels = TIER_LEVELS),
    Employment_Sector = factor(Employment_Sector, levels = sector_order),
    # Hours bands
    hours_band = cut(
      hours_worked_primary,
      breaks = c(0, 19, 43, 44, 68, Inf),
      labels = c("<20 hrs\n(very PT)", "20-43 hrs\n(part-time)",
                 "44 hrs\n(standard)", "45-68 hrs\n(OT tier 1)",
                 ">68 hrs\n(OT tier 2)"),
      right  = TRUE, include.lowest = TRUE
    ),
    is_overtime = hours_worked_primary > 44,
    snapshot    = dplyr::if_else(year_quarter %in% SNAP_QTRS, year_quarter, NA_character_)
  )

hours_formal   <- hours_base %>% filter(Employment_Status == "Formal")
hours_informal <- hours_base %>% filter(Employment_Status == "Informal")

cat("    Compliance base (excl. Govt):", nrow(comp_base), "\n")
cat("    Hours base:                  ", nrow(hours_base), "\n")


#===============================================================================
# STEP 2.  COMPLIANCE FIX — Recompute with Government excluded
#===============================================================================

cat("[2] Recomputing compliance rates (Government excluded)...\n")

COMPLIANCE_VARS <- tibble::tribble(
  ~concept,      ~col,                   ~label,
  "monthly",     "below_min_monthly",    "Monthly (Measure 1)",
  "hourly",      "below_min_hourly",     "Hourly (Measure 2 \u2014 Primary)",
  "hourly_eff",  "below_min_hourly_eff", "Hourly OT-adjusted (Measure 3)"
)

CONCEPT_ORDER <- c("Monthly (Measure 1)", "Hourly (Measure 2 \u2014 Primary)",
                   "Hourly OT-adjusted (Measure 3)")
CONCEPT_COLORS <- c("monthly"="#1B7BB4", "hourly"="#2C8C3C", "hourly_eff"="#C45C30")
CONCEPT_LTYPES <- c("monthly"="solid",   "hourly"="dashed",  "hourly_eff"="dotted")
CONCEPT_SHORT  <- c("monthly"="Monthly", "hourly"="Hourly",  "hourly_eff"="Hourly (OT-adj.)")

compute_nc <- function(df, group_vars) {
  purrr::map_dfr(seq_len(nrow(COMPLIANCE_VARS)), function(i) {
    cv <- COMPLIANCE_VARS[i, ]
    df %>%
      filter(!is.na(.data[[cv$col]])) %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        nc_rate = weighted.mean(.data[[cv$col]], FACTOR_EXPANSION, na.rm=TRUE),
        n_obs   = n(),
        .groups = "drop"
      ) %>%
      mutate(concept=cv$concept, concept_label=cv$label)
  })
}

scope_list <- list(
  list(data=comp_formal, scope="formal",      label="Formal only"),
  list(data=comp_base,   scope="all_private", label="Formal + Informal")
)

# Economy-wide (excl. Govt)
comp_econ_fix <- purrr::map_dfr(scope_list, function(s) {
  compute_nc(s$data, "year_quarter") %>%
    mutate(scope=s$scope, scope_label=s$label)
})

# By tier (known Wage_group, excl. Govt)
comp_tier_fix <- purrr::map_dfr(scope_list, function(s) {
  s$data %>%
    filter(Wage_group %in% TIER_LEVELS) %>%
    { compute_nc(., c("year_quarter","Wage_group")) } %>%
    mutate(scope=s$scope, scope_label=s$label,
           sparse = n_obs < 30)
})

# By sector (formal only, excl. Govt already)
comp_sec_fix <- compute_nc(comp_formal %>%
                             filter(Wage_group %in% TIER_LEVELS),
                           c("year_quarter","Employment_Sector")) %>%
  mutate(scope="formal", scope_label="Formal only", sparse = n_obs < 30)

cat("    Done. Recomputed for", n_distinct(comp_econ_fix$year_quarter), "quarters.\n")


#===============================================================================
# STEP 3.  FIXED COMPLIANCE FIGURES
#===============================================================================

cat("[3] Building fixed compliance figures...\n")

COMP_CAPTION <- paste(
  "Non-compliance = share earning below tier-specific MW floor.",
  "Government excluded (different MW schedule). Private-sector employees only.",
  SRC, sep="\n"
)

# ── E1 fixed: Economy-wide ───────────────────────────────────────────────────
e1 <- comp_econ_fix %>%
  mutate(concept_label = factor(concept_label, levels=CONCEPT_ORDER),
         scope_label   = factor(scope_label, levels=c("Formal only","Formal + Informal")))

qtrs_e1 <- sort(unique(e1$year_quarter))

fig_E1_fix <- ggplot(e1, aes(x=year_quarter, y=nc_rate,
                             colour=scope_label, linetype=scope_label,
                             group=interaction(concept,scope_label))) +
  covid_rect(qtrs_e1) +
  geom_vline(xintercept=event_pos(qtrs_e1), linetype="dashed",
             colour="red", linewidth=0.4) +
  geom_line(linewidth=0.7) +
  facet_wrap(~concept_label, ncol=1, scales="free_y") +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  scale_colour_manual(values=c("Formal only"="#2C5F8A","Formal + Informal"="#C45C30"),
                      name="Worker scope") +
  scale_linetype_manual(values=c("Formal only"="solid","Formal + Informal"="dashed"),
                        name="Worker scope") +
  scale_x_discrete(breaks=qtrs_e1[seq(1,length(qtrs_e1),by=4)]) +
  labs(title    = "Non-Compliance Rate Over Time \u2014 Economy-Wide (Govt Excluded)",
       subtitle = "Share earning below tier-specific MW floor. All three measures.",
       y="Non-compliance rate", x=NULL,
       caption=COMP_CAPTION) +
  theme_surveytools(legend_position="bottom") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=7),
        strip.text=element_text(face="bold",size=9),
        strip.background=element_rect(fill="grey93",colour="grey70"),
        panel.spacing=unit(0.5,"lines"))

save_fig(fig_E1_fix, "figE1_compliance_econ_govtexcl",
         w=config$fig_defaults$width*1.7, h=config$fig_defaults$height*3)

# ── E2 fixed: By tier ────────────────────────────────────────────────────────
e2 <- comp_tier_fix %>%
  filter(scope=="formal", !sparse) %>%
  mutate(Wage_group=factor(Wage_group,levels=TIER_LEVELS))

qtrs_e2 <- sort(unique(e2$year_quarter))

fig_E2_fix <- ggplot(e2, aes(x=year_quarter, y=nc_rate,
                             colour=concept, linetype=concept, group=concept)) +
  covid_rect(qtrs_e2) +
  geom_vline(xintercept=event_pos(qtrs_e2), linetype="dashed",
             colour="red", linewidth=0.35) +
  geom_line(linewidth=0.7) +
  facet_wrap(~Wage_group, ncol=2, scales="free_y") +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  scale_colour_manual(values=CONCEPT_COLORS, labels=CONCEPT_SHORT, name="Measure") +
  scale_linetype_manual(values=CONCEPT_LTYPES, labels=CONCEPT_SHORT, name="Measure") +
  scale_x_discrete(breaks=qtrs_e2[seq(1,length(qtrs_e2),by=4)]) +
  labs(title    = "Non-Compliance by Firm Size Tier \u2014 Formal Workers (Govt Excluded)",
       subtitle = "Three income measures. Free y-axis.",
       y="Non-compliance rate", x=NULL, caption=COMP_CAPTION) +
  theme_surveytools(legend_position="bottom") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=6),
        strip.text=element_text(face="bold",size=10),
        strip.background=element_rect(fill="grey93",colour="grey70"))

save_fig(fig_E2_fix, "figE2_compliance_tier_govtexcl",
         w=config$fig_defaults$width*2, h=config$fig_defaults$height*1.8)

cat("    Fixed compliance figures saved.\n")


#===============================================================================
# STEP 4.  HOURS FIGURES
#===============================================================================

cat("[4] Building hours figures...\n")

# ── H1: Distribution of hours — snapshot 2016Q2 vs 2025Q2 ───────────────────
cat("    H1: Hours distribution snapshot...\n")

h1_data <- hours_base %>%
  filter(year_quarter %in% SNAP_QTRS, !is.na(Employment_Status)) %>%
  mutate(snap_label = dplyr::recode(year_quarter,
                                    "2016Q2"="2016 Q2 (baseline)",
                                    "2025Q2"="2025 Q2 (recent)"),
         snap_label = factor(snap_label, levels=c("2016 Q2 (baseline)","2025 Q2 (recent)")))

# Normalise weights within snapshot × formality status
h1_data <- h1_data %>%
  group_by(snap_label, Employment_Status) %>%
  mutate(w_norm = FACTOR_EXPANSION / sum(FACTOR_EXPANSION, na.rm=TRUE)) %>%
  ungroup()

fig_H1 <- ggplot(h1_data,
                 aes(x=hours_worked_primary, weight=w_norm,
                     fill=Employment_Status, colour=Employment_Status)) +
  geom_histogram(binwidth=4, alpha=0.5, position="identity") +
  geom_vline(xintercept=44, linetype="dashed", colour="grey30", linewidth=0.6) +
  geom_vline(xintercept=68, linetype="dotted", colour="grey50", linewidth=0.5) +
  annotate("text", x=45, y=Inf, label="44 hrs\n(standard)", hjust=0, vjust=1.3,
           size=2.5, colour="grey30") +
  annotate("text", x=69, y=Inf, label="68 hrs\n(OT tier 2)", hjust=0, vjust=1.3,
           size=2.5, colour="grey50") +
  facet_wrap(~snap_label) +
  scale_fill_manual(values=c("Formal"="#2C5F8A","Informal"="#C45C30"), name=NULL) +
  scale_colour_manual(values=c("Formal"="#2C5F8A","Informal"="#C45C30"), name=NULL) +
  scale_x_continuous(breaks=c(0,10,20,30,44,55,68,80), limits=c(0,90)) +
  labs(title    = "Distribution of Weekly Hours Worked \u2014 Formal vs Informal",
       subtitle = "Private-sector employees. Snapshot: 2016Q2 vs 2025Q2.",
       x        = "Weekly hours worked (primary job)",
       y        = "Proportion of workers",
       caption  = paste(
         "Bin width = 4 hours. Vertical dashed line = 44hrs (legal standard week).",
         "Dotted line = 68hrs (boundary between OT tier 1 and tier 2).",
         "Weights normalised within snapshot \u00d7 formality status.",
         "Workers reporting <1 or >98 hours excluded.",
         SRC, sep="\n")) +
  theme_surveytools(legend_position="bottom") +
  theme(strip.text=element_text(face="bold",size=11),
        strip.background=element_rect(fill="grey93",colour="grey70"))

save_fig(fig_H1, "figH1_hours_distribution_snapshot",
         w=config$fig_defaults$width*2, h=config$fig_defaults$height*1.4)


# ── H2: Share of workers by hours band over time ────────────────────────────
cat("    H2: Hours band shares over time...\n")

h2_data <- hours_base %>%
  filter(!is.na(hours_band)) %>%
  group_by(year_quarter, hours_band) %>%
  summarise(wtd_n=sum(FACTOR_EXPANSION, na.rm=TRUE), .groups="drop") %>%
  group_by(year_quarter) %>%
  mutate(share = wtd_n / sum(wtd_n)) %>%
  ungroup()

qtrs_h2 <- sort(unique(h2_data$year_quarter))

BAND_COLORS <- c(
  "<20 hrs\n(very PT)"    = "#d73027",
  "20-43 hrs\n(part-time)"= "#fc8d59",
  "44 hrs\n(standard)"    = "#2C8C3C",
  "45-68 hrs\n(OT tier 1)"= "#4575b4",
  ">68 hrs\n(OT tier 2)"  = "#313695"
)

fig_H2 <- ggplot(h2_data,
                 aes(x=year_quarter, y=share, fill=hours_band, group=hours_band)) +
  covid_rect(qtrs_h2) +
  geom_vline(xintercept=event_pos(qtrs_h2), linetype="dashed",
             colour="red", linewidth=0.4) +
  geom_area(alpha=0.85, position="stack") +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  scale_fill_manual(values=BAND_COLORS, name="Hours band") +
  scale_x_discrete(breaks=qtrs_h2[seq(1,length(qtrs_h2),by=4)]) +
  labs(title    = "Share of Private-Sector Employees by Weekly Hours Band",
       subtitle = "Formal + informal workers. Shows whether overtime prevalence has changed over time.",
       x=NULL, y="Share of workers",
       caption  = paste(
         "Hours bands: <20 (very part-time), 20-43 (part-time), 44 (standard),",
         "45-68 (overtime tier 1), >68 (overtime tier 2, double time).",
         "Red dashed: MW events. Grey: COVID-19.",
         SRC, sep="\n")) +
  theme_surveytools(legend_position="bottom") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=7))

save_fig(fig_H2, "figH2_hours_bands_over_time",
         w=config$fig_defaults$width*1.7, h=config$fig_defaults$height*1.2)


# ── H3: Mean + median hours over time — by firm size tier ────────────────────
cat("    H3: Mean/median hours by tier...\n")

wt_quantile <- function(x, w, prob) {
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cumw <- cumsum(w) / sum(w)
  x[which.min(abs(cumw - prob))]
}

h3_data <- hours_formal %>%
  filter(Wage_group %in% TIER_LEVELS) %>%
  group_by(year_quarter, Wage_group) %>%
  summarise(
    mean_hrs   = weighted.mean(hours_worked_primary, FACTOR_EXPANSION, na.rm=TRUE),
    median_hrs = wt_quantile(hours_worked_primary, FACTOR_EXPANSION, 0.5),
    pct_ot     = weighted.mean(is_overtime, FACTOR_EXPANSION, na.rm=TRUE),
    n_obs      = n(),
    .groups    = "drop"
  ) %>%
  mutate(Wage_group = factor(Wage_group, levels=TIER_LEVELS))

qtrs_h3 <- sort(unique(h3_data$year_quarter))

fig_H3a <- ggplot(h3_data,
                  aes(x=year_quarter, y=mean_hrs,
                      colour=Wage_group, group=Wage_group)) +
  covid_rect(qtrs_h3) +
  geom_vline(xintercept=event_pos(qtrs_h3), linetype="dashed",
             colour="red", linewidth=0.35) +
  geom_hline(yintercept=44, linetype="dotted", colour="grey40", linewidth=0.4) +
  geom_line(linewidth=0.7) +
  scale_colour_manual(values=TIER_COLORS, name="Firm size tier") +
  scale_x_discrete(breaks=qtrs_h3[seq(1,length(qtrs_h3),by=4)]) +
  labs(title="Mean Weekly Hours \u2014 Formal Workers by Firm Size",
       subtitle="Dotted line = 44hrs standard week. Values above indicate overtime is common.",
       y="Mean weekly hours", x=NULL) +
  theme_surveytools() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=7))

fig_H3b <- ggplot(h3_data,
                  aes(x=year_quarter, y=pct_ot,
                      colour=Wage_group, group=Wage_group)) +
  covid_rect(qtrs_h3) +
  geom_vline(xintercept=event_pos(qtrs_h3), linetype="dashed",
             colour="red", linewidth=0.35) +
  geom_line(linewidth=0.7) +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  scale_colour_manual(values=TIER_COLORS, name="Firm size tier") +
  scale_x_discrete(breaks=qtrs_h3[seq(1,length(qtrs_h3),by=4)]) +
  labs(title="Share Working Overtime (>44hrs) \u2014 Formal Workers by Firm Size",
       subtitle="High overtime share = main driver of hourly NC exceeding monthly NC.",
       y="Share working >44 hrs/week", x=NULL,
       caption=paste("Formal private-sector employees with known firm size.",
                     "Red dashed: MW events. Grey: COVID-19.", SRC, sep="\n")) +
  theme_surveytools() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=7))

fig_H3 <- fig_H3a / fig_H3b +
  plot_annotation(
    title    = "Hours Worked by Firm Size Tier \u2014 Formal Private-Sector Employees",
    subtitle = "Top: mean hours. Bottom: share working overtime (>44hrs).",
    theme    = theme(plot.title=element_text(size=13,face="bold"),
                     plot.subtitle=element_text(size=9,colour="grey30"))
  )

save_fig(fig_H3, "figH3_hours_by_tier",
         w=config$fig_defaults$width*1.7, h=config$fig_defaults$height*2.2)


# ── H4: Mean hours over time — by sector ────────────────────────────────────
cat("    H4: Mean hours by sector...\n")

h4_data <- hours_formal %>%
  filter(Employment_Sector != "Government") %>%
  group_by(year_quarter, Employment_Sector) %>%
  summarise(
    mean_hrs = weighted.mean(hours_worked_primary, FACTOR_EXPANSION, na.rm=TRUE),
    pct_ot   = weighted.mean(is_overtime, FACTOR_EXPANSION, na.rm=TRUE),
    n_obs    = n(),
    .groups  = "drop"
  ) %>%
  mutate(Employment_Sector = factor(Employment_Sector, levels=sector_order)) %>%
  filter(!is.na(Employment_Sector))

qtrs_h4 <- sort(unique(h4_data$year_quarter))

fig_H4 <- ggplot(h4_data,
                 aes(x=year_quarter, y=mean_hrs, group=Employment_Sector)) +
  covid_rect(qtrs_h4) +
  geom_vline(xintercept=event_pos(qtrs_h4), linetype="dashed",
             colour="red", linewidth=0.3) +
  geom_hline(yintercept=44, linetype="dotted", colour="grey40", linewidth=0.4) +
  geom_line(colour="#2C5F8A", linewidth=0.6) +
  facet_wrap(~Employment_Sector, ncol=2, scales="free_y") +
  scale_x_discrete(breaks=qtrs_h4[seq(1,length(qtrs_h4),by=8)]) +
  labs(title    = "Mean Weekly Hours by Economic Sector \u2014 Formal Workers",
       subtitle = "Dotted = 44hr standard week. Free y-axis reveals sector heterogeneity.",
       y="Mean weekly hours", x=NULL,
       caption  = paste(
         "Formal private-sector employees. Government excluded.",
         "Sectors with hours far above 44hrs drive hourly NC > monthly NC.",
         "Red dashed: MW events. Grey: COVID-19.", SRC, sep="\n")) +
  theme_surveytools() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=6),
        strip.text=element_text(face="bold",size=8),
        strip.background=element_rect(fill="grey93",colour="grey70"),
        panel.spacing=unit(0.4,"lines"))

save_fig(fig_H4, "figH4_hours_by_sector",
         w=config$fig_defaults$width*2,
         h=config$fig_defaults$height*ceiling(n_distinct(h4_data$Employment_Sector)/2)*0.85)


# ── H5: Overtime share by formality — national + by sector ──────────────────
cat("    H5: Overtime share formal vs informal...\n")

h5_econ <- hours_base %>%
  filter(!is.na(Employment_Status)) %>%
  group_by(year_quarter, Employment_Status) %>%
  summarise(pct_ot=weighted.mean(is_overtime, FACTOR_EXPANSION, na.rm=TRUE),
            .groups="drop")

qtrs_h5 <- sort(unique(h5_econ$year_quarter))

fig_H5 <- ggplot(h5_econ,
                 aes(x=year_quarter, y=pct_ot,
                     colour=Employment_Status, group=Employment_Status)) +
  covid_rect(qtrs_h5) +
  geom_vline(xintercept=event_pos(qtrs_h5), linetype="dashed",
             colour="red", linewidth=0.4) +
  geom_line(linewidth=0.8) +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  scale_colour_manual(values=c("Formal"="#2C5F8A","Informal"="#C45C30"),
                      name="Employment status") +
  scale_x_discrete(breaks=qtrs_h5[seq(1,length(qtrs_h5),by=4)]) +
  labs(title    = "Share of Workers Doing Overtime (>44 hrs/week)",
       subtitle = "Formal vs informal private-sector employees. National level.",
       y="Share working >44 hrs/week", x=NULL,
       caption  = paste(
         "Key context for compliance figures: a high overtime share means many workers",
         "earn the monthly MW through long hours, appearing hourly non-compliant.",
         "This explains why hourly NC > monthly NC when overtime workers are prevalent.",
         "Red dashed: MW events. Grey: COVID-19.", SRC, sep="\n")) +
  theme_surveytools() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=7))

save_fig(fig_H5, "figH5_overtime_share_formal_informal",
         w=config$fig_defaults$width*1.7, h=config$fig_defaults$height)


# ── H6: Compliance by hours band — snapshot ─────────────────────────────────
# This is the key diagnostic: shows directly how the three measures diverge
# by hours band, confirming whether part-time or overtime is the dominant effect.
cat("    H6: Compliance by hours band...\n")

h6_data <- comp_formal %>%
  filter(
    year_quarter %in% SNAP_QTRS,
    !is.na(hours_worked_primary),
    hours_worked_primary >= 1,
    hours_worked_primary <= 98,
    !is.na(below_min_monthly),
    !is.na(below_min_hourly)
  ) %>%
  mutate(
    hours_band = cut(
      hours_worked_primary,
      breaks = c(0, 19, 43, 44, 68, Inf),
      labels = c("<20 hrs\n(very PT)", "20-43 hrs\n(part-time)",
                 "44 hrs\n(standard)", "45-68 hrs\n(OT tier 1)",
                 ">68 hrs\n(OT tier 2)"),
      right=TRUE, include.lowest=TRUE
    ),
    snap_label = dplyr::recode(year_quarter,
                               "2016Q2"="2016 Q2 (baseline)",
                               "2025Q2"="2025 Q2 (recent)"),
    snap_label = factor(snap_label, levels=c("2016 Q2 (baseline)","2025 Q2 (recent)"))
  ) %>%
  filter(!is.na(hours_band))

h6_rates <- h6_data %>%
  group_by(snap_label, hours_band) %>%
  summarise(
    nc_monthly   = weighted.mean(below_min_monthly,   FACTOR_EXPANSION, na.rm=TRUE),
    nc_hourly    = weighted.mean(below_min_hourly,     FACTOR_EXPANSION, na.rm=TRUE),
    nc_hourly_ot = weighted.mean(below_min_hourly_eff, FACTOR_EXPANSION, na.rm=TRUE),
    n_obs        = n(),
    .groups      = "drop"
  ) %>%
  tidyr::pivot_longer(cols=c(nc_monthly,nc_hourly,nc_hourly_ot),
                      names_to="measure", values_to="nc_rate") %>%
  mutate(
    measure = dplyr::recode(measure,
                            nc_monthly   = "Monthly",
                            nc_hourly    = "Hourly",
                            nc_hourly_ot = "Hourly (OT-adj.)"),
    measure = factor(measure, levels=c("Monthly","Hourly","Hourly (OT-adj.)"))
  )

fig_H6 <- ggplot(h6_rates,
                 aes(x=hours_band, y=nc_rate, fill=measure, group=measure)) +
  geom_col(position="dodge", alpha=0.85) +
  facet_wrap(~snap_label) +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  scale_fill_manual(values=c("Monthly"="#1B7BB4","Hourly"="#2C8C3C",
                             "Hourly (OT-adj.)"="#C45C30"),
                    name="Compliance measure") +
  labs(
    title    = "Non-Compliance Rate by Hours Band \u2014 Formal Workers",
    subtitle = paste0(
      "Key diagnostic: shows which workers drive the divergence between measures.\n",
      "Part-timers (<44hrs): monthly NC should exceed hourly NC.\n",
      "Overtime workers (>44hrs): hourly NC should exceed monthly NC."
    ),
    x = "Hours band",
    y = "Non-compliance rate",
    caption = paste(
      "Formal private-sector employees. Government excluded.",
      "If hourly > monthly for >44hrs bands: workers earn monthly MW through long hours (hours extraction).",
      "If monthly > hourly for <44hrs bands: part-timers correctly reclassified as compliant.",
      SRC, sep="\n"
    )
  ) +
  theme_surveytools(legend_position="bottom") +
  theme(strip.text=element_text(face="bold",size=11),
        strip.background=element_rect(fill="grey93",colour="grey70"),
        axis.text.x=element_text(size=8))

save_fig(fig_H6, "figH6_compliance_by_hours_band",
         w=config$fig_defaults$width*2, h=config$fig_defaults$height*1.5)


# ── H7: Hourly earnings vs hours worked — with MW floor line ─────────────────
#
# DESIGN
#   Y-axis: worker's effective hourly earnings normalised to own-tier MW hourly
#           rate (y = 1 means earning exactly the hourly MW).
#           Normalising removes tier differences — all four tiers share the
#           same horizontal compliance line at y = 1.
#   X-axis: weekly hours worked.
#   Horizontal line at y = 1: the hourly MW floor.
#           Points ABOVE = hourly compliant.
#           Points BELOW = hourly non-compliant.
#   Colour: monthly compliance status (income >= monthly MW).
#
# TWO PANELS: 2016Q2 vs 2025Q2 — colour shift shows compliance change over time.

cat("    H7: Hourly earnings vs hours (normalised, MW floor line)...\n")

WEEKS_PM <- 52 / 12

set.seed(42)
h7_data <- comp_formal %>%
  filter(
    year_quarter %in% SNAP_QTRS,
    !is.na(hours_worked_primary),
    hours_worked_primary >= 1,
    hours_worked_primary <= 90,
    !is.na(real_wage_compliance_primary),
    real_wage_compliance_primary > 0,
    !is.na(real_minwage_harmonized),
    real_minwage_harmonized > 0,
    Wage_group %in% TIER_LEVELS
  ) %>%
  mutate(
    snap_label = factor(
      dplyr::recode(year_quarter,
                    "2016Q2" = "2016 Q2 (baseline)",
                    "2025Q2" = "2025 Q2 (recent)"),
      levels = c("2016 Q2 (baseline)", "2025 Q2 (recent)")
    ),
    worker_hourly  = real_wage_compliance_primary / (WEEKS_PM * hours_worked_primary),
    mw_hourly_tier = real_minwage_harmonized / (23.83 * 8),
    hourly_ratio   = worker_hourly / mw_hourly_tier,
    monthly_status = dplyr::if_else(
      real_wage_compliance_primary >= real_minwage_harmonized,
      "Monthly compliant", "Monthly non-compliant"
    )
  ) %>%
  filter(hourly_ratio <= 4, hourly_ratio > 0) %>%
  group_by(snap_label) %>%
  slice_sample(n = 2500, weight_by = FACTOR_EXPANSION) %>%
  ungroup()

cap_pct <- comp_formal %>%
  filter(year_quarter %in% SNAP_QTRS,
         !is.na(real_wage_compliance_primary), !is.na(real_minwage_harmonized),
         real_minwage_harmonized > 0, !is.na(hours_worked_primary),
         hours_worked_primary > 0, Wage_group %in% TIER_LEVELS) %>%
  mutate(
    worker_hourly  = real_wage_compliance_primary / (WEEKS_PM * hours_worked_primary),
    mw_hourly_tier = real_minwage_harmonized / (23.83 * 8),
    hourly_ratio   = worker_hourly / mw_hourly_tier
  ) %>%
  group_by(year_quarter) %>%
  summarise(pct_above = weighted.mean(hourly_ratio > 4, FACTOR_EXPANSION, na.rm=TRUE) * 100,
            .groups = "drop") %>%
  summarise(pct = mean(pct_above)) %>% pull(pct) %>% round(1)

fig_H7 <- ggplot(h7_data,
                 aes(x = hours_worked_primary, y = hourly_ratio,
                     colour = monthly_status)) +
  annotate("rect", xmin = 0, xmax = 90, ymin = 0, ymax = 1,
           fill = "#C45C30", alpha = 0.07) +
  geom_point(alpha = 0.18, size = 0.55) +
  geom_hline(yintercept = 1, colour = "black", linewidth = 0.9, linetype = "dashed") +
  geom_vline(xintercept = 44, colour = "grey50", linewidth = 0.45, linetype = "dotted") +
  annotate("text", x = 2, y = 3.75, label = "Hourly compliant (above line)",
           hjust = 0, size = 2.5, colour = "grey30", fontface = "italic") +
  annotate("text", x = 2, y = 0.12, label = "Hourly non-compliant (below line)",
           hjust = 0, size = 2.5, colour = "#C45C30", fontface = "italic") +
  annotate("text", x = 45, y = 3.85, label = "44 hrs\n(standard week)",
           hjust = 0, size = 2.3, colour = "grey50") +
  facet_wrap(~snap_label) +
  scale_colour_manual(
    values = c("Monthly compliant" = "#2C5F8A", "Monthly non-compliant" = "#C45C30"),
    name = "Monthly compliance"
  ) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 44, 55, 68, 80), limits = c(0, 90)) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1, 1.5, 2, 3, 4),
    labels = c("0", "0.5\u00d7", "1\u00d7 MW\n(floor)", "1.5\u00d7", "2\u00d7", "3\u00d7", "4\u00d7"),
    limits = c(0, 4)
  ) +
  labs(
    title    = "Hourly Earnings vs Hours Worked \u2014 Formal Workers",
    subtitle = paste0(
      "Y = hourly earnings as multiple of own-tier hourly MW floor (y=1: exactly at the floor).\n",
      "Dashed line = hourly MW floor. Above = hourly compliant. Below = hourly non-compliant.\n",
      "Colour = monthly compliance status. Blue below line = monthly MW earned only through excessive hours."
    ),
    x = "Weekly hours worked (primary job)",
    y = "Hourly earnings relative to tier hourly MW floor",
    caption = paste(
      "Formal private-sector employees with known firm size. Government excluded.",
      "Hourly earnings = monthly compliance income / (weeks per month \u00d7 weekly hours).",
      "Y-axis normalised to own-tier hourly MW so all four tiers are on the same scale.",
      paste0(cap_pct, "% of workers earn >4\u00d7 hourly MW (all compliant, excluded from plot)."),
      "Weighted random sample of 2,500 workers per panel. 2016Q2 vs 2025Q2.",
      SRC, sep = "\n"
    )
  ) +
  theme_surveytools(legend_position = "bottom") +
  theme(strip.text = element_text(face = "bold", size = 11),
        strip.background = element_rect(fill = "grey93", colour = "grey70"),
        panel.grid.minor = element_blank())

save_fig(fig_H7, "figH7_hours_income_scatter",
         w = config$fig_defaults$width * 2.2,
         h = config$fig_defaults$height * 1.8)


#===============================================================================
# DONE
#===============================================================================

cat("\n=== 05c complete ===\n")
cat("Saved to:", save_path, "\n\n")
cat("COMPLIANCE (Government excluded):\n")
cat("  figE1_compliance_econ_govtexcl\n")
cat("  figE2_compliance_tier_govtexcl\n\n")
cat("HOURS:\n")
cat("  figH1_hours_distribution_snapshot    — distribution 2016Q2 vs 2025Q2\n")
cat("  figH2_hours_bands_over_time          — shares by hours band over time\n")
cat("  figH3_hours_by_tier                  — mean hours + OT share by tier\n")
cat("  figH4_hours_by_sector                — mean hours by sector\n")
cat("  figH5_overtime_share_formal_informal — OT share formal vs informal\n")
cat("  figH6_compliance_by_hours_band       — KEY: compliance rate by hours band\n")
cat("  figH7_hours_income_scatter           — hours x income with MW boundary\n")