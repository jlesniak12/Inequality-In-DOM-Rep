


source(here::here("Code", "R", "clean scripts", "00_setup.R"))
source(here::here("Code", "R", "clean scripts", "03_sample_definitions.R"))




# ====== Diagnosing Trends in Firm size Reporting ===== #

# Compare the rates of Do Not Know firm size and a non positive salary
# These could conceivably be related problems
#calculates probabilities 
df_private_employees %>%
  filter(!Principal_Category %in% c("Domestic Worker", "Free Trade Zone"),
         !Employment_Sector %in% "Electricity and Water") %>%
  mutate(dk_size = !wage_group %in% TIER_LEVELS,
         dk_wage = is.na(real_salary_income_wage_primary) |
           real_salary_income_wage_primary <= 0) %>%
  group_by(year) %>%
  summarise(p_size    = mean(dk_size),
            p_wage    = mean(dk_wage),
            p_both    = mean(dk_size & dk_wage),
            p_indep   = mean(dk_size) * mean(dk_wage),   # benchmark if unrelated
            lift      = mean(dk_size[dk_wage]) / mean(dk_size[!dk_wage]),
            .groups   = "drop")



# Shift Shares
#check share of workers in sectors and the sectors propensity of sectors to say Dont know Firm size
#test the idea that workers are moving to sectors more likely to report Dont Know firm size

ss <- df_mw_covered %>%
  mutate(dk = !wage_group %in% TIER_LEVELS) %>%
  filter(year %in% c(2015, 2025)) %>%
  group_by(year, Employment_Sector) %>%
  summarise(rate = mean(dk), share = n(), .groups = "drop_last") %>%
  mutate(share = share / sum(share)) %>%
  ungroup() %>%
  tidyr::pivot_wider(id_cols = Employment_Sector, names_from = year,
                     values_from = c(rate, share))

ss %>% summarise(
  within  = sum(share_2015 * (rate_2025 - rate_2015), na.rm = TRUE),
  between = sum(rate_2015 * (share_2025 - share_2015), na.rm = TRUE),
  interaction = sum((rate_2025 - rate_2015) * (share_2025 - share_2015), na.rm = TRUE)
)


#checking trend of Do Not Know Against Min Wage Policy

dk_qtr <- svyby(~I(TOTAL_PERSONAS_TRABAJAN_EMP == 98), ~year_quarter,
                design_mw_covered, svymean, na.rm = TRUE)

names(dk_qtr)   # look at what actually came back

design_mw_covered <- update(design_mw_covered,
                            dk98 = as.numeric(TOTAL_PERSONAS_TRABAJAN_EMP == 98))

dk_qtr <- svyby(~dk98, ~year_quarter, design_mw_covered, svymean, na.rm = TRUE) %>%
  tibble::as_tibble() %>%
  dplyr::rename(share = dk98, se = se) %>%
  dplyr::mutate(qdate = zoo::as.yearqtr(year_quarter, format = "%YQ%q"),
                t     = dplyr::row_number())

print(dk_qtr, n = Inf)


MW_EVENTS <- zoo::as.yearqtr(c("2015 Q2","2017 Q2","2019 Q3","2021 Q3","2023 Q2","2025 Q2"))
MW_PHASEIN <- zoo::as.yearqtr(c("2017 Q4","2022 Q1","2024 Q1"))   # completion dates

ggplot(dk_qtr, aes(qdate, share)) +
  geom_vline(xintercept = as.numeric(MW_EVENTS), colour = "firebrick", linetype = 2) +
  geom_vline(xintercept = as.numeric(MW_PHASEIN), colour = "grey60", linetype = 3) +
  geom_ribbon(aes(ymin = share - 1.96*se, ymax = share + 1.96*se), alpha = 0.15) +
  geom_line() + geom_point(size = 0.8) +
  zoo::scale_x_yearqtr(format = "%YQ%q", n = 14) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = NULL,
       title = "Don't-know firm size, MW-covered private employees",
       subtitle = "Red = MW revision; grey = phase-in completion")




dk_qtr %>%
  mutate(d = share - lag(share),
         is_event = qdate %in% MW_EVENTS,
         is_phase = qdate %in% MW_PHASEIN) %>%
  arrange(desc(abs(d))) %>%
  select(year_quarter, share, d, is_event, is_phase) %>%
  head(12)


dk_qtr <- dk_qtr %>%
  mutate(post_2021Q3 = as.numeric(qdate >= zoo::as.yearqtr("2021 Q3")),
         post_2023Q2 = as.numeric(qdate >= zoo::as.yearqtr("2023 Q2")),
         post_2020Q2 = as.numeric(qdate >= zoo::as.yearqtr("2020 Q2")))  # COVID control


summary(lm(share ~ t + post_2020Q2 + post_2021Q3 + post_2023Q2, data = dk_qtr))


placebo <- purrr::map_dfr(4:(nrow(dk_qtr) - 4), function(k) {
  d <- dk_qtr %>% mutate(brk = as.numeric(t >= k))
  m <- lm(share ~ t + brk, data = d)
  tibble(t = k, qtr = dk_qtr$year_quarter[k],
         coef = coef(m)["brk"], tstat = summary(m)$coefficients["brk","t value"])
}) %>% arrange(desc(abs(tstat)))

head(placebo, 10)


dd <- dk_qtr %>%
  mutate(d = share - lag(share),
         ev_2021Q3 = as.numeric(year_quarter == "2021Q3"),
         ev_2023Q2 = as.numeric(year_quarter == "2023Q2"),
         ev_2023Q3 = as.numeric(year_quarter == "2023Q3"),
         ev_2020Q2 = as.numeric(year_quarter == "2020Q2")) %>%
  filter(!is.na(d))

summary(lm(d ~ ev_2020Q2 + ev_2021Q3 + ev_2023Q2 + ev_2023Q3, data = dd))

library(lmtest)
lmtest::coeftest(lm(share ~ t + post_2020Q2 + post_2021Q3 + post_2023Q2, data = dk_qtr),
                 vcov = sandwich::NeweyWest)


# Does DK rise more in size bins near the legal thresholds (10, 50)?
# Proxy: pre-event tier composition by province x sector cell.
svyby(~dk98, ~year + Employment_Sector,
      design_mw_covered, svymean, na.rm = TRUE) %>%
  tibble::as_tibble() %>%
  tidyr::pivot_wider(id_cols = Employment_Sector, names_from = year, values_from = dk98)


# Does DK rise more among workers paid near the minimum?
v <- design_mw_covered$variables
floor_map <- tapply(v$nom_minwage_harmonized, as.character(v$year_quarter),
                    min, na.rm = TRUE)

design_mw_covered <- update(
  design_mw_covered,
  dk       = as.numeric(TOTAL_PERSONAS_TRABAJAN_EMP == 98),
  mw_floor = as.numeric(floor_map[as.character(year_quarter)])
)

design_mw_covered <- update(
  design_mw_covered,
  near_mw = as.numeric(real_salary_income_wage_primary < 1.25 * mw_floor)
)

svyby(~dk, ~near_mw + year, design_mw_covered, svymean, na.rm = TRUE)




# 1. Does the PSU frame turn over in 2023?
upm_sets <- split(Full_ENCFT_clean$UPM, Full_ENCFT_clean$year_quarter)
tibble(qtr = names(upm_sets)[-1],
       overlap = purrr::map2_dbl(upm_sets[-length(upm_sets)], upm_sets[-1],
                                 ~length(intersect(unique(.x), unique(.y))) /
                                   length(unique(.x)))) %>% print(n = Inf)

# 2. Do UNRELATED don't-know items jump at the same quarter?
df_mw_covered %>%
  transmute(year_quarter,
            firm_size = TOTAL_PERSONAS_TRABAJAN_EMP == 98,
            ars       = SEGURO_AFILIADO == 98,
            regimen   = REGIMEN_SALUD_AFILIADO == 98,
            emp_name  = NOMBRE_EMPRESA_TRABAJA == 98) %>%
  group_by(year_quarter) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  print(n = Inf)

table(df_mw_covered$wage_group, df_mw_covered$nom_minwage_harmonized)


