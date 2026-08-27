#===============================================================================
# 
# File to centralize file paths and other universal parameters for project 
# analyzing Inequality in Dominican Republic.

# Filed called in in setup script. Setup script should be run before running most
# other files in the project.
#
# =============================================================================



config <- list(
  
  # --- Outputs File Tree --- #
  output_stage = "WIP",
  
  out_subdirs = list(
    data_check          = "Data Validation",
    trends              = "General Trends",
    desc_fig            = "Descriptive Figures",
    desc_tables         = "Desciptive Tables",
    reg_results         = "Regression Results",
    exp_validation      = "Exposure Validation"
  ),
  
  # --- File Paths --- #
  # here::here() resolves the repo root from a marker (.Rproj / .git) so these are robust to R's working directory.
  
  paths = list(
    raw_data       = here::here("Raw Datasets"),
    processed_data = here::here("Processed Data"),
    outputs        = here::here("Outputs"),
    scripts        = here::here("Code", "R", "clean scripts")
  ),
  
  # --- Derived-data subdirectories --- #
  # Resolved paths (unlike out_subdirs, which are names joined to output_stage
  # at the call site). Created by 00_setup.R. Shared inputs
  # (Full_ENCFT_clean.rds, Min_Wage.rds, CPI.rds) stay at the processed_data
  # root; these folders hold script-specific derived artifacts only.
  data_dirs = list(
    minwage = here::here("Processed Data", "MW Context and Bindingness"),
    regression = here::here("Processed Data", "Regression"),  # 07A/07B/08/08B
    inequality = here::here("Processed Data", "Inequality"),  # 05A/05B
    labor      = here::here("Processed Data", "Labor Market"), # 06A/06B
    exposure   = here::here("processed Data", "Exposure")
  ),
  
  
  # --- Parameters for loading survey data --- #
  first_year = 2014,
  last_year  = 2025,
  
  
  var_imports = c(
    "TRIMESTRE", "PERIODO", "ESTRATO", "DES_ESTRATO",
    "FACTOR_EXPANSION", "UPM", "VIVIENDA", "ID_HOGAR", "MIEMBRO", "ID_PERSONA",
    "ID_PROVINCIA", "DES_PROVINCIA", "GRUPO_REGION", "ORDEN_REGION",
    "SEXO", "EDAD", "PARENTESCO", "PAIS_NACIMIENTO",
    "OCUPADO", "SUBOCUPADO", "PEA", "DESOCUPADO", "INACTIVO", "HORAS_TRABAJA_SEMANA_PRINCIPAL",
    "ORDEN_RAMA", "GRUPO_RAMA", "ORDEN_OCUPACION", "OCUPACION_PRINCIPAL_COD", "GRUPO_OCUPACION",
    "ORDEN_CATEGORIA", "GRUPO_CATEGORIA",
    "ORDEN_EDUCACION", "GRUPO_EDUCACION", "GRUPO_EMPLEO", "CATEGORIA_PRINCIPAL",
    "INGRESO_ASALARIADO", "COMISIONES", "PROPINAS", "HORAS_EXTRA", "OTROS_PAGOS",
    "BONO_VACACIONES", "BONIFICACIONES", "REGALIA_PASCUAL", "INCENTIVO_ANTIGUEDAD", "OTROS_BENEFICIOS",
    "ESPECIE_ALIMENTOS", "ESPECIE_VIVIENDA", "ESPECIE_TRANSPORTE", "ESPECIE_COMBUSTIBLE",
    "ESPECIE_CELULAR", "OTROS_ESPECIE",
    "INGRESO_INDEPENDIENTES", "CONSUMO_BIENES", "ESPECIE_INDEPENDIENTES",
    "INGRESO_ASALARIADO_SECUN", "OTROS_PAGOS_SECUN", "OTROS_BENEFICIOS_SECUN", "PAGO_ESPECIE_SECUN",
    "INGRESO_INDEPENDIENTES_SECUN", "CONSUMO_BIENES_SECUN", "ESPECIE_INDEPENDIENTES_SECUN",
    "OTROS_TRABAJOS",
    "TOTAL_PERSONAS_TRABAJAN_EMP", "CANTIDAD_PERSONAS_TRABAJAN_EMP", "EMPRESA_INSCRITA_RNC",
    "HORAS_TRABAJO_EFECT_TOTAL", "HORAS_TRABAJA_SEMANA_PRINCIPAL", "RAZON_JORNADA_DIFERENTE",
    "HORAS_SEM_OCUP_PRINC", "INGRESO_LABORAL_MENSUAL", "INGRESO_LABORAL_HORA",
    "TIENE_CONTRATO", "EMPRESA_INSCRITA_RNC", "AFILIADO_AFP_PRINC", "EMPRESA_TIENE_LICENCIA", "REGISTRO_TRANSACCIONES_EMPRESA",
    "MAYOR_NIVEL_OBTENIDO"
  ),
  
  
  
  # --- Data and Modeling Parameters --- #
  
  # --- CPI base deflator ---#
  CPI_base_year = 2025,
  CPI_base_qtr  = 4,
  
  
  # --- Analysis window ----------------------------------------------------- #
  sample = list(
    start_qtr = "2016Q1",
    end_qtr   = "2025Q4"
  ),
  
  
  
  # --- Legal hours constants ----------------------------------------------- #
  hours = list(
    standard_week   = 44,        # Art. 147 LC
    weeks_per_month = 52 / 12
  ),
  
  # -- MW change events -- #
  
  #Phase-in quarters are folded into POST (not separate
  # events). Treatment quarter itself is EXCLUDED (partial exposure).
  # COVID quarters used in analysis to control for/ drop COVID effects
  
  events = list(
    event_qtrs    = c("2017Q2", "2019Q3", "2021Q3", "2023Q2", "2025Q2"),
    phase_in_qtrs = c("2017Q4", "2022Q1", "2024Q1"),
    covid_qtrs    = c("2020Q1", "2020Q2", "2020Q3", "2020Q4")
  ),
  
  
  
  
  # --- Income concepts (headline + robustness) ------------------------------ #
  #
  # Each entry pairs an INCOME variable with its matching FLOOR variable and a
  # log-variance outcome PREFIX (used in 09 to select the right outcome column
  # from the panel built in 08). Adding a new income concept means adding one
  # entry here; the exposure and estimation scripts read from this block via
  # config$active_income.
  #
  # hourly_base : Hourly, 44h-capped. Headline. Removes part-time and overtime
  #               composition from the wage measure. The 44h cap prevents the
  #               contamination Parente flags (regions with more part-timers
  #               would otherwise look mechanically more exposed under a monthly
  #               MW measure).
  # monthly     : Monthly salary. Robustness arm. Reintroduces the part-time
  #               composition confound acknowledged above; comparability with
  #               headline requires that finding.
  #
  # NAMING SEAM: real_minwage_harmonized is the MONTHLY real MW harmonized
  # across tiers in 01B (Micro pre-2021Q3 mapped to Small floor). Kept under
  # that name to preserve the compliance-measure plumbing in 02.
  # ------------------------------------------------------------------------- #
  
  income_specs = list(
    hourly_base = list(
      label          = "Hourly, 44h-capped (headline)",
      tag            = "hourly",
      income         = "real_salary_primary_hourly_base",
      minwage        = "real_minwage_hourly",
      compliance_var = "below_min_hourly_base_salary",
      log_var_prefix = "log_var_hwage"
    ),
    monthly = list(
      label          = "Monthly salary (robustness)",
      tag            = "monthly",
      income         = "real_salary_income_wage_primary",
      minwage        = "real_minwage_harmonized",   # naming seam - see block above
      compliance_var = "below_min_monthly_salary",
      log_var_prefix = "log_var_mwage"
    )
  ),
  
  # Active income concept. Loop in run_all_exposures.R overrides at runtime.
  active_income = "hourly_base",
  
  # --- Baselines (Parente-style exposure construction periods) -------------- #
  #
  # Each entry defines a "baseline period" for exposure construction. 07A reads
  # config$active_baseline and produces one set of exposure files per entry.
  # File suffix combines income tag and baseline tag, e.g.
  #   exposure_geo.rds                             hourly + base2016 (headline)
  #   exposure_geo_monthly.rds                     monthly + base2016
  #   exposure_geo_micro2021.rds                   hourly + base2021q2
  #   exposure_geo_monthly_micro2021.rds           monthly + base2021q2
  #
  # base2016_all_tiers :
  #   Pooled 2016 annual. Formal workers in all four firm-size tiers, each
  #   worker compared to their contemporaneous legally-binding floor
  #   (real_minwage_hourly via the 01B harmonization: Micro pre-2021Q3 -> Small).
  #   Exposure aggregated across tiers with employment-share weights.
  #   Rationale: 4 quarters remove seasonality; folds the 2015Q2 MW increase
  #   into the baseline so 2017Q2 is the first clean treatment.
  #
  # base2021q2_micro :
  #   Single quarter (2021Q2). Formal MICRO workers only, compared to the
  #   INCOMING 2021Q3 Micro floor (looked up from the survey via
  #   samples$reg_tier$data). No tier aggregation - single-tier design.
  #   Rationale: identifies exposure to the 2021Q3 carve-out event where a new
  #   Micro floor was introduced (previously micro firms faced the Small floor).
  #
  # CPI CAVEAT (base2021q2_micro): the incoming floor lookup returns the 2021Q3
  # real value in 2025Q4 pesos; worker wages at 2021Q2 are in 2025Q4 pesos
  # deflated by CPI(2021Q2). Ratio picks up a ~1.8% CPI wedge relative to a
  # nominal comparison. Small and common across regions (does not affect
  # tercile membership); noted here for the paper.
  # ------------------------------------------------------------------------- #
  
  baselines = list(
    base2016_all_tiers = list(
      label        = "2016 annual, all tiers, employment-weighted",
      tag          = "base2016_all_tiers",              # was ""
      period       = list(type = "year",    value = 2016),
      tiers        = "all",
      floor        = list(source = "worker"),
      weight_tiers = TRUE
    ),
    base2021q2_micro = list(
      label        = "2021Q2, micro tier only, incoming 2021Q3 floor",
      tag          = "base2021q2_micro",                # was "_micro2021"
      period       = list(type = "quarter", value = "2021Q2"),
      tiers        = "Micro",
      floor        = list(source = "incoming", qtr = "2021Q3", tier = "Micro"),
      weight_tiers = FALSE
    )
  ),
  
  # Active baseline. Loop in run_all_exposures.R overrides at runtime.
  active_baseline = "base2016_all_tiers",
  
  
  
  
  
  # --- Legacy config$income block ------------------------------------------- #
  # DEPRECATED. Kept because other scripts (02, 04A, 05A, 06A, possibly others)
  # still reference config$income$income and config$income$minwage_4tier_inc.
  # 07A and downstream regression scripts now read from config$income_specs.
  # Delete this block once all callers are migrated.
  # ------------------------------------------------------------------------- #
  income = list(
    income                = "real_salary_primary_hourly_base",
    income_monthly        = "real_salary_income_wage_primary",
    minwage_4tier_inc     = "real_minwage_hourly"
    # minwage_3tier_inc removed: 3tier scaffolding stripped (columns never built in 02)
  ),
  
  
  
  
  
  # --- Construction of Exposure to Min Wage -------------------------------- #
  # construct_geo: 4 regions (survey inference domain), 10 regions, or 32
  # provinces. Region10 balances estimate stability against treatment variation.
  #
  # (tier_scheme removed: 3tier scaffolding stripped since columns
  # real_minwage_hourly_3tier / Wage_group_3tier are never built in 02.
  # Reintroduce here if/when those columns get added upstream.)
  # ------------------------------------------------------------------------- #
  
  # --- Minimum-wage band & compliance tolerance (TWO DISTINCT PARAMETERS) -- #
  # mw_compliance_tolerance: DATA-QUALITY parameter. Survey rounding/recall
  #   cushion. Below-floor share uses (1 - tol) as the cutoff.
  #
  # mw_band_upper: ECONOMIC-CONCEPT parameter. Upper edge of the "at the floor"
  #   band. Workers above it earn enough that the floor is not plausibly
  #   binding. Ranking stability across mw_band_upper_grid checked in 07.
  # ------------------------------------------------------------------------- #
  
  exposure = list(
    construct_geo           = "Region10",
    mw_compliance_tolerance = 0.01,
    mw_band_upper           = 1.20,
    mw_band_upper_grid      = c(1.10, 1.20, 1.30, 1.50)
  ),
  
  TIER_LEVELS  = c("Micro", "Small", "Medium", "Large"),
  
  
  # Geography of INFERENCE (clustering level for SEs).
  regression = list(
    inference_geo           = "Region4",
    cluster_geo             = "Region10"
  ),
  
  
  
  
  # --- Figure-wide parameters ---------------------------------------------- #
  
  # --- General figure and table output settings --- #
  fig_defaults   = list(width = 7, height = 4.5, units = "in", dpi = 300, format = "png"),
  
  table_defaults = list(digits = 2),
  
  
  figures = list(
    min_cell_n           = 30,
    headline_concept     = "monthly",
    dist_focal_qtrs      = c("2019Q4", "2021Q2", "2023Q1", "2025Q4"),
    dist_pool_halfwidth  = 1L,
    bunch_groups         = c("Micro", "Small", "Rest"),
    micro_tier_start_qtr = "2021Q3",
    tier_colors = c("Micro"  = "#1b7837",
                    "Small"  = "#762a83",
                    "Medium" = "#e08214",
                    "Large"  = "#1f78b4")
  )
)
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  

 
  
  
  
  
  
  
  
  














