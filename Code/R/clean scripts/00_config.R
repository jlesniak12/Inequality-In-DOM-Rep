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
    reg_results         = "Regression Results"
    
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
    regression = here::here("Processed Data", "Regression"),  # 07A/07B/08/08B
    inequality = here::here("Processed Data", "Inequality"),  # 05A/05B
    labor      = here::here("Processed Data", "Labor Market") # 06A/06B
  ),
  
  # --- General figure and table output settings --- #
  fig_defaults   = list(width = 7, height = 4.5, units = "in", dpi = 300, format = "png"),
  
  table_defaults = list(digits = 2),
  
  
  # --- Parameters for loading survey data --- #
  
  first_year = 2014,
  last_year  = 2025,
  
  CPI_base_year = 2025,
  CPI_base_qtr  = 4,
  
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
  
  
  
  
  # --- Modeling Parameters --- #
  
  # -- MW change events -- #
  
  #Phase-in quarters are folded into POST (not separate
  # events). Treatment quarter itself is EXCLUDED (partial exposure).
  # COVID quarters used in analysis to control for/ drop COVID effects
  
  events = list(
    event_qtrs    = c("2017Q2", "2019Q3", "2021Q3", "2023Q2", "2025Q2"),
    phase_in_qtrs = c("2017Q4", "2022Q1", "2024Q1"),
    covid_qtrs    = c("2020Q1", "2020Q2", "2020Q3", "2020Q4")
  ),
  
  
  # --- Income concept for exposure & below_min (HOURLY BASE, standard 44h week) --- #
  #   income: defines the income variable. 
  #        "real_salary_primary_hourly_base" refers to monthly worker salary converted
  #         to an hourly rate BUT with a cap of 44 hours a week so that so >44h workers are evaluated at the standard-week.
  #         Reasons for this are:
  #              1) To deal with the issue of part time workers reporting 
  #                 monthly salary < min (corrected using hourly view)
  #              2) To remove the issue of overtime for workers who work more than
  #                 44 hours per week since overtime compliance is a different
  #                 question than minimum wage compliance.
  
  #         
  #
  #    minwage_4tier_inc:
  #
  #    minwage_3tier_inc
  #
  #    "_base" refers to hourly salary which caps hours at 44 
  #   so >44h workers are evaluated at the standard-week. This implied
  #   rate (a wage-floor question), NOT spread over actual hours (which would be
  #   an overtime question and would inject hours-composition bias into the
  #   treatment). No overtime adjustment in the base spec.
  
  
  income = list(
   income               = "real_salary_primary_hourly_base",
    minwage_4tier_inc    = "real_minwage_hourly",
    minwage_3tier_inc         = "real_minwage_hourly_3tier"
    
  ),
  
  
  # -- Construction of Exposure to Min Wage -- #
  
  # construct_geo: Survey offers geographic variation at 4 regions (survey inference level),
  # 10 regions, and 32 provinces. We use 10 regions for balancing stability of estimate
  # and variation.
  
  # tier_scheme : Firm-size tier scheme used as the weighting dimension and floor selector.
  
  #   "4tier"  -> Wage_group        + real_minwage_hourly        (legal categories;
  #               matches descriptive figures; MAIN spec)
  #   "3tier"  -> Wage_group_3tier  + real_minwage_hourly_3tier  (Medium/Large
  #               collapsed at MEDIUM floor; ROBUSTNESS)
  
  # NEITHER is unbiased: 4tier overstates non-compliance in the 100+ bin (legal
  # mediums (firm between 100-150) judged against the higher large floor); 3tier understates it (true
  # larges judged against the lower medium floor). The bias is TREATMENT-
  # CORRELATED (varies with regional firm-size mix), so we report BOTH as bounds.
  
  # baseline_year: set baseline year. We use 2016 for following reasons
  #     (1) full year -> removes seasonality;
  #     (2) folds the 2015Q2 MW increase into the baseline so 2017Q2 is the first
  #     clean treatment (data start 2014Q3 leaves too little pre-2015Q2)
  
  
  
  # --- Minimum-wage band & compliance tolerance (TWO DISTINCT PARAMETERS) --- #
  # mw_compliance_tolerance: DATA-QUALITY parameter. Accounts for survey
  #   rounding/recall error in reported income. Used to calculate share of workers
  #   below min wage as (1 - tol) cushion. NOT an economic concept.
  #
  #
  # mw_band_upper: ECONOMIC-CONCEPT parameter — distinct from the
  #   data-quality tolerance above. It defines the set of workers whose wage is
  #   BOUND BY the minimum wage: those sitting at or just above the floor whose wage
  #   are plausibly bound by the minimum wage. Designed to capture workers near but
  #   not necessarily at the minimum wage.
  #
  #   Band = [1 - mw_compliance_tolerance, mw_band_upper] applied to the
  #   income/floor ratio:
  #     - LOWER edge = (1 - tolerance), deliberately identical to the compliance
  #       boundary so the "compliant" and "at-the-floor / exposed" cutoffs
  #       coincide — no gap where a worker is neither compliant nor exposed.
  #       Workers strictly below this edge are NON-COMPLIANT (wage < min wage) but
  #       NOT EXPOSED (assumed that if they were not making min wage it is not binding).
  #
  #     - UPPER edge (1.20 default) is tuned to the observed bunching in the
  #       formal wage distribution (figs MW6): the spike at the floor plus its
  #       immediate right shoulder. Workers above it earn enough that the floor
  #       is not plausibly binding on them.
  #
  #   The 1.20 cutoff is a judgement call, so exposure ranking stability across
  #   mw_band_upper_grid = c(1.10, 1.20, 1.30, 1.50) is checked in script 07
  #   (Spearman rank corr of geo exposure vs the default band).
  

  
  exposure = list(
    construct_geo           = "Region10",   # 32 provinces; fine treatment variation
    tier_scheme             = "4tier",           # "4tier" (MAIN) | "3tier" (ROBUSTNESS)
    baseline_year           = 2016,              # folds 2015Q2 event into baseline
    mw_compliance_tolerance = 0.01,              # data-quality cushion (1 - tol)
    mw_band_upper           = 1.20,              # economic "at-the-floor" upper edge
    mw_band_upper_grid      = c(1.10, 1.20, 1.30, 1.50)
  ),
  
  TIER_LEVELS  = c("Micro", "Small", "Medium", "Large"),
  


  # Geography of INFERENCE (clustering level for SEs). Set to Region10 to
  # MATCH the level at which treatment is assigned (exposure varies across
  # the 10 regions). Region4 reported as a
  # coarser-clustering robustness row to address the survey's certified
  # inference domain.
  
  regression = list(
    inference_geo           = "Region4", # official inference domain (Diseno_muestral)
    cluster_geo             = "Region10"
  )
  
)
  
  















