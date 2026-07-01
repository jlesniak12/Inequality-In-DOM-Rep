#===============================================================================
# 
# File to centralize file paths and other universal parameters for project 
# analyzing Inequality in Dominican Republic.

# Filed called in in setup script. Setup script should be run before running most
# other files in the project.
#
# =============================================================================



config <- list(
  
  # --- Standard Outputs File Tree --- #
  output_stage = "Work In Progress",
  
  out_subdirs = list(
    data_checks         = "Data and Sample Analysis",
    labor_market        = "Labor Market and Demographics",
    income              = "Income Trends",
    inequality          = "Income Inequality Trends",
    minwage             = "Minimum Wage Compliance and Hours",
    
    inequality_minwage  = "Income Inequality and Min Wage",
   
    reg_sector          = "Regression Results/Sector",
    reg_sector_firmsize = "Regression Results/Sector x Firm Size"
  ),
  
  # --- Other File Paths --- #
  paths = list(
    raw_data            = "Raw Datasets",
    processed_data      = "Processed Data",
    outputs             = "Outputs",
    r_scripts           = "Code/R"
  ),
  
  # --- General figure and table settings --- #
  fig_defaults = list(width = 7, height = 4.5, units = "in", dpi = 300, format = "png"),
  table_defaults = list(digits = 2),
  
  
  
  # --- parameters for loading survey data --- #
  first_year = 2014,
  last_year = 2025,
  
  var_imports = c("TRIMESTRE", "PERIODO", "ESTRATO",	"DES_ESTRATO",
    "FACTOR_EXPANSION", "UPM", "VIVIENDA", "ID_HOGAR", "MIEMBRO", "ID_PERSONA",
    "ID_PROVINCIA", "DES_PROVINCIA", "GRUPO_REGION", "ORDEN_REGION",
    "SEXO", "EDAD", "PARENTESCO", "PAIS_NACIMIENTO",
    "OCUPADO", "SUBOCUPADO", "PEA", "DESOCUPADO","INACTIVO", "HORAS_TRABAJA_SEMANA_PRINCIPAL",
    "ORDEN_RAMA", "GRUPO_RAMA", "ORDEN_OCUPACION", "OCUPACION_PRINCIPAL_COD", "GRUPO_OCUPACION", "ORDEN_CATEGORIA", "GRUPO_CATEGORIA", 
    "ORDEN_EDUCACION", "GRUPO_EDUCACION", "GRUPO_EMPLEO", "CATEGORIA_PRINCIPAL",
    
    "INGRESO_ASALARIADO", "COMISIONES", "PROPINAS", "HORAS_EXTRA", "OTROS_PAGOS",
    "BONO_VACACIONES", "BONIFICACIONES", "REGALIA_PASCUAL", "INCENTIVO_ANTIGUEDAD", "OTROS_BENEFICIOS",
    "ESPECIE_ALIMENTOS", "ESPECIE_VIVIENDA", "ESPECIE_TRANSPORTE", "ESPECIE_COMBUSTIBLE", "ESPECIE_CELULAR", "OTROS_ESPECIE",
    "INGRESO_INDEPENDIENTES", "CONSUMO_BIENES", "ESPECIE_INDEPENDIENTES",
    
    "INGRESO_ASALARIADO_SECUN", "OTROS_PAGOS_SECUN", "OTROS_BENEFICIOS_SECUN", "PAGO_ESPECIE_SECUN",
    "INGRESO_INDEPENDIENTES_SECUN", "CONSUMO_BIENES_SECUN", "ESPECIE_INDEPENDIENTES_SECUN",
    "OTROS_TRABAJOS",
    
    "TOTAL_PERSONAS_TRABAJAN_EMP", "CANTIDAD_PERSONAS_TRABAJAN_EMP", "EMPRESA_INSCRITA_RNC", 
    "HORAS_TRABAJO_EFECT_TOTAL", "HORAS_TRABAJA_SEMANA_PRINCIPAL", "RAZON_JORNADA_DIFERENTE",
    
    "HORAS_SEM_OCUP_PRINC", "INGRESO_LABORAL_MENSUAL", "INGRESO_LABORAL_HORA"
  ),
  
  # --- Exposure / regression methodology parameters --- #
  
  exposure = list(
    
    # Geography of EXPOSURE CONSTRUCTION (fine treatment-intensity variation).
    # Province is below the survey's certified inference domain, but exposure is
    # built on a POOLED baseline YEAR (2016), which greatly reduces sampling
    # error relative to the quarterly design-variable estimates the Central Bank
    # retreated from. So province is acceptable for CONSTRUCTING a baseline
    # characteristic, even though it is not a certified inference domain.
    construct_geo = "DES_PROVINCIA",   # 32 provinces
    
    # Geography of INFERENCE (clustering level for SEs).
    # Region4 (Gran Santo Domingo / Norte / Sur / Este) is the survey's OFFICIAL
    # domain of inference per Diseno_muestral.pdf (Ficha Tecnica, p.16). This is
    # the conservative / defensible default. 4 clusters is few -> wild bootstrap
    # is load-bearing, not optional. Region10 and province offered as robustness.
    inference_geo = "Region4",
    
    # Firm-size tier scheme used as the weighting dimension and floor selector.
    #   "4tier"  -> Wage_group        + real_minwage_hourly        (legal categories;
    #               matches descriptive figures; MAIN spec)
    #   "3tier"  -> Wage_group_3tier  + real_minwage_hourly_3tier  (Medium/Large
    #               collapsed at MEDIUM floor; ROBUSTNESS)
    # NEITHER is unbiased: 4tier overstates non-compliance in the 100+ bin (legal
    # mediums judged against the higher large floor); 3tier understates it (true
    # larges judged against the lower medium floor). The bias is TREATMENT-
    # CORRELATED (varies with regional firm-size mix), so we report BOTH as bounds.
    tier_scheme = "4tier",
    
    # Baseline year for fixed exposure. 2016 annual average:
    #   (1) full year -> removes seasonality;
    #   (2) folds the 2015Q2 MW increase into the baseline so 2017Q2 is the first
    #       clean treatment (data start 2014Q3 leaves too little pre-2015Q2).
    baseline_year = 2016,
    
    # --- Minimum-wage band & compliance tolerance (TWO DISTINCT PARAMETERS) ---
    # MW_COMPLIANCE_TOLERANCE: DATA-QUALITY parameter. Accounts for survey
    #   rounding/recall error in reported income. Used by below_min in script 02
    #   as the (1 - tol) cushion. NOT an economic concept.
    mw_compliance_tolerance = 0.01,
    
    # MW band: ECONOMIC-CONCEPT parameter. Defines who counts as a "minimum-wage
    #   worker" (whose wage is bound by the floor) -> Parente's exposure concept.
    #   Lower edge = (1 - tolerance) so the "compliant" boundary and the
    #   "at-the-floor-exposed" boundary coincide (no 1% no-man's-land at the seam).
    #   Workers strictly below this edge are NON-COMPLIANT (-> below_min), NOT
    #   "exposed". Upper edge tuned to observed bunching (figs MW6): the spike +
    #   immediate right shoulder. 1.20 default; ranking stability checked across
    #   the grid below.
    mw_band_upper        = 1.20,
    mw_band_upper_grid   = c(1.10, 1.20, 1.30, 1.50),
    
    # Income concept for exposure & below_min (HOURLY BASE, standard 44h week).
    #   _base caps hours at 44 so >44h workers are evaluated at the standard-week
    #   rate (a wage-floor question), NOT spread over actual hours (which would be
    #   an overtime question and would inject hours-composition bias into the
    #   treatment). No overtime adjustment in the base spec.
    income_hourly = "real_salary_primary_hourly_base",
    minwage_hourly_4tier = "real_minwage_hourly",
    minwage_hourly_3tier = "real_minwage_hourly_3tier"
  ),
  
  events = list(
    # MW change events. Phase-in quarters are folded into POST (not separate
    # events). Treatment quarter itself is EXCLUDED (partial exposure).
    event_qtrs   = c("2017Q2", "2019Q3", "2021Q3", "2023Q2"),
    phase_in_qtrs = c("2017Q4", "2022Q1", "2024Q1"),
    covid_qtrs   = c("2020Q1","2020Q2","2020Q3","2020Q4","2021Q1","2021Q2")
  )
)


  
  
  


