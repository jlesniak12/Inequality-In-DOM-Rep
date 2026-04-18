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
    labor_market        = "Labor Market and Demographics",
    income              = "Incomes",
    inequality          = "Income Inequality",
    inequality_minwage  = "Income Inequality and Min Wage",
    data_checks         = "Data and Sample Analysis",
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
    "HORAS_TRABAJO_EFECT_TOTAL", "HORAS_TRABAJA_SEMANA_PRINCIPAL", "RAZON_JORNADA_DIFERENTE"
  )
)

