
config <- list(
  
  # --- Standard Outputs File Tree --- #
  output_stage = "Work In Progress",
  
  out_subdirs = list(
    charts = "Charts",
    tables = "Tables",
    regs   = "Regression Results",
    report = "Markdown Files"
  ),
  
  # --- Other File Paths --- #
  paths = list(
    raw_data            = "Raw Datasets",
    processed_data      = "Processed Data",
    outputs             = "Outputs",
    r_scripts           = "Code/R"
  ),
  
  # --- General figure and table settings ---#
  
  fig_defaults = list(width = 7, height = 4.5, units = "in", dpi = 300, format = "png"),
  
  table_defaults = list(digits = 2)
)