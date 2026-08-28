

# Build tagged filenames following: <item>__<income>__<baseline>__<geo>.rds
#
# Defaults pull from the currently-active baseline / income / geo in config.
# Any argument can be overridden explicitly to read a file from a different
# spec than the active one (e.g. reading a base2016 exposure while running a
# base2021q2 estimation).
#
# Examples:
#   mw_file("exposure_geo")
#     -> file.path(pd, "exposure_geo__hourly__base2016_all_tiers__Region10.rds")
#
#   mw_file("panel_geo_quarter", baseline_tag = "base2016_all_tiers")
#     -> reads the 2016 baseline panel even during a 2021q2 driver iteration
#
mw_file <- function(item,
                    dir,
                    income_tag   = NULL,
                    baseline_tag = NULL,
                    geo          = NULL) {
  
  income_tag   <- income_tag   %||% config$income_specs[[config$active_income]]$tag
  baseline_tag <- baseline_tag %||% config$baselines[[config$active_baseline]]$tag
  geo          <- geo          %||% config$exposure$construct_geo
  
  missing <- c(dir          = missing(dir) || is.null(dir) || !nzchar(dir),
               income_tag   = is.null(income_tag)   || !nzchar(income_tag),
               baseline_tag = is.null(baseline_tag) || !nzchar(baseline_tag),
               geo          = is.null(geo)          || !nzchar(geo))
  if (any(missing)) {
    stop("mw_file('", item, "'): missing arg(s): ",
         paste(names(missing)[missing], collapse = ", "), ".", call. = FALSE)
  }
  
  file.path(dir,
            paste0(paste(item, income_tag, baseline_tag, geo, sep = "__"),
                   ".rds"))
}