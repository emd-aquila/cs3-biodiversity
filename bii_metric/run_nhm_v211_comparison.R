# Build a matched NHM BII v2.1.1 benchmark comparison.  V1 is explicitly
# projected for all five published raster years under a distinct output prefix
# so it cannot overwrite the normal 2010/2030 LUH2 outputs.

launcher_code_dir <- Sys.getenv("CS3_BII_CODE_DIR", unset = "")
if (nzchar(launcher_code_dir)) {
  code_dir <- normalizePath(launcher_code_dir)
} else {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else NA_character_
  code_dir <- if (!is.na(script_path)) {
    dirname(normalizePath(file.path(dirname(script_path), "code", "17_validate_nhm_v211.R")))
  } else {
    normalizePath(file.path(getwd(), "code"))
  }
}
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(code_dir)

# This launcher's V1 comparison is intentional. It does not alter the user’s
# default configuration or existing V2 outputs; V2 is read only as its
# available-year sensitivity check in 17_validate_nhm_v211.R.
Sys.setenv(CS3_BII_VERSION = "v1")
source("00_libraries.R")
source("01_config.R")
source("02_helpers.R")
source("16_summarise_nhm_v211.R")

luh2_projection_years <- paste(nhm_v211_years, collapse = ",")
luh2_output_prefix <- nhm_v211_projection_prefix
source("run_luh2_bii_analysis.R")
source("17_validate_nhm_v211.R")
