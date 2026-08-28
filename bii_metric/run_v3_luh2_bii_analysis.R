# Run the opt-in V3 strict-recuration / detailed-LUH2 BII comparison.
launcher_code_dir <- Sys.getenv("CS3_BII_CODE_DIR", unset = "")
if (nzchar(launcher_code_dir)) {
  code_dir <- normalizePath(launcher_code_dir)
} else {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1L]]) else NA_character_
  code_dir <- if (!is.na(script_path)) dirname(normalizePath(file.path(dirname(script_path), "code", "19_bii_v3.R"))) else normalizePath(file.path(getwd(), "code"))
}
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(code_dir)

source("00_libraries.R")
source("01_config.R")
source("02_helpers.R")
source("19_bii_v3.R")
for (path in c(hill2018_site_recuration_path, luh2_historical_path, luh2_future_path, luh2_static_path,
               luh2_grid_lookup_path, v3_population_2010_path, v3_population_2020_path, v3_population_2030_path)) {
  assert_file_exists(path, "required V3 input")
}
fit_bii_v3_models()

python_path <- Sys.getenv("CS3_BII_LUH2_PYTHON", unset = file.path(bii_dir, "tmp", "luh2_venv", "bin", "python"))
if (!file.exists(python_path)) stop("V3 LUH2 preparation requires Python with netCDF4 and NumPy.", call. = FALSE)
arguments <- c(
  "20_prepare_luh2_v3_inputs.py", "--historical", luh2_historical_path, "--future", luh2_future_path,
  "--static", luh2_static_path, "--grid-lookup", luh2_grid_lookup_path,
  "--population-2010", v3_population_2010_path, "--population-2020", v3_population_2020_path,
  "--population-2030", v3_population_2030_path, "--intensity-mixture", v3_intensity_mixture_path,
  "--landuse-output", v3_luh2_landuse_path, "--pressure-output", v3_luh2_pressure_path
)
status <- system2(python_path, args = shQuote(arguments), stdout = TRUE, stderr = TRUE)
message(paste(status, collapse = "\n"))
if (!is.null(attr(status, "status")) && attr(status, "status") != 0) stop("V3 LUH2 input preparation failed.", call. = FALSE)
project_bii_landuse_v3()
source("21_validate_v3_nhm_v211.R")
message("V3 LUH2 BII projection and V1/V2/V3 comparison complete: ", output_dir)
