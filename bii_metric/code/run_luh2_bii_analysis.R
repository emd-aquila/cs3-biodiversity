# Run the direct LUH2 BII projection after the PREDICTS response models exist.
if (isTRUE(bii_v2_enabled)) {
  for (path in c(
    luh2_historical_path, luh2_future_path, luh2_static_path, luh2_grid_lookup_path,
    v2_luh2_population_2010_path, v2_luh2_population_2030_path, v2_luh2_crosswalk_path
  )) {
    assert_file_exists(path, "required V2 LUH2 input")
  }
  source("03_prepare_predicts.R")
  source("13_prepare_v2_site_population.R")
  source("12_bii_v2.R")
  fit_bii_v2_models()

  python_path <- Sys.getenv(
    "CS3_BII_LUH2_PYTHON",
    unset = file.path(bii_dir, "tmp", "luh2_venv", "bin", "python")
  )
  if (!file.exists(python_path)) {
    stop("V2 LUH2 preparation requires Python with netCDF4 and NumPy.", call. = FALSE)
  }
  arguments <- c(
    "14_prepare_luh2_v2_inputs.py",
    "--historical", luh2_historical_path,
    "--future", luh2_future_path,
    "--static", luh2_static_path,
    "--grid-lookup", luh2_grid_lookup_path,
    "--population-2010", v2_luh2_population_2010_path,
    "--population-2030", v2_luh2_population_2030_path,
    "--landuse-output", v2_luh2_landuse_path,
    "--pressure-output", v2_luh2_pressure_path
  )
  status <- system2(python_path, args = shQuote(arguments), stdout = TRUE, stderr = TRUE)
  message(paste(status, collapse = "\n"))
  if (!identical(attr(status, "status"), NULL) && attr(status, "status") != 0) {
    stop("V2 LUH2 input preparation failed.", call. = FALSE)
  }
  landuse <- prepare_landuse_with_crosswalk(v2_luh2_landuse_path, v2_luh2_crosswalk_path, "LUH2 V2")
  project_bii_landuse_v2(landuse, v2_luh2_pressure_path, "luh2")
  source("15_validate_luh2_v2.R")
  message("V2 LUH2 BII projection and validation complete: ", output_dir)
} else {
assert_file_exists(response_table_path, "fitted PREDICTS BII response table")
for (path in c(luh2_historical_path, luh2_future_path, luh2_static_path, luh2_country_boundaries_path)) {
  assert_file_exists(path, "required LUH2 input")
}

source("10_prepare_luh2_grid_lookup.R")

python_path <- Sys.getenv(
  "CS3_BII_LUH2_PYTHON",
  unset = file.path(bii_dir, "tmp", "luh2_venv", "bin", "python")
)
if (!file.exists(python_path)) {
  stop(
    "LUH2 projection requires Python with netCDF4 and NumPy. Set CS3_BII_LUH2_PYTHON or create bii_metric/tmp/luh2_venv.",
    call. = FALSE
  )
}

arguments <- c(
  "11_project_luh2.py",
  "--historical", luh2_historical_path,
  "--future", luh2_future_path,
  "--static", luh2_static_path,
  "--grid-lookup", luh2_grid_lookup_path,
  "--responses", response_table_path,
  "--published", published_bii_path,
  "--output-dir", output_dir,
  "--years", luh2_projection_years,
  "--output-prefix", luh2_output_prefix
)
status <- system2(python_path, args = shQuote(arguments), stdout = TRUE, stderr = TRUE)
message(paste(status, collapse = "\n"))
if (!identical(attr(status, "status"), NULL) && attr(status, "status") != 0) {
  stop("LUH2 BII projection failed.", call. = FALSE)
}
}
