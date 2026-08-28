# =====================================================
# Configuration for OV calculation step
# =====================================================

calculation_dir <- normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = TRUE)
ov_dir <- normalizePath(file.path(calculation_dir, ".."), winslash = "/", mustWork = TRUE)
ov_metric_dir <- normalizePath(file.path(ov_dir, ".."), winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(file.path(ov_metric_dir, ".."), winslash = "/", mustWork = TRUE)

code_dir <- file.path(calculation_dir, "code")
tmp_dir <- file.path(calculation_dir, "tmp")
output_dir <- file.path(calculation_dir, "output")
build_output_dir <- file.path(ov_dir, "build", "output")

for (dir_path in c(tmp_dir, output_dir)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

integration_output_dir <- file.path(ov_metric_dir, "01_biodiversity_data_integration", "output")
database_specs <- data.table::data.table(
  database_key = c("predicts", "biotime", "combined"),
  database_label = c("PREDICTS", "BioTIME", "Combined"),
  input_rds = file.path(
    integration_output_dir,
    c("predicts_database.rds", "biotime_database.rds", "combined_database.rds")
  ),
  pd_csv = file.path(
    build_output_dir,
    c("predicts_pd_result.csv", "biotime_pd_result.csv", "combined_pd_result.csv")
  )
)

aez_path <- file.path(project_dir, "00_spatial_data", "aez", "AEZ_shp_file.shp")

required_files <- c(database_specs$input_rds, database_specs$pd_csv, aez_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing required OV calculation inputs: ",
    paste(normalizePath(missing_files, mustWork = FALSE), collapse = ", "),
    call. = FALSE
  )
}
