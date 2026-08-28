# =====================================================
# Configuration for PD build step
# =====================================================

build_dir <- normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = TRUE)
ov_dir <- normalizePath(file.path(build_dir, ".."), winslash = "/", mustWork = TRUE)
ov_metric_dir <- normalizePath(file.path(ov_dir, ".."), winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(file.path(ov_metric_dir, ".."), winslash = "/", mustWork = TRUE)

code_dir <- file.path(build_dir, "code")
input_dir <- file.path(build_dir, "input")
tmp_dir <- file.path(build_dir, "tmp")
output_dir <- file.path(build_dir, "output")

for (dir_path in c(input_dir, tmp_dir, output_dir)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

integration_output_dir <- file.path(ov_metric_dir, "01_biodiversity_data_integration", "output")
database_specs <- data.table::data.table(
  database_key = c("predicts", "biotime", "combined"),
  database_label = c("PREDICTS", "BioTIME", "Combined"),
  input_rds = file.path(
    integration_output_dir,
    c("predicts_database.rds", "biotime_database.rds", "combined_database.rds")
  )
)

tree_file <- file.path(input_dir, "iphylo_tree.nwk")
build_manifest_path <- file.path(output_dir, "output.csv")
reuse_existing_pd_outputs <- TRUE
rebuild_pd_datasets <- strsplit(Sys.getenv("OV_REBUILD_PD_DATASETS", unset = ""), ",", fixed = TRUE)[[1]]
rebuild_pd_datasets <- trimws(rebuild_pd_datasets[nzchar(trimws(rebuild_pd_datasets))])

required_files <- c(database_specs$input_rds, tree_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing required PD build inputs: ",
    paste(normalizePath(missing_files, mustWork = FALSE), collapse = ", "),
    call. = FALSE
  )
}
