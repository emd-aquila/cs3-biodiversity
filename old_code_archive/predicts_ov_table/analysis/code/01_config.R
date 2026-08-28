# =====================================================
# Configuration for the PREDICTS analysis pipeline
# =====================================================

analysis_dir <- ".."
tmp_dir <- file.path(analysis_dir, "tmp")
output_dir <- file.path(analysis_dir, "output")
build_output_dir <- file.path("..", "..", "build", "output")
aez_path <- file.path("..", "..", "..", "spatial_data", "aez", "AEZ_shp_file.shp")

dirs_to_create <- c(tmp_dir, output_dir)

for (dir_path in dirs_to_create) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
}

predicts_file <- file.path(build_output_dir, "predicts_filtered_2000_2024.csv")
pd_file <- file.path(build_output_dir, "pd_result.csv")

required_files <- c(predicts_file, pd_file, aez_path)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required analysis inputs: ",
    paste(normalizePath(missing_files, mustWork = FALSE), collapse = ", "),
    call. = FALSE
  )
}
