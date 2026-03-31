# =====================================================
# Load cluster input for the current method-radius run
# =====================================================

if (!exists("cluster_file_path")) {
  stop("cluster_file_path is not defined. Run set_cluster_run_paths() first.")
}

if (!file.exists(cluster_file_path)) {
  stop(
    "Cluster file not found: ",
    normalizePath(cluster_file_path, mustWork = FALSE)
  )
}

# This reads the METHOD_rad_RADIUSkm_model_df_clustered.csv specified in the current loop
# iteration in run_build.R
cluster_raw <- read_csv(
  cluster_file_path,
  show_col_types = FALSE
)

assert_has_cols(
  cluster_raw,
  c(
    "sample_id",
    "AEZ",
    "cluster_id",
    "Latitude",
    "Longitude",
    "year",
    "ov_score"
  ),
  "cluster_raw"
)

message("Loaded cluster input:")
message("  file: ", basename(cluster_file_path))
message("  rows: ", nrow(cluster_raw))