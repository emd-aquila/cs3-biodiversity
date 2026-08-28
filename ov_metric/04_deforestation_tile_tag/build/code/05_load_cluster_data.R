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

# This reads output/radius_<radius>/<method>/model_df_clustered.csv
# for the current loop iteration in run_build.R.
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

available_ov_score_cols <- intersect(ov_score_cols, names(cluster_raw))

if (length(available_ov_score_cols) == 0) {
  stop(
    "None of the configured OV score columns were found in cluster_raw: ",
    paste(ov_score_cols, collapse = ", "),
    call. = FALSE
  )
}

message("Loaded raw cluster inputs:")
message("  cluster_raw file: ", basename(cluster_file_path))
message("  cluster_raw rows: ", nrow(cluster_raw))
message("  OV score columns available: ", paste(available_ov_score_cols, collapse = ", "))
