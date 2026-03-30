# =====================================================
# Load input data from deforestation_file_tag/analysis and build regression dataset.
# =====================================================

assert_exists(cluster_deltas_path)

cluster_deltas_raw <- read_csv(cluster_deltas_path, show_col_types = FALSE)

cluster_deltas_neg_delta_ov <- cluster_deltas_raw %>% 
  filter(delta_ov < 0)

assert_has_cols(
  cluster_deltas_raw,
  cluster_deltas_required_cols,
  "cluster_deltas_raw"
)

# clean up cluster_deltas_raw so we have usable regression data
# regression_data <- build_regression_data(cluster_deltas_raw)
regression_data <- build_regression_data(cluster_deltas_neg_delta_ov)
  
message("Loaded cluster_deltas from: ", cluster_deltas_path)
message("Rows in cluster_deltas_raw: ", nrow(cluster_deltas_raw))
message("Created regression dataset 'regression_data'.")
message("Rows in regression_data: ", nrow(regression_data))