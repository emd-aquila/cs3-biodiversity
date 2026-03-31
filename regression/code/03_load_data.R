# =====================================================
# Load input data from deforestation_file_tag/analysis and build regression dataset.
# =====================================================

# ------------------------------
# Load input data and ensure it has necessary columns
# ------------------------------
assert_exists(cluster_deltas_path)

cluster_deltas_raw <- read_csv(cluster_deltas_path, show_col_types = FALSE)

assert_has_cols(
  cluster_deltas_raw,
  cluster_deltas_required_cols,
  "cluster_deltas_raw"
)

# ------------------------------
# Filter and transform raw data as needed
# ------------------------------

# Filter to only negative delta OV values, if applicable
if (filter_neg_delta_ov == TRUE) {
  cluster_deltas <- cluster_deltas_raw %>% 
    filter(delta_ov < 0)
} else {
  cluster_deltas <- cluster_deltas_raw
}

# add the log1p transformed deltaDefor as a new column 
cluster_deltas <- cluster_deltas %>%
  mutate(
    log1p_delta_defor_ha = log1p(delta_defor_ha)
  )

# clean up cluster_deltas_raw so we have usable regression data
regression_data <- build_regression_data(cluster_deltas)
  
message("Loaded cluster_deltas from: ", cluster_deltas_path)
message("Rows in cluster_deltas_raw: ", nrow(cluster_deltas_raw))
message("Created regression dataset 'regression_data'.")
message("Rows in regression_data: ", nrow(regression_data))