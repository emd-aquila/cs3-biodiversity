# =====================================================
# Load input data from deforestation_file_tag/analysis and build regression dataset.
# =====================================================

# ------------------------------
# Assert input data exists, load and normalize it, and ensure it has necessary columns
# ------------------------------
assert_exists(cluster_deltas_path)

cluster_deltas_raw <- read_csv(cluster_deltas_path, show_col_types = FALSE) %>%
  normalize_cluster_deltas_schema()

assert_has_cols(
  cluster_deltas_raw,
  required_cluster_deltas_cols,
  "cluster_deltas_raw"
)

assert_has_cols(
  cluster_deltas_raw,
  current_defor_source_col,
  "cluster_deltas_raw"
)

assert_has_cols(
  cluster_deltas_raw,
  c(current_delta_ov_source_col, current_delta_ov_annualized_source_col),
  "cluster_deltas_raw"
)

# ------------------------------
# Select the configured scale, filter, and transform raw data
# ------------------------------

# Select the configured deforestation and OV scale, then build log1p from that selected deforestation value.
cluster_deltas <- cluster_deltas_raw %>%
  mutate(
    delta_ov_non_annualized = as.numeric(.data[[current_delta_ov_source_col]]),
    delta_defor_ha_non_annualized = as.numeric(.data[[current_defor_source_col]]),
    delta_defor_ha_annualized_selected = if_else(
      !is.na(year_gap) & year_gap > 0,
      delta_defor_ha_non_annualized / year_gap,
      NA_real_
    ),
    delta_ov = if (identical(current_regression_scale, "annualized")) {
      as.numeric(.data[[current_delta_ov_annualized_source_col]])
    } else {
      delta_ov_non_annualized
    },
    delta_defor_ha = if (identical(current_regression_scale, "annualized")) {
      delta_defor_ha_annualized_selected
    } else {
      delta_defor_ha_non_annualized
    },
    delta_defor_ha_annualized = if_else(
      !is.na(year_gap) & year_gap > 0,
      delta_defor_ha_non_annualized / year_gap,
      NA_real_
    ),
    inverse_change = !is.na(delta_ov) & !is.na(delta_defor_ha) &
      (
        (delta_ov > 0 & delta_defor_ha < 0) |
          (delta_ov < 0 & delta_defor_ha > 0)
      )
    )

# Filter to only negative delta OV values on the selected scale, if applicable.
if (filter_neg_delta_ov == TRUE) {
  cluster_deltas <- cluster_deltas %>%
    filter(delta_ov < 0)
}

cluster_deltas <- cluster_deltas %>%
  log1p_defor()

# clean up cluster_deltas_raw so regression data is usable
regression_data <- build_regression_data(cluster_deltas)
  
message("Loaded cluster_deltas from: ", cluster_deltas_path)
message("Rows in cluster_deltas_raw: ", nrow(cluster_deltas_raw))
message("Regression scale: ", current_regression_scale)
message("Regression model family: ", current_regression_model_family)
message("Selected OV calculation method: ", current_ov_calculation_method)
message("Selected OV delta source column: ", current_delta_ov_source_col)
message("Selected deforestation source column: ", current_defor_source_col)
message("Created regression dataset 'regression_data'.")
message("Rows in regression_data: ", nrow(regression_data))
