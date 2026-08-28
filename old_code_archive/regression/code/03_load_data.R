# =====================================================
# Load input data from deforestation_file_tag/analysis and build regression dataset.
# =====================================================

# ------------------------------
# Assert input data exists, load and normalize it, and ensure it has necessary columns
# ------------------------------
cluster_deltas_raw <- read_cluster_deltas_cached(cluster_deltas_path)

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

assert_has_cols(
  cluster_deltas_raw,
  current_starting_ov_source_col,
  "cluster_deltas_raw"
)

# ------------------------------
# Select the configured annualization mode, filter, and transform raw data
# ------------------------------

# Select the configured deforestation and ∆OV annualization. Percent-change
# scenarios overwrite delta_ov after the selected linear delta is available.
cluster_deltas <- cluster_deltas_raw %>%
  mutate(
    starting_ov = as.numeric(.data[[current_starting_ov_source_col]]),
    delta_ov_non_annualized = as.numeric(.data[[current_delta_ov_source_col]]),
    delta_defor_ha_non_annualized = as.numeric(.data[[current_defor_source_col]]),
    delta_defor_ha_annualized_selected = if_else(
      !is.na(year_gap) & year_gap > 0,
      delta_defor_ha_non_annualized / year_gap,
      NA_real_
    ),
    delta_ov_linear = if (is_annualized_delta_mode()) {
      as.numeric(.data[[current_delta_ov_annualized_source_col]])
    } else {
      delta_ov_non_annualized
    },
    delta_ov = if (ov_change_uses_percent()) {
      if_else(
        !is.na(starting_ov) & starting_ov > 0,
        100 * delta_ov_linear / starting_ov,
        NA_real_
      )
    } else {
      delta_ov_linear
    },
    delta_defor_ha = if (is_annualized_delta_mode()) {
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

rows_before_ov_change <- nrow(cluster_deltas)
cluster_deltas <- apply_ov_change(cluster_deltas)
rows_after_ov_change <- nrow(cluster_deltas)

# Filter to only negative ∆OV values on the selected annualization mode, if applicable.
if (filter_neg_delta_ov == TRUE) {
  cluster_deltas <- cluster_deltas %>%
    filter(delta_ov < 0)
}

cluster_deltas <- cluster_deltas %>%
  log1p_defor()

# clean up cluster_deltas_raw so regression data is usable
regression_data <- build_regression_data(cluster_deltas)
  
log_verbose("Loaded cluster_deltas from: ", cluster_deltas_path)
log_verbose("Rows in cluster_deltas_raw: ", nrow(cluster_deltas_raw))
log_verbose("Annualization mode: ", current_annualization_mode)
log_verbose("Regression model: ", current_regression_model)
log_verbose("Selected OV calculation method: ", current_ov_calculation_method)
log_verbose("Selected OV delta source column: ", current_delta_ov_source_col)
log_verbose("OV change mode: ", current_ov_change_mode)
log_verbose("Starting OV source column: ", current_starting_ov_source_col)
log_verbose("Starting OV threshold: ", ifelse(is.na(current_ov_threshold), "none", current_ov_threshold))
log_verbose("Percentage OV change outcome: ", ov_change_uses_percent())
log_verbose(
  "Rows after OV change preprocessing: ",
  rows_after_ov_change,
  " (removed ",
  rows_before_ov_change - rows_after_ov_change,
  ")"
)
log_verbose("Selected deforestation source column: ", current_defor_source_col)
log_verbose("Created regression dataset 'regression_data'.")
log_verbose("Rows in regression_data: ", nrow(regression_data))
