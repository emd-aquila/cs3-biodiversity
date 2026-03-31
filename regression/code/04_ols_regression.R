# =====================================================
# 04_ols_regression.R
# Fit raw OLS models and trimmed AEZ-specific variants, and save them
# =====================================================

# -----------------------------------------------------
# Preconditions
# -----------------------------------------------------

if (!exists("regression_data")) {
  stop("regression_data was not created in 03_load_data.R", call. = FALSE)
}

if (!is.data.frame(regression_data)) {
  stop("regression_data exists but is not a data frame.", call. = FALSE)
}

if (nrow(regression_data) == 0) {
  stop("regression_data has 0 rows after preprocessing.", call. = FALSE)
}

if (!exists("current_defor_transform")) {
  stop("current_defor_transform is not defined. Run set_regression_run_paths(...) first.", call. = FALSE)
}

# -----------------------------------------------------
# Choose regressor column for current run
# -----------------------------------------------------

regressor_col <- case_when(
  current_defor_transform == "raw" ~ "delta_defor_ha",
  current_defor_transform == "log1p" ~ "log1p_delta_defor_ha",
  TRUE ~ NA_character_
)

if (is.na(regressor_col)) {
  stop("Unknown current_defor_transform: ", current_defor_transform, call. = FALSE)
}

required_regression_cols <- c(
  "AEZ",
  "delta_ov",
  "delta_defor_ha",
  "log1p_delta_defor_ha",
  regressor_col
)

assert_has_cols(
  regression_data,
  required_regression_cols,
  "regression_data"
)

message("Regression sample rows: ", nrow(regression_data))
message("Unique AEZs in regression sample: ", n_distinct(regression_data$AEZ))
message("Current deforestation transform: ", current_defor_transform)
message("Regressor column: ", regressor_col)

transform_label <- paste0("defor_", current_defor_transform)








aez_counts_raw <- regression_data %>%
  filter(
    !is.na(AEZ),
    !is.na(.data[[regressor_col]]),
    !is.na(delta_ov)
  ) %>%
  group_by(AEZ) %>%
  summarise(
    n_obs = n(),
    n_unique_regressor = n_distinct(.data[[regressor_col]]),
    .groups = "drop"
  ) %>%
  arrange(AEZ)

eligible_aez_raw <- aez_counts_raw %>%
  filter(
    n_obs >= min_observations_per_aez_regression,
    n_unique_regressor >= 2
  ) %>%
  pull(AEZ) %>%
  as.character()

regression_data_aez_raw <- regression_data %>%
  filter(
    as.character(AEZ) %in% eligible_aez_raw,
    !is.na(.data[[regressor_col]]),
    !is.na(delta_ov)
  )

message(
  "AEZs meeting minimum observation threshold for baseline OLS (",
  min_observations_per_aez_regression,
  "): ",
  length(eligible_aez_raw)
)

if (length(eligible_aez_raw) == 0) {
  warning(
    "No AEZ has at least min_observations_per_aez_regression = ",
    min_observations_per_aez_regression,
    ". Skipping AEZ-specific baseline OLS."
  )
  models_aez_raw_ols <- list()
} else {
  models_aez_raw_ols <- fit_ols_by_aez_current(regression_data_aez_raw, regressor_col)
}

# -----------------------------------------------------
# AEZ-specific OLS after within-AEZ IQR trimming
# Trimming is performed on raw delta_defor_ha.
# -----------------------------------------------------

regression_data_iqr_trimmed_aez <- regression_data %>%
  filter(
    !is.na(AEZ),
    !is.na(delta_defor_ha),
    !is.na(delta_ov)
  ) %>%
  group_by(AEZ) %>%
  mutate(
    q1_defor = quantile(delta_defor_ha, 0.25, na.rm = TRUE),
    q3_defor = quantile(delta_defor_ha, 0.75, na.rm = TRUE),
    iqr_defor = q3_defor - q1_defor,
    lower_defor = q1_defor - 1.5 * iqr_defor,
    upper_defor = q3_defor + 1.5 * iqr_defor
  ) %>%
  filter(
    delta_defor_ha >= lower_defor,
    delta_defor_ha <= upper_defor
  ) %>%
  ungroup() %>%
  select(-q1_defor, -q3_defor, -iqr_defor, -lower_defor, -upper_defor)

aez_counts_iqr_trimmed <- regression_data_iqr_trimmed_aez %>%
  filter(
    !is.na(.data[[regressor_col]]),
    !is.na(delta_ov)
  ) %>%
  group_by(AEZ) %>%
  summarise(
    n_obs = n(),
    n_unique_regressor = n_distinct(.data[[regressor_col]]),
    .groups = "drop"
  ) %>%
  arrange(AEZ)

eligible_aez_iqr_trimmed <- aez_counts_iqr_trimmed %>%
  filter(
    n_obs >= min_observations_per_aez_regression,
    n_unique_regressor >= 2
  ) %>%
  pull(AEZ) %>%
  as.character()

regression_data_aez_iqr_trimmed <- regression_data_iqr_trimmed_aez %>%
  filter(
    as.character(AEZ) %in% eligible_aez_iqr_trimmed,
    !is.na(.data[[regressor_col]]),
    !is.na(delta_ov)
  )

message(
  "AEZs meeting minimum observation threshold after within-AEZ IQR trimming (",
  min_observations_per_aez_regression,
  "): ",
  length(eligible_aez_iqr_trimmed)
)

if (length(eligible_aez_iqr_trimmed) == 0) {
  warning(
    "No AEZ has at least min_observations_per_aez_regression = ",
    min_observations_per_aez_regression,
    " after within-AEZ IQR trimming. Skipping AEZ-specific IQR-trimmed OLS."
  )
  models_aez_iqr_trimmed_ols <- list()
} else {
  models_aez_iqr_trimmed_ols <- fit_ols_by_aez_current(
    regression_data_aez_iqr_trimmed,
    regressor_col
  )
}

# -----------------------------------------------------
# AEZ-specific OLS after within-AEZ top-5% trimming
# Trimming is performed on raw delta_defor_ha.
# -----------------------------------------------------

regression_data_top5_trimmed_aez <- regression_data %>%
  filter(
    !is.na(AEZ),
    !is.na(delta_defor_ha),
    !is.na(delta_ov)
  ) %>%
  group_by(AEZ) %>%
  mutate(
    p95_defor = quantile(delta_defor_ha, 0.95, na.rm = TRUE)
  ) %>%
  filter(delta_defor_ha <= p95_defor) %>%
  ungroup() %>%
  select(-p95_defor)

aez_counts_top5_trimmed <- regression_data_top5_trimmed_aez %>%
  filter(
    !is.na(.data[[regressor_col]]),
    !is.na(delta_ov)
  ) %>%
  group_by(AEZ) %>%
  summarise(
    n_obs = n(),
    n_unique_regressor = n_distinct(.data[[regressor_col]]),
    .groups = "drop"
  ) %>%
  arrange(AEZ)

eligible_aez_top5_trimmed <- aez_counts_top5_trimmed %>%
  filter(
    n_obs >= min_observations_per_aez_regression,
    n_unique_regressor >= 2
  ) %>%
  pull(AEZ) %>%
  as.character()

regression_data_aez_top5_trimmed <- regression_data_top5_trimmed_aez %>%
  filter(
    as.character(AEZ) %in% eligible_aez_top5_trimmed,
    !is.na(.data[[regressor_col]]),
    !is.na(delta_ov)
  )

message(
  "AEZs meeting minimum observation threshold after within-AEZ top-5% trimming (",
  min_observations_per_aez_regression,
  "): ",
  length(eligible_aez_top5_trimmed)
)

if (length(eligible_aez_top5_trimmed) == 0) {
  warning(
    "No AEZ has at least min_observations_per_aez_regression = ",
    min_observations_per_aez_regression,
    " after within-AEZ top-5% trimming. Skipping AEZ-specific top-5%-trimmed OLS."
  )
  models_aez_top5_trimmed_ols <- list()
} else {
  models_aez_top5_trimmed_ols <- fit_ols_by_aez_current(
    regression_data_aez_top5_trimmed,
    regressor_col
  )
}

# -----------------------------------------------------
# Save model objects
# -----------------------------------------------------

save_model_rds(
  models_aez_raw_ols,
  file.path(models_dir, paste0("models_aez_raw_ols__", transform_label, ".rds"))
)

save_model_rds(
  models_aez_iqr_trimmed_ols,
  file.path(models_dir, paste0("models_aez_iqr_trimmed_ols__", transform_label, ".rds"))
)

save_model_rds(
  models_aez_top5_trimmed_ols,
  file.path(models_dir, paste0("models_aez_top5_trimmed_ols__", transform_label, ".rds"))
)

# -----------------------------------------------------
# Export regression tables
# -----------------------------------------------------

if (length(models_aez_raw_ols) > 0) {
  save_modelsummary_table(
    models_aez_raw_ols,
    path = file.path(tables_dir, paste0("table_aez_raw_ols__", transform_label, ".html")),
    title = paste0("AEZ-Specific OLS: Delta OV on ", regressor_col)
  )
}

if (length(models_aez_iqr_trimmed_ols) > 0) {
  save_modelsummary_table(
    models_aez_iqr_trimmed_ols,
    path = file.path(tables_dir, paste0("table_aez_iqr_trimmed_ols__", transform_label, ".html")),
    title = paste0("AEZ-Specific OLS after Within-AEZ IQR Trimming: ", regressor_col)
  )
}

if (length(models_aez_top5_trimmed_ols) > 0) {
  save_modelsummary_table(
    models_aez_top5_trimmed_ols,
    path = file.path(tables_dir, paste0("table_aez_top5_trimmed_ols__", transform_label, ".html")),
    title = paste0("AEZ-Specific OLS after Within-AEZ Top-5% Trimming: ", regressor_col)
  )
}

message("Finished 04_ols_regression.R")
message("  current run: ", current_run_stub)
message("  regressor column: ", regressor_col)