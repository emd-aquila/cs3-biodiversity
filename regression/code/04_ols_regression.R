# =====================================================
# 04_ols_regression.R
# Fit raw OLS models and trimmed AEZ-specific variants, and save them
# =====================================================

# TODO:
# create a folder pattern to differentiate between different approaches





# -----------------------------------------------------
# Basic sample checks
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

required_regression_cols <- c(
  "AEZ",
  "delta_ov",
  "delta_defor_ha"
)

assert_has_cols(
  regression_data,
  required_regression_cols,
  "regression_data"
)

message("Regression sample rows: ", nrow(regression_data))
message("Unique AEZs in regression sample: ", n_distinct(regression_data$AEZ))

# -----------------------------------------------------
# Baseline AEZ-specific raw OLS
# Outcome: delta_ov
# Regressor: delta_defor_ha
# -----------------------------------------------------

aez_counts_raw <- regression_data %>%
  filter(
    !is.na(AEZ),
    !is.na(delta_defor_ha),
    !is.na(delta_ov)
  ) %>%
  group_by(AEZ) %>%
  summarise(
    n_obs = n(),
    n_unique_delta_defor_ha = n_distinct(delta_defor_ha),
    .groups = "drop"
  ) %>%
  arrange(AEZ)

eligible_aez_raw <- aez_counts_raw %>%
  filter(
    n_obs >= min_observations_per_aez_regression,
    n_unique_delta_defor_ha >= 2
  ) %>%
  pull(AEZ) %>%
  as.character()

regression_data_aez_raw <- regression_data %>%
  filter(
    as.character(AEZ) %in% eligible_aez_raw,
    !is.na(delta_defor_ha),
    !is.na(delta_ov)
  )

message(
  "AEZs meeting minimum observation threshold for raw OLS (",
  min_observations_per_aez_regression,
  "): ",
  length(eligible_aez_raw)
)

if (length(eligible_aez_raw) == 0) {
  warning(
    "No AEZ has at least min_observations_per_aez_regression = ",
    min_observations_per_aez_regression,
    ". Skipping AEZ-specific raw OLS."
  )
  models_aez_raw_ols <- list()
} else {
  models_aez_raw_ols <- fit_ols_by_aez(regression_data_aez_raw)
}

# -----------------------------------------------------
# AEZ-specific raw OLS after within-AEZ IQR trimming
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
  group_by(AEZ) %>%
  summarise(
    n_obs = n(),
    n_unique_delta_defor_ha = n_distinct(delta_defor_ha),
    .groups = "drop"
  ) %>%
  arrange(AEZ)

eligible_aez_iqr_trimmed <- aez_counts_iqr_trimmed %>%
  filter(
    n_obs >= min_observations_per_aez_regression,
    n_unique_delta_defor_ha >= 2
  ) %>%
  pull(AEZ) %>%
  as.character()

regression_data_aez_iqr_trimmed <- regression_data_iqr_trimmed_aez %>%
  filter(as.character(AEZ) %in% eligible_aez_iqr_trimmed)

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
  models_aez_iqr_trimmed_ols <- fit_ols_by_aez(regression_data_aez_iqr_trimmed)
}

# -----------------------------------------------------
# AEZ-specific raw OLS after within-AEZ top-5% trimming
# Removes observations above the 95th percentile of
# delta_defor_ha within each AEZ
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
  group_by(AEZ) %>%
  summarise(
    n_obs = n(),
    n_unique_delta_defor_ha = n_distinct(delta_defor_ha),
    .groups = "drop"
  ) %>%
  arrange(AEZ)

eligible_aez_top5_trimmed <- aez_counts_top5_trimmed %>%
  filter(
    n_obs >= min_observations_per_aez_regression,
    n_unique_delta_defor_ha >= 2
  ) %>%
  pull(AEZ) %>%
  as.character()

regression_data_aez_top5_trimmed <- regression_data_top5_trimmed_aez %>%
  filter(as.character(AEZ) %in% eligible_aez_top5_trimmed)

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
  models_aez_top5_trimmed_ols <- fit_ols_by_aez(regression_data_aez_top5_trimmed)
}

# -----------------------------------------------------
# Save model objects
# -----------------------------------------------------

save_model_rds(
  models_aez_raw_ols,
  file.path(models_dir, "models_aez_raw_ols.rds")
)

save_model_rds(
  models_aez_iqr_trimmed_ols,
  file.path(models_dir, "models_aez_iqr_trimmed_ols.rds")
)

save_model_rds(
  models_aez_top5_trimmed_ols,
  file.path(models_dir, "models_aez_top5_trimmed_ols.rds")
)

# ------------------------------
# Export regression tables
# ------------------------------
if (length(models_aez_raw_ols) > 0) {
  save_modelsummary_table(
    models_aez_raw_ols,
    path = file.path(tables_dir, "table_aez_raw_ols.html"),
    title = "AEZ-Specific Raw OLS: Delta OV on Delta Deforestation"
  )
}

if (length(models_aez_iqr_trimmed_ols) > 0) {
  save_modelsummary_table(
    models_aez_iqr_trimmed_ols,
    path = file.path(tables_dir, "table_aez_iqr_trimmed_ols.html"),
    title = "AEZ-Specific Raw OLS after Within-AEZ IQR Trimming"
  )
}

if (length(models_aez_top5_trimmed_ols) > 0) {
  save_modelsummary_table(
    models_aez_top5_trimmed_ols,
    path = file.path(tables_dir, "table_aez_top5_trimmed_ols.html"),
    title = "AEZ-Specific Raw OLS after Within-AEZ Top-5% Trimming"
  )
}

message("Finished 04_ols_regression.R")