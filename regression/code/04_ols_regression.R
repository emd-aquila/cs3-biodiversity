# =====================================================
# 04_ols_regression.R
# Fit AEZ-specific regression variants for the current cluster run.
# Outputs are organized by deforestation-treatment family.
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

assert_has_cols(
  regression_data,
  required_regression_cols,
  "regression_data"
)

message("Regression sample rows: ", nrow(regression_data))
message("Unique AEZs in regression sample: ", n_distinct(regression_data$AEZ))
message("Current run: ", current_run_label)

# -----------------------------------------------------
# Output directories by transform
# -----------------------------------------------------

output_dirs_by_transform <- build_output_dirs_by_transform(defor_transforms)
run_labels_by_transform <- build_run_labels_by_transform(defor_transforms)
current_defor_label <- defor_approach_specs[[current_defor_approach]]$label

# -----------------------------------------------------
# Build regression datasets for each transform
# -----------------------------------------------------

transform_preparation_specs <- list(
  raw = list(
    regressor_col = "delta_defor_ha",
    variant_label = paste0(current_defor_label, " | raw")
  ),
  log1p = list(
    regressor_col = "log1p_delta_defor_ha",
    variant_label = paste0(current_defor_label, " | log1p")
  ),
  p90 = list(
    regressor_col = "delta_defor_ha",
    variant_label = paste0(current_defor_label, " | p90 trim")
  ),
  winsorized = list(
    regressor_col = "delta_defor_ha",
    variant_label = paste0(current_defor_label, " | winsorized")
  )
)

regression_data_by_transform <- list(
  raw = regression_data,
  log1p = regression_data,
  p90 = p90_trim(regression_data, threshold = winsorization_threshold),
  winsorized = winsorize(regression_data, threshold = winsorization_threshold)
)

prepared_by_transform <- purrr::imap(
  transform_preparation_specs,
  ~ prepare_regression_variant(
    regression_data_by_transform[[.y]],
    .x$regressor_col,
    .x$variant_label
  )
)

# -----------------------------------------------------
# Fit models
# -----------------------------------------------------

transform_model_specs <- list(
  raw = list(
    prepared_transform = "raw",
    fit_kind = "ols",
    variant_label = paste0(current_defor_label, " | raw"),
    model_filename = "ols.rds",
    table_filename = "ols.html",
    table_title = paste0("AEZ-Specific OLS | ", current_defor_label, " | raw"),
    plot_title = paste0("AEZ-Specific OLS | ", current_defor_label, " | raw")
  ),
  rlm = list(
    prepared_transform = "raw",
    fit_kind = "robust",
    variant_label = paste0(current_defor_label, " | rlm"),
    model_filename = "rlm.rds",
    table_filename = "rlm.html",
    table_title = paste0("AEZ-Specific Robust Regression | ", current_defor_label, " | rlm"),
    plot_title = paste0("AEZ-Specific Robust Regression | ", current_defor_label, " | rlm")
  ),
  log1p = list(
    prepared_transform = "log1p",
    fit_kind = "ols",
    variant_label = paste0(current_defor_label, " | log1p"),
    model_filename = "ols.rds",
    table_filename = "ols.html",
    table_title = paste0("AEZ-Specific OLS | ", current_defor_label, " | log1p"),
    plot_title = paste0("AEZ-Specific OLS | ", current_defor_label, " | log1p")
  ),
  p90 = list(
    prepared_transform = "p90",
    fit_kind = "ols",
    variant_label = paste0(current_defor_label, " | p90 trim"),
    model_filename = "ols.rds",
    table_filename = "ols.html",
    table_title = paste0("AEZ-Specific OLS | ", current_defor_label, " | p90 trim"),
    plot_title = paste0("AEZ-Specific OLS | ", current_defor_label, " | p90 trim")
  ),
  winsorized = list(
    prepared_transform = "winsorized",
    fit_kind = "ols",
    variant_label = paste0(current_defor_label, " | winsorized"),
    model_filename = "ols.rds",
    table_filename = "ols.html",
    table_title = paste0("AEZ-Specific OLS | ", current_defor_label, " | winsorized"),
    plot_title = paste0("AEZ-Specific OLS | ", current_defor_label, " | winsorized")
  )
)

models_by_transform <- purrr::imap(
  transform_model_specs,
  ~ {
    prepared_variant <- prepared_by_transform[[.x$prepared_transform]]

    if (identical(.x$fit_kind, "robust")) {
      fit_robust_variant(prepared_variant, .x$variant_label)
    } else {
      fit_ols_variant(prepared_variant, .x$variant_label)
    }
  }
)

# -----------------------------------------------------
# Save model objects
# -----------------------------------------------------

purrr::iwalk(
  transform_model_specs,
  ~ {
    current_defor_transform <<- .y
    save_model_rds(
      models_by_transform[[.y]],
      file.path(output_dirs_by_transform[[.y]]$family_root, .x$model_filename),
      run_label = run_labels_by_transform[[.y]],
      defor_approach = current_defor_approach,
      defor_transform = .y
    )
  }
)

# -----------------------------------------------------
# Export regression tables
# -----------------------------------------------------

purrr::iwalk(
  transform_model_specs,
  ~ {
    models_list <- models_by_transform[[.y]]

    if (length(models_list) == 0) {
      return(invisible(NULL))
    }

    save_modelsummary_table(
      models_list,
      path = file.path(output_dirs_by_transform[[.y]]$family_root, .x$table_filename),
      title = .x$table_title,
      run_label = run_labels_by_transform[[.y]]
    )
  }
)

message("Finished 04_ols_regression.R")
message("  current run: ", current_run_label)
