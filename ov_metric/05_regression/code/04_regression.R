# =====================================================
# 04_ols_regression.R
# Fit group-specific regression variants for the current cluster run.
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

log_verbose("Regression sample rows: ", nrow(regression_data))
log_verbose("Unique groups in regression sample: ", n_distinct(regression_data$group_value))
log_verbose("Current run: ", current_run_label)

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
  defor_raw = list(
    regressor_col = "delta_defor_ha",
    variant_label = paste0(current_defor_label, " | raw ∆")
  ),
  defor_log1p = list(
    regressor_col = "log1p_delta_defor_ha",
    variant_label = paste0(current_defor_label, " | log1p ∆")
  ),
  defor_p90_trimmed = list(
    regressor_col = "delta_defor_ha",
    variant_label = paste0(current_defor_label, " | p90-trimmed ∆")
  )
)
transform_preparation_specs <- transform_preparation_specs[defor_transforms]

regression_data_by_transform <- purrr::map(
  defor_transforms,
  ~ if (identical(.x, "defor_p90_trimmed")) {
    p90_trim(regression_data)
  } else {
    regression_data
  }
) |>
  rlang::set_names(defor_transforms)

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

regression_model_label <- dplyr::case_when(
  identical(current_regression_model, "ols") ~ "OLS",
  identical(current_regression_model, "robust_linear") ~ "Robust Linear",
  identical(current_regression_model, "polynomial") ~ "Polynomial",
  identical(current_regression_model, "gam_spline") ~ "GAM/Spline",
  TRUE ~ current_regression_model
)

transform_model_specs <- purrr::imap(
  transform_preparation_specs,
  ~ list(
    prepared_transform = .y,
    fit_kind = current_regression_model,
    variant_label = .x$variant_label,
    model_filename = paste0(current_regression_model, ".rds"),
    table_filename = paste0(current_regression_model, ".html"),
    table_title = paste0(
      stringr::str_to_title(current_group_label),
      "-Specific ",
      regression_model_label,
      " | ",
      .x$variant_label
    ),
    plot_title = paste0(
      stringr::str_to_title(current_group_label),
      " | ",
      regression_model_label
    )
  )
)

models_by_transform <- purrr::imap(
  transform_model_specs,
  ~ {
    prepared_variant <- prepared_by_transform[[.x$prepared_transform]]

    if (identical(.x$fit_kind, "polynomial")) {
      fit_polynomial_variant(prepared_variant, .x$variant_label)
    } else if (identical(.x$fit_kind, "robust_linear")) {
      fit_robust_variant(prepared_variant, .x$variant_label)
    } else if (identical(.x$fit_kind, "gam_spline")) {
      fit_gam_spline_variant(prepared_variant, .x$variant_label)
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

if (isTRUE(write_model_tables)) {
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
}

log_verbose("Finished 04_ols_regression.R")
log_verbose("  current run: ", current_run_label)
