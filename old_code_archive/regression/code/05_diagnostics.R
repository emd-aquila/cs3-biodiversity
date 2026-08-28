# =====================================================
# 05_diagnostics.R
# Create summaries and plots for the current regression run.
# Outputs are written within treatment-family folders.
# =====================================================

# -----------------------------------------------------
# Preconditions
# -----------------------------------------------------

required_objects <- c(
  "regression_data",
  "regression_data_by_transform",
  "prepared_by_transform",
  "models_by_transform",
  "transform_model_specs",
  "output_dirs_by_transform"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    paste0(
      "05_diagnostics.R is missing required objects: ",
      paste(missing_objects, collapse = ", ")
    ),
    call. = FALSE
  )
}

run_labels_by_transform <- build_run_labels_by_transform(defor_transforms)
plot_labels_by_transform <- build_compact_run_labels_by_transform(defor_transforms)

# -----------------------------------------------------
# Family-specific summaries
# -----------------------------------------------------

summary_by_transform <- purrr::imap(
  transform_model_specs,
  ~ {
    prepared_variant <- prepared_by_transform[[.x$prepared_transform]]
    model_label <- .x$fit_kind

    build_group_fit_summary(
      models_by_transform[[.y]],
      prepared_variant$regression_data,
      model_label,
      prepared_variant$regressor_col
    )
  }
)

purrr::iwalk(
  summary_by_transform,
  ~ write_fit_summary(
    .x,
    output_dirs_by_transform[[.y]],
    .y,
    run_labels_by_transform[[.y]],
    "fit_stats.csv"
  )
)

# -----------------------------------------------------
# Main fitted scatterplots
# -----------------------------------------------------

if (isTRUE(write_regression_plots)) {
  purrr::iwalk(
    transform_model_specs,
    ~ {
      models_list <- models_by_transform[[.y]]

      if (length(models_list) == 0) {
        return(invisible(NULL))
      }

      prepared_variant <- prepared_by_transform[[.x$prepared_transform]]

      save_family_plot(
        build_family_plot_from_models(
          prepared_variant$regression_data,
          models_list,
          prepared_variant$regressor_col,
          .x$plot_title
        ),
        output_dirs_by_transform[[.y]],
        paste0(.x$fit_kind, ".png"),
        run_label = plot_labels_by_transform[[.y]]
      )
    }
  )
}

# -----------------------------------------------------
# Distribution plots
# -----------------------------------------------------

regressor_hist_specs <- list(
  defor_raw = list(
    data = regression_data_by_transform[["defor_raw"]],
    x_col = "delta_defor_ha",
    title = "Distribution of raw ∆ deforestation"
  ),
  defor_log1p = list(
    data = regression_data_by_transform[["defor_log1p"]],
    x_col = "log1p_delta_defor_ha",
    title = "Distribution of log1p ∆ deforestation"
  ),
  defor_p90_trimmed = list(
    data = regression_data_by_transform[["defor_p90_trimmed"]],
    x_col = "delta_defor_ha",
    title = "Distribution of p90-trimmed ∆ deforestation"
  )
)
regressor_hist_specs <- regressor_hist_specs[defor_transforms]

if (isTRUE(write_histogram_plots)) {
  purrr::iwalk(
    regressor_hist_specs,
    ~ save_family_plot(
      ggplot(.x$data, aes(x = .data[[.x$x_col]])) +
        geom_histogram(bins = 40) +
        labs(
          x = .x$x_col,
          y = "Count",
          title = .x$title
        ) +
        theme_minimal(),
      output_dirs_by_transform[[.y]],
      "hist_regressor.png",
      run_label = plot_labels_by_transform[[.y]],
      width = 8,
      height = 6
    )
  )

  delta_ov_hist_plot <- ggplot(regression_data, aes(x = delta_ov)) +
    geom_histogram(bins = 40) +
    labs(
      x = get_delta_ov_axis_label(),
      y = "Count",
      title = paste("Distribution of", get_delta_ov_axis_label())
    ) +
    theme_minimal()

  for (transform_name in names(output_dirs_by_transform)) {
    save_family_plot(
      delta_ov_hist_plot,
      output_dirs_by_transform[[transform_name]],
      "hist_delta_ov.png",
      run_label = build_compact_run_label(defor_transform = transform_name),
      width = 8,
      height = 6
    )
  }
}

log_verbose("Finished 05_diagnostics.R")
log_verbose("  current run: ", current_run_label)
