make_ov_over_time_figure <- function() {
  assert_file_exists(clean_sample_year_path, "Clean BioTIME assemblage-year OV table")
  ov_data <- readr::read_csv(clean_sample_year_path, show_col_types = FALSE)
  assert_has_cols(
    ov_data,
    c("time_series_id", "sample_year", "ov", "taxon_group", "AEZ"),
    "Clean BioTIME assemblage-year OV table"
  )

  ov_data <- ov_data |>
    dplyr::mutate(
      taxon_group = dplyr::if_else(
        is.na(taxon_group) | !nzchar(as.character(taxon_group)),
        "Unknown",
        as.character(taxon_group)
      ),
      AEZ = dplyr::if_else(
        is.na(AEZ) | !nzchar(as.character(AEZ)),
        "Unknown",
        as.character(AEZ)
      )
    ) |>
    dplyr::filter(
      is.finite(sample_year),
      is.finite(ov)
    )
  taxon_counts <- ov_data |>
    dplyr::count(taxon_group, name = "n_assemblage_years") |>
    dplyr::arrange(dplyr::desc(n_assemblage_years), taxon_group)
  ov_data$taxon_group <- factor(ov_data$taxon_group, levels = taxon_counts$taxon_group)
  ov_data <- set_aez_order(ov_data)

  yearly_summary <- ov_data |>
    dplyr::group_by(sample_year) |>
    dplyr::summarise(
      n_assemblage_years = dplyr::n(),
      ov_p25 = stats::quantile(ov, 0.25, na.rm = TRUE),
      ov_median = stats::median(ov, na.rm = TRUE),
      ov_p75 = stats::quantile(ov, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  ov_plot <- ggplot2::ggplot(
    ov_data,
    ggplot2::aes(x = sample_year, y = ov)
  ) +
    ggplot2::geom_point(alpha = 0.16, size = 0.65, color = "#2F6F7E") +
    ggplot2::geom_ribbon(
      data = yearly_summary,
      ggplot2::aes(x = sample_year, ymin = ov_p25, ymax = ov_p75),
      inherit.aes = FALSE,
      fill = "#D9B36C",
      alpha = 0.28
    ) +
    ggplot2::geom_line(
      data = yearly_summary,
      ggplot2::aes(x = sample_year, y = ov_median),
      inherit.aes = FALSE,
      linewidth = 1,
      color = "#B04A3A"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(min(ov_data$sample_year), max(ov_data$sample_year), by = 2)) +
    ggplot2::labs(
      title = "Composite OV Score Over Time",
      x = "Year",
      y = "Composite OV score"
    ) +
    base_theme()

  save_plot(ov_plot, "ov_over_time.png", width = 9, height = 5.5)

  taxon_yearly_summary <- ov_data |>
    dplyr::group_by(taxon_group, sample_year) |>
    dplyr::summarise(
      n_assemblage_years = dplyr::n(),
      ov_p25 = stats::quantile(ov, 0.25, na.rm = TRUE),
      ov_median = stats::median(ov, na.rm = TRUE),
      ov_p75 = stats::quantile(ov, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  taxon_facet_plot <- ggplot2::ggplot(
    ov_data,
    ggplot2::aes(x = sample_year, y = ov)
  ) +
    ggplot2::geom_point(alpha = 0.13, size = 0.45, color = "#2F6F7E") +
    ggplot2::geom_ribbon(
      data = taxon_yearly_summary,
      ggplot2::aes(x = sample_year, ymin = ov_p25, ymax = ov_p75),
      inherit.aes = FALSE,
      fill = "#D9B36C",
      alpha = 0.28
    ) +
    ggplot2::geom_line(
      data = taxon_yearly_summary,
      ggplot2::aes(x = sample_year, y = ov_median),
      inherit.aes = FALSE,
      linewidth = 0.8,
      color = "#B04A3A"
    ) +
    ggplot2::facet_wrap(~taxon_group) +
    ggplot2::scale_x_continuous(breaks = seq(min(ov_data$sample_year), max(ov_data$sample_year), by = 4)) +
    ggplot2::coord_cartesian(ylim = range(ov_data$ov, na.rm = TRUE)) +
    ggplot2::labs(
      title = "Composite OV Score Over Time By Taxon",
      x = "Year",
      y = "Composite OV score"
    ) +
    base_theme()

  save_plot(taxon_facet_plot, "ov_over_time_by_taxon.png", width = 12, height = 8)

  aez_yearly_summary <- ov_data |>
    dplyr::group_by(AEZ, sample_year) |>
    dplyr::summarise(
      n_assemblage_years = dplyr::n(),
      ov_p25 = stats::quantile(ov, 0.25, na.rm = TRUE),
      ov_median = stats::median(ov, na.rm = TRUE),
      ov_p75 = stats::quantile(ov, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  aez_panel_plot <- ggplot2::ggplot(
    ov_data,
    ggplot2::aes(x = sample_year, y = ov)
  ) +
    ggplot2::geom_point(alpha = 0.13, size = 0.45, color = "#2F6F7E") +
    ggplot2::geom_ribbon(
      data = aez_yearly_summary,
      ggplot2::aes(x = sample_year, ymin = ov_p25, ymax = ov_p75),
      inherit.aes = FALSE,
      fill = "#D9B36C",
      alpha = 0.28
    ) +
    ggplot2::geom_line(
      data = aez_yearly_summary,
      ggplot2::aes(x = sample_year, y = ov_median),
      inherit.aes = FALSE,
      linewidth = 0.8,
      color = "#B04A3A"
    ) +
    ggplot2::facet_wrap(~AEZ) +
    ggplot2::scale_x_continuous(breaks = seq(min(ov_data$sample_year), max(ov_data$sample_year), by = 4)) +
    ggplot2::coord_cartesian(ylim = range(ov_data$ov, na.rm = TRUE)) +
    ggplot2::labs(
      title = "Composite OV Score Over Time By AEZ",
      x = "Year",
      y = "Composite OV score"
    ) +
    base_theme()

  save_plot(aez_panel_plot, "ov_over_time_by_aez.png", width = 12, height = 9)

  for (taxon in levels(ov_data$taxon_group)) {
    taxon_data <- ov_data |>
      dplyr::filter(taxon_group == taxon)
    taxon_summary <- taxon_yearly_summary |>
      dplyr::filter(taxon_group == taxon)

    taxon_plot <- ggplot2::ggplot(
      taxon_data,
      ggplot2::aes(x = sample_year, y = ov)
    ) +
      ggplot2::geom_point(alpha = 0.18, size = 0.75, color = "#2F6F7E") +
      ggplot2::geom_ribbon(
        data = taxon_summary,
        ggplot2::aes(x = sample_year, ymin = ov_p25, ymax = ov_p75),
        inherit.aes = FALSE,
        fill = "#D9B36C",
        alpha = 0.28
      ) +
      ggplot2::geom_line(
        data = taxon_summary,
        ggplot2::aes(x = sample_year, y = ov_median),
        inherit.aes = FALSE,
        linewidth = 1,
        color = "#B04A3A"
      ) +
      ggplot2::scale_x_continuous(breaks = seq(min(ov_data$sample_year), max(ov_data$sample_year), by = 2)) +
      ggplot2::coord_cartesian(ylim = range(ov_data$ov, na.rm = TRUE)) +
      ggplot2::labs(
        title = paste("Composite OV Score Over Time:", taxon),
        x = "Year",
        y = "Composite OV score"
      ) +
      base_theme()

    save_plot(
      taxon_plot,
      paste0("ov_over_time_", sanitize_filename(taxon), ".png"),
      width = 9,
      height = 5.5,
      subdir = "ov_over_time_by_taxon"
    )
  }
}

make_ov_over_time_figure()

make_variant_figures <- function(variant_key, variant_label) {
  variant_dir <- file.path(regression_output_dir, variant_key)
  regression_dataset_path <- file.path(variant_dir, "biotime_regression_dataset.csv")
  prediction_dir <- file.path(variant_dir, "predictions")
  table_dir <- file.path(variant_dir, "tables")

  global_prediction_path <- file.path(prediction_dir, "global_linear_predictions.csv")
  mixed_aez_prediction_path <- file.path(prediction_dir, "mixed_aez_predictions.csv")
  mixed_taxon_prediction_path <- file.path(prediction_dir, "mixed_taxon_predictions.csv")
  gam_aez_prediction_path <- file.path(prediction_dir, "gam_aez_predictions.csv")
  r2_annotation_path <- file.path(table_dir, "plot_r2_annotations.csv")

  assert_file_exists(regression_dataset_path, paste(variant_label, "regression dataset"))
  assert_file_exists(global_prediction_path, paste(variant_label, "global linear predictions"))
  assert_file_exists(gam_aez_prediction_path, paste(variant_label, "GAM AEZ predictions"))
  assert_file_exists(r2_annotation_path, paste(variant_label, "R2 annotations"))

  regression_data <- readr::read_csv(regression_dataset_path, show_col_types = FALSE) |>
    set_aez_order()
  global_predictions <- readr::read_csv(global_prediction_path, show_col_types = FALSE)
  gam_predictions <- readr::read_csv(gam_aez_prediction_path, show_col_types = FALSE) |>
    set_aez_order()
  r2_annotations <- readr::read_csv(r2_annotation_path, show_col_types = FALSE)

  limits <- plot_limits(regression_data)
  label_position <- annotation_position(limits)

  annotation_for <- function(model_name, group_type = "global", facet_col = NULL, prefix = "R^2") {
    ann <- r2_annotations |>
      dplyr::filter(model == model_name, group_type == group_type) |>
      dplyr::mutate(label = format_r2_label(prediction_r2, prefix = prefix))

    ann <- dplyr::bind_cols(ann, label_position[rep(1, nrow(ann)), ])

    if (!is.null(facet_col)) {
      ann[[facet_col]] <- ann$group
      if (identical(facet_col, "AEZ")) {
        ann[[facet_col]] <- factor(ann[[facet_col]], levels = levels(regression_data$AEZ))
      }
    }
    ann
  }

  global_annotation <- annotation_for("Global Linear")
  global_plot <- ggplot2::ggplot(
    regression_data,
    ggplot2::aes(x = .data[[predictor_col]], y = .data[[response_col]])
  ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey55") +
    ggplot2::geom_point(alpha = 0.35, size = 1.2, color = "#2F6F7E") +
    ggplot2::geom_line(
      data = global_predictions,
      ggplot2::aes(y = prediction),
      linewidth = 1,
      color = "#B04A3A"
    ) +
    r2_annotation_layer(global_annotation) +
    ggplot2::coord_cartesian(xlim = limits$x, ylim = limits$y) +
    ggplot2::labs(
      title = paste("Global Linear model:", variant_label),
      x = x_label,
      y = y_label
    ) +
    base_theme()

  save_plot(global_plot, "global_linear_model.png", width = 8, height = 5, subdir = variant_key)
  if (identical(variant_key, "all_delta_ov")) {
    save_plot(global_plot, "global_linear_model.png", width = 8, height = 5)
  }

  if (file.exists(mixed_aez_prediction_path)) {
    mixed_aez_predictions <- readr::read_csv(mixed_aez_prediction_path, show_col_types = FALSE) |>
      set_aez_order()
    mixed_aez_annotation <- annotation_for("Mixed AEZ", "AEZ", "AEZ", prefix = "pred R^2")
    mixed_aez_plot <- ggplot2::ggplot(
      regression_data,
      ggplot2::aes(x = .data[[predictor_col]], y = .data[[response_col]])
    ) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.25, color = "grey60") +
      ggplot2::geom_point(alpha = 0.25, size = 0.9, color = "#4D6A3D") +
      ggplot2::geom_line(
        data = mixed_aez_predictions,
        ggplot2::aes(y = prediction),
        linewidth = 0.8,
        color = "#B04A3A"
      ) +
      r2_annotation_layer(mixed_aez_annotation) +
      ggplot2::facet_wrap(~AEZ) +
      ggplot2::coord_cartesian(xlim = limits$x, ylim = limits$y) +
      ggplot2::labs(
        title = paste("Mixed AEZ model:", variant_label),
        x = x_label,
        y = y_label
      ) +
      base_theme()

    save_plot(mixed_aez_plot, "mixed_aez_model_facets.png", width = 12, height = 8, subdir = variant_key)
    if (identical(variant_key, "all_delta_ov")) {
      save_plot(mixed_aez_plot, "mixed_aez_model_facets.png", width = 12, height = 8)
    }
  }

  if (file.exists(mixed_taxon_prediction_path)) {
    mixed_taxon_predictions <- readr::read_csv(mixed_taxon_prediction_path, show_col_types = FALSE)
    mixed_taxon_annotation <- annotation_for("Mixed Taxon", "taxon_group", "taxon_group", prefix = "pred R^2")
    mixed_taxon_plot <- ggplot2::ggplot(
      regression_data,
      ggplot2::aes(x = .data[[predictor_col]], y = .data[[response_col]])
    ) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.25, color = "grey60") +
      ggplot2::geom_point(alpha = 0.25, size = 0.9, color = "#5F527A") +
      ggplot2::geom_line(
        data = mixed_taxon_predictions,
        ggplot2::aes(y = prediction),
        linewidth = 0.8,
        color = "#C27D38"
      ) +
      r2_annotation_layer(mixed_taxon_annotation) +
      ggplot2::facet_wrap(~taxon_group) +
      ggplot2::coord_cartesian(xlim = limits$x, ylim = limits$y) +
      ggplot2::labs(
        title = paste("Mixed Taxon model:", variant_label),
        x = x_label,
        y = y_label
      ) +
      base_theme()

    save_plot(mixed_taxon_plot, "mixed_taxon_model_facets.png", width = 11, height = 7, subdir = variant_key)
    if (identical(variant_key, "all_delta_ov")) {
      save_plot(mixed_taxon_plot, "mixed_taxon_model_facets.png", width = 11, height = 7)
    }
  }

  gam_annotation <- annotation_for("GAM AEZ", "AEZ", "AEZ")
  gam_plot <- ggplot2::ggplot(
    regression_data,
    ggplot2::aes(x = .data[[predictor_col]], y = .data[[response_col]])
  ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.25, color = "grey60") +
    ggplot2::geom_point(alpha = 0.25, size = 0.9, color = "#2F6F7E") +
    ggplot2::geom_line(
      data = gam_predictions,
      ggplot2::aes(y = prediction, linetype = model_type),
      linewidth = 0.8,
      color = "#B04A3A"
    ) +
    r2_annotation_layer(gam_annotation) +
    ggplot2::facet_wrap(~AEZ) +
    ggplot2::coord_cartesian(xlim = limits$x, ylim = limits$y) +
    ggplot2::labs(
      title = paste("GAM/Spline AEZ models:", variant_label),
      x = x_label,
      y = y_label,
      linetype = "Fit"
    ) +
    base_theme()

  save_plot(gam_plot, "gam_aez_model_facets.png", width = 12, height = 8, subdir = variant_key)
  if (identical(variant_key, "all_delta_ov")) {
    save_plot(gam_plot, "gam_aez_model_facets.png", width = 12, height = 8)
  }
}

purrr::pwalk(
  regression_variants,
  ~ make_variant_figures(..1, ..2)
)

message("Visualization complete.")
