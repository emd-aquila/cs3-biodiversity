# =====================================================
# Helper functions for regression pipeline
# =====================================================

log_verbose <- function(...) {
  if (isTRUE(verbose_console_output)) {
    message(...)
  }
}

cluster_deltas_cache <- new.env(parent = emptyenv())

read_cluster_deltas_cached <- function(path) {
  assert_exists(path)
  cache_key <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (!exists(cache_key, envir = cluster_deltas_cache, inherits = FALSE)) {
    cluster_deltas <- readr::read_csv(path, show_col_types = FALSE) %>%
      normalize_cluster_deltas_schema()

    assign(cache_key, cluster_deltas, envir = cluster_deltas_cache)
    log_verbose("Cached cluster_deltas: ", path)
  }

  get(cache_key, envir = cluster_deltas_cache, inherits = FALSE)
}

# ------------------------------
# Build run identifiers and directories used for a given regression
# ------------------------------

# Check that the selected OV approach is valid.
validate_delta_ov_approach <- function(delta_ov_approach) {
  valid_modes <- delta_ov_approaches
  
  if (!delta_ov_approach %in% valid_modes) {
    stop(
      "Unknown delta_ov_approach: ",
      delta_ov_approach,
      ". Valid modes are: ",
      paste(valid_modes, collapse = ", "),
      call. = FALSE
    )
  }
}

validate_cluster_database <- function(cluster_database) {
  if (!cluster_database %in% cluster_databases) {
    stop(
      "Unknown cluster_database: ",
      cluster_database,
      ". Valid databases are: ",
      paste(cluster_databases, collapse = ", "),
      call. = FALSE
    )
  }
}

get_annualization_mode_spec <- function(annualization_mode = current_annualization_mode) {
  validate_annualization_mode(annualization_mode)
  annualization_mode_specs[[annualization_mode]]
}

get_delta_ov_approach_spec <- function(delta_ov_approach = current_delta_ov_approach) {
  validate_delta_ov_approach(delta_ov_approach)
  delta_ov_approach_specs[[delta_ov_approach]]
}

get_defor_transform_spec <- function(defor_transform) {
  if (is.null(defor_transform)) {
    return(NULL)
  }

  if (!defor_transform %in% defor_transforms) {
    stop(
      "Unknown defor_transform: ",
      defor_transform,
      ". Valid transforms are: ",
      paste(defor_transforms, collapse = ", "),
      call. = FALSE
    )
  }

  defor_transform_specs[[defor_transform]]
}

output_hierarchy_axis_order <- c(
  "cluster_database",
  "cluster_method",
  "cluster_radius",
  "buffer",
  "regression_group",
  "annualization_mode",
  "defor_bin",
  "delta_ov_approach",
  "single_tile_collapse_mode",
  "ov_calculation_method",
  "ov_change_mode",
  "starting_ov_adjustment",
  "defor_tile_sum",
  "regression_model",
  "defor_transform"
)

get_output_hierarchy_axis_counts <- function() {
  c(
    cluster_database = length(cluster_databases),
    cluster_method = length(cluster_methods),
    cluster_radius = length(cluster_radii),
    buffer = length(buffers),
    regression_group = length(regression_groups),
    annualization_mode = length(annualization_modes),
    defor_bin = length(defor_bins),
    delta_ov_approach = length(delta_ov_approaches),
    single_tile_collapse_mode = length(single_tile_collapse_modes),
    ov_calculation_method = length(ov_calculation_methods),
    ov_change_mode = length(ov_change_modes),
    starting_ov_adjustment = length(starting_ov_adjustment_modes),
    defor_tile_sum = length(defor_tile_sum_methods),
    regression_model = length(regression_models),
    defor_transform = length(defor_transforms)
  )
}

include_output_hierarchy_axis <- function(axis_name) {
  axis_counts <- get_output_hierarchy_axis_counts()

  if (!axis_name %in% names(axis_counts)) {
    stop("Unknown output hierarchy axis: ", axis_name, call. = FALSE)
  }

  axis_counts[[axis_name]] > 1 ||
    axis_name %in% c("cluster_method", "starting_ov_adjustment")
}

build_conditional_output_path <- function(path_dirs,
                                          through_axis,
                                          base_dir = output_dir) {
  if (!through_axis %in% output_hierarchy_axis_order) {
    stop("Unknown output hierarchy endpoint: ", through_axis, call. = FALSE)
  }

  endpoint <- match(through_axis, output_hierarchy_axis_order)
  candidate_axes <- output_hierarchy_axis_order[seq_len(endpoint)]
  included_axes <- candidate_axes[
    vapply(candidate_axes, include_output_hierarchy_axis, logical(1))
  ]
  path_parts <- unname(unlist(path_dirs[included_axes], use.names = FALSE))
  path_parts <- path_parts[!is.na(path_parts) & nzchar(path_parts)]

  if (length(path_parts) == 0) {
    return(base_dir)
  }

  do.call(file.path, as.list(c(base_dir, path_parts)))
}

format_readme_values <- function(values) {
  paste(values, collapse = ", ")
}

format_readme_dirs <- function(values) {
  paste0("`", paste(values, collapse = "`, `"), "`")
}

get_output_hierarchy_axis_metadata <- function() {
  list(
    list(
      axis = "cluster_database",
      config = "cluster_databases",
      description = "Underlying biodiversity database",
      selected = cluster_databases,
      folders = cluster_databases
    ),
    list(
      axis = "cluster_method",
      config = "cluster_methods",
      description = "Clustering method",
      selected = cluster_methods,
      folders = cluster_methods
    ),
    list(
      axis = "cluster_radius",
      config = "cluster_radii",
      description = "Clustering radius",
      selected = paste0(sprintf("%.1f", cluster_radii), " km"),
      folders = paste0("radius_", sprintf("%.1fkm", cluster_radii))
    ),
    list(
      axis = "buffer",
      config = "buffers",
      description = "Cluster buffer size",
      selected = paste0(buffers, " km"),
      folders = paste0("buf_", buffers, "km")
    ),
    list(
      axis = "regression_group",
      config = "regression_groups",
      description = "Grouping used for group-specific regressions",
      selected = regression_groups,
      folders = purrr::map_chr(regression_groups, ~ regression_group_specs[[.x]]$output_dir)
    ),
    list(
      axis = "annualization_mode",
      config = "annualization_modes",
      description = "Whether delta OV and deforestation are raw totals or annualized by year_gap",
      selected = annualization_modes,
      folders = purrr::map_chr(annualization_modes, ~ annualization_mode_specs[[.x]]$output_dir)
    ),
    list(
      axis = "defor_bin",
      config = "defor_bins",
      description = "Deforestation exposure window",
      selected = defor_bins,
      folders = purrr::map_chr(defor_bins, ~ defor_bin_specs[[.x]]$output_dir)
    ),
    list(
      axis = "delta_ov_approach",
      config = "delta_ov_approaches",
      description = "How delta OV is calculated within clusters",
      selected = delta_ov_approaches,
      folders = purrr::map_chr(delta_ov_approaches, ~ delta_ov_approach_specs[[.x]]$output_dir)
    ),
    list(
      axis = "single_tile_collapse_mode",
      config = "single_tile_collapse_modes",
      description = "Whether same-single-tile clusters are collapsed",
      selected = single_tile_collapse_modes,
      folders = purrr::map_chr(single_tile_collapse_modes, ~ single_tile_collapse_mode_specs[[.x]]$output_dir)
    ),
    list(
      axis = "ov_calculation_method",
      config = "ov_calculation_methods",
      description = "OV calculation method",
      selected = ov_calculation_methods,
      folders = purrr::map_chr(ov_calculation_methods, ~ ov_calculation_specs[[.x]]$output_dir)
    ),
    list(
      axis = "ov_change_mode",
      config = "ov_change_modes",
      description = "How low starting OV and percent-change outcomes are handled",
      selected = ov_change_modes,
      folders = purrr::map_chr(ov_change_modes, ~ ov_change_mode_specs[[.x]]$output_dir)
    ),
    list(
      axis = "starting_ov_adjustment",
      config = "starting_ov_adjustment_modes",
      description = "How starting OV enters the regression formula",
      selected = starting_ov_adjustment_modes,
      folders = purrr::map_chr(starting_ov_adjustment_modes, ~ starting_ov_adjustment_specs[[.x]]$output_dir)
    ),
    list(
      axis = "defor_tile_sum",
      config = "defor_tile_sum_methods",
      description = "How tagged-tile deforestation is summarized for each cluster",
      selected = defor_tile_sum_methods,
      folders = purrr::map_chr(defor_tile_sum_methods, ~ defor_tile_sum_specs[[.x]]$output_dir)
    ),
    list(
      axis = "regression_model",
      config = "regression_models",
      description = "Regression model family",
      selected = regression_models,
      folders = regression_models
    ),
    list(
      axis = "defor_transform",
      config = "defor_transforms",
      description = "Deforestation regressor transformation",
      selected = defor_transforms,
      folders = purrr::map_chr(defor_transforms, ~ defor_transform_specs[[.x]]$output_dir)
    )
  )
}

write_output_readme <- function(path = file.path(output_dir, "README.md")) {
  axis_metadata <- get_output_hierarchy_axis_metadata()
  included_axes <- axis_metadata[
    vapply(axis_metadata, function(axis) include_output_hierarchy_axis(axis$axis), logical(1))
  ]
  fixed_axes <- axis_metadata[
    !vapply(axis_metadata, function(axis) include_output_hierarchy_axis(axis$axis), logical(1))
  ]

  included_lines <- if (length(included_axes) > 0) {
    purrr::imap_chr(
      included_axes,
      ~ paste0(
        .y,
        ". ",
        .x$description,
        " (`",
        .x$axis,
        "`): ",
        format_readme_dirs(.x$folders)
      )
    )
  } else {
    "No hierarchy axes have more than one selected option, so outputs are written directly under the output root."
  }

  fixed_lines <- if (length(fixed_axes) > 0) {
    purrr::map_chr(
      fixed_axes,
      ~ paste0(
        "- ",
        .x$description,
        " (`",
        .x$config,
        "`): ",
        format_readme_values(.x$selected),
        " [no folder level]"
      )
    )
  } else {
    "- None. Every configured hierarchy axis has more than one selected option."
  }

  selected_lines <- purrr::map_chr(
    axis_metadata,
    ~ paste0(
      "- ",
      .x$description,
      " (`",
      .x$config,
      "`): ",
      format_readme_values(.x$selected),
      " -> folders ",
      format_readme_dirs(.x$folders)
    )
  )

  threshold_lines <- purrr::imap_chr(
    ov_thresholds,
    ~ paste0("- ", .y, ": ", .x)
  )

  lines <- c(
    "# Regression Output README",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("Output root: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE)),
    "",
    "This directory contains regression outputs for the options selected in `05_regression/code/01_config.R` at run time.",
    "Folder levels are created for hierarchy axes with more than one selected option. `starting_ov_adjustment_modes` is always created as a folder to prevent adjusted and unadjusted fits from mixing.",
    "",
    "## Folder Hierarchy Created For This Run",
    included_lines,
    "",
    "## Fixed Choices Saved Only In This README",
    fixed_lines,
    "",
    "## Selected Options",
    selected_lines,
    "",
    "## Starting OV Thresholds",
    threshold_lines,
    "",
    "## Output Toggles",
    paste0("- Model tables: ", write_model_tables),
    paste0("- Regression OLS plots: ", write_regression_plots),
    paste0("- Histogram plots: ", write_histogram_plots),
    paste0("- Master output report: ", build_master_output_report),
    "",
    "## Notes",
    "- `raw_delta` uses raw delta OV and raw deforestation totals.",
    "- `annualized_delta` divides delta OV and deforestation by `year_gap`.",
    "- `baseline_deforestation` uses all deforestation between the first and last delta-OV years, inclusive.",
    "- `cutoff_defor` preserves the previous cutoff behavior, removing the final two exposure years when the interval permits.",
    "- `lagged_deforestation` shifts the full deforestation exposure interval back one year and drops intervals without complete prior-year coverage.",
    "- Thresholded OV-change modes remove rows where starting OV is below the configured threshold for that OV calculation method.",
    "- Percent OV-change modes replace linear delta OV with `100 * delta_ov / starting_ov` after the selected raw or annualized delta is chosen.",
    "- Starting-OV adjustment modes use within-group centered starting OV. Interaction-mode deforestation slopes are evaluated at average starting OV for the group."
  )

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
  path
}

is_annualized_delta_mode <- function(annualization_mode = current_annualization_mode) {
  identical(get_annualization_mode_spec(annualization_mode)$source_mode, "annualized")
}

validate_single_tile_collapse_mode <- function(single_tile_collapse_mode) {
  valid_modes <- names(single_tile_collapse_mode_specs)

  if (!single_tile_collapse_mode %in% valid_modes) {
    stop(
      "Unknown single_tile_collapse_mode: ",
      single_tile_collapse_mode,
      ". Valid modes are: ",
      paste(valid_modes, collapse = ", "),
      call. = FALSE
    )
  }
}

validate_ov_change_mode <- function(ov_change_mode) {
  valid_modes <- names(ov_change_mode_specs)

  if (!ov_change_mode %in% valid_modes) {
    stop(
      "Unknown ov_change_mode: ",
      ov_change_mode,
      ". Valid modes are: ",
      paste(valid_modes, collapse = ", "),
      call. = FALSE
    )
  }
}

validate_starting_ov_adjustment_mode <- function(starting_ov_adjustment_mode) {
  valid_modes <- names(starting_ov_adjustment_specs)

  if (!starting_ov_adjustment_mode %in% valid_modes) {
    stop(
      "Unknown starting_ov_adjustment_mode: ",
      starting_ov_adjustment_mode,
      ". Valid modes are: ",
      paste(valid_modes, collapse = ", "),
      call. = FALSE
    )
  }
}

# Check that the selected OV calculation method is valid.
validate_ov_calculation_method <- function(ov_calculation_method) {
  if (!ov_calculation_method %in% ov_calculation_methods) {
    stop(
      "Unknown ov_calculation_method: ",
      ov_calculation_method,
      ". Valid methods are: ",
      paste(ov_calculation_methods, collapse = ", "),
      call. = FALSE
    )
  }
}

# Check that the selected deforestation approach is valid.
validate_defor_approach <- function(defor_approach) {
  valid_approaches <- names(defor_approach_specs)

  if (!defor_approach %in% valid_approaches) {
    stop(
      "Unknown defor_approach: ",
      defor_approach,
      ". Valid approaches are: ",
      paste(valid_approaches, collapse = ", "),
      call. = FALSE
    )
  }
}

# Check that the selected annualization mode is valid.
validate_annualization_mode <- function(annualization_mode) {
  if (!annualization_mode %in% annualization_modes) {
    stop(
      "Unknown annualization_mode: ",
      annualization_mode,
      ". Valid modes are: ",
      paste(annualization_modes, collapse = ", "),
      call. = FALSE
    )
  }
}

# Check that the selected regression model is valid.
validate_regression_model <- function(regression_model) {
  if (!regression_model %in% regression_models) {
    stop(
      "Unknown regression_model: ",
      regression_model,
      ". Valid models are: ",
      paste(regression_models, collapse = ", "),
      call. = FALSE
    )
  }
}

# Check that the selected regression group is valid.
validate_regression_group <- function(regression_group) {
  if (!regression_group %in% regression_groups) {
    stop(
      "Unknown regression_group: ",
      regression_group,
      ". Valid levels are: ",
      paste(regression_groups, collapse = ", "),
      call. = FALSE
    )
  }
}

# Check that the selected deforestation bin is valid.
validate_defor_bin <- function(defor_bin) {
  if (!defor_bin %in% defor_bins) {
    stop(
      "Unknown defor_bin: ",
      defor_bin,
      ". Valid modes are: ",
      paste(defor_bins, collapse = ", "),
      call. = FALSE
    )
  }
}

# Return the configured metadata for one deforestation approach.
get_defor_approach_spec <- function(defor_approach = current_defor_approach) {
  validate_defor_approach(defor_approach)
  defor_approach_specs[[defor_approach]]
}

get_ov_calculation_spec <- function(ov_calculation_method = current_ov_calculation_method) {
  validate_ov_calculation_method(ov_calculation_method)
  ov_calculation_specs[[ov_calculation_method]]
}

get_starting_ov_adjustment_spec <- function(starting_ov_adjustment_mode = current_starting_ov_adjustment_mode) {
  validate_starting_ov_adjustment_mode(starting_ov_adjustment_mode)
  starting_ov_adjustment_specs[[starting_ov_adjustment_mode]]
}

# Set whether regressions use raw deltas or annualized deltas.
set_annualization_mode <- function(annualization_mode) {
  validate_annualization_mode(annualization_mode)

  current_annualization_mode <<- annualization_mode

  invisible(
    list(
      current_annualization_mode = current_annualization_mode
    )
  )
}

# Set the regression model used for the current regression run.
set_regression_model <- function(regression_model) {
  validate_regression_model(regression_model)

  current_regression_model <<- regression_model

  invisible(
    list(
      current_regression_model = current_regression_model
    )
  )
}

# Set the regression group used for regressions and diagnostics.
set_regression_group <- function(regression_group) {
  validate_regression_group(regression_group)

  current_regression_group <<- regression_group
  current_group_col <<- regression_group_specs[[regression_group]]$group_col
  current_group_label <<- regression_group_specs[[regression_group]]$label

  invisible(
    list(
      current_regression_group = current_regression_group,
      current_group_col = current_group_col,
      current_group_label = current_group_label
    )
  )
}

# Set the active OV approach and point later code to the shared 04_deforestation_tile_tag output root.
set_delta_ov_approach <- function(delta_ov_approach) {
  validate_delta_ov_approach(delta_ov_approach)
  
  current_delta_ov_approach <<- delta_ov_approach
  analysis_output_dir_current <<- file.path(
    ov_metric_dir,
    "04_deforestation_tile_tag",
    "analysis",
    "output"
  )
  
  invisible(
    list(
      current_delta_ov_approach = current_delta_ov_approach,
      analysis_output_dir_current = analysis_output_dir_current
    )
  )
}

set_single_tile_collapse_mode <- function(single_tile_collapse_mode) {
  validate_single_tile_collapse_mode(single_tile_collapse_mode)

  current_single_tile_collapse_mode <<- single_tile_collapse_mode
  current_single_tile_collapse_label <<- single_tile_collapse_mode_specs[[single_tile_collapse_mode]]$label

  invisible(
    list(
      current_single_tile_collapse_mode = current_single_tile_collapse_mode,
      current_single_tile_collapse_label = current_single_tile_collapse_label
    )
  )
}

get_ov_threshold <- function(ov_change_mode = current_ov_change_mode,
                             ov_calculation_method = current_ov_calculation_method) {
  validate_ov_change_mode(ov_change_mode)
  validate_ov_calculation_method(ov_calculation_method)

  spec <- ov_change_mode_specs[[ov_change_mode]]

  if (!isTRUE(spec$apply_threshold)) {
    return(NA_real_)
  }

  threshold <- ov_thresholds[[ov_calculation_method]]

  if (is.null(threshold)) {
    threshold <- ov_thresholds[["default"]]
  }

  if (is.null(threshold) || is.na(threshold)) {
    stop(
      "No starting-OV threshold configured for ",
      ov_change_mode,
      " with ",
      ov_calculation_method,
      ".",
      call. = FALSE
    )
  }

  as.numeric(threshold)
}

ov_change_uses_percent <- function(ov_change_mode = current_ov_change_mode) {
  validate_ov_change_mode(ov_change_mode)
  isTRUE(ov_change_mode_specs[[ov_change_mode]]$use_percent_change)
}

ov_change_applies_threshold <- function(ov_change_mode = current_ov_change_mode) {
  validate_ov_change_mode(ov_change_mode)
  isTRUE(ov_change_mode_specs[[ov_change_mode]]$apply_threshold)
}

get_delta_ov_axis_label <- function(ov_change_mode = current_ov_change_mode) {
  if (ov_change_uses_percent(ov_change_mode)) {
    return("∆OV (% of starting OV)")
  }

  "∆OV"
}

set_ov_change_mode <- function(ov_change_mode) {
  validate_ov_change_mode(ov_change_mode)

  current_ov_change_mode <<- ov_change_mode
  current_ov_change_label <<- ov_change_mode_specs[[ov_change_mode]]$label
  current_ov_threshold <<- NA_real_

  if (!is.na(current_ov_calculation_method)) {
    current_ov_threshold <<- get_ov_threshold(
      ov_change_mode = current_ov_change_mode,
      ov_calculation_method = current_ov_calculation_method
    )
  }

  invisible(
    list(
      current_ov_change_mode = current_ov_change_mode,
      current_ov_change_label = current_ov_change_label,
      current_ov_threshold = current_ov_threshold
    )
  )
}

set_starting_ov_adjustment_mode <- function(starting_ov_adjustment_mode) {
  validate_starting_ov_adjustment_mode(starting_ov_adjustment_mode)
  spec <- get_starting_ov_adjustment_spec(starting_ov_adjustment_mode)

  current_starting_ov_adjustment_mode <<- starting_ov_adjustment_mode
  current_starting_ov_adjustment_label <<- spec$label
  current_starting_ov_adjustment_formula_mode <<- spec$formula_mode

  invisible(
    list(
      current_starting_ov_adjustment_mode = current_starting_ov_adjustment_mode,
      current_starting_ov_adjustment_label = current_starting_ov_adjustment_label,
      current_starting_ov_adjustment_formula_mode = current_starting_ov_adjustment_formula_mode
    )
  )
}

# Set which OV calculation's delta columns should feed the canonical regression fields.
set_ov_calculation_method <- function(ov_calculation_method) {
  validate_ov_calculation_method(ov_calculation_method)
  spec <- get_ov_calculation_spec(ov_calculation_method)

  current_ov_calculation_method <<- ov_calculation_method
  current_ov_calculation_label <<- spec$label
  current_delta_ov_source_col <<- spec$delta_col
  current_delta_ov_annualized_source_col <<- spec$annualized_col
  current_starting_ov_source_col <<- spec$initial_col

  if (!is.na(current_ov_change_mode)) {
    current_ov_threshold <<- get_ov_threshold(
      ov_change_mode = current_ov_change_mode,
      ov_calculation_method = current_ov_calculation_method
    )
  }

  invisible(
    list(
      current_ov_calculation_method = current_ov_calculation_method,
      current_ov_calculation_label = current_ov_calculation_label,
      current_delta_ov_source_col = current_delta_ov_source_col,
      current_delta_ov_annualized_source_col = current_delta_ov_annualized_source_col,
      current_starting_ov_source_col = current_starting_ov_source_col,
      current_ov_threshold = current_ov_threshold
    )
  )
}

# Return the configured source column for one deforestation tile-sum/bin pair.
get_defor_source_col <- function(defor_approach,
                                 defor_bin = current_defor_bin) {
  validate_defor_approach(defor_approach)
  validate_defor_bin(defor_bin)

  spec <- get_defor_approach_spec(defor_approach)

  defor_bin_source_mode <- defor_bin_specs[[defor_bin]]$source_mode

  if ("source_cols" %in% names(spec)) {
    source_col <- spec$source_cols[[defor_bin_source_mode]]
  } else if (identical(defor_bin_source_mode, "baseline")) {
    source_col <- spec$source_col
  } else {
    source_col <- NULL
  }

  if (is.null(source_col) || is.na(source_col)) {
    stop(
      "No source column configured for ",
      defor_approach,
      " with defor_bin ",
      defor_bin,
      ".",
      call. = FALSE
    )
  }

  source_col
}

# Set the active deforestation bin.
set_defor_bin <- function(defor_bin) {
  validate_defor_bin(defor_bin)

  current_defor_bin <<- defor_bin

  if (!is.na(current_defor_approach)) {
    current_defor_source_col <<- get_defor_source_col(
      current_defor_approach,
      current_defor_bin
    )
  }

  invisible(
    list(
      current_defor_bin = current_defor_bin,
      current_defor_source_col = current_defor_source_col
    )
  )
}

# Set the active deforestation approach and the source column it should use from cluster_deltas.
set_defor_approach <- function(defor_approach) {
  validate_defor_approach(defor_approach)
  spec <- get_defor_approach_spec(defor_approach)

  current_defor_approach <<- defor_approach
  current_defor_tile_sum <<- spec$tile_sum_dir

  if (is.na(current_defor_bin)) {
    stop(
      "current_defor_bin is not set. Call set_defor_bin(...) before set_defor_approach(...).",
      call. = FALSE
    )
  }

  current_defor_source_col <<- get_defor_source_col(
    current_defor_approach,
    current_defor_bin
  )

  invisible(
    list(
      current_defor_approach = current_defor_approach,
      current_defor_tile_sum = current_defor_tile_sum,
      current_defor_source_col = current_defor_source_col
    )
  )
}

# Build readable labels and path fragments for the current regression run.
build_run_label <- function(cluster_database = current_cluster_database,
                            cluster_method = current_cluster_method,
                            cluster_radius_km = current_cluster_radius_km,
                            buffer_km = current_buffer_km,
                            ov_calculation_method = current_ov_calculation_method,
                            annualization_mode = current_annualization_mode,
                            delta_ov_approach = current_delta_ov_approach,
                            starting_ov_adjustment_mode = current_starting_ov_adjustment_mode,
                            defor_approach = current_defor_approach,
                            regression_model = current_regression_model,
                            defor_transform = NULL,
                            label_type = c(
                              "human_readable",
                              "cluster_database_folder",
                              "cluster_method_folder",
                              "cluster_radius_folder",
                              "buffer_folder",
                              "regression_group_folder",
                              "ov_calculation_folder",
                              "ov_change_folder",
                              "defor_bin_folder",
                              "annualization_mode_folder",
                              "delta_ov_approach_folder",
                              "starting_ov_adjustment_folder",
                              "defor_tile_sum_folder",
                              "regression_model_folder",
                              "defor_transform_folder",
                              "regression_group_path",
                              "ov_calculation_path",
                              "defor_bin_path",
                            "annualization_mode_path",
                            "delta_ov_approach_path",
                            "single_tile_collapse_path",
                            "ov_change_path",
                            "starting_ov_adjustment_path",
                            "defor_tile_sum_path",
                            "regression_model_path",
                            "defor_transform_path"
                            ),
                            base_dir = output_dir) {
  label_type <- match.arg(label_type)
  
  validate_cluster_database(cluster_database)
  validate_annualization_mode(annualization_mode)
  validate_delta_ov_approach(delta_ov_approach)
  validate_single_tile_collapse_mode(current_single_tile_collapse_mode)
  validate_ov_change_mode(current_ov_change_mode)
  validate_starting_ov_adjustment_mode(starting_ov_adjustment_mode)
  validate_ov_calculation_method(ov_calculation_method)
  validate_regression_model(regression_model)

  if (!is.null(defor_approach)) {
    validate_defor_approach(defor_approach)
  }

  defor_spec <- if (!is.null(defor_approach)) get_defor_approach_spec(defor_approach) else NULL
  annualization_spec <- get_annualization_mode_spec(annualization_mode)
  delta_ov_spec <- get_delta_ov_approach_spec(delta_ov_approach)
  defor_transform_spec <- get_defor_transform_spec(defor_transform)
  ov_calc_spec <- get_ov_calculation_spec(ov_calculation_method)
  starting_ov_adjustment_spec <- get_starting_ov_adjustment_spec(starting_ov_adjustment_mode)

  cluster_database_dir <- cluster_database
  cluster_method_dir <- cluster_method
  cluster_radius_dir <- paste0("radius_", sprintf("%.1fkm", cluster_radius_km))
  buffer_dir <- paste0("buf_", buffer_km, "km")
  grouping_dir <- regression_group_specs[[current_regression_group]]$output_dir
  ov_calculation_dir <- ov_calc_spec$output_dir
  exposure_dir <- defor_bin_specs[[current_defor_bin]]$output_dir
  scale_dir <- annualization_spec$output_dir
  ov_dir <- delta_ov_spec$output_dir
  single_tile_collapse_dir <- single_tile_collapse_mode_specs[[current_single_tile_collapse_mode]]$output_dir
  ov_change_dir <- ov_change_mode_specs[[current_ov_change_mode]]$output_dir
  starting_ov_adjustment_dir <- starting_ov_adjustment_spec$output_dir
  defor_tile_sum_dir <- if (!is.null(defor_spec)) defor_spec$tile_sum_dir else NULL
  regression_model_dir <- regression_model
  transform_dir <- if (!is.null(defor_transform_spec)) defor_transform_spec$output_dir else NULL

  path_dirs <- list(
    cluster_database = cluster_database_dir,
    cluster_method = cluster_method_dir,
    cluster_radius = cluster_radius_dir,
    buffer = buffer_dir,
    regression_group = grouping_dir,
    annualization_mode = scale_dir,
    defor_bin = exposure_dir,
    delta_ov_approach = ov_dir,
    single_tile_collapse_mode = single_tile_collapse_dir,
    ov_calculation_method = ov_calculation_dir,
    ov_change_mode = ov_change_dir,
    starting_ov_adjustment = starting_ov_adjustment_dir,
    defor_tile_sum = defor_tile_sum_dir,
    regression_model = regression_model_dir,
    defor_transform = transform_dir
  )
  
  if (label_type == "cluster_database_folder") {
    return(cluster_database_dir)
  }

  if (label_type == "cluster_method_folder") {
    return(cluster_method_dir)
  }
  
  if (label_type == "cluster_radius_folder") {
    return(cluster_radius_dir)
  }
  
  if (label_type == "buffer_folder") {
    return(buffer_dir)
  }

  if (label_type == "regression_group_folder") {
    return(grouping_dir)
  }

  if (label_type == "ov_calculation_folder") {
    return(ov_calculation_dir)
  }

  if (label_type == "ov_change_folder") {
    return(ov_change_dir)
  }

  if (label_type == "defor_bin_folder") {
    return(exposure_dir)
  }

  if (label_type == "annualization_mode_folder") {
    return(scale_dir)
  }
  
  if (label_type == "delta_ov_approach_folder") {
    return(ov_dir)
  }

  if (label_type == "starting_ov_adjustment_folder") {
    return(starting_ov_adjustment_dir)
  }
  
  if (label_type == "defor_tile_sum_folder") {
    return(defor_tile_sum_dir)
  }

  if (label_type == "regression_model_folder") {
    return(regression_model_dir)
  }
  
  if (label_type == "defor_transform_folder") {
    return(transform_dir)
  }

  if (label_type == "regression_group_path") {
    return(build_conditional_output_path(path_dirs, "regression_group", base_dir))
  }

  if (label_type == "ov_calculation_path") {
    return(build_conditional_output_path(path_dirs, "ov_calculation_method", base_dir))
  }

  if (label_type == "ov_change_path") {
    return(build_conditional_output_path(path_dirs, "ov_change_mode", base_dir))
  }

  if (label_type == "starting_ov_adjustment_path") {
    return(build_conditional_output_path(path_dirs, "starting_ov_adjustment", base_dir))
  }

  if (label_type == "defor_bin_path") {
    return(build_conditional_output_path(path_dirs, "defor_bin", base_dir))
  }

  if (label_type == "annualization_mode_path") {
    return(build_conditional_output_path(path_dirs, "annualization_mode", base_dir))
  }

  if (label_type == "delta_ov_approach_path") {
    return(build_conditional_output_path(path_dirs, "delta_ov_approach", base_dir))
  }

  if (label_type == "single_tile_collapse_path") {
    return(build_conditional_output_path(path_dirs, "single_tile_collapse_mode", base_dir))
  }
  
  if (label_type == "defor_tile_sum_path") {
    return(build_conditional_output_path(path_dirs, "defor_tile_sum", base_dir))
  }

  if (label_type == "regression_model_path") {
    return(build_conditional_output_path(path_dirs, "regression_model", base_dir))
  }

  if (label_type == "defor_transform_path") {
    return(build_conditional_output_path(path_dirs, "defor_transform", base_dir))
  }
  
  defor_label <- if (!is.null(defor_approach)) {
    defor_spec$label
  } else {
    NULL
  }

  display_parts <- c(
    paste0("cluster_database = ", cluster_database),
    paste0("cluster_method = ", cluster_method),
    paste0("cluster_radius_km = ", sprintf("%.1f", cluster_radius_km)),
    paste0("buffer_km = ", buffer_km),
    paste0("regression_group = ", current_regression_group),
    paste0("ov_calculation_method = ", ov_calculation_method),
    paste0("defor_bin = ", current_defor_bin),
    paste0("annualization_mode = ", annualization_mode),
    paste0("delta_ov_approach = ", delta_ov_approach),
    paste0("single_tile_collapse_mode = ", current_single_tile_collapse_mode),
    paste0("ov_change_mode = ", current_ov_change_mode),
    paste0("starting_ov_adjustment_mode = ", starting_ov_adjustment_mode),
    if (!is.null(defor_approach)) paste0("defor_approach = ", defor_approach),
    if (!is.null(defor_tile_sum_dir)) paste0("defor_tile_sum = ", defor_tile_sum_dir),
    paste0("regression_model = ", regression_model),
    if (!is.null(defor_label)) paste0("defor_label = ", defor_label),
    if (!is.null(defor_transform)) paste0("defor_transform = ", defor_transform)
  )
  
  paste(display_parts[!is.na(display_parts) & nzchar(display_parts)], collapse = " | ")
}

# Set the current run labels and derive the input and output paths for that run.
set_regression_run_paths <- function(cluster_database,
                                     cluster_method, 
                                     cluster_radius_km, 
                                     buffer_km) {
  if (is.na(current_delta_ov_approach)) {
    stop(
      "current_delta_ov_approach is not set. Call set_delta_ov_approach(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_annualization_mode)) {
    stop(
      "current_annualization_mode is not set. Call set_annualization_mode(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_ov_calculation_method) || is.na(current_delta_ov_source_col)) {
    stop(
      "current_ov_calculation_method is not set. Call set_ov_calculation_method(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_ov_change_mode)) {
    stop(
      "current_ov_change_mode is not set. Call set_ov_change_mode(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_starting_ov_adjustment_mode)) {
    stop(
      "current_starting_ov_adjustment_mode is not set. Call set_starting_ov_adjustment_mode(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_single_tile_collapse_mode)) {
    stop(
      "current_single_tile_collapse_mode is not set. Call set_single_tile_collapse_mode(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_regression_group) || is.na(current_group_col) || is.na(current_group_label)) {
    stop(
      "current_regression_group is not set. Call set_regression_group(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_defor_bin)) {
    stop(
      "current_defor_bin is not set. Call set_defor_bin(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_defor_approach) || is.na(current_defor_source_col)) {
    stop(
      "current_defor_approach is not set. Call set_defor_approach(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_regression_model)) {
    stop(
      "current_regression_model is not set. Call set_regression_model(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }
  validate_cluster_database(cluster_database)

  current_cluster_database <<- cluster_database
  current_cluster_method <<- cluster_method
  current_cluster_radius_km <<- cluster_radius_km
  current_buffer_km <<- buffer_km
  
  current_cluster_stub <<- paste0(
    current_cluster_method,
    "_rad_",
    sprintf("%.1fkm", current_cluster_radius_km)
  )
  current_buffer_stub <<- paste0("buf_", current_buffer_km, "km")
  current_run_label <<- build_run_label(label_type = "human_readable")
  
  # Input from 04_deforestation_tile_tag/analysis
  cluster_deltas_path <<- file.path(
    analysis_output_dir_current,
    current_cluster_database,
    current_cluster_method,
    paste0("radius_", sprintf("%.1fkm", current_cluster_radius_km)),
    current_buffer_stub,
    delta_ov_approach_specs[[current_delta_ov_approach]]$source_dir,
    single_tile_collapse_mode_specs[[current_single_tile_collapse_mode]]$source_dir,
    "tables",
    "cluster_deltas.csv"
  )
  
  # Optional access to canonical build outputs for maps / exploration
  canonical_tabular_dir <<- file.path(
    build_output_dir,
    current_cluster_database,
    current_cluster_method,
    paste0("radius_", sprintf("%.1fkm", current_cluster_radius_km)),
    "canonical_tabular"
  )
  
  canonical_spatial_dir <<- file.path(
    build_output_dir,
    current_cluster_database,
    current_cluster_method,
    paste0("radius_", sprintf("%.1fkm", current_cluster_radius_km)),
    "canonical_spatial"
  )
  
  # Regression output directory for this run and deforestation approach
  ov_calculation_output_dir <<- build_run_label(label_type = "ov_calculation_path")
  annualization_mode_output_dir <<- build_run_label(label_type = "annualization_mode_path")
  ov_output_dir <<- build_run_label(label_type = "delta_ov_approach_path")
  single_tile_collapse_output_dir <<- build_run_label(label_type = "single_tile_collapse_path")
  ov_change_output_dir <<- build_run_label(label_type = "ov_change_path")
  starting_ov_adjustment_output_dir <<- build_run_label(label_type = "starting_ov_adjustment_path")
  defor_approach_output_dir <<- build_run_label(label_type = "defor_tile_sum_path")
  regression_model_output_dir <<- build_run_label(label_type = "regression_model_path")
  
  for (dir_path in c(ov_calculation_output_dir, annualization_mode_output_dir, ov_output_dir, single_tile_collapse_output_dir, ov_change_output_dir, starting_ov_adjustment_output_dir, defor_approach_output_dir, regression_model_output_dir)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  invisible(
    list(
      current_delta_ov_approach = current_delta_ov_approach,
      current_single_tile_collapse_mode = current_single_tile_collapse_mode,
      current_cluster_database = current_cluster_database,
      current_ov_calculation_method = current_ov_calculation_method,
      current_ov_change_mode = current_ov_change_mode,
      current_starting_ov_adjustment_mode = current_starting_ov_adjustment_mode,
      current_annualization_mode = current_annualization_mode,
      current_regression_model = current_regression_model,
      current_defor_approach = current_defor_approach,
      cluster_deltas_path = cluster_deltas_path,
      canonical_tabular_dir = canonical_tabular_dir,
      canonical_spatial_dir = canonical_spatial_dir,
      ov_calculation_output_dir = ov_calculation_output_dir,
      annualization_mode_output_dir = annualization_mode_output_dir,
      ov_output_dir = ov_output_dir,
      single_tile_collapse_output_dir = single_tile_collapse_output_dir,
      ov_change_output_dir = ov_change_output_dir,
      starting_ov_adjustment_output_dir = starting_ov_adjustment_output_dir,
      defor_approach_output_dir = defor_approach_output_dir,
      regression_model_output_dir = regression_model_output_dir
    )
  )
}

# ------------------------------
# Check required inputs and save model artifacts safely.
# ------------------------------

# Stop execution if a required file path does not exist.
assert_exists <- function(path) {
  if (!file.exists(path)) {
    stop("Missing required file: ", path, call. = FALSE)
  }
}

# Stop execution if a data frame is missing any required columns.
assert_has_cols <- function(data, cols, data_name = deparse(substitute(data))) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        data_name,
        " is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

# Save a model object to disk after attaching run metadata and ensuring the folder exists.
save_model_rds <- function(model,
                           path,
                           run_label = NULL,
                           defor_approach = current_defor_approach,
                           defor_transform = current_defor_transform) {
  if (!is.null(run_label)) {
    attr(model, "run_label") <- run_label
  }
  if (!is.null(defor_approach)) {
    attr(model, "defor_approach") <- defor_approach
  }
  if (!is.null(defor_transform)) {
    attr(model, "defor_transform") <- defor_transform
  }
  attr(model, "regression_group") <- current_regression_group
  attr(model, "defor_bin") <- current_defor_bin
  attr(model, "defor_tile_sum") <- current_defor_tile_sum
  attr(model, "annualization_mode") <- current_annualization_mode
  attr(model, "single_tile_collapse_mode") <- current_single_tile_collapse_mode
  attr(model, "ov_calculation_method") <- current_ov_calculation_method
  attr(model, "ov_change_mode") <- current_ov_change_mode
  attr(model, "ov_threshold") <- current_ov_threshold
  attr(model, "ov_change_uses_percent") <- ov_change_uses_percent()
  attr(model, "starting_ov_adjustment_mode") <- current_starting_ov_adjustment_mode
  attr(model, "starting_ov_adjustment_formula_mode") <- current_starting_ov_adjustment_formula_mode
  attr(model, "delta_ov_source_col") <- current_delta_ov_source_col
  attr(model, "starting_ov_source_col") <- current_starting_ov_source_col
  attr(model, "delta_ov_approach") <- current_delta_ov_approach
  attr(model, "regression_model") <- current_regression_model
  attr(model, "buffer_km") <- current_buffer_km
  attr(model, "cluster_database") <- current_cluster_database
  attr(model, "cluster_method") <- current_cluster_method
  attr(model, "cluster_radius_km") <- current_cluster_radius_km
  
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(model, path)
  log_verbose("Wrote model: ", path)
}

# ------------------------------
# Standardize labels and generate consistent file and sample naming stubs.
# ------------------------------

# Order AEZ labels numerically when possible while preserving their original text values.
standardize_aez_order <- function(x) {
  x_chr <- as.character(x)
  x_num <- readr::parse_number(x_chr)
  
  ord <- order(is.na(x_num), x_num, x_chr)
  factor(x_chr, levels = unique(x_chr[ord]))
}

# Preserve the intended climate-zone ordering.
standardize_climate_zone_order <- function(x) {
  factor(
    as.character(x),
    levels = c("tropical", "temperate", "cold")
  )
}

# Order country labels alphabetically while preserving the observed spelling.
standardize_country_order <- function(x) {
  x_chr <- as.character(x)
  factor(x_chr, levels = sort(unique(x_chr[!is.na(x_chr)])))
}

# Order the active grouping variable appropriately for plots and summaries.
standardize_group_order <- function(x, regression_group = current_regression_group) {
  if (identical(regression_group, "by_climate_zone")) {
    return(standardize_climate_zone_order(x))
  }

  if (identical(regression_group, "by_country")) {
    return(standardize_country_order(x))
  }

  standardize_aez_order(x)
}

# Collapse AEZ codes into broader climate-zone groups.
derive_climate_zone_from_aez <- function(aez_value) {
  aez_num <- readr::parse_number(as.character(aez_value))

  dplyr::case_when(
    !is.na(aez_num) & aez_num >= 1 & aez_num <= 6 ~ "tropical",
    !is.na(aez_num) & aez_num >= 7 & aez_num <= 12 ~ "temperate",
    !is.na(aez_num) & aez_num >= 13 & aez_num <= 18 ~ "cold",
    TRUE ~ NA_character_
  )
}

# ------------------------------
# Normalize cluster ∆ inputs into the format expected by the regressions
# ------------------------------

# Standardize cluster-∆ column names once at the regression boundary.
normalize_cluster_deltas_schema <- function(df) {
  names(df) <- tolower(names(df))

  rename_map <- c(
    year_start = "year_t1",
    year_final = "year_t2",
    ov_start = "ov_t1",
    ov_final = "ov_t2",
    n_sites_start = "n_sites_t1",
    n_sites_final = "n_sites_t2"
  )

  for (source_col in names(rename_map)) {
    target_col <- rename_map[[source_col]]
    if (source_col %in% names(df) && !target_col %in% names(df)) {
      df[[target_col]] <- df[[source_col]]
    }
  }

  if ("analysis_unit_id" %in% names(df) && !"cluster_id" %in% names(df)) {
    df$cluster_id <- df$analysis_unit_id
  }

  if ("cluster_id" %in% names(df) && !"analysis_unit_id" %in% names(df)) {
    df$analysis_unit_id <- df$cluster_id
  }

  df
}

# Coerce cluster-∆ columns to the types expected by the regression pipeline.
coerce_cluster_deltas_types <- function(df) {
  df <- normalize_cluster_deltas_schema(df)
  assert_has_cols(df, required_cluster_deltas_cols, "cluster_deltas")
  
  if (!"n_matched_tiles_with_ha" %in% names(df)) {
    df$n_matched_tiles_with_ha <- NA_real_
  }
  
  if (!"medoid_latitude" %in% names(df)) {
    df$medoid_latitude <- NA_real_
  }
  
  if (!"medoid_longitude" %in% names(df)) {
    df$medoid_longitude <- NA_real_
  }
  
  df %>%
    mutate(
      aez = as.character(aez),
      cluster_id = as.character(cluster_id),
      analysis_unit_id = as.character(analysis_unit_id),
      primary_country_id = as.character(primary_country_id),
      primary_country_name = as.character(primary_country_name),
      year_t1 = as.integer(year_t1),
      year_t2 = as.integer(year_t2),
      year_gap = as.integer(year_gap),
      ov_t1 = as.numeric(ov_t1),
      ov_t2 = as.numeric(ov_t2),
      delta_ov = as.numeric(delta_ov),
      n_sites_t1 = as.numeric(n_sites_t1),
      n_sites_t2 = as.numeric(n_sites_t2),
      n_defor_years = as.numeric(n_defor_years),
      delta_defor_ha = as.numeric(delta_defor_ha),
      n_matched_tiles_with_ha = as.numeric(n_matched_tiles_with_ha),
      medoid_latitude = as.numeric(medoid_latitude),
      medoid_longitude = as.numeric(medoid_longitude)
    )
}

# Normalize cluster-∆ inputs into the regression-ready format used downstream.
build_regression_data <- function(cluster_deltas) {
  cluster_deltas <- normalize_cluster_deltas_schema(cluster_deltas)
  assert_has_cols(cluster_deltas, required_cluster_deltas_cols, "cluster_deltas")
  
  cluster_deltas %>%
    coerce_cluster_deltas_types() %>%
    mutate(
      aez = standardize_aez_order(aez),
      climate_zone = standardize_climate_zone_order(
        derive_climate_zone_from_aez(aez)
      ),
      country = standardize_country_order(primary_country_name),
      group_value = standardize_group_order(.data[[current_group_col]]),
      starting_ov = as.numeric(starting_ov),
      delta_ov_annualized = if (is_annualized_delta_mode()) {
        delta_ov
      } else {
        if_else(
          !is.na(year_gap) & year_gap > 0,
          delta_ov / year_gap,
          NA_real_
        )
      },
      mean_n_sites = (n_sites_t1 + n_sites_t2) / 2
    ) %>%
    group_by(group_value) %>%
    mutate(
      starting_ov_group_mean = mean(starting_ov, na.rm = TRUE),
      starting_ov_group_mean = if_else(
        is.nan(starting_ov_group_mean),
        NA_real_,
        starting_ov_group_mean
      ),
      starting_ov_centered = starting_ov - starting_ov_group_mean
    ) %>%
    ungroup() %>%
    arrange(
      group_value, cluster_id, year_t1, year_t2,
      ov_t1, ov_t2, delta_ov, delta_defor_ha,
      delta_defor_ha_annualized, inverse_change,
      n_sites_t1, n_sites_t2, n_matched_tiles_with_ha,
      medoid_latitude, medoid_longitude
    )
}

apply_ov_change <- function(data) {
  validate_ov_change_mode(current_ov_change_mode)
  assert_has_cols(data, "starting_ov", "data")

  if (!ov_change_applies_threshold()) {
    return(data)
  }

  threshold <- get_ov_threshold(
    ov_change_mode = current_ov_change_mode,
    ov_calculation_method = current_ov_calculation_method
  )

  data %>%
    filter(!is.na(starting_ov), starting_ov >= threshold)
}

# ------------------------------
# Recompute transformed deforestation fields and trim outliers within each group.
# ------------------------------

# Rebuild the log1p deforestation column after any row filtering or value changes.
log1p_defor <- function(data) {
  assert_has_cols(data, "delta_defor_ha", "data")

  data %>%
    mutate(
      log1p_delta_defor_ha = log1p(delta_defor_ha)
    )
}

# Drop within-group observations above the chosen quantile cutoff.
p90_trim <- function(data, threshold = 0.90) {
  assert_has_cols(data, c("group_value", "delta_defor_ha", "delta_ov"), "data")

  data %>%
    filter(
      !is.na(group_value),
      !is.na(delta_defor_ha),
      !is.na(delta_ov)
    ) %>%
    group_by(group_value) %>%
    mutate(
      upper_defor = quantile(delta_defor_ha, threshold, na.rm = TRUE)
    ) %>%
    filter(delta_defor_ha <= upper_defor) %>%
    ungroup() %>%
    dplyr::select(-upper_defor) %>%
    log1p_defor()
}

starting_ov_adjustment_required_cols <- function() {
  if (identical(current_starting_ov_adjustment_formula_mode, "none")) {
    return(character())
  }

  c("starting_ov", "starting_ov_centered")
}

get_model_required_cols <- function(regressor_col) {
  c("group_value", "cluster_id", "delta_ov", regressor_col, starting_ov_adjustment_required_cols())
}

build_regression_formula <- function(regressor_col,
                                     model_kind = current_regression_model) {
  validate_starting_ov_adjustment_mode(current_starting_ov_adjustment_mode)

  regressor_terms <- if (identical(model_kind, "polynomial")) {
    c(regressor_col, paste0("I(", regressor_col, "^2)"))
  } else if (identical(model_kind, "gam_spline")) {
    regressor_col
  } else {
    regressor_col
  }

  adjustment_terms <- dplyr::case_when(
    identical(current_starting_ov_adjustment_formula_mode, "none") ~ "",
    identical(current_starting_ov_adjustment_formula_mode, "control") ~ "starting_ov_centered",
    identical(current_starting_ov_adjustment_formula_mode, "interaction") ~ paste(
      "starting_ov_centered",
      paste0(regressor_col, ":starting_ov_centered"),
      sep = " + "
    ),
    TRUE ~ NA_character_
  )

  if (is.na(adjustment_terms)) {
    stop(
      "Unknown starting OV formula mode: ",
      current_starting_ov_adjustment_formula_mode,
      call. = FALSE
    )
  }

  rhs_terms <- c(regressor_terms, adjustment_terms)
  rhs_terms <- rhs_terms[!is.na(rhs_terms) & nzchar(rhs_terms)]
  rhs <- paste(rhs_terms, collapse = " + ")
  stats::as.formula(paste("delta_ov ~", rhs))
}

build_gam_regression_formula <- function(regressor_col, spline_k) {
  validate_starting_ov_adjustment_mode(current_starting_ov_adjustment_mode)

  smooth_term <- paste0("s(", regressor_col, ", k = ", spline_k, ")")
  adjustment_terms <- dplyr::case_when(
    identical(current_starting_ov_adjustment_formula_mode, "none") ~ "",
    identical(current_starting_ov_adjustment_formula_mode, "control") ~ "starting_ov_centered",
    identical(current_starting_ov_adjustment_formula_mode, "interaction") ~ paste(
      "starting_ov_centered",
      paste0(regressor_col, ":starting_ov_centered"),
      sep = " + "
    ),
    TRUE ~ NA_character_
  )

  if (is.na(adjustment_terms)) {
    stop(
      "Unknown starting OV formula mode: ",
      current_starting_ov_adjustment_formula_mode,
      call. = FALSE
    )
  }

  rhs_terms <- c(smooth_term, adjustment_terms)
  rhs_terms <- rhs_terms[!is.na(rhs_terms) & nzchar(rhs_terms)]
  rhs <- paste(rhs_terms, collapse = " + ")
  stats::as.formula(paste("delta_ov ~", rhs))
}

# Count usable rows by grouping variable and keep only groups that meet the
# minimum regression thresholds. Outcome variation is required because fixest
# cannot estimate an intercept model with a constant dependent variable.
prepare_regression_variant <- function(data_variant, regressor_col, variant_label) {
  required_cols <- get_model_required_cols(regressor_col)
  assert_has_cols(data_variant, required_cols, "data_variant")

  group_counts <- data_variant %>%
    filter(
      !is.na(group_value),
      !is.na(.data[[regressor_col]]),
      !is.na(delta_ov),
      if (length(starting_ov_adjustment_required_cols()) > 0) {
        !is.na(starting_ov_centered)
      } else {
        TRUE
      }
    ) %>%
    group_by(group_value) %>%
    summarise(
      n_obs = n(),
      n_clusters = n_distinct(cluster_id),
      n_negative_delta_clusters = n_distinct(cluster_id[delta_ov < 0]),
      n_unique_regressor = n_distinct(.data[[regressor_col]]),
      n_unique_delta_ov = n_distinct(delta_ov),
      .groups = "drop"
    ) %>%
    arrange(group_value)

  eligible_groups <- group_counts %>%
    filter(
      n_obs >= min_observations_per_group_regression,
      n_unique_regressor >= 2,
      n_unique_delta_ov >= 2,
      if (identical(current_regression_group, "by_country")) {
        n_negative_delta_clusters >= min_observations_per_group_regression
      } else {
        TRUE
      }
    ) %>%
    pull(group_value) %>%
    as.character()

  regression_data_variant <- data_variant %>%
    filter(
      as.character(group_value) %in% eligible_groups,
      !is.na(.data[[regressor_col]]),
      !is.na(delta_ov),
      if (length(starting_ov_adjustment_required_cols()) > 0) {
        !is.na(starting_ov_centered)
      } else {
        TRUE
      }
    )

  log_verbose(
    current_group_label,
    " groups meeting minimum regression thresholds for ",
    variant_label,
    " (",
    min_observations_per_group_regression,
    "): ",
    length(eligible_groups)
  )

  list(
    regressor_col = regressor_col,
    group_counts = group_counts,
    eligible_groups = eligible_groups,
    regression_data = regression_data_variant
  )
}

# Fit group-specific OLS models after checking that the prepared sample still has eligible groups.
fit_ols_variant <- function(prepared_variant, variant_label) {
  if (length(prepared_variant$eligible_groups) == 0) {
    warning(
      "No ",
      current_group_label,
      " group has at least min_observations_per_group_regression = ",
      min_observations_per_group_regression,
      " for ",
      variant_label,
      ". Skipping group-specific OLS."
    )
    return(list())
  }

  fit_ols_by_group(
    prepared_variant$regression_data,
    prepared_variant$regressor_col
  )
}

# Fit group-specific robust models after checking that the prepared sample still has eligible groups.
fit_robust_variant <- function(prepared_variant, variant_label) {
  if (length(prepared_variant$eligible_groups) == 0) {
    warning(
      "No ",
      current_group_label,
      " group has at least min_observations_per_group_regression = ",
      min_observations_per_group_regression,
      " for ",
      variant_label,
      ". Skipping group-specific robust regression."
    )
    return(list())
  }

  fit_robust_by_group(
    prepared_variant$regression_data,
    prepared_variant$regressor_col
  )
}

fit_polynomial_variant <- function(prepared_variant, variant_label) {
  if (length(prepared_variant$eligible_groups) == 0) {
    warning("No eligible groups for polynomial regression: ", variant_label, call. = FALSE)
    return(list())
  }

  fit_polynomial_by_group(
    prepared_variant$regression_data,
    prepared_variant$regressor_col
  )
}

fit_gam_spline_variant <- function(prepared_variant, variant_label) {
  if (length(prepared_variant$eligible_groups) == 0) {
    warning("No eligible groups for GAM/spline regression: ", variant_label, call. = FALSE)
    return(list())
  }

  fit_gam_spline_by_group(
    prepared_variant$regression_data,
    prepared_variant$regressor_col
  )
}

# ------------------------------
# Fit pooled and group-specific regression models for the prepared analysis data.
# ------------------------------

# Fit one OLS model per active grouping value using the requested regressor column.
fit_ols_by_group <- function(data, regressor_col) {
  assert_has_cols(data, get_model_required_cols(regressor_col), "data")
  
  data_split <- data %>%
    group_by(group_value) %>%
    group_split(.keep = TRUE)
  
  group_names <- data %>%
    group_by(group_value) %>%
    group_keys() %>%
    pull(group_value) %>%
    as.character()
  
  model_formula <- build_regression_formula(regressor_col, "ols")
  
  map(
    rlang::set_names(data_split, group_names),
    ~ feols(model_formula, data = .x)
  )
}

# Fit one robust linear model per active grouping value using the requested regressor column.
fit_robust_by_group <- function(data, regressor_col) {
  assert_has_cols(data, get_model_required_cols(regressor_col), "data")

  data_split <- data %>%
    group_by(group_value) %>%
    group_split(.keep = TRUE)

  group_names <- data %>%
    group_by(group_value) %>%
    group_keys() %>%
    pull(group_value) %>%
    as.character()

  model_formula <- build_regression_formula(regressor_col, "robust_linear")

  map(
    rlang::set_names(data_split, group_names),
    ~ MASS::rlm(model_formula, data = .x, maxit = 100)
  )
}

fit_polynomial_by_group <- function(data, regressor_col) {
  assert_has_cols(data, get_model_required_cols(regressor_col), "data")

  data_split <- data %>%
    group_by(group_value) %>%
    group_split(.keep = TRUE)

  group_names <- data %>%
    group_by(group_value) %>%
    group_keys() %>%
    pull(group_value) %>%
    as.character()

  model_formula <- build_regression_formula(regressor_col, "polynomial")

  map(
    rlang::set_names(data_split, group_names),
    ~ feols(model_formula, data = .x)
  )
}

fit_gam_spline_by_group <- function(data, regressor_col) {
  assert_has_cols(data, get_model_required_cols(regressor_col), "data")

  data_filtered <- data %>%
    filter(
      !is.na(delta_ov),
      !is.na(.data[[regressor_col]]),
      if (length(starting_ov_adjustment_required_cols()) > 0) {
        !is.na(starting_ov_centered)
      } else {
        TRUE
      }
    ) %>%
    group_by(group_value) %>%
    filter(
      n() >= min_observations_per_group_regression,
      n_distinct(.data[[regressor_col]]) >= 4
    ) %>%
    ungroup()

  if (nrow(data_filtered) == 0) {
    return(list())
  }

  data_split <- data_filtered %>%
    group_by(group_value) %>%
    group_split(.keep = TRUE)

  group_names <- data_filtered %>%
    group_by(group_value) %>%
    group_keys() %>%
    pull(group_value) %>%
    as.character()

  models <- purrr::map(
    data_split,
    function(group_data) {
      n_unique_regressor <- dplyr::n_distinct(group_data[[regressor_col]])
      spline_k <- min(5, n_unique_regressor - 1)

      model_formula <- build_gam_regression_formula(regressor_col, spline_k)

      mgcv::gam(model_formula, data = group_data, method = "REML")
    }
  )

  rlang::set_names(models, group_names)
}

# Fit one pooled annualized OLS model using annualized deforestation and OV changes.
fit_annualized_ols <- function(data) {
  required_cols <- c("delta_ov_annualized", "delta_defor_ha_annualized")
  assert_has_cols(data, required_cols, "data")
  
  feols(
    delta_ov_annualized ~ delta_defor_ha_annualized,
    data = data
  )
}

# Fit one annualized OLS model per active grouping value after dropping incomplete rows.
fit_annualized_ols_by_group <- function(data) {
  required_cols <- c("group_value", "delta_ov_annualized", "delta_defor_ha_annualized")
  assert_has_cols(data, required_cols, "data")
  
  data_filtered <- data %>%
    filter(
      !is.na(group_value),
      !is.na(delta_ov_annualized),
      !is.na(delta_defor_ha_annualized)
    )
  
  data_split <- data_filtered %>%
    group_by(group_value) %>%
    group_split(.keep = TRUE)
  
  group_names <- data_filtered %>%
    group_by(group_value) %>%
    group_keys() %>%
    pull(group_value) %>%
    as.character()
  
  map(
    set_names(data_split, group_names),
    ~ feols(delta_ov_annualized ~ delta_defor_ha_annualized, data = .x)
  )
}

# Extract a comparable fit statistic from either fixest OLS or MASS robust models.
extract_fitstat_numeric <- function(model, stat_name) {
  if (inherits(model, "fixest")) {
    return(unname(as.numeric(fixest::fitstat(model, stat_name))))
  }

  if (inherits(model, "rlm")) {
    y <- model$model$delta_ov
    n_obs <- stats::nobs(model)
    n_coef <- length(stats::coef(model))
    rss <- sum(stats::residuals(model)^2, na.rm = TRUE)
    tss <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)

    r2 <- if (is.finite(tss) && tss > 0) 1 - rss / tss else NA_real_

    if (identical(stat_name, "r2")) {
      return(unname(as.numeric(r2)))
    }

    if (identical(stat_name, "ar2")) {
      if (is.na(r2) || is.na(n_obs) || n_obs <= n_coef) {
        return(NA_real_)
      }

      adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - n_coef))
      return(unname(as.numeric(adj_r2)))
    }
  }

  if (inherits(model, "gam")) {
    model_summary <- summary(model)
    gam_r2 <- if (!is.null(model_summary$r.sq)) {
      model_summary$r.sq
    } else if (!is.null(model_summary$dev.expl)) {
      model_summary$dev.expl
    } else {
      NA_real_
    }

    if (identical(stat_name, "r2")) {
      return(unname(as.numeric(gam_r2)))
    }

    if (identical(stat_name, "ar2")) {
      return(NA_real_)
    }
  }

  NA_real_
}

# Summarize per-group model fit and correlation diagnostics for one model family.
build_group_fit_summary <- function(models_list, data_used, model_label, regressor_col) {
  if (length(models_list) == 0) {
    return(tibble::tibble())
  }

  interaction_coef_name <- function(coef_names) {
    candidate_names <- c(
      paste0(regressor_col, ":starting_ov_centered"),
      paste0("starting_ov_centered:", regressor_col)
    )
    matched <- candidate_names[candidate_names %in% coef_names]

    if (length(matched) == 0) {
      return(NA_character_)
    }

    matched[[1]]
  }

  fit_summary <- purrr::imap_dfr(
    models_list,
    ~ {
      model_coef <- stats::coef(.x)
      interaction_name <- interaction_coef_name(names(model_coef))

      tibble::tibble(
        group_value = .y,
        model = model_label,
        regressor = regressor_col,
        starting_ov_adjustment_mode = current_starting_ov_adjustment_mode,
        n_obs = unname(as.numeric(stats::nobs(.x))),
        r_squared = extract_fitstat_numeric(.x, "r2"),
        adj_r_squared = extract_fitstat_numeric(.x, "ar2"),
        slope = if (regressor_col %in% names(model_coef)) {
          unname(model_coef[[regressor_col]])
        } else {
          NA_real_
        },
        starting_ov_slope = if ("starting_ov_centered" %in% names(model_coef)) {
          unname(model_coef[["starting_ov_centered"]])
        } else {
          NA_real_
        },
        defor_starting_ov_interaction = if (!is.na(interaction_name)) {
          unname(model_coef[[interaction_name]])
        } else {
          NA_real_
        }
      )
    }
  )

  correlations <- data_used %>%
    dplyr::group_by(group_value) %>%
    dplyr::summarise(
      n_obs_data = dplyr::n(),
      n_unique_regressor = dplyr::n_distinct(.data[[regressor_col]]),
      correlation = if (
        dplyr::n() >= 2 &&
        dplyr::n_distinct(.data[[regressor_col]]) >= 2 &&
        dplyr::n_distinct(delta_ov) >= 2
      ) {
        stats::cor(.data[[regressor_col]], delta_ov)
      } else {
        NA_real_
      },
      .groups = "drop"
    )

  fit_summary %>%
    dplyr::left_join(correlations, by = "group_value")
}

# Write one transform-level fit-stat table. The folder hierarchy carries the
# run metadata, so keep the CSV focused on group-level diagnostics.
write_fit_summary <- function(summary_df,
                              output_dirs,
                              defor_transform,
                              run_label,
                              filename = "fit_stats.csv") {
  if (nrow(summary_df) == 0) {
    return(invisible(NULL))
  }

  readr::write_csv(
    summary_df %>%
      dplyr::select(
        group_value,
        model,
        starting_ov_adjustment_mode,
        n_obs,
        r_squared,
        adj_r_squared,
        slope,
        starting_ov_slope,
        defor_starting_ov_interaction,
        n_unique_regressor,
        correlation
      ) %>%
      mutate(
        across(where(is.numeric), ~ round(.x, 3))
      ),
    file.path(output_dirs$family_root, filename)
  )
}

# Build a compact chart subtitle from the active hierarchy options.
build_compact_run_label <- function(defor_transform = NULL) {
  paste(
    c(
      current_regression_group,
      paste0("buf_", current_buffer_km, "km"),
      current_annualization_mode,
      current_defor_bin,
      current_delta_ov_approach,
      current_single_tile_collapse_mode,
      current_ov_calculation_method,
      current_ov_change_mode,
      current_starting_ov_adjustment_mode,
      current_defor_tile_sum,
      current_regression_model,
      defor_transform
    ),
    collapse = " | "
  )
}

# Save a plot inside one family folder and append the run label as a subtitle when supplied.
save_family_plot <- function(plot_obj,
                             output_dirs,
                             filename,
                             run_label = NULL,
                             width = 12,
                             height = 8) {
  if (!is.null(run_label)) {
    plot_obj <- plot_obj + labs(subtitle = stringr::str_wrap(run_label, width = 95))
  }

  ggplot2::ggsave(
    filename = file.path(output_dirs$family_root, filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = 300
  )
}

# Create an x-grid and fitted values for plotting one regression line per group.
build_prediction_grid <- function(models_list, data_used, regressor_col, n_points = 100) {
  if (length(models_list) == 0 || nrow(data_used) == 0) {
    return(tibble::tibble())
  }

  purrr::imap_dfr(
    models_list,
    ~ {
      data_group <- data_used %>%
        dplyr::filter(as.character(group_value) == .y)

      if (nrow(data_group) == 0) {
        return(tibble::tibble())
      }

      x_vals <- data_group[[regressor_col]]
      x_vals <- x_vals[is.finite(x_vals)]

      if (length(x_vals) == 0) {
        return(tibble::tibble())
      }

      x_min <- min(x_vals)
      x_max <- max(x_vals)

      grid_x <- if (isTRUE(all.equal(x_min, x_max))) {
        x_min
      } else {
        seq(x_min, x_max, length.out = n_points)
      }

      newdata <- tibble::tibble(!!regressor_col := grid_x)
      if (length(starting_ov_adjustment_required_cols()) > 0) {
        newdata$starting_ov_centered <- 0
      }
      newdata$group_value <- .y
      newdata$delta_ov_hat <- as.numeric(stats::predict(.x, newdata = newdata))
      newdata
    }
  )
}

# Build readable per-facet fit labels for diagnostic plots.
build_plot_fit_labels <- function(models_list, group_levels) {
  if (length(models_list) == 0) {
    return(tibble::tibble())
  }

  purrr::imap_dfr(
    models_list,
    ~ {
      r_squared <- extract_fitstat_numeric(.x, "r2")

      tibble::tibble(
        group_value = .y,
        r_squared = r_squared,
        r_squared_label = if (is.finite(r_squared)) {
          sprintf("R^2 = %.3f", r_squared)
        } else {
          "R^2 = n/a"
        }
      )
    }
  ) %>%
    mutate(
      group_value = factor(as.character(group_value), levels = group_levels)
    )
}

# Bin observation windows using year_gap so diagnostics can show short,
# medium, and long OV intervals without changing the underlying regression data.
add_year_gap_bin <- function(data) {
  assert_has_cols(data, "year_gap", "data")

  data %>%
    mutate(
      year_gap_bin = dplyr::case_when(
        !is.na(year_gap) & year_gap >= 1L & year_gap <= 2L ~ "1-2 years",
        !is.na(year_gap) & year_gap >= 3L & year_gap <= 6L ~ "3-6 years",
        !is.na(year_gap) & year_gap >= 7L ~ "7+ years",
        TRUE ~ NA_character_
      ),
      year_gap_bin = factor(
        year_gap_bin,
        levels = c("1-2 years", "3-6 years", "7+ years")
      )
    )
}

# Build a faceted scatterplot with fitted group-specific regression lines for one family.
build_family_plot_from_models <- function(data_used,
                                          models_list,
                                          regressor_col,
                                          title_text) {
  prediction_grid <- build_prediction_grid(models_list, data_used, regressor_col)
  group_levels <- levels(standardize_group_order(c(data_used$group_value, prediction_grid$group_value)))
  fit_labels <- build_plot_fit_labels(models_list, group_levels)

  data_used <- data_used %>%
    add_year_gap_bin() %>%
    mutate(
      group_value = factor(as.character(group_value), levels = group_levels),
      plot_y = delta_ov
    )

  prediction_grid <- prediction_grid %>%
    mutate(group_value = factor(as.character(group_value), levels = group_levels))

  ggplot(
    data_used,
    aes(x = .data[[regressor_col]], y = plot_y, color = year_gap_bin)
  ) +
    geom_point(alpha = 0.3) +
    geom_line(
      data = prediction_grid,
      aes(x = .data[[regressor_col]], y = delta_ov_hat),
      color = "steelblue4",
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    geom_label(
      data = fit_labels,
      aes(label = r_squared_label),
      x = -Inf,
      y = Inf,
      hjust = -0.05,
      vjust = 1.15,
      size = 3,
      linewidth = 0.2,
      fill = "white",
      alpha = 0.85,
      color = "grey15",
      inherit.aes = FALSE
    ) +
    facet_wrap(
      ~ group_value,
      scales = "fixed",
      axes = "all",
      axis.labels = "all"
    ) +
    scale_color_manual(
      values = c(
        "1-2 years" = "#1b9e77",
        "3-6 years" = "#d95f02",
        "7+ years" = "#7570b3"
      ),
      drop = FALSE,
      name = "Observation window"
    ) +
    labs(
      x = regressor_col,
      y = get_delta_ov_axis_label(),
      title = title_text
    ) +
    theme_minimal()
}

# ------------------------------
# Create regression output folders and export formatted summary tables.
# ------------------------------

# Return the output directory object for one regression transform and create it if requested.
get_regression_output_dirs <- function(defor_transform, create = TRUE) {
  family_root <- build_run_label(
    defor_transform = defor_transform,
    label_type = "defor_transform_path"
  )

  dirs <- list(family_root = family_root)

  if (isTRUE(create)) {
    walk(dirs, ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))
  }

  dirs
}

# Build one named list of transform output directories from the configured transform list.
build_output_dirs_by_transform <- function(transforms = defor_transforms) {
  setNames(
    lapply(transforms, get_regression_output_dirs),
    transforms
  )
}

# Build one human-readable label for each configured transform.
build_run_labels_by_transform <- function(transforms = defor_transforms) {
  setNames(
    purrr::map_chr(
      transforms,
      ~ build_run_label(defor_transform = .x)
    ),
    transforms
  )
}

# Build compact chart labels for each configured transform.
build_compact_run_labels_by_transform <- function(transforms = defor_transforms) {
  setNames(
    purrr::map_chr(
      transforms,
      ~ build_compact_run_label(defor_transform = .x)
    ),
    transforms
  )
}

# Write a formatted modelsummary table to disk and include the run label in the title when provided.
save_modelsummary_table <- function(models, path, title = NULL, run_label = NULL, ...) {
  if (!is.null(run_label)) {
    title <- if (is.null(title)) run_label else paste0(title, " | ", run_label)
  }
  
  modelsummary::modelsummary(
    models,
    output = path,
    title = title,
    gof_omit = "IC|Log|Adj|Within|Pseudo|Std.Errors",
    ...
  )
  
  log_verbose("Wrote table: ", path)
}
