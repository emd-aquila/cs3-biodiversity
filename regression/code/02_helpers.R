# =====================================================
# Helper functions for regression pipeline
# =====================================================

# ------------------------------
# Build run identifiers and directories used for a given regression
# ------------------------------

# Check that the selected OV approach is valid.
validate_delta_ov_approach <- function(delta_ov_approach) {
  valid_modes <- ov_approaches
  
  if (!delta_ov_approach %in% valid_modes) {
    stop(
      "Unknown ov_approach: ",
      delta_ov_approach,
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
  if (!defor_approach %in% defor_approaches) {
    stop(
      "Unknown defor_approach: ",
      defor_approach,
      ". Valid approaches are: ",
      paste(defor_approaches, collapse = ", "),
      call. = FALSE
    )
  }
}

# Check that the selected regression scale is valid.
validate_regression_scale <- function(regression_scale) {
  if (!regression_scale %in% regression_scales) {
    stop(
      "Unknown regression_scale: ",
      regression_scale,
      ". Valid scales are: ",
      paste(regression_scales, collapse = ", "),
      call. = FALSE
    )
  }
}

# Check that the selected regression model family is valid.
validate_regression_model_family <- function(model_family) {
  if (!model_family %in% regression_model_families) {
    stop(
      "Unknown regression_model_family: ",
      model_family,
      ". Valid families are: ",
      paste(regression_model_families, collapse = ", "),
      call. = FALSE
    )
  }
}

# Check that the selected grouping level is valid.
validate_regression_grouping_level <- function(grouping_level) {
  if (!grouping_level %in% regression_grouping_levels) {
    stop(
      "Unknown regression_grouping_level: ",
      grouping_level,
      ". Valid levels are: ",
      paste(regression_grouping_levels, collapse = ", "),
      call. = FALSE
    )
  }
}

# Check that the selected deforestation exposure mode is valid.
validate_defor_exposure_mode <- function(exposure_mode) {
  if (!exposure_mode %in% defor_exposure_modes) {
    stop(
      "Unknown defor_exposure_mode: ",
      exposure_mode,
      ". Valid modes are: ",
      paste(defor_exposure_modes, collapse = ", "),
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

# Set whether regressions use raw deltas or annualized deltas.
set_regression_scale <- function(regression_scale) {
  validate_regression_scale(regression_scale)

  current_regression_scale <<- regression_scale

  invisible(
    list(
      current_regression_scale = current_regression_scale
    )
  )
}

# Set the model family used for the current regression run.
set_regression_model_family <- function(model_family) {
  validate_regression_model_family(model_family)

  current_regression_model_family <<- model_family

  invisible(
    list(
      current_regression_model_family = current_regression_model_family
    )
  )
}

# Set the grouping level used for regressions and diagnostics.
set_regression_grouping_level <- function(grouping_level) {
  validate_regression_grouping_level(grouping_level)

  current_grouping_level <<- grouping_level
  current_group_col <<- regression_grouping_specs[[grouping_level]]$group_col
  current_group_label <<- regression_grouping_specs[[grouping_level]]$label

  invisible(
    list(
      current_grouping_level = current_grouping_level,
      current_group_col = current_group_col,
      current_group_label = current_group_label
    )
  )
}

# Set the active OV approach and point later code to the shared deforestation_tile_tag output root.
set_delta_ov_approach <- function(delta_ov_approach) {
  validate_delta_ov_approach(delta_ov_approach)
  
  current_ov_approach <<- delta_ov_approach
  analysis_output_dir_current <<- file.path(
    repo_root,
    "deforestation_tile_tag",
    "analysis",
    "output"
  )
  
  invisible(
    list(
      current_ov_approach = current_ov_approach,
      analysis_output_dir_current = analysis_output_dir_current
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

  invisible(
    list(
      current_ov_calculation_method = current_ov_calculation_method,
      current_ov_calculation_label = current_ov_calculation_label,
      current_delta_ov_source_col = current_delta_ov_source_col,
      current_delta_ov_annualized_source_col = current_delta_ov_annualized_source_col
    )
  )
}

# Return the configured source column for one deforestation approach/exposure pair.
get_defor_source_col <- function(defor_approach,
                                 exposure_mode = current_defor_exposure_mode) {
  validate_defor_approach(defor_approach)
  validate_defor_exposure_mode(exposure_mode)

  spec <- get_defor_approach_spec(defor_approach)

  if ("source_cols" %in% names(spec)) {
    source_col <- spec$source_cols[[exposure_mode]]
  } else if (identical(exposure_mode, "baseline")) {
    source_col <- spec$source_col
  } else {
    source_col <- NULL
  }

  if (is.null(source_col) || is.na(source_col)) {
    stop(
      "No source column configured for ",
      defor_approach,
      " with exposure mode ",
      exposure_mode,
      ".",
      call. = FALSE
    )
  }

  source_col
}

# Set the active deforestation exposure mode.
set_defor_exposure_mode <- function(exposure_mode) {
  validate_defor_exposure_mode(exposure_mode)

  current_defor_exposure_mode <<- exposure_mode

  if (!is.na(current_defor_approach)) {
    current_defor_source_col <<- get_defor_source_col(
      current_defor_approach,
      current_defor_exposure_mode
    )
  }

  invisible(
    list(
      current_defor_exposure_mode = current_defor_exposure_mode,
      current_defor_source_col = current_defor_source_col
    )
  )
}

# Set the active deforestation approach and the source column it should use from cluster_deltas.
set_defor_approach <- function(defor_approach) {
  validate_defor_approach(defor_approach)
  spec <- get_defor_approach_spec(defor_approach)

  current_defor_approach <<- defor_approach
  current_defor_summing <<- spec$summing_dir
  current_defor_data_type <<- spec$data_dir

  if (is.na(current_defor_exposure_mode)) {
    stop(
      "current_defor_exposure_mode is not set. Call set_defor_exposure_mode(...) before set_defor_approach(...).",
      call. = FALSE
    )
  }

  current_defor_source_col <<- get_defor_source_col(
    current_defor_approach,
    current_defor_exposure_mode
  )

  invisible(
    list(
      current_defor_approach = current_defor_approach,
      current_defor_summing = current_defor_summing,
      current_defor_data_type = current_defor_data_type,
      current_defor_source_col = current_defor_source_col
    )
  )
}

# Build readable labels and path fragments for the current regression run.
build_run_label <- function(cluster_method = current_cluster_method,
                            cluster_radius_km = current_cluster_radius_km,
                            buffer_km = current_buffer_km,
                            ov_calculation_method = current_ov_calculation_method,
                            regression_scale = current_regression_scale,
                            ov_approach = current_ov_approach,
                            defor_approach = current_defor_approach,
                            model_family = current_regression_model_family,
                            defor_transform = NULL,
                            label_type = c(
                              "human_readable",
                              "cluster_method_folder",
                              "cluster_radius_folder",
                              "buffer_folder",
                              "grouping_level_folder",
                              "ov_calculation_folder",
                              "defor_exposure_folder",
                              "regression_scale_folder",
                              "ov_approach_folder",
                              "defor_summing_folder",
                              "defor_data_folder",
                              "model_family_folder",
                              "defor_transform_folder",
                              "grouping_level_path",
                              "ov_calculation_path",
                              "defor_exposure_path",
                              "regression_scale_path",
                              "ov_approach_path",
                              "defor_summing_path",
                              "defor_data_path",
                              "model_family_path",
                              "defor_transform_path"
                            ),
                            base_dir = output_dir) {
  label_type <- match.arg(label_type)
  
  validate_regression_scale(regression_scale)
  validate_delta_ov_approach(ov_approach)
  validate_ov_calculation_method(ov_calculation_method)
  validate_regression_model_family(model_family)

  if (!is.null(defor_approach)) {
    validate_defor_approach(defor_approach)
  }

  if (!is.null(defor_transform) && !defor_transform %in% defor_transforms) {
    stop(
      "Unknown defor_transform: ",
      defor_transform,
      ". Valid transforms are: ",
      paste(defor_transforms, collapse = ", "),
      call. = FALSE
    )
  }
  
  defor_spec <- if (!is.null(defor_approach)) get_defor_approach_spec(defor_approach) else NULL
  ov_calc_spec <- get_ov_calculation_spec(ov_calculation_method)

  cluster_method_dir <- cluster_method
  cluster_radius_dir <- paste0("radius_", sprintf("%.1fkm", cluster_radius_km))
  buffer_dir <- paste0("buf_", buffer_km, "km")
  grouping_dir <- regression_grouping_specs[[current_grouping_level]]$output_dir
  ov_calculation_dir <- ov_calc_spec$output_dir
  exposure_dir <- defor_exposure_mode_specs[[current_defor_exposure_mode]]$output_dir
  scale_dir <- regression_scale
  ov_dir <- ov_approach
  defor_summing_dir <- if (!is.null(defor_spec)) defor_spec$summing_dir else NULL
  defor_data_dir <- if (!is.null(defor_spec)) defor_spec$data_dir else NULL
  model_family_dir <- model_family
  transform_dir <- defor_transform
  
  if (label_type == "cluster_method_folder") {
    return(cluster_method_dir)
  }
  
  if (label_type == "cluster_radius_folder") {
    return(cluster_radius_dir)
  }
  
  if (label_type == "buffer_folder") {
    return(buffer_dir)
  }

  if (label_type == "grouping_level_folder") {
    return(grouping_dir)
  }

  if (label_type == "ov_calculation_folder") {
    return(ov_calculation_dir)
  }

  if (label_type == "defor_exposure_folder") {
    return(exposure_dir)
  }

  if (label_type == "regression_scale_folder") {
    return(scale_dir)
  }
  
  if (label_type == "ov_approach_folder") {
    return(ov_dir)
  }
  
  if (label_type == "defor_summing_folder") {
    return(defor_summing_dir)
  }

  if (label_type == "defor_data_folder") {
    return(defor_data_dir)
  }

  if (label_type == "model_family_folder") {
    return(model_family_dir)
  }
  
  if (label_type == "defor_transform_folder") {
    return(transform_dir)
  }

  if (label_type == "grouping_level_path") {
    path_parts <- c(base_dir, cluster_method_dir, cluster_radius_dir, buffer_dir, grouping_dir)
    return(do.call(file.path, as.list(path_parts)))
  }

  if (label_type == "ov_calculation_path") {
    path_parts <- c(base_dir, cluster_method_dir, cluster_radius_dir, buffer_dir, grouping_dir, ov_calculation_dir)
    return(do.call(file.path, as.list(path_parts)))
  }

  if (label_type == "defor_exposure_path") {
    path_parts <- c(base_dir, cluster_method_dir, cluster_radius_dir, buffer_dir, grouping_dir, ov_calculation_dir, exposure_dir)
    return(do.call(file.path, as.list(path_parts)))
  }

  if (label_type == "regression_scale_path") {
    path_parts <- c(base_dir, cluster_method_dir, cluster_radius_dir, buffer_dir, grouping_dir, ov_calculation_dir, exposure_dir, scale_dir)
    return(do.call(file.path, as.list(path_parts)))
  }

  if (label_type == "ov_approach_path") {
    path_parts <- c(base_dir, cluster_method_dir, cluster_radius_dir, buffer_dir, grouping_dir, ov_calculation_dir, exposure_dir, scale_dir, ov_dir)
    return(do.call(file.path, as.list(path_parts)))
  }
  
  if (label_type == "defor_summing_path") {
    path_parts <- c(base_dir, cluster_method_dir, cluster_radius_dir, buffer_dir, grouping_dir, ov_calculation_dir, exposure_dir, scale_dir, ov_dir, defor_summing_dir)
    path_parts <- path_parts[!vapply(path_parts, is.null, logical(1))]
    return(do.call(file.path, as.list(path_parts)))
  }

  if (label_type == "defor_data_path") {
    path_parts <- c(base_dir, cluster_method_dir, cluster_radius_dir, buffer_dir, grouping_dir, ov_calculation_dir, exposure_dir, scale_dir, ov_dir, defor_summing_dir, defor_data_dir)
    path_parts <- path_parts[!vapply(path_parts, is.null, logical(1))]
    return(do.call(file.path, as.list(path_parts)))
  }

  if (label_type == "model_family_path") {
    path_parts <- c(base_dir, cluster_method_dir, cluster_radius_dir, buffer_dir, grouping_dir, ov_calculation_dir, exposure_dir, scale_dir, ov_dir, defor_summing_dir, defor_data_dir, model_family_dir)
    path_parts <- path_parts[!vapply(path_parts, is.null, logical(1))]
    return(do.call(file.path, as.list(path_parts)))
  }

  if (label_type == "defor_transform_path") {
    path_parts <- c(base_dir, cluster_method_dir, cluster_radius_dir, buffer_dir, grouping_dir, ov_calculation_dir, exposure_dir, scale_dir, ov_dir, defor_summing_dir, defor_data_dir, model_family_dir, transform_dir)
    path_parts <- path_parts[!vapply(path_parts, is.null, logical(1))]
    return(do.call(file.path, as.list(path_parts)))
  }
  
  defor_label <- if (!is.null(defor_approach)) {
    defor_spec$label
  } else {
    NULL
  }

  display_parts <- c(
    paste0("cluster_method = ", cluster_method),
    paste0("cluster_radius_km = ", sprintf("%.1f", cluster_radius_km)),
    paste0("buffer_km = ", buffer_km),
    paste0("grouping_level = ", current_grouping_level),
    paste0("ov_calculation_method = ", ov_calculation_method),
    paste0("defor_exposure_mode = ", current_defor_exposure_mode),
    paste0("regression_scale = ", regression_scale),
    paste0("ov_approach = ", ov_approach),
    if (!is.null(defor_approach)) paste0("defor_approach = ", defor_approach),
    if (!is.null(defor_summing_dir)) paste0("defor_summing = ", defor_summing_dir),
    if (!is.null(defor_data_dir)) paste0("defor_data = ", defor_data_dir),
    paste0("model_family = ", model_family),
    if (!is.null(defor_label)) paste0("defor_label = ", defor_label),
    if (!is.null(defor_transform)) paste0("defor_transform = ", defor_transform)
  )
  
  paste(display_parts[!is.na(display_parts) & nzchar(display_parts)], collapse = " | ")
}

# Set the current run labels and derive the input and output paths for that run.
set_regression_run_paths <- function(cluster_method, 
                                     cluster_radius_km, 
                                     buffer_km) {
  if (is.na(current_ov_approach)) {
    stop(
      "current_ov_approach is not set. Call set_delta_ov_approach('ov_year_pair') ",
      "or set_delta_ov_approach('ov_whole_cluster') before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_regression_scale)) {
    stop(
      "current_regression_scale is not set. Call set_regression_scale('non_annualized') ",
      "or set_regression_scale('annualized') before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_ov_calculation_method) || is.na(current_delta_ov_source_col)) {
    stop(
      "current_ov_calculation_method is not set. Call set_ov_calculation_method(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_grouping_level) || is.na(current_group_col) || is.na(current_group_label)) {
    stop(
      "current_grouping_level is not set. Call set_regression_grouping_level(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_defor_exposure_mode)) {
    stop(
      "current_defor_exposure_mode is not set. Call set_defor_exposure_mode(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_defor_approach) || is.na(current_defor_source_col)) {
    stop(
      "current_defor_approach is not set. Call set_defor_approach(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }

  if (is.na(current_regression_model_family)) {
    stop(
      "current_regression_model_family is not set. Call set_regression_model_family(...) before set_regression_run_paths().",
      call. = FALSE
    )
  }
  
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
  
  # Input from deforestation_tile_tag/analysis
  cluster_deltas_path <<- file.path(
    analysis_output_dir_current,
    current_cluster_method,
    paste0("radius_", sprintf("%.1fkm", current_cluster_radius_km)),
    current_buffer_stub,
    current_ov_approach,
    "tables",
    "cluster_deltas.csv"
  )
  
  # Optional access to canonical build outputs for maps / exploration
  canonical_tabular_dir <<- file.path(
    build_output_dir,
    current_cluster_stub,
    "canonical_tabular"
  )
  
  canonical_spatial_dir <<- file.path(
    build_output_dir,
    current_cluster_stub,
    "canonical_spatial"
  )
  
  # Regression output directory for this run and deforestation approach
  ov_calculation_output_dir <<- build_run_label(label_type = "ov_calculation_path")
  regression_scale_output_dir <<- build_run_label(label_type = "regression_scale_path")
  ov_output_dir <<- build_run_label(label_type = "ov_approach_path")
  defor_approach_output_dir <<- build_run_label(label_type = "defor_data_path")
  model_family_output_dir <<- build_run_label(label_type = "model_family_path")
  
  for (dir_path in c(ov_calculation_output_dir, regression_scale_output_dir, ov_output_dir, defor_approach_output_dir, model_family_output_dir)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  invisible(
    list(
      current_ov_approach = current_ov_approach,
      current_ov_calculation_method = current_ov_calculation_method,
      current_regression_scale = current_regression_scale,
      current_regression_model_family = current_regression_model_family,
      current_defor_approach = current_defor_approach,
      cluster_deltas_path = cluster_deltas_path,
      canonical_tabular_dir = canonical_tabular_dir,
      canonical_spatial_dir = canonical_spatial_dir,
      ov_calculation_output_dir = ov_calculation_output_dir,
      regression_scale_output_dir = regression_scale_output_dir,
      ov_output_dir = ov_output_dir,
      defor_approach_output_dir = defor_approach_output_dir,
      model_family_output_dir = model_family_output_dir
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
  attr(model, "grouping_level") <- current_grouping_level
  attr(model, "defor_exposure_mode") <- current_defor_exposure_mode
  attr(model, "defor_summing") <- current_defor_summing
  attr(model, "defor_data_type") <- current_defor_data_type
  attr(model, "regression_scale") <- current_regression_scale
  attr(model, "ov_calculation_method") <- current_ov_calculation_method
  attr(model, "delta_ov_source_col") <- current_delta_ov_source_col
  attr(model, "ov_approach") <- current_ov_approach
  attr(model, "regression_model_family") <- current_regression_model_family
  attr(model, "buffer_km") <- current_buffer_km
  attr(model, "cluster_method") <- current_cluster_method
  attr(model, "cluster_radius_km") <- current_cluster_radius_km
  
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(model, path)
  message("Wrote model: ", path)
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

# Order the active grouping variable appropriately for plots and summaries.
standardize_group_order <- function(x, grouping_level = current_grouping_level) {
  if (identical(grouping_level, "climate_zone")) {
    return(standardize_climate_zone_order(x))
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
# Normalize cluster delta inputs into the format expected by the regressions
# ------------------------------

# Edit cluster-delta column names to be usable for start-to-final cluster OV calculation
normalize_cluster_deltas_schema <- function(df) {
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

  df
}

# Coerce cluster-delta columns to the types expected by the regression pipeline.
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
      AEZ = as.character(AEZ),
      cluster_id = as.character(cluster_id),
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

# Normalize cluster-delta inputs into the regression-ready format used downstream
build_regression_data <- function(cluster_deltas) {
  cluster_deltas <- normalize_cluster_deltas_schema(cluster_deltas)
  assert_has_cols(cluster_deltas, required_cluster_deltas_cols, "cluster_deltas")
  
  cluster_deltas %>%
    coerce_cluster_deltas_types() %>%
    mutate(
      AEZ = standardize_aez_order(AEZ),
      climate_zone = standardize_climate_zone_order(
        derive_climate_zone_from_aez(AEZ)
      ),
      group_value = standardize_group_order(.data[[current_group_col]]),
      delta_ov_annualized = if (identical(current_regression_scale, "annualized")) {
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
    arrange(
      group_value, cluster_id, year_t1, year_t2,
      ov_t1, ov_t2, delta_ov, delta_defor_ha,
      delta_defor_ha_annualized, inverse_change,
      n_sites_t1, n_sites_t2, n_matched_tiles_with_ha,
      medoid_latitude, medoid_longitude
    )
}

# ------------------------------
# Recompute transformed deforestation fields and trim outliers within each AEZ.
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

# Cap extreme deforestation values within each group at the chosen quantile threshold.
winsorize <- function(data, threshold = 0.90) {
  assert_has_cols(data, c("group_value", "delta_defor_ha"), "data")

  data %>%
    filter(!is.na(group_value), !is.na(delta_defor_ha)) %>%
    group_by(group_value) %>%
    mutate(
      winsor_cap_defor = quantile(delta_defor_ha, threshold, na.rm = TRUE),
      delta_defor_ha = pmin(delta_defor_ha, winsor_cap_defor)
    ) %>%
    ungroup() %>%
    dplyr::select(-winsor_cap_defor) %>%
    log1p_defor()
}

# Count usable rows by grouping variable and keep only groups that meet the minimum regression threshold.
prepare_regression_variant <- function(data_variant, regressor_col, variant_label) {
  group_counts <- data_variant %>%
    filter(
      !is.na(group_value),
      !is.na(.data[[regressor_col]]),
      !is.na(delta_ov)
    ) %>%
    group_by(group_value) %>%
    summarise(
      n_obs = n(),
      n_unique_regressor = n_distinct(.data[[regressor_col]]),
      .groups = "drop"
    ) %>%
    arrange(group_value)

  eligible_groups <- group_counts %>%
    filter(
      n_obs >= min_observations_per_group_regression,
      n_unique_regressor >= 2
    ) %>%
    pull(group_value) %>%
    as.character()

  regression_data_variant <- data_variant %>%
    filter(
      as.character(group_value) %in% eligible_groups,
      !is.na(.data[[regressor_col]]),
      !is.na(delta_ov)
    )

  message(
    current_group_label,
    " groups meeting minimum observation threshold for ",
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
  assert_has_cols(data, c("group_value", "delta_ov", regressor_col), "data")
  
  data_split <- data %>%
    group_by(group_value) %>%
    group_split(.keep = TRUE)
  
  group_names <- data %>%
    group_by(group_value) %>%
    group_keys() %>%
    pull(group_value) %>%
    as.character()
  
  model_formula <- stats::as.formula(
    paste("delta_ov ~", regressor_col)
  )
  
  map(
    rlang::set_names(data_split, group_names),
    ~ feols(model_formula, data = .x)
  )
}

# Fit one robust linear model per active grouping value using the requested regressor column.
fit_robust_by_group <- function(data, regressor_col) {
  assert_has_cols(data, c("group_value", "delta_ov", regressor_col), "data")

  data_split <- data %>%
    group_by(group_value) %>%
    group_split(.keep = TRUE)

  group_names <- data %>%
    group_by(group_value) %>%
    group_keys() %>%
    pull(group_value) %>%
    as.character()

  model_formula <- stats::as.formula(
    paste("delta_ov ~", regressor_col)
  )

  map(
    rlang::set_names(data_split, group_names),
    ~ MASS::rlm(model_formula, data = .x, maxit = 100)
  )
}

fit_polynomial_by_group <- function(data, regressor_col) {
  assert_has_cols(data, c("group_value", "delta_ov", regressor_col), "data")

  data_split <- data %>%
    group_by(group_value) %>%
    group_split(.keep = TRUE)

  group_names <- data %>%
    group_by(group_value) %>%
    group_keys() %>%
    pull(group_value) %>%
    as.character()

  model_formula <- stats::as.formula(
    paste0("delta_ov ~ ", regressor_col, " + I(", regressor_col, "^2)")
  )

  map(
    rlang::set_names(data_split, group_names),
    ~ feols(model_formula, data = .x)
  )
}

fit_gam_spline_by_group <- function(data, regressor_col) {
  assert_has_cols(data, c("group_value", "delta_ov", regressor_col), "data")

  data_filtered <- data %>%
    filter(!is.na(delta_ov), !is.na(.data[[regressor_col]])) %>%
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

      model_formula <- stats::as.formula(
        paste0("delta_ov ~ s(", regressor_col, ", k = ", spline_k, ")")
      )

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

  fit_summary <- purrr::imap_dfr(
    models_list,
    ~ {
      model_coef <- stats::coef(.x)

      tibble::tibble(
        group_value = .y,
        model = model_label,
        regressor = regressor_col,
        n_obs = unname(as.numeric(stats::nobs(.x))),
        r_squared = extract_fitstat_numeric(.x, "r2"),
        adj_r_squared = extract_fitstat_numeric(.x, "ar2"),
        slope = if (regressor_col %in% names(model_coef)) {
          unname(model_coef[[regressor_col]])
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

# Write one transform-level fit-stat table with run metadata columns prepended.
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
      mutate(
        grouping_level = current_grouping_level,
        group_label = current_group_label,
        defor_exposure_mode = current_defor_exposure_mode,
        defor_summing = current_defor_summing,
        defor_data_type = current_defor_data_type,
        ov_calculation_method = current_ov_calculation_method,
        ov_calculation_label = current_ov_calculation_label,
        delta_ov_source_col = current_delta_ov_source_col,
        ov_approach = current_ov_approach,
        regression_scale = current_regression_scale,
        buffer_km = current_buffer_km,
        cluster_method = current_cluster_method,
        cluster_radius_km = current_cluster_radius_km,
        defor_approach = current_defor_approach,
        defor_source_col = current_defor_source_col,
        regression_model_family = current_regression_model_family,
        defor_transform = defor_transform,
        run_label = run_label,
        .before = 1
      ) %>%
      mutate(
        across(where(is.numeric), ~ round(.x, 3))
      ),
    file.path(output_dirs$family_root, filename)
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
    plot_obj <- plot_obj + labs(subtitle = run_label)
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
      newdata$group_value <- .y
      newdata$delta_ov_hat <- as.numeric(stats::predict(.x, newdata = newdata))
      newdata
    }
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
    facet_wrap(~ group_value, scales = "fixed") +
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
      y = "delta_ov",
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
  
  message("Wrote table: ", path)
}
