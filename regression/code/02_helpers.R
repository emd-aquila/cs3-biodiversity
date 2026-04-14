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

# Set the active deforestation approach and the source column it should use from cluster_deltas.
set_defor_approach <- function(defor_approach) {
  validate_defor_approach(defor_approach)

  current_defor_approach <<- defor_approach
  current_defor_source_col <<- defor_approach_specs[[defor_approach]]$source_col

  invisible(
    list(
      current_defor_approach = current_defor_approach,
      current_defor_source_col = current_defor_source_col
    )
  )
}

# Build readable labels and path fragments that follow cluster -> radius -> buffer -> scale -> OV -> defor approach -> transform.
build_run_label <- function(cluster_method = current_cluster_method,
                            cluster_radius_km = current_cluster_radius_km,
                            buffer_km = current_buffer_km,
                            regression_scale = current_regression_scale,
                            ov_approach = current_ov_approach,
                            defor_approach = current_defor_approach,
                            defor_transform = NULL,
                            label_type = c(
                              "human_readable",
                              "cluster_method_folder",
                              "cluster_radius_folder",
                              "buffer_folder",
                              "regression_scale_folder",
                              "ov_approach_folder",
                              "defor_approach_folder",
                              "defor_transform_folder",
                              "regression_scale_path",
                              "ov_approach_path",
                              "defor_approach_path",
                              "defor_transform_path"
                            ),
                            base_dir = output_dir) {
  label_type <- match.arg(label_type)
  
  validate_regression_scale(regression_scale)
  validate_delta_ov_approach(ov_approach)

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
  
  cluster_method_dir <- cluster_method
  cluster_radius_dir <- paste0("radius_", sprintf("%.1fkm", cluster_radius_km))
  buffer_dir <- paste0("buf_", buffer_km, "km")
  scale_dir <- regression_scale
  ov_dir <- ov_approach
  defor_dir <- defor_approach
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

  if (label_type == "regression_scale_folder") {
    return(scale_dir)
  }
  
  if (label_type == "ov_approach_folder") {
    return(ov_dir)
  }
  
  if (label_type == "defor_approach_folder") {
    return(defor_dir)
  }
  
  if (label_type == "defor_transform_folder") {
    return(transform_dir)
  }

  if (label_type == "regression_scale_path") {
    path_parts <- c(base_dir, cluster_method_dir, cluster_radius_dir, buffer_dir, scale_dir)
    return(do.call(file.path, as.list(path_parts)))
  }

  if (label_type == "ov_approach_path") {
    path_parts <- c(base_dir, cluster_method_dir, cluster_radius_dir, buffer_dir, scale_dir, ov_dir)
    return(do.call(file.path, as.list(path_parts)))
  }
  
  if (label_type == "defor_approach_path") {
    path_parts <- c(base_dir, cluster_method_dir, cluster_radius_dir, buffer_dir, scale_dir, ov_dir, defor_dir)
    path_parts <- path_parts[!vapply(path_parts, is.null, logical(1))]
    return(do.call(file.path, as.list(path_parts)))
  }

  if (label_type == "defor_transform_path") {
    path_parts <- c(base_dir, cluster_method_dir, cluster_radius_dir, buffer_dir, scale_dir, ov_dir, defor_dir, transform_dir)
    path_parts <- path_parts[!vapply(path_parts, is.null, logical(1))]
    return(do.call(file.path, as.list(path_parts)))
  }
  
  defor_label <- if (!is.null(defor_approach)) {
    defor_approach_specs[[defor_approach]]$label
  } else {
    NULL
  }

  display_parts <- c(
    paste0("cluster_method = ", cluster_method),
    paste0("cluster_radius_km = ", sprintf("%.1f", cluster_radius_km)),
    paste0("buffer_km = ", buffer_km),
    paste0("regression_scale = ", regression_scale),
    paste0("ov_approach = ", ov_approach),
    if (!is.null(defor_approach)) paste0("defor_approach = ", defor_approach),
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

  if (is.na(current_defor_approach) || is.na(current_defor_source_col)) {
    stop(
      "current_defor_approach is not set. Call set_defor_approach(...) before set_regression_run_paths().",
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
  regression_scale_output_dir <<- build_run_label(label_type = "regression_scale_path")
  ov_output_dir <<- build_run_label(label_type = "ov_approach_path")
  defor_approach_output_dir <<- build_run_label(label_type = "defor_approach_path")
  
  for (dir_path in c(regression_scale_output_dir, ov_output_dir, defor_approach_output_dir)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  invisible(
    list(
      current_ov_approach = current_ov_approach,
      current_regression_scale = current_regression_scale,
      current_defor_approach = current_defor_approach,
      cluster_deltas_path = cluster_deltas_path,
      canonical_tabular_dir = canonical_tabular_dir,
      canonical_spatial_dir = canonical_spatial_dir,
      regression_scale_output_dir = regression_scale_output_dir,
      ov_output_dir = ov_output_dir,
      defor_approach_output_dir = defor_approach_output_dir
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
  attr(model, "regression_scale") <- current_regression_scale
  attr(model, "ov_approach") <- current_ov_approach
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
      delta_defor_ha_annualized = as.numeric(delta_defor_ha_annualized),
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
      AEZ, cluster_id, year_t1, year_t2,
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

# Drop within-AEZ observations above the chosen quantile cutoff.
p90_trim <- function(data, threshold = 0.90) {
  assert_has_cols(data, c("AEZ", "delta_defor_ha", "delta_ov"), "data")

  data %>%
    filter(
      !is.na(AEZ),
      !is.na(delta_defor_ha),
      !is.na(delta_ov)
    ) %>%
    group_by(AEZ) %>%
    mutate(
      upper_defor = quantile(delta_defor_ha, threshold, na.rm = TRUE)
    ) %>%
    filter(delta_defor_ha <= upper_defor) %>%
    ungroup() %>%
    dplyr::select(-upper_defor) %>%
    log1p_defor()
}

# Cap extreme deforestation values within each AEZ at the chosen quantile threshold.
winsorize <- function(data, threshold = 0.90) {
  assert_has_cols(data, c("AEZ", "delta_defor_ha"), "data")

  data %>%
    filter(!is.na(AEZ), !is.na(delta_defor_ha)) %>%
    group_by(AEZ) %>%
    mutate(
      winsor_cap_defor = quantile(delta_defor_ha, threshold, na.rm = TRUE),
      delta_defor_ha = pmin(delta_defor_ha, winsor_cap_defor)
    ) %>%
    ungroup() %>%
    dplyr::select(-winsor_cap_defor) %>%
    log1p_defor()
}

# Count usable rows by AEZ and keep only groups that meet the minimum regression threshold.
prepare_regression_variant <- function(data_variant, regressor_col, variant_label) {
  aez_counts <- data_variant %>%
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

  eligible_aez <- aez_counts %>%
    filter(
      n_obs >= min_observations_per_aez_regression,
      n_unique_regressor >= 2
    ) %>%
    pull(AEZ) %>%
    as.character()

  regression_data_variant <- data_variant %>%
    filter(
      as.character(AEZ) %in% eligible_aez,
      !is.na(.data[[regressor_col]]),
      !is.na(delta_ov)
    )

  message(
    "AEZs meeting minimum observation threshold for ",
    variant_label,
    " (",
    min_observations_per_aez_regression,
    "): ",
    length(eligible_aez)
  )

  list(
    regressor_col = regressor_col,
    aez_counts = aez_counts,
    eligible_aez = eligible_aez,
    regression_data = regression_data_variant
  )
}

# Fit AEZ-specific OLS models after checking that the prepared sample still has eligible groups.
fit_ols_variant <- function(prepared_variant, variant_label) {
  if (length(prepared_variant$eligible_aez) == 0) {
    warning(
      "No AEZ has at least min_observations_per_aez_regression = ",
      min_observations_per_aez_regression,
      " for ",
      variant_label,
      ". Skipping AEZ-specific OLS."
    )
    return(list())
  }

  fit_ols_by_aez(
    prepared_variant$regression_data,
    prepared_variant$regressor_col
  )
}

# Fit AEZ-specific robust models after checking that the prepared sample still has eligible groups.
fit_robust_variant <- function(prepared_variant, variant_label) {
  if (length(prepared_variant$eligible_aez) == 0) {
    warning(
      "No AEZ has at least min_observations_per_aez_regression = ",
      min_observations_per_aez_regression,
      " for ",
      variant_label,
      ". Skipping AEZ-specific robust regression."
    )
    return(list())
  }

  fit_robust_by_aez(
    prepared_variant$regression_data,
    prepared_variant$regressor_col
  )
}

# ------------------------------
# Fit pooled and AEZ-specific regression models for the prepared analysis data.
# ------------------------------

# Fit one OLS model per AEZ using the requested regressor column.
fit_ols_by_aez <- function(data, regressor_col) {
  assert_has_cols(data, c("AEZ", "delta_ov", regressor_col), "data")
  
  data_split <- data %>%
    group_by(AEZ) %>%
    group_split(.keep = TRUE)
  
  aez_names <- data %>%
    group_by(AEZ) %>%
    group_keys() %>%
    pull(AEZ) %>%
    as.character()
  
  model_formula <- stats::as.formula(
    paste("delta_ov ~", regressor_col)
  )
  
  map(
    rlang::set_names(data_split, aez_names),
    ~ feols(model_formula, data = .x)
  )
}

# Fit one robust linear model per AEZ using the requested regressor column.
fit_robust_by_aez <- function(data, regressor_col) {
  assert_has_cols(data, c("AEZ", "delta_ov", regressor_col), "data")

  data_split <- data %>%
    group_by(AEZ) %>%
    group_split(.keep = TRUE)

  aez_names <- data %>%
    group_by(AEZ) %>%
    group_keys() %>%
    pull(AEZ) %>%
    as.character()

  model_formula <- stats::as.formula(
    paste("delta_ov ~", regressor_col)
  )

  map(
    rlang::set_names(data_split, aez_names),
    ~ MASS::rlm(model_formula, data = .x, maxit = 100)
  )
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

# Fit one annualized OLS model per AEZ after dropping incomplete rows.
fit_annualized_ols_by_aez <- function(data) {
  required_cols <- c("AEZ", "delta_ov_annualized", "delta_defor_ha_annualized")
  assert_has_cols(data, required_cols, "data")
  
  data_filtered <- data %>%
    filter(
      !is.na(AEZ),
      !is.na(delta_ov_annualized),
      !is.na(delta_defor_ha_annualized)
    )
  
  data_split <- data_filtered %>%
    group_by(AEZ) %>%
    group_split(.keep = TRUE)
  
  aez_names <- data_filtered %>%
    group_by(AEZ) %>%
    group_keys() %>%
    pull(AEZ) %>%
    as.character()
  
  map(
    set_names(data_split, aez_names),
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

  NA_real_
}

# Summarize per-AEZ model fit and correlation diagnostics for one model family.
build_aez_fit_summary <- function(models_list, data_used, model_label, regressor_col) {
  if (length(models_list) == 0) {
    return(tibble::tibble())
  }

  fit_summary <- purrr::imap_dfr(
    models_list,
    ~ {
      model_coef <- stats::coef(.x)

      tibble::tibble(
        AEZ = .y,
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
    dplyr::group_by(AEZ) %>%
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
    dplyr::left_join(correlations, by = "AEZ")
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
        ov_approach = current_ov_approach,
        regression_scale = current_regression_scale,
        buffer_km = current_buffer_km,
        cluster_method = current_cluster_method,
        cluster_radius_km = current_cluster_radius_km,
        defor_approach = current_defor_approach,
        defor_source_col = current_defor_source_col,
        defor_transform = defor_transform,
        run_label = run_label,
        .before = 1
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

# Create an x-grid and fitted values for plotting one regression line per AEZ.
build_prediction_grid <- function(models_list, data_used, regressor_col, n_points = 100) {
  if (length(models_list) == 0 || nrow(data_used) == 0) {
    return(tibble::tibble())
  }

  purrr::imap_dfr(
    models_list,
    ~ {
      data_aez <- data_used %>%
        dplyr::filter(as.character(AEZ) == .y)

      if (nrow(data_aez) == 0) {
        return(tibble::tibble())
      }

      x_vals <- data_aez[[regressor_col]]
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
      newdata$AEZ <- .y
      newdata$delta_ov_hat <- as.numeric(stats::predict(.x, newdata = newdata))
      newdata
    }
  )
}

# Build a faceted scatterplot with fitted AEZ-specific regression lines for one family.
build_family_plot_from_models <- function(data_used,
                                          models_list,
                                          regressor_col,
                                          title_text) {
  prediction_grid <- build_prediction_grid(models_list, data_used, regressor_col)
  aez_levels <- levels(standardize_aez_order(c(data_used$AEZ, prediction_grid$AEZ)))

  data_used <- data_used %>%
    mutate(AEZ = factor(as.character(AEZ), levels = aez_levels))

  prediction_grid <- prediction_grid %>%
    mutate(AEZ = factor(as.character(AEZ), levels = aez_levels))

  ggplot(
    data_used,
    aes(x = .data[[regressor_col]], y = delta_ov)
  ) +
    geom_point(alpha = 0.3) +
    geom_line(
      data = prediction_grid,
      aes(x = .data[[regressor_col]], y = delta_ov_hat),
      color = "steelblue4",
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    facet_wrap(~ AEZ, scales = "free") +
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
