# -----------------------------------------------------------------------------
# V2: PREDICTS-style pressure model (optional; V1 remains unchanged)
# -----------------------------------------------------------------------------
# This module adds the pressure structure used in the published PREDICTS BII
# work: land use x use intensity and human population density. Road density at
# 1 km and 50 km scales is available as an opt-in extension. The model is
# deliberately isolated from V1, writes to V2-specific paths, and is run only
# when CS3_BII_VERSION=v2.

v2_site_pressure_columns <- c("study_id", "site_id", "human_population_density")
v2_spatial_pressure_columns <- c("scenario", "cell_id", "year", "human_population_density")
if (isTRUE(v2_include_roads)) {
  v2_site_pressure_columns <- c(
    v2_site_pressure_columns, "road_density_1km_km_per_km2", "road_density_50km_km_per_km2"
  )
  v2_spatial_pressure_columns <- c(
    v2_spatial_pressure_columns, "road_density_1km_km_per_km2", "road_density_50km_km_per_km2"
  )
}

v2_transform_predictor <- function(x, transformation) {
  x <- pmax(0, as.numeric(x))
  switch(
    transformation,
    log1p = log1p(x),
    cube_root = x^(1 / 3),
    identity = x,
    stop("Unknown V2 predictor transformation: ", transformation, call. = FALSE)
  )
}

read_v2_pressure_table <- function(path, required_columns, label, key_columns) {
  assert_file_exists(path, label)
  data <- data.table::as.data.table(readr::read_csv(path, show_col_types = FALSE))
  assert_has_cols(data, required_columns, label)
  if (anyDuplicated(data, by = key_columns)) {
    stop(label, " must have exactly one row per ", paste(key_columns, collapse = "/"), ".", call. = FALSE)
  }
  numeric_columns <- setdiff(required_columns, key_columns)
  data[, (numeric_columns) := lapply(.SD, as.numeric), .SDcols = numeric_columns]
  if (any(vapply(data[, ..numeric_columns], function(x) any(!is.finite(x) | x < 0), logical(1)))) {
    stop(label, " requires finite, non-negative pressure values.", call. = FALSE)
  }
  data
}

v2_make_predictor_spec <- function(site) {
  variables <- data.table(
    raw_variable = "human_population_density",
    scaled_variable = "human_population_density_scaled",
    transformation = "log1p"
  )
  if (isTRUE(v2_include_roads)) {
    variables <- rbind(variables, data.table(
      raw_variable = c("road_density_1km_km_per_km2", "road_density_50km_km_per_km2"),
      scaled_variable = c("road_density_1km_scaled", "road_density_50km_scaled"),
      transformation = c("cube_root", "cube_root")
    ))
  }
  if (isTRUE(v2_include_secondary_age)) {
    variables <- rbind(variables, data.table(
      raw_variable = "secondary_age_years", scaled_variable = "secondary_age_scaled", transformation = "log1p"
    ))
  }
  rows <- lapply(seq_len(nrow(variables)), function(i) {
    variable <- variables$raw_variable[[i]]
    transformed <- v2_transform_predictor(site[[variable]], variables$transformation[[i]])
    lower <- min(transformed, na.rm = TRUE)
    upper <- max(transformed, na.rm = TRUE)
    centre <- mean(transformed, na.rm = TRUE)
    scale <- stats::sd(transformed, na.rm = TRUE)
    if (!is.finite(lower) || !is.finite(upper) || upper <= lower || !is.finite(scale) || scale <= 0) {
      stop("V2 predictor has no usable variation: ", variable, call. = FALSE)
    }
    data.table(
      raw_variable = variable,
      scaled_variable = variables$scaled_variable[[i]],
      transformation = variables$transformation[[i]],
      training_lower_transformed = lower,
      training_upper_transformed = upper,
      training_center_transformed = centre,
      training_scale_transformed = scale
    )
  })
  rbindlist(rows)
}

v2_apply_predictor_spec <- function(data, predictor_spec) {
  output <- copy(data)
  for (i in seq_len(nrow(predictor_spec))) {
    raw_variable <- predictor_spec$raw_variable[[i]]
    scaled_variable <- predictor_spec$scaled_variable[[i]]
    assert_has_cols(output, raw_variable, "V2 pressure data")
    transformed <- v2_transform_predictor(output[[raw_variable]], predictor_spec$transformation[[i]])
    transformed <- pmin(
      predictor_spec$training_upper_transformed[[i]],
      pmax(predictor_spec$training_lower_transformed[[i]], transformed)
    )
    output[, (scaled_variable) :=
      (transformed - predictor_spec$training_center_transformed[[i]]) /
      predictor_spec$training_scale_transformed[[i]]]
  }
  output
}

prepare_bii_v2_site_data <- function(site) {
  site <- copy(site)
  # V1 stores the original PREDICTS land-use and intensity labels, so the V2
  # pressure class can be created without changing the V1 preparation code.
  site[, pressure_class := make_pressure_class(land_use, intensity, variant = "land_use_intensity")]
  site <- site[!is.na(pressure_class)]
  pressure_required <- v2_site_pressure_columns
  if (isTRUE(v2_include_secondary_age)) pressure_required <- c(pressure_required, "secondary_age_years")
  pressures <- read_v2_pressure_table(
    v2_site_pressure_path, pressure_required,
    "V2 PREDICTS site-pressure input", c("study_id", "site_id")
  )
  site <- merge(site, pressures, by = c("study_id", "site_id"), all.x = TRUE, sort = FALSE)
  pressure_variables <- setdiff(pressure_required, c("study_id", "site_id", "secondary_age_years"))
  pressure_covered <- stats::complete.cases(site[, ..pressure_variables])
  missing <- site[!pressure_covered, unique(paste(study_id, site_id, sep = "/"))]
  if (length(missing) > 0) {
    missing_share <- length(missing) / nrow(site)
    if (missing_share > v2_max_missing_site_pressure_share) {
      stop(
        "V2 site-pressure input does not cover ", length(missing), " PREDICTS site(s) (",
        round(100 * missing_share, 2), "%); this exceeds v2_max_missing_site_pressure_share. Example: ",
        missing[[1]], call. = FALSE
      )
    }
    warning(
      "Dropping ", length(missing), " PREDICTS site(s) without V2 pressure coverage (",
      round(100 * missing_share, 3), "%); example: ", missing[[1]], call. = FALSE
    )
    site <- site[pressure_covered]
  }
  if (isTRUE(v2_include_secondary_age)) {
    site[land_use != "secondary", secondary_age_years := 0]
    if (any(site[land_use == "secondary", !is.finite(secondary_age_years) | secondary_age_years < 0])) {
      stop("V2 secondary sites require finite, non-negative secondary_age_years.", call. = FALSE)
    }
  }
  predictor_spec <- v2_make_predictor_spec(site)
  site <- v2_apply_predictor_spec(site, predictor_spec)
  if (isTRUE(v2_include_study_mean_hpd_control)) {
    site[, study_mean_hpd_scaled := mean(human_population_density_scaled), by = study_id]
  }
  list(site = site, predictor_spec = predictor_spec)
}

prepare_bii_v2_pairs <- function(site, composition) {
  rebuild <- identical(Sys.getenv("CS3_BII_REBUILD_V2_PAIRS"), "1")
  if (file.exists(v2_pairs_path) && !rebuild) {
    message("Reading cached V2 compositional-similarity pairs: ", v2_pairs_path)
    pairs <- as.data.table(readRDS(v2_pairs_path))
  } else {
    message("Constructing V2 balanced Bray-Curtis PREDICTS site pairs.")
    # This calls the unchanged V1 balanced-Bray pair builder, but with the V2
    # land-use-intensity pressure class already present on the site table.
    pairs <- build_composition_pairs(site, composition)
    saveRDS(pairs, v2_pairs_path, compress = "gzip")
    message("Wrote: ", v2_pairs_path)
  }
  predictor_columns <- c(
    "human_population_density_scaled",
    if (isTRUE(v2_include_roads)) c("road_density_1km_scaled", "road_density_50km_scaled"),
    if (isTRUE(v2_include_secondary_age)) "secondary_age_scaled"
  )
  target_covariates <- site[, c("study_id", "site_id", predictor_columns), with = FALSE]
  setnames(target_covariates, "site_id", "target_site_id")
  pairs <- merge(pairs, target_covariates, by = c("study_id", "target_site_id"), all.x = TRUE, sort = FALSE)
  if (any(!stats::complete.cases(pairs[, ..predictor_columns]))) {
    stop("V2 compositional pairs are missing target-site pressure data.", call. = FALSE)
  }
  pairs
}

fit_scope_models_v2 <- function(site, pairs, scope_row) {
  data <- scope_data(site, pairs, scope_row$scope_type, scope_row$scope_value)
  abundance <- data$site
  composition <- data$pairs
  if (
    nrow(abundance) < v2_minimum_sites_per_scope || nrow(composition) < v2_minimum_pairs_per_scope ||
      !reference_pressure_class %in% abundance$pressure_class ||
      !reference_pressure_class %in% composition$pressure_class
  ) {
    return(NULL)
  }
  abundance[, pressure_class := relevel(factor(pressure_class), ref = reference_pressure_class)]
  composition[, pressure_class := relevel(factor(pressure_class), ref = reference_pressure_class)]

  abundance_formula <- sqrt_relative_abundance ~
    pressure_class * human_population_density_scaled +
    (1 | study_id) + (1 | block_id)
  composition_formula <- logit_similarity ~
    pressure_class * human_population_density_scaled +
    log10_geographic_distance + (1 | study_id) + (1 | target_site_id)
  if (isTRUE(v2_include_roads)) {
    abundance_formula <- update(abundance_formula, . ~ . + pressure_class * road_density_50km_scaled)
    composition_formula <- update(
      composition_formula,
      . ~ . + pressure_class * road_density_1km_scaled + pressure_class * road_density_50km_scaled
    )
  }
  if (isTRUE(v2_include_secondary_age)) {
    abundance_formula <- update(abundance_formula, . ~ . + secondary_age_scaled)
    composition_formula <- update(composition_formula, . ~ . + secondary_age_scaled)
  }
  if (isTRUE(v2_include_study_mean_hpd_control)) {
    abundance_formula <- update(abundance_formula, . ~ . + study_mean_hpd_scaled)
  }
  if (has_lme4) {
    abundance_model <- lme4::lmer(abundance_formula, data = abundance, REML = TRUE)
    composition_model <- lme4::lmer(composition_formula, data = composition, REML = TRUE)
    fit_type <- "mixed_effects_lme4"
  } else {
    abundance_fixed_formula <- sqrt_relative_abundance ~
      pressure_class * human_population_density_scaled
    composition_fixed_formula <- logit_similarity ~
      pressure_class * human_population_density_scaled +
      log10_geographic_distance
    if (isTRUE(v2_include_roads)) {
      abundance_fixed_formula <- update(
        abundance_fixed_formula, . ~ . + pressure_class * road_density_50km_scaled
      )
      composition_fixed_formula <- update(
        composition_fixed_formula,
        . ~ . + pressure_class * road_density_1km_scaled + pressure_class * road_density_50km_scaled
      )
    }
    if (isTRUE(v2_include_secondary_age)) {
      abundance_fixed_formula <- update(abundance_fixed_formula, . ~ . + secondary_age_scaled)
      composition_fixed_formula <- update(composition_fixed_formula, . ~ . + secondary_age_scaled)
    }
    if (isTRUE(v2_include_study_mean_hpd_control)) {
      abundance_fixed_formula <- update(abundance_fixed_formula, . ~ . + study_mean_hpd_scaled)
    }
    abundance_model <- stats::lm(abundance_fixed_formula, data = abundance)
    composition_model <- stats::lm(composition_fixed_formula, data = composition)
    fit_type <- "fixed_effect_fallback"
  }
  list(
    model_id = scope_row$model_id,
    scope_type = scope_row$scope_type,
    scope_value = scope_row$scope_value,
    fit_type = fit_type,
    abundance_model = abundance_model,
    composition_model = composition_model,
    n_sites = nrow(abundance),
    n_pairs = nrow(composition),
    pressure_classes = intersect(levels(abundance$pressure_class), levels(composition$pressure_class))
  )
}

describe_v2_model <- function(model_record, component) {
  model <- model_record[[paste0(component, "_model")]]
  if (!inherits(model, "merMod")) {
    return(data.table(
      model_id = model_record$model_id, scope_type = model_record$scope_type,
      scope_value = model_record$scope_value, component = component,
      fit_type = model_record$fit_type, singular = NA, convergence_messages = NA_character_
    ))
  }
  messages <- model@optinfo$conv$lme4$messages
  data.table(
    model_id = model_record$model_id, scope_type = model_record$scope_type,
    scope_value = model_record$scope_value, component = component,
    fit_type = model_record$fit_type, singular = lme4::isSingular(model, tol = 1e-4),
    convergence_messages = if (length(messages)) paste(messages, collapse = "; ") else NA_character_
  )
}

fit_bii_v2_models <- function() {
  assert_file_exists(predicts_site_path, "prepared V1 PREDICTS site table")
  assert_file_exists(predicts_composition_path, "prepared PREDICTS composition table")
  v1_site <- as.data.table(readRDS(predicts_site_path))
  composition <- as.data.table(readRDS(predicts_composition_path))
  prepared <- prepare_bii_v2_site_data(v1_site)
  site <- prepared$site
  pairs <- prepare_bii_v2_pairs(site, composition)
  scopes <- make_model_scopes(site, pairs)
  records <- vector("list", nrow(scopes))
  for (i in seq_len(nrow(scopes))) {
    scope_row <- scopes[i]
    message("Fitting V2 BII model: ", scope_row$model_id)
    records[[i]] <- tryCatch(
      fit_scope_models_v2(site, pairs, scope_row),
      error = function(error) {
        warning("Skipping V2 ", scope_row$model_id, ": ", conditionMessage(error), call. = FALSE)
        NULL
      }
    )
  }
  records <- Filter(Negate(is.null), records)
  if (length(records) == 0) {
    stop("No V2 BII models could be fitted; inspect V2 pressure coverage and model diagnostics.", call. = FALSE)
  }
  saveRDS(list(models = records, predictor_spec = prepared$predictor_spec), v2_model_bundle_path, compress = "gzip")
  write_csv_safe(prepared$predictor_spec, v2_predictor_spec_path)
  summary <- rbindlist(lapply(records, function(record) data.table(
    model_id = record$model_id, scope_type = record$scope_type, scope_value = record$scope_value,
    fit_type = record$fit_type, n_sites = record$n_sites, n_pairs = record$n_pairs,
    pressure_classes = paste(record$pressure_classes, collapse = ";")
  )))
  write_csv_safe(summary, file.path(output_dir, "bii_v2_model_summary.csv"))
  diagnostics <- rbindlist(unlist(lapply(records, function(record) list(
    describe_v2_model(record, "abundance"), describe_v2_model(record, "composition")
  )), recursive = FALSE))
  write_csv_safe(diagnostics, file.path(output_dir, "bii_v2_model_diagnostics.csv"))
  coverage <- site[, .(n_sites = .N, n_studies = uniqueN(study_id)), by = pressure_class]
  write_csv_safe(coverage, file.path(output_dir, "bii_v2_predicts_pressure_coverage.csv"))
  invisible(list(models = records, predictor_spec = prepared$predictor_spec))
}

prepare_bii_v2_spatial_pressures <- function(landuse, pressure_path, label, predictor_spec) {
  pressure_required <- v2_spatial_pressure_columns
  if (isTRUE(v2_include_secondary_age)) pressure_required <- c(pressure_required, "secondary_age_years")
  pressure <- read_v2_pressure_table(
    pressure_path, pressure_required, paste(label, "V2 spatial-pressure input"),
    c("scenario", "cell_id", "year")
  )
  pressure[, `:=`(scenario = as.character(scenario), cell_id = as.character(cell_id), year = as.integer(year))]
  input <- copy(landuse)
  input[, `:=`(scenario = as.character(scenario), cell_id = as.character(cell_id), year = as.integer(year))]
  keys <- c("scenario", "cell_id", "year")
  output <- merge(input, pressure, by = keys, all.x = TRUE, sort = FALSE)
  # Secondary age is a cell-level description of secondary vegetation. It is
  # therefore zero for every other land-use/intensity share in that cell before
  # applying the site-trained transformation and scaling.
  if (isTRUE(v2_include_secondary_age)) {
    output[!grepl("^secondary__", pressure_class), secondary_age_years := 0]
  }
  output <- v2_apply_predictor_spec(output, predictor_spec)
  scaled_columns <- predictor_spec$scaled_variable
  if (any(!stats::complete.cases(output[, ..scaled_columns]))) {
    stop(label, " V2 spatial-pressure input does not cover every land-use cell/year.", call. = FALSE)
  }
  output
}

v2_baseline_predictors <- function(predictor_spec) {
  raw <- as.data.table(setNames(as.list(rep(0, nrow(predictor_spec))), predictor_spec$raw_variable))
  v2_apply_predictor_spec(raw, predictor_spec)
}

v2_predict_components <- function(model_record, input, predictor_spec) {
  available <- model_record$pressure_classes
  unsupported <- setdiff(unique(input$pressure_class), available)
  if (length(unsupported) > 0) {
    return(list(unsupported = unsupported))
  }
  abundance_levels <- levels(stats::model.frame(model_record$abundance_model)$pressure_class)
  composition_levels <- levels(stats::model.frame(model_record$composition_model)$pressure_class)
  predictor_columns <- predictor_spec$scaled_variable
  abundance_new <- as.data.frame(input[, ..predictor_columns])
  abundance_new$pressure_class <- factor(input$pressure_class, levels = abundance_levels)
  if (isTRUE(v2_include_study_mean_hpd_control)) abundance_new$study_mean_hpd_scaled <- 0
  composition_new <- as.data.frame(input[, ..predictor_columns])
  composition_new$pressure_class <- factor(input$pressure_class, levels = composition_levels)
  composition_new$log10_geographic_distance <- 0
  abundance <- predict_no_random_effects(model_record$abundance_model, abundance_new)^2
  composition <- inverse_adjusted_logit(predict_no_random_effects(model_record$composition_model, composition_new))

  baseline <- v2_baseline_predictors(predictor_spec)
  baseline_abundance <- as.data.frame(baseline[, ..predictor_columns])
  baseline_abundance$pressure_class <- factor(reference_pressure_class, levels = abundance_levels)
  if (isTRUE(v2_include_study_mean_hpd_control)) baseline_abundance$study_mean_hpd_scaled <- 0
  baseline_composition <- as.data.frame(baseline[, ..predictor_columns])
  baseline_composition$pressure_class <- factor(reference_pressure_class, levels = composition_levels)
  baseline_composition$log10_geographic_distance <- 0
  abundance_baseline <- predict_no_random_effects(model_record$abundance_model, baseline_abundance)^2
  composition_baseline <- inverse_adjusted_logit(
    predict_no_random_effects(model_record$composition_model, baseline_composition)
  )
  if (!is.finite(abundance_baseline) || !is.finite(composition_baseline) ||
      abundance_baseline <= 0 || composition_baseline <= 0) {
    stop("V2 model ", model_record$model_id, " has no valid reference prediction.", call. = FALSE)
  }
  list(
    abundance_relative = abundance / abundance_baseline,
    composition_relative = composition / composition_baseline,
    unsupported = character()
  )
}

v2_predict_components_with_global_fallback <- function(model_record, global_record, input, predictor_spec) {
  fallback_pressure_classes <- setdiff(unique(input$pressure_class), model_record$pressure_classes)
  if (length(fallback_pressure_classes) == 0L) {
    output <- v2_predict_components(model_record, input, predictor_spec)
    output$fallback_pressure_classes <- character()
    return(output)
  }
  unavailable <- setdiff(fallback_pressure_classes, global_record$pressure_classes)
  if (length(unavailable) > 0L) {
    stop("Neither scoped nor global V2 model supports: ", paste(unavailable, collapse = ", "), call. = FALSE)
  }
  abundance <- numeric(nrow(input))
  composition <- numeric(nrow(input))
  for (pressure_class_value in unique(input$pressure_class)) {
    rows <- which(input$pressure_class == pressure_class_value)
    selected_model <- if (pressure_class_value %in% model_record$pressure_classes) model_record else global_record
    prediction <- v2_predict_components(selected_model, input[rows], predictor_spec)
    abundance[rows] <- prediction$abundance_relative
    composition[rows] <- prediction$composition_relative
  }
  list(
    abundance_relative = abundance,
    composition_relative = composition,
    unsupported = character(),
    fallback_pressure_classes = fallback_pressure_classes
  )
}

project_bii_landuse_v2 <- function(landuse, pressure_path, projection_name) {
  assert_file_exists(v2_model_bundle_path, "V2 fitted model bundle")
  bundle <- readRDS(v2_model_bundle_path)
  input <- prepare_bii_v2_spatial_pressures(landuse, pressure_path, projection_name, bundle$predictor_spec)
  global_records <- Filter(function(record) identical(record$model_id, "global__all"), bundle$models)
  if (length(global_records) != 1L) stop("V2 model bundle requires one global__all model for fallback.", call. = FALSE)
  global_record <- global_records[[1L]]
  outputs <- list()
  for (i in seq_along(bundle$models)) {
    record <- bundle$models[[i]]
    current <- copy(input)
    if (record$scope_type == "region") current <- current[region == record$scope_value]
    if (nrow(current) == 0) next
    components <- v2_predict_components_with_global_fallback(record, global_record, current, bundle$predictor_spec)
    current[, `:=`(
      abundance_relative = components$abundance_relative,
      composition_relative = components$composition_relative
    )]
    cell_keys <- c("scenario", "cell_id", "year", "region", "country", "continent", "area_km2")
    outputs[[i]] <- current[, .(
      relative_abundance = sum(share * abundance_relative),
      compositional_similarity = sum(share * composition_relative)
    ), by = cell_keys][, `:=`(
      model_id = record$model_id, scope_type = record$scope_type, scope_value = record$scope_value,
      projection_name = projection_name,
      response_mode = if (length(components$fallback_pressure_classes)) "hybrid_global_fallback" else "strict",
      fallback_pressure_classes = paste(components$fallback_pressure_classes, collapse = ";"),
      bii = relative_abundance * compositional_similarity
    )]
  }
  cells <- rbindlist(outputs, fill = TRUE)
  if (nrow(cells) == 0) stop("No V2 BII cells were projected.", call. = FALSE)
  cells[, bii_bounded := pmin(1, pmax(0, bii))]
  write_csv_safe(cells, file.path(output_dir, paste0("bii_v2_", projection_name, "_cells.csv")))
  weighted <- function(values, weights) {
    if (isTRUE(require_area_weight)) weighted.mean(values, weights, na.rm = TRUE) else mean(values, na.rm = TRUE)
  }
  summarise_groups <- function(groups) cells[, .(
    bii = weighted(bii, area_km2),
    bii_bounded = weighted(bii_bounded, area_km2),
    n_cells = .N,
    total_area_km2 = sum(area_km2)
  ), by = groups]
  summary_keys <- c(
    "projection_name", "model_id", "scope_type", "scope_value", "response_mode",
    "fallback_pressure_classes", "scenario", "year"
  )
  global <- summarise_groups(summary_keys)
  region <- summarise_groups(c(summary_keys, "region"))
  country <- summarise_groups(c(summary_keys, "country"))
  continent <- summarise_groups(c(summary_keys, "continent"))
  write_csv_safe(global, file.path(output_dir, paste0("bii_v2_", projection_name, "_global.csv")))
  write_csv_safe(region, file.path(output_dir, paste0("bii_v2_", projection_name, "_regions.csv")))
  write_csv_safe(country, file.path(output_dir, paste0("bii_v2_", projection_name, "_countries.csv")))
  write_csv_safe(continent, file.path(output_dir, paste0("bii_v2_", projection_name, "_continents.csv")))
  invisible(list(cells = cells, global = global, region = region, country = country, continent = continent))
}
