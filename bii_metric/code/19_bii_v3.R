# -----------------------------------------------------------------------------
# V3: Hill (2018)-style strict PREDICTS re-curation and detailed LUH2 classes
# -----------------------------------------------------------------------------
# This is an opt-in comparison pathway.  It does not alter V1 or V2 artefacts.
# It retains only sites that the reproducible re-curation can assign to a
# detailed LUH2 class and use-intensity level.  Primary forest and non-forest
# are retained in the spatial data but combined as primary vegetation for the
# response model, matching the BII reference concept.

v3_make_site_key <- function(study_id, site_id) {
  study_id <- trimws(as.character(study_id))
  site_id <- trimws(as.character(site_id))
  source_id <- sub(" [^ ]+$", "", study_id)
  study_number <- sub("^.* ", "", study_id)
  site_number <- sub("^.*[[:space:]]+", "", site_id)
  paste(source_id, study_number, site_number, sep = "::")
}

v3_make_pressure_class <- function(land_use, intensity) {
  ifelse(
    land_use == "primary" & intensity == "minimal", v3_reference_pressure_class,
    ifelse(!is.na(land_use) & !is.na(intensity), paste(land_use, intensity, sep = "__"), NA_character_)
  )
}

v3_prepare_predicts <- function() {
  assert_file_exists(predicts_raw_path, "raw PREDICTS extract")
  assert_file_exists(hill2018_site_recuration_path, "Hill 2018 site re-curation")
  raw <- data.table::as.data.table(readRDS(predicts_raw_path))
  recuration <- data.table::as.data.table(readRDS(hill2018_site_recuration_path))
  required <- c(
    "Diversity_metric_type", "SS", "SSB", "SSBS", "Biome", "Longitude", "Latitude",
    "Taxon_name_entered", "Measurement", "Effort_corrected_measurement", "Sampling_effort",
    "Kingdom", "Class", "UN_region", "Country"
  )
  assert_has_cols(raw, required, "Raw PREDICTS data")
  recuration <- recuration[
    recuration_status == "retained" & hill2018_land_use != "excluded_timber_plantation" &
      !is.na(hill2018_land_use) & !is.na(hill2018_intensity),
    .(site_key, hill2018_land_use, hill2018_intensity)
  ]
  if (anyDuplicated(recuration$site_key)) stop("Hill re-curation has duplicate site keys.", call. = FALSE)

  dt <- raw[
    Diversity_metric_type == "Abundance" & !is.na(Biome) & nzchar(trimws(as.character(Biome))) &
      !is.na(SS) & !is.na(SSB) & !is.na(SSBS) &
      is.finite(as.numeric(Effort_corrected_measurement)) & is.finite(as.numeric(Measurement))
  ]
  dt[, `:=`(
    study_id = as.character(SS), block_id = as.character(SSB), site_id = as.character(SSBS),
    taxon_group = make_bii_taxon_group(Kingdom, Class),
    region = fifelse(!is.na(UN_region) & nzchar(trimws(as.character(UN_region))), as.character(UN_region), "Unknown"),
    country = clean_chr(Country), longitude = as.numeric(Longitude), latitude = as.numeric(Latitude),
    sampling_effort = as.numeric(Sampling_effort), effort_corrected = as.numeric(Effort_corrected_measurement),
    raw_measurement = as.numeric(Measurement), species_id = clean_chr(Taxon_name_entered)
  )]
  dt[, site_key := v3_make_site_key(study_id, site_id)]
  dt <- merge(dt, recuration, by = "site_key", all = FALSE, sort = FALSE)
  dt[, land_use := fifelse(hill2018_land_use %in% c("primary_forest", "primary_nonforest"),
                           "primary", hill2018_land_use)]
  dt[, intensity := hill2018_intensity]
  dt[, pressure_class := v3_make_pressure_class(land_use, intensity)]
  dt <- dt[!is.na(pressure_class) & !is.na(species_id) & !is.na(taxon_group)]

  site <- dt[, .(
    total_abundance = sum(effort_corrected, na.rm = TRUE), pressure_class = first_present(pressure_class),
    block_id = first_present(block_id), land_use = first_present(land_use), intensity = first_present(intensity),
    taxon_group = first_present(taxon_group), region = first_present(region), country = first_present(country),
    longitude = suppressWarnings(as.numeric(first_present(as.character(longitude)))),
    latitude = suppressWarnings(as.numeric(first_present(as.character(latitude)))),
    sampling_effort = suppressWarnings(as.numeric(first_present(as.character(sampling_effort))))
  ), by = .(study_id, site_id)]
  site <- site[is.finite(total_abundance) & total_abundance >= 0]
  site[, max_study_abundance := max(total_abundance, na.rm = TRUE), by = study_id]
  site <- site[is.finite(max_study_abundance) & max_study_abundance > 0]
  site[, relative_abundance := total_abundance / max_study_abundance]
  site[, sqrt_relative_abundance := sqrt(relative_abundance)]
  composition <- dt[, .(raw_measurement = sum(raw_measurement, na.rm = TRUE)), by = .(study_id, site_id, species_id)]
  saveRDS(site, v3_site_path, compress = "gzip")
  saveRDS(composition, v3_composition_path, compress = "gzip")
  list(site = site, composition = composition)
}

v3_add_site_population <- function(site) {
  if (!requireNamespace("terra", quietly = TRUE)) stop("V3 requires the R package 'terra'.", call. = FALSE)
  assert_file_exists(v3_population_2010_path, "V3 2010 population grid")
  population <- terra::rast(v3_population_2010_path)
  area <- terra::cellSize(population, unit = "km")
  points <- terra::vect(as.data.frame(site[, .(longitude, latitude)]),
                        geom = c("longitude", "latitude"), crs = "OGC:CRS84")
  count <- terra::extract(population, points, method = "bilinear")[[2L]]
  cell_area <- terra::extract(area, points, method = "bilinear")[[2L]]
  site[, human_population_density := as.numeric(count) / as.numeric(cell_area)]
  site[is.finite(human_population_density) & human_population_density >= 0]
}

v3_make_predictor_spec <- function(site) {
  transformed <- log1p(site$human_population_density)
  data.table::data.table(
    raw_variable = "human_population_density", scaled_variable = "human_population_density_scaled",
    transformation = "log1p", training_lower_transformed = min(transformed),
    training_upper_transformed = max(transformed), training_center_transformed = mean(transformed),
    training_scale_transformed = stats::sd(transformed)
  )
}

v3_apply_predictor_spec <- function(data, spec) {
  output <- data.table::copy(data)
  values <- pmin(spec$training_upper_transformed[[1L]],
                 pmax(spec$training_lower_transformed[[1L]], log1p(as.numeric(output$human_population_density))))
  output[, human_population_density_scaled :=
           (values - spec$training_center_transformed[[1L]]) / spec$training_scale_transformed[[1L]]]
  output
}

v3_model_scopes <- function(site, pairs) {
  scopes <- list(data.table::data.table(model_id = "global__all", scope_type = "global", scope_value = "all"))
  if (isTRUE(fit_taxon_models)) {
    taxa <- site[, .N, by = taxon_group][N >= minimum_taxon_sites, taxon_group]
    taxa <- intersect(taxa, taxon_groups_to_fit)
    scopes[[length(scopes) + 1L]] <- data.table::data.table(
      model_id = paste0("taxon__", make.names(taxa)), scope_type = "taxon", scope_value = taxa
    )
  }
  if (isTRUE(fit_region_models)) {
    regions <- site[, .N, by = region][N >= minimum_region_sites, region]
    scopes[[length(scopes) + 1L]] <- data.table::data.table(
      model_id = paste0("region__", make.names(regions)), scope_type = "region", scope_value = regions
    )
  }
  data.table::rbindlist(scopes, fill = TRUE)
}

v3_fit_scope <- function(site, pairs, scope_row) {
  data <- scope_data(site, pairs, scope_row$scope_type, scope_row$scope_value)
  abundance <- data$site
  composition <- data$pairs
  if (nrow(abundance) < v3_minimum_sites_per_scope || nrow(composition) < v3_minimum_pairs_per_scope ||
      !v3_reference_pressure_class %in% abundance$pressure_class ||
      !v3_reference_pressure_class %in% composition$pressure_class) return(NULL)
  abundance[, pressure_class := stats::relevel(factor(pressure_class), ref = v3_reference_pressure_class)]
  composition[, pressure_class := stats::relevel(factor(pressure_class), ref = v3_reference_pressure_class)]
  abundance_formula <- sqrt_relative_abundance ~ pressure_class * human_population_density_scaled +
    study_mean_hpd_scaled + (1 | study_id) + (1 | block_id)
  composition_formula <- logit_similarity ~ pressure_class * human_population_density_scaled +
    log10_geographic_distance + (1 | study_id) + (1 | target_site_id)
  if (has_lme4) {
    abundance_model <- lme4::lmer(abundance_formula, data = abundance, REML = TRUE)
    composition_model <- lme4::lmer(composition_formula, data = composition, REML = TRUE)
    fit_type <- "mixed_effects_lme4"
  } else {
    abundance_model <- stats::lm(sqrt_relative_abundance ~ pressure_class * human_population_density_scaled + study_mean_hpd_scaled,
                                 data = abundance)
    composition_model <- stats::lm(logit_similarity ~ pressure_class * human_population_density_scaled + log10_geographic_distance,
                                   data = composition)
    fit_type <- "fixed_effect_fallback"
  }
  list(model_id = scope_row$model_id, scope_type = scope_row$scope_type, scope_value = scope_row$scope_value,
       fit_type = fit_type, abundance_model = abundance_model, composition_model = composition_model,
       n_sites = nrow(abundance), n_pairs = nrow(composition),
       pressure_classes = intersect(levels(abundance$pressure_class), levels(composition$pressure_class)))
}

fit_bii_v3_models <- function() {
  old_reference <- reference_pressure_class
  reference_pressure_class <<- v3_reference_pressure_class
  on.exit(reference_pressure_class <<- old_reference, add = TRUE)
  prepared <- v3_prepare_predicts()
  site <- v3_add_site_population(prepared$site)
  spec <- v3_make_predictor_spec(site)
  site <- v3_apply_predictor_spec(site, spec)
  site[, study_mean_hpd_scaled := mean(human_population_density_scaled), by = study_id]
  pairs <- build_composition_pairs(site, prepared$composition)
  target <- site[, .(study_id, target_site_id = site_id, human_population_density_scaled)]
  pairs <- merge(pairs, target, by = c("study_id", "target_site_id"), all.x = TRUE, sort = FALSE)
  scopes <- v3_model_scopes(site, pairs)
  records <- lapply(seq_len(nrow(scopes)), function(i) {
    current <- scopes[i]
    message("Fitting V3 BII model: ", current$model_id)
    tryCatch(v3_fit_scope(site, pairs, current), error = function(error) {
      warning("Skipping V3 ", current$model_id, ": ", conditionMessage(error), call. = FALSE); NULL
    })
  })
  records <- Filter(Negate(is.null), records)
  if (!length(records)) stop("No V3 models could be fitted.", call. = FALSE)
  mixture <- site[, .N, by = .(land_use, intensity)][, allocation_share := N / sum(N), by = land_use][
    , .(land_use, intensity, allocation_share, n_recurated_sites = N)]
  data.table::setorder(mixture, land_use, intensity)
  saveRDS(list(models = records, predictor_spec = spec), v3_model_bundle_path, compress = "gzip")
  write_csv_safe(spec, v3_predictor_spec_path)
  write_csv_safe(mixture, v3_intensity_mixture_path)
  write_csv_safe(data.table::rbindlist(lapply(records, function(record) data.table::data.table(
    model_id = record$model_id, scope_type = record$scope_type, scope_value = record$scope_value,
    fit_type = record$fit_type, n_sites = record$n_sites, n_pairs = record$n_pairs,
    pressure_classes = paste(record$pressure_classes, collapse = ";")
  ))), file.path(output_dir, "bii_v3_model_summary.csv"))
  write_csv_safe(site[, .(n_sites = .N, n_studies = data.table::uniqueN(study_id)), by = .(land_use, intensity)],
                 file.path(output_dir, "bii_v3_predicts_pressure_coverage.csv"))
  invisible(list(models = records, predictor_spec = spec))
}

v3_predict_components <- function(record, input, spec) {
  if (length(setdiff(unique(input$pressure_class), record$pressure_classes))) return(NULL)
  abundance_levels <- levels(stats::model.frame(record$abundance_model)$pressure_class)
  composition_levels <- levels(stats::model.frame(record$composition_model)$pressure_class)
  abundance_new <- data.frame(human_population_density_scaled = input$human_population_density_scaled,
                              pressure_class = factor(input$pressure_class, levels = abundance_levels),
                              study_mean_hpd_scaled = 0)
  composition_new <- data.frame(human_population_density_scaled = input$human_population_density_scaled,
                                pressure_class = factor(input$pressure_class, levels = composition_levels),
                                log10_geographic_distance = 0)
  baseline <- data.frame(human_population_density_scaled = 0,
                         pressure_class = factor(v3_reference_pressure_class, levels = abundance_levels),
                         study_mean_hpd_scaled = 0)
  baseline_composition <- data.frame(human_population_density_scaled = 0,
                                     pressure_class = factor(v3_reference_pressure_class, levels = composition_levels),
                                     log10_geographic_distance = 0)
  abundance_base <- predict_no_random_effects(record$abundance_model, baseline)^2
  similarity_base <- inverse_adjusted_logit(predict_no_random_effects(record$composition_model, baseline_composition))
  list(abundance_relative = predict_no_random_effects(record$abundance_model, abundance_new)^2 / abundance_base,
       composition_relative = inverse_adjusted_logit(predict_no_random_effects(record$composition_model, composition_new)) / similarity_base)
}

v3_predict_with_fallback <- function(record, global_record, input, spec) {
  abundance <- numeric(nrow(input)); composition <- numeric(nrow(input)); fallback <- character()
  for (pressure in unique(input$pressure_class)) {
    rows <- which(input$pressure_class == pressure)
    selected <- if (pressure %in% record$pressure_classes) record else global_record
    if (!identical(selected$model_id, record$model_id)) fallback <- c(fallback, pressure)
    predicted <- v3_predict_components(selected, input[rows], spec)
    if (is.null(predicted)) stop("V3 global model lacks pressure class: ", pressure, call. = FALSE)
    abundance[rows] <- predicted$abundance_relative
    composition[rows] <- predicted$composition_relative
  }
  list(abundance_relative = abundance, composition_relative = composition,
       fallback_pressure_classes = sort(unique(fallback)))
}

project_bii_landuse_v3 <- function(scope_types = c("global", "taxon"), output_tag = "luh2") {
  assert_file_exists(v3_model_bundle_path, "V3 fitted model bundle")
  assert_file_exists(v3_luh2_landuse_path, "V3 LUH2 land-use input")
  assert_file_exists(v3_luh2_pressure_path, "V3 LUH2 pressure input")
  bundle <- readRDS(v3_model_bundle_path)
  landuse <- data.table::as.data.table(readr::read_csv(v3_luh2_landuse_path, show_col_types = FALSE))
  pressure <- data.table::as.data.table(readr::read_csv(v3_luh2_pressure_path, show_col_types = FALSE))
  keys <- c("scenario", "cell_id", "year")
  input <- merge(landuse, pressure, by = keys, all.x = TRUE, sort = FALSE)
  input <- v3_apply_predictor_spec(input, bundle$predictor_spec)
  if (any(!is.finite(input$human_population_density_scaled))) stop("V3 pressure input has missing population density.", call. = FALSE)
  global_record <- Filter(function(x) identical(x$model_id, "global__all"), bundle$models)[[1L]]
  selected_records <- Filter(function(x) x$scope_type %in% scope_types, bundle$models)
  if (!length(selected_records)) stop("No V3 models match the requested projection scope.", call. = FALSE)
  cells <- data.table::rbindlist(lapply(selected_records, function(record) {
    current <- data.table::copy(input)
    if (record$scope_type == "region") current <- current[region == record$scope_value]
    if (!nrow(current)) return(NULL)
    prediction <- v3_predict_with_fallback(record, global_record, current, bundle$predictor_spec)
    current[, `:=`(abundance_relative = prediction$abundance_relative,
                   composition_relative = prediction$composition_relative)]
    current[, .(relative_abundance = sum(share * abundance_relative),
                compositional_similarity = sum(share * composition_relative)),
            by = .(scenario, cell_id, year, region, country, continent, area_km2)][,
              `:=`(model_id = record$model_id, scope_type = record$scope_type,
                   scope_value = record$scope_value,
                   response_mode = if (length(prediction$fallback_pressure_classes)) "hybrid_global_fallback" else "strict",
                   fallback_pressure_classes = paste(prediction$fallback_pressure_classes, collapse = ";"),
                   bii = relative_abundance * compositional_similarity)]
  }), fill = TRUE)
  cells[, bii_bounded := pmin(1, pmax(0, bii))]
  weighted <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
  common <- c("model_id", "scope_type", "scope_value", "response_mode", "fallback_pressure_classes", "scenario", "year")
  summarise <- function(groups) cells[, .(bii = weighted(bii, area_km2), bii_bounded = weighted(bii_bounded, area_km2),
                                          n_cells = .N, total_area_km2 = sum(area_km2)), by = groups]
  outputs <- list(cells = cells, global = summarise(common), regions = summarise(c(common, "region")),
                  countries = summarise(c(common, "country")), continents = summarise(c(common, "continent")))
  for (name in names(outputs)) write_csv_safe(outputs[[name]], file.path(output_dir, paste0("bii_v3_", output_tag, "_", name, ".csv")))
  if ("taxon" %in% scope_types) {
    taxa <- outputs$global[scope_type == "taxon", .(
      taxon_group = scope_value, scenario, year, response_mode, fallback_pressure_classes,
      bii, bii_bounded, n_cells, total_area_km2
    )]
    write_csv_safe(taxa, file.path(output_dir, paste0("bii_v3_", output_tag, "_taxa.csv")))
  }
  write_csv_safe(data.table::data.table(
    setting = c("projection_grid", "secondary_age_method", "secondary_age_start_year", "intensity_allocation", "population"),
    value = c("1 degree, area-weighted aggregation of 0.25-degree LUH2", "net_change_proxy_from_states",
              "1960; all pre-1960 secondary vegetation is mature", "strict-recuration site-count mixture by detailed land-use class",
              "NCAR SSP2 total population for 2010, 2020, and 2030")
  ), file.path(output_dir, paste0("bii_v3_", output_tag, "_metadata.csv")))
  invisible(outputs)
}
