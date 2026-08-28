assert_file_exists <- function(path, label) {
  if (!file.exists(path)) stop("Missing ", label, ": ", path, call. = FALSE)
}

assert_has_cols <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    stop(label, " is missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

write_csv_safe <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, path, na = "")
  message("Wrote: ", path)
  invisible(path)
}

clean_chr <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

first_present <- function(x) {
  x <- x[!is.na(x) & nzchar(as.character(x))]
  if (length(x) == 0) NA_character_ else as.character(x[[1]])
}

normalise_land_use <- function(land_use) {
  x <- tolower(clean_chr(land_use))
  out <- rep(NA_character_, length(x))
  out[x == "primary vegetation"] <- "primary_other"
  out[grepl("secondary vegetation", x, fixed = TRUE)] <- "secondary"
  out[x == "plantation forest"] <- "plantation"
  out[x == "cropland"] <- "cropland"
  out[x == "pasture"] <- "pasture"
  out[x == "urban"] <- "urban"
  out
}

normalise_intensity <- function(intensity) {
  x <- tolower(clean_chr(intensity))
  out <- rep(NA_character_, length(x))
  out[x %in% c("minimal use", "minimal")] <- "minimal"
  out[x %in% c("light use", "light")] <- "light"
  out[x %in% c("intense use", "intense")] <- "intense"
  out
}

make_bii_taxon_group <- function(kingdom, taxon_class) {
  kingdom <- tolower(clean_chr(kingdom))
  taxon_class <- clean_chr(taxon_class)
  out <- rep(NA_character_, length(kingdom))

  out[kingdom == "plantae"] <- "Plants"
  out[kingdom == "fungi"] <- "Fungi"
  out[kingdom == "animalia"] <- "Invertebrates"
  out[kingdom == "animalia" & taxon_class == "Mammalia"] <- "Mammals"
  out[kingdom == "animalia" & taxon_class == "Aves"] <- "Birds"
  out[kingdom == "animalia" & taxon_class == "Amphibia"] <- "Amphibians"
  out[kingdom == "animalia" & taxon_class == "Reptilia"] <- "Reptiles"
  out
}

make_pressure_class <- function(land_use, intensity, variant = model_variant) {
  land_use <- as.character(land_use)
  intensity <- as.character(intensity)
  out <- land_use
  is_reference <- land_use == "primary_other" & intensity == "minimal"
  out[is_reference] <- reference_pressure_class
  if (identical(variant, "land_use_intensity")) {
    non_reference <- !is_reference & !is.na(land_use) & !is.na(intensity)
    out[non_reference] <- paste(land_use[non_reference], intensity[non_reference], sep = "__")
    out[!is_reference & is.na(intensity)] <- NA_character_
  }
  out
}

adjusted_logit <- function(x, adjustment = similarity_adjustment) {
  x <- pmin(1, pmax(0, as.numeric(x)))
  qlogis((x + adjustment) / (1 + 2 * adjustment))
}

inverse_adjusted_logit <- function(x, adjustment = similarity_adjustment) {
  pmin(1, pmax(0, plogis(x) * (1 + 2 * adjustment) - adjustment))
}

haversine_km <- function(lon_1, lat_1, lon_2, lat_2) {
  rad <- pi / 180
  lat_1 <- lat_1 * rad
  lat_2 <- lat_2 * rad
  dlat <- lat_2 - lat_1
  dlon <- (lon_2 - lon_1) * rad
  a <- sin(dlat / 2)^2 + cos(lat_1) * cos(lat_2) * sin(dlon / 2)^2
  6371.0088 * 2 * atan2(sqrt(a), sqrt(1 - a))
}

prepare_predicts_records <- function(raw_predicts) {
  required <- c(
    "Diversity_metric_type", "SS", "SSB", "SSBS", "Predominant_land_use", "Use_intensity",
    "Biome", "Longitude", "Latitude", "Taxon_name_entered", "Measurement",
    "Effort_corrected_measurement", "Sampling_effort", "Kingdom", "Class", "UN_region", "Country"
  )
  assert_has_cols(raw_predicts, required, "Raw PREDICTS data")
  dt <- data.table::as.data.table(raw_predicts)
  dt <- dt[
    Diversity_metric_type == "Abundance" &
      !is.na(Biome) & nzchar(trimws(as.character(Biome))) &
      !is.na(SS) & !is.na(SSB) & !is.na(SSBS) &
      is.finite(as.numeric(Effort_corrected_measurement)) &
      is.finite(as.numeric(Measurement))
  ]

  dt[, `:=`(
    study_id = as.character(SS),
    block_id = as.character(SSB),
    site_id = as.character(SSBS),
    land_use = normalise_land_use(Predominant_land_use),
    intensity = normalise_intensity(Use_intensity),
    taxon_group = make_bii_taxon_group(Kingdom, Class),
    region = fifelse(
      !is.na(UN_region) & nzchar(trimws(as.character(UN_region))),
      as.character(UN_region),
      "Unknown"
    ),
    country = clean_chr(Country),
    longitude = as.numeric(Longitude),
    latitude = as.numeric(Latitude),
    sampling_effort = as.numeric(Sampling_effort),
    effort_corrected = as.numeric(Effort_corrected_measurement),
    raw_measurement = as.numeric(Measurement),
    species_id = clean_chr(Taxon_name_entered)
  )]
  dt[, pressure_class := make_pressure_class(land_use, intensity)]
  dt <- dt[!is.na(pressure_class) & !is.na(species_id) & !is.na(taxon_group)]

  site <- dt[, .(
    total_abundance = sum(effort_corrected, na.rm = TRUE),
    pressure_class = first_present(pressure_class),
    block_id = first_present(block_id),
    land_use = first_present(land_use),
    intensity = first_present(intensity),
    taxon_group = first_present(taxon_group),
    region = first_present(region),
    country = first_present(country),
    longitude = suppressWarnings(as.numeric(first_present(as.character(longitude)))),
    latitude = suppressWarnings(as.numeric(first_present(as.character(latitude)))),
    sampling_effort = suppressWarnings(as.numeric(first_present(as.character(sampling_effort))))
  ), by = .(study_id, site_id)]
  site <- site[is.finite(total_abundance) & total_abundance >= 0]
  site[, max_study_abundance := max(total_abundance, na.rm = TRUE), by = study_id]
  site <- site[is.finite(max_study_abundance) & max_study_abundance > 0]
  site[, relative_abundance := total_abundance / max_study_abundance]
  site[, sqrt_relative_abundance := sqrt(relative_abundance)]

  composition <- dt[, .(raw_measurement = sum(raw_measurement, na.rm = TRUE)),
                    by = .(study_id, site_id, species_id)]
  list(site = site, composition = composition)
}

balanced_bray_similarity <- function(x, y) {
  species <- union(names(x), names(y))
  x_values <- x[match(species, names(x))]
  y_values <- y[match(species, names(y))]
  x_values[is.na(x_values)] <- 0
  y_values[is.na(y_values)] <- 0
  if (sum(x_values) == 0 && sum(y_values) == 0) return(NA_real_)
  shared <- sum(pmin(x_values, y_values))
  excess_x <- sum(x_values) - shared
  excess_y <- sum(y_values) - shared
  denominator <- 2 * shared + excess_x + excess_y
  if (denominator <= 0) return(NA_real_)
  1 - (2 * min(excess_x, excess_y) / denominator)
}

build_composition_pairs <- function(site, composition) {
  eligible <- copy(site)[
    is.finite(longitude) & is.finite(latitude) & is.finite(sampling_effort)
  ]
  eligible[, n_efforts := uniqueN(sampling_effort), by = study_id]
  eligible <- eligible[n_efforts == 1L]
  eligible[, n_primary := sum(pressure_class == reference_pressure_class), by = study_id]
  eligible <- eligible[n_primary > 0]
  study_ids <- unique(eligible$study_id)
  pair_tables <- vector("list", length(study_ids))
  set.seed(random_seed)

  for (i in seq_along(study_ids)) {
    current_study <- study_ids[[i]]
    meta <- eligible[study_id == current_study]
    baseline <- meta[pressure_class == reference_pressure_class, site_id]
    targets <- meta$site_id
    pair_index <- CJ(baseline_site_id = baseline, target_site_id = targets, unique = TRUE)
    pair_index <- pair_index[baseline_site_id != target_site_id]
    if (is.finite(max_pairs_per_study) && nrow(pair_index) > max_pairs_per_study) {
      pair_index <- pair_index[sample(.N, max_pairs_per_study)]
    }
    if (nrow(pair_index) == 0) next

    records <- composition[study_id == current_study & site_id %in% unique(c(baseline, targets))]
    vectors <- split(records$raw_measurement, records$site_id)
    species <- split(records$species_id, records$site_id)
    vectors <- Map(function(values, names) stats::setNames(values, names), vectors, species)
    pair_index[, similarity := mapply(
      function(baseline_site_id, target_site_id) {
        balanced_bray_similarity(vectors[[baseline_site_id]], vectors[[target_site_id]])
      },
      baseline_site_id,
      target_site_id
    )]
    pair_index <- merge(
      pair_index,
      meta[, .(
        target_site_id = site_id, pressure_class, taxon_group, region,
        target_longitude = longitude, target_latitude = latitude
      )],
      by = "target_site_id",
      all.x = TRUE,
      sort = FALSE
    )
    pair_index <- merge(
      pair_index,
      meta[, .(
        baseline_site_id = site_id,
        baseline_longitude = longitude, baseline_latitude = latitude
      )],
      by = "baseline_site_id",
      all.x = TRUE,
      sort = FALSE
    )
    pair_index[, `:=`(
      study_id = current_study,
      geographic_distance_km = haversine_km(
        baseline_longitude, baseline_latitude, target_longitude, target_latitude
      )
    )]
    pair_index[, log10_geographic_distance := log10(geographic_distance_km + 1)]
    pair_tables[[i]] <- pair_index[
      is.finite(similarity) & is.finite(log10_geographic_distance) & !is.na(pressure_class),
      .(study_id, baseline_site_id, target_site_id, pressure_class, taxon_group, region,
        similarity, geographic_distance_km, log10_geographic_distance)
    ]
  }
  pairs <- rbindlist(pair_tables, fill = TRUE)
  if (nrow(pairs) == 0) stop("No valid PREDICTS compositional-similarity pairs were produced.", call. = FALSE)
  pairs[, logit_similarity := adjusted_logit(similarity)]
  pairs
}

make_model_scopes <- function(site, pairs) {
  scopes <- list(data.table(model_id = "global__all", scope_type = "global", scope_value = "all"))
  if (isTRUE(fit_taxon_models)) {
    taxa <- site[, .N, by = taxon_group][N >= minimum_taxon_sites, taxon_group]
    if (length(taxon_groups_to_fit) > 0) taxa <- intersect(taxa, taxon_groups_to_fit)
    scopes[[length(scopes) + 1L]] <- data.table(
      model_id = paste0("taxon__", make.names(taxa)), scope_type = "taxon", scope_value = taxa
    )
  }
  if (isTRUE(fit_region_models)) {
    regions <- site[, .N, by = region][N >= minimum_region_sites, region]
    if (length(region_values_to_fit) > 0) regions <- intersect(regions, region_values_to_fit)
    scopes[[length(scopes) + 1L]] <- data.table(
      model_id = paste0("region__", make.names(regions)), scope_type = "region", scope_value = regions
    )
  }
  rbindlist(scopes, fill = TRUE)
}

scope_data <- function(site, pairs, scope_type, scope_value) {
  if (scope_type == "global") return(list(site = site, pairs = pairs))
  column <- if (scope_type == "taxon") "taxon_group" else "region"
  list(
    site = site[get(column) == scope_value],
    pairs = pairs[get(column) == scope_value]
  )
}

fit_scope_models <- function(site, pairs, scope_row) {
  data <- scope_data(site, pairs, scope_row$scope_type, scope_row$scope_value)
  abundance <- data$site
  composition <- data$pairs
  if (
    nrow(abundance) < minimum_sites_per_scope || nrow(composition) < minimum_pairs_per_scope ||
      !reference_pressure_class %in% abundance$pressure_class ||
      !reference_pressure_class %in% composition$pressure_class
  ) {
    return(NULL)
  }
  abundance[, pressure_class := relevel(factor(pressure_class), ref = reference_pressure_class)]
  composition[, pressure_class := relevel(factor(pressure_class), ref = reference_pressure_class)]

  if (has_lme4) {
    abundance_model <- lme4::lmer(
      sqrt_relative_abundance ~ pressure_class + (1 | study_id) + (1 | block_id),
      data = abundance,
      REML = TRUE
    )
    composition_model <- lme4::lmer(
      logit_similarity ~ pressure_class + log10_geographic_distance + (1 | study_id) + (1 | target_site_id),
      data = composition, REML = TRUE
    )
    fit_type <- "mixed_effects_lme4"
  } else {
    abundance_model <- stats::lm(sqrt_relative_abundance ~ pressure_class, data = abundance)
    composition_model <- stats::lm(logit_similarity ~ pressure_class + log10_geographic_distance, data = composition)
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
    n_pairs = nrow(composition)
  )
}

predict_no_random_effects <- function(model, new_data) {
  if (inherits(model, "merMod")) {
    as.numeric(stats::predict(model, newdata = new_data, re.form = NA, allow.new.levels = TRUE))
  } else {
    as.numeric(stats::predict(model, newdata = new_data))
  }
}

model_response_table <- function(model_record) {
  abundance_classes <- levels(stats::model.frame(model_record$abundance_model)$pressure_class)
  composition_classes <- levels(stats::model.frame(model_record$composition_model)$pressure_class)
  pressure_classes <- intersect(abundance_classes, composition_classes)
  if (!reference_pressure_class %in% pressure_classes) {
    stop("Model ", model_record$model_id, " has no common primary-minimal response class.", call. = FALSE)
  }
  new_abundance <- data.frame(
    pressure_class = factor(pressure_classes, levels = abundance_classes)
  )
  new_composition <- data.frame(
    pressure_class = factor(pressure_classes, levels = composition_classes),
    log10_geographic_distance = 0
  )
  abundance_prediction <- predict_no_random_effects(model_record$abundance_model, new_abundance)^2
  composition_prediction <- inverse_adjusted_logit(
    predict_no_random_effects(model_record$composition_model, new_composition)
  )
  baseline_index <- match(reference_pressure_class, pressure_classes)
  if (is.na(baseline_index) || abundance_prediction[[baseline_index]] <= 0 || composition_prediction[[baseline_index]] <= 0) {
    stop("Model ", model_record$model_id, " has no valid primary-minimal reference prediction.", call. = FALSE)
  }
  data.table(
    model_id = model_record$model_id,
    scope_type = model_record$scope_type,
    scope_value = model_record$scope_value,
    fit_type = model_record$fit_type,
    pressure_class = pressure_classes,
    abundance_relative = abundance_prediction / abundance_prediction[[baseline_index]],
    composition_relative = composition_prediction / composition_prediction[[baseline_index]]
  )[, bii_class_response := abundance_relative * composition_relative]
}

prepare_landuse_with_crosswalk <- function(landuse_path, crosswalk_path, label) {
  assert_file_exists(landuse_path, paste(label, "land-use input"))
  assert_file_exists(crosswalk_path, paste(label, "crosswalk"))
  landuse <- as.data.table(readr::read_csv(landuse_path, show_col_types = FALSE))
  crosswalk <- as.data.table(readr::read_csv(crosswalk_path, show_col_types = FALSE))
  assert_has_cols(landuse, c("cell_id", "year", "source_class", "share", "area_km2"), paste(label, "land-use input"))
  assert_has_cols(crosswalk, c("source_class", "bii_class", "allocation_share"), paste(label, "crosswalk"))
  if (!"scenario" %in% names(landuse)) landuse[, scenario := "historical"]
  if (!"region" %in% names(landuse)) landuse[, region := "Unknown"]
  if (!"country" %in% names(landuse)) landuse[, country := NA_character_]
  if (!"continent" %in% names(landuse)) landuse[, continent := NA_character_]
  if (!"intensity" %in% names(crosswalk)) crosswalk[, intensity := NA_character_]
  landuse[, `:=`(share = as.numeric(share), area_km2 = as.numeric(area_km2), year = as.integer(year))]
  crosswalk[, allocation_share := as.numeric(allocation_share)]
  if (!all(is.finite(landuse$share)) || !all(is.finite(landuse$area_km2) & landuse$area_km2 > 0)) {
    stop(label, " land-use rows require finite share and positive area_km2 values.", call. = FALSE)
  }
  keys <- c("scenario", "cell_id", "year")
  input_sum <- landuse[, .(input_share = sum(share)), by = keys]
  if (any(abs(input_sum$input_share - 1) > 1e-6)) {
    stop(label, " land-use shares must sum to 1 within every scenario/cell/year.", call. = FALSE)
  }
  expanded <- merge(landuse, crosswalk, by = "source_class", all.x = TRUE, allow.cartesian = TRUE, sort = FALSE)
  if (any(is.na(expanded$bii_class) | is.na(expanded$allocation_share))) {
    unresolved <- unique(expanded[is.na(bii_class) | is.na(allocation_share), source_class])
    stop(label, " crosswalk does not resolve source class(es): ", paste(unresolved, collapse = ", "), call. = FALSE)
  }
  expanded[, `:=`(
    bii_class = clean_chr(bii_class),
    intensity = normalise_intensity(intensity),
    share = share * allocation_share
  )]
  projection_variant <- if (isTRUE(bii_v2_enabled)) "land_use_intensity" else model_variant
  expanded[, pressure_class := make_pressure_class(bii_class, intensity, variant = projection_variant)]
  if (identical(projection_variant, "land_use_intensity") && any(is.na(expanded$pressure_class))) {
    stop(label, " needs intensity mappings for the V2 land-use-intensity model.", call. = FALSE)
  }
  prepared <- expanded[, .(
    share = sum(share),
    area_km2 = first(area_km2),
    region = first_present(region),
    country = first_present(country),
    continent = first_present(continent)
  ), by = c(keys, "pressure_class")]
  output_sum <- prepared[, .(output_share = sum(share)), by = keys]
  if (any(abs(output_sum$output_share - 1) > 1e-6)) {
    stop(label, " crosswalk allocations do not conserve land area.", call. = FALSE)
  }
  prepared
}

project_bii_landuse <- function(landuse, projection_name) {
  responses <- as.data.table(readr::read_csv(response_table_path, show_col_types = FALSE))
  assert_has_cols(landuse, c("scenario", "cell_id", "year", "pressure_class", "share", "area_km2", "region"), "Prepared land use")
  outputs <- vector("list", length(unique(responses$model_id)))
  skipped <- vector("list", length(unique(responses$model_id)))
  response_ids <- unique(responses$model_id)
  for (i in seq_along(response_ids)) {
    model_id_value <- response_ids[[i]]
    response <- responses[model_id == model_id_value]
    scope_type <- response$scope_type[[1]]
    scope_value <- response$scope_value[[1]]
    input <- copy(landuse)
    if (scope_type == "region") input <- input[region == scope_value]
    if (nrow(input) == 0) next
    unsupported_classes <- setdiff(unique(input$pressure_class), unique(response$pressure_class))
    if (length(unsupported_classes) > 0) {
      skipped[[i]] <- data.table(
        model_id = model_id_value,
        scope_type = scope_type,
        scope_value = scope_value,
        unsupported_pressure_classes = paste(unsupported_classes, collapse = ";"),
        n_input_rows = nrow(input)
      )
      next
    }
    projected <- merge(input, response[, .(pressure_class, abundance_relative, composition_relative)],
                       by = "pressure_class", all.x = TRUE, sort = FALSE)
    if (any(!is.finite(projected$abundance_relative) | !is.finite(projected$composition_relative))) {
      missing <- unique(projected[!is.finite(abundance_relative) | !is.finite(composition_relative), pressure_class])
      stop("Response table for ", model_id_value, " lacks projected class(es): ", paste(missing, collapse = ", "), call. = FALSE)
    }
    cell_keys <- c("scenario", "cell_id", "year", "region", "country", "area_km2")
    outputs[[i]] <- projected[, .(
      relative_abundance = sum(share * abundance_relative),
      compositional_similarity = sum(share * composition_relative)
    ), by = cell_keys][, `:=`(
      model_id = model_id_value,
      scope_type = scope_type,
      scope_value = scope_value,
      # The De Palma PREDICTS implementation first projects the abundance and
      # compositional-similarity maps separately, then multiplies those maps.
      bii = relative_abundance * compositional_similarity,
      projection_name = projection_name
    )]
  }
  cells <- rbindlist(outputs, fill = TRUE)
  if (nrow(cells) == 0) stop("No BII cells were projected.", call. = FALSE)
  unsupported <- rbindlist(skipped, fill = TRUE)
  write_csv_safe(
    unsupported,
    file.path(output_dir, paste0("bii_", projection_name, "_unsupported_scope_classes.csv"))
  )
  cells[, bii_bounded := pmin(1, pmax(0, bii))]
  cell_path <- file.path(output_dir, paste0("bii_", projection_name, "_cells.csv"))
  write_csv_safe(cells, cell_path)

  weight_column <- if (isTRUE(require_area_weight)) "area_km2" else NULL
  weighted_mean <- function(x, value) {
    if (is.null(weight_column)) mean(x[[value]], na.rm = TRUE) else weighted.mean(x[[value]], x$area_km2, na.rm = TRUE)
  }
  summarise_groups <- function(groups) {
    cells[, .(
      bii = weighted_mean(.SD, "bii"),
      bii_bounded = weighted_mean(.SD, "bii_bounded"),
      n_cells = .N,
      total_area_km2 = sum(area_km2)
    ), by = groups]
  }
  global <- summarise_groups(c("projection_name", "model_id", "scope_type", "scope_value", "scenario", "year"))
  region <- summarise_groups(c("projection_name", "model_id", "scope_type", "scope_value", "scenario", "year", "region"))
  country <- summarise_groups(c("projection_name", "model_id", "scope_type", "scope_value", "scenario", "year", "country"))
  write_csv_safe(global, file.path(output_dir, paste0("bii_", projection_name, "_global.csv")))
  write_csv_safe(region, file.path(output_dir, paste0("bii_", projection_name, "_regions.csv")))
  write_csv_safe(country, file.path(output_dir, paste0("bii_", projection_name, "_countries.csv")))
  invisible(list(cells = cells, global = global, region = region, country = country))
}
