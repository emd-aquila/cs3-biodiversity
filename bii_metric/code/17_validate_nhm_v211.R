# Compare fitted PREDICTS/LUH2 projections with the separately summarised NHM
# BII v2.1.1 rasters.  The public product is aggregate-taxa only, so taxon
# response functions are intentionally excluded from this validation.

published_paths <- list(
  global = file.path(output_dir, "bii_v211_published_global.csv"),
  countries = file.path(output_dir, "bii_v211_published_countries.csv"),
  continents = file.path(output_dir, "bii_v211_published_continents.csv"),
  regions = file.path(output_dir, "bii_v211_published_regions.csv")
)
for (path in unlist(published_paths, use.names = FALSE)) {
  assert_file_exists(path, "NHM v2.1.1 published summary")
}
published <- lapply(published_paths, function(path) {
  data.table::as.data.table(readr::read_csv(path, show_col_types = FALSE))
})

comparison_spec <- data.table::data.table(
  level = c("global", "countries", "continents", "regions"),
  id_column = c(NA_character_, "country_iso3", "continent", "predicts_region")
)

validation_metrics <- function(data, level, version) {
  n <- nrow(data)
  data.table::data.table(
    version = version,
    comparison = level,
    n_matched_rows = n,
    years = if (n > 0L) paste(sort(unique(data$year)), collapse = ",") else NA_character_,
    mean_absolute_difference = if (n > 0L) mean(abs(data$difference)) else NA_real_,
    mean_signed_difference = if (n > 0L) mean(data$difference) else NA_real_,
    pearson_correlation = if (n > 1L) stats::cor(data$modelled_bii, data$published_bii) else NA_real_,
    spearman_correlation = if (n > 1L) stats::cor(data$modelled_bii, data$published_bii, method = "spearman") else NA_real_
  )
}

filter_global_model <- function(data) {
  required <- c("model_id", "scope_type", "bii_bounded")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Model table is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  data[model_id == "global__all" & scope_type == "global"]
}

compare_level <- function(model_data, published_data, level, id_column, version) {
  model_data <- data.table::copy(model_data)
  published_data <- data.table::copy(published_data)
  model_data[, modelled_bii := as.numeric(bii_bounded)]
  join_columns <- c("year", if (!is.na(id_column)) id_column)
  missing_model <- setdiff(join_columns, names(model_data))
  missing_published <- setdiff(join_columns, names(published_data))
  if (length(missing_model) > 0L || length(missing_published) > 0L) {
    stop(
      "Cannot compare ", version, " at ", level, ": missing join columns (model: ",
      paste(missing_model, collapse = ", "), "; published: ", paste(missing_published, collapse = ", "), ").",
      call. = FALSE
    )
  }
  keep_model <- unique(c(join_columns, "modelled_bii"))
  keep_published <- unique(c(join_columns, "published_bii", "total_area_km2"))
  matched <- merge(
    model_data[, ..keep_model], published_data[, ..keep_published],
    by = join_columns, all = FALSE, allow.cartesian = FALSE
  )
  matched[, `:=`(version = version, comparison = level, difference = modelled_bii - published_bii)]
  data.table::setcolorder(matched, c("version", "comparison", join_columns,
                                     "modelled_bii", "published_bii", "difference", "total_area_km2"))
  matched
}

validate_version <- function(version, paths, id_columns) {
  model_paths <- unlist(paths, use.names = FALSE)
  if (!all(file.exists(model_paths))) {
    missing <- model_paths[!file.exists(model_paths)]
    message("Skipping NHM v2.1.1 ", version, " comparison; model outputs are missing: ",
            paste(basename(missing), collapse = ", "))
    return(list(
      summary = data.table::data.table(
        version = version, comparison = "not_run", n_matched_rows = 0L, years = NA_character_,
        mean_absolute_difference = NA_real_, mean_signed_difference = NA_real_,
        pearson_correlation = NA_real_, spearman_correlation = NA_real_,
        status = "model_outputs_missing"
      ),
      status = "model_outputs_missing"
    ))
  }

  model_tables <- lapply(paths, function(path) {
    filter_global_model(data.table::as.data.table(readr::read_csv(path, show_col_types = FALSE)))
  })
  matched <- Map(
    compare_level,
    model_tables, published[names(paths)], names(paths), id_columns, MoreArgs = list(version = version)
  )
  for (level in names(matched)) {
    write_csv_safe(matched[[level]], file.path(output_dir, paste0("bii_v211_", tolower(version),
                                                                   "_validation_", level, ".csv")))
  }
  summary <- data.table::rbindlist(Map(
    validation_metrics, matched, names(matched), MoreArgs = list(version = version)
  ))
  summary[, status := "completed"]
  write_csv_safe(summary, file.path(output_dir, paste0("bii_v211_", tolower(version), "_validation_summary.csv")))
  list(summary = summary, status = "completed")
}

v1_paths <- list(
  global = file.path(output_dir, paste0(nhm_v211_projection_prefix, "_global.csv")),
  countries = file.path(output_dir, paste0(nhm_v211_projection_prefix, "_countries.csv")),
  continents = file.path(output_dir, paste0(nhm_v211_projection_prefix, "_continents.csv")),
  regions = file.path(output_dir, paste0(nhm_v211_projection_prefix, "_published_regions.csv"))
)
v1_result <- validate_version(
  "V1", v1_paths,
  id_columns = comparison_spec$id_column[match(names(v1_paths), comparison_spec$level)]
)

# V2's existing direct-LUH2 run covers 2010 and 2030.  Its only intersection
# with the five v2.1.1 raster years is 2010; use it as an explicitly limited
# sensitivity comparison rather than pretending that it provides all years.
v2_paths <- list(
  global = file.path(output_dir, "bii_v2_luh2_global.csv"),
  countries = file.path(output_dir, "bii_v2_luh2_countries.csv"),
  continents = file.path(output_dir, "bii_v2_luh2_continents.csv"),
  regions = file.path(output_dir, "bii_v2_luh2_regions.csv")
)
v2_id_columns <- c(global = NA_character_, countries = "country", continents = "continent", regions = "region")

# Rename V2 identifiers to the stable published-summary identifiers before
# merging. This isolates column-name differences from the actual comparison.
prepare_v2_table <- function(path, level) {
  table <- data.table::as.data.table(readr::read_csv(path, show_col_types = FALSE))
  if (level == "countries") data.table::setnames(table, "country", "country_iso3")
  if (level == "regions") data.table::setnames(table, "region", "predicts_region")
  filter_global_model(table)
}
validate_v2 <- function() {
  if (!all(file.exists(unlist(v2_paths, use.names = FALSE)))) {
    missing <- basename(unlist(v2_paths, use.names = FALSE)[!file.exists(unlist(v2_paths, use.names = FALSE))])
    return(list(
      summary = data.table::data.table(
        version = "V2", comparison = "not_run", n_matched_rows = 0L, years = NA_character_,
        mean_absolute_difference = NA_real_, mean_signed_difference = NA_real_,
        pearson_correlation = NA_real_, spearman_correlation = NA_real_,
        status = paste0("model_outputs_missing: ", paste(missing, collapse = ", "))
      ),
      status = "model_outputs_missing"
    ))
  }
  model_tables <- Map(prepare_v2_table, v2_paths, names(v2_paths))
  matched <- Map(
    compare_level, model_tables, published[names(v2_paths)], names(v2_paths),
    comparison_spec$id_column[match(names(v2_paths), comparison_spec$level)], MoreArgs = list(version = "V2")
  )
  for (level in names(matched)) {
    write_csv_safe(matched[[level]], file.path(output_dir, paste0("bii_v211_v2_validation_", level, ".csv")))
  }
  summary <- data.table::rbindlist(Map(
    validation_metrics, matched, names(matched), MoreArgs = list(version = "V2")
  ))
  summary[, status := "completed_2010_intersection_only"]
  write_csv_safe(summary, file.path(output_dir, "bii_v211_v2_validation_summary.csv"))
  list(summary = summary, status = "completed_2010_intersection_only")
}
v2_result <- validate_v2()

scope <- data.table::data.table(
  published_product = "NHM BII v2.1.1 limited release",
  comparison = c(
    "V1 aggregate BII: 2000, 2005, 2010, 2015, 2020",
    "V2 aggregate BII: overlapping model years only",
    "Taxon-specific BII"
  ),
  status = c(v1_result$status, v2_result$status, "not_available_in_published_v2.1.1_rasters"),
  note = c(
    "V1 is projected from the direct LUH2 land-use crosswalk; it is not a production-model equivalence claim.",
    "Current direct V2 LUH2 outputs only overlap v2.1.1 in 2010; its intensity allocation remains a sensitivity mapping.",
    "The v2.1.1 archive supplies aggregate global rasters, not taxon-specific layers or response coefficients."
  )
)
write_csv_safe(scope, file.path(output_dir, "bii_v211_comparison_scope.csv"))
all_summary <- data.table::rbindlist(list(v1_result$summary, v2_result$summary), fill = TRUE)
write_csv_safe(all_summary, file.path(output_dir, "bii_v211_validation_summary.csv"))

message("Wrote NHM v2.1.1 validation tables to ", output_dir)
