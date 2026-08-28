# Compare V3's aggregate global/administrative outputs with NHM BII v2.1.1.
# V3 shares 2010 and 2020 with the public raster product; V2's existing run
# shares 2010 only; V1 has all five historical benchmark years.

published_paths <- list(
  global = file.path(output_dir, "bii_v211_published_global.csv"),
  countries = file.path(output_dir, "bii_v211_published_countries.csv"),
  continents = file.path(output_dir, "bii_v211_published_continents.csv"),
  regions = file.path(output_dir, "bii_v211_published_regions.csv")
)
for (path in unlist(published_paths, use.names = FALSE)) assert_file_exists(path, "NHM v2.1.1 published summary")
published <- lapply(published_paths, function(path) data.table::as.data.table(readr::read_csv(path, show_col_types = FALSE)))

v3_filter_global <- function(data) {
  data[model_id == "global__all" & scope_type == "global"]
}

v3_compare <- function(model, published_data, level, id_column) {
  model <- data.table::copy(model)
  if (level == "countries") data.table::setnames(model, "country", "country_iso3")
  if (level == "regions") data.table::setnames(model, "region", "predicts_region")
  model <- v3_filter_global(model)
  join <- c("year", if (!is.na(id_column)) id_column)
  matched <- merge(model[, c(join, "bii_bounded"), with = FALSE],
                   published_data[, c(join, "published_bii", "total_area_km2"), with = FALSE],
                   by = join, all = FALSE, allow.cartesian = FALSE)
  data.table::setnames(matched, "bii_bounded", "modelled_bii")
  matched[, `:=`(version = "V3", comparison = level, difference = modelled_bii - published_bii)]
  data.table::setcolorder(matched, c("version", "comparison", join, "modelled_bii", "published_bii", "difference", "total_area_km2"))
  matched
}

v3_metrics <- function(data, level) data.table::data.table(
  version = "V3", comparison = level, n_matched_rows = nrow(data),
  years = paste(sort(unique(data$year)), collapse = ","),
  mean_absolute_difference = mean(abs(data$difference)), mean_signed_difference = mean(data$difference),
  pearson_correlation = if (nrow(data) > 1L) stats::cor(data$modelled_bii, data$published_bii) else NA_real_,
  spearman_correlation = if (nrow(data) > 1L) stats::cor(data$modelled_bii, data$published_bii, method = "spearman") else NA_real_,
  status = "completed_2010_2020_intersection"
)

model_paths <- list(
  global = file.path(output_dir, "bii_v3_luh2_global.csv"),
  countries = file.path(output_dir, "bii_v3_luh2_countries.csv"),
  continents = file.path(output_dir, "bii_v3_luh2_continents.csv"),
  regions = file.path(output_dir, "bii_v3_luh2_regions.csv")
)
for (path in unlist(model_paths, use.names = FALSE)) assert_file_exists(path, "V3 LUH2 output")
id_columns <- c(global = NA_character_, countries = "country_iso3", continents = "continent", regions = "predicts_region")
matched <- Map(v3_compare, lapply(model_paths, function(path) data.table::as.data.table(readr::read_csv(path, show_col_types = FALSE))),
               published[names(model_paths)], names(model_paths), id_columns)
for (level in names(matched)) write_csv_safe(matched[[level]], file.path(output_dir, paste0("bii_v211_v3_validation_", level, ".csv")))
summary <- data.table::rbindlist(Map(v3_metrics, matched, names(matched)))
write_csv_safe(summary, file.path(output_dir, "bii_v211_v3_validation_summary.csv"))

existing <- c("bii_v211_v1_validation_summary.csv", "bii_v211_v2_validation_summary.csv")
existing <- file.path(output_dir, existing)
tables <- c(lapply(existing[file.exists(existing)], function(path) data.table::as.data.table(readr::read_csv(path, show_col_types = FALSE))),
            list(summary))
all_summary <- data.table::rbindlist(tables, fill = TRUE)
write_csv_safe(all_summary, file.path(output_dir, "bii_v211_v1_v2_v3_validation_summary.csv"))

# A compact, year-aligned global table makes the three in-repository variants
# and the published raster benchmark directly inspectable without inferring
# values from the separate validation files.
read_global <- function(path, version) {
  if (!file.exists(path)) return(NULL)
  data <- data.table::as.data.table(readr::read_csv(path, show_col_types = FALSE))
  data <- data[model_id == "global__all" & scope_type == "global"]
  data.table::data.table(version = version, year = as.integer(data$year), bii = as.numeric(data$bii_bounded))
}
global_comparison <- data.table::rbindlist(list(
  read_global(file.path(output_dir, paste0(nhm_v211_projection_prefix, "_global.csv")), "V1"),
  read_global(file.path(output_dir, "bii_v2_luh2_global.csv"), "V2"),
  read_global(file.path(output_dir, "bii_v3_luh2_global.csv"), "V3"),
  published$global[, .(version = "NHM BII v2.1.1", year = as.integer(year), bii = as.numeric(published_bii))]
), fill = TRUE)
data.table::setorder(global_comparison, year, version)
write_csv_safe(global_comparison, file.path(output_dir, "bii_v1_v2_v3_nhm_global_comparison.csv"))
message("Wrote V3 NHM v2.1.1 validation tables to ", output_dir)
