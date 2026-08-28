# -----------------------------------------------------------------------------
# Compare the direct V2 LUH2 projection with the bundled NHM BII summaries.
# -----------------------------------------------------------------------------

v2_global_path <- file.path(output_dir, "bii_v2_luh2_global.csv")
v2_region_path <- file.path(output_dir, "bii_v2_luh2_regions.csv")
v2_country_path <- file.path(output_dir, "bii_v2_luh2_countries.csv")
v2_continent_path <- file.path(output_dir, "bii_v2_luh2_continents.csv")
for (path in c(v2_global_path, v2_region_path, v2_country_path, v2_continent_path, published_bii_path)) {
  assert_file_exists(path, "V2 LUH2 validation input")
}

published <- data.table::as.data.table(readr::read_csv(published_bii_path, show_col_types = FALSE))[
  variable == "bii" & year %in% c(2010L, 2030L) & is.finite(value)
]
for (dataset_name in c("global", "region", "country", "continent")) {
  assign(paste0("v2_", dataset_name), data.table::as.data.table(readr::read_csv(
    get(paste0("v2_", dataset_name, "_path")), show_col_types = FALSE
  )))
}

global_model <- function(data) data[model_id == "global__all" & scope_type == "global"]
v2_global_all <- data.table::copy(v2_global)
v2_global <- global_model(v2_global)
v2_region <- global_model(v2_region)
v2_country <- global_model(v2_country)
v2_continent <- global_model(v2_continent)

published_global <- published[area_code == "global", .(scenario, year, published_bii = value)]
validation_global <- merge(v2_global, published_global, by = c("scenario", "year"), all = FALSE)
validation_global[, difference := bii_bounded - published_bii]

region_codes <- c(Africa = "001-002", Americas = "001-019", Asia = "001-142", Europe = "001-150", Oceania = "001-009")
published_region <- published[area_code %in% unname(region_codes), .(scenario, year, area_code, published_bii = value)]
v2_region[, area_code := unname(region_codes[region])]
validation_regions <- merge(v2_region, published_region, by = c("scenario", "year", "area_code"), all = FALSE)
validation_regions[, difference := bii_bounded - published_bii]

published_country <- published[nchar(sub(".*-", "", area_code)) == 3L, .(
  scenario, year, country = sub(".*-", "", area_code), published_bii = value
)]
validation_countries <- merge(v2_country[nchar(country) == 3L], published_country, by = c("scenario", "year", "country"), all = FALSE)
validation_countries[, difference := bii_bounded - published_bii]

validation_summary <- data.table::rbindlist(lapply(
  list(global = validation_global, regions = validation_regions, countries = validation_countries),
  function(data) {
    modelled <- data$bii_bounded
    published_values <- data$published_bii
    data.table::data.table(
      n_matched_rows = nrow(data),
      mean_absolute_difference = mean(abs(data$difference)),
      mean_signed_difference = mean(data$difference),
      pearson_correlation = if (nrow(data) > 1L) stats::cor(modelled, published_values) else NA_real_,
      spearman_correlation = if (nrow(data) > 1L) stats::cor(modelled, published_values, method = "spearman") else NA_real_
    )
  }
), idcol = "comparison")

write_csv_safe(validation_global, file.path(output_dir, "bii_v2_luh2_validation_global_vs_published.csv"))
write_csv_safe(validation_regions, file.path(output_dir, "bii_v2_luh2_validation_regions_vs_published.csv"))
write_csv_safe(validation_countries, file.path(output_dir, "bii_v2_luh2_validation_countries_vs_published.csv"))
write_csv_safe(validation_summary, file.path(output_dir, "bii_v2_luh2_validation_summary.csv"))

paired_report <- function(data, dimensions) {
  data <- data[year %in% c(2010L, 2030L)]
  wide <- data.table::dcast(
    data,
    stats::as.formula(paste(paste(dimensions, collapse = " + "), "~ year")),
    value.var = "bii_bounded"
  )
  data.table::setnames(wide, c("2010", "2030"), c("bii_2010", "bii_2030"), skip_absent = TRUE)
  if (all(c("bii_2010", "bii_2030") %in% names(wide))) {
    wide[, change_2030_minus_2010 := bii_2030 - bii_2010]
  }
  wide
}

global_report <- paired_report(v2_global, c("model_id", "response_mode", "fallback_pressure_classes"))
global_published_wide <- data.table::dcast(
  validation_global[, .(model_id, year, published_bii)],
  model_id ~ year, value.var = "published_bii"
)
data.table::setnames(global_published_wide, c("2010", "2030"), c("published_bii_2010", "published_bii_2030"), skip_absent = TRUE)
global_report <- merge(global_report, global_published_wide, by = "model_id", all.x = TRUE)
global_report[, `:=`(
  difference_vs_published_2010 = bii_2010 - published_bii_2010,
  difference_vs_published_2030 = bii_2030 - published_bii_2030
)]
write_csv_safe(global_report, file.path(output_dir, "bii_v2_luh2_report_global.csv"))
write_csv_safe(paired_report(v2_country, c("country", "response_mode", "fallback_pressure_classes")), file.path(output_dir, "bii_v2_luh2_report_national.csv"))
write_csv_safe(paired_report(v2_continent, c("continent", "response_mode", "fallback_pressure_classes")), file.path(output_dir, "bii_v2_luh2_report_continents.csv"))
write_csv_safe(paired_report(v2_global_all[scope_type == "taxon"], c("scope_value", "response_mode", "fallback_pressure_classes")), file.path(output_dir, "bii_v2_luh2_report_taxa.csv"))

v1_summary_path <- file.path(output_dir, "bii_luh2_validation_summary.csv")
if (file.exists(v1_summary_path)) {
  v1_summary <- data.table::as.data.table(readr::read_csv(v1_summary_path, show_col_types = FALSE))
  v1_summary[, version := "V1"]
  validation_summary[, version := "V2"]
  comparison <- data.table::rbindlist(list(v1_summary, validation_summary), fill = TRUE)
  data.table::setcolorder(comparison, c("version", "comparison", setdiff(names(comparison), c("version", "comparison"))))
  write_csv_safe(comparison, file.path(output_dir, "bii_luh2_v1_v2_validation_summary.csv"))
}

message("Wrote V2 LUH2 validation and reporting tables to ", output_dir)
