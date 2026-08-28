# Compare the modelled historical global and UN-region BII values with the
# published PREDICTS/NHM BII benchmark bundled under 00_biodiversity_data.
assert_file_exists(published_bii_path, "published PREDICTS/NHM BII benchmark")
global_path <- file.path(output_dir, "bii_landsat_global.csv")
region_path <- file.path(output_dir, "bii_landsat_regions.csv")
country_path <- file.path(output_dir, "bii_landsat_countries.csv")
assert_file_exists(global_path, "Landsat global BII projection")
assert_file_exists(region_path, "Landsat regional BII projection")
assert_file_exists(country_path, "Landsat country BII projection")

published <- as.data.table(readr::read_csv(published_bii_path, show_col_types = FALSE))
assert_has_cols(published, c("area_code", "scenario", "value", "variable", "year"), "Published BII benchmark")
published <- published[
  variable == "bii" & scenario == published_scenario & is.finite(value),
  .(area_code = as.character(area_code), year = as.integer(year), published_bii = as.numeric(value))
]

projected_global <- as.data.table(readr::read_csv(global_path, show_col_types = FALSE))[
  model_id == "global__all" & scenario == published_scenario,
  .(year = as.integer(year), modelled_bii = as.numeric(bii), modelled_bii_bounded = as.numeric(bii_bounded))
]
published_global <- published[area_code == published_global_area_code]
global_validation <- merge(projected_global, published_global, by = "year", all = FALSE)
if (nrow(global_validation) > 0) {
  global_validation[, `:=`(
    absolute_difference = modelled_bii - published_bii,
    bounded_absolute_difference = modelled_bii_bounded - published_bii
  )]
}
write_csv_safe(global_validation, file.path(output_dir, "bii_validation_global_vs_published.csv"))

published_region_codes <- data.table(
  region = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
  area_code = c("001-002", "001-019", "001-142", "001-150", "001-009")
)
projected_region <- as.data.table(readr::read_csv(region_path, show_col_types = FALSE))[
  model_id == "global__all" & scenario == published_scenario,
  .(region, year = as.integer(year), modelled_bii = as.numeric(bii), modelled_bii_bounded = as.numeric(bii_bounded))
]
regional_validation <- merge(projected_region, published_region_codes, by = "region", all = FALSE)
regional_validation <- merge(regional_validation, published, by = c("area_code", "year"), all = FALSE)
if (nrow(regional_validation) > 0) {
  regional_validation[, `:=`(
    absolute_difference = modelled_bii - published_bii,
    bounded_absolute_difference = modelled_bii_bounded - published_bii
  )]
}
write_csv_safe(regional_validation, file.path(output_dir, "bii_validation_regions_vs_published.csv"))

published_country <- published[grepl("-[A-Z]{3}$", area_code), .(
  country = sub("^.*-", "", area_code),
  year,
  published_bii
)]
projected_country <- as.data.table(readr::read_csv(
  country_path, show_col_types = FALSE
))[model_id == "global__all" & scenario == published_scenario &
    !is.na(country) & grepl("^[A-Za-z]{3}$", country),
  .(
    country = toupper(trimws(as.character(country))),
    year = as.integer(year),
    modelled_bii = as.numeric(bii),
    modelled_bii_bounded = as.numeric(bii_bounded)
  )
]
country_validation <- merge(projected_country, published_country, by = c("country", "year"), all = FALSE)
if (nrow(country_validation) > 0) {
  country_validation[, `:=`(
    absolute_difference = modelled_bii - published_bii,
    bounded_absolute_difference = modelled_bii_bounded - published_bii
  )]
}
write_csv_safe(country_validation, file.path(output_dir, "bii_validation_countries_vs_published.csv"))

taxon_responses <- as.data.table(readr::read_csv(response_table_path, show_col_types = FALSE))[scope_type == "taxon"]
write_csv_safe(taxon_responses, file.path(output_dir, "bii_taxon_response_validation_table.csv"))

safe_correlation <- function(data, method) {
  if (nrow(data) < 2 ||
      !is.finite(stats::sd(data$modelled_bii)) || !is.finite(stats::sd(data$published_bii)) ||
      stats::sd(data$modelled_bii) == 0 || stats::sd(data$published_bii) == 0) {
    return(NA_real_)
  }
  stats::cor(data$modelled_bii, data$published_bii, method = method)
}

comparison_metrics <- function(data) {
  if (nrow(data) == 0) {
    return(list(n = 0L, mean_absolute_difference = NA_real_, mean_signed_difference = NA_real_,
                pearson_correlation = NA_real_, spearman_correlation = NA_real_))
  }
  list(
    n = nrow(data),
    mean_absolute_difference = mean(abs(data$absolute_difference)),
    mean_signed_difference = mean(data$absolute_difference),
    pearson_correlation = safe_correlation(data, "pearson"),
    spearman_correlation = safe_correlation(data, "spearman")
  )
}
global_metrics <- comparison_metrics(global_validation)
regional_metrics <- comparison_metrics(regional_validation)
country_metrics <- comparison_metrics(country_validation)

validation_summary <- data.table(
  comparison = c("global", "UN_regions", "countries", "taxon_response_models"),
  n_matched_rows = c(global_metrics$n, regional_metrics$n, country_metrics$n, nrow(taxon_responses)),
  mean_absolute_difference = c(
    global_metrics$mean_absolute_difference,
    regional_metrics$mean_absolute_difference,
    country_metrics$mean_absolute_difference,
    NA_real_
  ),
  mean_signed_difference = c(
    global_metrics$mean_signed_difference,
    regional_metrics$mean_signed_difference,
    country_metrics$mean_signed_difference,
    NA_real_
  ),
  pearson_correlation = c(
    global_metrics$pearson_correlation,
    regional_metrics$pearson_correlation,
    country_metrics$pearson_correlation,
    NA_real_
  ),
  spearman_correlation = c(
    global_metrics$spearman_correlation,
    regional_metrics$spearman_correlation,
    country_metrics$spearman_correlation,
    NA_real_
  ),
  note = c(
    "Requires matching historical years between Landsat and NHM data.",
    "Uses UN-region labels and published NHM area-code crosswalk.",
    "Requires ISO3 country values in the Landsat input; matches the ISO3 suffix in published NHM area codes.",
    "Published bundled benchmark is aggregate-taxa only; taxon models are reported for response-function review."
  )
)
write_csv_safe(validation_summary, file.path(output_dir, "bii_validation_summary.csv"))
