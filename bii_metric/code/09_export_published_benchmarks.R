# Export the bundled published PREDICTS/NHM BII series in the same aggregate
# units used by this workflow. These are benchmarks, not workflow projections.
assert_file_exists(published_bii_path, "published PREDICTS/NHM BII benchmark")

published <- as.data.table(readr::read_csv(published_bii_path, show_col_types = FALSE))
assert_has_cols(
  published,
  c("area_code", "scenario", "value", "variable", "year"),
  "Published BII benchmark"
)
published <- published[
  variable == "bii" & scenario == published_scenario & is.finite(value)
][, `:=`(
  published_bii = as.numeric(value),
  year = as.integer(year)
)]

published_global <- published[area_code == published_global_area_code, .(
  year, published_bii, lower_uncertainty, upper_uncertainty
)]
write_csv_safe(published_global, file.path(output_dir, "bii_published_benchmark_global.csv"))

published_region_codes <- data.table(
  region = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
  area_code = c("001-002", "001-019", "001-142", "001-150", "001-009")
)
published_regions <- merge(published_region_codes, published, by = "area_code", all.x = FALSE)[, .(
  region, year, published_bii, lower_uncertainty, upper_uncertainty
)]
write_csv_safe(published_regions, file.path(output_dir, "bii_published_benchmark_regions.csv"))

published_countries <- published[grepl("-[A-Z]{3}$", area_code), .(
  country = sub("^.*-", "", area_code),
  published_area_code = area_code,
  year,
  published_bii,
  lower_uncertainty,
  upper_uncertainty
)]
write_csv_safe(published_countries, file.path(output_dir, "bii_published_benchmark_countries.csv"))

model_summary_file <- if (isTRUE(bii_v2_enabled)) "bii_v2_model_summary.csv" else "bii_model_summary.csv"
taxon_models <- as.data.table(readr::read_csv(
  file.path(output_dir, model_summary_file), show_col_types = FALSE
))[scope_type == "taxon"]
taxon_output_file <- if (isTRUE(bii_v2_enabled)) {
  "bii_v2_predicts_taxon_model_coverage.csv"
} else {
  "bii_predicts_taxon_model_coverage.csv"
}
write_csv_safe(taxon_models, file.path(output_dir, taxon_output_file))
