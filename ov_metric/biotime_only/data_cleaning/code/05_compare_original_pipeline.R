# =====================================================
# Compare raw-only BioTIME handoff size with original pipeline outputs
# This is an audit only; the BioTIME-only modeling pipeline does not use these
# original processed files as inputs.
# =====================================================

original_filter_summary_path <- file.path(
  ov_metric_dir,
  "01_biodiversity_data_integration",
  "biotime_info",
  "biotime_filter_summary.csv"
)
original_biotime_database_path <- file.path(
  ov_metric_dir,
  "01_biodiversity_data_integration",
  "output",
  "biotime_database.rds"
)
original_biotime_ov_path <- file.path(
  ov_metric_dir,
  "02_ov_calculation",
  "calculation",
  "output",
  "biotime_ov_scores.csv"
)

new_sample_year <- readr::read_csv(clean_sample_year_path, show_col_types = FALSE)
new_timeseries <- readr::read_csv(clean_timeseries_path, show_col_types = FALSE)

original_database <- if (file.exists(original_biotime_database_path)) {
  readRDS(original_biotime_database_path)
} else {
  data.frame()
}

original_ov <- if (file.exists(original_biotime_ov_path)) {
  readr::read_csv(original_biotime_ov_path, show_col_types = FALSE)
} else {
  data.frame()
}

comparison <- data.frame(
  metric = c(
    "original_standardized_taxon_rows",
    "original_ov_sample_rows",
    "original_unique_sites",
    "original_unique_studies",
    "new_time_series_year_rows",
    "new_unique_time_series",
    "new_unique_studies"
  ),
  value = c(
    nrow(original_database),
    nrow(original_ov),
    dplyr::n_distinct(original_ov$site_id),
    dplyr::n_distinct(original_ov$study_id),
    nrow(new_sample_year),
    nrow(new_timeseries),
    dplyr::n_distinct(new_sample_year$study_id)
  )
)

write_csv_safe(comparison, file.path(output_dir, "biotime_original_pipeline_comparison.csv"))

if (file.exists(original_filter_summary_path)) {
  original_filter_summary <- readr::read_csv(original_filter_summary_path, show_col_types = FALSE)
  write_csv_safe(
    original_filter_summary,
    file.path(output_dir, "biotime_original_filter_summary_copy.csv")
  )
}
