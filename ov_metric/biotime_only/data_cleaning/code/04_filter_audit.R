# =====================================================
# Detailed filtering audit for the BioTIMEr/composite OV workflow
# =====================================================

assert_file_exists(cleaning_summary_path, "BioTIME cleaning summary")
assert_file_exists(clean_sample_year_path, "Composite OV sample-year table")
assert_file_exists(clean_timeseries_path, "Composite OV time-series table")

message("Building detailed BioTIME filter funnel")

summary <- readr::read_csv(cleaning_summary_path, show_col_types = FALSE)
sample_year <- readr::read_csv(clean_sample_year_path, show_col_types = FALSE)
timeseries <- readr::read_csv(clean_timeseries_path, show_col_types = FALSE)

get_metric <- function(metric_name) {
  value <- summary$value[summary$metric == metric_name][1]
  if (length(value) == 0 || is.na(value)) return(NA_character_)
  value
}

as_num_metric <- function(metric_name) {
  suppressWarnings(as.numeric(get_metric(metric_name)))
}

funnel <- data.frame(
  step = c(
    "01_raw_observations",
    "02_unique_raw_observation_ids",
    "03_terrestrial_abundance_metadata",
    "04_year_2000_2025",
    "05_valid_coordinates",
    "06_nonmissing_abundance",
    "07_taxon_name_resolution_filter",
    "08_taxonomy_resolution",
    "09_biotimer_standardized_taxon_rows",
    "10_original_composite_ov_sample_years",
    "11_keep_assemblages_with_ge_2_composite_ov_years"
  ),
  retained_unit = c(
    rep("raw_observation_row", 8),
    "standardized_taxon_row",
    "assemblage_year",
    "assemblage_year"
  ),
  retained_rows = c(
    as_num_metric("raw_observation_rows_total"),
    as_num_metric("raw_observation_rows_after_id_deduplication"),
    as_num_metric("rows_after_terrestrial_abundance_metadata"),
    as_num_metric("rows_after_year_2000_2025"),
    as_num_metric("rows_after_valid_coordinates"),
    as_num_metric("rows_after_nonmissing_abundance"),
    as_num_metric("rows_after_taxon_name_resolution_filter"),
    as_num_metric("rows_after_taxonomy_resolution"),
    as_num_metric("standardized_taxon_rows"),
    as_num_metric("composite_ov_rows_before_two_year_filter"),
    as_num_metric("sample_year_rows_retained")
  ),
  retained_time_series = c(
    rep(NA_real_, 9),
    as_num_metric("time_series_before_two_year_filter"),
    as_num_metric("time_series_retained")
  ),
  retained_studies = c(
    rep(NA_real_, 9),
    dplyr::n_distinct(sample_year$study_id),
    dplyr::n_distinct(sample_year$study_id)
  ),
  retained_years = c(
    rep(NA_real_, 9),
    dplyr::n_distinct(sample_year$sample_year),
    dplyr::n_distinct(sample_year$sample_year)
  ),
  note = c(
    "Raw BioTIME observation rows",
    "Drop duplicated ID_ALL_RAW_DATA rows",
    "Keep studies with REALM == Terrestrial and AB_BIO containing A",
    "Keep rows in the BioTIME-Hansen overlap period",
    "Keep valid latitude/longitude rows",
    "Keep rows with finite ABUNDANCE values; zero abundance is allowed for BioTIMEr gridding",
    "Drop rows with unusable or very coarse taxonomic names before taxonomy lookup",
    "Attach resolved taxonomy, including Family, from the lookup resources used by the original pipeline",
    "BioTIMEr gridding plus deterministic sample-event rarefaction creates assemblage-year taxon rows",
    "Original composite OV is calculated here: Shannon + log mean species abundance + phylogenetic diversity",
    "Keep only assemblages with at least two post-2000 composite OV readings"
  )
)

funnel$rows_removed_from_previous <- dplyr::lag(funnel$retained_rows) - funnel$retained_rows
funnel$pct_removed_from_previous <- round(
  100 * funnel$rows_removed_from_previous / dplyr::lag(funnel$retained_rows),
  2
)
funnel$pct_of_raw_observation_rows <- round(
  100 * funnel$retained_rows / funnel$retained_rows[1],
  3
)
funnel$rows_removed_from_previous[1] <- NA_real_
funnel$pct_removed_from_previous[1] <- NA_real_

write_csv_safe(funnel, file.path(output_dir, "biotime_filter_funnel_detailed.csv"))

positive_delta_note <- data.frame(
  metric = c("time_series_retained", "sample_year_rows_retained"),
  value = c(nrow(timeseries), nrow(sample_year)),
  note = "Positive-delta OV filtering is applied later as a regression variant, not during data cleaning."
)
write_csv_safe(positive_delta_note, file.path(output_dir, "biotime_positive_delta_filter_note.csv"))
