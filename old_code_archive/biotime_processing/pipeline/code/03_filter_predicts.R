# =====================================================
# Filter and slim PREDICTS for OV calculation and geotagging.
# =====================================================

assert_file_exists(predicts_full_rds_path, "cached full PREDICTS RDS")

predicts_raw <- data.table::as.data.table(readRDS(predicts_full_rds_path))
assert_has_cols(
  predicts_raw,
  c(
    "Source_ID", "Reference", "Study_number", "Study_name", "SSBS",
    "Diversity_metric_type", "Sample_start_earliest", "Sample_end_latest",
    "Sample_midpoint", "Latitude", "Longitude", "Biome",
    "Predominant_land_use", "Use_intensity", "Class", "Family", "Genus",
    "Taxon", "Measurement", "Effort_corrected_measurement"
  ),
  "full PREDICTS extract"
)

predicts_raw[, Sample_start_earliest := as.Date(Sample_start_earliest)]
predicts_raw[, Sample_end_latest := as.Date(Sample_end_latest)]
predicts_raw[, Sample_midpoint := as.Date(Sample_midpoint)]

# PREDICTS is a terrestrial database. Do not filter or retain its biogeographic
# Realm column. Filter study units by their earliest abundance observation.
predicts_study_starts <- predicts_raw[
  Diversity_metric_type == "Abundance" & !is.na(Sample_start_earliest),
  .(study_start_date = min(Sample_start_earliest)),
  by = .(Source_ID, Study_number)
]
eligible_predicts_studies <- predicts_study_starts[
  study_start_date >= as.Date(paste0(minimum_year, "-01-01"))
]

predicts_filtered <- merge(
  predicts_raw[Diversity_metric_type == "Abundance"],
  eligible_predicts_studies,
  by = c("Source_ID", "Study_number"),
  all = FALSE,
  sort = FALSE
)

# Keep fields required for OV components, site/date geotagging, measurement
# interpretation, and traceability to the original PREDICTS source.
predicts_output_cols <- intersect(
  c(
    "Source_ID", "Reference", "Study_number", "Study_name",
    "Site_name", "Site_number", "SSBS",
    "Country", "UN_subregion", "Biome",
    "Predominant_land_use", "Use_intensity",
    "Latitude", "Longitude",
    "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Taxon",
    "Diversity_metric", "Diversity_metric_type", "Diversity_metric_unit",
    "Sampling_method", "Sampling_effort", "Sampling_effort_unit",
    "Rescaled_sampling_effort", "Measurement", "Effort_corrected_measurement",
    "Sample_date_resolution", "Sample_start_earliest", "Sample_end_latest",
    "Sample_midpoint", "study_start_date"
  ),
  names(predicts_filtered)
)
predicts_filtered <- predicts_filtered[, ..predicts_output_cols]

saveRDS(
  as.data.frame(predicts_filtered),
  predicts_filtered_observations_path,
  compress = "gzip"
)
message("Wrote: ", predicts_filtered_observations_path)

predicts_filter_summary <- data.table::data.table(
  metric = c(
    "raw_observation_rows",
    "raw_abundance_observation_rows",
    "raw_abundance_study_units",
    "retained_abundance_study_units_starting_2000_or_later",
    "retained_observation_rows",
    "retained_sites",
    "retained_columns"
  ),
  value = c(
    nrow(predicts_raw),
    nrow(predicts_raw[Diversity_metric_type == "Abundance"]),
    nrow(predicts_study_starts),
    nrow(eligible_predicts_studies),
    nrow(predicts_filtered),
    uniqueN(predicts_filtered$SSBS),
    ncol(predicts_filtered)
  )
)
write_csv_safe(predicts_filter_summary, predicts_filter_summary_path)
