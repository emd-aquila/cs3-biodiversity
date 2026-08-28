# =====================================================
# Configuration for biodiversity data integration inputs
# =====================================================

# Working directory is currently the code directory
integration_dir <- normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = TRUE)
ov_metric_dir <- normalizePath(file.path(integration_dir, ".."), winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(file.path(ov_metric_dir, ".."), winslash = "/", mustWork = TRUE)
biodiversity_data_dir <- file.path(project_dir, "00_biodiversity_data")

# Derive other directories from integration_dir and create them
code_dir <- file.path(integration_dir, "code")
predicts_raw_dir <- file.path(biodiversity_data_dir, "predicts")
biotime_raw_dir <- file.path(biodiversity_data_dir, "biotime")
lookup_tables_dir <- file.path(integration_dir, "lookup_tables")
output_dir <- file.path(integration_dir, "output")
predicts_dir <- file.path(integration_dir, "predicts_info")
biotime_dir <- file.path(integration_dir, "biotime_info")
tmp_dir <- file.path(integration_dir, ".tmp")
overlap_info_dir <- file.path(integration_dir, "overlap_info")

for (dir_path in c(
  tmp_dir,
  lookup_tables_dir,
  output_dir,
  predicts_dir,
  biotime_dir,
  overlap_info_dir,
  tmp_dir
)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

# Specifications and paths for PREDICTS database
predicts_release_years <- c(2016, 2022)
predicts_raw_rds_path <- file.path(predicts_raw_dir, "predicts_database_raw.rds")
predicts_raw_aux_json_path <- paste0(predicts_raw_rds_path, ".aux.json")
predicts_metadata_raw_rds_path <- file.path(predicts_raw_dir, "predicts_site_summaries_raw.rds")
predicts_reference_2016_path <- file.path(predicts_raw_dir, "predicts_references_2016.csv")
predicts_reference_2022_path <- file.path(predicts_raw_dir, "predicts_references_2022.csv")

output_manifest_path <- file.path(output_dir, "output.csv")
predicts_output_rds_path <- file.path(output_dir, "predicts_database.rds")
predicts_output_csv_path <- file.path(output_dir, "predicts_database.csv")
biotime_output_rds_path <- file.path(output_dir, "biotime_database.rds")
biotime_output_csv_path <- file.path(output_dir, "biotime_database.csv")
combined_output_rds_path <- file.path(output_dir, "combined_database.rds")
combined_output_csv_path <- file.path(output_dir, "combined_database.csv")

predicts_filtered_csv_path <- file.path(predicts_dir, "predicts_data.csv")
predicts_filtered_rds_path <- file.path(predicts_dir, "predicts_data.rds")
predicts_metadata_csv_path <- file.path(predicts_dir, "predicts_metadata.csv")
predicts_metadata_rds_path <- file.path(predicts_dir, "predicts_metadata.rds")
predicts_references_path <- file.path(predicts_dir, "predicts_references.csv")
predicts_references_rds_path <- file.path(predicts_dir, "predicts_references.rds")
predicts_reference_overlap_path <- file.path(tmp_dir, "predicts_reference_overlap_screen.csv")
predicts_summary_path <- file.path(tmp_dir, "predicts_download_summary.csv")
predicts_readme_path <- file.path(tmp_dir, "predicts_readme.md")

# PREDICTS remains on its established 2000+ window. BioTIME can be rebuilt for
# a longer historical period without changing the PREDICTS extraction.
date_min <- as.Date("2000-01-01")
biotime_min_year <- suppressWarnings(as.integer(Sys.getenv("BIOTIME_MIN_YEAR", unset = "2000")))
if (!is.finite(biotime_min_year) || biotime_min_year < 1800L || biotime_min_year > 2025L) {
  stop("BIOTIME_MIN_YEAR must be a year between 1800 and 2025.", call. = FALSE)
}
biotime_date_min <- as.Date(sprintf("%04d-01-01", biotime_min_year))
date_max <- as.Date("2024-12-31")

predicts_filtered_cols <- c(
  "Reference", "Study_number", "Site_name", "Site_number", "Source_ID", "SSBS",
  "Country",
  "Biome", "Predominant_land_use", "Use_intensity",
  "Years_since_fragmentation_or_conversion",
  "UN_subregion",
  "Hotspot",
  "Latitude", "Longitude",
  "Kingdom", "Phylum", "Class", "Order", "Family", "Genus",
  "Diversity_metric_unit",
  "Effort_corrected_measurement", "Measurement",
  "Sampling_effort", "Sampling_effort_unit",
  "Sample_date_resolution", "Sample_start_earliest", "Sample_end_latest", "Sample_midpoint"
)

# BioTIME 2.0 raw files are downloaded by 00_biodiversity_data.
biotime_raw_data_rds_path <- file.path(biotime_raw_dir, "biotime_data_raw.rds")
biotime_raw_metadata_rds_path <- file.path(biotime_raw_dir, "biotime_metadata_raw.rds")
biotime_raw_references_rds_path <- file.path(biotime_raw_dir, "biotime_references_raw.rds")

gbif_taxon_extract_dir <- file.path(tmp_dir, "gbif_backbone_current")

biotime_filtered_rds_path <- file.path(biotime_dir, "biotime_data.rds")
biotime_metadata_review_rds_path <- file.path(biotime_dir, "biotime_metadata.rds")
biotime_references_rds_path <- file.path(biotime_dir, "biotime_references.rds")
biotime_filtered_unstandardized_rds_path <- file.path(tmp_dir, "biotime_filtered_unstandardized.rds")
biotime_gridded_event_lookup_rds_path <- file.path(tmp_dir, "biotime_gridded_event_lookup.rds")
biotime_rarefied_events_rds_path <- file.path(tmp_dir, "biotime_rarefied_sample_events.rds")
biotime_study_summary_rds_path <- file.path(tmp_dir, "biotime_study_summary.rds")
biotime_taxonomy_resolution_rds_path <- file.path(tmp_dir, "biotime_taxonomy_resolution.rds")
biotime_taxonomy_cache_rds_path <- file.path(tmp_dir, "biotime_taxonomy_cache.rds")

biotime_reference_table_path <- file.path(biotime_dir, "biotime_references.csv")
biotime_database_csv_path <- file.path(biotime_dir, "biotime_data.csv")
biotime_metadata_csv_path <- file.path(biotime_dir, "biotime_metadata.csv")
biotime_taxonomy_resolution_path <- file.path(tmp_dir, "biotime_taxonomy_resolution.csv")
biotime_filter_summary_path <- file.path(biotime_dir, "biotime_filter_summary.csv")

bird_codes_path <- file.path(lookup_tables_dir, "bird_codes.rds")
plant_codes_path <- file.path(lookup_tables_dir, "plant_codes.rds")
taxon_path <- file.path(lookup_tables_dir, "taxon.rds")

biotime_grid_resolution <- 12L
biotime_rarefaction_seed <- 42L

integrated_database_cols <- c(
  "database", "sample_id", "site_id", "assemblage_id", "study_id",
  "source_id", "standard_source_id", "reference", "doi",
  "sample_year", "sample_start_date", "sample_end_date", "sample_midpoint",
  "latitude", "longitude",
  "taxon_name", "resolved_name", "Kingdom", "Phylum", "Class", "Order",
  "Family", "Genus", "Species",
  "measurement", "abundance", "effort_corrected_measurement",
  "sampling_effort", "sampling_effort_unit", "effort_standardized",
  "effort_standardization_method",
  "raw_sample_events_available", "sample_events_selected",
  "n_observation_rows", "rarefaction_seed", "biotime_grid_resolution"
)

biotime_reference_overlap_path <- file.path(overlap_info_dir, "biotime_predicts_doi_overlap_candidates.csv")
biotime_overlap_excluded_studies_path <- file.path(overlap_info_dir, "biotime_studies_excluded_from_combined_by_doi.csv")
biotime_spatiotemporal_overlap_path <- file.path(overlap_info_dir, "biotime_predicts_spatiotemporal_overlap_candidates.csv")
biotime_predicts_overlap_summary_path <- file.path(overlap_info_dir, "biotime_predicts_overlap_summary.csv")
