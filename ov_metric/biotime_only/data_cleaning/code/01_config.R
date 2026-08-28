code_dir <- getwd()
stage_dir <- normalizePath(file.path(code_dir, ".."), mustWork = TRUE)
workflow_dir <- normalizePath(file.path(stage_dir, ".."), mustWork = TRUE)
ov_metric_dir <- normalizePath(file.path(workflow_dir, ".."), mustWork = TRUE)
project_dir <- normalizePath(file.path(ov_metric_dir, ".."), mustWork = TRUE)

local_lib <- file.path(workflow_dir, ".Rlib")
if (dir.exists(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

output_dir <- file.path(stage_dir, "output")
tmp_dir <- file.path(stage_dir, "tmp")
processed_data_dir <- file.path(workflow_dir, "data", "processed")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_data_dir, recursive = TRUE, showWarnings = FALSE)

raw_biotime_data_rds <- file.path(project_dir, "00_biodiversity_data", "biotime", "biotime_data_raw.rds")
raw_biotime_metadata_rds <- file.path(project_dir, "00_biodiversity_data", "biotime", "biotime_metadata_raw.rds")
raw_biotime_references_rds <- file.path(project_dir, "00_biodiversity_data", "biotime", "biotime_references_raw.rds")
aez_path <- file.path(project_dir, "00_spatial_data", "aez", "AEZ_shp_file.shp")
integration_helper_path <- file.path(ov_metric_dir, "01_biodiversity_data_integration", "code", "02_helpers.R")
lookup_tables_dir <- file.path(ov_metric_dir, "01_biodiversity_data_integration", "lookup_tables")
taxon_path <- file.path(lookup_tables_dir, "taxon.rds")
bird_codes_path <- file.path(lookup_tables_dir, "bird_codes.rds")
plant_codes_path <- file.path(lookup_tables_dir, "plant_codes.rds")
phylo_tree_path <- file.path(ov_metric_dir, "02_ov_calculation", "build", "input", "iphylo_tree.nwk")

min_biotime_year <- 2000L
max_hansen_year <- 2025L
min_years_per_time_series <- 2L
coordinate_digits <- 6L
analysis_crs <- 6933
biotime_grid_resolution <- 12L
biotime_rarefaction_seed <- 42L

clean_sample_year_path <- file.path(output_dir, "biotime_clean_sample_year.csv")
clean_timeseries_path <- file.path(output_dir, "biotime_timeseries.csv")
cleaning_summary_path <- file.path(output_dir, "biotime_cleaning_summary.csv")
taxon_summary_path <- file.path(output_dir, "biotime_taxon_group_summary.csv")
standardized_taxon_rows_path <- file.path(output_dir, "biotime_standardized_taxon_rows.csv")
ov_component_scores_path <- file.path(output_dir, "biotime_composite_ov_components.csv")
pd_result_path <- file.path(output_dir, "biotime_pd_result.csv")
pd_matched_families_path <- file.path(tmp_dir, "biotime_family_names_present.txt")
pd_unmatched_families_path <- file.path(tmp_dir, "biotime_family_names_unmatched.txt")
biotime_filtered_unstandardized_rds_path <- file.path(tmp_dir, "biotime_filtered_unstandardized.rds")
biotime_gridded_event_lookup_rds_path <- file.path(tmp_dir, "biotime_gridded_event_lookup.rds")
biotime_rarefied_events_rds_path <- file.path(tmp_dir, "biotime_rarefied_sample_events.rds")
biotime_taxonomy_cache_rds_path <- file.path(tmp_dir, "biotime_taxonomy_cache.rds")
biotime_taxonomy_resolution_path <- file.path(output_dir, "biotime_taxonomy_resolution.csv")

integrated_database_cols <- c(
  "database", "sample_id", "site_id", "assemblage_id", "study_id",
  "source_id", "standard_source_id", "reference", "doi",
  "sample_year", "sample_start_date", "sample_end_date", "sample_midpoint",
  "latitude", "longitude",
  "taxon_name", "resolved_name", "Kingdom", "Phylum", "Class", "Order",
  "Family", "Genus", "Species",
  "measurement", "abundance", "effort_corrected_measurement",
  "sampling_effort", "sampling_effort_unit",
  "effort_standardized", "effort_standardization_method",
  "raw_sample_events_available", "sample_events_selected",
  "n_observation_rows", "rarefaction_seed", "biotime_grid_resolution"
)
