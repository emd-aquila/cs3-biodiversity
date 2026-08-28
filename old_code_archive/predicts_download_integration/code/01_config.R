# =====================================================
# Configuration for PREDICTS integration inputs.
# =====================================================

code_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
integration_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = TRUE)

tmp_dir <- file.path(integration_dir, ".tmp")
raw_downloads_dir <- file.path(integration_dir, "raw_downloads")
rds_outputs_dir <- file.path(integration_dir, "rds_outputs")
predicts_info_dir <- file.path(integration_dir, "predicts_info")

for (dir_path in c(
  tmp_dir,
  raw_downloads_dir,
  rds_outputs_dir,
  predicts_info_dir
)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

# PREDICTS is distributed through two NHM Data Portal release pages. The
predicts_release_years <- c(2016, 2022)
predicts_raw_rds_path <- file.path(raw_downloads_dir, "predicts_database_raw.rds")
predicts_raw_aux_json_path <- paste0(predicts_raw_rds_path, ".aux.json")
predicts_reference_2016_path <- file.path(raw_downloads_dir, "predicts_references_2016.csv")
predicts_reference_2022_path <- file.path(raw_downloads_dir, "predicts_references_2022.csv")

predicts_filtered_rds_path <- file.path(rds_outputs_dir, "predicts_database_filtered.rds")
predicts_sites_rds_path <- file.path(rds_outputs_dir, "predicts_site_summaries.rds")
rds_outputs_manifest_path <- file.path(rds_outputs_dir, "rds_outputs.csv")

predicts_columns_path <- file.path(predicts_info_dir, "predicts_data_extract_columns.csv")
predicts_references_path <- file.path(predicts_info_dir, "predicts_reference_table.csv")
predicts_database_preview_path <- file.path(predicts_info_dir, "predicts_database_preview.csv")
predicts_sites_preview_path <- file.path(predicts_info_dir, "predicts_site_summaries_preview.csv")
predicts_column_inventory_path <- file.path(tmp_dir, "predicts_column_inventory.csv")
predicts_reference_overlap_path <- file.path(tmp_dir, "predicts_reference_overlap_screen.csv")
predicts_summary_path <- file.path(tmp_dir, "predicts_download_summary.csv")
predicts_readme_path <- file.path(tmp_dir, "predicts_readme.md")

preview_n_rows <- 1000L
date_min <- as.Date("2000-01-01")
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
