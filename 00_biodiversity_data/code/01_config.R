# =====================================================
# Configuration for raw biodiversity data downloads
# =====================================================

# Working directory is currently 00_biodiversity_data/code
biodiversity_data_dir <- normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(file.path(biodiversity_data_dir, ".."), winslash = "/", mustWork = TRUE)

predicts_raw_dir <- file.path(biodiversity_data_dir, "predicts")
biotime_raw_dir <- file.path(biodiversity_data_dir, "biotime")
tmp_dir <- file.path(biodiversity_data_dir, ".tmp")

integration_dir <- file.path(project_dir, "ov_metric", "01_biodiversity_data_integration")
lookup_tables_dir <- file.path(integration_dir, "lookup_tables")
predicts_info_dir <- file.path(integration_dir, "predicts_info")

for (dir_path in c(predicts_raw_dir, biotime_raw_dir, tmp_dir, lookup_tables_dir)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

# PREDICTS raw database inputs
predicts_release_years <- c(2016, 2022)
predicts_raw_rds_path <- file.path(predicts_raw_dir, "predicts_database_raw.rds")
predicts_raw_aux_json_path <- paste0(predicts_raw_rds_path, ".aux.json")
predicts_metadata_raw_rds_path <- file.path(predicts_raw_dir, "predicts_site_summaries_raw.rds")
predicts_reference_2016_path <- file.path(predicts_raw_dir, "predicts_references_2016.csv")
predicts_reference_2022_path <- file.path(predicts_raw_dir, "predicts_references_2022.csv")
predicts_metadata_rds_path <- file.path(predicts_info_dir, "predicts_metadata.rds")

# BioTIME 2.0 raw database inputs from the official Zenodo record.
biotime_query_url <- "https://zenodo.org/records/15222193/files/biotime_v2_query_15April25.rds?download=1"
biotime_metadata_url <- "https://zenodo.org/records/15222193/files/biotime_v2_metadata_15April25.csv?download=1"
biotime_references_url <- "https://zenodo.org/records/15222193/files/references_biotime_v2_15April25.csv?download=1"

biotime_raw_data_rds_path <- file.path(biotime_raw_dir, "biotime_data_raw.rds")
biotime_raw_metadata_rds_path <- file.path(biotime_raw_dir, "biotime_metadata_raw.rds")
biotime_raw_references_rds_path <- file.path(biotime_raw_dir, "biotime_references_raw.rds")

# Taxonomy source used to rebuild the BioTIME plant lookup table.
gbif_backbone_url <- "https://hosted-datasets.gbif.org/datasets/backbone/current/backbone.zip"
gbif_backbone_zip_path <- file.path(biotime_raw_dir, "gbif_backbone_current.zip")
gbif_taxon_extract_dir <- file.path(tmp_dir, "gbif_backbone_current")
taxon_path <- file.path(lookup_tables_dir, "taxon.rds")
bird_codes_path <- file.path(lookup_tables_dir, "bird_codes.rds")
