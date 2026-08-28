# =====================================================
# Download/cache BioTIME and taxonomy inputs
# =====================================================

options(timeout = max(1200, getOption("timeout", 60)))

message("Preparing BioTIME raw downloads")
download_if_missing(biotime_query_url, biotime_raw_data_rds_path)
download_csv_as_rds_if_missing(biotime_metadata_url, biotime_raw_metadata_rds_path)
download_csv_as_rds_if_missing(biotime_references_url, biotime_raw_references_rds_path)

assert_file_exists(biotime_raw_data_rds_path, "BioTIME observation RDS")
assert_file_exists(biotime_raw_metadata_rds_path, "BioTIME metadata RDS")
assert_file_exists(biotime_raw_references_rds_path, "BioTIME reference RDS")

message("Ensuring GBIF backbone taxon lookup is available")
ensure_gbif_taxon_rds()

if (!file.exists(bird_codes_path)) {
  stop(
    "Missing manually curated BioTIME bird code lookup: ",
    bird_codes_path,
    "\nThis file is a curated local input and is not downloaded by this script.",
    call. = FALSE
  )
}

message("BioTIME download  complete")
