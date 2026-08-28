# =====================================================
# Write combined PREDICTS-BioTIME handoff outputs
# =====================================================

assert_file_exists(predicts_output_rds_path, "OV-ready PREDICTS RDS")
assert_file_exists(biotime_output_rds_path, "OV-ready BioTIME RDS")

message("Reading OV-ready individual databases")
predicts_output <- select_integrated_cols(readRDS(predicts_output_rds_path))
biotime_output <- select_integrated_cols(readRDS(biotime_output_rds_path))
predicts_references <- data.table::as.data.table(readRDS(predicts_references_rds_path))
biotime_references <- data.table::as.data.table(readRDS(biotime_references_rds_path))

message("Screening BioTIME/PREDICTS DOI overlaps before combined output")
doi_overlap <- screen_biotime_predicts_doi_overlap(
  predicts_references = predicts_references,
  biotime_references = biotime_references,
  biotime_output = biotime_output
)
biotime_for_combined <- biotime_output[
  !study_id %in% doi_overlap$excluded_study_ids
]
message(
  "BioTIME rows excluded from combined by DOI overlap: ",
  nrow(biotime_output) - nrow(biotime_for_combined)
)

message("Combining PREDICTS and BioTIME OV-ready databases")
combined_output <- data.table::rbindlist(
  list(predicts_output, biotime_for_combined),
  fill = TRUE,
  use.names = TRUE
)
combined_output <- ascii_normalize_table(combined_output)

saveRDS(as.data.frame(combined_output), combined_output_rds_path, compress = "gzip")
message("Wrote: ", combined_output_rds_path)
write_csv_safe(combined_output, combined_output_csv_path)

manifest <- write_output_manifest(list(
  manifest_row(
    "predicts_database",
    predicts_output_rds_path,
    predicts_output,
    "OV-ready PREDICTS database with common source, site, taxonomy, date, coordinate, and abundance fields."
  ),
  manifest_row(
    "predicts_database_csv",
    predicts_output_csv_path,
    predicts_output,
    "CSV copy of the OV-ready PREDICTS database."
  ),
  manifest_row(
    "biotime_database",
    biotime_output_rds_path,
    biotime_output,
    "OV-ready BioTIME database after BioTIMEr gridding and sample-event rarefaction."
  ),
  manifest_row(
    "biotime_database_csv",
    biotime_output_csv_path,
    biotime_output,
    "CSV copy of the OV-ready BioTIME database."
  ),
  manifest_row(
    "combined_database",
    combined_output_rds_path,
    combined_output,
    "OV-ready combined PREDICTS and BioTIME database, excluding BioTIME studies with DOI overlap against retained PREDICTS references."
  ),
  manifest_row(
    "combined_database_csv",
    combined_output_csv_path,
    combined_output,
    "CSV copy of the DOI-screened OV-ready combined PREDICTS and BioTIME database."
  ),
  manifest_row(
    "predicts_info_data",
    predicts_filtered_rds_path,
    readRDS(predicts_filtered_rds_path),
    "Full filtered PREDICTS data table in the human-readable info folder."
  ),
  manifest_row(
    "predicts_info_metadata",
    predicts_metadata_rds_path,
    readRDS(predicts_metadata_rds_path),
    "PREDICTS site-level metadata for retained SSBS values."
  ),
  manifest_row(
    "predicts_info_references",
    predicts_references_rds_path,
    readRDS(predicts_references_rds_path),
    "Combined and renumbered PREDICTS reference table filtered to retained Source_ID values."
  ),
  manifest_row(
    "biotime_info_data",
    biotime_filtered_rds_path,
    biotime_output,
    "Full processed BioTIME data table in the human-readable info folder."
  ),
  manifest_row(
    "biotime_info_metadata",
    biotime_metadata_review_rds_path,
    readRDS(biotime_metadata_review_rds_path),
    "BioTIME metadata for retained rarefied studies."
  ),
  manifest_row(
    "biotime_info_references",
    biotime_references_rds_path,
    biotime_references,
    "BioTIME references for retained rarefied studies, with parsed citation fields."
  ),
  manifest_row(
    "biotime_predicts_doi_overlap_summary",
    biotime_predicts_overlap_summary_path,
    doi_overlap$summary,
    "Summary of DOI-based BioTIME/PREDICTS source overlap screening used only for the combined handoff."
  )
))

message("Combined output manifest rows: ", nrow(manifest))
message("Biodiversity data integration complete")
