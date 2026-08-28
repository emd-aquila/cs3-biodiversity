# =====================================================
# Process PREDICTS inputs into info and OV-ready outputs
# =====================================================

assert_file_exists(predicts_raw_rds_path, "raw PREDICTS database RDS")
assert_file_exists(predicts_metadata_raw_rds_path, "raw PREDICTS site summary RDS")

message("Reading raw PREDICTS inputs")
predicts_dt <- data.table::as.data.table(readRDS(predicts_raw_rds_path))
sites <- data.table::as.data.table(readRDS(predicts_metadata_raw_rds_path))
assert_nonempty_df(predicts_dt, "raw PREDICTS database")
assert_nonempty_df(sites, "PREDICTS site summaries")

message("Screening and combining manually downloaded NHM reference tables")
refs_2016 <- data.table::fread(predicts_reference_2016_path, encoding = "UTF-8")
refs_2022 <- data.table::fread(predicts_reference_2022_path, encoding = "UTF-8")

overlap <- screen_reference_overlap(refs_2016, refs_2022)
write_csv_safe(overlap$summary, predicts_reference_overlap_path)
blocking_overlap_details <- overlap$details[overlap_key != "_id"]
if (nrow(blocking_overlap_details) > 0) {
  details_path <- sub("\\.csv$", "_details.csv", predicts_reference_overlap_path)
  write_csv_safe(blocking_overlap_details, details_path)
  stop("Reference overlap detected between 2016 and 2022 tables. Review: ", details_path, call. = FALSE)
}
references <- ascii_normalize_table(combine_reference_tables(refs_2016, refs_2022))

message("Filtering PREDICTS to abundance records with sampling starts dated 2000-2024")
predicts_dt[, Sample_start_earliest := as_date_safe(Sample_start_earliest)]
predicts_filtered <- predicts_dt[
  Diversity_metric_type == "Abundance" &
    !is.na(Sample_start_earliest) &
    Sample_start_earliest >= date_min &
    Sample_start_earliest <= date_max
]
predicts_filtered_cols_available <- intersect(predicts_filtered_cols, names(predicts_filtered))
predicts_filtered <- predicts_filtered[, ..predicts_filtered_cols_available]
predicts_filtered <- ascii_normalize_table(predicts_filtered)

message("Filtering PREDICTS site-level metadata and references to retained rows")
if ("SSBS" %in% names(sites) && "SSBS" %in% names(predicts_filtered)) {
  predicts_metadata <- sites[SSBS %in% unique(predicts_filtered$SSBS)]
} else {
  predicts_metadata <- sites
}
predicts_metadata <- ascii_normalize_table(predicts_metadata)
if ("Source_ID" %in% names(references) && "Source_ID" %in% names(predicts_filtered)) {
  predicts_references <- references[Source_ID %in% unique(predicts_filtered$Source_ID)]
} else {
  predicts_references <- references
}

message("Building OV-ready PREDICTS database")
predicts_output <- build_predicts_output_database(predicts_filtered, predicts_references)

saveRDS(as.data.frame(predicts_filtered), predicts_filtered_rds_path, compress = "gzip")
saveRDS(as.data.frame(predicts_metadata), predicts_metadata_rds_path, compress = "gzip")
saveRDS(as.data.frame(predicts_references), predicts_references_rds_path, compress = "gzip")
saveRDS(as.data.frame(predicts_output), predicts_output_rds_path, compress = "gzip")
message("Wrote: ", predicts_filtered_rds_path)
message("Wrote: ", predicts_metadata_rds_path)
message("Wrote: ", predicts_references_rds_path)
message("Wrote: ", predicts_output_rds_path)

write_csv_safe(predicts_filtered, predicts_filtered_csv_path)
write_csv_safe(predicts_metadata, predicts_metadata_csv_path)
write_csv_safe(predicts_references, predicts_references_path)
write_csv_safe(predicts_output, predicts_output_csv_path)

summary_table <- data.table::data.table(
  artifact = c(
    "raw_combined_database_rds",
    "raw_site_summaries_rds",
    "filtered_database_csv",
    "filtered_database_rds",
    "metadata_csv",
    "metadata_rds",
    "combined_reference_table_csv",
    "combined_reference_table_rds",
    "ov_ready_output_csv",
    "ov_ready_output_rds",
    "reference_overlap_screen_csv"
  ),
  path = c(
    predicts_raw_rds_path,
    predicts_metadata_raw_rds_path,
    predicts_filtered_csv_path,
    predicts_filtered_rds_path,
    predicts_metadata_csv_path,
    predicts_metadata_rds_path,
    predicts_references_path,
    predicts_references_rds_path,
    predicts_output_csv_path,
    predicts_output_rds_path,
    predicts_reference_overlap_path
  ),
  rows = c(
    nrow(predicts_dt),
    nrow(sites),
    nrow(predicts_filtered),
    nrow(predicts_filtered),
    nrow(predicts_metadata),
    nrow(predicts_metadata),
    nrow(predicts_references),
    nrow(predicts_references),
    nrow(predicts_output),
    nrow(predicts_output),
    nrow(overlap$summary)
  ),
  columns = c(
    ncol(predicts_dt),
    ncol(sites),
    ncol(predicts_filtered),
    ncol(predicts_filtered),
    ncol(predicts_metadata),
    ncol(predicts_metadata),
    ncol(references),
    ncol(references),
    ncol(predicts_output),
    ncol(predicts_output),
    ncol(overlap$summary)
  )
)
write_csv_safe(summary_table, predicts_summary_path)

readme_lines <- c(
  "# PREDICTS Inputs",
  "",
  "Raw data are downloaded by `../00_biodiversity_data/code/03_download_predicts.R` and processed by `code/03_process_predicts.R`.",
  "",
  "Downloads follow the `predictsr` package workflow: `LoadPredictsData()` and `GetSitelevelSummaries()`.",
  "",
  "The data table is filtered to abundance records with `Sample_start_earliest` from 2000-01-01 through 2024-12-31.",
  "",
  "## Human-readable CSVs",
  "",
  paste0("- Data: `", predicts_filtered_csv_path, "`"),
  paste0("- Metadata: `", predicts_metadata_csv_path, "`"),
  paste0("- References: `", predicts_references_path, "`"),
  "",
  "## OV-ready handoff",
  "",
  paste0("- CSV: `", predicts_output_csv_path, "`"),
  paste0("- RDS: `", predicts_output_rds_path, "`"),
  "",
  "## Summary",
  "",
  paste0("- Raw combined database rows: ", format(nrow(predicts_dt), big.mark = ",")),
  paste0("- Filtered database rows: ", format(nrow(predicts_filtered), big.mark = ",")),
  paste0("- OV-ready rows: ", format(nrow(predicts_output), big.mark = ",")),
  paste0("- Metadata rows: ", format(nrow(predicts_metadata), big.mark = ",")),
  paste0("- Filtered reference rows: ", format(nrow(predicts_references), big.mark = ","))
)
writeLines(readme_lines, predicts_readme_path)
message("Wrote: ", predicts_readme_path)

message("PREDICTS processing complete")
