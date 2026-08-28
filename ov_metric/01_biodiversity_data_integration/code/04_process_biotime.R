# =====================================================
# Process BioTIME inputs into info and OV-ready outputs
# =====================================================

assert_file_exists(biotime_raw_data_rds_path, "BioTIME observation RDS")
assert_file_exists(biotime_raw_metadata_rds_path, "BioTIME metadata RDS")
assert_file_exists(biotime_raw_references_rds_path, "BioTIME reference RDS")
assert_file_exists(taxon_path, "GBIF taxon lookup")
assert_file_exists(bird_codes_path, "curated bird code lookup")

build_plant_lookup_if_missing()

message("Reading BioTIME metadata and references")
biotime_metadata_raw <- data.table::as.data.table(readRDS(biotime_raw_metadata_rds_path))
biotime_references_raw <- data.table::as.data.table(readRDS(biotime_raw_references_rds_path))
biotime_metadata <- ascii_normalize_table(biotime_metadata_raw)
biotime_references <- ascii_normalize_table(biotime_references_raw)

assert_has_cols(
  biotime_metadata,
  c("STUDY_ID", "REALM", "AB_BIO", "START_YEAR", "END_YEAR"),
  "BioTIME metadata"
)
assert_has_cols(biotime_references, c("STUDY_ID", "CITATION_ID", "BIB"), "BioTIME references")

message("Parsing BioTIME reference fields")
biotime_references[, DOI := extract_bib_field(BIB, "DOI")]
biotime_references[, reference_author := extract_bib_field(BIB, "Author")]
biotime_references[, reference_year := extract_bib_field(BIB, "Year")]
biotime_references[, reference_title := extract_bib_field(BIB, "Title")]
biotime_references[, Journal := extract_bib_field(BIB, "Journal")]
biotime_references[, Number := extract_bib_field(BIB, "Number")]
biotime_references[, Pages := extract_bib_field(BIB, "Pages")]
biotime_references[, Volume := extract_bib_field(BIB, "Volume")]
biotime_references[, Issue := extract_bib_field(BIB, "Issue")]
biotime_references[, URL := extract_bib_field(BIB, "Url")]

message("Flagging BioTIME metadata eligibility")
biotime_metadata[, terrestrial_realm := !is.na(REALM) & REALM == "Terrestrial"]
biotime_metadata[, abundance_based := !is.na(AB_BIO) & grepl("A", toupper(AB_BIO), fixed = TRUE)]
biotime_metadata[, starts_before_2000 := !is.na(START_YEAR) & START_YEAR < 2000L]
biotime_metadata[, eligible := terrestrial_realm & abundance_based]
biotime_metadata[
  ,
  exclusion_reason := vapply(
    seq_len(.N),
    function(row_index) {
      reasons <- c(
        if (!terrestrial_realm[[row_index]]) "not_terrestrial",
        if (!abundance_based[[row_index]]) "not_abundance_based"
      )
      paste(reasons, collapse = ";")
    },
    character(1)
  )
]

eligible_study_ids <- biotime_metadata[eligible == TRUE, STUDY_ID]
observation_cols <- c(
  "ID_ALL_RAW_DATA", "ABUNDANCE", "BIOMASS", "ID_SPECIES", "SAMPLE_DESC",
  "LATITUDE", "LONGITUDE", "DEPTH", "DAY", "MONTH", "YEAR", "STUDY_ID",
  "newID", "valid_name", "resolution", "taxon"
)

check_biotimer_schema(
  observation_cols,
  c("STUDY_ID", "REALM", "AB_BIO", "START_YEAR", "END_YEAR")
)

message("Reading BioTIME observations")
biotime_observations <- load_biotime_observations(biotime_raw_data_rds_path, observation_cols)
biotime_observations <- ascii_normalize_table(biotime_observations)

message("Filtering BioTIME observations before taxonomy assignment")
biotime_filtered_pre_taxonomy <- biotime_observations[
  STUDY_ID %in% eligible_study_ids &
    !is.na(YEAR) &
    YEAR >= as.integer(format(biotime_date_min, "%Y")) &
    YEAR <= as.integer(format(date_max, "%Y")) &
    !is.na(ABUNDANCE)
]

invalid_terms <- c(
  "morphospecies", "unknown", "undetermined", "unidentif",
  "sp\\.", "spp\\.", "sp$", "cf\\.", "indet\\.", "sp[0-9a-zA-Z]+"
)
invalid_pattern <- paste(invalid_terms, collapse = "|")
biotime_filtered <- biotime_filtered_pre_taxonomy[
  !is.na(valid_name) &
    nzchar(valid_name) &
    !grepl(invalid_pattern, valid_name, ignore.case = TRUE) &
    (
      is.na(resolution) |
        !(tolower(resolution) %in% c("phylum", "subphylum", "superclass", "kingdom"))
    )
]
biotime_filtered[
  ,
  sample_event_id := paste(
    STUDY_ID, SAMPLE_DESC, LATITUDE, LONGITUDE, YEAR, MONTH, DAY,
    sep = "__"
  )
]

message("Resolving BioTIME taxonomy with cache")
biotime_filtered <- resolve_biotime_taxonomy(biotime_filtered)
saveRDS(as.data.frame(biotime_filtered), biotime_filtered_unstandardized_rds_path, compress = "gzip")
message("Wrote: ", biotime_filtered_unstandardized_rds_path)

message("Applying BioTIME sample-event rarefaction")
biotime_standardized <- standardize_biotime_sampling(
  biotime_filtered = biotime_filtered,
  biotime_metadata = biotime_metadata[eligible == TRUE],
  biotime_reference_table = biotime_references,
  grid_resolution = biotime_grid_resolution,
  rarefaction_seed = biotime_rarefaction_seed
)

saveRDS(as.data.frame(biotime_standardized), biotime_filtered_rds_path, compress = "gzip")
saveRDS(as.data.frame(biotime_standardized), biotime_output_rds_path, compress = "gzip")
message("Wrote: ", biotime_filtered_rds_path)
message("Wrote: ", biotime_output_rds_path)

message("Building BioTIME study-level metadata review table")
biotime_study_summary <- biotime_standardized[
  ,
  .(
    n_standardized_rows = .N,
    n_samples = data.table::uniqueN(sample_id),
    n_sites = data.table::uniqueN(site_id),
    n_taxa = data.table::uniqueN(taxon_name),
    n_families = data.table::uniqueN(Family[!is.na(Family)]),
    observation_start_year = min(sample_year, na.rm = TRUE),
    observation_end_year = max(sample_year, na.rm = TRUE),
    observation_centroid_latitude = mean(latitude, na.rm = TRUE),
    observation_centroid_longitude = mean(longitude, na.rm = TRUE)
  ),
  by = .(STUDY_ID = as.integer(study_id))
]

biotime_metadata_review <- merge(
  biotime_metadata,
  biotime_study_summary,
  by = "STUDY_ID",
  all.x = TRUE,
  sort = FALSE
)
review_first_cols <- intersect(
  c(
    "STUDY_ID", "TITLE", "REALM", "TAXA", "ORGANISMS", "AB_BIO", "ABUNDANCE_TYPE",
    "START_YEAR", "END_YEAR", "eligible", "exclusion_reason", "starts_before_2000",
    "n_standardized_rows", "n_samples", "n_sites", "n_taxa", "n_families",
    "observation_start_year", "observation_end_year",
    "CEN_LATITUDE", "CEN_LONGITUDE", "CENT_LAT", "CENT_LONG",
    "observation_centroid_latitude", "observation_centroid_longitude", "WEB_LINK"
  ),
  names(biotime_metadata_review)
)
data.table::setcolorder(
  biotime_metadata_review,
  c(review_first_cols, setdiff(names(biotime_metadata_review), review_first_cols))
)
retained_study_ids <- unique(as.integer(biotime_standardized$study_id))
biotime_metadata_output <- biotime_metadata_review[STUDY_ID %in% retained_study_ids]

saveRDS(as.data.frame(biotime_study_summary), biotime_study_summary_rds_path, compress = "gzip")
message("Wrote: ", biotime_study_summary_rds_path)

message("Building BioTIME reference review table")
reference_metadata_cols <- intersect(
  c(
    "STUDY_ID", "TITLE", "REALM", "TAXA", "ORGANISMS", "AB_BIO", "ABUNDANCE_TYPE",
    "START_YEAR", "END_YEAR", "eligible", "exclusion_reason", "starts_before_2000", "WEB_LINK"
  ),
  names(biotime_metadata_review)
)
biotime_reference_table <- merge(
  biotime_references,
  biotime_metadata_review[, ..reference_metadata_cols],
  by = "STUDY_ID",
  all.x = TRUE,
  sort = FALSE
)
biotime_reference_table <- biotime_reference_table[STUDY_ID %in% retained_study_ids]

write_csv_safe(biotime_reference_table, biotime_reference_table_path)
write_csv_safe(biotime_standardized, biotime_database_csv_path)
write_csv_safe(biotime_metadata_output, biotime_metadata_csv_path)
write_csv_safe(biotime_standardized, biotime_output_csv_path)
saveRDS(as.data.frame(biotime_metadata_output), biotime_metadata_review_rds_path, compress = "gzip")
saveRDS(as.data.frame(biotime_reference_table), biotime_references_rds_path, compress = "gzip")
message("Wrote: ", biotime_metadata_review_rds_path)
message("Wrote: ", biotime_references_rds_path)

taxonomy_resolution <- unique(
  biotime_filtered[, c("valid_name_original", "resolved_name", "resolution_method", taxonomy_cols()), with = FALSE]
)
write_csv_safe(taxonomy_resolution, biotime_taxonomy_resolution_path)
saveRDS(as.data.frame(taxonomy_resolution), biotime_taxonomy_resolution_rds_path, compress = "gzip")
message("Wrote: ", biotime_taxonomy_resolution_rds_path)

taxonomy_method_counts <- biotime_filtered[
  ,
  .N,
  by = .(resolution_method = ifelse(is.na(resolution_method), "unresolved", resolution_method))
][order(-N)]

selected_events <- data.table::as.data.table(readRDS(biotime_rarefied_events_rds_path))
biotime_filter_summary <- data.table::data.table(
  metric = c(
    "metadata_studies_total",
    "metadata_studies_eligible",
    "metadata_studies_excluded",
    "pre_taxonomy_observation_rows",
    "pre_taxonomy_studies_with_observations",
    "taxonomy_filter_removed_rows",
    "taxonomy_filtered_observation_rows",
    "taxonomy_filtered_sample_events",
    "taxonomy_filtered_taxa",
    "standardized_rows",
    "standardized_samples",
    "standardized_sites",
    "standardized_studies",
    "rarefied_sample_events_selected",
    "taxonomy_cache_rows",
    "taxonomy_resolution_methods"
  ),
  value = c(
    nrow(biotime_metadata),
    nrow(biotime_metadata[eligible == TRUE]),
    nrow(biotime_metadata[eligible == FALSE]),
    nrow(biotime_filtered_pre_taxonomy),
    data.table::uniqueN(biotime_filtered_pre_taxonomy$STUDY_ID),
    nrow(biotime_filtered_pre_taxonomy) - nrow(biotime_filtered),
    nrow(biotime_filtered),
    data.table::uniqueN(biotime_filtered$sample_event_id),
    data.table::uniqueN(biotime_filtered$valid_name),
    nrow(biotime_standardized),
    data.table::uniqueN(biotime_standardized$sample_id),
    data.table::uniqueN(biotime_standardized$site_id),
    data.table::uniqueN(biotime_standardized$study_id),
    data.table::uniqueN(selected_events$sample_event_id),
    nrow(readRDS(biotime_taxonomy_cache_rds_path)),
    paste(taxonomy_method_counts[, paste0(resolution_method, "=", N)], collapse = ";")
  )
)
write_csv_safe(biotime_filter_summary, biotime_filter_summary_path)

message("BioTIME processing complete")
