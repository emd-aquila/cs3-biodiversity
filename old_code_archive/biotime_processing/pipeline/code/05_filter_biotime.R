# =====================================================
# Filter BioTIME to terrestrial abundance records after 2000.
# =====================================================

assert_file_exists(biotime_data_path, "BioTIME observation CSV")
assert_file_exists(biotime_metadata_path, "BioTIME metadata CSV")
assert_file_exists(biotime_references_path, "BioTIME reference CSV")

biotime_metadata <- data.table::fread(biotime_metadata_path, showProgress = FALSE)
biotime_references <- data.table::fread(biotime_references_path, showProgress = FALSE)

assert_has_cols(
  biotime_metadata,
  c("STUDY_ID", "REALM", "AB_BIO", "START_YEAR", "END_YEAR"),
  "BioTIME metadata"
)
assert_has_cols(biotime_references, c("STUDY_ID", "CITATION_ID", "BIB"), "BioTIME references")

biotime_references[, DOI := extract_dois(BIB)]
biotime_reference_summary <- biotime_references[
  ,
  .(
    CITATION_ID = collapse_unique(CITATION_ID),
    DOI = collapse_unique(DOI),
    BIB = collapse_unique(BIB)
  ),
  by = STUDY_ID
]

biotime_metadata[
  ,
  terrestrial_realm := !is.na(REALM) & REALM == "Terrestrial"
]
biotime_metadata[
  ,
  abundance_based := !is.na(AB_BIO) & grepl("A", toupper(AB_BIO), fixed = TRUE)
]
biotime_metadata[
  ,
  starts_2000_or_later := !is.na(START_YEAR) & START_YEAR >= minimum_year
]
biotime_metadata[
  ,
  eligible := terrestrial_realm & abundance_based & starts_2000_or_later
]
biotime_metadata[
  ,
  exclusion_reason := vapply(
    seq_len(.N),
    function(row_index) {
      reasons <- c(
        if (!terrestrial_realm[[row_index]]) "not_terrestrial",
        if (!abundance_based[[row_index]]) "not_abundance_based",
        if (!starts_2000_or_later[[row_index]]) "starts_before_2000"
      )
      paste(reasons, collapse = ";")
    },
    character(1)
  )
]

biotime_studies <- merge(
  biotime_metadata,
  biotime_reference_summary,
  by = "STUDY_ID",
  all.x = TRUE,
  sort = FALSE
)

study_output_cols <- intersect(
  c(
    "STUDY_ID", "TITLE", "START_YEAR", "END_YEAR", "AB_BIO", "ABUNDANCE_TYPE",
    "CEN_LATITUDE", "CEN_LONGITUDE", "CENT_LAT", "CENT_LONG", "PERMISSIONS",
    "WEB_LINK", "eligible", "exclusion_reason", "CITATION_ID", "DOI", "BIB"
  ),
  names(biotime_studies)
)

biotime_eligible_studies <- biotime_studies[eligible == TRUE, ..study_output_cols]
biotime_excluded_studies <- biotime_studies[eligible == FALSE, ..study_output_cols]

write_csv_safe(biotime_eligible_studies, biotime_eligible_studies_path)
write_csv_safe(biotime_excluded_studies, biotime_excluded_studies_path)

observation_cols <- c(
  "ID_ALL_RAW_DATA", "ABUNDANCE", "ID_SPECIES", "SAMPLE_DESC",
  "LATITUDE", "LONGITUDE", "DAY", "MONTH", "YEAR", "STUDY_ID", "valid_name",
  "resolution", "taxon"
)
biotime_observations <- data.table::fread(
  biotime_data_path,
  select = observation_cols,
  showProgress = FALSE
)
assert_has_cols(biotime_observations, observation_cols, "BioTIME observations")

eligible_study_ids <- biotime_eligible_studies$STUDY_ID
biotime_observations <- biotime_observations[
  STUDY_ID %in% eligible_study_ids &
    !is.na(YEAR) &
    YEAR >= minimum_year &
    !is.na(ABUNDANCE)
]
biotime_observations[
  ,
  sample_event_id := paste(
    STUDY_ID, SAMPLE_DESC, LATITUDE, LONGITUDE, YEAR, MONTH, DAY,
    sep = "__"
  )
]

saveRDS(
  as.data.frame(biotime_observations),
  biotime_filtered_observations_path,
  compress = "gzip"
)
message("Wrote: ", biotime_filtered_observations_path)

biotime_study_summary <- biotime_observations[
  ,
  .(
    n_observation_rows = .N,
    n_sample_events = uniqueN(sample_event_id),
    n_taxa = uniqueN(valid_name),
    observation_start_year = min(YEAR, na.rm = TRUE),
    observation_end_year = max(YEAR, na.rm = TRUE),
    observation_centroid_latitude = mean(LATITUDE, na.rm = TRUE),
    observation_centroid_longitude = mean(LONGITUDE, na.rm = TRUE)
  ),
  by = STUDY_ID
]
biotime_study_summary <- merge(
  biotime_eligible_studies,
  biotime_study_summary,
  by = "STUDY_ID",
  all.x = TRUE,
  sort = FALSE
)

write_csv_safe(biotime_study_summary, biotime_study_summary_path)

biotime_filter_summary <- data.table::data.table(
  metric = c(
    "metadata_studies_total",
    "metadata_studies_eligible",
    "metadata_studies_excluded",
    "study_start_2000_observation_rows",
    "study_start_2000_studies_with_observations",
    "study_start_2000_sample_events",
    "study_start_2000_taxa"
  ),
  value = c(
    nrow(biotime_metadata),
    nrow(biotime_eligible_studies),
    nrow(biotime_excluded_studies),
    nrow(biotime_observations),
    uniqueN(biotime_observations$STUDY_ID),
    uniqueN(biotime_observations$sample_event_id),
    uniqueN(biotime_observations$valid_name)
  )
)

write_csv_safe(biotime_filter_summary, biotime_filter_summary_path)
