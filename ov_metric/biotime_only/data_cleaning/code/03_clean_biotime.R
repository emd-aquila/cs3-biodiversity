# =====================================================
# Clean raw BioTIME and compute original composite OV
# =====================================================

assert_file_exists(raw_biotime_data_rds, "Raw BioTIME observation RDS")
assert_file_exists(raw_biotime_metadata_rds, "Raw BioTIME metadata RDS")
assert_file_exists(raw_biotime_references_rds, "Raw BioTIME references RDS")
assert_file_exists(aez_path, "AEZ shapefile")
assert_file_exists(integration_helper_path, "BioTIME integration helper")
assert_file_exists(taxon_path, "GBIF taxon lookup")
assert_file_exists(bird_codes_path, "curated bird code lookup")
assert_file_exists(plant_codes_path, "curated plant code lookup")
assert_file_exists(phylo_tree_path, "family phylogenetic tree")

build_plant_lookup_if_missing()

message("Reading raw BioTIME metadata and references")
metadata <- data.table::as.data.table(readRDS(raw_biotime_metadata_rds))
references <- data.table::as.data.table(readRDS(raw_biotime_references_rds))
metadata <- ascii_normalize_table(metadata)
references <- ascii_normalize_table(references)

assert_has_cols(
  metadata,
  c(
    "STUDY_ID", "REALM", "AB_BIO", "TAXA", "ORGANISMS",
    "START_YEAR", "END_YEAR"
  ),
  "BioTIME metadata"
)
assert_has_cols(references, c("STUDY_ID", "CITATION_ID", "BIB"), "BioTIME references")

message("Parsing BioTIME reference fields")
references[, DOI := extract_bib_field(BIB, "DOI")]
references[, reference_title := extract_bib_field(BIB, "Title")]

message("Flagging eligible BioTIME studies")
metadata[, `:=`(
  terrestrial_realm = !is.na(REALM) & REALM == "Terrestrial",
  abundance_based = !is.na(AB_BIO) & grepl("A", toupper(AB_BIO), fixed = TRUE),
  taxon_group = clean_taxon_group(TAXA, ORGANISMS)
)]
metadata[, eligible := terrestrial_realm & abundance_based]
eligible_study_ids <- unique(metadata[eligible == TRUE, STUDY_ID])

metadata_keep <- metadata[
  eligible == TRUE,
  .(
    study_id = as.character(STUDY_ID),
    STUDY_ID,
    TITLE,
    REALM,
    TAXA,
    ORGANISMS,
    AB_BIO,
    ABUNDANCE_TYPE,
    START_YEAR,
    END_YEAR,
    taxon_group,
    CLIMATE = if ("CLIMATE" %in% names(metadata)) CLIMATE else NA_character_,
    HABITAT = if ("HABITAT" %in% names(metadata)) HABITAT else NA_character_,
    AREA_SQ_KM = if ("AREA_SQ_KM" %in% names(metadata)) AREA_SQ_KM else NA_real_
  )
]

observation_cols <- c(
  "ID_ALL_RAW_DATA", "ABUNDANCE", "BIOMASS", "ID_SPECIES", "SAMPLE_DESC",
  "LATITUDE", "LONGITUDE", "DEPTH", "DAY", "MONTH", "YEAR", "STUDY_ID",
  "newID", "valid_name", "resolution", "taxon"
)

check_biotimer_schema(
  observation_cols,
  c("STUDY_ID", "REALM", "AB_BIO", "START_YEAR", "END_YEAR")
)

message("Reading raw BioTIME observations")
observations <- load_biotime_observations(raw_biotime_data_rds, observation_cols)
observations <- ascii_normalize_table(observations)
raw_observation_rows <- nrow(observations)
observations <- unique(observations, by = "ID_ALL_RAW_DATA")
deduplicated_observation_rows <- nrow(observations)

message("Filtering raw BioTIME observations")
observations <- observations[STUDY_ID %in% eligible_study_ids]
rows_after_metadata_filter <- nrow(observations)

observations[, `:=`(
  abundance_num = suppressWarnings(as.numeric(ABUNDANCE)),
  latitude_num = suppressWarnings(as.numeric(LATITUDE)),
  longitude_num = suppressWarnings(as.numeric(LONGITUDE)),
  sample_year_num = suppressWarnings(as.integer(YEAR))
)]

observations <- observations[
  !is.na(sample_year_num) &
    sample_year_num >= min_biotime_year &
    sample_year_num <= max_hansen_year
]
rows_after_year_filter <- nrow(observations)

observations <- observations[
  is.finite(latitude_num) &
    is.finite(longitude_num) &
    latitude_num >= -90 &
    latitude_num <= 90 &
    longitude_num >= -180 &
    longitude_num <= 180
]
rows_after_coordinate_filter <- nrow(observations)

observations <- observations[is.finite(abundance_num)]
rows_after_abundance_filter <- nrow(observations)

observations[, `:=`(
  ABUNDANCE = abundance_num,
  LATITUDE = latitude_num,
  LONGITUDE = longitude_num,
  YEAR = sample_year_num
)]
observations[, c("abundance_num", "latitude_num", "longitude_num", "sample_year_num") := NULL]

invalid_terms <- c(
  "morphospecies", "unknown", "undetermined", "unidentif",
  "sp\\.", "spp\\.", "sp$", "cf\\.", "indet\\.", "sp[0-9a-zA-Z]+"
)
invalid_pattern <- paste(invalid_terms, collapse = "|")
observations <- observations[
  !is.na(valid_name) &
    nzchar(valid_name) &
    !grepl(invalid_pattern, valid_name, ignore.case = TRUE) &
    (
      is.na(resolution) |
        !(tolower(resolution) %in% c("phylum", "subphylum", "superclass", "kingdom"))
    )
]
rows_after_taxon_name_filter <- nrow(observations)

observations[
  ,
  sample_event_id := paste(
    STUDY_ID, SAMPLE_DESC, LATITUDE, LONGITUDE, YEAR, MONTH, DAY,
    sep = "__"
  )
]

message("Resolving BioTIME taxonomy from lookup tables")
observations <- resolve_biotime_taxonomy(observations, cache_path = biotime_taxonomy_cache_rds_path)
rows_with_family <- nrow(observations[!is.na(Family) & nzchar(as.character(Family))])
rows_after_taxonomy_resolution <- nrow(observations)
saveRDS(as.data.frame(observations), biotime_filtered_unstandardized_rds_path, compress = "gzip")
message("Wrote: ", biotime_filtered_unstandardized_rds_path)

taxonomy_resolution <- unique(
  observations[, c("valid_name_original", "resolved_name", "resolution_method", taxonomy_cols()), with = FALSE]
)
write_csv_safe(taxonomy_resolution, biotime_taxonomy_resolution_path)

message("Applying BioTIMEr gridding and deterministic sample-event rarefaction")
biotime_standardized <- standardize_biotime_sampling(
  biotime_filtered = observations,
  biotime_metadata = metadata[eligible == TRUE],
  biotime_reference_table = references,
  grid_resolution = biotime_grid_resolution,
  rarefaction_seed = biotime_rarefaction_seed
)
biotime_standardized <- data.table::as.data.table(biotime_standardized)
write_csv_safe(biotime_standardized, standardized_taxon_rows_path)

message("Computing phylogenetic diversity")
phylo_tree <- ape::read.tree(phylo_tree_path)
pd_result <- calculate_pd_for_biotime(biotime_standardized, phylo_tree)

message("Computing original composite OV components")
component_scores <- calculate_composite_ov_scores(biotime_standardized, pd_result)
component_scores <- data.table::as.data.table(component_scores)
component_scores[
  ,
  `:=`(
    time_series_id = as.character(assemblage_id),
    assemblageID = sub("^BioTIME:", "", as.character(assemblage_id))
  )
]
component_scores <- merge(component_scores, metadata_keep, by = "study_id", all.x = TRUE, sort = FALSE)

component_scores <- component_scores[
  !is.na(time_series_id) &
    !is.na(sample_year) &
    is.finite(ov_score)
]

pre_timeseries_n <- data.table::uniqueN(component_scores$time_series_id)
sample_year_rows_before_two_year_filter <- nrow(component_scores)
timeseries_year_counts <- component_scores[
  ,
  .(
    n_years_post_2000 = data.table::uniqueN(sample_year),
    first_year = min(sample_year, na.rm = TRUE),
    last_year = max(sample_year, na.rm = TRUE)
  ),
  by = time_series_id
]
retained_timeseries_ids <- timeseries_year_counts[
  n_years_post_2000 >= min_years_per_time_series,
  time_series_id
]
sample_year <- component_scores[time_series_id %in% retained_timeseries_ids]

message("Assigning AEZ labels to BioTIMEr assemblages")
timeseries_points <- sample_year[
  ,
  .(
    assemblage_id = dplyr::first(assemblage_id),
    assemblageID = dplyr::first(assemblageID),
    site_id = dplyr::first(site_id),
    study_id = collapse_unique(study_id),
    latitude = mean(as.numeric(latitude), na.rm = TRUE),
    longitude = mean(as.numeric(longitude), na.rm = TRUE),
    taxon_group = collapse_unique(taxon_group),
    taxa_raw = collapse_unique(TAXA),
    organisms_raw = collapse_unique(ORGANISMS),
    title = collapse_unique(TITLE),
    first_year = min(sample_year, na.rm = TRUE),
    last_year = max(sample_year, na.rm = TRUE),
    n_years_post_2000 = data.table::uniqueN(sample_year),
    n_sample_year_rows = .N
  ),
  by = time_series_id
]

timeseries_tagged <- assign_aez_to_points(
  points_df = as.data.frame(timeseries_points),
  aez_path = aez_path,
  analysis_crs = analysis_crs
)

sample_year <- merge(
  sample_year,
  data.table::as.data.table(
    timeseries_tagged[, c("time_series_id", "AEZ", "AEZ_id", "AEZ_assigned_by_nearest")]
  ),
  by = "time_series_id",
  all.x = TRUE,
  sort = FALSE
)
sample_year <- sample_year[order(time_series_id, sample_year)]

sample_year_out <- sample_year |>
  as.data.frame() |>
  dplyr::transmute(
    time_series_id,
    assemblage_id,
    assemblageID,
    site_id,
    sample_id,
    study_id,
    sample_year,
    sample_start_date,
    sample_end_date,
    sample_midpoint,
    latitude,
    longitude,
    AEZ,
    AEZ_id,
    AEZ_assigned_by_nearest,
    taxon_group,
    taxa_raw = TAXA,
    organisms_raw = ORGANISMS,
    ov = ov_score,
    ov_score,
    ov_obs_only,
    shannon,
    phylo_div,
    SR,
    msa,
    log_msa,
    shannon_scaled,
    log_msa_scaled,
    phylo_scaled,
    effective_species,
    richness,
    total_abundance,
    site_total_abundance,
    site_total_taxa,
    n_observation_rows,
    n_standardized_taxon_rows,
    n_sample_desc = sample_events_selected,
    sample_events_selected,
    raw_sample_events_available,
    title = TITLE,
    climate = CLIMATE,
    habitat = HABITAT,
    abundance_type = ABUNDANCE_TYPE,
    start_year = START_YEAR,
    end_year = END_YEAR,
    sampling_effort,
    sampling_effort_unit,
    biotime_grid_resolution,
    rarefaction_seed
  )

timeseries_out <- timeseries_tagged |>
  dplyr::arrange(AEZ, taxon_group, time_series_id)

selected_events <- if (file.exists(biotime_rarefied_events_rds_path)) {
  data.table::as.data.table(readRDS(biotime_rarefied_events_rds_path))
} else {
  data.table::data.table(sample_event_id = character())
}

taxonomy_method_counts <- observations[
  ,
  .N,
  by = .(resolution_method = ifelse(is.na(resolution_method), "unresolved", resolution_method))
][order(-N)]

taxonomy_cache_rows <- if (file.exists(biotime_taxonomy_cache_rds_path)) {
  nrow(readRDS(biotime_taxonomy_cache_rds_path))
} else {
  0L
}

cleaning_summary <- data.frame(
  metric = c(
    "raw_metadata_studies_total",
    "eligible_terrestrial_abundance_studies",
    "raw_observation_rows_total",
    "raw_observation_rows_after_id_deduplication",
    "rows_after_terrestrial_abundance_metadata",
    "rows_after_year_2000_2025",
    "rows_after_valid_coordinates",
    "rows_after_nonmissing_abundance",
    "rows_after_taxon_name_resolution_filter",
    "rows_after_taxonomy_resolution",
    "rows_with_resolved_family",
    "standardized_taxon_rows",
    "standardized_assemblage_year_samples",
    "rarefied_sample_events_selected",
    "composite_ov_rows_before_two_year_filter",
    "time_series_before_two_year_filter",
    "time_series_retained",
    "time_series_removed_lt_two_composite_ov_years",
    "sample_year_rows_retained",
    "aez_nearest_assignments",
    "taxonomy_cache_rows",
    "taxonomy_resolution_methods"
  ),
  value = c(
    nrow(metadata),
    length(eligible_study_ids),
    raw_observation_rows,
    deduplicated_observation_rows,
    rows_after_metadata_filter,
    rows_after_year_filter,
    rows_after_coordinate_filter,
    rows_after_abundance_filter,
    rows_after_taxon_name_filter,
    rows_after_taxonomy_resolution,
    rows_with_family,
    nrow(biotime_standardized),
    data.table::uniqueN(biotime_standardized$sample_id),
    data.table::uniqueN(selected_events$sample_event_id),
    sample_year_rows_before_two_year_filter,
    pre_timeseries_n,
    nrow(timeseries_out),
    pre_timeseries_n - nrow(timeseries_out),
    nrow(sample_year_out),
    sum(timeseries_out$AEZ_assigned_by_nearest, na.rm = TRUE),
    taxonomy_cache_rows,
    paste(taxonomy_method_counts[, paste0(resolution_method, "=", N)], collapse = ";")
  )
)

taxon_summary <- sample_year_out |>
  dplyr::distinct(time_series_id, taxon_group, AEZ) |>
  dplyr::count(taxon_group, AEZ, name = "n_time_series") |>
  dplyr::arrange(taxon_group, AEZ)

write_csv_safe(sample_year_out, clean_sample_year_path)
write_csv_safe(timeseries_out, clean_timeseries_path)
write_csv_safe(cleaning_summary, cleaning_summary_path)
write_csv_safe(taxon_summary, taxon_summary_path)
write_csv_safe(sample_year_out, file.path(processed_data_dir, "biotime_clean_sample_year.csv"))
write_csv_safe(timeseries_out, file.path(processed_data_dir, "biotime_timeseries.csv"))

message("BioTIME cleaning and composite OV calculation complete.")
