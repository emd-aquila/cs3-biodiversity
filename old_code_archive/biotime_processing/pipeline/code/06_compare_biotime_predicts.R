# =====================================================
# Screen BioTIME and PREDICTS for study-level redundancy.
# =====================================================

assert_file_exists(predicts_filtered_observations_path, "filtered slim PREDICTS RDS")
assert_file_exists(biotime_study_summary_path, "filtered BioTIME study summary")
assert_file_exists(biotime_references_path, "BioTIME reference CSV")

biotime_study_summary <- data.table::fread(biotime_study_summary_path, showProgress = FALSE)
biotime_references <- data.table::fread(biotime_references_path, showProgress = FALSE)
predicts_eligible <- data.table::as.data.table(readRDS(predicts_filtered_observations_path))

assert_has_cols(
  predicts_eligible,
  c(
    "Source_ID", "Reference", "Study_number", "Study_name", "SSBS",
    "Sample_midpoint", "Latitude", "Longitude",
    "Diversity_metric_type", "Taxon"
  ),
  "filtered slim PREDICTS extract"
)

predicts_eligible[, Sample_midpoint := as.Date(Sample_midpoint)]
predicts_eligible[, observation_year := as.integer(format(Sample_midpoint, "%Y"))]

predicts_study_summary <- predicts_eligible[
  ,
  .(
    n_observation_rows = .N,
    n_sites = uniqueN(SSBS),
    n_taxa = uniqueN(Taxon),
    observation_start_year = min(observation_year, na.rm = TRUE),
    observation_end_year = max(observation_year, na.rm = TRUE),
    observation_centroid_latitude = mean(Latitude, na.rm = TRUE),
    observation_centroid_longitude = mean(Longitude, na.rm = TRUE)
  ),
  by = .(Source_ID, Reference, Study_number, Study_name)
]
write_csv_safe(predicts_study_summary, predicts_study_summary_path)

# Exact citation screening: compare first-author surname and publication year.
biotime_reference_index <- unique(
  biotime_references[
    ,
    .(
      STUDY_ID,
      CITATION_ID,
      DOI = extract_dois(BIB),
      biotime_title = extract_bib_field(BIB, "Title"),
      biotime_author_year_key = build_biotime_author_year_key(BIB)
    )
  ]
)
predicts_reference_index <- unique(
  predicts_study_summary[
    ,
    .(
      Source_ID,
      Reference,
      predicts_author_year_key = build_predicts_author_year_key(Reference)
    )
  ]
)

reference_overlap <- merge(
  biotime_reference_index[!is.na(biotime_author_year_key)],
  predicts_reference_index[!is.na(predicts_author_year_key)],
  by.x = "biotime_author_year_key",
  by.y = "predicts_author_year_key",
  allow.cartesian = TRUE
)
setnames(reference_overlap, "biotime_author_year_key", "author_year_key")
setorder(reference_overlap, author_year_key, STUDY_ID, Source_ID)
write_csv_safe(reference_overlap, reference_overlap_path)

# Spatiotemporal screening: nearby centroids with overlapping observation years.
biotime_spatial <- biotime_study_summary[
  !is.na(observation_centroid_latitude) &
    !is.na(observation_centroid_longitude) &
    !is.na(observation_start_year) &
    !is.na(observation_end_year),
  .(
    STUDY_ID,
    biotime_title = TITLE,
    biotime_start_year = observation_start_year,
    biotime_end_year = observation_end_year,
    biotime_latitude = observation_centroid_latitude,
    biotime_longitude = observation_centroid_longitude
  )
]
predicts_spatial <- predicts_study_summary[
  !is.na(observation_centroid_latitude) &
    !is.na(observation_centroid_longitude) &
    !is.na(observation_start_year) &
    !is.na(observation_end_year),
  .(
    Source_ID,
    Study_number,
    Reference,
    predicts_start_year = observation_start_year,
    predicts_end_year = observation_end_year,
    predicts_latitude = observation_centroid_latitude,
    predicts_longitude = observation_centroid_longitude
  )
]

biotime_spatial[, join_key := 1L]
predicts_spatial[, join_key := 1L]
spatiotemporal_overlap <- merge(
  biotime_spatial,
  predicts_spatial,
  by = "join_key",
  allow.cartesian = TRUE
)[
  biotime_start_year <= predicts_end_year &
    predicts_start_year <= biotime_end_year
]
spatiotemporal_overlap[
  ,
  centroid_distance_km := haversine_km(
    biotime_latitude,
    biotime_longitude,
    predicts_latitude,
    predicts_longitude
  )
]
spatiotemporal_overlap <- spatiotemporal_overlap[
  is.finite(centroid_distance_km) &
    centroid_distance_km <= possible_spatial_match_km
]

reference_pair_keys <- unique(
  reference_overlap[
    ,
    paste(STUDY_ID, Source_ID, sep = "__")
  ]
)
spatiotemporal_overlap[
  ,
  exact_reference_match := paste(STUDY_ID, Source_ID, sep = "__") %in% reference_pair_keys
]
spatiotemporal_overlap[
  ,
  candidate_strength := data.table::fcase(
    exact_reference_match, "exact_reference_and_spatiotemporal",
    centroid_distance_km <= strong_spatial_match_km, "spatiotemporal_within_1km",
    default = "spatiotemporal_within_10km"
  )
]
spatiotemporal_overlap[, join_key := NULL]
setorder(spatiotemporal_overlap, centroid_distance_km, STUDY_ID, Source_ID)
write_csv_safe(spatiotemporal_overlap, spatiotemporal_overlap_path)

reference_matched_biotime_ids <- unique(reference_overlap$STUDY_ID)
spatial_matched_biotime_ids <- unique(spatiotemporal_overlap$STUDY_ID)
possibly_redundant_biotime_ids <- union(reference_matched_biotime_ids, spatial_matched_biotime_ids)
biotime_studies_with_observations <- unique(
  biotime_study_summary[!is.na(n_observation_rows)]$STUDY_ID
)
incremental_biotime_ids <- setdiff(biotime_studies_with_observations, possibly_redundant_biotime_ids)

comparison_summary <- data.table::data.table(
  metric = c(
    "biotime_terrestrial_abundance_studies_starting_2000_or_later",
    "biotime_terrestrial_abundance_observation_rows_study_start_2000",
    "predicts_abundance_studies_starting_2000_or_later",
    "predicts_abundance_observation_rows_study_start_2000",
    "predicts_abundance_sites_study_start_2000",
    "exact_reference_candidate_pairs",
    "exact_reference_matched_biotime_studies",
    "spatiotemporal_candidate_pairs_within_1km",
    "spatiotemporal_candidate_pairs_within_10km",
    "possibly_redundant_biotime_studies",
    "potentially_incremental_biotime_studies",
    "potentially_incremental_biotime_share"
  ),
  value = c(
    length(biotime_studies_with_observations),
    sum(biotime_study_summary$n_observation_rows, na.rm = TRUE),
    nrow(predicts_study_summary),
    nrow(predicts_eligible),
    uniqueN(predicts_eligible$SSBS),
    nrow(reference_overlap),
    length(reference_matched_biotime_ids),
    nrow(spatiotemporal_overlap[centroid_distance_km <= strong_spatial_match_km]),
    nrow(spatiotemporal_overlap),
    length(possibly_redundant_biotime_ids),
    length(incremental_biotime_ids),
    length(incremental_biotime_ids) / length(biotime_studies_with_observations)
  )
)
write_csv_safe(comparison_summary, comparison_summary_path)

summary_value <- function(metric) {
  comparison_summary$value[comparison_summary$metric == metric][[1]]
}

incremental_share <- summary_value("potentially_incremental_biotime_share")
screening_assessment <- if (
  summary_value("potentially_incremental_biotime_studies") >= 50 &&
    incremental_share >= 0.20
) {
  "The screening results support a harmonization pilot: BioTIME appears likely to add a meaningful number of studies after conservative redundancy flags are removed."
} else {
  "The screening results suggest a targeted pilot before a full merge: manually review overlap candidates and measure the incremental gain after harmonization."
}

report_lines <- c(
  "# BioTIME and PREDICTS Redundancy Screening",
  "",
  "## Scope",
  "",
  paste0(
    "Both databases were filtered to abundance-based studies starting in ",
    minimum_year,
    " or later. BioTIME was additionally filtered to terrestrial studies. PREDICTS is ",
    "already a terrestrial database, so its biogeographic `Realm` field is neither used ",
    "as a filter nor retained. Both cleaned extracts retain only OV, geotagging, date, ",
    "measurement, and provenance fields."
  ),
  "",
  "## Data Volume",
  "",
  paste0(
    "- BioTIME: ",
    format_count(summary_value("biotime_terrestrial_abundance_studies_starting_2000_or_later")),
    " studies and ",
    format_count(summary_value("biotime_terrestrial_abundance_observation_rows_study_start_2000")),
    " observation rows."
  ),
  paste0(
    "- PREDICTS: ",
    format_count(summary_value("predicts_abundance_studies_starting_2000_or_later")),
    " studies, ",
    format_count(summary_value("predicts_abundance_sites_study_start_2000")),
    " sites, and ",
    format_count(summary_value("predicts_abundance_observation_rows_study_start_2000")),
    " observation rows."
  ),
  "",
  "## Redundancy Signals",
  "",
  paste0(
    "- Exact first-author/year citation candidates: ",
    format_count(summary_value("exact_reference_candidate_pairs")),
    " pairs affecting ",
    format_count(summary_value("exact_reference_matched_biotime_studies")),
    " BioTIME studies."
  ),
  paste0(
    "- Spatiotemporal candidates within 1 km: ",
    format_count(summary_value("spatiotemporal_candidate_pairs_within_1km")),
    " study pairs."
  ),
  paste0(
    "- Spatiotemporal candidates within 10 km: ",
    format_count(summary_value("spatiotemporal_candidate_pairs_within_10km")),
    " study pairs."
  ),
  paste0(
    "- BioTIME studies not flagged by either screen: ",
    format_count(summary_value("potentially_incremental_biotime_studies")),
    " (",
    sprintf("%.1f%%", 100 * incremental_share),
    ")."
  ),
  "",
  "## Interpretation",
  "",
  screening_assessment,
  "",
  "The overlap screens are deliberately conservative candidate generators. Matching ",
  "first-author surname and publication year can produce false positives. Nearby ",
  "centroids and overlapping years can indicate shared sites, but they can also indicate ",
  "independent studies in intensively sampled landscapes. Manually review the candidate ",
  "tables before deduplicating any observations.",
  "",
  "## Recommended Merge Pilot",
  "",
  "1. Manually classify exact-reference candidates first.",
  "2. Review <=1 km spatiotemporal candidates next, using titles, methods, taxa, and sample design.",
  "3. Harmonize a small non-overlapping BioTIME subset to the PREDICTS site-date-taxon schema.",
  "4. Recalculate the biodiversity metric on PREDICTS alone and on the pilot union.",
  "5. Compare added sites, temporal coverage, geographic coverage, and model sensitivity before scaling up.",
  "",
  "Do not directly row-bind the raw tables. BioTIME is a longitudinal assemblage database, ",
  "while PREDICTS is organized around spatial comparisons of land use and intensity. ",
  "Sampling effort, taxonomic resolution, site identity, and repeated-measure structure ",
  "must be harmonized explicitly."
)

writeLines(sub("[[:space:]]+$", "", report_lines), comparison_report_path)
message("Wrote: ", comparison_report_path)
