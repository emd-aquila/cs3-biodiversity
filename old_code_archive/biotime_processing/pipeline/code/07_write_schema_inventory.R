# =====================================================
# Record original and retained BioTIME/PREDICTS fields.
# =====================================================

assert_file_exists(predicts_full_rds_path, "cached full PREDICTS RDS")
assert_file_exists(predicts_filtered_observations_path, "filtered slim PREDICTS RDS")
assert_file_exists(biotime_data_path, "BioTIME observation CSV")
assert_file_exists(biotime_metadata_path, "BioTIME metadata CSV")
assert_file_exists(biotime_references_path, "BioTIME reference CSV")
assert_file_exists(biotime_filtered_observations_path, "filtered slim BioTIME RDS")
assert_file_exists(biotime_eligible_studies_path, "filtered slim BioTIME study CSV")

predicts_original_fields <- names(readRDS(predicts_full_rds_path))
predicts_retained_fields <- names(readRDS(predicts_filtered_observations_path))
biotime_original_fields <- names(
  data.table::fread(biotime_data_path, nrows = 0, showProgress = FALSE)
)
biotime_metadata_fields <- names(
  data.table::fread(biotime_metadata_path, nrows = 0, showProgress = FALSE)
)
biotime_reference_fields <- names(
  data.table::fread(biotime_references_path, nrows = 0, showProgress = FALSE)
)
biotime_retained_fields <- names(readRDS(biotime_filtered_observations_path))
biotime_retained_study_fields <- names(
  data.table::fread(biotime_eligible_studies_path, nrows = 0, showProgress = FALSE)
)

predicts_field_groups <- list(
  provenance = c("Source_ID", "Reference"),
  study = c(
    "Study_number", "Study_name", "SS", "Diversity_metric",
    "Diversity_metric_unit", "Diversity_metric_type",
    "Diversity_metric_is_effort_sensitive", "Diversity_metric_is_suitable_for_Chao",
    "Sampling_method", "Sampling_effort_unit", "Study_common_taxon",
    "Rank_of_study_common_taxon"
  ),
  site = c(
    "Site_number", "Site_name", "Block", "SSS", "SSB", "SSBS",
    "Sample_start_earliest", "Sample_end_latest", "Sample_midpoint",
    "Sample_date_resolution", "Max_linear_extent_metres",
    "Habitat_patch_area_square_metres", "Sampling_effort",
    "Rescaled_sampling_effort", "Habitat_as_described",
    "Predominant_land_use", "Source_for_predominant_land_use", "Use_intensity",
    "Km_to_nearest_edge_of_habitat", "Years_since_fragmentation_or_conversion",
    "Transect_details", "Coordinates_method", "Longitude", "Latitude",
    "Country_distance_metres", "Country", "UN_subregion", "UN_region",
    "Ecoregion_distance_metres", "Ecoregion", "Biome", "Realm", "Hotspot",
    "Wilderness_area"
  ),
  taxon = c(
    "Taxon_number", "Taxon_name_entered", "Indication", "Parsed_name", "Taxon",
    "COL_ID", "Name_status", "Rank", "Kingdom", "Phylum", "Class", "Order",
    "Family", "Genus", "Species", "Best_guess_binomial", "Higher_taxon"
  ),
  measurement = c("Measurement", "Effort_corrected_measurement")
)

# `predictsr::GetColumnDescriptions()` documents two site-summary fields that
# are available from `GetSitelevelSummaries()` but are not columns in the full
# local observation extract loaded by `LoadPredictsData()`.
predicts_site_summary_extra_fields <- c("N_samples", "Higher_taxa")

biotime_observation_field_groups <- list(
  observation_id = c("ID_ALL_RAW_DATA"),
  measurement = c("ABUNDANCE", "BIOMASS"),
  taxon = c("ID_SPECIES", "newID", "valid_name", "resolution", "taxon"),
  sample_event = c("SAMPLE_DESC", "DEPTH", "DAY", "MONTH", "YEAR"),
  location = c("LATITUDE", "LONGITUDE"),
  provenance = c("STUDY_ID")
)

biotime_metadata_field_groups <- list(
  provenance = c(
    "STUDY_ID", "TITLE", "CONTACT_1", "CONTACT_2", "CONT_1_MAIL", "CONT_2_MAIL",
    "PERMISSIONS", "WEB_LINK", "DATA_SOURCE", "DATE_TO_DB", "LINK_ID", "COMMENTS",
    "DATES_CHANGED", "CURATOR", "LOC_ADDED", "DATE_STUDY_ADDED"
  ),
  location_environment = c(
    "REALM", "CLIMATE", "CEN_LATITUDE", "CEN_LONGITUDE", "HABITAT",
    "PROTECTED_AREA", "AREA", "BIOME_MAP", "CENT_LAT", "CENT_LONG", "AREA_SQ_KM"
  ),
  study_design = c(
    "GENERAL_TREAT", "TREATMENT", "TREAT_COMMENTS", "TREAT_DATE", "DATA_POINTS",
    "NUMBER_OF_SPECIES", "NUMBER_OF_SAMPLES", "NUMBER_LAT_LONG", "TOTAL",
    "GRAIN_SIZE_TEXT", "METHODS", "SUMMARY_METHODS", "SAMPLE_DESC_NAME"
  ),
  biodiversity = c("TAXA", "ORGANISMS", "AB_BIO", "ABUNDANCE_TYPE", "BIOMASS_TYPE"),
  time = c("START_YEAR", "END_YEAR")
)

lookup_field_group <- function(field, groups) {
  matches <- names(groups)[vapply(groups, function(values) field %in% values, logical(1))]
  if (length(matches) == 0) return("other")
  matches[[1]]
}

build_inventory <- function(source_table, fields, groups, retained_fields = character()) {
  data.table::data.table(
    source_table = source_table,
    field_order = seq_along(fields),
    field = fields,
    field_group = vapply(fields, lookup_field_group, character(1), groups = groups),
    retained_in_slim_pipeline = fields %in% retained_fields
  )
}

inventory <- data.table::rbindlist(
  list(
    build_inventory(
      "PREDICTS full observation extract",
      predicts_original_fields,
      predicts_field_groups,
      predicts_retained_fields
    ),
    build_inventory(
      "PREDICTS documented site-summary extras",
      predicts_site_summary_extra_fields,
      list(site = predicts_site_summary_extra_fields)
    ),
    build_inventory(
      "BioTIME observation query",
      biotime_original_fields,
      biotime_observation_field_groups,
      biotime_retained_fields
    ),
    build_inventory(
      "BioTIME study metadata",
      biotime_metadata_fields,
      biotime_metadata_field_groups,
      biotime_retained_study_fields
    ),
    build_inventory(
      "BioTIME study references",
      biotime_reference_fields,
      list(provenance = biotime_reference_fields),
      biotime_retained_study_fields
    )
  ),
  use.names = TRUE
)

write_csv_safe(inventory, schema_inventory_csv_path)

format_inventory_section <- function(title, source_name, explanation) {
  section <- inventory[source_table == source_name]
  group_lines <- unlist(
    lapply(
      unique(section$field_group),
      function(group_name) {
        fields <- section[field_group == group_name]
        c(
          paste0("### ", group_name),
          "",
          paste0(
            "- `",
            fields$field,
            "`",
            ifelse(fields$retained_in_slim_pipeline, " (retained)", "")
          ),
          ""
        )
      }
    ),
    use.names = FALSE
  )

  c(
    paste0("## ", title),
    "",
    explanation,
    "",
    paste0("Original field count: ", nrow(section), "."),
    "",
    group_lines
  )
}

schema_lines <- c(
  "# Original BioTIME and PREDICTS Field Inventory",
  "",
  "This inventory is generated from the actual local source files. Fields marked",
  "`(retained)` are kept in the slim pipeline outputs because they support OV",
  "calculation, site/date geotagging, measurement interpretation, or provenance.",
  "",
  "PREDICTS is already terrestrial. Its `Realm` field is documented below because",
  "it exists in the original extract, but it is not used as an eligibility filter",
  "and is not retained in the slim output.",
  "",
  format_inventory_section(
    "PREDICTS full observation extract",
    "PREDICTS full observation extract",
    "The PREDICTS extract stores source, study, site, taxon, and measurement fields together."
  ),
  format_inventory_section(
    "PREDICTS documented site-summary extras",
    "PREDICTS documented site-summary extras",
    "`predictsr::GetColumnDescriptions()` documents these additional fields for `GetSitelevelSummaries()`. They are not columns in the full local observation extract."
  ),
  format_inventory_section(
    "BioTIME observation query",
    "BioTIME observation query",
    "The BioTIME query contains sample-event observations joined to taxon names."
  ),
  format_inventory_section(
    "BioTIME study metadata",
    "BioTIME study metadata",
    "BioTIME provides a separate study-level metadata table."
  ),
  format_inventory_section(
    "BioTIME study references",
    "BioTIME study references",
    "BioTIME provides a separate citation table keyed by study and citation IDs."
  ),
  "## Machine-readable version",
  "",
  "`schema_inventory.csv` records the source table, original field order, field",
  "group, and whether each field is retained by the slim pipeline."
)

writeLines(schema_lines, schema_inventory_md_path)
message("Wrote: ", schema_inventory_md_path)
