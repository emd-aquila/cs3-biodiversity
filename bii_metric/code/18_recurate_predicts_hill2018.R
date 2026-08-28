# Re-curate retained PREDICTS site metadata to the LUH2-compatible categories
# used by Hill et al. (2018). This stage never changes the raw biodiversity
# data. Instead it produces a site-level mapping, an auditable rule trail, and
# a review queue for cases where the public extract does not contain enough
# evidence to reproduce the original manual curation.

if (!requireNamespace("terra", quietly = TRUE)) {
  stop("Hill 2018 re-curation requires the installed R package 'terra'.", call. = FALSE)
}
assert_file_exists(predicts_raw_path, "PREDICTS raw database")
assert_file_exists(luh2_static_path, "LUH2 static file")
assert_file_exists(hill2018_rules_path, "Hill 2018 re-curation rule table")

normalise_text <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x) | x %in% c("", "na", "n/a")] <- NA_character_
  x
}

single_value <- function(x) {
  values <- unique(x[!is.na(x) & nzchar(x)])
  if (length(values) == 1L) values[[1]] else NA_character_
}

joined_values <- function(x) {
  values <- unique(x[!is.na(x) & nzchar(x)])
  if (length(values) == 0L) NA_character_ else paste(values, collapse = " | ")
}

raw <- data.table::as.data.table(readRDS(predicts_raw_path))
reference_paths <- c(
  file.path(repo_root, "00_biodiversity_data", "predicts", "predicts_references_2016.csv"),
  file.path(repo_root, "00_biodiversity_data", "predicts", "predicts_references_2022.csv")
)
available_reference_paths <- reference_paths[file.exists(reference_paths)]
reference_titles <- if (length(available_reference_paths) > 0L) {
  references <- data.table::rbindlist(lapply(available_reference_paths, function(path) {
    data.table::as.data.table(readr::read_csv(path, show_col_types = FALSE))
  }), fill = TRUE)
  references[, .(source_id = as.character(Source_ID), reference_title = as.character(Title))][
    !is.na(source_id) & nzchar(source_id), .(reference_title = joined_values(reference_title)), by = source_id
  ]
} else {
  data.table::data.table(source_id = character(), reference_title = character())
}
site_columns <- c(
  "Source_ID", "Reference", "Study_number", "Site_number", "SSBS",
  "Predominant_land_use", "Use_intensity", "Habitat_as_described",
  "Source_for_predominant_land_use", "Longitude", "Latitude", "Country",
  "UN_subregion", "Years_since_fragmentation_or_conversion"
)
missing_columns <- setdiff(site_columns, names(raw))
if (length(missing_columns) > 0L) {
  stop("PREDICTS extract lacks columns required for Hill 2018 re-curation: ",
       paste(missing_columns, collapse = ", "), call. = FALSE)
}
site_raw <- unique(raw[, lapply(.SD, as.character), .SDcols = site_columns])
site_raw[, site_key := paste(Source_ID, Study_number, Site_number, sep = "::")]

# Multiple biodiversity records share one site. Preserve all distinct retained
# text descriptions in one searchable evidence field and expose conflicts.
sites <- site_raw[, .(
  reference = single_value(Reference),
  ssbs = single_value(SSBS),
  original_land_use = single_value(Predominant_land_use),
  original_land_use_values = joined_values(Predominant_land_use),
  original_intensity = single_value(Use_intensity),
  original_intensity_values = joined_values(Use_intensity),
  habitat_description = joined_values(Habitat_as_described),
  land_use_source = joined_values(Source_for_predominant_land_use),
  longitude = suppressWarnings(as.numeric(single_value(Longitude))),
  latitude = suppressWarnings(as.numeric(single_value(Latitude))),
  country = single_value(Country),
  un_subregion = single_value(UN_subregion),
  years_since_conversion = suppressWarnings(as.numeric(single_value(Years_since_fragmentation_or_conversion))),
  n_site_metadata_rows = .N
), by = .(site_key, source_id = Source_ID, study_number = Study_number, site_number = Site_number)]
sites <- merge(sites, reference_titles, by = "source_id", all.x = TRUE, sort = FALSE)

sites[, `:=`(
  original_land_use = fifelse(original_land_use == "Cannot decide", NA_character_, original_land_use),
  hill2018_land_use = NA_character_,
  recuration_status = "review_required",
  recuration_evidence = NA_character_,
  recuration_confidence = NA_character_,
  review_reason = NA_character_
)]
sites[, hill2018_intensity := dplyr::case_when(
  original_intensity == "Minimal use" ~ "minimal",
  original_intensity == "Light use" ~ "light",
  original_intensity == "Intense use" ~ "intense",
  TRUE ~ NA_character_
)]

# Hill split primary vegetation into forest and non-forest using LUH2's static
# potential-forest mask. This is spatially deterministic for georeferenced
# sites and avoids trying to infer canopy type from prose descriptions.
valid_coordinates <- is.finite(sites$longitude) & is.finite(sites$latitude) &
  abs(sites$longitude) <= 180 & abs(sites$latitude) <= 90
sites[, potential_forest := NA_real_]
if (any(valid_coordinates)) {
  forest_mask <- terra::rast(luh2_static_path, subds = "fstnf")
  site_points <- terra::vect(
    data.frame(longitude = sites$longitude[valid_coordinates], latitude = sites$latitude[valid_coordinates]),
    geom = c("longitude", "latitude"), crs = "EPSG:4326"
  )
  forest_values <- terra::extract(forest_mask, site_points)[, 2]
  sites[which(valid_coordinates), potential_forest := as.numeric(forest_values)]
}
sites[original_land_use == "Primary vegetation" & potential_forest >= 0.5, `:=`(
  hill2018_land_use = "primary_forest", recuration_status = "retained",
  recuration_evidence = "LUH2 fstnf potential-forest mask", recuration_confidence = "high"
)]
sites[original_land_use == "Primary vegetation" & potential_forest < 0.5, `:=`(
  hill2018_land_use = "primary_nonforest", recuration_status = "retained",
  recuration_evidence = "LUH2 fstnf potential-forest mask", recuration_confidence = "high"
)]
sites[original_land_use == "Primary vegetation" & is.na(potential_forest),
      review_reason := "primary vegetation lacks usable coordinates for LUH2 potential-forest assignment"]

secondary_map <- c(
  "Young secondary vegetation" = "secondary_young",
  "Intermediate secondary vegetation" = "secondary_intermediate",
  "Mature secondary vegetation" = "secondary_mature",
  "Secondary vegetation (indeterminate age)" = "secondary_indeterminate"
)
for (original_class in names(secondary_map)) {
  sites[original_land_use == original_class, `:=`(
    hill2018_land_use = secondary_map[[original_class]], recuration_status = "retained",
    recuration_evidence = "retained PREDICTS secondary-vegetation age label", recuration_confidence = "high"
  )]
}
sites[original_land_use == "Urban", `:=`(
  hill2018_land_use = "urban", recuration_status = "retained",
  recuration_evidence = "retained PREDICTS urban label", recuration_confidence = "high"
)]

rules <- data.table::as.data.table(readr::read_csv(hill2018_rules_path, show_col_types = FALSE))
required_rule_columns <- c("rule_id", "original_land_use", "hill2018_land_use", "pattern", "priority", "confidence", "method_note")
if (length(setdiff(required_rule_columns, names(rules))) > 0L) {
  stop("Hill 2018 rule table has missing columns.", call. = FALSE)
}
data.table::setorder(rules, priority)
sites[, searchable_text := normalise_text(paste(
  habitat_description, reference, reference_title, source_id, country, un_subregion, sep = " | "
))]

# Apply only the explicit rules. If several rules match, the first (lowest
# priority number) is retained and the complete match list is recorded.
for (original_class in unique(rules$original_land_use)) {
  candidate_rows <- which(sites$original_land_use == original_class)
  if (length(candidate_rows) == 0L) next
  class_rules <- rules[original_land_use == original_class]
  for (index in candidate_rows) {
    text <- sites$searchable_text[[index]]
    if (is.na(text)) next
    matched <- class_rules[vapply(pattern, function(pattern_value) {
      grepl(pattern_value, text, perl = TRUE)
    }, logical(1))]
    if (nrow(matched) == 0L) next
    chosen <- matched[1]
    sites[index, `:=`(
      hill2018_land_use = chosen$hill2018_land_use,
      recuration_status = ifelse(chosen$hill2018_land_use == "excluded_timber_plantation", "excluded_no_luh2_equivalent", "retained"),
      recuration_evidence = paste0("text rule: ", chosen$rule_id),
      recuration_confidence = chosen$confidence,
      review_reason = if (nrow(matched) > 1L) paste0("multiple matching rules; selected ", chosen$rule_id,
                                                      "; also matched ", paste(matched$rule_id[-1], collapse = ", ")) else NA_character_
    )]
  }
}

sites[original_land_use %in% c("Cropland", "Pasture", "Plantation forest") &
        is.na(hill2018_land_use),
      review_reason := "retained metadata does not identify the Hill 2018 LUH2-compatible subclass"]
sites[is.na(original_land_use), review_reason := "PREDICTS predominant land use is unavailable or cannot decide"]
sites[recuration_status == "retained" & is.na(hill2018_intensity), `:=`(
  recuration_status = "review_required",
  review_reason = fifelse(is.na(review_reason), "PREDICTS use intensity is unavailable or cannot decide", review_reason)
)]
sites[, searchable_text := NULL]

coverage <- sites[, .N, by = .(original_land_use, hill2018_land_use, recuration_status, recuration_confidence)][order(original_land_use, recuration_status, hill2018_land_use)]
review_queue <- sites[recuration_status == "review_required" | !is.na(review_reason), .(
  site_key, source_id, reference, study_number, site_number, country, un_subregion,
  original_land_use, original_intensity, habitat_description, reference_title, land_use_source,
  hill2018_land_use, recuration_status, recuration_evidence, recuration_confidence, review_reason
)]
metadata <- data.table::data.table(
  metadata_key = c(
    "method", "primary_assignment", "secondary_assignment", "crop_pasture_plantation_assignment",
    "unresolved_case_policy", "source_data", "citation"
  ),
  value = c(
    "Site-level LUH2-compatible re-curation following Hill et al. (2018), applied only when retained metadata supplies evidence.",
    "Primary vegetation is split using LUH2 v2h static fstnf (potential-forest) mask at each site coordinate.",
    "Existing PREDICTS young/intermediate/mature/indeterminate secondary labels are retained; LUH2 transition histories are needed separately for gridded projection age shares.",
    "Versioned regular-expression rules use retained site descriptions. They are not a substitute for unavailable paper-by-paper data-provider correspondence.",
    "Ambiguous cropland, pasture and plantation sites are kept in the review queue and excluded from a strict Hill-compatible fitting subset.",
    basename(predicts_raw_path),
    "Hill et al. 2018, bioRxiv 10.1101/311787; Appendix: PREDICTS definitions of LUH2 land-use classes."
  )
)
data.table::setnames(metadata, "metadata_key", "key")

saveRDS(sites, hill2018_site_recuration_path, compress = "gzip")
write_csv_safe(sites, hill2018_site_recuration_csv_path)
write_csv_safe(coverage, hill2018_recuration_coverage_path)
write_csv_safe(review_queue, hill2018_recuration_review_path)
write_csv_safe(metadata, hill2018_recuration_metadata_path)

message("Wrote Hill 2018 PREDICTS site re-curation: ", nrow(sites), " sites; ",
        sum(sites$recuration_status == "retained"), " retained; ", nrow(review_queue), " review records.")
