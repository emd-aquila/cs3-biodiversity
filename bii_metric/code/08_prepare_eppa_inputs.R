# Convert EPPA regional land-use areas to the same long, share-based contract
# consumed by the Landsat projection code. This makes the projection step
# source-agnostic once the two crosswalks have been reviewed.
assert_file_exists(eppa_landuse_path, "EPPA land-use input")
eppa_raw <- as.data.table(readr::read_csv(eppa_landuse_path, show_col_types = FALSE))
assert_has_cols(eppa_raw, c("scenario", "year", "region", "source_class", "area_km2"), "EPPA land-use input")
if (!"cell_id" %in% names(eppa_raw)) eppa_raw[, cell_id := paste0("EPPA:", region)]
eppa_raw[, `:=`(year = as.integer(year), area_km2 = as.numeric(area_km2))]
if (any(!is.finite(eppa_raw$area_km2) | eppa_raw$area_km2 < 0)) {
  stop("EPPA area_km2 must be finite and non-negative.", call. = FALSE)
}
eppa_keys <- c("scenario", "year", "region", "cell_id")
eppa_raw[, total_area_km2 := sum(area_km2), by = eppa_keys]
if (any(eppa_raw$total_area_km2 <= 0)) stop("Every EPPA scenario/year/region requires positive total land area.", call. = FALSE)
eppa_raw[, share := area_km2 / total_area_km2]
eppa_raw[, area_km2 := total_area_km2]

eppa_normalised_path <- file.path(tmp_dir, "eppa_landuse_normalised.csv")
write_csv_safe(eppa_raw[, .(scenario, year, region, cell_id, source_class, share, area_km2)], eppa_normalised_path)
effective_crosswalk_path <- eppa_crosswalk_path
use_intensity_projection <- isTRUE(bii_v2_enabled) || identical(model_variant, "land_use_intensity")
if (use_intensity_projection) {
  base_crosswalk <- as.data.table(readr::read_csv(eppa_crosswalk_path, show_col_types = FALSE))
  assert_file_exists(eppa_intensity_path, "EPPA intensity scenario table")
  intensity <- as.data.table(readr::read_csv(eppa_intensity_path, show_col_types = FALSE))[
    intensity_scenario == eppa_intensity_scenario
  ]
  assert_has_cols(
    intensity, c("bii_class", "intensity", "intensity_share"), "EPPA intensity scenario table"
  )
  intensity[, intensity_share := as.numeric(intensity_share)]
  if (any(!is.finite(intensity$intensity_share)) || any(abs(intensity[, sum(intensity_share), by = bii_class]$V1 - 1) > 1e-6)) {
    stop("EPPA intensity shares must be finite and sum to 1 within each BII class.", call. = FALSE)
  }
  base_crosswalk <- merge(
    base_crosswalk, intensity, by = "bii_class", all.x = TRUE,
    allow.cartesian = TRUE, suffixes = c("_crosswalk", "_scenario")
  )
  if (any(is.na(base_crosswalk$intensity_share))) {
    missing <- unique(base_crosswalk[is.na(intensity_share), bii_class])
    stop("EPPA intensity scenario is missing BII class(es): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  base_crosswalk[, `:=`(
    allocation_share = as.numeric(allocation_share) * intensity_share,
    intensity = as.character(intensity_scenario)
  )]
  effective_crosswalk_path <- file.path(tmp_dir, "eppa_to_bii_crosswalk_with_intensity.csv")
  write_csv_safe(base_crosswalk[, .(source_class, bii_class, allocation_share, intensity)], effective_crosswalk_path)
}
eppa_landuse <- prepare_landuse_with_crosswalk(eppa_normalised_path, effective_crosswalk_path, "EPPA")
saveRDS(eppa_landuse, eppa_prepared_path, compress = "gzip")
message("Wrote: ", eppa_prepared_path)

eppa_audit <- eppa_landuse[, .(
  n_regions = uniqueN(region), n_cells = uniqueN(cell_id), total_area_km2 = sum(unique(area_km2))
), by = .(scenario, year, pressure_class)]
write_csv_safe(eppa_audit, file.path(output_dir, "eppa_bii_crosswalk_audit.csv"))
