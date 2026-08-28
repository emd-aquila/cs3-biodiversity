assert_file_exists(clean_sample_year_path, "Clean BioTIME sample-year table")
assert_file_exists(site_buffer_gpkg_path, "BioTIME 1 km site buffer GPKG")
assert_file_exists(hansen_site_year_defor_path, "Hansen BioTIME site-year deforestation CSV")

message("Reading Hansen site-year deforestation")
hansen_site_year <- readr::read_csv(hansen_site_year_defor_path, show_col_types = FALSE)
assert_has_cols(
  hansen_site_year,
  c(
    "time_series_id",
    "AEZ",
    "buffer_km",
    "year",
    "defor_ha_total_raw",
    "hansen_land_area_ha",
    "hansen_treecover2000_equiv_ha",
    "hansen_treecover2000_mean_pct"
  ),
  "Hansen BioTIME site-year deforestation"
)

hansen_site_year <- hansen_site_year |>
  dplyr::mutate(
    year = as.integer(year),
    buffer_km = as.numeric(buffer_km),
    defor_ha_total_raw = dplyr::coalesce(as.numeric(defor_ha_total_raw), 0),
    hansen_land_area_ha = as.numeric(hansen_land_area_ha),
    hansen_treecover2000_equiv_ha = as.numeric(hansen_treecover2000_equiv_ha),
    hansen_treecover2000_mean_pct = as.numeric(hansen_treecover2000_mean_pct)
  ) |>
  dplyr::filter(year %in% hansen_years)

site_buffers <- sf::read_sf(site_buffer_gpkg_path)
site_buffer_lookup <- site_buffers |>
  sf::st_drop_geometry() |>
  dplyr::select(
    time_series_id,
    AEZ,
    taxon_group,
    latitude,
    longitude,
    buffer_km,
    buffer_area_ha
  )

missing_hansen_ids <- setdiff(site_buffer_lookup$time_series_id, hansen_site_year$time_series_id)
if (length(missing_hansen_ids) > 0) {
  stop(
    paste0(
      "Hansen export is missing ", length(missing_hansen_ids),
      " BioTIME assemblage buffer IDs. The export is probably stale; rerun with ",
      "BIOTIME_ONLY_FORCE_HANSEN_EXPORT=true."
    ),
    call. = FALSE
  )
}

site_hansen_meta <- hansen_site_year |>
  dplyr::group_by(time_series_id, buffer_km) |>
  dplyr::summarise(
    hansen_land_area_ha = max(hansen_land_area_ha, na.rm = TRUE),
    hansen_treecover2000_equiv_ha = max(hansen_treecover2000_equiv_ha, na.rm = TRUE),
    hansen_treecover2000_mean_pct = dplyr::first(hansen_treecover2000_mean_pct),
    n_hansen_years = dplyr::n_distinct(year),
    has_hansen_data = is.finite(hansen_land_area_ha) & hansen_land_area_ha > 0,
    .groups = "drop"
  )

matched_tiles <- site_buffer_lookup |>
  dplyr::left_join(site_hansen_meta, by = c("time_series_id", "buffer_km")) |>
  dplyr::mutate(
    tile_id = paste0("hansen_1km_buffer__", time_series_id),
    has_ha_info = has_hansen_data,
    intersection_area_ha = hansen_land_area_ha,
    normalized_overlap_share = dplyr::if_else(has_hansen_data, 1, NA_real_),
    n_matched_tiles = dplyr::if_else(has_hansen_data, 1L, 0L),
    tile_note = "Earth Engine reduced Hansen pixels over the exact 1 km BioTIME buffer; no separate local raster tile split is used."
  ) |>
  dplyr::select(
    time_series_id,
    AEZ,
    taxon_group,
    latitude,
    longitude,
    buffer_km,
    buffer_area_ha,
    tile_id,
    has_ha_info,
    intersection_area_ha,
    normalized_overlap_share,
    n_matched_tiles,
    hansen_land_area_ha,
    hansen_treecover2000_equiv_ha,
    hansen_treecover2000_mean_pct,
    n_hansen_years,
    tile_note
  )

sample_year <- readr::read_csv(clean_sample_year_path, show_col_types = FALSE)
assert_has_cols(
  sample_year,
  c(
    "time_series_id",
    "sample_year",
    "AEZ",
    "taxon_group",
    "ov",
    "shannon",
    "effective_species",
    "richness",
    "total_abundance",
    "n_observation_rows",
    "n_sample_desc"
  ),
  "Clean BioTIME sample-year table"
)

if (!"simpson" %in% names(sample_year)) {
  sample_year$simpson <- NA_real_
}

year_pairs <- sample_year |>
  dplyr::arrange(time_series_id, sample_year) |>
  dplyr::group_by(time_series_id) |>
  dplyr::mutate(
    year_t1 = sample_year,
    year_t2 = dplyr::lead(sample_year),
    ov_t1 = ov,
    ov_t2 = dplyr::lead(ov),
    shannon_t1 = shannon,
    shannon_t2 = dplyr::lead(shannon),
    richness_t1 = richness,
    richness_t2 = dplyr::lead(richness),
    effective_species_t1 = effective_species,
    effective_species_t2 = dplyr::lead(effective_species),
    simpson_t1 = simpson,
    simpson_t2 = dplyr::lead(simpson),
    total_abundance_t1 = total_abundance,
    total_abundance_t2 = dplyr::lead(total_abundance),
    n_observation_rows_t1 = n_observation_rows,
    n_observation_rows_t2 = dplyr::lead(n_observation_rows),
    n_sample_desc_t1 = n_sample_desc,
    n_sample_desc_t2 = dplyr::lead(n_sample_desc)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(year_t2), year_t2 > year_t1) |>
  dplyr::mutate(
    year_gap = year_t2 - year_t1,
    delta_ov = ov_t2 - ov_t1,
    delta_ov_annualized = delta_ov / year_gap,
    delta_shannon = shannon_t2 - shannon_t1,
    delta_shannon_annualized = delta_shannon / year_gap,
    delta_richness = richness_t2 - richness_t1,
    delta_richness_annualized = delta_richness / year_gap,
    delta_effective_species = effective_species_t2 - effective_species_t1,
    delta_effective_species_annualized = delta_effective_species / year_gap,
    delta_simpson = simpson_t2 - simpson_t1,
    delta_simpson_annualized = delta_simpson / year_gap
  ) |>
  dplyr::select(
    time_series_id,
    study_id,
    latitude,
    longitude,
    AEZ,
    taxon_group,
    taxa_raw,
    organisms_raw,
    title,
    year_t1,
    year_t2,
    year_gap,
    dplyr::everything(),
    -sample_year,
    -ov,
    -shannon,
    -effective_species,
    -simpson,
    -richness,
    -total_abundance,
    -n_observation_rows,
    -n_sample_desc
  )

interval_defor <- year_pairs |>
  dplyr::select(time_series_id, year_t1, year_t2, year_gap) |>
  dplyr::left_join(hansen_site_year, by = "time_series_id", relationship = "many-to-many") |>
  dplyr::filter(year > year_t1, year <= year_t2) |>
  dplyr::group_by(time_series_id, year_t1, year_t2) |>
  dplyr::summarise(
    delta_defor_ha = sum(defor_ha_total_raw, na.rm = TRUE),
    hansen_land_area_ha = dplyr::first(hansen_land_area_ha),
    hansen_treecover2000_equiv_ha = dplyr::first(hansen_treecover2000_equiv_ha),
    hansen_treecover2000_mean_pct = dplyr::first(hansen_treecover2000_mean_pct),
    n_defor_years = dplyr::n_distinct(year),
    defor_years = paste(sort(unique(year)), collapse = ";"),
    .groups = "drop"
  )

expected_interval_years <- year_pairs |>
  dplyr::rowwise() |>
  dplyr::mutate(
    n_defor_years_expected = length(complete_interval_years(year_t1, year_t2, hansen_years)),
    expected_defor_years = paste(complete_interval_years(year_t1, year_t2, hansen_years), collapse = ";")
  ) |>
  dplyr::ungroup() |>
  dplyr::select(time_series_id, year_t1, year_t2, n_defor_years_expected, expected_defor_years)

year_pair_defor <- year_pairs |>
  dplyr::left_join(interval_defor, by = c("time_series_id", "year_t1", "year_t2")) |>
  dplyr::left_join(expected_interval_years, by = c("time_series_id", "year_t1", "year_t2")) |>
  dplyr::mutate(
    delta_defor_ha = dplyr::coalesce(delta_defor_ha, 0),
    n_defor_years = dplyr::coalesce(n_defor_years, 0L),
    has_complete_hansen_interval = n_defor_years == n_defor_years_expected,
    delta_defor_ha_annualized = delta_defor_ha / year_gap,
    delta_defor_land_share = dplyr::if_else(
      hansen_land_area_ha > 0,
      delta_defor_ha / hansen_land_area_ha,
      NA_real_
    ),
    delta_defor_land_share_annualized = delta_defor_land_share / year_gap,
    delta_defor_pct = 100 * delta_defor_land_share,
    delta_defor_pct_annualized = 100 * delta_defor_land_share_annualized,
    delta_defor_treecover_share = dplyr::if_else(
      hansen_treecover2000_equiv_ha > 0,
      delta_defor_ha / hansen_treecover2000_equiv_ha,
      NA_real_
    ),
    delta_defor_treecover_share_annualized = delta_defor_treecover_share / year_gap,
    log1p_delta_defor_ha_annualized = log1p(delta_defor_ha_annualized)
  ) |>
  dplyr::arrange(AEZ, taxon_group, time_series_id, year_t1, year_t2)

matching_summary <- data.frame(
  metric = c(
    "time_series_buffers",
    "hansen_site_year_rows",
    "time_series_with_hansen_land_area",
    "year_pairs_total",
    "year_pairs_complete_hansen_interval",
    "year_pairs_missing_hansen_interval"
  ),
  value = c(
    nrow(site_buffer_lookup),
    nrow(hansen_site_year),
    sum(matched_tiles$has_ha_info, na.rm = TRUE),
    nrow(year_pair_defor),
    sum(year_pair_defor$has_complete_hansen_interval, na.rm = TRUE),
    sum(!year_pair_defor$has_complete_hansen_interval, na.rm = TRUE)
  )
)

write_csv_safe(hansen_site_year, site_year_defor_output_path)
write_csv_safe(matched_tiles, matched_tiles_path)
write_csv_safe(year_pair_defor, year_pair_defor_path)
write_csv_safe(matching_summary, defor_summary_path)
write_csv_safe(year_pair_defor, file.path(processed_data_dir, "biotime_year_pair_deforestation.csv"))

message("BioTIME Hansen matching complete.")
