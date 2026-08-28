assert_file_exists(clean_timeseries_path, "Clean BioTIME time-series table")

message("Reading cleaned BioTIME time-series table")
timeseries <- readr::read_csv(clean_timeseries_path, show_col_types = FALSE)
assert_has_cols(
  timeseries,
  c("time_series_id", "latitude", "longitude", "AEZ", "taxon_group"),
  "Clean BioTIME time-series table"
)

sf::sf_use_s2(FALSE)

site_points <- timeseries |>
  dplyr::filter(!is.na(latitude), !is.na(longitude)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  sf::st_transform(analysis_crs)

site_buffers <- site_points |>
  dplyr::mutate(
    buffer_km = buffer_km,
    geometry = sf::st_buffer(geometry, dist = buffer_km * 1000),
    buffer_area_ha = as.numeric(sf::st_area(geometry)) / 10000,
    hansen_site_buffer_id = time_series_id
  ) |>
  sf::st_make_valid()

write_gpkg_safe(site_buffers, site_buffer_gpkg_path)

site_buffers_export <- site_buffers |>
  dplyr::select(
    hansen_site_buffer_id,
    time_series_id,
    AEZ,
    taxon_group,
    buffer_km,
    buffer_area_ha
  ) |>
  sf::st_transform(4326)

sf::st_write(
  site_buffers_export,
  dsn = site_buffer_geojson_path,
  delete_dsn = TRUE,
  quiet = TRUE
)
message("Wrote: ", site_buffer_geojson_path)

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

write_csv_safe(site_buffer_lookup, file.path(tabular_output_dir, "biotime_timeseries_1km_buffer_lookup.csv"))

