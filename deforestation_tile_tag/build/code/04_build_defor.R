# =====================================================
# Build canonical deforestation tables
# Creates:
#   - defor_tile_attributes : one row per tile
#   - defor_tile_year.csv       : one row per tile-year
#   - defor_tile_geometry.gpkg   : spatial tile layer for matching
# =====================================================

sf_use_s2(FALSE)

# -----------------------
# Canonical tile attribute table
# one row per tile
# -----------------------

tile_country <- defor_spatial_raw %>%
  st_drop_geometry() %>%
  transmute(
    tile_id = as.character(id),
    tile_country_id = as.character(iso3),
    country_name = as.character(name)
  ) %>%
  group_by(tile_id) %>%
  summarise(
    tile_country_id = dplyr::first(tile_country_id),
    country_name = dplyr::first(country_name),
    .groups = "drop"
  )

tile_coverage <- defor_tabular_raw %>%
  transmute(
    tile_id = as.character(id),
    year = as.integer(lossyear_mode),
    has_ha_info_year = !is.na(total_deforested_ha)
  ) %>%
  filter(!is.na(tile_id), !is.na(year)) %>%
  group_by(tile_id) %>%
  summarise(
    has_ha_info = any(has_ha_info_year),
    .groups = "drop"
  )

defor_tile_attributes <- tile_country %>%
  left_join(tile_coverage, by = "tile_id") %>%
  mutate(
    has_ha_info = coalesce(has_ha_info, FALSE)
  )

assert_no_duplicate_keys(
  defor_tile_attributes,
  c("tile_id"),
  "defor_tile_attributes"
)

defor_years <- defor_tabular_raw %>%
  transmute(year = as.integer(lossyear_mode)) %>%
  filter(!is.na(year)) %>%
  distinct(year) %>%
  arrange(year) %>%
  pull(year)

if (length(defor_years) == 0) {
  stop("No valid deforestation years found in defor_tabular_raw.", call. = FALSE)
}

# -----------------------
# Canonical tile-year table
# one row per tile-year.
# Missing tile-years are zero agricultural deforestation, not unknown.
# -----------------------

defor_tile_year_observed <- defor_tabular_raw %>%
  transmute(
    tile_id = as.character(id),
    year = as.integer(lossyear_mode),
    defor_total_ha = coalesce(as.numeric(total_deforested_ha), 0),
    defor_crops_ha = coalesce(as.numeric(total_deforested_ha_crops), 0),
    defor_livestock_ha = coalesce(as.numeric(total_deforested_ha_livestock), 0)
  ) %>%
  filter(!is.na(tile_id), !is.na(year))

assert_no_duplicate_keys(
  defor_tile_year_observed,
  c("tile_id", "year"),
  "defor_tile_year_observed"
)

defor_tile_year <- tidyr::crossing(
  tile_id = defor_tile_attributes$tile_id,
  year = defor_years
) %>%
  left_join(
    defor_tile_year_observed,
    by = c("tile_id", "year")
  ) %>%
  mutate(
    defor_total_ha = coalesce(defor_total_ha, 0),
    defor_crops_ha = coalesce(defor_crops_ha, 0),
    defor_livestock_ha = coalesce(defor_livestock_ha, 0)
  )

assert_no_duplicate_keys(
  defor_tile_year,
  c("tile_id", "year"),
  "defor_tile_year"
)

# -----------------------
# Natural Earth country geometry layer
# one row per country-level polygon unit
# -----------------------

country_geometry <- country_boundaries_raw %>%
  transmute(
    country_id = as.character(ADM0_A3),
    country_iso3 = dplyr::if_else(
      is.na(ISO_A3) | ISO_A3 == "-99",
      as.character(ADM0_A3),
      as.character(ISO_A3)
    ),
    country_name = as.character(ADMIN),
    country_name_long = as.character(NAME_LONG),
    sovereign_name = as.character(SOVEREIGNT),
    geometry
  ) %>%
  filter(!is.na(country_id), nzchar(country_id)) %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON", warn = FALSE) %>%
  st_transform(analysis_crs)

assert_no_duplicate_keys(
  st_drop_geometry(country_geometry),
  c("country_id"),
  "country_geometry"
)

# -----------------------
# Spatial tile geometry layer
# may contain multiple rows per tile_id
# -----------------------

defor_tile_geometry <- read_or_build(
  path = defor_tiles_cache,
  build_fn = function() {
    defor_spatial_raw %>%
      transmute(
        tile_id = as.character(id),
        geometry
      ) %>%
      left_join(defor_tile_attributes, by = "tile_id") %>%
      st_make_valid() %>%
      st_collection_extract("POLYGON", warn = FALSE) %>%
      st_transform(analysis_crs)
  }
)

message("Built deforestation tables:")
message("  defor_tile_attributes rows: ", nrow(defor_tile_attributes))
message("  defor_tile_year rows: ", nrow(defor_tile_year))
message("  defor_tile_geometry rows: ", nrow(defor_tile_geometry))
message("  country_geometry rows: ", nrow(country_geometry))
