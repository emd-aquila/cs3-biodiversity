# =====================================================
# Build canonical deforestation tables
# Creates:
#   - defor_tile_attributes : one row per tile
#   - defor_tile_year       : one row per tile-year
#   - defor_tile_geometry   : spatial tile layer for matching
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
    country_name = as.character(name)
  ) %>%
  group_by(tile_id) %>%
  summarise(
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
    n_years_with_ha = sum(has_ha_info_year, na.rm = TRUE),
    has_ha_info = n_years_with_ha > 0,
    .groups = "drop"
  )

defor_tile_attributes <- tile_country %>%
  left_join(tile_coverage, by = "tile_id") %>%
  mutate(
    n_years_with_ha = coalesce(n_years_with_ha, 0L),
    has_ha_info = coalesce(has_ha_info, FALSE)
  )

assert_no_duplicate_keys(
  defor_tile_attributes,
  c("tile_id"),
  "defor_tile_attributes"
)

# -----------------------
# Canonical tile-year table
# one row per tile-year
# -----------------------

defor_tile_year <- defor_tabular_raw %>%
  transmute(
    tile_id = as.character(id),
    year = as.integer(lossyear_mode),
    defor_total_ha = as.numeric(total_deforested_ha)
  ) %>%
  filter(!is.na(tile_id), !is.na(year))

assert_no_duplicate_keys(
  defor_tile_year,
  c("tile_id", "year"),
  "defor_tile_year"
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