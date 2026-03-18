## =====================================================
# Load canonical data from build pipeline
# =====================================================

# -----------------------
# Paths
# -----------------------

cluster_year_ov_path <- file.path(canonical_tabular_dir, "cluster_year_ov.csv")
cluster_buffer_year_defor_path <- file.path(canonical_tabular_dir, "cluster_buffer_year_defor.csv")
cluster_buffer_tile_path <- file.path(canonical_tabular_dir, "cluster_buffer_tile.csv")
defor_tile_year_path <- file.path(canonical_tabular_dir, "defor_tile_year.csv")

cluster_buffer_path <- file.path(canonical_spatial_dir, "cluster_buffer.gpkg")
cluster_sites_path <- file.path(canonical_spatial_dir, "cluster_sites.gpkg")
defor_tile_geometry_path <- file.path(canonical_spatial_dir, "defor_tile_geometry.gpkg")

assert_exists(cluster_year_ov_path)
assert_exists(cluster_buffer_year_defor_path)
assert_exists(cluster_buffer_tile_path)
assert_exists(defor_tile_year_path)
assert_exists(cluster_buffer_path)
assert_exists(cluster_sites_path)
assert_exists(defor_tile_geometry_path)

# -----------------------
# Read tabular files
# -----------------------

cluster_year_ov <- read_csv(cluster_year_ov_path, show_col_types = FALSE)
cluster_buffer_year_defor <- read_csv(cluster_buffer_year_defor_path, show_col_types = FALSE)
cluster_buffer_tile <- read_csv(cluster_buffer_tile_path, show_col_types = FALSE)
defor_tile_year <- read_csv(defor_tile_year_path, show_col_types = FALSE)

# -----------------------
# Read spatial files
# -----------------------

cluster_buffer <- read_sf(cluster_buffer_path)
cluster_sites <- read_sf(cluster_sites_path)
defor_tile_geometry <- read_sf(defor_tile_geometry_path)

# testing
names(defor_tile_geometry)
table(defor_tile_geometry$has_ha_info, useNA = "ifany")

# -----------------------
# Validate the data read in has the expected columns
# -----------------------

assert_has_cols(
  cluster_year_ov,
  c("AEZ", "cluster_id", "year", "median_ov_year", "n_sites_year"),
  "cluster_year_ov"
)

assert_has_cols(
  cluster_buffer_year_defor,
  c("AEZ", "cluster_id", "buffer_km", "year", "defor_total_ha_year", "n_tiles_with_ha"),
  "cluster_buffer_year_defor"
)

assert_has_cols(
  cluster_buffer_tile,
  c("AEZ", "cluster_id", "buffer_km", "tile_id", "country_name", "n_years_with_ha", "has_ha_info"),
  "cluster_buffer_tile"
)

assert_has_cols(
  defor_tile_year,
  c("tile_id", "year", "defor_total_ha"),
  "defor_tile_year"
)

assert_has_cols(
  cluster_buffer,
  c("AEZ", "cluster_id", "buffer_km", "n_sites", "n_matched_tiles",
    "n_matched_tiles_with_ha", "n_matched_tiles_missing_ha",
    "tagged_any_tile", "tagged_ha_tile"),
  "cluster_buffer"
)

assert_has_cols(
  cluster_sites,
  c("sample_id", "AEZ", "year", "cluster_id", "ov",
    "latitude", "longitude", "method", "dist_to_medoid"),
  "cluster_sites"
)

assert_has_cols(
  defor_tile_geometry,
  c("tile_id", "has_ha_info", "geom"),
  "defor_tile_geometry"
)

# -----------------------
# Harmonize data types (AEZ order, as character/integer)
# -----------------------

cluster_year_ov <- cluster_year_ov %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id),
    year = as.integer(year)
  )

cluster_buffer_year_defor <- cluster_buffer_year_defor %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id),
    buffer_km = as.numeric(buffer_km),
    year = as.integer(year)
  )

cluster_buffer_tile <- cluster_buffer_tile %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id),
    buffer_km = as.numeric(buffer_km),
    tile_id = as.character(tile_id)
  )

defor_tile_year <- defor_tile_year %>%
  mutate(
    tile_id = as.character(tile_id),
    year = as.integer(year)
  )

cluster_sites <- cluster_sites %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id),
    year = as.integer(year),
  )

cluster_buffer <- cluster_buffer %>%
  dplyr::mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id),
    buffer_km = as.numeric(buffer_km)
  )

defor_tiles_all_sf <- defor_tile_geometry %>%
  mutate(
    has_ha_info = coalesce(has_ha_info, FALSE)
  )