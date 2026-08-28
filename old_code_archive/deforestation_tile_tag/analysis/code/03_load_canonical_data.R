## =====================================================
# Load canonical data from build pipeline for the current cluster run
# =====================================================

# -----------------------
# Preconditions
# -----------------------

required_objects <- c(
  "canonical_tabular_dir",
  "canonical_spatial_dir"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    paste0(
      "03_load_canonical_data.R is missing required objects: ",
      paste(missing_objects, collapse = ", ")
    ),
    call. = FALSE
  )
}

# -----------------------
# File paths for current cluster run
# -----------------------

cluster_year_ov_path <- file.path(canonical_tabular_dir, "cluster_year_ov.csv")
cluster_buffer_year_defor_path <- file.path(canonical_tabular_dir, "cluster_year_defor.csv")
cluster_buffer_tile_path <- file.path(canonical_tabular_dir, "matched_clusters_tiles.csv")
cluster_buffer_country_path <- file.path(canonical_tabular_dir, "matched_clusters_countries.csv")
defor_tile_year_path <- file.path(canonical_tabular_dir, "defor_tile_year.csv")

cluster_buffer_path <- file.path(canonical_spatial_dir, "cluster_buffer.gpkg")
cluster_sites_path <- file.path(canonical_spatial_dir, "cluster_sites.gpkg")
defor_tile_geometry_path <- file.path(canonical_spatial_dir, "defor_tile_geometry.gpkg")

assert_exists(cluster_year_ov_path)
assert_exists(cluster_buffer_year_defor_path)
assert_exists(cluster_buffer_tile_path)
assert_exists(cluster_buffer_country_path)
assert_exists(defor_tile_year_path)
assert_exists(cluster_buffer_path)
assert_exists(cluster_sites_path)
assert_exists(defor_tile_geometry_path)

# -----------------------
# Read GeoPackages through a staged temp copy when needed
# -----------------------

read_sf_safely <- function(path, temp_dir) {
  if (!grepl("\\.gpkg$", path, ignore.case = TRUE)) {
    return(sf::read_sf(path))
  }

  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

  staged_path <- file.path(temp_dir, basename(path))
  journal_path <- paste0(path, "-journal")
  staged_journal_path <- paste0(staged_path, "-journal")

  file.copy(path, staged_path, overwrite = TRUE)

  if (file.exists(staged_journal_path)) {
    file.remove(staged_journal_path)
  }

  if (file.exists(journal_path)) {
    message("Ignoring GeoPackage journal while staging: ", basename(journal_path))
  }

  sf::read_sf(staged_path)
}

# -----------------------
# Read tabular files
# -----------------------

cluster_year_ov <- read_csv(cluster_year_ov_path, show_col_types = FALSE)
cluster_buffer_year_defor <- read_csv(cluster_buffer_year_defor_path, show_col_types = FALSE)
cluster_buffer_tile <- read_csv(cluster_buffer_tile_path, show_col_types = FALSE)
cluster_buffer_country <- read_csv(cluster_buffer_country_path, show_col_types = FALSE)
defor_tile_year <- read_csv(defor_tile_year_path, show_col_types = FALSE)

# -----------------------
# Read spatial files
# -----------------------

sf_stage_dir <- file.path(analysis_tmp_dir, "sf_stage", current_cluster_stub)

cluster_buffer <- read_sf_safely(cluster_buffer_path, sf_stage_dir)
cluster_sites <- read_sf_safely(cluster_sites_path, sf_stage_dir)
defor_tile_geometry <- read_sf_safely(defor_tile_geometry_path, sf_stage_dir)

# -----------------------
# Validate required columns
# -----------------------

assert_has_cols(
  cluster_year_ov,
  c("AEZ", "cluster_id", "year", "median_ov_year", "n_sites_year"),
  "cluster_year_ov"
)

assert_has_cols(
  cluster_buffer_year_defor,
  c(
    "AEZ", "cluster_id", "buffer_km", "year", "n_tiles", "n_tiles_with_ha",
    "defor_ha_total_raw", "defor_ha_total_avg", "defor_ha_total_rel_pct",
    "defor_ha_crops_raw", "defor_ha_crops_avg", "defor_ha_crops_rel_pct"
  ),
  "cluster_buffer_year_defor"
)

assert_has_cols(
  cluster_buffer_tile,
  c(
    "AEZ", "cluster_id", "buffer_km", "tile_id", "country_name", "has_ha_info",
    "intersection_area_ha", "normalized_overlap_share"
  ),
  "cluster_buffer_tile"
)

assert_has_cols(
  cluster_buffer_country,
  c(
    "AEZ", "cluster_id", "buffer_km", "country_id", "country_iso3",
    "country_name", "country_name_long", "sovereign_name",
    "country_intersection_area_ha", "normalized_country_overlap_share"
  ),
  "cluster_buffer_country"
)

assert_has_cols(
  defor_tile_year,
  c("tile_id", "year", "defor_total_ha", "defor_crops_ha", "defor_livestock_ha"),
  "defor_tile_year"
)

assert_has_cols(
  cluster_buffer,
  c(
    "AEZ", "cluster_id", "buffer_km", "n_sites", "n_matched_tiles",
    "n_matched_tiles_with_ha", "n_matched_tiles_missing_ha",
    "tagged_any_tile", "tagged_ha_tile",
    "n_matched_countries", "primary_country_id", "primary_country_name"
  ),
  "cluster_buffer"
)

assert_has_cols(
  cluster_sites,
  c(
    "sample_id", "AEZ", "year", "cluster_id", "ov",
    "latitude", "longitude", "method", "dist_to_medoid"
  ),
  "cluster_sites"
)

assert_has_cols(
  defor_tile_geometry,
  c("tile_id", "has_ha_info"),
  "defor_tile_geometry"
)


# -----------------------
# Harmonize types
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
    year = as.integer(year),
    n_tiles = as.integer(n_tiles),
    n_tiles_with_ha = as.integer(n_tiles_with_ha)
  )

cluster_buffer_tile <- cluster_buffer_tile %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id),
    buffer_km = as.numeric(buffer_km),
    tile_id = as.character(tile_id),
    intersection_area_ha = as.numeric(intersection_area_ha),
    normalized_overlap_share = as.numeric(normalized_overlap_share)
  )

cluster_buffer_country <- cluster_buffer_country %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id),
    buffer_km = as.numeric(buffer_km),
    country_id = as.character(country_id),
    country_iso3 = as.character(country_iso3),
    country_name = as.character(country_name),
    country_name_long = as.character(country_name_long),
    sovereign_name = as.character(sovereign_name),
    country_intersection_area_ha = as.numeric(country_intersection_area_ha),
    normalized_country_overlap_share = as.numeric(normalized_country_overlap_share)
  )

defor_tile_year <- defor_tile_year %>%
  mutate(
    tile_id = as.character(tile_id),
    year = as.integer(year),
    defor_total_ha = as.numeric(defor_total_ha),
    defor_crops_ha = as.numeric(defor_crops_ha),
    defor_livestock_ha = as.numeric(defor_livestock_ha)
  )

cluster_sites <- cluster_sites %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id),
    year = as.integer(year),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    dist_to_medoid = as.numeric(dist_to_medoid)
  )

cluster_buffer <- cluster_buffer %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id),
    buffer_km = as.numeric(buffer_km),
    n_matched_countries = as.integer(n_matched_countries),
    primary_country_id = as.character(primary_country_id),
    primary_country_iso3 = as.character(primary_country_iso3),
    primary_country_name = as.character(primary_country_name),
    primary_country_name_long = as.character(primary_country_name_long),
    primary_sovereign_name = as.character(primary_sovereign_name),
    primary_country_overlap_share = as.numeric(primary_country_overlap_share)
  )

defor_tiles_all_sf <- defor_tile_geometry %>%
  mutate(
    has_ha_info = coalesce(has_ha_info, FALSE)
  )

# -----------------------
# Derive one medoid row per AEZ-cluster
#
# cluster_sites is sample-level, so the same medoid site may appear
# in multiple years/observations, so need to deduplicate first, then take the row
# with minimum dist_to_medoid within each AEZ-cluster.
# -----------------------

cluster_medoids <- cluster_sites %>%
  st_drop_geometry() %>%
  dplyr::select(AEZ, cluster_id, latitude, longitude, dist_to_medoid) %>%
  distinct() %>%
  group_by(AEZ, cluster_id) %>%
  slice_min(dist_to_medoid, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(
    medoid_latitude = latitude,
    medoid_longitude = longitude
  )

assert_has_cols(
  cluster_medoids,
  c("AEZ", "cluster_id", "medoid_latitude", "medoid_longitude", "dist_to_medoid"),
  "cluster_medoids"
)

# -----------------------
# Preserve full multi-buffer objects for per-buffer analysis loop
# -----------------------

cluster_buffer_all <- cluster_buffer
cluster_buffer_tile_all <- cluster_buffer_tile
cluster_buffer_country_all <- cluster_buffer_country
cluster_buffer_year_defor_all <- cluster_buffer_year_defor

# -----------------------
# Build audit
# -----------------------

message("Loaded canonical data for current cluster run:")
if (exists("current_cluster_stub")) {
  message("  cluster run: ", current_cluster_stub)
}
message("  cluster_year_ov rows: ", nrow(cluster_year_ov))
message("  cluster_buffer rows: ", nrow(cluster_buffer))
message("  cluster_buffer_country rows: ", nrow(cluster_buffer_country))
message("  cluster_buffer_tile rows: ", nrow(cluster_buffer_tile))
message("  cluster_buffer_year_defor rows: ", nrow(cluster_buffer_year_defor))
message("  cluster_sites rows: ", nrow(cluster_sites))
message("  cluster_medoids rows: ", nrow(cluster_medoids))
message("  defor_tile_geometry rows: ", nrow(defor_tile_geometry))
