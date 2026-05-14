# =====================================================
# Build canonical cluster-buffer deforestation tables
# Creates:
#   - cluster_buffer.gpkg         : one row per cluster-buffer
#   - matched_clusters_tiles.csv  : one row per cluster-buffer-tile
#   - cluster_year_defor.csv      : one row per cluster-buffer-year
# =====================================================

sf::sf_use_s2(FALSE)

# -----------------------
# Preconditions
# -----------------------

required_objects <- c(
  "cluster_sites",
  "defor_tile_geometry",
  "defor_tile_year",
  "country_geometry",
  "buffer_km_vals",
  "cluster_footprints_cache"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    "Missing required objects for 07_build_cluster_footprints.R: ",
    paste(missing_objects, collapse = ", ")
  )
}

# -----------------------
# Build unified cluster polygons, then buffer them
# one row per AEZ-cluster-buffer
# -----------------------

cluster_buffer <- read_or_build(
  path = cluster_footprints_cache,
  build_fn = function() {
    cluster_polygons <- cluster_sites %>%
      group_by(AEZ, cluster_id) %>%
      summarise(
        n_sites = n(),
        .groups = "drop"
      ) %>%
      st_make_valid() %>%
      mutate(
        geometry = sf::st_convex_hull(geometry)
      )

    purrr::map_dfr(
      buffer_km_vals,
      function(buffer_km) {
        buffer_m <- buffer_km * 1000

        cluster_polygons %>%
          mutate(
            geometry = st_buffer(geometry, dist = buffer_m)
          ) %>%
          st_make_valid() %>%
          mutate(buffer_km = buffer_km) %>%
          relocate(buffer_km, .after = cluster_id)
      }
    )
  }
)

assert_no_duplicate_keys(
  st_drop_geometry(cluster_buffer),
  c("AEZ", "cluster_id", "buffer_km"),
  "cluster_buffer"
)

# -----------------------
# Intersect cluster-buffer polygons with country polygons
# then collapse to one row per unique cluster-buffer-country match
# one row per AEZ-cluster-buffer-country
# -----------------------

cluster_buffer_country <- cluster_buffer %>%
  dplyr::select(AEZ, cluster_id, buffer_km) %>%
  sf::st_intersection(
    country_geometry %>%
      dplyr::select(country_id, country_iso3, country_name, country_name_long, sovereign_name)
  ) %>%
  mutate(
    country_intersection_area_ha = as.numeric(sf::st_area(.)) / 10000
  ) %>%
  filter(is.finite(country_intersection_area_ha), country_intersection_area_ha > 0) %>%
  st_drop_geometry() %>%
  group_by(
    AEZ,
    cluster_id,
    buffer_km,
    country_id,
    country_iso3,
    country_name,
    country_name_long,
    sovereign_name
  ) %>%
  summarise(
    country_intersection_area_ha = sum(country_intersection_area_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(AEZ, cluster_id, buffer_km) %>%
  mutate(
    normalized_country_overlap_share =
      country_intersection_area_ha / sum(country_intersection_area_ha, na.rm = TRUE)
  ) %>%
  ungroup()

assert_no_duplicate_keys(
  cluster_buffer_country,
  c("AEZ", "cluster_id", "buffer_km", "country_id"),
  "cluster_buffer_country"
)

assert_groupwise_share_sum(
  cluster_buffer_country,
  keys = c("AEZ", "cluster_id", "buffer_km"),
  share_col = "normalized_country_overlap_share",
  data_name = "cluster_buffer_country"
)

# -----------------------
# Intersect cluster-buffer polygons with deforestation tiles
# then collapse to one row per unique cluster-buffer-tile match
# one row per AEZ-cluster-buffer-tile
# -----------------------

cluster_buffer_tile <- cluster_buffer %>%
  dplyr::select(AEZ, cluster_id, buffer_km) %>%
  sf::st_intersection(
    defor_tile_geometry %>%
      dplyr::select(tile_id, country_name, has_ha_info)
  ) %>%
  mutate(
    intersection_area_ha = as.numeric(sf::st_area(.)) / 10000
  ) %>%
  filter(is.finite(intersection_area_ha), intersection_area_ha > 0) %>%
  st_drop_geometry() %>%
  group_by(AEZ, cluster_id, buffer_km, tile_id, country_name, has_ha_info) %>%
  summarise(
    intersection_area_ha = sum(intersection_area_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(AEZ, cluster_id, buffer_km) %>%
  mutate(
    normalized_overlap_share = intersection_area_ha / sum(intersection_area_ha, na.rm = TRUE)
  ) %>%
  ungroup()

assert_no_duplicate_keys(
  cluster_buffer_tile,
  c("AEZ", "cluster_id", "buffer_km", "tile_id"),
  "cluster_buffer_tile"
)

assert_groupwise_share_sum(
  cluster_buffer_tile,
  keys = c("AEZ", "cluster_id", "buffer_km"),
  share_col = "normalized_overlap_share",
  data_name = "cluster_buffer_tile"
)

# -----------------------
# Add tagging summaries back to cluster_buffer
# -----------------------

cluster_buffer_tag_summary <- cluster_buffer_tile %>%
  group_by(AEZ, cluster_id, buffer_km) %>%
  summarise(
    n_matched_tiles = n_distinct(tile_id),
    n_matched_tiles_with_ha = n_distinct(tile_id[has_ha_info %in% TRUE]),
    n_matched_tiles_missing_ha = n_distinct(tile_id[has_ha_info %in% FALSE]),
    tagged_any_tile = n_matched_tiles > 0,
    tagged_ha_tile = n_matched_tiles_with_ha > 0,
    .groups = "drop"
  )

cluster_buffer_country_summary <- cluster_buffer_country %>%
  arrange(
    AEZ,
    cluster_id,
    buffer_km,
    desc(normalized_country_overlap_share),
    country_name
  ) %>%
  group_by(AEZ, cluster_id, buffer_km) %>%
  summarise(
    n_matched_countries = n_distinct(country_id),
    primary_country_id = dplyr::first(country_id),
    primary_country_iso3 = dplyr::first(country_iso3),
    primary_country_name = dplyr::first(country_name),
    primary_country_name_long = dplyr::first(country_name_long),
    primary_sovereign_name = dplyr::first(sovereign_name),
    primary_country_overlap_share = dplyr::first(normalized_country_overlap_share),
    matched_country_ids = paste(sort(unique(country_id)), collapse = ";"),
    matched_country_names = paste(sort(unique(country_name)), collapse = ";"),
    .groups = "drop"
  )

cluster_buffer <- cluster_buffer %>%
  left_join(
    cluster_buffer_tag_summary,
    by = c("AEZ", "cluster_id", "buffer_km")
  ) %>%
  left_join(
    cluster_buffer_country_summary,
    by = c("AEZ", "cluster_id", "buffer_km")
  ) %>%
  mutate(
    n_matched_tiles = coalesce(n_matched_tiles, 0L),
    n_matched_tiles_with_ha = coalesce(n_matched_tiles_with_ha, 0L),
    n_matched_tiles_missing_ha = coalesce(n_matched_tiles_missing_ha, 0L),
    tagged_any_tile = coalesce(tagged_any_tile, FALSE),
    tagged_ha_tile = coalesce(tagged_ha_tile, FALSE),
    n_matched_countries = coalesce(n_matched_countries, 0L)
  )

assert_no_duplicate_keys(
  st_drop_geometry(cluster_buffer),
  c("AEZ", "cluster_id", "buffer_km"),
  "cluster_buffer"
)

# -----------------------
# Build cluster-buffer-year deforestation table
# All matched tiles contribute. Tiles absent from the hectare table carry
# zero-valued annual deforestation rows in defor_tile_year.
# one row per AEZ-cluster-buffer-year
# -----------------------

cluster_buffer_year_defor <- cluster_buffer_tile %>%
  dplyr::select(
    AEZ,
    cluster_id,
    buffer_km,
    tile_id,
    has_ha_info,
    normalized_overlap_share
  ) %>%
  left_join(
    defor_tile_year,
    by = "tile_id",
    relationship = "many-to-many"
  ) %>%
  group_by(AEZ, cluster_id, buffer_km, year) %>%
  summarise(
    n_tiles = n_distinct(tile_id),
    n_tiles_with_ha = n_distinct(tile_id[has_ha_info %in% TRUE]),
    defor_ha_total_raw = sum(defor_total_ha, na.rm = TRUE),
    defor_ha_total_avg = defor_ha_total_raw / n_tiles,
    defor_ha_total_rel_pct = sum(defor_total_ha * normalized_overlap_share, na.rm = TRUE),
    defor_ha_crops_raw = sum(defor_crops_ha, na.rm = TRUE),
    defor_ha_crops_avg = defor_ha_crops_raw / n_tiles,
    defor_ha_crops_rel_pct = sum(defor_crops_ha * normalized_overlap_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(AEZ, cluster_id, buffer_km, year)

assert_no_duplicate_keys(
  cluster_buffer_year_defor,
  c("AEZ", "cluster_id", "buffer_km", "year"),
  "cluster_buffer_year_defor"
)

message("Built cluster-buffer tables:")
message("  cluster_buffer rows: ", nrow(cluster_buffer))
message("  cluster_buffer_country rows: ", nrow(cluster_buffer_country))
message("  cluster_buffer_tile rows: ", nrow(cluster_buffer_tile))
message("  cluster_buffer_year_defor rows: ", nrow(cluster_buffer_year_defor))
