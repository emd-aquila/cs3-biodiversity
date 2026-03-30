# =====================================================
# Build canonical cluster-buffer deforestation tables
# Creates:
#   - cluster_buffer             : one row per cluster-buffer
#   - cluster_buffer_tile        : one row per cluster-buffer-tile
#   - cluster_buffer_year_defor  : one row per cluster-buffer-year
# =====================================================

sf::sf_use_s2(FALSE)

# -----------------------
# Preconditions
# -----------------------

required_objects <- c(
  "cluster_sites",
  "defor_tile_geometry",
  "defor_tile_attributes",
  "defor_tile_year",
  "buffer_km_vals",
  "cluster_footprints_cache"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    "Missing required objects for 07_build_cluster_footprints_tag_defor.R: ",
    paste(missing_objects, collapse = ", ")
  )
}

# -----------------------
# Build buffered cluster footprints
# one row per AEZ-cluster-buffer
# -----------------------

cluster_buffer <- read_or_build(
  path = cluster_footprints_cache,
  build_fn = function() {
    purrr::map_dfr(
      buffer_km_vals,
      function(buffer_km) {
        buffer_m <- buffer_km * 1000
        
        cluster_sites %>%
          mutate(buffer_geom = st_buffer(geometry, dist = buffer_m)) %>%
          st_set_geometry("buffer_geom") %>%
          group_by(AEZ, cluster_id) %>%
          summarise(
            n_sites = n(),
            .groups = "drop"
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
# Spatial match: cluster-buffer to tile polygons
# then collapse to unique tile_id matches
# one row per AEZ-cluster-buffer-tile
# -----------------------

cluster_buffer_tile <- cluster_buffer %>%
  select(AEZ, cluster_id, buffer_km) %>%
  st_join(
    defor_tile_geometry,
    join = st_intersects,
    left = TRUE
  ) %>%
  st_drop_geometry() %>%
  filter(!is.na(tile_id)) %>%
  distinct(AEZ, cluster_id, buffer_km, tile_id) %>%
  left_join(defor_tile_attributes, by = "tile_id")

assert_no_duplicate_keys(
  cluster_buffer_tile,
  c("AEZ", "cluster_id", "buffer_km", "tile_id"),
  "cluster_buffer_tile"
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

cluster_buffer <- cluster_buffer %>%
  left_join(
    cluster_buffer_tag_summary,
    by = c("AEZ", "cluster_id", "buffer_km")
  ) %>%
  mutate(
    n_matched_tiles = coalesce(n_matched_tiles, 0L),
    n_matched_tiles_with_ha = coalesce(n_matched_tiles_with_ha, 0L),
    n_matched_tiles_missing_ha = coalesce(n_matched_tiles_missing_ha, 0L),
    tagged_any_tile = coalesce(tagged_any_tile, FALSE),
    tagged_ha_tile = coalesce(tagged_ha_tile, FALSE)
  )

assert_no_duplicate_keys(
  st_drop_geometry(cluster_buffer),
  c("AEZ", "cluster_id", "buffer_km"),
  "cluster_buffer"
)

# -----------------------
# Build cluster-buffer-year deforestation table
# only tiles with hectare info contribute here
# one row per AEZ-cluster-buffer-year
# -----------------------

cluster_buffer_year_defor <- cluster_buffer_tile %>%
  filter(has_ha_info %in% TRUE) %>%
  select(AEZ, cluster_id, buffer_km, tile_id) %>%
  left_join(
    defor_tile_year,
    by = "tile_id",
    relationship = "many-to-many"
  ) %>%
  group_by(AEZ, cluster_id, buffer_km, year) %>%
  summarise(
    defor_total_ha_year = sum(defor_total_ha, na.rm = TRUE),
    n_tiles_with_ha = n_distinct(tile_id),
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
message("  cluster_buffer_tile rows: ", nrow(cluster_buffer_tile))
message("  cluster_buffer_year_defor rows: ", nrow(cluster_buffer_year_defor))