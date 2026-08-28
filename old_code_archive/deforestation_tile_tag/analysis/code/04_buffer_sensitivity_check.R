# =====================================================
# Check buffer sensitivity for the current cluster run
# =====================================================

required_objects <- c(
  "cluster_buffer",
  "cluster_buffer_tile",
  "current_cluster_method",
  "current_cluster_radius_km",
  "current_cluster_stub"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    paste0(
      "04_buffer_sensitivity_check.R is missing required objects: ",
      paste(missing_objects, collapse = ", ")
    ),
    call. = FALSE
  )
}

buffer_cluster_summary <- cluster_buffer %>%
  sf::st_drop_geometry() %>%
  distinct(
    AEZ,
    cluster_id,
    buffer_km,
    tagged_any_tile,
    tagged_ha_tile,
    n_matched_tiles,
    n_matched_tiles_with_ha
  ) %>%
  group_by(buffer_km) %>%
  summarise(
    n_clusters = n(),
    n_tagged_any_tile = sum(tagged_any_tile, na.rm = TRUE),
    n_tagged_ha_tile = sum(tagged_ha_tile, na.rm = TRUE),
    pct_tagged_any_tile = 100 * n_tagged_any_tile / n_clusters,
    pct_tagged_ha_tile = 100 * n_tagged_ha_tile / n_clusters,
    total_matched_tiles = sum(n_matched_tiles, na.rm = TRUE),
    total_matched_ha_tiles = sum(n_matched_tiles_with_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(buffer_km)

buffer_tag_totals <- cluster_buffer_tile %>%
  distinct(AEZ, cluster_id, buffer_km, tile_id, has_ha_info) %>%
  group_by(buffer_km) %>%
  summarise(
    total_cluster_tile_tags = n(),
    total_cluster_ha_tile_tags = sum(has_ha_info, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(buffer_km)

buffer_sensitivity_report_current <- buffer_cluster_summary %>%
  left_join(buffer_tag_totals, by = "buffer_km") %>%
  mutate(
    cluster_method = current_cluster_method,
    cluster_radius_km = current_cluster_radius_km,
    cluster_stub = current_cluster_stub,
    .before = 1
  ) %>%
  arrange(buffer_km)

message("Finished 04_buffer_sensitivity_check.R")
message("  cluster run: ", current_cluster_stub)
print(buffer_sensitivity_report_current)