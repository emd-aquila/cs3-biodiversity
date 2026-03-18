# =====================================================
# Check buffer sensitivity
# =====================================================

# Goal:
# Summarize whether increasing buffer size changes:
# 1. number of clusters tagged to >=1 tile with ha data
# 2. total number of cluster/tile tags

# -----------------------
# Cluster-level tagging summary by buffer
# -----------------------

buffer_cluster_summary <- cluster_buffer %>%
  st_drop_geometry() %>%
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

# -----------------------
# Total cluster/tile tags by buffer
# -----------------------

buffer_tag_totals <- cluster_buffer_tile %>%
  distinct(AEZ, cluster_id, buffer_km, tile_id, has_ha_info) %>%
  group_by(buffer_km) %>%
  summarise(
    total_cluster_tile_tags = n(),
    total_cluster_ha_tile_tags = sum(has_ha_info, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(buffer_km)

# -----------------------
# Final compact report
# -----------------------

buffer_sensitivity_report <- buffer_cluster_summary %>%
  left_join(buffer_tag_totals, by = "buffer_km") %>%
  arrange(buffer_km)

# -----------------------
# Write report to tmp
# -----------------------

write_csv_safe(
  buffer_sensitivity_report,
  file.path(analysis_tmp_dir, "buffer_sensitivity_report.csv")
)

# -----------------------
# Print to console
# -----------------------

message("Buffer sensitivity report:")
print(buffer_sensitivity_report)


# -----------------------
# Decide the buffer to use based on results: 10km
# 10km gets about 10 more clusters assigned to a tile with ha, and increases
# tile assignment generally across the clusters already assigned 1+.
# -----------------------
target_buffer_km <- 10

cluster_buffer <- cluster_buffer %>%
  filter(buffer_km == target_buffer_km)

cluster_buffer_tile <- cluster_buffer_tile %>%
  filter(buffer_km == target_buffer_km)

cluster_buffer_year_defor <- cluster_buffer_year_defor %>%
  filter(buffer_km == target_buffer_km)

message("Analysis restricted to buffer_km = ", target_buffer_km)