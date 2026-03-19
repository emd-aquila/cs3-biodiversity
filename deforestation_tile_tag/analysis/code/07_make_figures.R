# =====================================================
# Make figures
# =====================================================

plot_list <- list()

# -----------------------
# Load AEZ polygons and define shared color palette
# -----------------------

assert_exists(aez_path)
aez_sf <- read_sf(aez_path)

ov_trend_colors <- c(
  ov_down = "red2",
  ov_flat = "grey50",
  ov_up   = "forestgreen"
)

change_type_colors <- c(
  inverse_change = "forestgreen",
  aligned_change = "red2"
)

# transform later to match cluster CRS if needed

# -----------------------
# Build first-to-last OV trend by cluster
# aka does OV increase or decrease?
# -----------------------

cluster_ov_trend <- cluster_year_ov %>%
  arrange(AEZ, cluster_id, year) %>%
  group_by(AEZ, cluster_id) %>%
  summarise(
    year_first = first(year),
    year_last = last(year),
    ov_first = first(median_ov_year),
    ov_last = last(median_ov_year),
    delta_ov = ov_last - ov_first,
    .groups = "drop"
  ) %>%
  mutate(
    ov_trend = case_when(
      delta_ov > 0.1 ~ "ov_up",
      delta_ov < -0.1 ~ "ov_down",
      TRUE ~ "ov_flat"
    )
  )

# -----------------------
# Build one representative point per cluster
# use the site nearest the medoid
# -----------------------

cluster_points <- cluster_sites %>%
  arrange(AEZ, cluster_id, dist_to_medoid) %>%
  group_by(AEZ, cluster_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(AEZ, cluster_id, sample_id, year, dist_to_medoid)

cluster_points <- cluster_points %>%
  left_join(
    cluster_ov_trend,
    by = c("AEZ", "cluster_id")
  ) %>%
  left_join(
    cluster_tag_status %>%
      select(AEZ, cluster_id, tagged_any_tile, tagged_ha_tile),
    by = c("AEZ", "cluster_id")
  )

# -----------------------
# Build matched tile layers
# -----------------------

matched_tile_ids <- cluster_buffer_tile %>%
  distinct(tile_id)

matched_ha_tile_ids <- cluster_buffer_tile %>%
  filter(has_ha_info) %>%
  distinct(tile_id)

matched_tiles_sf <- defor_tile_geometry %>%
  semi_join(matched_tile_ids, by = "tile_id") %>%
  mutate(tile_status = "matched")

matched_ha_tiles_sf <- defor_tile_geometry %>%
  semi_join(matched_ha_tile_ids, by = "tile_id") %>%
  mutate(tile_status = "ha")

# -----------------------
# Align CRS
# -----------------------

target_crs <- st_crs(cluster_points)

if (st_crs(aez_sf) != target_crs) {
  aez_sf <- st_transform(aez_sf, target_crs)
}

if (st_crs(matched_tiles_sf) != target_crs) {
  matched_tiles_sf <- st_transform(matched_tiles_sf, target_crs)
}

if (st_crs(matched_ha_tiles_sf) != target_crs) {
  matched_ha_tiles_sf <- st_transform(matched_ha_tiles_sf, target_crs)
}

# -----------------------
# Global overview map
# -----------------------

world_map <- map_data("world")

plot_global_map <- ggplot() +
  # world basemap
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "grey95",
    color = "grey80",
    linewidth = 0.2
  ) +
  
  # all deforestation tiles
  geom_sf(
    data = defor_tiles_all_sf,
    aes(fill = has_ha_info),
    color = NA,
    alpha = 0.30,
    inherit.aes = FALSE
  ) +
  
  # Cluster medoid points
  geom_sf(
    data = cluster_points,
    aes(color = ov_trend),
    size = 1.6,
    alpha = 0.9,
    inherit.aes = FALSE
  ) +
  
  # tile coloring
  scale_fill_manual(
    values = c(
      `TRUE` = "darkgreen",
      `FALSE` = "tomato"
    ),
    name = "Tile has\nha info"
  ) +
  scale_color_manual(
    values = ov_trend_colors,
    name = "OV trend"
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Cluster points and deforestation tile coverage",
    subtitle = "Points colored by first-to-last OV change"
  ) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "grey85", linewidth = 0.2)
  )
  
  
plot_list$global_map <- plot_global_map

# -----------------------
# Tagging rate by AEZ
# derive tagging rate from the compact AEZ summary
# -----------------------

cluster_tag_plot <- aez_summary %>%
  mutate(
    pct_tagged_to_ha_tile = dplyr::if_else(
      total_clusters > 0,
      100 * clusters_tagged_to_ha_tile / total_clusters,
      0
    )
  )

plot_tagging_rate <- ggplot(
  cluster_tag_plot,
  aes(x = AEZ, y = pct_tagged_to_ha_tile, group = 1)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Clusters tagged to ha tiles",
    x = "AEZ",
    y = "% tagged"
  ) +
  theme_minimal()

plot_list$tagging_rate <- plot_tagging_rate

# -----------------------
# OV direction counts
# -----------------------

ov_direction_counts <- cluster_ov_trend %>%
  count(AEZ, ov_trend, name = "n_clusters")

plot_ov_direction <- ggplot(
  ov_direction_counts,
  aes(x = AEZ, y = n_clusters, fill = ov_trend)
) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = ov_trend_colors,
    name = "OV trend"
  ) +
  labs(
    title = "OV direction by AEZ",
    x = "AEZ",
    y = "Clusters"
  ) +
  theme_minimal()

plot_list$ov_direction <- plot_ov_direction