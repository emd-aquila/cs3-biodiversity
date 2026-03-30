# =====================================================
# Make figures for the current cluster run and current buffer
# =====================================================

# -----------------------
# Preconditions
# -----------------------

required_objects <- c(
  "current_buffer_km",
  "cluster_year_ov",
  "cluster_sites",
  "defor_tiles_all_sf",
  "defor_tile_geometry",
  "cluster_buffer_tile_this",
  "cluster_tag_status",
  "aez_summary",
  "aez_path"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    paste0(
      "07_make_figures.R is missing required objects: ",
      paste(missing_objects, collapse = ", ")
    ),
    call. = FALSE
  )
}

if (is.na(current_buffer_km)) {
  stop("current_buffer_km is NA. Run set_buffer_output_dirs(buffer_km) first.", call. = FALSE)
}

assert_exists(aez_path)

# -----------------------
# Shared color palette
# -----------------------

ov_trend_colors <- c(
  ov_down = "red2",
  ov_flat = "grey50",
  ov_up   = "forestgreen"
)

# -----------------------
# Build first-to-last OV trend by cluster
# This is not buffer-dependent within a cluster run,
# but is joined to current-buffer tagging status below.
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
# Empty-buffer handling
# -----------------------

if (nrow(cluster_tag_status) == 0 || nrow(aez_summary) == 0) {
  plot_list <- list()
  warning(
    "Current buffer has no tagging or AEZ summary data for ",
    current_buffer_km,
    " km. No figures created."
  )
} else {
  
  # -----------------------
  # AEZ polygons
  # -----------------------
  
  aez_sf <- sf::read_sf(aez_path)
  
  # -----------------------
  # Build one representative point per cluster
  # Use site nearest the medoid
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
  # Build matched tile layers for current buffer
  # -----------------------
  
  matched_tile_ids <- cluster_buffer_tile_this %>%
    distinct(tile_id)
  
  matched_ha_tile_ids <- cluster_buffer_tile_this %>%
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
  
  target_crs <- sf::st_crs(cluster_points)
  
  aez_sf_this <- aez_sf
  if (sf::st_crs(aez_sf_this) != target_crs) {
    aez_sf_this <- sf::st_transform(aez_sf_this, target_crs)
  }
  
  if (sf::st_crs(matched_tiles_sf) != target_crs) {
    matched_tiles_sf <- sf::st_transform(matched_tiles_sf, target_crs)
  }
  
  if (sf::st_crs(matched_ha_tiles_sf) != target_crs) {
    matched_ha_tiles_sf <- sf::st_transform(matched_ha_tiles_sf, target_crs)
  }
  
  # -----------------------
  # Global overview map
  # -----------------------
  
  world_map <- ggplot2::map_data("world")
  
  plot_global_map <- ggplot() +
    geom_polygon(
      data = world_map,
      aes(x = long, y = lat, group = group),
      fill = "grey95",
      color = "grey80",
      linewidth = 0.2
    ) +
    geom_sf(
      data = defor_tiles_all_sf,
      aes(fill = has_ha_info),
      color = NA,
      alpha = 0.30,
      inherit.aes = FALSE
    ) +
    geom_sf(
      data = cluster_points,
      aes(color = ov_trend),
      size = 1.6,
      alpha = 0.9,
      inherit.aes = FALSE
    ) +
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
      subtitle = paste(
        "Points colored by first-to-last OV change | Buffer =",
        current_buffer_km,
        "km"
      )
    ) +
    theme(
      legend.position = "right",
      panel.grid.major = element_line(color = "grey85", linewidth = 0.2)
    )
  
  # -----------------------
  # Tagging rate by AEZ
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
      subtitle = paste("Buffer =", current_buffer_km, "km"),
      x = "AEZ",
      y = "% tagged"
    ) +
    theme_minimal()
  
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
      subtitle = paste("Buffer =", current_buffer_km, "km"),
      x = "AEZ",
      y = "Clusters"
    ) +
    theme_minimal()
  
  # -----------------------
  # Collect figure objects
  # -----------------------
  
  plot_list <- list(
    global_map = plot_global_map,
    tagging_rate = plot_tagging_rate,
    ov_direction = plot_ov_direction
  )
}

message("Finished 07_make_figures.R")
if (exists("current_cluster_stub")) {
  message("  cluster run: ", current_cluster_stub)
}
message("  buffer_km: ", current_buffer_km)