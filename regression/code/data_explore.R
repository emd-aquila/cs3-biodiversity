# =====================================================
# data_explore.R
# Exploratory plots for one selected regression run
# =====================================================

message("Starting data exploration...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

# -----------------------
# User settings
# -----------------------

cluster_method_target <- "clara"
cluster_radius_km_target <- 12.5
buffer_km_target <- 1
defor_transform_target <- "raw"

focus_aez_map <- 5
save_outputs <- TRUE

# -----------------------
# Set current run paths
# -----------------------

set_regression_run_paths(
  cluster_method = cluster_method_target,
  cluster_radius_km = cluster_radius_km_target,
  buffer_km = buffer_km_target,
  defor_transform = defor_transform_target
)

message("Sourcing 03_load_data.R")
source("03_load_data.R")

# -----------------------
# Build canonical paths for current cluster run
# -----------------------

cluster_sites_path <- file.path(canonical_spatial_dir, "cluster_sites.gpkg")
defor_tile_geometry_path <- file.path(canonical_spatial_dir, "defor_tile_geometry.gpkg")
cluster_buffer_tile_path <- file.path(canonical_tabular_dir, "cluster_buffer_tile.csv")
aez_path <- file.path(repo_root, "spatial_data", "aez", "AEZ_shp_file.shp")

explore_output_dir <- file.path(
  "..",
  "data_explore",
  current_buffer_stub,
  current_cluster_stub,
  paste0("defor_", current_defor_transform)
)

dir.create(explore_output_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------
# Helpers
# -----------------------

is_focus_aez <- function(x, focus_aez_number) {
  readr::parse_number(as.character(x)) == focus_aez_number
}

save_plot_if_requested <- function(plot_obj, filename, width = 11, height = 8, dpi = 300) {
  if (isTRUE(save_outputs)) {
    ggplot2::ggsave(
      filename = file.path(explore_output_dir, filename),
      plot = plot_obj,
      width = width,
      height = height,
      dpi = dpi
    )
    message("Wrote: ", file.path(explore_output_dir, filename))
  }
}

# -----------------------
# Read inputs
# -----------------------

walk(
  c(cluster_deltas_path, cluster_sites_path, defor_tile_geometry_path, cluster_buffer_tile_path, aez_path),
  assert_exists
)

cluster_deltas_explore <- readr::read_csv(cluster_deltas_path, show_col_types = FALSE) %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id)
  )

cluster_sites <- sf::read_sf(cluster_sites_path) %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id)
  )

defor_tile_geometry <- sf::read_sf(defor_tile_geometry_path)

cluster_buffer_tile <- readr::read_csv(cluster_buffer_tile_path, show_col_types = FALSE) %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id),
    buffer_km = as.numeric(buffer_km),
    tile_id = as.character(tile_id)
  )

aez_sf <- sf::read_sf(aez_path) %>%
  mutate(
    AEZ = standardize_aez_order(AEZ)
  )

# -----------------------
# Choose x-axis variable for scatterplots
# -----------------------

x_col <- dplyr::case_when(
  current_defor_transform == "raw" ~ "delta_defor_ha",
  current_defor_transform == "log1p" ~ "log1p_delta_defor_ha",
  TRUE ~ NA_character_
)

if (is.na(x_col)) {
  stop("Unknown current_defor_transform: ", current_defor_transform, call. = FALSE)
}

if (!"log1p_delta_defor_ha" %in% names(cluster_deltas_explore)) {
  cluster_deltas_explore <- cluster_deltas_explore %>%
    mutate(
      log1p_delta_defor_ha = log1p(delta_defor_ha)
    )
}

# -----------------------
# 1) AEZ5 clusters + tiles map
# -----------------------

cluster_summary_focus_aez <- cluster_deltas_explore %>%
  filter(is_focus_aez(AEZ, focus_aez_map)) %>%
  group_by(AEZ, cluster_id) %>%
  summarise(
    n_pairs = n(),
    mean_delta_ov = mean(delta_ov, na.rm = TRUE),
    median_delta_ov = median(delta_ov, na.rm = TRUE),
    mean_delta_defor_ha = mean(delta_defor_ha, na.rm = TRUE),
    share_positive_delta_ov = mean(delta_ov > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ov_direction = case_when(
      mean_delta_ov > 0 ~ "positive",
      mean_delta_ov < 0 ~ "negative",
      TRUE ~ "zero"
    )
  )

focus_cluster_points <- cluster_sites %>%
  filter(is_focus_aez(AEZ, focus_aez_map)) %>%
  arrange(AEZ, cluster_id, dist_to_medoid) %>%
  group_by(AEZ, cluster_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(AEZ, cluster_id, sample_id, year, dist_to_medoid) %>%
  left_join(cluster_summary_focus_aez, by = c("AEZ", "cluster_id"))

focus_tile_ids <- cluster_buffer_tile %>%
  filter(
    buffer_km == buffer_km_target,
    is_focus_aez(AEZ, focus_aez_map),
    cluster_id %in% focus_cluster_points$cluster_id
  ) %>%
  distinct(tile_id)

focus_tiles_sf <- defor_tile_geometry %>%
  semi_join(focus_tile_ids, by = "tile_id")

focus_aez_sf <- aez_sf %>%
  filter(is_focus_aez(AEZ, focus_aez_map))

target_crs <- sf::st_crs(focus_cluster_points)

if (sf::st_crs(focus_aez_sf) != target_crs) {
  focus_aez_sf <- sf::st_transform(focus_aez_sf, target_crs)
}

if (sf::st_crs(focus_tiles_sf) != target_crs) {
  focus_tiles_sf <- sf::st_transform(focus_tiles_sf, target_crs)
}

## PLOTTING WORLD MAP
world_map <- map_data("world")

focus_cluster_points <- st_transform(focus_cluster_points, 4326)
focus_tiles_sf <- st_transform(focus_tiles_sf, 4326)
focus_aez_sf <- st_transform(focus_aez_sf, 4326)  

plot_aez5_clusters_tiles <- ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "grey95",
    color = "grey60",
    linewidth = 0.2
  ) +
  geom_sf(
    data = focus_tiles_sf,
    fill = "grey70",
    color = NA,
    alpha = 0.35,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = focus_cluster_points,
    aes(color = mean_delta_ov, size = n_pairs),
    alpha = 0.95,
    inherit.aes = FALSE
  ) +
  scale_color_gradient2(
    low = "firebrick3",
    mid = "grey70",
    high = "forestgreen",
    midpoint = 0,
    name = "Mean delta OV"
  ) +
  scale_size_continuous(name = "N pairs") +
  labs(
    title = paste0("AEZ", focus_aez_map, ": clusters and matched tiles"),
    subtitle = paste0(
      "Method = ", current_cluster_method,
      " | Radius = ", sprintf("%.1f", current_cluster_radius_km), " km",
      " | Buffer = ", buffer_km_target, " km"
    ),
    x = NULL,
    y = NULL
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-30, 30),
    expand = FALSE
  ) +
  theme_minimal()

print(plot_aez5_clusters_tiles)

save_plot_if_requested(
  plot_aez5_clusters_tiles,
  paste0("aez", focus_aez_map, "_clusters_tiles.png"),
  width = 10,
  height = 8
)

readr::write_csv(
  sf::st_drop_geometry(focus_cluster_points) %>%
    arrange(desc(mean_delta_ov)),
  file.path(explore_output_dir, paste0("aez", focus_aez_map, "_cluster_summary.csv"))
)

# -----------------------
# 2) AEZ-specific regression plots colored by cluster_id
# -----------------------

regression_plot_data <- cluster_deltas_explore %>%
  filter(
    !is.na(AEZ),
    !is.na(.data[[x_col]]),
    !is.na(delta_ov)
  ) %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id)
  )

aez_values_to_plot <- regression_plot_data %>%
  distinct(AEZ) %>%
  pull(AEZ) %>%
  as.character()

for (aez_value in aez_values_to_plot) {
  regression_plot_data_aez <- regression_plot_data %>%
    filter(as.character(AEZ) == aez_value) %>%
    mutate(cluster_id = forcats::fct_inorder(cluster_id))
  
  if (nrow(regression_plot_data_aez) == 0) {
    next
  }
  
  plot_regression_single_aez_colored_cluster <- ggplot(
    regression_plot_data_aez,
    aes(
      x = .data[[x_col]],
      y = delta_ov,
      color = cluster_id
    )
  ) +
    geom_point(alpha = 0.85, size = 2.1) +
    geom_smooth(
      data = regression_plot_data_aez,
      mapping = aes(
        x = .data[[x_col]],
        y = delta_ov
      ),
      method = "lm",
      se = FALSE,
      color = "black",
      linewidth = 0.7,
      inherit.aes = FALSE
    ) +
    labs(
      title = paste0("AEZ ", aez_value, ": delta OV vs deforestation"),
      subtitle = paste0(
        "Method = ", current_cluster_method,
        " | Radius = ", sprintf("%.1f", current_cluster_radius_km), " km",
        " | Buffer = ", buffer_km_target, " km",
        " | Transform = ", current_defor_transform
      ),
      x = x_col,
      y = "delta_ov",
      color = "Cluster ID"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9)
    )
  
  print(plot_regression_single_aez_colored_cluster)
  
  save_plot_if_requested(
    plot_regression_single_aez_colored_cluster,
    paste0("aez_", readr::parse_number(aez_value), "_regression_colored_cluster.png"),
    width = 11,
    height = 8
  )
}

message("Finished data_explore.R")
message("  output dir: ", explore_output_dir)