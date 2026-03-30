# =====================================================
# data_explore.R
# Exploratory plots for cluster_deltas outputs from deforestation_tile_tag
# =====================================================

library(tidyverse)
library(sf)
library(here)
library(scales)

# -----------------------
# User settings
# -----------------------

buffer_km_target <- 10
focus_aez_number <- 5
save_outputs <- TRUE

# -----------------------
# Paths
# Assumes working directory is regression/code/
# -----------------------

repo_root <- file.path("..", "..")
defor_tag_dir <- file.path(repo_root, "deforestation_tile_tag")

analysis_output_dir <- file.path(defor_tag_dir, "analysis", "output")
buffer_dir <- file.path(analysis_output_dir, paste0("buf_", buffer_km_target, "km"))
analysis_tables_dir <- file.path(buffer_dir, "tables")

build_output_dir <- file.path(defor_tag_dir, "build", "output")
canonical_tabular_dir <- file.path(build_output_dir, "canonical_tabular")
canonical_spatial_dir <- file.path(build_output_dir, "canonical_spatial")

aez_path <- file.path(repo_root, "spatial_data", "aez", "AEZ_shp_file.shp")

cluster_deltas_path <- file.path(analysis_tables_dir, "cluster_deltas.csv")
cluster_sites_path <- file.path(canonical_spatial_dir, "cluster_sites.gpkg")
defor_tile_geometry_path <- file.path(canonical_spatial_dir, "defor_tile_geometry.gpkg")
cluster_buffer_tile_path <- file.path(canonical_tabular_dir, "cluster_buffer_tile.csv")

explore_output_dir <- file.path(repo_root, "regression", "output", "data_explore", paste0("buf_", buffer_km_target, "km"))
dir.create(explore_output_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------
# Helpers
# -----------------------

assert_exists <- function(path) {
  if (!file.exists(path)) {
    stop("Missing required file: ", path, call. = FALSE)
  }
}

standardize_aez_order <- function(x) {
  x_chr <- as.character(x)
  x_num <- readr::parse_number(x_chr)
  factor(x_chr, levels = unique(x_chr[order(x_num, x_chr)]))
}

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
  }
}

# -----------------------
# Read inputs
# -----------------------

walk(
  c(cluster_deltas_path, cluster_sites_path, defor_tile_geometry_path, cluster_buffer_tile_path, aez_path),
  assert_exists
)

cluster_deltas <- readr::read_csv(cluster_deltas_path, show_col_types = FALSE) %>%
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
  mutate(AEZ = standardize_aez_order(AEZ))

# -----------------------
# 1) AEZ5 clusters + tiles map
# -----------------------

cluster_summary_focus_aez <- cluster_deltas %>%
  filter(is_focus_aez(AEZ, focus_aez_number)) %>%
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
  filter(is_focus_aez(AEZ, focus_aez_number)) %>%
  arrange(AEZ, cluster_id, dist_to_medoid) %>%
  group_by(AEZ, cluster_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(AEZ, cluster_id, sample_id, year, dist_to_medoid) %>%
  left_join(cluster_summary_focus_aez, by = c("AEZ", "cluster_id"))

focus_tile_ids <- cluster_buffer_tile %>%
  filter(
    buffer_km == buffer_km_target,
    is_focus_aez(AEZ, focus_aez_number),
    cluster_id %in% focus_cluster_points$cluster_id
  ) %>%
  distinct(tile_id)

focus_tiles_sf <- defor_tile_geometry %>%
  semi_join(focus_tile_ids, by = "tile_id")

focus_aez_sf <- aez_sf %>%
  filter(is_focus_aez(AEZ, focus_aez_number))

target_crs <- st_crs(focus_cluster_points)
if (st_crs(focus_aez_sf) != target_crs) {
  focus_aez_sf <- st_transform(focus_aez_sf, target_crs)
}
if (st_crs(focus_tiles_sf) != target_crs) {
  focus_tiles_sf <- st_transform(focus_tiles_sf, target_crs)
}

plot_aez_focus_clusters_tiles <- ggplot() +
  geom_sf(data = focus_aez_sf, fill = NA, color = "grey35", linewidth = 0.4) +
  geom_sf(data = focus_tiles_sf, fill = "grey70", color = NA, alpha = 0.35) +
  geom_sf(
    data = focus_cluster_points,
    aes(color = mean_delta_ov, size = n_pairs),
    alpha = 0.95
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
    title = paste0("AEZ", focus_aez_number, ": clusters and matched tiles"),
    subtitle = paste0("Buffer = ", buffer_km_target, " km; points colored by mean cluster-level delta OV"),
    x = NULL,
    y = NULL
  ) +
  theme_minimal()

print(plot_aez_focus_clusters_tiles)
save_plot_if_requested(plot_aez_focus_clusters_tiles, paste0("aez", focus_aez_number, "_clusters_tiles.png"), width = 10, height = 8)

if (isTRUE(save_outputs)) {
  readr::write_csv(
    st_drop_geometry(focus_cluster_points) %>% arrange(desc(mean_delta_ov)),
    file.path(explore_output_dir, paste0("aez", focus_aez_number, "_cluster_summary.csv"))
  )
}

# -----------------------
# 2) Separate AEZ regression plots colored by cluster_id
# -----------------------

regression_plot_data <- cluster_deltas %>%
  filter(!is.na(AEZ), !is.na(delta_defor_ha), !is.na(delta_ov)) %>%
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
    aes(x = delta_defor_ha, y = delta_ov, color = cluster_id)
  ) +
    geom_point(alpha = 0.85, size = 2.1) +
    geom_smooth(
      data = regression_plot_data_aez,
      mapping = aes(x = delta_defor_ha, y = delta_ov),
      method = "lm",
      se = FALSE,
      color = "black",
      linewidth = 0.7,
      inherit.aes = FALSE
    ) +
    labs(
      title = paste0("AEZ ", aez_value, ": delta OV vs delta deforestation"),
      subtitle = paste0("Buffer = ", buffer_km_target, " km; colors identify cluster_id; black line = AEZ-level OLS"),
      x = "Delta deforestation (ha)",
      y = "Delta OV",
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
