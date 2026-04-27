# =====================================================
# data_explore.R
# AEZ-focused spatial diagnostics for one selected cluster run.
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

data_explore_dir <- file.path(output_dir, "data_explore")
cluster_method_target <- "clara"
cluster_radius_km_target <- 10.0
buffer_km_targets <- c(1, 10)
target_aez_values <- NULL
save_outputs <- TRUE

# -----------------------
# Set current run paths
# -----------------------

set_regression_scale("non_annualized")
set_regression_grouping_level("aez")
set_defor_exposure_mode("baseline")
set_delta_ov_approach("ov_year_pair")
set_defor_approach("defor_tile_total_ha_total")

set_regression_run_paths(
  cluster_method = cluster_method_target,
  cluster_radius_km = cluster_radius_km_target,
  buffer_km = max(buffer_km_targets)
)

explore_root_dir <- file.path(
  data_explore_dir,
  cluster_method_target,
  paste0("radius_", sprintf("%.1fkm", cluster_radius_km_target))
)

dir.create(explore_root_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------
# Helpers
# -----------------------

save_plot_if_requested <- function(plot_obj,
                                   filename,
                                   output_dir = explore_root_dir,
                                   width = 14,
                                   height = 7,
                                   dpi = 300) {
  if (isTRUE(save_outputs)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    plot_path <- file.path(output_dir, filename)
    ggplot2::ggsave(
      filename = plot_path,
      plot = plot_obj,
      width = width,
      height = height,
      dpi = dpi
    )
    message("Wrote: ", plot_path)
  }
}

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

replicate_sf_by_buffer <- function(sf_obj, buffer_vals) {
  purrr::map_dfr(
    buffer_vals,
    ~ sf_obj %>% mutate(buffer_km = .x),
    .id = NULL
  )
}

build_panel_label <- function(buffer_km) {
  paste0(buffer_km, " km buffer")
}

build_cluster_plot_path <- function(buffer_km) {
  file.path(current_aez_output_dir, paste0("buf_", buffer_km, "km"))
}

sanitize_cluster_id <- function(cluster_id) {
  gsub("[^A-Za-z0-9_-]", "_", cluster_id)
}

sanitize_aez_id <- function(aez_value) {
  tolower(gsub("[^A-Za-z0-9_-]", "", aez_value))
}

build_aez_output_dir <- function(aez_value) {
  file.path(
    explore_root_dir,
    paste0(sanitize_aez_id(aez_value), "_cluster_tile_diagnostic")
  )
}

compute_bbox_with_padding <- function(sf_obj, pad_fraction = 0.20, min_pad = 0.15) {
  bbox <- sf::st_bbox(sf_obj)
  x_range <- bbox$xmax - bbox$xmin
  y_range <- bbox$ymax - bbox$ymin
  x_pad <- max(min_pad, x_range * pad_fraction)
  y_pad <- max(min_pad, y_range * pad_fraction)

  list(
    xlim = c(bbox$xmin - x_pad, bbox$xmax + x_pad),
    ylim = c(bbox$ymin - y_pad, bbox$ymax + y_pad)
  )
}

# -----------------------
# Build canonical paths for current cluster run
# -----------------------

cluster_sites_path <- file.path(canonical_spatial_dir, "cluster_sites.gpkg")
cluster_buffer_path <- file.path(canonical_spatial_dir, "cluster_buffer.gpkg")
defor_tile_geometry_path <- file.path(canonical_spatial_dir, "defor_tile_geometry.gpkg")
cluster_buffer_tile_path <- file.path(canonical_tabular_dir, "matched_clusters_tiles.csv")
aez_path <- file.path(repo_root, "spatial_data", "aez", "AEZ_shp_file.shp")
sf_stage_dir <- file.path(explore_root_dir, "tmp_sf_stage")

walk(
  c(
    cluster_sites_path,
    cluster_buffer_path,
    defor_tile_geometry_path,
    cluster_buffer_tile_path,
    aez_path
  ),
  assert_exists
)

# -----------------------
# Read and harmonize inputs
# -----------------------

cluster_sites <- read_sf_safely(cluster_sites_path, sf_stage_dir) %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id),
    dist_to_medoid = as.numeric(dist_to_medoid)
  )

cluster_buffer <- read_sf_safely(cluster_buffer_path, sf_stage_dir) %>%
  mutate(
    AEZ = standardize_aez_order(AEZ),
    cluster_id = as.character(cluster_id),
    buffer_km = as.numeric(buffer_km)
  )

defor_tile_geometry <- read_sf_safely(defor_tile_geometry_path, sf_stage_dir) %>%
  mutate(
    tile_id = as.character(tile_id)
  )

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
# Restrict to requested AEZs and buffers
# -----------------------

world_map <- ggplot2::map_data("world") %>%
  tidyr::crossing(
    tibble(
      buffer_km = buffer_km_targets,
      buffer_label = factor(build_panel_label(buffer_km_targets), levels = build_panel_label(buffer_km_targets))
    )
  )

available_aez_values <- cluster_buffer %>%
  st_drop_geometry() %>%
  filter(buffer_km %in% buffer_km_targets) %>%
  distinct(AEZ) %>%
  pull(AEZ) %>%
  as.character() %>%
  sort()

focus_aez_values <- if (is.null(target_aez_values)) {
  available_aez_values
} else {
  intersect(as.character(target_aez_values), available_aez_values)
}

if (length(focus_aez_values) == 0) {
  stop("No AEZ values available for the requested run and buffers.", call. = FALSE)
}

purrr::walk(
  focus_aez_values,
  function(focus_aez_value) {
    current_aez_output_dir <<- build_aez_output_dir(focus_aez_value)
    dir.create(current_aez_output_dir, recursive = TRUE, showWarnings = FALSE)

    focus_aez_sf <- aez_sf %>%
      filter(as.character(AEZ) == focus_aez_value)

    if (nrow(focus_aez_sf) == 0) {
      warning("Skipping ", focus_aez_value, ": no AEZ polygon found.")
      return(invisible(NULL))
    }

    focus_cluster_points <- cluster_sites %>%
      filter(as.character(AEZ) == focus_aez_value) %>%
      arrange(cluster_id, dist_to_medoid) %>%
      group_by(cluster_id) %>%
      slice(1) %>%
      ungroup() %>%
      dplyr::select(AEZ, cluster_id, sample_id, year, dist_to_medoid)

    focus_cluster_buffers <- cluster_buffer %>%
      filter(
        as.character(AEZ) == focus_aez_value,
        buffer_km %in% buffer_km_targets
      ) %>%
      dplyr::select(AEZ, cluster_id, buffer_km, n_sites, n_matched_tiles, tagged_ha_tile)

    focus_matched_tile_ids <- cluster_buffer_tile %>%
      filter(
        as.character(AEZ) == focus_aez_value,
        buffer_km %in% buffer_km_targets
      ) %>%
      distinct(buffer_km, tile_id)

    focus_matched_tiles <- defor_tile_geometry %>%
      inner_join(
        focus_matched_tile_ids,
        by = "tile_id",
        relationship = "many-to-many"
      )

    if (nrow(focus_cluster_points) == 0 || nrow(focus_cluster_buffers) == 0 || nrow(focus_matched_tiles) == 0) {
      warning("Skipping ", focus_aez_value, ": incomplete cluster/tile data for requested buffers.")
      return(invisible(NULL))
    }

    focus_aez_sf <- sf::st_transform(focus_aez_sf, 4326)
    focus_cluster_points <- sf::st_transform(focus_cluster_points, 4326)
    focus_cluster_buffers <- sf::st_transform(focus_cluster_buffers, 4326)
    focus_matched_tiles <- sf::st_transform(focus_matched_tiles, 4326)

    focus_cluster_points <- replicate_sf_by_buffer(
      focus_cluster_points,
      buffer_km_targets
    ) %>%
      mutate(buffer_label = factor(build_panel_label(buffer_km), levels = build_panel_label(buffer_km_targets)))

    focus_aez_sf <- replicate_sf_by_buffer(
      focus_aez_sf,
      buffer_km_targets
    ) %>%
      mutate(buffer_label = factor(build_panel_label(buffer_km), levels = build_panel_label(buffer_km_targets)))

    focus_cluster_buffers <- focus_cluster_buffers %>%
      mutate(buffer_label = factor(build_panel_label(buffer_km), levels = build_panel_label(buffer_km_targets)))

    focus_matched_tiles <- focus_matched_tiles %>%
      mutate(buffer_label = factor(build_panel_label(buffer_km), levels = build_panel_label(buffer_km_targets)))

    focus_bbox <- sf::st_bbox(focus_aez_sf)
    x_range <- focus_bbox$xmax - focus_bbox$xmin
    y_range <- focus_bbox$ymax - focus_bbox$ymin
    x_pad <- max(1, x_range * 0.15)
    y_pad <- max(1, y_range * 0.15)

    plot_aez_cluster_tile_diagnostic <- ggplot() +
      geom_polygon(
        data = world_map,
        aes(x = long, y = lat, group = group),
        fill = "grey96",
        color = "grey82",
        linewidth = 0.15
      ) +
      geom_sf(
        data = focus_aez_sf,
        fill = "grey92",
        color = "grey45",
        linewidth = 0.5,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = focus_matched_tiles,
        aes(fill = "Matched deforestation tile"),
        color = "grey35",
        linewidth = 0.15,
        alpha = 0.45,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = focus_cluster_buffers,
        aes(color = "Cluster buffer"),
        fill = NA,
        linewidth = 0.35,
        alpha = 0.85,
        inherit.aes = FALSE
      ) +
      geom_sf(
        data = focus_cluster_points,
        aes(shape = "Cluster point"),
        color = "black",
        fill = "goldenrod1",
        size = 1.9,
        stroke = 0.35,
        inherit.aes = FALSE
      ) +
      scale_fill_manual(
        values = c("Matched deforestation tile" = "#4c78a8"),
        name = NULL
      ) +
      scale_color_manual(
        values = c("Cluster buffer" = "#f58518"),
        name = NULL
      ) +
      scale_shape_manual(
        values = c("Cluster point" = 21),
        name = NULL
      ) +
      coord_sf(
        xlim = c(focus_bbox$xmin - x_pad, focus_bbox$xmax + x_pad),
        ylim = c(focus_bbox$ymin - y_pad, focus_bbox$ymax + y_pad),
        expand = FALSE
      ) +
      facet_wrap(~ buffer_label) +
      labs(
        title = paste0(focus_aez_value, " cluster-buffer and matched deforestation tile diagnostic"),
        subtitle = paste0(
          "Method = ", cluster_method_target,
          " | Radius = ", sprintf("%.1f", cluster_radius_km_target), " km",
          " | Geographic context shown with world outline and AEZ boundary"
        ),
        x = NULL,
        y = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(color = "grey88", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.box = "horizontal"
      )

    save_plot_if_requested(
      plot_aez_cluster_tile_diagnostic,
      paste0(sanitize_aez_id(focus_aez_value), "_cluster_tile_diagnostic_compare_buffers.png"),
      output_dir = current_aez_output_dir
    )

    purrr::walk(
      buffer_km_targets,
      function(buffer_km_value) {
        cluster_points_buffer <- focus_cluster_points %>%
          filter(buffer_km == buffer_km_value)

        cluster_buffers_buffer <- focus_cluster_buffers %>%
          filter(buffer_km == buffer_km_value)

        matched_tiles_buffer <- focus_matched_tiles %>%
          filter(buffer_km == buffer_km_value)

        cluster_ids_buffer <- cluster_buffers_buffer %>%
          st_drop_geometry() %>%
          distinct(cluster_id) %>%
          pull(cluster_id)

        cluster_output_dir <- build_cluster_plot_path(buffer_km_value)

        purrr::walk(
          cluster_ids_buffer,
          function(cluster_id_value) {
            cluster_point_this <- cluster_points_buffer %>%
              filter(cluster_id == cluster_id_value)

            cluster_buffer_this <- cluster_buffers_buffer %>%
              filter(cluster_id == cluster_id_value)

            tile_ids_this <- cluster_buffer_tile %>%
              filter(
                as.character(AEZ) == focus_aez_value,
                buffer_km == buffer_km_value,
                cluster_id == cluster_id_value
              ) %>%
              distinct(tile_id)

            matched_tiles_this <- matched_tiles_buffer %>%
              semi_join(tile_ids_this, by = "tile_id")

            if (nrow(cluster_point_this) == 0 || nrow(cluster_buffer_this) == 0) {
              return(invisible(NULL))
            }

            zoom_geom <- c(
              sf::st_geometry(cluster_buffer_this),
              sf::st_geometry(cluster_point_this)
            )

            if (nrow(matched_tiles_this) > 0) {
              zoom_geom <- c(zoom_geom, sf::st_geometry(matched_tiles_this))
            }

            zoom_bbox <- compute_bbox_with_padding(
              sf::st_as_sf(tibble::tibble(id = seq_along(zoom_geom)), geometry = sf::st_sfc(zoom_geom, crs = 4326))
            )

            plot_cluster_diagnostic <- ggplot() +
              geom_polygon(
                data = world_map %>% filter(buffer_km == buffer_km_value),
                aes(x = long, y = lat, group = group),
                fill = "grey96",
                color = "grey82",
                linewidth = 0.15
              ) +
              geom_sf(
                data = focus_aez_sf %>% filter(buffer_km == buffer_km_value),
                aes(color = "AEZ boundary"),
                fill = "grey92",
                linewidth = 0.45,
                inherit.aes = FALSE
              ) +
              geom_sf(
                data = matched_tiles_this,
                aes(fill = "Matched deforestation tile"),
                color = "grey30",
                linewidth = 0.18,
                alpha = 0.45,
                inherit.aes = FALSE
              ) +
              geom_sf(
                data = cluster_buffer_this,
                aes(color = "Cluster buffer footprint"),
                fill = NA,
                linewidth = 0.55,
                alpha = 0.95,
                inherit.aes = FALSE
              ) +
              geom_sf(
                data = cluster_point_this,
                aes(shape = "Representative cluster point"),
                color = "black",
                fill = "goldenrod1",
                size = 2.4,
                stroke = 0.4,
                inherit.aes = FALSE
              ) +
              scale_fill_manual(
                values = c("Matched deforestation tile" = "#4c78a8"),
                name = NULL
              ) +
              scale_color_manual(
                values = c(
                  "AEZ boundary" = "grey45",
                  "Cluster buffer footprint" = "#f58518"
                ),
                name = NULL
              ) +
              scale_shape_manual(
                values = c("Representative cluster point" = 21),
                name = NULL
              ) +
              coord_sf(
                xlim = zoom_bbox$xlim,
                ylim = zoom_bbox$ylim,
                expand = FALSE
              ) +
              labs(
                title = paste0(cluster_id_value, " diagnostic"),
                subtitle = paste0(
                  focus_aez_value,
                  " | Buffer = ", buffer_km_value, " km",
                  " | Method = ", cluster_method_target,
                  " | Radius = ", sprintf("%.1f", cluster_radius_km_target), " km"
                ),
                x = NULL,
                y = NULL
              ) +
              theme_minimal() +
              theme(
                panel.grid.major = element_line(color = "grey88", linewidth = 0.2),
                panel.grid.minor = element_blank(),
                legend.position = "bottom",
                legend.box = "vertical"
              )

            save_plot_if_requested(
              plot_cluster_diagnostic,
              paste0(sanitize_cluster_id(cluster_id_value), "_diagnostic.png"),
              output_dir = cluster_output_dir,
              width = 8,
              height = 8
            )
          }
        )
      }
    )

    readr::write_csv(
      focus_matched_tile_ids %>%
        count(buffer_km, name = "n_matched_tiles") %>%
        left_join(
          focus_cluster_buffers %>%
            sf::st_drop_geometry() %>%
            count(buffer_km, name = "n_cluster_buffers"),
          by = "buffer_km"
        ) %>%
        mutate(
          cluster_method = cluster_method_target,
          cluster_radius_km = cluster_radius_km_target,
          AEZ = focus_aez_value
        ) %>%
        arrange(buffer_km),
      file.path(
        current_aez_output_dir,
        paste0(sanitize_aez_id(focus_aez_value), "_cluster_tile_diagnostic_summary.csv")
      )
    )
  }
)

message("Finished data_explore.R")
message("  output root: ", explore_root_dir)
