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

data_explore_dir <- file.path(regression_dir, "data_explore")
cluster_method_target <- "clara"
cluster_radius_km_target <- 10.0
buffer_km_targets <- c(1, 10)
target_aez_values <- "AEZ3"
single_tile_collapse_mode_target <- "collapse_single_tile_clusters"
save_outputs <- TRUE
write_reverse_starting_ov_plot <- FALSE
run_aez_spatial_diagnostics <- FALSE
run_whole_cluster_delta_ov_maps <- TRUE
whole_cluster_map_buffer_km_targets <- buffer_km_targets

# -----------------------
# Set current run paths
# -----------------------

set_annualization_mode("annualized_delta")
set_regression_group(if ("by_aez" %in% regression_groups) "by_aez" else regression_groups[[1]])
set_defor_bin("baseline_deforestation")
set_delta_ov_approach("year_pair_delta_ov")
set_single_tile_collapse_mode(single_tile_collapse_modes[[1]])
set_ov_calculation_method("ov_full")
set_ov_change_mode("linear_delta_ov")
set_defor_approach("total_tagged_tile_deforestation")
set_regression_model("ols")

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

build_cluster_deltas_explore_path <- function(buffer_km, delta_ov_approach) {
  file.path(
    repo_root,
    "deforestation_tile_tag",
    "analysis",
    "output",
    cluster_method_target,
    paste0("radius_", sprintf("%.1fkm", cluster_radius_km_target)),
    paste0("buf_", buffer_km, "km"),
    delta_ov_approach_specs[[delta_ov_approach]]$source_dir,
    single_tile_collapse_mode_specs[[single_tile_collapse_mode_target]]$source_dir,
    "tables",
    "cluster_deltas.csv"
  )
}

country_boundaries_path <- file.path(
  repo_root,
  "spatial_data",
  "naturalearth_10m_admin_0_countries",
  "ne_10m_admin_0_countries.shp"
)

starting_ov_specs <- tibble::tibble(
  ov_calculation_method = c("ov_full", "ov_obs_only"),
  ov_calculation_label = c("Full OV", "Observed-only OV"),
  starting_ov_col = c("ov_t1", "ov_obs_only_t1"),
  delta_ov_col = c("delta_ov", "delta_ov_obs_only")
)

starting_ov_reference_lines <- tibble::tibble(
  ov_calculation_label = c("Full OV", "Full OV", "Observed-only OV", "Observed-only OV"),
  starting_ov_threshold = c(0.80, 0.95, 0.10, 0.25),
  threshold_label = c("Full OV <= 0.80", "Full OV <= 0.95", "Observed-only OV <= 0.10", "Observed-only OV <= 0.25")
)

starting_ov_shaded_regions <- tibble::tibble(
  ov_calculation_label = c("Full OV", "Observed-only OV"),
  starting_ov_threshold = c(0.95, 0.25)
)

delta_ov_approach_labels <- c(
  year_pair_delta_ov = "Year-pair ∆OV",
  whole_cluster_delta_ov = "Whole-cluster ∆OV"
)

delta_ov_approach_file_stubs <- c(
  year_pair_delta_ov = "year_pair",
  whole_cluster_delta_ov = "whole_cluster"
)

read_starting_ov_delta_data <- function(buffer_km, delta_ov_approach) {
  cluster_delta_path <- build_cluster_deltas_explore_path(buffer_km, delta_ov_approach)
  assert_exists(cluster_delta_path)

  cluster_deltas <- readr::read_csv(cluster_delta_path, show_col_types = FALSE) %>%
    normalize_cluster_deltas_schema()

  purrr::pmap_dfr(
    starting_ov_specs,
    function(ov_calculation_method,
             ov_calculation_label,
             starting_ov_col,
             delta_ov_col) {
      assert_has_cols(
        cluster_deltas,
        c("aez", "cluster_id", "year_t1", "year_t2", "year_gap", starting_ov_col, delta_ov_col),
        basename(cluster_delta_path)
      )

      cluster_deltas %>%
        transmute(
          ov_calculation_method = ov_calculation_method,
          ov_calculation_label = ov_calculation_label,
          delta_ov_approach = delta_ov_approach,
          buffer_km = as.numeric(buffer_km),
          aez = standardize_aez_order(aez),
          cluster_id = as.character(cluster_id),
          year_t1 = as.integer(year_t1),
          year_t2 = as.integer(year_t2),
          year_gap = as.integer(year_gap),
          starting_ov = as.numeric(.data[[starting_ov_col]]),
          delta_ov = as.numeric(.data[[delta_ov_col]])
        )
    }
  )
}

plot_starting_ov_delta_diagnostic <- function(starting_ov_delta_data,
                                              delta_ov_approach_filter = NULL) {
  plot_data <- starting_ov_delta_data %>%
    filter(
      is.finite(starting_ov),
      is.finite(delta_ov)
    ) %>%
    mutate(
      buffer_label = factor(
        build_panel_label(buffer_km),
        levels = build_panel_label(buffer_km_targets)
      ),
      ov_calculation_label = factor(
        ov_calculation_label,
        levels = starting_ov_specs$ov_calculation_label
      ),
      delta_ov_approach = factor(
        delta_ov_approach,
        levels = delta_ov_approaches
      )
    )

  if (!is.null(delta_ov_approach_filter)) {
    plot_data <- plot_data %>%
      filter(delta_ov_approach == delta_ov_approach_filter)
  }

  approach_title <- if (is.null(delta_ov_approach_filter)) {
    "Year-pair and whole-cluster ∆OV"
  } else {
    unname(delta_ov_approach_labels[[delta_ov_approach_filter]])
  }

  low_ov_band <- plot_data %>%
    distinct(ov_calculation_label, buffer_label) %>%
    left_join(starting_ov_shaded_regions, by = "ov_calculation_label") %>%
    mutate(
      xmin = -Inf,
      xmax = starting_ov_threshold,
      ymin = -Inf,
      ymax = Inf
    )

  reference_lines <- plot_data %>%
    distinct(ov_calculation_label, buffer_label) %>%
    left_join(
      starting_ov_reference_lines,
      by = "ov_calculation_label",
      relationship = "many-to-many"
    ) %>%
    mutate(
      threshold_label = factor(
        threshold_label,
        levels = starting_ov_reference_lines$threshold_label
      )
    )

  ggplot(
    plot_data,
    aes(x = starting_ov, y = delta_ov, color = delta_ov_approach)
  ) +
    geom_rect(
      data = low_ov_band,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "#f2c94c",
      alpha = 0.13,
      color = NA,
      inherit.aes = FALSE
    ) +
    geom_hline(yintercept = 0, color = "grey35", linewidth = 0.45) +
    geom_vline(
      data = reference_lines,
      aes(xintercept = starting_ov_threshold, linetype = threshold_label),
      color = "grey45",
      linewidth = 0.45,
      inherit.aes = FALSE
    ) +
    geom_point(alpha = 0.32, size = 1.7) +
    geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = TRUE,
      linewidth = 1.05,
      span = 0.8
    ) +
    facet_grid(
      ov_calculation_label ~ buffer_label,
      scales = "free_x"
    ) +
    scale_color_manual(
      values = c(
        "year_pair_delta_ov" = "#2a6fbb",
        "whole_cluster_delta_ov" = "#c75d2c"
      ),
      labels = c(
        "year_pair_delta_ov" = "Year-pair ∆OV",
        "whole_cluster_delta_ov" = "Whole-cluster ∆OV"
      ),
      drop = FALSE,
      name = NULL
    ) +
    scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    scale_linetype_manual(
      values = c(
        "Full OV <= 0.80" = "dotted",
        "Full OV <= 0.95" = "dashed",
        "Observed-only OV <= 0.10" = "dotted",
        "Observed-only OV <= 0.25" = "dashed"
      ),
      name = "Starting OV reference"
    ) +
    labs(
      title = paste0("Starting biodiversity value vs. subsequent ∆OV: ", approach_title),
      subtitle = paste0(
        "All cluster-delta observations for ",
        cluster_method_target,
        " radius ",
        sprintf("%.1f", cluster_radius_km_target),
        " km. Shading marks Full OV <= 0.95 and observed-only OV <= 0.25."
      ),
      x = "Starting OV value",
      y = "∆OV",
      caption = "Negative ∆OV means biodiversity value decreased over the observation window."
    ) +
    theme_minimal(base_size = 15) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
      strip.text.x = element_text(face = "bold", size = 13),
      strip.text.y = element_text(face = "bold", size = 12, angle = 0),
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 12, margin = margin(b = 8)),
      plot.caption = element_text(size = 10, color = "grey35"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

plot_delta_starting_ov_diagnostic <- function(starting_ov_delta_data,
                                              delta_ov_approach_filter = NULL) {
  plot_data <- starting_ov_delta_data %>%
    filter(
      is.finite(starting_ov),
      is.finite(delta_ov)
    ) %>%
    mutate(
      buffer_label = factor(
        build_panel_label(buffer_km),
        levels = build_panel_label(buffer_km_targets)
      ),
      ov_calculation_label = factor(
        ov_calculation_label,
        levels = starting_ov_specs$ov_calculation_label
      ),
      delta_ov_approach = factor(
        delta_ov_approach,
        levels = delta_ov_approaches
      )
    )

  if (!is.null(delta_ov_approach_filter)) {
    plot_data <- plot_data %>%
      filter(delta_ov_approach == delta_ov_approach_filter)
  }

  approach_title <- if (is.null(delta_ov_approach_filter)) {
    "Year-pair and whole-cluster ∆OV"
  } else {
    unname(delta_ov_approach_labels[[delta_ov_approach_filter]])
  }

  low_ov_band <- plot_data %>%
    distinct(ov_calculation_label, buffer_label) %>%
    left_join(starting_ov_shaded_regions, by = "ov_calculation_label") %>%
    mutate(
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = starting_ov_threshold
    )

  reference_lines <- plot_data %>%
    distinct(ov_calculation_label, buffer_label) %>%
    left_join(
      starting_ov_reference_lines,
      by = "ov_calculation_label",
      relationship = "many-to-many"
    ) %>%
    mutate(
      threshold_label = factor(
        threshold_label,
        levels = starting_ov_reference_lines$threshold_label
      )
    )

  ggplot(
    plot_data,
    aes(x = delta_ov, y = starting_ov, color = delta_ov_approach)
  ) +
    geom_rect(
      data = low_ov_band,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "#f2c94c",
      alpha = 0.13,
      color = NA,
      inherit.aes = FALSE
    ) +
    geom_vline(xintercept = 0, color = "grey35", linewidth = 0.45) +
    geom_hline(
      data = reference_lines,
      aes(yintercept = starting_ov_threshold, linetype = threshold_label),
      color = "grey45",
      linewidth = 0.45,
      inherit.aes = FALSE
    ) +
    geom_point(alpha = 0.32, size = 1.7) +
    geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = TRUE,
      linewidth = 1.05,
      span = 0.8
    ) +
    facet_grid(
      ov_calculation_label ~ buffer_label,
      scales = "free_y"
    ) +
    scale_color_manual(
      values = c(
        "year_pair_delta_ov" = "#2a6fbb",
        "whole_cluster_delta_ov" = "#c75d2c"
      ),
      labels = c(
        "year_pair_delta_ov" = "Year-pair ∆OV",
        "whole_cluster_delta_ov" = "Whole-cluster ∆OV"
      ),
      drop = FALSE,
      name = NULL
    ) +
    scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    scale_linetype_manual(
      values = c(
        "Full OV <= 0.80" = "dotted",
        "Full OV <= 0.95" = "dashed",
        "Observed-only OV <= 0.10" = "dotted",
        "Observed-only OV <= 0.25" = "dashed"
      ),
      name = "Starting OV reference"
    ) +
    labs(
      title = paste0("Subsequent ∆OV vs. starting biodiversity value: ", approach_title),
      subtitle = paste0(
        "All cluster-delta observations for ",
        cluster_method_target,
        " radius ",
        sprintf("%.1f", cluster_radius_km_target),
        " km. Shading marks Full OV <= 0.95 and observed-only OV <= 0.25."
      ),
      x = "∆OV",
      y = "Starting OV value",
      caption = "Negative ∆OV means biodiversity value decreased over the observation window."
    ) +
    theme_minimal(base_size = 15) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
      strip.text.x = element_text(face = "bold", size = 13),
      strip.text.y = element_text(face = "bold", size = 12, angle = 0),
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 12, margin = margin(b = 8)),
      plot.caption = element_text(size = 10, color = "grey35"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.box = "vertical"
    )
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

build_continent_bbox <- function(sf_obj, pad_fraction = 0.08, min_pad = 2) {
  bbox <- sf::st_bbox(sf_obj)
  x_range <- bbox$xmax - bbox$xmin
  y_range <- bbox$ymax - bbox$ymin
  x_pad <- max(min_pad, x_range * pad_fraction)
  y_pad <- max(min_pad, y_range * pad_fraction)

  list(
    xlim = c(max(-180, bbox$xmin - x_pad), min(180, bbox$xmax + x_pad)),
    ylim = c(max(-60, bbox$ymin - y_pad), min(85, bbox$ymax + y_pad))
  )
}

build_point_centered_bbox <- function(point_sf, pad_lon = 8, pad_lat = 8) {
  bbox <- sf::st_bbox(point_sf)

  list(
    xlim = c(max(-180, bbox$xmin - pad_lon), min(180, bbox$xmax + pad_lon)),
    ylim = c(max(-60, bbox$ymin - pad_lat), min(85, bbox$ymax + pad_lat))
  )
}

build_regional_map_bbox <- function(continent_value, continent_points, continent_countries) {
  if (identical(continent_value, "Europe")) {
    return(list(xlim = c(-25, 45), ylim = c(34, 72)))
  }

  if (identical(continent_value, "North America")) {
    return(list(xlim = c(-170, -50), ylim = c(5, 85)))
  }

  if (identical(continent_value, "Oceania")) {
    return(build_point_centered_bbox(continent_points, pad_lon = 10, pad_lat = 8))
  }

  if (identical(continent_value, "South America")) {
    return(list(xlim = c(-85, -30), ylim = c(-60, 15)))
  }

  build_continent_bbox(continent_countries)
}

assign_delta_ov_map_continent <- function(continent, country_id, longitude, latitude) {
  dplyr::case_when(
    country_id == "FRA" &
      dplyr::between(longitude, -55, -50) &
      dplyr::between(latitude, 1, 7) ~ "South America",
    TRUE ~ continent
  )
}

build_continent_country_context <- function(country_sf, continent_value) {
  country_context <- country_sf %>%
    filter(continent == continent_value)

  if (identical(continent_value, "South America")) {
    country_context <- country_sf %>%
      filter(continent == continent_value | country_id == "FRA")
  }

  country_context
}

sanitize_continent_id <- function(continent_value) {
  tolower(gsub("[^A-Za-z0-9_-]", "_", continent_value))
}

read_whole_cluster_delta_ov_map_data <- function(buffer_km_values) {
  purrr::map_dfr(
    buffer_km_values,
    function(buffer_km_value) {
      cluster_delta_path <- build_cluster_deltas_explore_path(
        buffer_km = buffer_km_value,
        delta_ov_approach = "whole_cluster_delta_ov"
      )
      assert_exists(cluster_delta_path)

      cluster_deltas <- readr::read_csv(cluster_delta_path, show_col_types = FALSE) %>%
        normalize_cluster_deltas_schema()

      assert_has_cols(
        cluster_deltas,
        c(
          "cluster_id",
          "buffer_km",
          "medoid_latitude",
          "medoid_longitude",
          "delta_ov",
          "primary_country_id",
          "primary_country_name",
          "member_cluster_ids"
        ),
        basename(cluster_delta_path)
      )

      cluster_deltas %>%
        transmute(
          cluster_id = as.character(cluster_id),
          source_buffer_km = as.numeric(.data$buffer_km),
          medoid_latitude = as.numeric(medoid_latitude),
          medoid_longitude = as.numeric(medoid_longitude),
          delta_ov = as.numeric(delta_ov),
          country_id = as.character(primary_country_id),
          country_name = as.character(primary_country_name),
          member_cluster_ids = as.character(member_cluster_ids),
          n_member_clusters = if ("n_member_clusters" %in% names(cluster_deltas)) {
            as.integer(n_member_clusters)
          } else {
            NA_integer_
          }
        ) %>%
        filter(
          is.finite(medoid_latitude),
          is.finite(medoid_longitude),
          is.finite(delta_ov)
        )
    }
  ) %>%
    arrange(cluster_id, source_buffer_km) %>%
    distinct(cluster_id, .keep_all = TRUE)
}

read_cluster_observation_counts <- function() {
  cluster_sites_path <- file.path(canonical_spatial_dir, "cluster_sites.gpkg")
  assert_exists(cluster_sites_path)

  read_sf_safely(cluster_sites_path, file.path(explore_root_dir, "tmp_sf_stage")) %>%
    rename_with(tolower) %>%
    st_drop_geometry() %>%
    mutate(cluster_id = as.character(cluster_id)) %>%
    count(cluster_id, name = "n_cluster_observations")
}

add_cluster_observation_counts <- function(delta_map_data, cluster_observation_counts) {
  delta_map_data %>%
    mutate(
      member_cluster_id = strsplit(member_cluster_ids, ";", fixed = TRUE)
    ) %>%
    tidyr::unnest(member_cluster_id, keep_empty = TRUE) %>%
    mutate(member_cluster_id = as.character(member_cluster_id)) %>%
    left_join(
      cluster_observation_counts,
      by = c("member_cluster_id" = "cluster_id")
    ) %>%
    group_by(across(-c(member_cluster_id, n_cluster_observations))) %>%
    summarise(
      n_cluster_observations = sum(n_cluster_observations, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      n_cluster_observations = dplyr::if_else(
        n_cluster_observations > 0,
        n_cluster_observations,
        NA_real_
      )
    )
}

build_whole_cluster_delta_ov_map <- function(cluster_points_sf,
                                             country_sf,
                                             delta_scale_limits,
                                             title,
                                             subtitle,
                                             xlim = NULL,
                                             ylim = NULL) {
  plot_obj <- ggplot() +
    geom_sf(
      data = country_sf,
      fill = "grey97",
      color = "grey62",
      linewidth = 0.18,
      inherit.aes = FALSE
    ) +
    geom_sf(
      data = cluster_points_sf,
      aes(fill = delta_ov_for_map, size = n_cluster_observations),
      shape = 21,
      color = "grey12",
      stroke = 0.18,
      alpha = 0.88,
      inherit.aes = FALSE
    ) +
    scale_fill_gradient(
      low = "#7f0000",
      high = "#c7e9b4",
      limits = delta_scale_limits,
      oob = scales::squish,
      labels = scales::label_number(accuracy = 0.01),
      name = "Whole-cluster ∆OV"
    ) +
    scale_size_continuous(
      range = c(1.8, 5.8),
      breaks = c(1, 5, 10, 25, 50),
      name = "Cluster observations"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      caption = "Only clusters with negative whole-cluster ∆OV are shown."
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 17),
      plot.subtitle = element_text(size = 11, margin = margin(b = 7)),
      plot.caption = element_text(color = "grey35", size = 9),
      legend.position = "right"
    )

  if (!is.null(xlim) && !is.null(ylim)) {
    plot_obj <- plot_obj +
      coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)
  } else {
    plot_obj <- plot_obj +
      coord_sf(expand = FALSE)
  }

  plot_obj
}

# -----------------------
# Starting OV vs ∆OV diagnostic across all cluster-delta observations
# -----------------------

starting_ov_delta_data <- tidyr::crossing(
  buffer_km = buffer_km_targets,
  delta_ov_approach = delta_ov_approaches
) %>%
  purrr::pmap_dfr(read_starting_ov_delta_data) %>%
  filter(delta_ov < 0)

starting_ov_delta_summary <- starting_ov_delta_data %>%
  filter(
    is.finite(starting_ov),
    is.finite(delta_ov)
  ) %>%
  group_by(ov_calculation_method, delta_ov_approach, buffer_km) %>%
  summarise(
    n_obs = dplyr::n(),
    min_starting_ov = min(starting_ov),
    p10_starting_ov = quantile(starting_ov, 0.10, na.rm = TRUE),
    median_starting_ov = median(starting_ov),
    n_starting_ov_le_010 = sum(starting_ov <= 0.10),
    pct_starting_ov_le_010 = mean(starting_ov <= 0.10),
    n_starting_ov_le_025 = sum(starting_ov <= 0.25),
    pct_starting_ov_le_025 = mean(starting_ov <= 0.25),
    n_starting_ov_le_080 = sum(starting_ov <= 0.80),
    pct_starting_ov_le_080 = mean(starting_ov <= 0.80),
    n_starting_ov_le_095 = sum(starting_ov <= 0.95),
    pct_starting_ov_le_095 = mean(starting_ov <= 0.95),
    median_delta_ov_when_starting_ov_le_010 = ifelse(
      any(starting_ov <= 0.10),
      median(delta_ov[starting_ov <= 0.10], na.rm = TRUE),
      NA_real_
    ),
    median_delta_ov_when_starting_ov_le_025 = ifelse(
      any(starting_ov <= 0.25),
      median(delta_ov[starting_ov <= 0.25], na.rm = TRUE),
      NA_real_
    ),
    median_delta_ov_when_starting_ov_le_080 = ifelse(
      any(starting_ov <= 0.80),
      median(delta_ov[starting_ov <= 0.80], na.rm = TRUE),
      NA_real_
    ),
    median_delta_ov_when_starting_ov_le_095 = ifelse(
      any(starting_ov <= 0.95),
      median(delta_ov[starting_ov <= 0.95], na.rm = TRUE),
      NA_real_
    ),
    correlation_starting_ov_delta_ov = ifelse(
      dplyr::n_distinct(starting_ov) >= 2 && dplyr::n_distinct(delta_ov) >= 2,
      stats::cor(starting_ov, delta_ov),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )

starting_ov_delta_plot <- plot_starting_ov_delta_diagnostic(starting_ov_delta_data)

save_plot_if_requested(
  starting_ov_delta_plot,
  "starting_ov_vs_delta_ov__ov_full_obs_only.png",
  output_dir = explore_root_dir,
  width = 14,
  height = 9
)

if (isTRUE(write_reverse_starting_ov_plot)) {
  delta_starting_ov_plot <- plot_delta_starting_ov_diagnostic(starting_ov_delta_data)

  save_plot_if_requested(
    delta_starting_ov_plot,
    "delta_ov_vs_starting_ov__ov_full_obs_only.png",
    output_dir = explore_root_dir,
    width = 14,
    height = 9
  )
}

purrr::walk(
  delta_ov_approaches,
  function(delta_ov_approach) {
    approach_stub <- unname(delta_ov_approach_file_stubs[[delta_ov_approach]])

    save_plot_if_requested(
      plot_starting_ov_delta_diagnostic(
        starting_ov_delta_data,
        delta_ov_approach_filter = delta_ov_approach
      ),
      paste0("starting_ov_vs_delta_ov_", approach_stub, ".png"),
      output_dir = explore_root_dir,
      width = 14,
      height = 9
    )

    if (isTRUE(write_reverse_starting_ov_plot)) {
      save_plot_if_requested(
        plot_delta_starting_ov_diagnostic(
          starting_ov_delta_data,
          delta_ov_approach_filter = delta_ov_approach
        ),
        paste0("delta_ov_vs_starting_ov_", approach_stub, ".png"),
        output_dir = explore_root_dir,
        width = 14,
        height = 9
      )
    }
  }
)

if (isTRUE(save_outputs)) {
  readr::write_csv(
    starting_ov_delta_summary,
    file.path(explore_root_dir, "starting_ov_vs_delta_ov__summary.csv")
  )
  message("Wrote: ", file.path(explore_root_dir, "starting_ov_vs_delta_ov__summary.csv"))
}

# -----------------------
# Global and continental whole-cluster ∆OV maps
# -----------------------

if (isTRUE(run_whole_cluster_delta_ov_maps)) {
  assert_exists(country_boundaries_path)

  whole_cluster_map_output_dir <- file.path(explore_root_dir, "whole_cluster_delta_ov_maps")
  dir.create(whole_cluster_map_output_dir, recursive = TRUE, showWarnings = FALSE)

  cluster_observation_counts <- read_cluster_observation_counts()

  country_boundaries_sf <- sf::read_sf(country_boundaries_path) %>%
    rename_with(tolower) %>%
    st_make_valid() %>%
    st_collection_extract("POLYGON", warn = FALSE) %>%
    dplyr::select(
      country_id = adm0_a3,
      country_name = admin,
      continent,
      geometry
    ) %>%
    mutate(
      country_id = as.character(country_id),
      country_name = as.character(country_name),
      continent = as.character(continent)
    ) %>%
    st_transform(4326)

  whole_cluster_delta_map_data <- read_whole_cluster_delta_ov_map_data(
    whole_cluster_map_buffer_km_targets
  ) %>%
    add_cluster_observation_counts(cluster_observation_counts) %>%
    left_join(
      country_boundaries_sf %>%
        st_drop_geometry() %>%
        dplyr::select(country_id, continent),
      by = "country_id"
    ) %>%
    mutate(
      continent = assign_delta_ov_map_continent(
        dplyr::coalesce(continent, "Unassigned"),
        country_id,
        medoid_longitude,
        medoid_latitude
      ),
      delta_ov_for_map = delta_ov
    ) %>%
    filter(delta_ov < 0)

  if (nrow(whole_cluster_delta_map_data) == 0) {
    warning("No whole-cluster ∆OV rows available for map generation.", call. = FALSE)
  } else {
    most_negative_delta_ov <- min(whole_cluster_delta_map_data$delta_ov_for_map, na.rm = TRUE)
    if (!is.finite(most_negative_delta_ov) || most_negative_delta_ov >= 0) {
      most_negative_delta_ov <- -1
    }
    delta_scale_limits <- c(most_negative_delta_ov, 0)

    whole_cluster_delta_points_sf <- whole_cluster_delta_map_data %>%
      st_as_sf(
        coords = c("medoid_longitude", "medoid_latitude"),
        crs = 4326,
        remove = FALSE
      )

    map_metadata <- whole_cluster_delta_map_data %>%
      st_drop_geometry() %>%
      group_by(continent) %>%
      summarise(
        n_analysis_units = n(),
        n_cluster_observations = sum(n_cluster_observations, na.rm = TRUE),
        min_delta_ov = min(delta_ov, na.rm = TRUE),
        median_delta_ov = median(delta_ov, na.rm = TRUE),
        max_delta_ov = max(delta_ov, na.rm = TRUE),
        n_negative_delta_ov = sum(delta_ov < 0, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

    if (isTRUE(save_outputs)) {
      readr::write_csv(
        map_metadata,
        file.path(whole_cluster_map_output_dir, "whole_cluster_delta_ov_map_summary.csv")
      )
      message("Wrote: ", file.path(whole_cluster_map_output_dir, "whole_cluster_delta_ov_map_summary.csv"))
    }

    if (nrow(whole_cluster_delta_points_sf) == 0) {
      warning("Skipping whole-cluster ∆OV maps: no negative ∆OV rows.")
    } else {
      map_points <- whole_cluster_delta_points_sf

      global_map <- build_whole_cluster_delta_ov_map(
        cluster_points_sf = map_points,
        country_sf = country_boundaries_sf,
        delta_scale_limits = delta_scale_limits,
        title = "Global whole-cluster ∆OV",
        subtitle = paste0(
          "Method = ", cluster_method_target,
          " | Radius = ", sprintf("%.1f", cluster_radius_km_target), " km",
          " | Shared color scale across global and continental maps"
        ),
        xlim = c(-180, 180),
        ylim = c(-60, 85)
      )

      save_plot_if_requested(
        global_map,
        "whole_cluster_delta_ov_global.png",
        output_dir = whole_cluster_map_output_dir,
        width = 15,
        height = 8
      )

      continents_to_plot <- map_points %>%
        st_drop_geometry() %>%
        distinct(continent) %>%
        filter(!is.na(continent), continent != "Unassigned") %>%
        arrange(continent) %>%
        pull(continent)

      purrr::walk(
        continents_to_plot,
        function(continent_value) {
          continent_points <- map_points %>%
            filter(continent == continent_value)

          continent_countries <- build_continent_country_context(
            country_boundaries_sf,
            continent_value
          )

          if (nrow(continent_countries) == 0 || nrow(continent_points) == 0) {
            return(invisible(NULL))
          }

          continent_bbox <- build_regional_map_bbox(
            continent_value,
            continent_points,
            continent_countries
          )

          continent_map <- build_whole_cluster_delta_ov_map(
            cluster_points_sf = continent_points,
            country_sf = continent_countries,
            delta_scale_limits = delta_scale_limits,
            title = paste0(continent_value, " whole-cluster ∆OV"),
            subtitle = paste0(
              "Method = ", cluster_method_target,
              " | Radius = ", sprintf("%.1f", cluster_radius_km_target), " km",
              " | Dot size = number of observations in the cluster",
              " | Country outlines from Natural Earth admin-0 countries"
            ),
            xlim = continent_bbox$xlim,
            ylim = continent_bbox$ylim
          )

          save_plot_if_requested(
            continent_map,
            paste0(
              "whole_cluster_delta_ov_",
              sanitize_continent_id(continent_value),
              ".png"
            ),
            output_dir = whole_cluster_map_output_dir,
            width = 11,
            height = 8
          )
        }
      )
    }
  }
}

if (!isTRUE(run_aez_spatial_diagnostics)) {
  message("Skipping AEZ spatial diagnostics. Set run_aez_spatial_diagnostics <- TRUE to enable them.")
}

if (isTRUE(run_aez_spatial_diagnostics)) {

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
  rename_with(tolower) %>%
  mutate(
    aez = standardize_aez_order(aez),
    cluster_id = as.character(cluster_id),
    dist_to_medoid = as.numeric(dist_to_medoid)
  )

cluster_buffer <- read_sf_safely(cluster_buffer_path, sf_stage_dir) %>%
  rename_with(tolower) %>%
  mutate(
    aez = standardize_aez_order(aez),
    cluster_id = as.character(cluster_id),
    buffer_km = as.numeric(buffer_km)
  )

defor_tile_geometry <- read_sf_safely(defor_tile_geometry_path, sf_stage_dir) %>%
  rename_with(tolower) %>%
  mutate(
    tile_id = as.character(tile_id)
  )

cluster_buffer_tile <- readr::read_csv(cluster_buffer_tile_path, show_col_types = FALSE) %>%
  rename_with(tolower) %>%
  mutate(
    aez = standardize_aez_order(aez),
    cluster_id = as.character(cluster_id),
    buffer_km = as.numeric(buffer_km),
    tile_id = as.character(tile_id)
  )

aez_sf <- sf::read_sf(aez_path) %>%
  rename_with(tolower) %>%
  mutate(
    aez = standardize_aez_order(aez)
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
  distinct(aez) %>%
  pull(aez) %>%
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
      filter(as.character(aez) == focus_aez_value)

    if (nrow(focus_aez_sf) == 0) {
      warning("Skipping ", focus_aez_value, ": no AEZ polygon found.")
      return(invisible(NULL))
    }

    focus_cluster_points <- cluster_sites %>%
      filter(as.character(aez) == focus_aez_value) %>%
      arrange(cluster_id, dist_to_medoid) %>%
      group_by(cluster_id) %>%
      slice(1) %>%
      ungroup() %>%
      dplyr::select(aez, cluster_id, sample_id, year, dist_to_medoid)

    focus_cluster_buffers <- cluster_buffer %>%
      filter(
        as.character(aez) == focus_aez_value,
        buffer_km %in% buffer_km_targets
      ) %>%
      dplyr::select(aez, cluster_id, buffer_km, n_sites, n_matched_tiles, tagged_ha_tile)

    focus_matched_tile_ids <- cluster_buffer_tile %>%
      filter(
        as.character(aez) == focus_aez_value,
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
                as.character(aez) == focus_aez_value,
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
          aez = focus_aez_value
        ) %>%
        arrange(buffer_km),
      file.path(
        current_aez_output_dir,
        paste0(sanitize_aez_id(focus_aez_value), "_cluster_tile_diagnostic_summary.csv")
      )
    )
  }
)

}

message("Finished data_explore.R")
message("  output root: ", explore_root_dir)
