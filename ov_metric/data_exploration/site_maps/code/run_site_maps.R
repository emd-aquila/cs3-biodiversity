# =====================================================
# Map PREDICTS, BioTIME, and combined OV sampling sites
# =====================================================

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(sf)
library(scales)

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  normalizePath(getwd())
}

topic_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
repo_root <- normalizePath(file.path(topic_dir, "..", "..", ".."), mustWork = TRUE)
ov_metric_dir <- file.path(repo_root, "ov_metric")
output_dir <- file.path(topic_dir, "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ov_output_dir <- file.path(ov_metric_dir, "02_ov_calculation", "calculation", "output")
country_boundaries_path <- file.path(
  repo_root,
  "00_spatial_data",
  "naturalearth_10m_admin_0_countries",
  "ne_10m_admin_0_countries.shp"
)

dataset_specs <- tibble::tibble(
  dataset = c("predicts", "biotime", "combined"),
  database = c("PREDICTS", "BioTIME", "combined"),
  path = file.path(
    ov_output_dir,
    c("predicts_ov_scores.csv", "biotime_ov_scores.csv", "combined_ov_scores.csv")
  )
)

missing_inputs <- dataset_specs$path[!file.exists(dataset_specs$path)]
if (length(missing_inputs) > 0) {
  stop("Missing OV input file(s): ", paste(missing_inputs, collapse = ", "), call. = FALSE)
}
if (!file.exists(country_boundaries_path)) {
  stop("Country boundary shapefile not found: ", country_boundaries_path, call. = FALSE)
}

read_site_scores <- function(path, dataset, database) {
  readr::read_csv(path, show_col_types = FALSE) %>%
    mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      ov_score = as.numeric(ov_score),
      sample_year = as.integer(sample_year),
      lat_r = round(latitude, 4),
      lon_r = round(longitude, 4)
    ) %>%
    filter(!is.na(AEZ), !is.na(lat_r), !is.na(lon_r), is.finite(ov_score)) %>%
    group_by(AEZ, lat_r, lon_r) %>%
    summarise(
      n_samples = n_distinct(sample_id),
      n_years = n_distinct(sample_year),
      first_year = min(sample_year, na.rm = TRUE),
      last_year = max(sample_year, na.rm = TRUE),
      mean_ov_score = mean(ov_score, na.rm = TRUE),
      median_ov_score = median(ov_score, na.rm = TRUE),
      min_ov_score = min(ov_score, na.rm = TRUE),
      max_ov_score = max(ov_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      dataset = dataset,
      database = database,
      latitude = lat_r,
      longitude = lon_r,
      across(
        c(mean_ov_score, median_ov_score, min_ov_score, max_ov_score),
        ~ round(.x, 3)
      )
    ) %>%
    select(
      dataset,
      database,
      AEZ,
      latitude,
      longitude,
      n_samples,
      n_years,
      first_year,
      last_year,
      mean_ov_score,
      median_ov_score,
      min_ov_score,
      max_ov_score
    )
}

map_theme <- function() {
  theme_void(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      strip.text = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey30")
    )
}

plot_ov_map <- function(plot_data, title, filename, width = 12, height = 7) {
  plot_obj <- ggplot() +
    geom_sf(data = countries, fill = "grey94", color = "white", linewidth = 0.12) +
    geom_point(
      data = plot_data,
      aes(x = longitude, y = latitude, color = mean_ov_score, size = pmin(n_samples, 10)),
      alpha = 0.68
    ) +
    scale_color_gradient(low = "#2166AC", high = "#B2182B", name = "Mean OV") +
    scale_size_continuous(range = c(0.45, 1.8), guide = "none") +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    labs(title = title, subtitle = "Each point is one unique AEZ plus rounded latitude/longitude site.") +
    map_theme()

  ggsave(file.path(output_dir, filename), plot_obj, width = width, height = height, dpi = 300)
}

format_count_table <- function(dat) {
  dat %>%
    mutate(
      across(
        where(is.numeric),
        ~ scales::comma(.x, accuracy = if (any(.x %% 1 != 0, na.rm = TRUE)) 0.1 else 1)
      )
    )
}

site_scores <- purrr::pmap_dfr(dataset_specs, read_site_scores)
countries <- sf::read_sf(country_boundaries_path) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

aez_counts_long <- site_scores %>%
  count(dataset, database, AEZ, name = "n_sites") %>%
  group_by(dataset, database) %>%
  mutate(
    total_sites = sum(n_sites),
    pct_sites = round(100 * n_sites / total_sites, 1)
  ) %>%
  ungroup() %>%
  arrange(AEZ, dataset)

aez_counts_wide <- aez_counts_long %>%
  select(AEZ, dataset, n_sites) %>%
  pivot_wider(
    names_from = dataset,
    values_from = n_sites,
    names_prefix = "n_sites_",
    values_fill = 0
  ) %>%
  arrange(AEZ)

readr::write_csv(site_scores, file.path(output_dir, "site_mean_ov_scores_by_database.csv"))
readr::write_csv(format_count_table(aez_counts_long), file.path(output_dir, "aez_site_counts_by_database_long.csv"))
readr::write_csv(format_count_table(aez_counts_wide), file.path(output_dir, "aez_site_counts_by_database_wide.csv"))

predicts_biotime <- site_scores %>%
  filter(dataset %in% c("predicts", "biotime")) %>%
  mutate(database = factor(database, levels = c("PREDICTS", "BioTIME")))

together_plot <- ggplot() +
  geom_sf(data = countries, fill = "grey94", color = "white", linewidth = 0.12) +
  geom_point(
    data = predicts_biotime,
    aes(x = longitude, y = latitude, color = database),
    alpha = 0.58,
    size = 0.72
  ) +
  scale_color_manual(values = c(PREDICTS = "#0072B2", BioTIME = "#D55E00"), name = NULL) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  labs(
    title = "PREDICTS and BioTIME mapped sites",
    subtitle = "Each point is one unique AEZ plus rounded latitude/longitude site."
  ) +
  map_theme()

ggsave(
  file.path(output_dir, "predicts_biotime_sites_together_map.png"),
  together_plot,
  width = 12,
  height = 7,
  dpi = 300
)

facet_plot <- ggplot() +
  geom_sf(data = countries, fill = "grey94", color = "white", linewidth = 0.12) +
  geom_point(
    data = predicts_biotime,
    aes(x = longitude, y = latitude, color = mean_ov_score, size = pmin(n_samples, 10)),
    alpha = 0.68
  ) +
  facet_wrap(~ database, ncol = 1) +
  scale_color_gradient(low = "#2166AC", high = "#B2182B", name = "Mean OV") +
  scale_size_continuous(range = c(0.45, 1.8), guide = "none") +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  labs(
    title = "Site mean OV scores in PREDICTS and BioTIME",
    subtitle = "Each point is one unique AEZ plus rounded latitude/longitude site."
  ) +
  map_theme()

ggsave(
  file.path(output_dir, "predicts_biotime_site_mean_ov_scores_faceted_map.png"),
  facet_plot,
  width = 10,
  height = 11,
  dpi = 300
)

plot_ov_map(
  filter(site_scores, dataset == "predicts"),
  "PREDICTS site mean OV scores",
  "predicts_site_mean_ov_scores_map.png"
)
plot_ov_map(
  filter(site_scores, dataset == "biotime"),
  "BioTIME site mean OV scores",
  "biotime_site_mean_ov_scores_map.png"
)
plot_ov_map(
  filter(site_scores, dataset == "combined"),
  "Combined site mean OV scores",
  "combined_site_mean_ov_scores_map.png"
)

message("Wrote site-map data exploration outputs to: ", normalizePath(output_dir, mustWork = FALSE))
