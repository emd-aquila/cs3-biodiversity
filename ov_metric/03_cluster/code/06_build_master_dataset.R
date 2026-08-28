# =====================================================
# Build master clustering datasets and AEZ-level OV summaries by database
# =====================================================

# Master summaries should use every valid clustered output present on disk,
# not just the currently configured run grid.
target_radii <- NULL
target_methods <- NULL

round_summary_sigfigs <- function(df, cols, digits = 4) {
  df %>%
    mutate(
      across(
        any_of(cols),
        ~ signif(.x, digits)
      )
    )
}

cluster_level_sigfig_cols <- c(
  "raw_min",
  "raw_max",
  "raw_range",
  "raw_p25",
  "raw_p75",
  "raw_iqr",
  "mm_min",
  "mm_max",
  "mm_range",
  "mm_p25",
  "mm_p75",
  "mm_iqr"
)

aez_level_sigfig_cols <- c(
  "max_ov_range_raw",
  "max_ov_range_minmax",
  "median_cluster_raw_iqr",
  "median_cluster_mm_iqr"
)

for (dataset_i in seq_len(nrow(cluster_dataset_specs))) {
  dataset_spec <- cluster_dataset_specs[dataset_i, ]
  output_dir <- dataset_spec$output_dir
  summary_dir <- dataset_spec$summary_dir
  dir.create(summary_dir, showWarnings = FALSE, recursive = TRUE)

  clustered_paths <- list.files(
    output_dir,
    pattern = "^(model_df_clustered|.*_model_df_clustered)\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(clustered_paths) == 0) {
    warning("No clustered model files found for ", dataset_spec$dataset_label, ": ", normalizePath(output_dir, mustWork = FALSE))
    next
  }

  cluster_cols_list <- map(
    clustered_paths,
    cluster_column_from_path,
    target_radii = target_radii,
    target_methods = target_methods
  ) %>%
    compact()

  if (length(cluster_cols_list) == 0) {
    stop("No valid clustered files were parsed for ", dataset_spec$dataset_label, ".")
  }

  base_path <- first_clustered_path_for_grid(clustered_paths, target_radii, target_methods)

  base <- read_csv(base_path, show_col_types = FALSE) %>%
    select(
      sample_id,
      AEZ,
      Latitude,
      Longitude,
      Sample_midpoint,
      ov_score,
      Id,
      lat_r,
      lon_r,
      year
    ) %>%
    distinct(sample_id, .keep_all = TRUE)

  if (anyDuplicated(base$sample_id)) {
    stop("Base table still contains duplicated sample_id values for ", dataset_spec$dataset_label, ".")
  }

  clusters_wide <- reduce(cluster_cols_list, left_join, by = "sample_id")
  master <- base %>%
    left_join(clusters_wide, by = "sample_id")

  ov_rng <- range(master$ov_score, na.rm = TRUE)

  master <- master %>%
    mutate(
      dataset = dataset_spec$dataset_key,
      ov_minmax_global = if (ov_rng[2] == ov_rng[1]) {
        NA_real_
      } else {
        (ov_score - ov_rng[1]) / (ov_rng[2] - ov_rng[1])
      }
    ) %>%
    relocate(dataset, .before = sample_id)

  write_csv(
    master,
    file.path(summary_dir, "clustered_master_dataset.csv")
  )

  cluster_cols <- names(master)[str_detect(names(master), "^cluster_id_")]

  long <- master %>%
    pivot_longer(
      cols = all_of(cluster_cols),
      names_to = "cluster_key",
      values_to = "cluster_id"
    ) %>%
    mutate(
      method = str_match(cluster_key, "^cluster_id_([^_]+)_")[, 2],
      radius_km = as.numeric(str_match(cluster_key, "_([0-9]+(?:\\.[0-9]+)?)km$")[, 2])
    ) %>%
    filter(!is.na(method), !is.na(radius_km)) %>%
    filter(!is.na(cluster_id)) %>%
    filter(!is.na(AEZ), !is.na(ov_score), !is.na(ov_minmax_global))

  cluster_level_information <- long %>%
    group_by(dataset, method, radius_km, AEZ, cluster_id) %>%
    summarise(
      n_obs = n(),
      raw_min = min(ov_score, na.rm = TRUE),
      raw_max = max(ov_score, na.rm = TRUE),
      raw_range = raw_max - raw_min,
      raw_p25 = quantile(ov_score, 0.25, na.rm = TRUE),
      raw_p75 = quantile(ov_score, 0.75, na.rm = TRUE),
      raw_iqr = raw_p75 - raw_p25,
      mm_min = min(ov_minmax_global, na.rm = TRUE),
      mm_max = max(ov_minmax_global, na.rm = TRUE),
      mm_range = mm_max - mm_min,
      mm_p25 = quantile(ov_minmax_global, 0.25, na.rm = TRUE),
      mm_p75 = quantile(ov_minmax_global, 0.75, na.rm = TRUE),
      mm_iqr = mm_p75 - mm_p25,
      .groups = "drop"
    ) %>%
    arrange(method, radius_km, aez_sort_key(AEZ), AEZ, cluster_id) %>%
    round_summary_sigfigs(cluster_level_sigfig_cols)

  write_csv(
    cluster_level_information,
    file.path(summary_dir, "cluster_level_information.csv")
  )

  aez_level_information <- cluster_level_information %>%
    group_by(dataset, method, radius_km, AEZ) %>%
    summarise(
      n_clusters_in_aez = n(),
      max_ov_range_raw = max(raw_range, na.rm = TRUE),
      max_ov_range_minmax = max(mm_range, na.rm = TRUE),
      median_cluster_raw_iqr = median(raw_iqr, na.rm = TRUE),
      median_cluster_mm_iqr = median(mm_iqr, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(method, radius_km, aez_sort_key(AEZ), AEZ) %>%
    round_summary_sigfigs(aez_level_sigfig_cols)

  write_csv(
    aez_level_information,
    file.path(summary_dir, "aez_level_information.csv")
  )

  message("Finished master dataset and AEZ summaries for ", dataset_spec$dataset_label, ".")
}
