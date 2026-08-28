# =====================================================
# Summarize clustering bundles and write comparison tables by database
# =====================================================

# Diagnostics should report every valid clustering output present on disk,
# including older sensitivity radii that are not in the current run grid.
target_radii <- NULL
target_methods <- NULL

for (dataset_i in seq_len(nrow(cluster_dataset_specs))) {
  dataset_spec <- cluster_dataset_specs[dataset_i, ]
  output_dir <- dataset_spec$output_dir
  summary_dir <- dataset_spec$summary_dir
  dir.create(summary_dir, showWarnings = FALSE, recursive = TRUE)

  bundle_paths <- list.files(
    output_dir,
    pattern = "^(bundle|.*_bundle)\\.rds$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(bundle_paths) == 0) {
    warning("No bundle files found for ", dataset_spec$dataset_label, ": ", normalizePath(output_dir, mustWork = FALSE))
    next
  }

  bundle_tbl <- map_dfr(
    bundle_paths,
    summarize_bundle_path,
    target_radii = target_radii,
    target_methods = target_methods
  ) %>%
    arrange(method, radius_km)

  if (nrow(bundle_tbl) == 0) {
    stop("No valid bundle summaries were produced for ", dataset_spec$dataset_label, ".")
  }

  decision_tbl <- bundle_tbl %>%
    mutate(
      dataset = dataset_spec$dataset_key,
      pct_sites_clustered = round(pct_sites_clustered, 1),
      pct_clusters_2plus_years = round(pct_clusters_2plus_years, 1),
      median_cluster_size = round(median_cluster_size, 1),
      p90_cluster_size = round(p90_cluster_size, 1),
      max_radius_observed_km = round(max_radius_observed_km, 2)
    ) %>%
    select(
      dataset,
      method,
      radius_km,
      pct_sites_clustered,
      n_clusters,
      n_aez_with_clusters,
      effective_cluster_years,
      median_cluster_size,
      p90_cluster_size,
      max_cluster_size,
      max_radius_observed_km
    ) %>%
    arrange(method, radius_km)

  write_csv(
    decision_tbl,
    file.path(summary_dir, "cluster_method_comparison.csv")
  )

  clusters_by_aez_tbl <- map_dfr(
    bundle_paths,
    clusters_by_aez_path,
    target_radii = target_radii,
    target_methods = target_methods
  ) %>%
    mutate(dataset = dataset_spec$dataset_key) %>%
    select(dataset, everything()) %>%
    arrange(method, radius_km, aez_sort_key(AEZ), AEZ)

  if (nrow(clusters_by_aez_tbl) == 0) {
    stop("No AEZ-level cluster summaries were produced for ", dataset_spec$dataset_label, ".")
  }

  clusters_by_aez_wide <- clusters_by_aez_tbl %>%
    mutate(method_radius = paste0(method, "_", radius_km, "km")) %>%
    select(AEZ, method_radius, n_clusters) %>%
    pivot_wider(
      names_from = method_radius,
      values_from = n_clusters
    ) %>%
    arrange(aez_sort_key(AEZ), AEZ)

  write_csv(
    clusters_by_aez_tbl,
    file.path(summary_dir, "clusters_by_AEZ_long.csv")
  )

  write_csv(
    clusters_by_aez_wide,
    file.path(summary_dir, "clusters_by_AEZ_wide.csv")
  )

  message("Finished bundle diagnostics for ", dataset_spec$dataset_label, ".")
}
