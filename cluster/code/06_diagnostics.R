# =====================================================
# Summarize clustering bundles and write comparison tables
# 
# Read saved bundle outputs, compute method-radius diagnostics,
# and export overall and AEZ-level comparison tables.
# =====================================================


## ----------------------------
## Load bundle files from ./outputs
## ----------------------------

bundle_paths <- list.files(
  output_dir,
  pattern = "_bundle\\.rds$",
  full.names = TRUE
)

if (length(bundle_paths) == 0) {
  stop("No bundle files found in: ", normalizePath(output_dir, mustWork = FALSE))
}

target_radii <- sort(unique(run_grid$radius_km))
target_methods <- sort(unique(run_grid$method))

# -----------------------
# Summarize each bundle
# -----------------------

bundle_tbl <- map_dfr(
  bundle_paths,
  function(path) {
    radius_km <- parse_radius_km(path)
    method <- parse_method(path)
    
    if (is.na(radius_km) || !(radius_km %in% target_radii)) {
      return(NULL)
    }
    
    if (!(method %in% target_methods)) {
      return(NULL)
    }
    
    bundle <- readRDS(path)
    
    summarize_bundle(
      bundle = bundle,
      method = method,
      radius_km = radius_km,
      year_single_col = "years_single"
    )
  }
) %>%
  arrange(method, radius_km)

if (nrow(bundle_tbl) == 0) {
  stop("No valid bundle summaries were produced. Check file naming and parse helpers.")
}

# -----------------------
# Build decision table
# -----------------------

decision_tbl <- bundle_tbl %>%
  mutate(
    pct_sites_clustered = round(pct_sites_clustered, 1),
    pct_clusters_2plus_years = round(pct_clusters_2plus_years, 1),
    median_cluster_size = round(median_cluster_size, 1),
    p90_cluster_size = round(p90_cluster_size, 1),
    max_radius_observed_km = round(max_radius_observed_km, 2)
  ) %>%
  select(
    method,
    radius_km,
    n_sites_total,
    pct_sites_clustered,
    n_clusters,
    n_clusters_2plus_years,
    pct_clusters_2plus_years,
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

# -----------------------
# Count clusters within each AEZ
# -----------------------

clusters_by_aez_tbl <- map_dfr(
  bundle_paths,
  function(path) {
    radius_km <- parse_radius_km(path)
    method <- parse_method(path)
    
    if (is.na(radius_km) || !(radius_km %in% target_radii)) {
      return(NULL)
    }
    
    if (!(method %in% target_methods)) {
      return(NULL)
    }
    
    bundle <- readRDS(path)
    
    clusters_by_aez_one_bundle(
      bundle = bundle,
      method = method,
      radius_km = radius_km
    )
  }
) %>%
  arrange(method, radius_km, AEZ)

if (nrow(clusters_by_aez_tbl) == 0) {
  stop("No AEZ-level cluster summaries were produced.")
}

clusters_by_aez_wide <- clusters_by_aez_tbl %>%
  mutate(method_radius = paste0(method, "_", radius_km, "km")) %>%
  select(AEZ, method_radius, n_clusters) %>%
  pivot_wider(
    names_from = method_radius,
    values_from = n_clusters
  ) %>%
  arrange(AEZ)

write_csv(
  clusters_by_aez_tbl,
  file.path(summary_dir, "clusters_by_AEZ_long.csv")
)

write_csv(
  clusters_by_aez_wide,
  file.path(summary_dir, "clusters_by_AEZ_wide.csv")
)
message("Finished bundle diagnostics and comparison tables.")