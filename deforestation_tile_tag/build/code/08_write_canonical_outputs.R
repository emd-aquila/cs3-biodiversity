# =====================================================
# Write canonical build outputs
# =====================================================

if (!exists("canonical_tabular_dir") || !exists("canonical_spatial_dir")) {
  stop("Output directories are not defined. Run set_cluster_run_paths() first.")
}

legacy_tabular_paths <- file.path(
  canonical_tabular_dir,
  c("cluster_buffer_tile.csv", "cluster_buffer_year_defor.csv")
)

file.remove(legacy_tabular_paths[file.exists(legacy_tabular_paths)])

# -----------------------
# Write tabular outputs
# -----------------------

write_csv_safe(
  defor_tile_year,
  file.path(canonical_tabular_dir, "defor_tile_year.csv")
)

write_csv_safe(
  cluster_year_ov %>%
    mutate(
      across(
        where(is.numeric) & !any_of(c("year", "n_sites_year")),
        ~ round(.x, 3)
      )
    ),
  file.path(canonical_tabular_dir, "cluster_year_ov.csv")
)

write_csv_safe(
  cluster_buffer_tile,
  file.path(canonical_tabular_dir, "matched_clusters_tiles.csv")
)

write_csv_safe(
  cluster_buffer_country,
  file.path(canonical_tabular_dir, "matched_clusters_countries.csv")
)

write_csv_safe(
  cluster_buffer_year_defor %>%
    mutate(
      across(
        starts_with("defor_ha_"),
        ~ round(.x, 3)
      )
    ),
  file.path(canonical_tabular_dir, "cluster_year_defor.csv")
)

# -----------------------
# Write spatial outputs
# -----------------------

write_gpkg_safe(
  defor_tile_geometry,
  file.path(canonical_spatial_dir, "defor_tile_geometry.gpkg")
)

write_gpkg_safe(
  cluster_sites,
  file.path(canonical_spatial_dir, "cluster_sites.gpkg")
)

write_gpkg_safe(
  cluster_buffer,
  file.path(canonical_spatial_dir, "cluster_buffer.gpkg")
)

message("Wrote outputs for current run:")
if (exists("cluster_file_path")) {
  message("  cluster file: ", basename(cluster_file_path))
}
message("  tabular dir: ", canonical_tabular_dir)
message("  spatial dir: ", canonical_spatial_dir)

message("Tabular output files:")
print(list.files(canonical_tabular_dir, full.names = TRUE))
