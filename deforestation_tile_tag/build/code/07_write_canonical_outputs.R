# =====================================================
# Write canonical build outputs
# =====================================================

# -----------------------
# Write tabular outputs
# -----------------------

write_csv_safe(
  defor_tile_year,
  file.path(canonical_tabular_dir, "defor_tile_year.csv")
)

write_csv_safe(
  cluster_year_ov,
  file.path(canonical_tabular_dir, "cluster_year_ov.csv")
)

write_csv_safe(
  cluster_buffer_tile,
  file.path(canonical_tabular_dir, "cluster_buffer_tile.csv")
)

write_csv_safe(
  cluster_buffer_year_defor,
  file.path(canonical_tabular_dir, "cluster_buffer_year_defor.csv")
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

message("Tabular output files:")
print(list.files(canonical_tabular_dir, full.names = TRUE))

message("Spatial output files:")
print(list.files(canonical_spatial_dir, full.names = TRUE))