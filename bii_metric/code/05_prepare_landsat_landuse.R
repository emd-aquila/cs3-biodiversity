# Translate the user-supplied Landsat land-use product to the same BII pressure
# classes used in PREDICTS. The crosswalk must explicitly resolve forest state.
landsat_landuse <- prepare_landuse_with_crosswalk(
  landsat_landuse_path,
  landsat_crosswalk_path,
  "Landsat"
)
saveRDS(landsat_landuse, landsat_prepared_path, compress = "gzip")
message("Wrote: ", landsat_prepared_path)

landsat_audit <- landsat_landuse[, .(
  n_cells = uniqueN(cell_id),
  total_area_km2 = sum(unique(area_km2)),
  mean_share = mean(share)
), by = .(scenario, year, pressure_class)]
write_csv_safe(landsat_audit, file.path(output_dir, "landsat_bii_crosswalk_audit.csv"))
