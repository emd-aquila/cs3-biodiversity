# -----------------------------------------------------------------------------
# Derive the V2 PREDICTS site population-density covariate for the direct
# 2010/2030 LUH2 comparison.
# -----------------------------------------------------------------------------
# The NCAR SSP2 grid is used at its 2010 time slice so that the training and
# projection covariates have consistent units and spatial support. This is a
# reproducible comparison input, not a reconstruction of the survey-year GPW
# extraction in the published workflow.

if (!requireNamespace("terra", quietly = TRUE)) {
  stop("Preparing V2 population inputs requires the installed R package 'terra'.", call. = FALSE)
}
assert_file_exists(predicts_site_path, "prepared PREDICTS site table")
assert_file_exists(v2_luh2_population_2010_path, "NCAR SSP2 2010 population grid")

site <- data.table::as.data.table(readRDS(predicts_site_path))
assert_has_cols(site, c("study_id", "site_id", "longitude", "latitude"), "prepared PREDICTS site table")
if (anyDuplicated(site, by = c("study_id", "site_id"))) {
  stop("Prepared PREDICTS site table must have one row per study/site.", call. = FALSE)
}

population <- terra::rast(v2_luh2_population_2010_path)
cell_area <- terra::cellSize(population, unit = "km")
points <- terra::vect(
  as.data.frame(site[, .(longitude, latitude)]),
  geom = c("longitude", "latitude"), crs = "OGC:CRS84"
)
# A few published PREDICTS coordinates fall on a population-grid no-data pixel
# (often just offshore at the reporting precision). Bilinear extraction uses
# the surrounding grid instead of discarding those valid biodiversity sites.
population_count <- terra::extract(population, points, method = "bilinear")[[2L]]
population_area <- terra::extract(cell_area, points, method = "bilinear")[[2L]]
human_population_density <- as.numeric(population_count) / as.numeric(population_area)
keep <- is.finite(human_population_density) & human_population_density >= 0
if (sum(!keep) / nrow(site) > v2_max_missing_site_pressure_share) {
  missing <- which(!keep)
  stop(
    "NCAR population grid does not cover ", length(missing), " PREDICTS site(s), exceeding v2_max_missing_site_pressure_share; example: ",
    site$study_id[[missing[[1L]]]], "/", site$site_id[[missing[[1L]]]], call. = FALSE
  )
}
if (any(!keep)) {
  warning("Population grid does not cover ", sum(!keep), " PREDICTS site(s); they will be excluded from V2 fitting.", call. = FALSE)
}

pressures <- site[keep, .(study_id, site_id)]
pressures[, human_population_density := human_population_density[keep]]
write_csv_safe(pressures, v2_site_pressure_path)

diagnostic <- data.table::data.table(
  n_sites = nrow(pressures), n_sites_excluded = sum(!keep),
  minimum_people_per_km2 = min(pressures$human_population_density),
  median_people_per_km2 = stats::median(pressures$human_population_density),
  maximum_people_per_km2 = max(pressures$human_population_density),
  population_grid = v2_luh2_population_2010_path
)
write_csv_safe(diagnostic, file.path(output_dir, "bii_v2_site_population_diagnostic.csv"))
message("Wrote V2 PREDICTS site population pressures: ", v2_site_pressure_path)
