# Build a reusable modern-country lookup for LUH2's 0.25-degree grid.
#
# LUH2's bundled ccode grid uses a few historical aggregate territories.  This
# lookup instead assigns every land-containing grid-cell centre to a current
# Natural Earth administrative boundary, keeping national outputs interpretable
# in 2010 and 2030.  Coastal cells whose centres fall just outside a polygon
# are assigned to the nearest country and reported in the metadata.

if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("terra", quietly = TRUE)) {
  stop("The LUH2 grid lookup requires the installed R packages 'sf' and 'terra'.", call. = FALSE)
}
assert_file_exists(luh2_static_path, "LUH2 static grid file")
assert_file_exists(luh2_country_boundaries_path, "Natural Earth country boundaries")

force_rebuild <- identical(Sys.getenv("CS3_BII_REBUILD_LUH2_LOOKUP"), "1")
if (file.exists(luh2_grid_lookup_path) && !force_rebuild) {
  message("Using existing LUH2 country lookup: ", luh2_grid_lookup_path)
} else {
  water <- terra::rast(luh2_static_path, subds = "icwtr")
  water_values <- terra::values(water, mat = FALSE)
  # Terra cell order follows rows from north to south, matching the LUH2 NetCDF
  # latitude order.  Store zero-based ids so NumPy can index directly.
  land_cells <- which(is.finite(water_values) & water_values < 1)
  coordinates <- terra::xyFromCell(water, land_cells)
  points <- sf::st_as_sf(
    data.frame(grid_id = as.integer(land_cells - 1L), longitude = coordinates[, 1], latitude = coordinates[, 2]),
    coords = c("longitude", "latitude"), crs = "OGC:CRS84", remove = FALSE
  )

  boundaries <- sf::st_read(luh2_country_boundaries_path, quiet = TRUE)
  iso3 <- as.character(boundaries$ISO_A3_EH)
  bad_iso <- is.na(iso3) | iso3 == "-99" | !nzchar(iso3)
  iso3[bad_iso] <- as.character(boundaries$ADM0_A3[bad_iso])
  boundaries <- boundaries[, c("ADMIN", "CONTINENT")]
  boundaries$country <- as.character(boundaries$ADMIN)
  boundaries$continent <- as.character(boundaries$CONTINENT)
  boundaries$country_iso3 <- iso3
  boundaries$ADMIN <- NULL
  boundaries$CONTINENT <- NULL

  assigned <- sf::st_join(points, boundaries, join = sf::st_within, left = TRUE)
  unmatched <- is.na(assigned$country_iso3)
  if (any(unmatched)) {
    nearest <- sf::st_nearest_feature(assigned[unmatched, ], boundaries)
    assigned$country_iso3[unmatched] <- boundaries$country_iso3[nearest]
    assigned$country[unmatched] <- boundaries$country[nearest]
    assigned$continent[unmatched] <- boundaries$continent[nearest]
  }

  lookup <- data.table::as.data.table(sf::st_drop_geometry(assigned))
  lookup[, predicts_region := dplyr::case_when(
    continent %in% c("North America", "South America") ~ "Americas",
    continent %in% c("Africa", "Asia", "Europe", "Oceania") ~ continent,
    TRUE ~ NA_character_
  )]
  lookup[, country_assignment := ifelse(unmatched, "nearest_boundary", "within_boundary")]
  data.table::setorder(lookup, grid_id)
  write_csv_safe(lookup, luh2_grid_lookup_path)
  message("LUH2 country lookup: ", nrow(lookup), " land-containing cells; ",
          sum(unmatched), " assigned by nearest boundary.")
}
