# Summarise the NHM BII v2.1.1 rasters on the published five-year schedule.
#
# The release is a single aggregate-taxa BII map for each year.  This script
# keeps it separate from the fitted PREDICTS response functions: it converts
# the documented 0--100 raster values to proportions, calculates a direct
# area-weighted global mean, and uses a common Natural Earth boundary set for
# national, continental, and broad-region summaries.

if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("terra", quietly = TRUE)) {
  stop("NHM v2.1.1 summarisation requires the installed R packages 'sf' and 'terra'.", call. = FALSE)
}

nhm_v211_download_url <- paste0(
  "https://data.nhm.ac.uk/dataset/ed428544-c494-4289-961c-1a5adf8fae74/",
  "resource/c4c281c4-befa-4e1b-a162-ba2f25e5ae82/download/",
  "bii-v2-1-1-nhm-data-portal.zip"
)

if (!file.exists(nhm_v211_zip_path)) {
  stop(
    "NHM BII v2.1.1 archive is required but was not found at ", nhm_v211_zip_path,
    ". Download it manually from ", nhm_v211_download_url,
    " and place it at that exact path before running this script.",
    call. = FALSE
  )
}
assert_file_exists(luh2_country_boundaries_path, "Natural Earth country boundaries")

dir.create(nhm_v211_extract_dir, recursive = TRUE, showWarnings = FALSE)
archive_members <- utils::unzip(nhm_v211_zip_path, list = TRUE)$Name
if (length(archive_members) == 0L) {
  stop("NHM BII v2.1.1 archive contains no files: ", nhm_v211_zip_path, call. = FALSE)
}
if (!all(file.exists(file.path(nhm_v211_extract_dir, archive_members)))) {
  utils::unzip(nhm_v211_zip_path, exdir = nhm_v211_extract_dir)
}

# The portal currently distributes a small wrapper ZIP containing the actual
# data ZIP. Support both that layout and a direct archive without imposing a
# filename assumption on the inner archive.
nested_archives <- list.files(nhm_v211_extract_dir, pattern = "\\.zip$", recursive = TRUE,
                              full.names = TRUE, ignore.case = TRUE)
for (nested_archive in nested_archives) {
  nested_members <- utils::unzip(nested_archive, list = TRUE)$Name
  nested_tifs <- nested_members[grepl("\\.tif(f)?$", nested_members, ignore.case = TRUE)]
  if (length(nested_tifs) > 0L && !all(file.exists(file.path(dirname(nested_archive), nested_tifs)))) {
    utils::unzip(nested_archive, exdir = dirname(nested_archive))
  }
}

all_tifs <- list.files(nhm_v211_extract_dir, pattern = "\\.tif(f)?$", recursive = TRUE,
                       full.names = TRUE, ignore.case = TRUE)
if (length(all_tifs) == 0L) {
  stop("No GeoTIFF files were found after extracting ", nhm_v211_zip_path, call. = FALSE)
}

find_year_raster <- function(year) {
  year_pattern <- paste0("(^|[^0-9])", year, "([^0-9]|$)")
  candidates <- all_tifs[
    grepl("bii", basename(all_tifs), ignore.case = TRUE) &
      grepl(year_pattern, basename(all_tifs), perl = TRUE)
  ]
  if (length(candidates) != 1L) {
    stop(
      "Expected exactly one BII GeoTIFF for ", year, "; found ", length(candidates),
      ". Check the v2.1.1 archive layout or extend find_year_raster() for its filename convention.",
      call. = FALSE
    )
  }
  candidates[[1]]
}

# Match the country construction used by the LUH2 workflow.  Using one
# boundary product means national differences reflect the BII surfaces and
# resolution, not mismatched border datasets.
boundaries <- sf::st_read(luh2_country_boundaries_path, quiet = TRUE)
iso3 <- as.character(boundaries$ISO_A3_EH)
bad_iso <- is.na(iso3) | iso3 == "-99" | !nzchar(iso3)
iso3[bad_iso] <- as.character(boundaries$ADM0_A3[bad_iso])
keep <- !is.na(iso3) & iso3 != "-99" & nzchar(iso3)
boundaries <- boundaries[keep, ]
boundaries$country_iso3 <- iso3[keep]
boundaries$country <- as.character(boundaries$ADMIN)
boundaries$continent <- as.character(boundaries$CONTINENT)
boundaries$predicts_region <- dplyr::case_when(
  boundaries$continent %in% c("North America", "South America") ~ "Americas",
  boundaries$continent %in% c("Africa", "Asia", "Europe", "Oceania") ~ boundaries$continent,
  TRUE ~ NA_character_
)
boundaries$zone_id <- seq_len(nrow(boundaries))
boundary_lookup <- data.table::as.data.table(sf::st_drop_geometry(
  boundaries[, c("zone_id", "country_iso3", "country", "continent", "predicts_region")]
))

normalise_bii_raster <- function(raster, year) {
  range_values <- as.numeric(terra::global(raster, fun = "range", na.rm = TRUE)[1, ])
  if (length(range_values) != 2L || any(!is.finite(range_values)) || range_values[[1]] < -1e-6) {
    stop("NHM v2.1.1 BII raster has an invalid value range for ", year, call. = FALSE)
  }
  if (range_values[[2]] <= 1.000001) {
    list(raster = raster, raw_min = range_values[[1]], raw_max = range_values[[2]], scale = "already_proportion")
  } else if (range_values[[2]] <= 100.0001) {
    list(raster = raster / 100, raw_min = range_values[[1]], raw_max = range_values[[2]], scale = "percent_divided_by_100")
  } else {
    stop(
      "NHM v2.1.1 BII raster values exceed the documented 0--100 scale for ", year,
      ": maximum ", signif(range_values[[2]], 8), ".",
      call. = FALSE
    )
  }
}

zonal_sum <- function(values, zones, value_name) {
  result <- terra::zonal(values, zones, fun = "sum", na.rm = TRUE)
  result <- data.table::as.data.table(result)
  if (ncol(result) != 2L) {
    stop("Unexpected zonal-summary structure while calculating ", value_name, call. = FALSE)
  }
  data.table::setnames(result, names(result), c("zone_id", value_name))
  result
}

summarise_one_year <- function(year) {
  raster_path <- find_year_raster(year)
  raw_raster <- terra::rast(raster_path)
  if (terra::nlyr(raw_raster) != 1L) {
    stop("Expected one raster layer for NHM v2.1.1 year ", year, call. = FALSE)
  }
  normalised <- normalise_bii_raster(raw_raster, year)
  bii <- normalised$raster
  cell_area <- terra::cellSize(bii, unit = "km")
  valid_area <- terra::ifel(is.na(bii), NA, cell_area)
  numerator <- as.numeric(terra::global(bii * cell_area, fun = "sum", na.rm = TRUE)[1, 1])
  denominator <- as.numeric(terra::global(valid_area, fun = "sum", na.rm = TRUE)[1, 1])
  if (!is.finite(denominator) || denominator <= 0) {
    stop("NHM v2.1.1 raster has no non-missing land area for ", year, call. = FALSE)
  }

  zones <- terra::rasterize(terra::vect(boundaries), bii, field = "zone_id", background = NA,
                             touches = FALSE)
  country_sums <- merge(
    zonal_sum(bii * cell_area, zones, "numerator"),
    zonal_sum(valid_area, zones, "total_area_km2"),
    by = "zone_id", all = TRUE
  )
  countries <- merge(boundary_lookup, country_sums, by = "zone_id", all.x = TRUE)
  countries <- countries[is.finite(total_area_km2) & total_area_km2 > 0]
  countries[, `:=`(
    year = as.integer(year),
    published_bii = numerator / total_area_km2
  )]
  data.table::setcolorder(countries, c(
    "year", "country_iso3", "country", "continent", "predicts_region",
    "published_bii", "total_area_km2", "numerator", "zone_id"
  ))

  continents <- countries[!is.na(continent), .(
    numerator = sum(numerator, na.rm = TRUE),
    total_area_km2 = sum(total_area_km2, na.rm = TRUE)
  ), by = .(year, continent)]
  continents[, published_bii := numerator / total_area_km2]
  data.table::setcolorder(continents, c("year", "continent", "published_bii", "total_area_km2", "numerator"))

  regions <- countries[!is.na(predicts_region), .(
    numerator = sum(numerator, na.rm = TRUE),
    total_area_km2 = sum(total_area_km2, na.rm = TRUE)
  ), by = .(year, predicts_region)]
  regions[, published_bii := numerator / total_area_km2]
  data.table::setcolorder(regions, c("year", "predicts_region", "published_bii", "total_area_km2", "numerator"))

  list(
    global = data.table::data.table(
      year = as.integer(year), published_bii = numerator / denominator,
      total_area_km2 = denominator, numerator = numerator
    ),
    countries = countries,
    continents = continents,
    regions = regions,
    raster = data.table::data.table(
      year = as.integer(year), raster_path = normalizePath(raster_path),
      raw_min = normalised$raw_min, raw_max = normalised$raw_max,
      scale_conversion = normalised$scale, raster_crs = terra::crs(raw_raster, proj = TRUE),
      n_cells = as.integer(terra::ncell(raw_raster))
    )
  )
}

message("Summarising NHM BII v2.1.1 rasters for ", paste(nhm_v211_years, collapse = ", "), ".")
summary_list <- lapply(nhm_v211_years, summarise_one_year)
published_global <- data.table::rbindlist(lapply(summary_list, `[[`, "global"))
published_countries <- data.table::rbindlist(lapply(summary_list, `[[`, "countries"))
published_continents <- data.table::rbindlist(lapply(summary_list, `[[`, "continents"))
published_regions <- data.table::rbindlist(lapply(summary_list, `[[`, "regions"))
raster_index <- data.table::rbindlist(lapply(summary_list, `[[`, "raster"))

write_csv_safe(published_global, file.path(output_dir, "bii_v211_published_global.csv"))
write_csv_safe(published_countries, file.path(output_dir, "bii_v211_published_countries.csv"))
write_csv_safe(published_continents, file.path(output_dir, "bii_v211_published_continents.csv"))
write_csv_safe(published_regions, file.path(output_dir, "bii_v211_published_regions.csv"))
write_csv_safe(raster_index, file.path(output_dir, "bii_v211_published_raster_index.csv"))

message("Wrote NHM v2.1.1 global, country, continent, and region summaries to ", output_dir)
