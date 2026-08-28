# Build local WDPA inside/outside labels without sending BioTIME coordinates.
suppressPackageStartupMessages({ library(dplyr); library(readr); library(sf); library(curl) })

file_arg <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
code_dir <- if (length(file_arg)) dirname(normalizePath(file_arg[[1]])) else getwd()
topic_dir <- normalizePath(file.path(code_dir, ".."))
repo_root <- normalizePath(file.path(topic_dir, "..", "..", ".."))
input_dir <- file.path(topic_dir, "input")
dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
ov_path <- file.path(repo_root, "ov_metric", "02_ov_calculation", "calculation", "output", "biotime_ov_scores.csv")
countries_path <- file.path(repo_root, "00_spatial_data", "naturalearth_10m_admin_0_countries", "ne_10m_admin_0_countries.shp")
membership_path <- file.path(input_dir, "wdpa_site_membership.csv")
audit_path <- file.path(input_dir, "wdpa_download_audit.csv")
page_cache_dir <- file.path(input_dir, "wdpa_page_cache")
dir.create(page_cache_dir, recursive = TRUE, showWarnings = FALSE)
service_url <- "https://geoapps.esri.co/server/rest/services/Hosted/WDPA_poly_Feb2024/FeatureServer/0/query"

download_page <- function(iso3, offset) {
  cache_path <- file.path(page_cache_dir, sprintf("%s_%08d.geojson", iso3, offset))
  if (file.exists(cache_path)) return(st_read(cache_path, quiet = TRUE))
  params <- list(f = "geojson", where = sprintf("parent_iso3 = '%s' AND pa_def = '1' AND status IN ('Designated', 'Established', 'Inscribed')", iso3), outFields = "wdpaid,name,status,status_yr,parent_iso3", returnGeometry = "true", resultOffset = as.character(offset), resultRecordCount = "200", maxAllowableOffset = "0.02", outSR = "4326")
  url <- paste0(service_url, "?", paste(names(params), vapply(params, curl_escape, character(1)), sep = "=", collapse = "&"))
  result <- curl_fetch_memory(url)
  if (result$status_code != 200) stop("WDPA request failed for ", iso3, " at offset ", offset)
  writeBin(result$content, cache_path)
  st_read(cache_path, quiet = TRUE)
}

download_country <- function(iso3) {
  pages <- list(); offset <- 0L
  repeat {
    message("WDPA ", iso3, ": records ", offset + 1L, " onward")
    page <- download_page(iso3, offset)
    if (!nrow(page)) break
    pages[[length(pages) + 1L]] <- page
    if (nrow(page) < 200L) break
    offset <- offset + nrow(page)
  }
  if (!length(pages)) return(st_sf(wdpaid = numeric(), status_yr = integer(), geometry = st_sfc(crs = 4326)))
  do.call(rbind, pages)
}

sf_use_s2(FALSE)
ov <- read_csv(ov_path, show_col_types = FALSE) |> filter(database == "BioTIME") |> group_by(assemblage_id) |> summarise(longitude = first(longitude), latitude = first(latitude), first_year = min(sample_year), .groups = "drop")
countries <- st_read(countries_path, quiet = TRUE) |> st_make_valid() |> select(iso3 = ISO_A3_EH)
sites <- st_as_sf(ov, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
sites_country <- st_join(sites, countries, join = st_within, left = TRUE) |> group_by(assemblage_id) |> summarise(longitude = first(longitude), latitude = first(latitude), first_year = first(first_year), iso3 = first(na.omit(iso3), default = NA_character_), geometry = first(geometry), .groups = "drop")
iso3s <- sort(unique(na.omit(sites_country$iso3)))
target_args <- commandArgs(trailingOnly = TRUE)
target_iso3 <- if (length(target_args)) target_args[[1]] else Sys.getenv("WDPA_TARGET_ISO3", unset = "")
if (nzchar(target_iso3)) iso3s <- intersect(iso3s, target_iso3)
# Chile's first page exceeds the public-service timeout even after geometry
# simplification. Keep its sites visibly unclassified instead of silently
# calling them outside protected areas.
skip_iso3s <- c("CHL")
base_membership <- sites_country |> filter(is.na(iso3) | iso3 %in% skip_iso3s) |> st_drop_geometry() |> transmute(assemblage_id, protected_area = "Not classified", first_year, country_iso3 = iso3, wdpa_polygon_matches = NA_integer_, earliest_wdpa_status_year = NA_real_, wdpa_release = "Feb2024 public WDPA polygon service")
membership <- if (file.exists(membership_path)) read_csv(membership_path, show_col_types = FALSE) else base_membership
membership <- bind_rows(membership, anti_join(base_membership, membership |> select(assemblage_id), by = "assemblage_id"))
completed_iso3s <- unique(na.omit(membership$country_iso3))
audit <- if (file.exists(audit_path)) read_csv(audit_path, show_col_types = FALSE) else tibble(country_iso3 = character(), wdpa_polygons_downloaded = integer(), service_url = character(), simplified_boundary_tolerance_degrees = numeric())
for (iso3 in setdiff(setdiff(iso3s, skip_iso3s), completed_iso3s)) {
  country_sites <- sites_country |> filter(iso3 == .env$iso3)
  pa <- download_country(iso3) |> st_make_valid()
  matches <- st_intersects(country_sites, pa)
  inside <- vapply(seq_along(matches), function(i) { idx <- matches[[i]]; if (!length(idx)) return(FALSE); years <- pa$status_yr[idx]; any(is.na(years) | years <= country_sites$first_year[i]) }, logical(1))
  min_status_year <- vapply(matches, function(idx) if (!length(idx)) NA_real_ else suppressWarnings(min(pa$status_yr[idx], na.rm = TRUE)), numeric(1))
  min_status_year[is.infinite(min_status_year)] <- NA_real_
  country_membership <- country_sites |> st_drop_geometry() |> transmute(assemblage_id, protected_area = if_else(inside, "Inside", "Outside"), first_year, country_iso3 = iso3, wdpa_polygon_matches = lengths(matches), earliest_wdpa_status_year = min_status_year, wdpa_release = "Feb2024 public WDPA polygon service")
  membership <- bind_rows(membership, country_membership)
  audit <- bind_rows(audit, tibble(country_iso3 = iso3, wdpa_polygons_downloaded = nrow(pa), service_url, simplified_boundary_tolerance_degrees = 0.02))
  write_csv(membership, membership_path)
  write_csv(audit, audit_path)
  rm(pa, matches, country_sites, country_membership); gc()
}
message("Wrote local WDPA membership: ", membership_path)
