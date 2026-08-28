# Classify BioTIME assemblage locations against the official August 2026 WDPCA
# release (the combined WDPA + WD-OECM database) entirely locally.
suppressPackageStartupMessages({ library(dplyr); library(readr); library(sf) })

file_arg <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
code_dir <- if (length(file_arg)) dirname(normalizePath(file_arg[[1]])) else getwd()
topic_dir <- normalizePath(file.path(code_dir, ".."))
repo_root <- normalizePath(file.path(topic_dir, "..", "..", ".."))
input_dir <- file.path(topic_dir, "input")
ov_path <- file.path(repo_root, "ov_metric", "02_ov_calculation", "calculation", "output", "biotime_ov_scores.csv")
countries_path <- file.path(repo_root, "00_spatial_data", "naturalearth_10m_admin_0_countries", "ne_10m_admin_0_countries.shp")
gdb_path <- file.path(input_dir, "WDPA_WDOECM_Aug2026_Public.gdb")
membership_path <- file.path(input_dir, "wdpca_site_membership.csv")
audit_path <- file.path(input_dir, "wdpca_membership_audit.csv")
stopifnot(file.exists(ov_path), file.exists(countries_path), dir.exists(gdb_path))

poly_layer <- "WDPA_WDOECM_poly_Aug2026"
point_layer <- "WDPA_WDOECM_point_Aug2026"
sf_use_s2(FALSE)

ov <- read_csv(ov_path, show_col_types = FALSE) |>
  filter(database == "BioTIME") |>
  group_by(assemblage_id) |>
  summarise(longitude = first(longitude), latitude = first(latitude), first_year = min(sample_year), .groups = "drop")
countries <- st_read(countries_path, quiet = TRUE) |>
  st_make_valid() |>
  select(country_iso3 = ISO_A3_EH)
sites <- st_as_sf(ov, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
sites <- st_join(sites, countries, join = st_within, left = TRUE) |>
  group_by(assemblage_id) |>
  summarise(longitude = first(longitude), latitude = first(latitude), first_year = first(first_year), country_iso3 = first(na.omit(country_iso3), default = NA_character_), geometry = first(geometry), .groups = "drop") |>
  st_transform(6933)

# WDPA practice represents point localities with reported area as circles.
wdpca_points <- st_read(gdb_path, layer = point_layer, quiet = TRUE) |>
  filter(STATUS != "Proposed", !is.na(REP_AREA), REP_AREA > 0) |>
  st_transform(6933) |>
  mutate(buffer_m = sqrt(REP_AREA * 1e6 / pi))
wdpca_points <- st_buffer(wdpca_points, dist = wdpca_points$buffer_m) |>
  select(SITE_ID, SITE_TYPE, STATUS_YR, PRNT_ISO3)

classify_country <- function(iso3) {
  country_sites <- sites |> filter(country_iso3 == iso3)
  if (!is.na(target_batch)) {
    country_sites <- country_sites |> arrange(assemblage_id)
    start <- (target_batch - 1L) * batch_size + 1L
    country_sites <- country_sites[start:min(start + batch_size - 1L, nrow(country_sites)), ]
  }
  # Query only polygons that touch an assemblage point, rather than loading a
  # country's entire protected-area inventory (important for the United States).
  country_wkt <- country_sites |> st_transform(4326) |> st_union() |> st_as_text()
  pa_poly <- st_read(gdb_path, layer = poly_layer, wkt_filter = country_wkt, options = "METHOD=SKIP", quiet = TRUE) |>
    filter(PRNT_ISO3 == iso3, STATUS != "Proposed") |>
    st_transform(6933) |>
    select(SITE_ID, SITE_TYPE, STATUS_YR)
  pa_point <- wdpca_points |> filter(PRNT_ISO3 == iso3) |> select(SITE_ID, SITE_TYPE, STATUS_YR)
  pa <- bind_rows(pa_poly, pa_point)
  if (!nrow(pa)) {
    return(country_sites |> st_drop_geometry() |> transmute(assemblage_id, protected_area = "Outside", first_year, country_iso3, wdpca_matches = 0L, wdpca_pa_matches = 0L, wdpca_oecm_matches = 0L, earliest_status_year = NA_real_))
  }
  matches <- st_intersects(country_sites, pa)
  valid_matches <- lapply(seq_along(matches), function(i) {
    idx <- matches[[i]]
    idx[is.na(pa$STATUS_YR[idx]) | pa$STATUS_YR[idx] <= country_sites$first_year[i]]
  })
  n_all <- lengths(matches)
  n_pa <- vapply(valid_matches, function(idx) sum(pa$SITE_TYPE[idx] == "PA"), integer(1))
  n_oecm <- vapply(valid_matches, function(idx) sum(pa$SITE_TYPE[idx] == "OECM"), integer(1))
  earliest <- vapply(valid_matches, function(idx) {
    if (!length(idx)) return(NA_real_)
    y <- pa$STATUS_YR[idx]; if (all(is.na(y))) NA_real_ else min(y, na.rm = TRUE)
  }, numeric(1))
  country_sites |> st_drop_geometry() |> transmute(assemblage_id, protected_area = if_else(lengths(valid_matches) > 0, "Inside", "Outside"), first_year, country_iso3, wdpca_matches = n_all, wdpca_pa_matches = n_pa, wdpca_oecm_matches = n_oecm, earliest_status_year = earliest)
}

country_codes <- sort(unique(na.omit(sites$country_iso3)))
target_args <- commandArgs(trailingOnly = TRUE)
target_batch <- NA_integer_
batch_size <- 100L
if (length(target_args)) {
  target_parts <- strsplit(target_args[[1]], ":", fixed = TRUE)[[1]]
  country_codes <- intersect(country_codes, target_parts[[1]])
  if (length(target_parts) > 1L) target_batch <- as.integer(target_parts[[2]])
}
existing <- if (file.exists(membership_path)) {
  read_csv(membership_path, show_col_types = FALSE) |> distinct(assemblage_id, .keep_all = TRUE)
} else {
  tibble()
}
# Checkpoint after every country.  The combined August 2026 WDPCA geodatabase is
# large, so a stopped batch must never discard classifications already completed.
if (is.na(target_batch)) {
  completed_iso3 <- unique(na.omit(existing$country_iso3))
  country_codes <- setdiff(country_codes, completed_iso3)
}
for (iso3 in country_codes) {
  message("Classifying ", iso3)
  country_membership <- classify_country(iso3)
  existing <- bind_rows(existing |> filter(!assemblage_id %in% country_membership$assemblage_id), country_membership) |>
    distinct(assemblage_id, .keep_all = TRUE)
  write_csv(existing, membership_path)
  message("Checkpointed ", nrow(existing), " assemblages")
}
unclassified <- sites |> filter(is.na(country_iso3)) |> st_drop_geometry() |> transmute(assemblage_id, protected_area = "Not classified", first_year, country_iso3, wdpca_matches = NA_integer_, wdpca_pa_matches = NA_integer_, wdpca_oecm_matches = NA_integer_, earliest_status_year = NA_real_)
membership <- bind_rows(existing, unclassified) |>
  distinct(assemblage_id, .keep_all = TRUE) |>
  mutate(wdpca_release = "WDPA_WDOECM_Aug2026_Public", classification = "polygons plus REP_AREA-buffered point localities; non-proposed sites designated by first observation")
write_csv(membership, membership_path)
write_csv(tibble(
  release = "WDPA_WDOECM_Aug2026_Public", n_assemblages = nrow(membership),
  n_inside = sum(membership$protected_area == "Inside"), n_outside = sum(membership$protected_area == "Outside"),
  n_not_classified = sum(membership$protected_area == "Not classified"),
  point_localities_buffered = nrow(wdpca_points)
), audit_path)
message("Wrote: ", membership_path)
