# One-pass completion of the local WDPCA classification. This complements the
# country-checkpoint script by querying only PA/OECM features touching the union
# of all still-unclassified BioTIME locations.
suppressPackageStartupMessages({ library(dplyr); library(readr); library(sf) })
file_arg <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
code_dir <- if (length(file_arg)) dirname(normalizePath(file_arg[[1]])) else getwd()
topic_dir <- normalizePath(file.path(code_dir, ".."))
repo_root <- normalizePath(file.path(topic_dir, "..", "..", ".."))
input_dir <- file.path(topic_dir, "input")
gdb_path <- file.path(input_dir, "WDPA_WDOECM_Aug2026_Public.gdb")
membership_path <- file.path(input_dir, "wdpca_site_membership.csv")
candidate_path <- file.path(input_dir, "wdpca_site_membership_global_candidate.csv")
ov_path <- file.path(repo_root, "ov_metric", "02_ov_calculation", "calculation", "output", "biotime_ov_scores.csv")
countries_path <- file.path(repo_root, "00_spatial_data", "naturalearth_10m_admin_0_countries", "ne_10m_admin_0_countries.shp")
stopifnot(dir.exists(gdb_path))
sf_use_s2(FALSE)

ov <- read_csv(ov_path, show_col_types = FALSE) |>
  filter(database == "BioTIME") |>
  group_by(assemblage_id) |>
  summarise(longitude = first(longitude), latitude = first(latitude), first_year = min(sample_year), .groups = "drop")
countries <- st_read(countries_path, quiet = TRUE) |> st_make_valid() |> select(country_iso3 = ISO_A3_EH)
sites <- st_as_sf(ov, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  st_join(countries, join = st_within, left = TRUE) |> st_transform(6933)
existing <- if (file.exists(membership_path)) {
  read_csv(membership_path, show_col_types = FALSE) |> distinct(assemblage_id, .keep_all = TRUE)
} else {
  tibble(assemblage_id = character())
}
target <- sites |> filter(!assemblage_id %in% existing$assemblage_id, !is.na(country_iso3))
if (!nrow(target)) { write_csv(existing, candidate_path); quit(save = "no") }

message("One-pass classification of ", nrow(target), " remaining assemblages")
target_wkt <- target |> st_transform(4326) |> st_union() |> st_as_text()
polys <- st_read(gdb_path, layer = "WDPA_WDOECM_poly_Aug2026", wkt_filter = target_wkt, options = "METHOD=SKIP", quiet = TRUE) |>
  filter(STATUS != "Proposed") |> st_transform(6933) |> select(SITE_ID, SITE_TYPE, STATUS_YR)
points <- st_read(gdb_path, layer = "WDPA_WDOECM_point_Aug2026", quiet = TRUE) |>
  filter(STATUS != "Proposed", !is.na(REP_AREA), REP_AREA > 0) |> st_transform(6933) |>
  mutate(buffer_m = sqrt(REP_AREA * 1e6 / pi))
points <- st_buffer(points, dist = points$buffer_m) |> select(SITE_ID, SITE_TYPE, STATUS_YR)
pa <- bind_rows(polys, points)
matches <- st_intersects(target, pa)
valid_matches <- lapply(seq_along(matches), function(i) {
  z <- matches[[i]]
  z[is.na(pa$STATUS_YR[z]) | pa$STATUS_YR[z] <= target$first_year[i]]
})
completed <- target |> st_drop_geometry() |> transmute(
  assemblage_id, protected_area = if_else(lengths(valid_matches) > 0, "Inside", "Outside"), first_year, country_iso3,
  wdpca_matches = lengths(matches),
  wdpca_pa_matches = vapply(valid_matches, function(z) sum(pa$SITE_TYPE[z] == "PA"), integer(1)),
  wdpca_oecm_matches = vapply(valid_matches, function(z) sum(pa$SITE_TYPE[z] == "OECM"), integer(1)),
  earliest_status_year = vapply(valid_matches, function(z) if (!length(z) || all(is.na(pa$STATUS_YR[z]))) NA_real_ else min(pa$STATUS_YR[z], na.rm = TRUE), numeric(1))
)
unclassified <- sites |> filter(is.na(country_iso3)) |> st_drop_geometry() |> transmute(assemblage_id, protected_area = "Not classified", first_year, country_iso3, wdpca_matches = NA_integer_, wdpca_pa_matches = NA_integer_, wdpca_oecm_matches = NA_integer_, earliest_status_year = NA_real_)
all_membership <- bind_rows(existing |> filter(!assemblage_id %in% completed$assemblage_id), completed, unclassified) |>
  distinct(assemblage_id, .keep_all = TRUE) |>
  mutate(wdpca_release = "WDPA_WDOECM_Aug2026_Public", classification = "one-pass polygons plus REP_AREA-buffered point localities; non-proposed sites designated by first observation")
write_csv(all_membership, candidate_path)
message("Candidate coverage: ", nrow(all_membership))
