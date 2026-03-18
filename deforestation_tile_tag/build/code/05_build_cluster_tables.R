# =====================================================
# Build canonical biodiversity cluster tables
# Creates:
#   - cluster_sites    : one row per site observation
#   - cluster_year_ov  : one row per cluster-year
# =====================================================

# -----------------------
# Canonical cluster site table
# one row per site observation
# -----------------------

cluster_sites <- cluster_raw %>%
  transmute(
    sample_id = as.character(sample_id),
    AEZ = as.character(AEZ),
    year = as.integer(year),
    cluster_id = as.character(cluster_id),
    ov = as.numeric(ov_score),
    latitude = as.numeric(Latitude),
    longitude = as.numeric(Longitude),
    method = as.character(method),
    dist_to_medoid = as.numeric(dist_to_medoid),
  ) %>%
  filter(
    !is.na(sample_id),
    !is.na(AEZ),
    !is.na(cluster_id),
    !is.na(year),
    !is.na(latitude),
    !is.na(longitude)
  ) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(analysis_crs)

assert_no_duplicate_keys(
  st_drop_geometry(cluster_sites),
  c("sample_id"),
  "cluster_sites"
)

# -----------------------
# Canonical cluster-year OV table
# one row per AEZ-cluster-year
# -----------------------

cluster_year_ov <- cluster_sites %>%
  st_drop_geometry() %>%
  group_by(AEZ, cluster_id, year) %>%
  summarise(
    median_ov_year = median(ov, na.rm = TRUE),
    n_sites_year = n(),
    .groups = "drop"
  ) %>%
  arrange(AEZ, cluster_id, year)

assert_no_duplicate_keys(
  cluster_year_ov,
  c("AEZ", "cluster_id", "year"),
  "cluster_year_ov"
)

# -----------------------
# Simple build audit
# -----------------------

message("Built cluster tables:")
message("  cluster_sites rows: ", nrow(cluster_sites))
message("  cluster_year_ov rows: ", nrow(cluster_year_ov))
message(
  "  unique clusters: ",
  cluster_year_ov %>% distinct(AEZ, cluster_id) %>% nrow()
)