# =====================================================
# Load and prepare the tagged site data used for clustering.
# This stage reads the upstream tagged observations table, constructs one
# row per unique site, and attaches the year-coverage metadata needed
# for the clustering year rule.
# =====================================================

# -----------------------
# Read tagged site observations
# -----------------------

model_df_tagged <- read_csv(tagged_sites_file, show_col_types = FALSE) %>% 
  mutate(
    lat_r = round(Latitude, 4),
    lon_r = round(Longitude, 4),
    year  = as.integer(substr(Sample_midpoint, 1, 4))
  )

message("Loaded tagged site data:")
message("  rows: ", nrow(model_df_tagged))

# -----------------------
# Construct unique site table
# -----------------------

# Keep one record per AEZ and rounded location. These are the spatial
# points we cluster within each AEZ.
unique_sites <- model_df_tagged %>% 
  distinct(AEZ, lat_r, lon_r, .keep_all = TRUE) %>% 
  arrange(AEZ)

# Convert to sf points in geographic coordinates (EPSG 4326),
# then project into a meter-based CRS so the clustering radius is applied
# in meters rather than degrees.
unique_sf <- unique_sites %>% 
  st_as_sf(
    coords = c("lon_r", "lat_r"),
    crs = 4326,
    remove = FALSE,
    na.fail = FALSE
  )

# Project to equal-area meter-based CRS
sites_m <- st_transform(unique_sf, 6933)

# Extract projected coordinates
xy <- st_coordinates(sites_m)

sites_tbl <- sites_m %>% 
  st_drop_geometry() %>% 
  mutate(
    AEZ = as.character(AEZ),
    x   = xy[,1],
    y   = xy[,2]
  )

# Check geometry drop didn't change row count
stopifnot(nrow(sites_tbl) == nrow(unique_sites))

# -----------------------
# Compute year coverage per site
# -----------------------

site_years <- model_df_tagged %>% 
  group_by(AEZ, lat_r, lon_r) %>% 
  summarize(
    n_years = n_distinct(year),
    years_single = if_else(n_years == 1L, first(year), NA_integer_),
    .groups = "drop"
  )

# Join site-level year information onto the clustering table
sites_tbl <- sites_tbl %>% 
  left_join(site_years, by = c("AEZ", "lat_r", "lon_r")) %>% 
  select(
    AEZ,
    lat_r,
    lon_r,
    x,
    y,
    n_years,
    years_single
  )

message("Prepared unique site table:")
message("  rows: ", nrow(sites_tbl))
