# =====================================================
# Load raw input data needed by the Hansen deforestation workflow.
# Hansen exposure values are produced by the Earth Engine export script
# after cluster-buffer polygons have been written in 07_build_cluster_footprints.R.
# =====================================================

# -----------------------
# Read country boundaries for country tagging
# -----------------------

country_boundaries_raw <- read_sf(country_boundaries_file) %>%
  st_make_valid()

# -----------------------
# Validate required columns
# -----------------------

assert_has_cols(
  country_boundaries_raw,
  c("ADMIN", "ADM0_A3", "ISO_A3", "SOVEREIGNT", "geometry"),
  "country_boundaries_raw"
)

message("Loaded Hansen workflow inputs:")
message("  Hansen asset: ", hansen_asset_id)
message("  country_boundaries_raw rows: ", nrow(country_boundaries_raw))
