# =====================================================
# Build canonical Hansen support tables that do not depend on a cluster run.
# Cluster-buffer Hansen exposure is built in 07_build_cluster_footprints.R
# after run-specific cluster buffers are available.
# =====================================================

sf_use_s2(FALSE)

# -----------------------
# Natural Earth country geometry layer
# one row per country-level polygon unit
# -----------------------

country_geometry <- country_boundaries_raw %>%
  transmute(
    country_id = as.character(ADM0_A3),
    country_iso3 = dplyr::if_else(
      is.na(ISO_A3) | ISO_A3 == "-99",
      as.character(ADM0_A3),
      as.character(ISO_A3)
    ),
    country_name = as.character(ADMIN),
    country_name_long = as.character(NAME_LONG),
    sovereign_name = as.character(SOVEREIGNT),
    geometry
  ) %>%
  filter(!is.na(country_id), nzchar(country_id)) %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON", warn = FALSE) %>%
  st_transform(analysis_crs)

assert_no_duplicate_keys(
  st_drop_geometry(country_geometry),
  c("country_id"),
  "country_geometry"
)

defor_tile_attributes <- tibble()
defor_tile_year <- tibble(
  tile_id = character(),
  year = integer(),
  defor_total_ha = numeric(),
  defor_crops_ha = numeric(),
  defor_livestock_ha = numeric()
)
defor_tile_geometry <- sf::st_sf(
  tibble(
    tile_id = character(),
    country_name = character(),
    has_ha_info = logical()
  ),
  geometry = sf::st_sfc(crs = analysis_crs)
)

message("Built Hansen support tables:")
message("  Hansen annual loss years: ", min(hansen_years), "-", max(hansen_years))
message("  country_geometry rows: ", nrow(country_geometry))
