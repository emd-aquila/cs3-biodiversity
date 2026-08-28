# =====================================================
# Build canonical cluster-buffer Hansen deforestation tables
# Creates:
#   - cluster_buffer.gpkg         : one row per cluster-buffer
#   - matched_clusters_tiles.csv  : compatibility footprint table, one pseudo Hansen row per cluster-buffer
#   - cluster_year_defor.csv      : one row per cluster-buffer-year
# =====================================================

sf::sf_use_s2(FALSE)

# -----------------------
# Preconditions
# -----------------------

required_objects <- c(
  "cluster_sites",
  "country_geometry",
  "buffer_km_vals",
  "cluster_footprints_cache",
  "hansen_cluster_buffer_geojson",
  "hansen_cluster_year_defor_file",
  "hansen_years"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    "Missing required objects for 07_build_cluster_footprints.R: ",
    paste(missing_objects, collapse = ", ")
  )
}

read_hansen_gee_output <- function(path) {
  if (!file.exists(path)) {
    stop(
      paste0(
        "Missing Hansen Earth Engine summary CSV: ", path, "\n",
        "The cluster-buffer GeoJSON needed by the GEE script has been written to:\n",
        "  ", hansen_cluster_buffer_geojson, "\n\n",
        "Run from the repository root:\n",
        "  python3 04_deforestation_tile_tag/build/code/export_hansen_cluster_stats.py \\\n",
        "    --input ", hansen_cluster_buffer_geojson, " \\\n",
        "    --output ", path, "\n\n",
        "Then rerun 04_deforestation_tile_tag/run_tag.R."
      ),
      call. = FALSE
    )
  }

  hansen_raw <- readr::read_csv(path, show_col_types = FALSE)

  required_cols <- c(
    "AEZ",
    "cluster_id",
    "buffer_km",
    "year",
    "defor_ha_total_raw",
    "hansen_land_area_ha",
    "hansen_treecover2000_mean_pct"
  )

  assert_has_cols(hansen_raw, required_cols, "hansen_gee_output")

  if (!"hansen_treecover2000_equiv_ha" %in% names(hansen_raw)) {
    hansen_raw <- hansen_raw %>%
      mutate(
        hansen_treecover2000_equiv_ha =
          as.numeric(hansen_land_area_ha) * as.numeric(hansen_treecover2000_mean_pct) / 100
      )
  }

  hansen_raw %>%
    transmute(
      AEZ = as.character(AEZ),
      cluster_id = as.character(cluster_id),
      buffer_km = as.numeric(buffer_km),
      year = as.integer(year),
      n_tiles = 1L,
      n_tiles_with_ha = dplyr::if_else(hansen_land_area_ha > 0, 1L, 0L),
      defor_ha_total_raw = coalesce(as.numeric(defor_ha_total_raw), 0),
      # Compatibility aliases only. Hansen regressions use the raw total hectares.
      defor_ha_total_avg = defor_ha_total_raw,
      defor_ha_total_rel_pct = defor_ha_total_raw,
      defor_ha_crops_raw = NA_real_,
      defor_ha_crops_avg = NA_real_,
      defor_ha_crops_rel_pct = NA_real_,
      hansen_land_area_ha = as.numeric(hansen_land_area_ha),
      hansen_treecover2000_equiv_ha = as.numeric(hansen_treecover2000_equiv_ha),
      hansen_treecover2000_mean_pct = as.numeric(hansen_treecover2000_mean_pct)
    ) %>%
    filter(year %in% hansen_years) %>%
    arrange(AEZ, cluster_id, buffer_km, year)
}

# -----------------------
# Build unified cluster polygons, then buffer them
# one row per AEZ-cluster-buffer
# -----------------------

cluster_buffer <- read_or_build(
  path = cluster_footprints_cache,
  build_fn = function() {
    cluster_polygons <- cluster_sites %>%
      group_by(AEZ, cluster_id) %>%
      summarise(
        n_sites = n(),
        .groups = "drop"
      ) %>%
      st_make_valid() %>%
      mutate(
        geometry = sf::st_convex_hull(geometry)
      )

    purrr::map_dfr(
      buffer_km_vals,
      function(buffer_km) {
        buffer_m <- buffer_km * 1000
        # Some 0 km convex hulls are points/lines. Give them a one-pixel
        # footprint so Earth Engine can summarize raster cells.
        effective_buffer_m <- if (buffer_m == 0) 30 else buffer_m

        cluster_polygons %>%
          mutate(
            geometry = st_buffer(geometry, dist = effective_buffer_m)
          ) %>%
          st_make_valid() %>%
          st_collection_extract("POLYGON", warn = FALSE) %>%
          mutate(buffer_km = buffer_km) %>%
          relocate(buffer_km, .after = cluster_id)
      }
    )
  }
)

assert_no_duplicate_keys(
  st_drop_geometry(cluster_buffer),
  c("AEZ", "cluster_id", "buffer_km"),
  "cluster_buffer"
)

cluster_buffer_export <- cluster_buffer %>%
  mutate(
    hansen_cluster_buffer_id = paste(AEZ, cluster_id, buffer_km, sep = "__")
  ) %>%
  dplyr::select(hansen_cluster_buffer_id, AEZ, cluster_id, buffer_km) %>%
  sf::st_transform(4326)

sf::st_write(
  cluster_buffer_export,
  dsn = hansen_cluster_buffer_geojson,
  delete_dsn = TRUE,
  quiet = TRUE
)
message("Wrote Hansen cluster-buffer GeoJSON: ", hansen_cluster_buffer_geojson)

# -----------------------
# Intersect cluster-buffer polygons with country polygons
# then collapse to one row per unique cluster-buffer-country match
# one row per AEZ-cluster-buffer-country
# -----------------------

cluster_buffer_country <- cluster_buffer %>%
  dplyr::select(AEZ, cluster_id, buffer_km) %>%
  sf::st_intersection(
    country_geometry %>%
      dplyr::select(country_id, country_iso3, country_name, country_name_long, sovereign_name)
  ) %>%
  mutate(
    country_intersection_area_ha = as.numeric(sf::st_area(.)) / 10000
  ) %>%
  filter(is.finite(country_intersection_area_ha), country_intersection_area_ha > 0) %>%
  st_drop_geometry() %>%
  group_by(
    AEZ,
    cluster_id,
    buffer_km,
    country_id,
    country_iso3,
    country_name,
    country_name_long,
    sovereign_name
  ) %>%
  summarise(
    country_intersection_area_ha = sum(country_intersection_area_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(AEZ, cluster_id, buffer_km) %>%
  mutate(
    normalized_country_overlap_share =
      country_intersection_area_ha / sum(country_intersection_area_ha, na.rm = TRUE)
  ) %>%
  ungroup()

assert_no_duplicate_keys(
  cluster_buffer_country,
  c("AEZ", "cluster_id", "buffer_km", "country_id"),
  "cluster_buffer_country"
)

assert_groupwise_share_sum(
  cluster_buffer_country,
  keys = c("AEZ", "cluster_id", "buffer_km"),
  share_col = "normalized_country_overlap_share",
  data_name = "cluster_buffer_country"
)

cluster_buffer_country_summary <- cluster_buffer_country %>%
  arrange(
    AEZ,
    cluster_id,
    buffer_km,
    desc(normalized_country_overlap_share),
    country_name
  ) %>%
  group_by(AEZ, cluster_id, buffer_km) %>%
  summarise(
    n_matched_countries = n_distinct(country_id),
    primary_country_id = dplyr::first(country_id),
    primary_country_iso3 = dplyr::first(country_iso3),
    primary_country_name = dplyr::first(country_name),
    primary_country_name_long = dplyr::first(country_name_long),
    primary_sovereign_name = dplyr::first(sovereign_name),
    primary_country_overlap_share = dplyr::first(normalized_country_overlap_share),
    matched_country_ids = paste(sort(unique(country_id)), collapse = ";"),
    matched_country_names = paste(sort(unique(country_name)), collapse = ";"),
    .groups = "drop"
  )

# -----------------------
# Load Hansen Earth Engine summary
# -----------------------

cluster_buffer_year_defor <- read_hansen_gee_output(hansen_cluster_year_defor_file)

assert_no_duplicate_keys(
  cluster_buffer_year_defor,
  c("AEZ", "cluster_id", "buffer_km", "year"),
  "cluster_buffer_year_defor"
)

hansen_buffer_meta <- cluster_buffer_year_defor %>%
  group_by(AEZ, cluster_id, buffer_km) %>%
  summarise(
    hansen_land_area_ha = if (all(is.na(hansen_land_area_ha))) {
      NA_real_
    } else {
      max(hansen_land_area_ha, na.rm = TRUE)
    },
    hansen_treecover2000_equiv_ha = if (all(is.na(hansen_treecover2000_equiv_ha))) {
      NA_real_
    } else {
      max(hansen_treecover2000_equiv_ha, na.rm = TRUE)
    },
    hansen_treecover2000_mean_pct = dplyr::first(hansen_treecover2000_mean_pct),
    has_hansen_data = is.finite(hansen_land_area_ha) & hansen_land_area_ha > 0,
    .groups = "drop"
  )

cluster_buffer <- cluster_buffer %>%
  mutate(
    cluster_area_ha = as.numeric(sf::st_area(.)) / 10000
  ) %>%
  left_join(hansen_buffer_meta, by = c("AEZ", "cluster_id", "buffer_km")) %>%
  left_join(cluster_buffer_country_summary, by = c("AEZ", "cluster_id", "buffer_km")) %>%
  mutate(
    has_hansen_data = coalesce(has_hansen_data, FALSE),
    n_matched_tiles = dplyr::if_else(has_hansen_data, 1L, 0L),
    n_matched_tiles_with_ha = dplyr::if_else(has_hansen_data, 1L, 0L),
    n_matched_tiles_missing_ha = 0L,
    tagged_any_tile = has_hansen_data,
    tagged_ha_tile = has_hansen_data,
    n_matched_countries = coalesce(n_matched_countries, 0L),
    hansen_tile_id = paste(AEZ, cluster_id, buffer_km, sep = "__")
  )

assert_no_duplicate_keys(
  st_drop_geometry(cluster_buffer),
  c("AEZ", "cluster_id", "buffer_km"),
  "cluster_buffer"
)

# -----------------------
# Compatibility footprint table
# Hansen uses the cluster buffer itself as the exposure unit.
# -----------------------

cluster_buffer_tile <- cluster_buffer %>%
  st_drop_geometry() %>%
  transmute(
    AEZ,
    cluster_id,
    buffer_km,
    tile_id = hansen_tile_id,
    country_name = primary_country_name,
    has_ha_info = tagged_ha_tile,
    intersection_area_ha = hansen_land_area_ha,
    normalized_overlap_share = 1,
    hansen_land_area_ha,
    hansen_treecover2000_equiv_ha,
    hansen_treecover2000_mean_pct
  ) %>%
  arrange(AEZ, cluster_id, buffer_km)

assert_no_duplicate_keys(
  cluster_buffer_tile,
  c("AEZ", "cluster_id", "buffer_km", "tile_id"),
  "cluster_buffer_tile"
)

defor_tile_geometry <- cluster_buffer %>%
  transmute(
    tile_id = hansen_tile_id,
    country_name = primary_country_name,
    has_ha_info = tagged_ha_tile,
    hansen_land_area_ha,
    hansen_treecover2000_equiv_ha,
    hansen_treecover2000_mean_pct,
    geometry
  )

defor_tile_year <- cluster_buffer_year_defor %>%
  left_join(
    cluster_buffer %>%
      st_drop_geometry() %>%
      dplyr::select(AEZ, cluster_id, buffer_km, tile_id = hansen_tile_id),
    by = c("AEZ", "cluster_id", "buffer_km")
  ) %>%
  transmute(
    tile_id,
    year,
    defor_total_ha = defor_ha_total_raw,
    defor_crops_ha = NA_real_,
    defor_livestock_ha = NA_real_
  )

message("Built Hansen cluster-buffer tables:")
message("  cluster_buffer rows: ", nrow(cluster_buffer))
message("  cluster_buffer_country rows: ", nrow(cluster_buffer_country))
message("  cluster_buffer_tile rows: ", nrow(cluster_buffer_tile))
message("  cluster_buffer_year_defor rows: ", nrow(cluster_buffer_year_defor))
