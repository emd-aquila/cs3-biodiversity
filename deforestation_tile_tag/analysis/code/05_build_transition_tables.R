# =====================================================
# Build analysis-ready transition tables
# =====================================================

# -----------------------
# One row per cluster-year pair with consecutive observations.
# 
# Each cluster-year pair has consecutive observations within a single cluster,
# not non-consecutive (i.e. t1 and t3) if a cluster has 3+ years of data.
# -----------------------

cluster_year_pairs <- cluster_year_ov %>%
  arrange(AEZ, cluster_id, year) %>%
  group_by(AEZ, cluster_id) %>%
  mutate(
    year_t2 = lead(year),
    ov_t2 = lead(median_ov_year),
    n_sites_t2 = lead(n_sites_year)
  ) %>%
  ungroup() %>%
  filter(!is.na(year_t2)) %>%
  transmute(
    AEZ,
    cluster_id,
    year_t1 = year,
    year_t2 = as.integer(year_t2),
    year_gap = year_t2 - year_t1,
    ov_t1 = median_ov_year,
    ov_t2 = ov_t2,
    n_sites_t1 = n_sites_year,
    n_sites_t2 = n_sites_t2,
    delta_ov = ov_t2 - ov_t1,
    delta_ov_annualized = (ov_t2 - ov_t1) / (year_t2 - year_t1)
  )

# -----------------------
# Cluster-level tagging / footprint metadata
# -----------------------

cluster_buffer_meta <- cluster_buffer %>%
  st_drop_geometry() %>%
  distinct(
    AEZ,
    cluster_id,
    buffer_km,
    n_sites,
    n_matched_tiles,
    n_matched_tiles_with_ha,
    n_matched_tiles_missing_ha,
    tagged_any_tile,
    tagged_ha_tile
  )

assert_has_cols(
  cluster_buffer_meta,
  c(
    "AEZ", "cluster_id", "buffer_km",
    "n_matched_tiles_with_ha", "tagged_ha_tile"
  ),
  "cluster_buffer_meta"
)

# -----------------------
# Restrict regression inputs early.
# This avoids carrying non-ha-tagged clusters through the heavy interval join.
# -----------------------

cluster_pairs_tagged_ha <- cluster_year_pairs %>%
  inner_join(
    cluster_buffer_meta %>%
      filter(tagged_ha_tile, n_matched_tiles_with_ha > 0),
    by = c("AEZ", "cluster_id")
  )

# -----------------------
# Deforestation accumulated over each OV interval.
# Annual deforestation is incremental, so interval change is the sum
# from year_t1 through year_t2, inclusive.
# -----------------------

interval_defor_tagged_ha <- cluster_pairs_tagged_ha %>%
  select(AEZ, cluster_id, buffer_km, year_t1, year_t2) %>%
  left_join(
    cluster_buffer_year_defor,
    by = c("AEZ", "cluster_id", "buffer_km"),
    relationship = "many-to-many"
  ) %>%
  filter(year >= year_t1, year <= year_t2) %>%
  group_by(AEZ, cluster_id, buffer_km, year_t1, year_t2) %>%
  summarise(
    delta_defor_ha = sum(defor_total_ha_year, na.rm = TRUE),
    n_defor_years = n_distinct(year),
    n_defor_years_expected = first(year_t2 - year_t1 + 1L),
    .groups = "drop"
  )

# -----------------------
# Regression-ready cluster-pair deltas.
# One row per consecutive cluster-year pair with hectare-level tile coverage
# and non-missing deforestation information over the OV interval.
# -----------------------

cluster_deltas <- cluster_pairs_tagged_ha %>%
  left_join(
    interval_defor_tagged_ha,
    by = c("AEZ", "cluster_id", "buffer_km", "year_t1", "year_t2")
  ) %>%
  mutate(
    delta_defor_ha_annualized = dplyr::if_else(
      !is.na(n_defor_years) & n_defor_years > 0,
      delta_defor_ha / n_defor_years,
      NA_real_
    ),
    inverse_change = !is.na(delta_ov) & !is.na(delta_defor_ha) &
      ((delta_ov > 0 & delta_defor_ha < 0) |
         (delta_ov < 0 & delta_defor_ha > 0))
  ) %>%
  filter(!is.na(delta_defor_ha)) %>%
  transmute(
    AEZ,
    cluster_id,
    n_matched_tiles_with_ha,
    year_t1,
    year_t2,
    year_gap,
    ov_t1,
    ov_t2,
    delta_ov,
    n_sites_t1,
    n_sites_t2,
    delta_defor_ha,
    delta_defor_ha_annualized,
    inverse_change
  ) %>%
  arrange(AEZ, cluster_id, year_t1, year_t2)

assert_has_cols(
  cluster_deltas,
  c(
    "AEZ", "cluster_id", "n_matched_tiles_with_ha",
    "year_t1", "year_t2", "year_gap",
    "ov_t1", "ov_t2", "delta_ov",
    "n_sites_t1", "n_sites_t2",
    "delta_defor_ha", "delta_defor_ha_annualized",
    "inverse_change"
  ),
  "cluster_deltas"
)

# -----------------------
# Cluster-year panel
# one row per cluster-year for the built buffer
# -----------------------

cluster_year_panel <- cluster_buffer_year_defor %>%
  left_join(
    cluster_year_ov %>%
      select(AEZ, cluster_id, year, median_ov_year, n_sites_year),
    by = c("AEZ", "cluster_id", "year")
  ) %>%
  left_join(
    cluster_buffer_meta,
    by = c("AEZ", "cluster_id", "buffer_km")
  ) %>%
  arrange(AEZ, cluster_id, year)

# -----------------------
# Tile-level footprint summary
# -----------------------

cluster_tile_coverage <- cluster_buffer_tile %>%
  group_by(AEZ, cluster_id, buffer_km) %>%
  summarise(
    n_tiles = n_distinct(tile_id),
    n_countries = n_distinct(country_name),
    n_tiles_with_any_ha_info = sum(has_ha_info, na.rm = TRUE),
    mean_years_with_ha = mean(n_years_with_ha, na.rm = TRUE),
    median_years_with_ha = median(n_years_with_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(AEZ, cluster_id)

# -----------------------
# Cluster subsets for tagging diagnostics
# is the cluster tagged? how many to any tile? how many to a tile
# with hectare-level data?
# -----------------------

cluster_tag_status <- cluster_buffer_meta %>%
  arrange(AEZ, cluster_id)

clusters_tagged_any_tile <- cluster_tag_status %>%
  filter(tagged_any_tile)

clusters_tagged_ha_tile <- cluster_tag_status %>%
  filter(tagged_ha_tile)

# -----------------------
# Temporary outputs
# -----------------------

write_csv_safe(
  cluster_year_panel,
  file.path(analysis_tmp_dir, "cluster_year_panel.csv")
)

write_csv_safe(
  cluster_tile_coverage,
  file.path(analysis_tmp_dir, "cluster_tile_coverage.csv")
)
