# =====================================================
# Build analysis-ready transition tables
# =====================================================

# -----------------------
# One row per cluster-year pair with consecutive observations.
# consecutive observations within a single cluster are of interest, 
# not between t1 and t3 if a cluster has 3+ years of data
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
    AEZ, cluster_id, buffer_km,
    n_sites,
    n_matched_tiles,
    n_matched_tiles_with_ha,
    n_matched_tiles_missing_ha,
    tagged_any_tile,
    tagged_ha_tile
  )

# -----------------------
# Deforestation accumulated over each OV interval
# annual deforestation is incremental, so interval change is the sum
# from year_t1 through year_t2, inclusive
# -----------------------

interval_defor <- cluster_year_pairs %>%
  left_join(
    cluster_buffer_meta %>%
      select(AEZ, cluster_id, buffer_km),
    by = c("AEZ", "cluster_id")
  ) %>%
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
    n_tiles_with_ha_mean = mean(n_tiles_with_ha, na.rm = TRUE),
    .groups = "drop"
  )


# -----------------------
# Main report table
# -----------------------

cluster_pair_report <- cluster_year_pairs %>%
  left_join(
    cluster_buffer_meta,
    by = c("AEZ", "cluster_id")
  ) %>%
  left_join(
    interval_defor,
    by = c("AEZ", "cluster_id", "buffer_km", "year_t1", "year_t2")
  ) %>%
  mutate(
    delta_defor_annualized = delta_defor_ha / (year_gap+1),
    inverse_change = !is.na(delta_ov) & !is.na(delta_defor_ha) &
      ((delta_ov > 0 & delta_defor_ha < 0) |
         (delta_ov < 0 & delta_defor_ha > 0)),
    aligned_change = !is.na(delta_ov) & !is.na(delta_defor_ha) &
      ((delta_ov > 0 & delta_defor_ha > 0) |
         (delta_ov < 0 & delta_defor_ha < 0))
  ) %>%
  arrange(AEZ, cluster_id, year_t1, year_t2)

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

cluster_tag_status <- cluster_buffer %>%
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
  ) %>%
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