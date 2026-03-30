# =====================================================
# Build analysis-ready transition tables for all buffers
# =====================================================

# -----------------------
# Calculate cluster-year pairs (not buffer-dependent)
#
# one row per cluster-year pair with consecutive observations.
# Each pair has consecutive observations within a single cluster, not
# non-consecutive (i.e. t1 and t3) if a cluster has 3+ years of data.
# -----------------------


# -----------------------
# Preconditions
# -----------------------

required_objects <- c(
  "current_buffer_km",
  "current_buffer_key",
  "current_output_dirs",
  "cluster_year_ov",
  "cluster_buffer_all",
  "cluster_buffer_tile_all",
  "cluster_buffer_year_defor_all"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    paste0(
      "05_build_transition_tables.R is missing required objects: ",
      paste(missing_objects, collapse = ", ")
    ),
    call. = FALSE
  )
}

if (is.na(current_buffer_km)) {
  stop("current_buffer_km is NA. Run set_buffer_output_dirs(buffer_km) first.", call. = FALSE)
}

log_buffer_run(current_buffer_km)


# -----------------------
# Cluster-year pairs
#
# One row per consecutive cluster-year pair within a cluster.
# This table is not buffer-dependent, but it is used downstream
# after restricting to clusters tagged under the current buffer.
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
# Filter current-run canonical tables to current buffer
# -----------------------

cluster_buffer_this <- cluster_buffer_all %>%
  filter(buffer_km == current_buffer_km)

cluster_buffer_tile_this <- cluster_buffer_tile_all %>%
  filter(buffer_km == current_buffer_km)

cluster_buffer_year_defor_this <- cluster_buffer_year_defor_all %>%
  filter(buffer_km == current_buffer_km)

# -----------------------
# Handle empty current buffer gracefully
# -----------------------

if (nrow(cluster_buffer_this) == 0) {
  warning(
    "No rows found in cluster_buffer_all for buffer_km = ",
    current_buffer_km,
    ". Creating empty outputs for this buffer."
  )
  
  cluster_buffer_meta <- tibble()
  cluster_pairs_tagged_ha <- tibble()
  interval_defor_tagged_ha <- tibble()
  cluster_deltas <- tibble()
  cluster_year_panel <- tibble()
  cluster_tile_coverage <- tibble()
  cluster_tag_status <- tibble()
  clusters_tagged_any_tile <- tibble()
  clusters_tagged_ha_tile <- tibble()
  
  write_csv_safe(
    cluster_year_panel,
    file.path(current_output_dirs$tmp_dir, "cluster_year_panel.csv")
  )
  
  write_csv_safe(
    cluster_tile_coverage,
    file.path(current_output_dirs$tmp_dir, "cluster_tile_coverage.csv")
  )
  
  message("Finished 05_build_transition_tables.R for empty buffer: ", current_buffer_km)
} else {
  
  # -----------------------
  # Cluster-level tagging / footprint metadata
  # One row per AEZ-cluster-buffer
  # -----------------------
  
  cluster_buffer_meta <- cluster_buffer_this %>%
    sf::st_drop_geometry() %>%
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
    paste0("cluster_buffer_meta_", current_buffer_key)
  )
  
  # -----------------------
  # Restrict regression inputs to clusters tagged to >=1 ha tile
  # -----------------------
  
  cluster_pairs_tagged_ha <- cluster_year_pairs %>%
    inner_join(
      cluster_buffer_meta %>%
        filter(tagged_ha_tile, n_matched_tiles_with_ha > 0),
      by = c("AEZ", "cluster_id")
    )
  
  # -----------------------
  # Deforestation accumulated over each OV interval
  # -----------------------
  
  interval_defor_tagged_ha <- cluster_pairs_tagged_ha %>%
    select(AEZ, cluster_id, buffer_km, year_t1, year_t2) %>%
    left_join(
      cluster_buffer_year_defor_this,
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
  # Regression-ready cluster-pair deltas
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
        (
          (delta_ov > 0 & delta_defor_ha < 0) |
            (delta_ov < 0 & delta_defor_ha > 0)
        )
    ) %>%
    filter(!is.na(delta_defor_ha)) %>%
    transmute(
      AEZ,
      cluster_id,
      buffer_km,
      n_matched_tiles_with_ha,
      year_t1,
      year_t2,
      year_gap,
      ov_t1,
      ov_t2,
      delta_ov,
      delta_ov_annualized,
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
      "AEZ", "cluster_id", "buffer_km", "n_matched_tiles_with_ha",
      "year_t1", "year_t2", "year_gap",
      "ov_t1", "ov_t2", "delta_ov", "delta_ov_annualized",
      "n_sites_t1", "n_sites_t2",
      "delta_defor_ha", "delta_defor_ha_annualized",
      "inverse_change"
    ),
    paste0("cluster_deltas_", current_buffer_key)
  )
  
  # -----------------------
  # Cluster-year panel
  # One row per AEZ-cluster-buffer-year where deforestation is available
  # -----------------------
  
  cluster_year_panel <- cluster_buffer_year_defor_this %>%
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
  
  cluster_tile_coverage <- cluster_buffer_tile_this %>%
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
  # -----------------------
  
  cluster_tag_status <- cluster_buffer_meta %>%
    arrange(AEZ, cluster_id)
  
  clusters_tagged_any_tile <- cluster_tag_status %>%
    filter(tagged_any_tile)
  
  clusters_tagged_ha_tile <- cluster_tag_status %>%
    filter(tagged_ha_tile)
  
  # -----------------------
  # Temporary outputs for current buffer
  # -----------------------
  
  write_csv_safe(
    cluster_year_panel,
    file.path(current_output_dirs$tmp_dir, "cluster_year_panel.csv")
  )
  
  write_csv_safe(
    cluster_tile_coverage,
    file.path(current_output_dirs$tmp_dir, "cluster_tile_coverage.csv")
  )
  
  message("Finished 05_build_transition_tables.R")
  if (exists("current_cluster_stub")) {
    message("  cluster run: ", current_cluster_stub)
  }
  message("  buffer_km: ", current_buffer_km)
  message("  cluster_year_pairs rows: ", nrow(cluster_year_pairs))
  message("  cluster_pairs_tagged_ha rows: ", nrow(cluster_pairs_tagged_ha))
  message("  interval_defor_tagged_ha rows: ", nrow(interval_defor_tagged_ha))
  message("  cluster_deltas rows: ", nrow(cluster_deltas))
  message("  cluster_year_panel rows: ", nrow(cluster_year_panel))
  message("  cluster_tile_coverage rows: ", nrow(cluster_tile_coverage))
}