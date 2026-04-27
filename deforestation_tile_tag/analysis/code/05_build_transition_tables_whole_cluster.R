# =====================================================
# Build analysis-ready whole-cluster transition tables for all buffers
# =====================================================

required_objects <- c(
  "current_buffer_km",
  "current_buffer_key",
  "current_output_dirs",
  "ov_score_specs",
  "cluster_year_ov",
  "cluster_buffer_all",
  "cluster_buffer_tile_all",
  "cluster_buffer_year_defor_all",
  "cluster_medoids"
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

compute_lagged_interval_end <- function(interval_start_year, interval_end_year, year_gap) {
  dplyr::case_when(
    is.na(interval_start_year) | is.na(interval_end_year) | is.na(year_gap) ~ NA_integer_,
    year_gap >= 3L ~ as.integer(interval_end_year - 2L),
    year_gap == 2L ~ as.integer(interval_end_year - 1L),
    year_gap == 1L ~ as.integer(interval_start_year),
    TRUE ~ as.integer(interval_end_year)
  )
}

# -----------------------
# Cluster first-to-last intervals
# One row per cluster with at least two observed years.
# -----------------------

available_ov_score_specs <- get_available_ov_score_specs(cluster_year_ov)
cluster_year_pairs <- build_whole_cluster_ov_table(cluster_year_ov, available_ov_score_specs)
ov_variant_transition_cols <- available_ov_score_specs %>%
  filter(ov_method != "ov_full") %>%
  select(t1_col, t2_col, delta_col, annualized_col) %>%
  unlist(use.names = FALSE)

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
    round_numeric_cols(cluster_year_panel, digits = 3),
    file.path(current_output_dirs$tmp_dir, "cluster_year_panel.csv")
  )

  write_csv_safe(
    round_numeric_cols(cluster_tile_coverage, digits = 3),
    file.path(current_output_dirs$tmp_dir, "cluster_tile_coverage.csv")
  )

  message("Finished 05_build_transition_tables.R for empty buffer: ", current_buffer_km)
} else {

  # -----------------------
  # Cluster-level tagging / footprint metadata
  # -----------------------

  cluster_buffer_meta <- cluster_buffer_this %>%
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
    paste0("cluster_buffer_meta_", current_buffer_key)
  )

  # -----------------------
  # Restrict to clusters tagged to >=1 ha tile
  # -----------------------

  cluster_pairs_tagged_ha <- cluster_year_pairs %>%
    inner_join(
      cluster_buffer_meta %>%
        filter(tagged_ha_tile, n_matched_tiles_with_ha > 0),
      by = c("AEZ", "cluster_id")
    )

  # -----------------------
  # Deforestation accumulated from first through last observed year
  # -----------------------

  interval_defor_tagged_ha <- cluster_pairs_tagged_ha %>%
    mutate(
      lagged_year_end = compute_lagged_interval_end(year_start, year_final, year_gap)
    ) %>%
    dplyr::select(AEZ, cluster_id, buffer_km, year_start, year_final, year_gap, lagged_year_end) %>%
    left_join(
      cluster_buffer_year_defor_this,
      by = c("AEZ", "cluster_id", "buffer_km"),
      relationship = "many-to-many"
    ) %>%
    filter(year >= year_start, year <= year_final) %>%
    group_by(AEZ, cluster_id, buffer_km, year_start, year_final) %>%
    summarise(
      delta_defor_ha_total_raw = sum(defor_ha_total_raw, na.rm = TRUE),
      delta_defor_ha_total_avg = sum(defor_ha_total_avg, na.rm = TRUE),
      delta_defor_ha_total_rel_pct = sum(defor_ha_total_rel_pct, na.rm = TRUE),
      delta_defor_ha_crops_raw = sum(defor_ha_crops_raw, na.rm = TRUE),
      delta_defor_ha_crops_avg = sum(defor_ha_crops_avg, na.rm = TRUE),
      delta_defor_ha_crops_rel_pct = sum(defor_ha_crops_rel_pct, na.rm = TRUE),
      delta_defor_ha_total_raw_lagged = sum(
        dplyr::if_else(year <= first(lagged_year_end), defor_ha_total_raw, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_total_avg_lagged = sum(
        dplyr::if_else(year <= first(lagged_year_end), defor_ha_total_avg, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_total_rel_pct_lagged = sum(
        dplyr::if_else(year <= first(lagged_year_end), defor_ha_total_rel_pct, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_crops_raw_lagged = sum(
        dplyr::if_else(year <= first(lagged_year_end), defor_ha_crops_raw, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_crops_avg_lagged = sum(
        dplyr::if_else(year <= first(lagged_year_end), defor_ha_crops_avg, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_crops_rel_pct_lagged = sum(
        dplyr::if_else(year <= first(lagged_year_end), defor_ha_crops_rel_pct, 0),
        na.rm = TRUE
      ),
      n_defor_years = n_distinct(year),
      n_defor_years_lagged = sum(year <= first(lagged_year_end), na.rm = TRUE),
      n_defor_years_expected = first(year_final - year_start + 1L),
      .groups = "drop"
    )

  # -----------------------
  # Regression-ready cluster deltas
  # -----------------------

  cluster_deltas <- cluster_pairs_tagged_ha %>%
    left_join(
      interval_defor_tagged_ha,
      by = c("AEZ", "cluster_id", "buffer_km", "year_start", "year_final")
    ) %>%
    left_join(
      cluster_medoids %>%
        dplyr::select(AEZ, cluster_id, medoid_latitude, medoid_longitude),
      by = c("AEZ", "cluster_id")
    ) %>%
    mutate(
      delta_defor_ha = delta_defor_ha_total_raw,
      delta_defor_ha_lagged = delta_defor_ha_total_raw_lagged,
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
      medoid_latitude,
      medoid_longitude,
      n_matched_tiles_with_ha,
      n_defor_years,
      n_defor_years_lagged,
      year_start,
      year_final,
      year_gap,
      ov_start = ov_t1,
      ov_final = ov_t2,
      delta_ov,
      delta_ov_annualized,
      pick(any_of(ov_variant_transition_cols)),
      n_sites_start,
      n_sites_final,
      delta_defor_ha,
      delta_defor_ha_total_raw,
      delta_defor_ha_total_avg,
      delta_defor_ha_total_rel_pct,
      delta_defor_ha_crops_raw,
      delta_defor_ha_crops_avg,
      delta_defor_ha_crops_rel_pct,
      delta_defor_ha_lagged,
      delta_defor_ha_total_raw_lagged,
      delta_defor_ha_total_avg_lagged,
      delta_defor_ha_total_rel_pct_lagged,
      delta_defor_ha_crops_raw_lagged,
      delta_defor_ha_crops_avg_lagged,
      delta_defor_ha_crops_rel_pct_lagged,
      inverse_change
    ) %>%
    arrange(AEZ, cluster_id, year_start, year_final)

  assert_has_cols(
    cluster_deltas,
    c(
      "AEZ", "cluster_id", "buffer_km",
      "medoid_latitude", "medoid_longitude",
      "n_matched_tiles_with_ha",
      "n_defor_years", "n_defor_years_lagged",
      "year_start", "year_final", "year_gap",
      "ov_start", "ov_final", "delta_ov", "delta_ov_annualized",
      "n_sites_start", "n_sites_final",
      "delta_defor_ha_total_raw", "delta_defor_ha_total_avg", "delta_defor_ha_total_rel_pct",
      "delta_defor_ha_crops_raw", "delta_defor_ha_crops_avg", "delta_defor_ha_crops_rel_pct",
      "delta_defor_ha_lagged",
      "delta_defor_ha_total_raw_lagged", "delta_defor_ha_total_avg_lagged", "delta_defor_ha_total_rel_pct_lagged",
      "delta_defor_ha_crops_raw_lagged", "delta_defor_ha_crops_avg_lagged", "delta_defor_ha_crops_rel_pct_lagged",
      "delta_defor_ha",
      "inverse_change"
    ),
    paste0("cluster_deltas_", current_buffer_key)
  )

  # -----------------------
  # Cluster-year panel
  # -----------------------

  cluster_year_panel <- cluster_buffer_year_defor_this %>%
    left_join(
      cluster_year_ov %>%
        dplyr::select(
          AEZ,
          cluster_id,
          year,
          all_of(unique(available_ov_score_specs$cluster_year_col)),
          n_sites_year
        ),
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
      matched_area_ha = sum(intersection_area_ha, na.rm = TRUE),
      overlap_share_sum = sum(normalized_overlap_share, na.rm = TRUE),
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
  message("  cluster_deltas rows: ", nrow(cluster_deltas))
}
