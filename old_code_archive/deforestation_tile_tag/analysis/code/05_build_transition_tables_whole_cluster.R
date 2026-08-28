# =====================================================
# Build analysis-ready whole-cluster transition tables for all buffers
# =====================================================

required_objects <- c(
  "current_buffer_km",
  "current_buffer_key",
  "current_output_dirs",
  "ov_score_specs",
  "cluster_year_ov",
  "cluster_sites",
  "cluster_buffer_all",
  "cluster_buffer_tile_all",
  "cluster_buffer_country_all",
  "cluster_buffer_year_defor_all",
  "cluster_medoids",
  "defor_tile_year"
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

compute_cutoff_interval_end <- function(interval_start_year, interval_end_year, year_gap) {
  dplyr::case_when(
    is.na(interval_start_year) | is.na(interval_end_year) | is.na(year_gap) ~ NA_integer_,
    year_gap >= 3L ~ as.integer(interval_end_year - 2L),
    year_gap == 2L ~ as.integer(interval_end_year - 1L),
    year_gap == 1L ~ as.integer(interval_start_year),
    TRUE ~ as.integer(interval_end_year)
  )
}

# -----------------------
# Filter current-run canonical tables to current buffer
# -----------------------

cluster_buffer_this <- cluster_buffer_all %>%
  filter(buffer_km == current_buffer_km)

cluster_buffer_tile_this <- cluster_buffer_tile_all %>%
  filter(buffer_km == current_buffer_km)

cluster_buffer_country_this <- cluster_buffer_country_all %>%
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
  cluster_pairs_tagged_defor <- tibble()
  interval_defor_tagged_defor <- tibble()
  cluster_deltas <- tibble()
  cluster_year_panel <- tibble()
  cluster_tile_coverage <- tibble()
  cluster_tag_status <- tibble()
  clusters_tagged_any_tile <- tibble()
  clusters_tagged_ha_tile <- tibble()
  analysis_unit_membership <- tibble()
  analysis_unit_summary <- tibble()
  analysis_unit_sites_this <- cluster_sites[0, ]
  analysis_unit_year_ov_this <- tibble()
  cluster_country_coverage <- tibble()

  write_csv_safe(
    round_numeric_cols(cluster_year_panel, digits = 3),
    file.path(current_output_dirs$tmp_dir, "cluster_year_panel.csv")
  )

  write_csv_safe(
    round_numeric_cols(cluster_tile_coverage, digits = 3),
    file.path(current_output_dirs$tmp_dir, "cluster_tile_coverage.csv")
  )

  write_csv_safe(
    round_numeric_cols(cluster_country_coverage, digits = 3),
    file.path(current_output_dirs$tmp_dir, "cluster_country_coverage.csv")
  )

  message("Finished 05_build_transition_tables.R for empty buffer: ", current_buffer_km)
} else {

  # -----------------------
  # Cluster-level tagging / footprint metadata
  # One row per AEZ-analysis-unit-buffer.
  # Exclusive single-tile clusters are collapsed here before OV medians
  # and deltas are calculated.
  # -----------------------

  analysis_unit_tables <- build_analysis_unit_tables(
    cluster_sites = cluster_sites,
    cluster_buffer_this = cluster_buffer_this,
    cluster_buffer_tile_this = cluster_buffer_tile_this,
    cluster_buffer_country_this = cluster_buffer_country_this,
    cluster_buffer_year_defor_this = cluster_buffer_year_defor_this,
    cluster_medoids = cluster_medoids,
    defor_tile_year = defor_tile_year,
    ov_score_specs = ov_score_specs,
    collapse_single_tile_clusters = collapse_single_tile_clusters
  )

  analysis_unit_membership <- analysis_unit_tables$membership
  analysis_unit_summary <- analysis_unit_tables$summary
  analysis_unit_sites_this <- analysis_unit_tables$cluster_sites
  analysis_unit_year_ov_this <- analysis_unit_tables$cluster_year_ov
  cluster_buffer_meta <- analysis_unit_tables$cluster_buffer_meta
  cluster_buffer_tile_this <- analysis_unit_tables$cluster_buffer_tile
  cluster_buffer_country_this <- analysis_unit_tables$cluster_buffer_country
  cluster_buffer_year_defor_this <- analysis_unit_tables$cluster_buffer_year_defor
  cluster_medoids_this <- analysis_unit_tables$cluster_medoids

  available_ov_score_specs <- get_available_ov_score_specs(analysis_unit_year_ov_this)
  cluster_year_pairs <- build_whole_cluster_ov_table(analysis_unit_year_ov_this, available_ov_score_specs)
  ov_variant_transition_cols <- available_ov_score_specs %>%
    filter(ov_method != "ov_full") %>%
    select(t1_col, t2_col, delta_col, annualized_col) %>%
    unlist(use.names = FALSE)

  assert_has_cols(
    cluster_buffer_meta,
    c(
      "AEZ", "cluster_id", "buffer_km",
      "n_matched_tiles", "tagged_any_tile",
      "n_matched_tiles_with_ha", "tagged_ha_tile",
      "n_matched_countries", "primary_country_id", "primary_country_name"
    ),
    paste0("cluster_buffer_meta_", current_buffer_key)
  )

  # -----------------------
  # Restrict to clusters tagged to >=1 deforestation tile.
  # Tiles without hectare rows are valid zero-deforestation tiles.
  # -----------------------

  cluster_pairs_tagged_defor <- cluster_year_pairs %>%
    inner_join(
      cluster_buffer_meta %>%
        filter(tagged_any_tile, n_matched_tiles > 0) %>%
        dplyr::select(
          -any_of(c(
            "hansen_land_area_ha",
            "hansen_treecover2000_equiv_ha",
            "hansen_treecover2000_mean_pct"
          ))
        ),
      by = c("AEZ", "cluster_id")
    )

  # -----------------------
  # Deforestation accumulated from first through last observed year
  # -----------------------

  interval_defor_tagged_defor <- cluster_pairs_tagged_defor %>%
    mutate(
      cutoff_year_end = compute_cutoff_interval_end(year_start, year_final, year_gap),
      lagged_year_start = year_start - 1L,
      lagged_year_end = year_final - 1L
    ) %>%
    dplyr::select(
      AEZ, cluster_id, buffer_km, year_start, year_final, year_gap,
      cutoff_year_end, lagged_year_start, lagged_year_end
    ) %>%
    left_join(
      cluster_buffer_year_defor_this,
      by = c("AEZ", "cluster_id", "buffer_km"),
      relationship = "many-to-many"
    ) %>%
    filter(year >= lagged_year_start, year <= year_final) %>%
    group_by(AEZ, cluster_id, buffer_km, year_start, year_final) %>%
    summarise(
      delta_defor_ha_total_raw = sum(
        dplyr::if_else(year >= first(year_start) & year <= first(year_final), defor_ha_total_raw, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_total_avg = sum(
        dplyr::if_else(year >= first(year_start) & year <= first(year_final), defor_ha_total_avg, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_total_rel_pct = sum(
        dplyr::if_else(year >= first(year_start) & year <= first(year_final), defor_ha_total_rel_pct, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_crops_raw = sum(
        dplyr::if_else(year >= first(year_start) & year <= first(year_final), defor_ha_crops_raw, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_crops_avg = sum(
        dplyr::if_else(year >= first(year_start) & year <= first(year_final), defor_ha_crops_avg, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_crops_rel_pct = sum(
        dplyr::if_else(year >= first(year_start) & year <= first(year_final), defor_ha_crops_rel_pct, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_total_raw_cutoff = sum(
        dplyr::if_else(year >= first(year_start) & year <= first(cutoff_year_end), defor_ha_total_raw, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_total_avg_cutoff = sum(
        dplyr::if_else(year >= first(year_start) & year <= first(cutoff_year_end), defor_ha_total_avg, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_total_rel_pct_cutoff = sum(
        dplyr::if_else(year >= first(year_start) & year <= first(cutoff_year_end), defor_ha_total_rel_pct, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_crops_raw_cutoff = sum(
        dplyr::if_else(year >= first(year_start) & year <= first(cutoff_year_end), defor_ha_crops_raw, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_crops_avg_cutoff = sum(
        dplyr::if_else(year >= first(year_start) & year <= first(cutoff_year_end), defor_ha_crops_avg, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_crops_rel_pct_cutoff = sum(
        dplyr::if_else(year >= first(year_start) & year <= first(cutoff_year_end), defor_ha_crops_rel_pct, 0),
        na.rm = TRUE
      ),
      delta_defor_ha_total_raw_lagged = dplyr::if_else(
        sum(year >= first(lagged_year_start) & year <= first(lagged_year_end), na.rm = TRUE) ==
          first(year_final - year_start + 1L),
        sum(defor_ha_total_raw[year >= first(lagged_year_start) & year <= first(lagged_year_end)], na.rm = TRUE),
        NA_real_
      ),
      delta_defor_ha_total_avg_lagged = dplyr::if_else(
        sum(year >= first(lagged_year_start) & year <= first(lagged_year_end), na.rm = TRUE) ==
          first(year_final - year_start + 1L),
        sum(defor_ha_total_avg[year >= first(lagged_year_start) & year <= first(lagged_year_end)], na.rm = TRUE),
        NA_real_
      ),
      delta_defor_ha_total_rel_pct_lagged = dplyr::if_else(
        sum(year >= first(lagged_year_start) & year <= first(lagged_year_end), na.rm = TRUE) ==
          first(year_final - year_start + 1L),
        sum(defor_ha_total_rel_pct[year >= first(lagged_year_start) & year <= first(lagged_year_end)], na.rm = TRUE),
        NA_real_
      ),
      delta_defor_ha_crops_raw_lagged = NA_real_,
      delta_defor_ha_crops_avg_lagged = NA_real_,
      delta_defor_ha_crops_rel_pct_lagged = NA_real_,
      hansen_land_area_ha = dplyr::first(hansen_land_area_ha),
      hansen_treecover2000_equiv_ha = dplyr::first(hansen_treecover2000_equiv_ha),
      hansen_treecover2000_mean_pct = dplyr::first(hansen_treecover2000_mean_pct),
      n_defor_years = sum(year >= first(year_start) & year <= first(year_final), na.rm = TRUE),
      n_defor_years_cutoff = sum(year >= first(year_start) & year <= first(cutoff_year_end), na.rm = TRUE),
      n_defor_years_lagged = sum(year >= first(lagged_year_start) & year <= first(lagged_year_end), na.rm = TRUE),
      n_defor_years_expected = first(year_final - year_start + 1L),
      .groups = "drop"
    ) %>%
    mutate(
      delta_defor_land_share_raw = dplyr::if_else(
        hansen_land_area_ha > 0,
        delta_defor_ha_total_raw / hansen_land_area_ha,
        NA_real_
      ),
      delta_defor_land_share_raw_lagged = dplyr::if_else(
        hansen_land_area_ha > 0,
        delta_defor_ha_total_raw_lagged / hansen_land_area_ha,
        NA_real_
      ),
      delta_defor_land_share_raw_cutoff = dplyr::if_else(
        hansen_land_area_ha > 0,
        delta_defor_ha_total_raw_cutoff / hansen_land_area_ha,
        NA_real_
      ),
      delta_defor_treecover_share_raw = dplyr::if_else(
        hansen_treecover2000_equiv_ha > 0,
        delta_defor_ha_total_raw / hansen_treecover2000_equiv_ha,
        NA_real_
      ),
      delta_defor_treecover_share_raw_lagged = dplyr::if_else(
        hansen_treecover2000_equiv_ha > 0,
        delta_defor_ha_total_raw_lagged / hansen_treecover2000_equiv_ha,
        NA_real_
      ),
      delta_defor_treecover_share_raw_cutoff = dplyr::if_else(
        hansen_treecover2000_equiv_ha > 0,
        delta_defor_ha_total_raw_cutoff / hansen_treecover2000_equiv_ha,
        NA_real_
      )
    )

  # -----------------------
  # Regression-ready cluster deltas
  # -----------------------

  cluster_deltas <- cluster_pairs_tagged_defor %>%
    left_join(
      interval_defor_tagged_defor,
      by = c("AEZ", "cluster_id", "buffer_km", "year_start", "year_final")
    ) %>%
    left_join(
      cluster_medoids_this %>%
        dplyr::select(AEZ, cluster_id, medoid_latitude, medoid_longitude),
      by = c("AEZ", "cluster_id")
    ) %>%
    mutate(
      delta_defor_ha = delta_defor_ha_total_raw,
      delta_defor_ha_cutoff = delta_defor_ha_total_raw_cutoff,
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
      n_matched_tiles,
      n_matched_tiles_with_ha,
      n_matched_countries,
      primary_country_id,
      primary_country_iso3,
      primary_country_name,
      primary_country_name_long,
      primary_sovereign_name,
      primary_country_overlap_share,
      matched_country_ids,
      matched_country_names,
      analysis_unit_id = cluster_id,
      analysis_unit_type,
      collapse_tile_id,
      n_member_clusters,
      member_cluster_ids,
      n_defor_years,
      n_defor_years_cutoff,
      n_defor_years_lagged,
      hansen_land_area_ha,
      hansen_treecover2000_equiv_ha,
      hansen_treecover2000_mean_pct,
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
      delta_defor_ha_cutoff,
      delta_defor_ha_total_raw_cutoff,
      delta_defor_ha_total_avg_cutoff,
      delta_defor_ha_total_rel_pct_cutoff,
      delta_defor_ha_crops_raw_cutoff,
      delta_defor_ha_crops_avg_cutoff,
      delta_defor_ha_crops_rel_pct_cutoff,
      delta_defor_ha_lagged,
      delta_defor_ha_total_raw_lagged,
      delta_defor_ha_total_avg_lagged,
      delta_defor_ha_total_rel_pct_lagged,
      delta_defor_land_share_raw,
      delta_defor_land_share_raw_cutoff,
      delta_defor_land_share_raw_lagged,
      delta_defor_treecover_share_raw,
      delta_defor_treecover_share_raw_cutoff,
      delta_defor_treecover_share_raw_lagged,
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
      "n_matched_tiles", "n_matched_tiles_with_ha",
      "n_matched_countries", "primary_country_id", "primary_country_name",
      "n_defor_years", "n_defor_years_cutoff", "n_defor_years_lagged",
      "hansen_land_area_ha", "hansen_treecover2000_equiv_ha", "hansen_treecover2000_mean_pct",
      "year_start", "year_final", "year_gap",
      "ov_start", "ov_final", "delta_ov", "delta_ov_annualized",
      "n_sites_start", "n_sites_final",
      "delta_defor_ha_total_raw", "delta_defor_ha_total_avg", "delta_defor_ha_total_rel_pct",
      "delta_defor_ha_crops_raw", "delta_defor_ha_crops_avg", "delta_defor_ha_crops_rel_pct",
      "delta_defor_ha_cutoff",
      "delta_defor_ha_total_raw_cutoff", "delta_defor_ha_total_avg_cutoff", "delta_defor_ha_total_rel_pct_cutoff",
      "delta_defor_land_share_raw_cutoff", "delta_defor_treecover_share_raw_cutoff",
      "delta_defor_ha_lagged",
      "delta_defor_ha_total_raw_lagged", "delta_defor_ha_total_avg_lagged", "delta_defor_ha_total_rel_pct_lagged",
      "delta_defor_land_share_raw", "delta_defor_land_share_raw_lagged",
      "delta_defor_treecover_share_raw", "delta_defor_treecover_share_raw_lagged",
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
      analysis_unit_year_ov_this %>%
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
      analysis_unit_type = dplyr::first(analysis_unit_type),
      collapse_tile_id = dplyr::first(collapse_tile_id),
      .groups = "drop"
    ) %>%
    arrange(AEZ, cluster_id)

  cluster_country_coverage <- cluster_buffer_country_this %>%
    arrange(
      AEZ,
      cluster_id,
      buffer_km,
      desc(normalized_country_overlap_share),
      country_name
    )

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

  write_csv_safe(
    cluster_country_coverage,
    file.path(current_output_dirs$tmp_dir, "cluster_country_coverage.csv")
  )

  write_csv_safe(
    analysis_unit_membership,
    file.path(current_output_dirs$tmp_dir, "analysis_unit_membership.csv")
  )

  write_csv_safe(
    analysis_unit_summary,
    file.path(current_output_dirs$tmp_dir, "analysis_unit_summary.csv")
  )

  message("Finished 05_build_transition_tables.R")
  if (exists("current_cluster_stub")) {
    message("  cluster run: ", current_cluster_stub)
  }
  message("  buffer_km: ", current_buffer_km)
  message("  cluster_deltas rows: ", nrow(cluster_deltas))
}
