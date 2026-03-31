# =====================================================
# Summaries for AEZ-level and overall reporting
# tl;dr: reduces transition and tagging tables into AEZ coverage outputs
# for the current cluster run and current buffer.
# =====================================================

# -----------------------
# Preconditions
# -----------------------

required_objects <- c(
  "current_buffer_km",
  "current_buffer_key",
  "cluster_tag_status",
  "cluster_year_pairs",
  "cluster_deltas"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    paste0(
      "06_summarize_by_aez.R is missing required objects: ",
      paste(missing_objects, collapse = ", ")
    ),
    call. = FALSE
  )
}

if (is.na(current_buffer_km)) {
  stop("current_buffer_km is NA. Run set_buffer_output_dirs(buffer_km) first.", call. = FALSE)
}

# -----------------------
# Empty-buffer handling
# -----------------------

if (nrow(cluster_tag_status) == 0) {
  cluster_counts_by_aez <- tibble()
  pair_counts_by_aez <- tibble()
  ha_pair_summary_by_aez <- tibble()
  aez_summary <- tibble()
  
  message("Finished 06_summarize_by_aez.R for empty buffer: ", current_buffer_km)
} else {
  
  assert_has_cols(
    cluster_tag_status,
    c("AEZ", "cluster_id", "n_matched_tiles_with_ha", "tagged_ha_tile"),
    paste0("cluster_tag_status_", current_buffer_key)
  )
  
  assert_has_cols(
    cluster_year_pairs,
    c("AEZ", "cluster_id", "year_t1", "year_t2"),
    paste0("cluster_year_pairs_", current_buffer_key)
  )
  
  # cluster_deltas may be empty after filtering, but should still have structure
  if (nrow(cluster_deltas) > 0) {
    assert_has_cols(
      cluster_deltas,
      c("AEZ", "cluster_id", "inverse_change"),
      paste0("cluster_deltas_", current_buffer_key)
    )
  }
  
  # -----------------------
  # AEZ universe for current buffer
  # -----------------------
  
  all_aez <- cluster_tag_status %>%
    distinct(AEZ) %>%
    arrange(readr::parse_number(as.character(AEZ)), AEZ)
  
  # -----------------------
  # Cluster counts by AEZ
  # -----------------------
  
  cluster_counts_by_aez <- cluster_tag_status %>%
    group_by(AEZ) %>%
    summarise(
      total_clusters = n_distinct(cluster_id),
      clusters_tagged_to_ha_tile = sum(tagged_ha_tile, na.rm = TRUE),
      mean_n_matched_ha_tiles = dplyr::if_else(
        any(tagged_ha_tile),
        mean(n_matched_tiles_with_ha[tagged_ha_tile], na.rm = TRUE),
        0
      ),
      .groups = "drop"
    )
  
  # -----------------------
  # Cluster-year pair counts by AEZ
  # -----------------------
  
  pair_counts_by_aez <- cluster_year_pairs %>%
    group_by(AEZ) %>%
    summarise(
      total_cluster_pairs = n(),
      .groups = "drop"
    )
  
  # -----------------------
  # HA-tagged pair summaries by AEZ
  # -----------------------
  
  if (nrow(cluster_deltas) == 0) {
    ha_pair_summary_by_aez <- tibble(
      AEZ = character(),
      pairs_tagged_to_ha_tile = integer(),
      n_inverse_change_ha_tag = integer(),
      n_aligned_change_ha_tag = integer()
    )
  } else {
    ha_pair_summary_by_aez <- cluster_deltas %>%
      mutate(
        aligned_change = !is.na(delta_ov) & !is.na(delta_defor_ha) &
          (
            (delta_ov > 0 & delta_defor_ha > 0) |
              (delta_ov < 0 & delta_defor_ha < 0)
          )
      ) %>%
      group_by(AEZ) %>%
      summarise(
        pairs_tagged_to_ha_tile = n(),
        n_inverse_change_ha_tag = sum(inverse_change, na.rm = TRUE),
        n_aligned_change_ha_tag = sum(aligned_change, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # -----------------------
  # Final AEZ summary for current buffer
  # -----------------------
  
  aez_summary <- all_aez %>%
    left_join(cluster_counts_by_aez, by = "AEZ") %>%
    left_join(pair_counts_by_aez, by = "AEZ") %>%
    left_join(ha_pair_summary_by_aez, by = "AEZ") %>%
    mutate(
      across(
        c(
          total_clusters,
          total_cluster_pairs,
          clusters_tagged_to_ha_tile,
          pairs_tagged_to_ha_tile,
          n_inverse_change_ha_tag,
          n_aligned_change_ha_tag
        ),
        ~ tidyr::replace_na(.x, 0)
      ),
      mean_n_matched_ha_tiles = replace_na(mean_n_matched_ha_tiles, 0),
      share_inverse_change_ha_tag = if_else(
        pairs_tagged_to_ha_tile > 0,
        n_inverse_change_ha_tag / pairs_tagged_to_ha_tile,
        0
      ),
      share_aligned_change_ha_tag = if_else(
        pairs_tagged_to_ha_tile > 0,
        n_aligned_change_ha_tag / pairs_tagged_to_ha_tile,
        0
      ),
      buffer_km = current_buffer_km,
      cluster_method = if (exists("current_cluster_method")) current_cluster_method else NA_character_,
      cluster_radius_km = if (exists("current_cluster_radius_km")) current_cluster_radius_km else NA_real_,
      cluster_stub = if (exists("current_cluster_stub")) current_cluster_stub else NA_character_,
      .before = 1
    ) %>%
    select(
      cluster_method,
      cluster_radius_km,
      cluster_stub,
      buffer_km,
      AEZ,
      total_clusters,
      total_cluster_pairs,
      clusters_tagged_to_ha_tile,
      pairs_tagged_to_ha_tile,
      mean_n_matched_ha_tiles,
      n_inverse_change_ha_tag,
      n_aligned_change_ha_tag,
      share_inverse_change_ha_tag,
      share_aligned_change_ha_tag
    ) %>%
    arrange(readr::parse_number(as.character(AEZ)), AEZ)
  
  message("Finished 06_summarize_by_aez.R")
  if (exists("current_cluster_stub")) {
    message("  cluster run: ", current_cluster_stub)
  }
  message("  buffer_km: ", current_buffer_km)
  message("  aez_summary rows: ", nrow(aez_summary))
}