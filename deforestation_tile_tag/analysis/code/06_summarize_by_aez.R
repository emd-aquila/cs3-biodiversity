# =====================================================
# Summaries for AEZ-level and overall reporting
# tl;dr:  takes the tables from step 04 and reduces them to interpretable summaries
# =====================================================

# -----------------------
# Overall transition summary
# one-row totals: how much cluster-year pairs, average and median OV change,
# average and median deforestation change, counts of "favorable/unfavorable" cases
# -----------------------

overall_transition_summary <- cluster_pair_report %>%
  summarise(
    n_cluster_pairs = n(),
    n_clusters = n_distinct(cluster_id),
    mean_delta_ov = mean(delta_ov, na.rm = TRUE),
    median_delta_ov = median(delta_ov, na.rm = TRUE),
    minmax_delta_ov = calc_minmax(delta_ov),
    iqr_delta_ov = calc_iqr(delta_ov),
    mean_delta_defor_ha = mean(delta_defor_ha, na.rm = TRUE),
    median_delta_defor_ha = median(delta_defor_ha, na.rm = TRUE),
    minmax_delta_defor_ha = calc_minmax(delta_defor_ha),
    iqr_delta_defor_ha = calc_iqr(delta_defor_ha),
    n_inverse_change = sum(inverse_change, na.rm = TRUE),
    n_aligned_change = sum(aligned_change, na.rm = TRUE),
    share_inverse_change = n_inverse_change / n_cluster_pairs,
    share_aligned_change = n_aligned_change / n_cluster_pairs,
  )

# -----------------------
# AEZ-level transition summary
# "" but grouped by AEZ
# -----------------------

aez_transition_summary <- cluster_pair_report %>%
  group_by(AEZ) %>%
  summarise(
    n_cluster_pairs = n(),
    n_clusters = n_distinct(cluster_id),
    mean_delta_ov = mean(delta_ov, na.rm = TRUE),
    median_delta_ov = median(delta_ov, na.rm = TRUE),
    minmax_delta_ov = calc_minmax(delta_ov),
    iqr_delta_ov = calc_iqr(delta_ov),
    mean_delta_defor_ha = mean(delta_defor_ha, na.rm = TRUE),
    median_delta_defor_ha = median(delta_defor_ha, na.rm = TRUE),
    minmax_delta_defor_ha = calc_minmax(delta_defor_ha),
    iqr_delta_defor_ha = calc_iqr(delta_defor_ha),
    n_inverse_change = sum(inverse_change, na.rm = TRUE),
    n_aligned_change = sum(aligned_change, na.rm = TRUE),
    share_inverse_change = n_inverse_change / n_cluster_pairs,
    share_aligned_change = n_aligned_change / n_cluster_pairs,
    .groups = "drop"
  ) %>%
  arrange(AEZ)

# -----------------------
# AEZ-level OV spread
# -----------------------

aez_ov_spread_summary <- cluster_year_ov %>%
  group_by(AEZ) %>%
  summarise(
    n_cluster_years = n(),
    n_clusters = n_distinct(cluster_id),
    mean_ov = mean(median_ov_year, na.rm = TRUE),
    median_ov = median(median_ov_year, na.rm = TRUE),
    min_ov = min(median_ov_year, na.rm = TRUE),
    max_ov = max(median_ov_year, na.rm = TRUE),
    minmax_ov = calc_minmax(median_ov_year),
    iqr_ov = calc_iqr(median_ov_year),
    .groups = "drop"
  ) %>%
  arrange(AEZ)

# -----------------------
# AEZ-level deforestation spread
# -----------------------

aez_defor_spread_summary <- cluster_buffer_year_defor %>%
  group_by(AEZ) %>%
  summarise(
    n_cluster_years = n(),
    n_clusters = n_distinct(cluster_id),
    mean_defor_ha = mean(defor_total_ha_year, na.rm = TRUE),
    median_defor_ha = median(defor_total_ha_year, na.rm = TRUE),
    min_defor_ha = min(defor_total_ha_year, na.rm = TRUE),
    max_defor_ha = max(defor_total_ha_year, na.rm = TRUE),
    minmax_defor_ha = calc_minmax(defor_total_ha_year),
    iqr_defor_ha = calc_iqr(defor_total_ha_year),
    .groups = "drop"
  ) %>%
  arrange(AEZ)

# -----------------------
# Cluster tagging summary
# how many clusters in each AEZ were tagged to any tile?
# how many were tagged to a tile with hectare-level data?
# average number of matched tiles
# -----------------------

cluster_tag_summary <- cluster_tag_status %>%
  group_by(AEZ) %>%
  summarise(
    total_clusters = n(),
    n_tagged_to_tile = sum(tagged_any_tile, na.rm = TRUE),
    n_tagged_to_ha_tile = sum(tagged_ha_tile, na.rm = TRUE),
    pct_tagged_to_tile = 100 * n_tagged_to_tile / total_clusters,
    pct_tagged_to_ha_tile = 100 * n_tagged_to_ha_tile / total_clusters,
    n_not_tagged_to_any_tile = total_clusters - n_tagged_to_tile,
    n_tagged_but_no_ha_tile = n_tagged_to_tile - n_tagged_to_ha_tile,
    pct_clusters_missing_ha_data = 100 * n_tagged_but_no_ha_tile / total_clusters,
    mean_n_matched_tiles = mean(n_matched_tiles, na.rm = TRUE),
    mean_n_matched_tiles_with_ha = mean(n_matched_tiles_with_ha, na.rm = TRUE),
    mean_n_matched_tiles_missing_ha = mean(n_matched_tiles_missing_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(AEZ)

# -----------------------
# Overall row
# -----------------------

overall_row <- cluster_tag_status %>%
  summarise(
    AEZ = "Overall",
    total_clusters = n(),
    n_tagged_to_tile = sum(tagged_any_tile, na.rm = TRUE),
    n_tagged_to_ha_tile = sum(tagged_ha_tile, na.rm = TRUE),
    pct_tagged_to_tile = 100 * n_tagged_to_tile / total_clusters,
    pct_tagged_to_ha_tile = 100 * n_tagged_to_ha_tile / total_clusters,
    n_not_tagged_to_any_tile = total_clusters - n_tagged_to_tile,
    n_tagged_but_no_ha_tile = n_tagged_to_tile - n_tagged_to_ha_tile,
    pct_clusters_missing_ha_data = 100 * n_tagged_but_no_ha_tile / total_clusters,
    mean_n_matched_tiles = mean(n_matched_tiles, na.rm = TRUE),
    mean_n_matched_tiles_with_ha = mean(n_matched_tiles_with_ha, na.rm = TRUE),
    mean_n_matched_tiles_missing_ha = mean(n_matched_tiles_missing_ha, na.rm = TRUE)
  )

cluster_tag_summary <- bind_rows(
  cluster_tag_summary,
  overall_row
)