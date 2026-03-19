# =====================================================
# Summaries for AEZ-level and overall reporting
# tl;dr: reduces transition and tagging tables into AEZ coverage outputs
# =====================================================

assert_has_cols(
  cluster_tag_status,
  c("AEZ", "cluster_id", "n_matched_tiles_with_ha", "tagged_ha_tile"),
  "cluster_tag_status"
)

assert_has_cols(
  cluster_year_pairs,
  c("AEZ", "cluster_id", "year_t1", "year_t2"),
  "cluster_year_pairs"
)

assert_has_cols(
  cluster_deltas,
  c("AEZ", "cluster_id", "inverse_change"),
  "cluster_deltas"
)

all_aez <- cluster_tag_status %>%
  distinct(AEZ) %>%
  arrange(readr::parse_number(as.character(AEZ)), AEZ)

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

pair_counts_by_aez <- cluster_year_pairs %>%
  group_by(AEZ) %>%
  summarise(
    total_cluster_pairs = n(),
    .groups = "drop"
  )

ha_pair_summary_by_aez <- cluster_deltas %>%
  group_by(AEZ) %>%
  summarise(
    pairs_tagged_to_ha_tile = n(),
    n_inverse_change_ha_tag = sum(inverse_change, na.rm = TRUE),
    n_aligned_change_ha_tag = sum(!inverse_change, na.rm = TRUE),
    .groups = "drop"
  )

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
    mean_n_matched_ha_tiles = tidyr::replace_na(mean_n_matched_ha_tiles, 0),
    share_inverse_change_ha_tag = dplyr::if_else(
      pairs_tagged_to_ha_tile > 0,
      n_inverse_change_ha_tag / pairs_tagged_to_ha_tile,
      0
    ),
    share_aligned_change_ha_tag = dplyr::if_else(
      pairs_tagged_to_ha_tile > 0,
      n_aligned_change_ha_tag / pairs_tagged_to_ha_tile,
      0
    )
  ) %>%
  select(
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
