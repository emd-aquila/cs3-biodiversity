# =====================================================
# Save analysis outputs
# =====================================================

# -----------------------
# Tables
# -----------------------

write_csv_safe(
  round_numeric_cols(cluster_pair_report, 2),
  file.path(analysis_tables_dir, "cluster_pair_report.csv")
)

write_csv_safe(
  round_numeric_cols(cluster_ov_change, 2),
  file.path(analysis_tables_dir, "cluster_ov_change.csv")
)

write_csv_safe(
  round_numeric_cols(aez_transition_summary, 2),
  file.path(analysis_tables_dir, "aez_transition_summary.csv")
)

write_csv_safe(
  round_numeric_cols(clusters_tagged_ha_tile, 2),
  file.path(analysis_tables_dir, "clusters_tagged_ha_tile.csv")
)

write_csv_safe(
  round_numeric_cols(cluster_tag_summary, 2),
  file.path(analysis_tables_dir, "cluster_tag_summary.csv")
)

# -----------------------
# Figures
# -----------------------

write_plot_safe(
  plot_list$global_map,
  file.path(analysis_figures_dir, "global_map.png"),
  width = 14,
  height = 8
)

write_plot_safe(
  plot_list$tagging_rate,
  file.path(analysis_figures_dir, "tagging_rate.png")
)

write_plot_safe(
  plot_list$ov_direction,
  file.path(analysis_figures_dir, "ov_direction.png")
)