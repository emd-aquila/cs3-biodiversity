# =====================================================
# Save analysis outputs
# =====================================================

# -----------------------
# Tables
# -----------------------

write_csv_safe(
  round_numeric_cols(cluster_deltas, 2),
  file.path(analysis_tables_dir, "cluster_deltas.csv")
)

write_csv_safe(
  round_numeric_cols(cluster_ov_change, 2),
  file.path(analysis_tables_dir, "cluster_ov_change.csv")
)

write_csv_safe(
  round_numeric_cols(aez_summary, 2),
  file.path(analysis_tables_dir, "aez_summary.csv")
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
