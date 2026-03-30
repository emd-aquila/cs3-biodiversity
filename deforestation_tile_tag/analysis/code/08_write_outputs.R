# =====================================================
# Save analysis outputs for the current cluster run and current buffer
# =====================================================

# -----------------------
# Preconditions
# -----------------------

required_objects <- c(
  "current_buffer_km",
  "current_output_dirs",
  "cluster_deltas",
  "cluster_ov_trend",
  "aez_summary",
  "plot_list"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    paste0(
      "08_write_outputs.R is missing required objects: ",
      paste(missing_objects, collapse = ", ")
    ),
    call. = FALSE
  )
}

if (is.na(current_buffer_km)) {
  stop("current_buffer_km is NA. Run set_buffer_output_dirs(buffer_km) first.", call. = FALSE)
}

if (is.null(current_output_dirs)) {
  stop("current_output_dirs is NULL. Run set_buffer_output_dirs(buffer_km) first.", call. = FALSE)
}

# -----------------------
# Write tables
# -----------------------

write_csv_safe(
  round_numeric_cols(cluster_deltas, 2),
  file.path(current_output_dirs$tables_dir, "cluster_deltas.csv")
)

write_csv_safe(
  round_numeric_cols(cluster_ov_trend, 2),
  file.path(current_output_dirs$tables_dir, "cluster_ov_trend.csv")
)

write_csv_safe(
  round_numeric_cols(aez_summary, 2),
  file.path(current_output_dirs$tables_dir, "aez_summary.csv")
)

# -----------------------
# Write figures
# -----------------------

if (length(plot_list) == 0) {
  warning(
    "plot_list is empty for buffer_km = ",
    current_buffer_km,
    ". Skipping figure writes."
  )
} else {
  write_plot_safe(
    plot_list$global_map,
    file.path(current_output_dirs$figures_dir, "global_map.png"),
    width = 14,
    height = 8
  )
  
  write_plot_safe(
    plot_list$tagging_rate,
    file.path(current_output_dirs$figures_dir, "tagging_rate.png")
  )
  
  write_plot_safe(
    plot_list$ov_direction,
    file.path(current_output_dirs$figures_dir, "ov_direction.png")
  )
}

message("Finished 08_write_outputs.R")
if (exists("current_cluster_stub")) {
  message("  cluster run: ", current_cluster_stub)
}
message("  buffer_km: ", current_buffer_km)
message("  tables dir: ", current_output_dirs$tables_dir)
message("  figures dir: ", current_output_dirs$figures_dir)