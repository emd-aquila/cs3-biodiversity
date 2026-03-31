# =====================================================
# Run both analysis workflows from one code directory
# =====================================================

message("Starting analysis...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

for (analysis_mode in analysis_modes_to_run) {
  set_analysis_mode_paths(analysis_mode)

  message("\n######################################")
  message("Running analysis workflow: ", current_analysis_mode)
  message("Output root: ", analysis_output_dir)
  message("Temp root: ", analysis_tmp_dir)
  message("######################################")

  buffer_sensitivity_reports_all <- list()

  for (i in seq_len(nrow(cluster_run_grid))) {
    cluster_method_i <- cluster_run_grid$cluster_method[i]
    cluster_radius_km_i <- cluster_run_grid$cluster_radius_km[i]
    cluster_stub_i <- cluster_run_grid$cluster_stub[i]

    message("\n======================================")
    message(
      "Running cluster config: ",
      cluster_method_i,
      " / ",
      sprintf("%.1f", cluster_radius_km_i),
      " km"
    )
    message("Cluster stub: ", cluster_stub_i)
    message("Analysis workflow: ", current_analysis_mode)
    message("======================================")

    set_analysis_run_paths(
      cluster_method = cluster_method_i,
      cluster_radius_km = cluster_radius_km_i
    )

    message("Sourcing 03_load_canonical_data.R")
    source("03_load_canonical_data.R")

    message("Sourcing 04_buffer_sensitivity_check.R")
    source("04_buffer_sensitivity_check.R")

    buffer_sensitivity_reports_all[[current_cluster_stub]] <- buffer_sensitivity_report_current

    for (buffer_km in buffer_km_focus) {
      message("\n------------------------------")
      message("Running buffer: ", buffer_km, " km")
      message("------------------------------")

      set_buffer_output_dirs(buffer_km)

      if (current_analysis_mode == "whole_cluster") {
        message("Sourcing 05_build_transition_tables_whole_cluster.R")
        source("05_build_transition_tables_whole_cluster.R")

        message("Sourcing 06_summarize_by_aez_whole_cluster.R")
        source("06_summarize_by_aez_whole_cluster.R")
      } else {
        message("Sourcing 05_build_transition_tables_year_pair.R")
        source("05_build_transition_tables_year_pair.R")

        message("Sourcing 06_summarize_by_aez_year_pair.R")
        source("06_summarize_by_aez_year_pair.R")
      }

      message("Sourcing 07_make_figures.R")
      source("07_make_figures.R")

      message("Sourcing 08_write_outputs.R")
      source("08_write_outputs.R")
    }
  }

  if (length(buffer_sensitivity_reports_all) == 0) {
    warning("No buffer sensitivity reports were generated for ", current_analysis_mode, ".")
  } else {
    buffer_sensitivity_report <- dplyr::bind_rows(buffer_sensitivity_reports_all) %>%
      arrange(cluster_method, cluster_radius_km, buffer_km)

    write_csv_safe(
      round_numeric_cols(buffer_sensitivity_report, 2),
      file.path(analysis_tmp_dir, "buffer_sensitivity_report.csv")
    )
  }
}

message("Analysis stage complete.")
