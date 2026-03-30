# =====================================================
# Run all code in the "analysis" subdir of cluster_deforestation_tag
# =====================================================

message("Starting analysis...")
message("Working directory: ", getwd())

# -----------------------
# Load shared code and configuration
# -----------------------

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

# -----------------------
# Container for one global buffer-sensitivity report
# accumulated across all cluster method / radius runs
# -----------------------

buffer_sensitivity_reports_all <- list()

# -----------------------
# Outer loop:
# one iteration per cluster method / radius combination
# -----------------------

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
  message("======================================")
  
  # -----------------------
  # Set current cluster-run paths
  # -----------------------
  
  set_analysis_run_paths(
    cluster_method = cluster_method_i,
    cluster_radius_km = cluster_radius_km_i
  )
  
  # -----------------------
  # Load canonical data for current cluster run
  # -----------------------
  
  message("Sourcing 03_load_canonical_data.R")
  source("03_load_canonical_data.R")
  
  # -----------------------
  # Run-level diagnostic:
  # buffer sensitivity for the current cluster run
  # -----------------------
  
  message("Sourcing 04_buffer_sensitivity_check.R")
  source("04_buffer_sensitivity_check.R")
  
  # store current run's buffer sensitivity table for final global write
  buffer_sensitivity_reports_all[[current_cluster_stub]] <- buffer_sensitivity_report_current
  
  # -----------------------
  # Inner loop:
  # one iteration per selected buffer size
  # -----------------------
  
  for (buffer_km in buffer_km_focus) {
    message("\n------------------------------")
    message("Running buffer: ", buffer_km, " km")
    message("------------------------------")
    
    # -----------------------
    # Set current buffer-specific output directories
    # -----------------------
    
    set_buffer_output_dirs(buffer_km)
    
    # -----------------------
    # Build current-buffer transition tables
    # -----------------------
    
    message("Sourcing 05_build_transition_tables.R")
    source("05_build_transition_tables.R")
    
    # -----------------------
    # Summarize current-buffer outputs by AEZ
    # -----------------------
    
    message("Sourcing 06_summarize_by_aez.R")
    source("06_summarize_by_aez.R")
    
    # -----------------------
    # Build current-buffer figures
    # -----------------------
    
    message("Sourcing 07_make_figures.R")
    source("07_make_figures.R")
    
    # -----------------------
    # Write current-buffer outputs
    # -----------------------
    
    message("Sourcing 08_write_outputs.R")
    source("08_write_outputs.R")
  }
}

# -----------------------
# Write one global buffer sensitivity report
# across all cluster method / radius combinations
# -----------------------

if (length(buffer_sensitivity_reports_all) == 0) {
  warning("No buffer sensitivity reports were generated.")
} else {
  buffer_sensitivity_report <- dplyr::bind_rows(buffer_sensitivity_reports_all) %>%
    arrange(cluster_method, cluster_radius_km, buffer_km)
  
  write_csv_safe(
    round_numeric_cols(buffer_sensitivity_report, 2),
    file.path(analysis_tmp_dir, "buffer_sensitivity_report.csv")
  )
}

message("Analysis stage complete.")