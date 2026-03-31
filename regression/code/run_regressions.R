# =====================================================
# Run regression pipeline over all configured runs
# =====================================================

message("Starting regression pipeline...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

for (i in seq_len(nrow(regression_run_grid))) {
  cluster_method_i <- regression_run_grid$cluster_method[i]
  cluster_radius_km_i <- regression_run_grid$cluster_radius_km[i]
  buffer_km_i <- regression_run_grid$buffer_km[i]
  defor_transform_i <- regression_run_grid$defor_transform[i]
  
  message("\n======================================")
  message("Running regression spec:")
  message("  method: ", cluster_method_i)
  message("  clustering radius (km): ", sprintf("%.1f", cluster_radius_km_i))
  message("  buffer (km): ", buffer_km_i)
  message("  defor transform: ", defor_transform_i)
  message("======================================")
  
  set_regression_run_paths(
    cluster_method = cluster_method_i,
    cluster_radius_km = cluster_radius_km_i,
    buffer_km = buffer_km_i,
    defor_transform = defor_transform_i
  )
  
  message("Current cluster_deltas path: ", cluster_deltas_path)
  
  message("Sourcing 03_load_data.R")
  source("03_load_data.R")
  
  message("Sourcing 04_ols_regression.R")
  source("04_ols_regression.R")
  
  message("Sourcing 05_diagnostics.R")
  source("05_diagnostics.R")
  
  message("Sourcing 06_other_regressions.R")
  source("06_other_regressions.R")
}

message("Regression pipeline complete.")
