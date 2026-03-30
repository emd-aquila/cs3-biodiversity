# =====================================================
# Run all code in the "build" subdir of deforestation_tile_tag
# =====================================================

message("Starting build...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

# -----------------------
# Global, non-looping stages
# -----------------------

message("Sourcing 03_load_defor_data.R")
source("03_load_defor_data.R")

message("Sourcing 04_build_defor_tables.R")
source("04_build_defor_tables.R")

# -----------------------
# Loop over cluster method x radius runs
# -----------------------

for (i in seq_len(nrow(cluster_run_grid))) {
  cluster_method_i <- cluster_run_grid$cluster_method[i]
  cluster_radius_km_i <- cluster_run_grid$cluster_radius_km[i]
  
  message("======================================")
  message(
    "Running cluster config: ",
    cluster_method_i,
    " / ",
    sprintf("%.1f", cluster_radius_km_i),
    " km"
  )
  
  set_cluster_run_paths(
    cluster_method = cluster_method_i,
    cluster_radius_km = cluster_radius_km_i
  )
  
  message("Sourcing 05_load_cluster_data.R")
  source("05_load_cluster_data.R")
  
  message("Sourcing 06_build_cluster_tables.R")
  source("06_build_cluster_tables.R")
  
  message("Sourcing 07_build_cluster_footprints_tag_defor.R")
  source("07_build_cluster_footprints_tag_defor.R")
  
  message("Sourcing 08_write_canonical_outputs.R")
  source("08_write_canonical_outputs.R")
}

message("Build complete.")