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

message("Sourcing 03_load_raw_data.R")
source("03_load_raw_data.R")

message("Sourcing 04_build_defor_tables.R")
source("04_build_defor_tables.R")

message("Sourcing 05_build_cluster_tables.R")
source("05_build_cluster_tables.R")

message("Sourcing 06_build_cluster_footprints_tag_defor.R")
source("06_build_cluster_footprints_tag_defor.R")

message("Sourcing 07_write_canonical_outputs.R")
source("07_write_canonical_outputs.R")

message("Build complete.")