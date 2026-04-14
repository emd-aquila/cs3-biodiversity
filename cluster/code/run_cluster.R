# =====================================================
# Run the clustering pipeline from cluster/code
# =====================================================

message("Starting cluster build...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

message("Sourcing 03_load_data.R")
source("03_load_data.R")

message("Sourcing 04_perform_clustering.R")
source("04_perform_clustering.R")

message("Sourcing 05_diagnostics.R")
source("05_diagnostics.R")

message("Sourcing 06_build_master_dataset.R")
source("06_build_master_dataset.R")

message("Cluster build complete.")
