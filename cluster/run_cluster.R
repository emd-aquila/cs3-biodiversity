# =====================================================
# Run all code in the "code" subdir of cluster
# =====================================================

message("Starting cluster build...")
message("Working directory: ", getwd())

message("Sourcing code/00_libraries.R")
source(file.path("code", "00_libraries.R"))

message("Sourcing code/01_config.R")
source(file.path("code", "01_config.R"))

message("Sourcing code/02_helpers.R")
source(file.path("code", "02_helpers.R"))

message("Sourcing code/03_load_data.R")
source(file.path("code", "03_load_data.R"))

message("Sourcing code/04_run_clustering.R")
source(file.path("code", "04_run_clustering.R"))

message("Sourcing code/05_evaluate_radius.R")
source(file.path("code", "05_evaluate_radius.R"))

message("Sourcing code/06_diagnostics.R")
source(file.path("code", "06_diagnostics.R"))

message("Sourcing code/07_build_master_dataset.R")
source(file.path("code", "07_build_master_dataset.R"))

message("Cluster build complete.")