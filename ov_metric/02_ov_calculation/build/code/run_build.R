# =====================================================
# Run PD build step
# =====================================================

message("Starting OV build step...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

message("Sourcing 03_load_integrated_inputs.R")
source("03_load_integrated_inputs.R")

message("Sourcing 04_calculate_phylogenetic_diversity.R")
source("04_calculate_phylogenetic_diversity.R")

message("Sourcing 05_write_build_manifest.R")
source("05_write_build_manifest.R")

message("OV build step complete.")
