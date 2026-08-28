# =====================================================
# Run OV calculation step
# =====================================================

message("Starting OV calculation step...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

message("Sourcing 03_load_inputs.R")
source("03_load_inputs.R")

message("Sourcing 04_calculate_ov_scores.R")
source("04_calculate_ov_scores.R")

message("Sourcing 05_assign_aez.R")
source("05_assign_aez.R")

message("OV calculation step complete.")
