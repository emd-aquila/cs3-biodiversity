# =====================================================
# Run the PREDICTS build pipeline
# =====================================================

message("Starting predicts_ov_table build...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

message("Sourcing 03_import_predicts_data.R")
source("03_import_predicts_data.R")

message("Sourcing 04_prepare_community_matrix.R")
source("04_prepare_community_matrix.R")

message("Build complete.")
