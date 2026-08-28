# =====================================================
# Run the PREDICTS analysis pipeline
# =====================================================

message("Starting predicts_ov_table analysis...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

message("Sourcing 03_load_build_outputs.R")
source("03_load_build_outputs.R")

message("Sourcing 04_calculate_components.R")
source("04_calculate_components.R")

message("Sourcing 05_build_ov_scores.R")
source("05_build_ov_scores.R")

message("Sourcing 06_assign_aez.R")
source("06_assign_aez.R")

message("Analysis complete.")
