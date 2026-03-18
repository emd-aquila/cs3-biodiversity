# =====================================================
# Run all code in the "analysis" subdir of cluster_deforestation_tag
# =====================================================

message("Starting analysis...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

message("Sourcing 03_load_canonical_data.R")
source("03_load_canonical_data.R")

message("Sourcing 04_buffer_sensitivity_check.R")
source("04_buffer_sensitivity_check.R")

message("Sourcing 05_build_transition_tables.R")
source("05_build_transition_tables.R")

message("Sourcing 06_summarize_by_aez.R")
source("06_summarize_by_aez.R")

message("Sourcing 07_make_figures.R")
source("07_make_figures.R")

message("Sourcing 08_write_outputs.R")
source("08_write_outputs.R")

message("Analysis stage complete.")