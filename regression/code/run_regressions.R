message("Starting regression pipeline...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

message("Sourcing 03_load_data.R")
source("03_load_data.R")

message("Sourcing 04_ols_regression.R")
source("04_ols_regression.R")

message("Sourcing 05_diagnostics.R")
source("05_diagnostics.R")

message("Sourcing 06_other_regressions.R")
source("06_other_regressions.R")

message("Regression pipeline complete.")
