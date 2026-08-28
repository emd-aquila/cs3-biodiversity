# =====================================================
# Run the PREDICTS-BioTIME processing and integration pipeline
# =====================================================

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

integration_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  normalizePath(getwd())
}

code_dir <- file.path(integration_dir, "code")
if (!dir.exists(code_dir)) {
  stop("Integration code directory not found: ", code_dir, call. = FALSE)
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(code_dir)

message("Starting biodiversity data integration...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

message("Sourcing 03_process_predicts.R")
source("03_process_predicts.R")

message("Sourcing 04_process_biotime.R")
source("04_process_biotime.R")

message("Sourcing 05_write_combined_outputs.R")
source("05_write_combined_outputs.R")

message("Integration pipeline complete.")
