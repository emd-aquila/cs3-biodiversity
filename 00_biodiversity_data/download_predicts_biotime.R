# =====================================================
# Run raw biodiversity data downloads
# =====================================================

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

biodiversity_data_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  normalizePath(getwd())
}

code_dir <- file.path(biodiversity_data_dir, "code")
if (!dir.exists(code_dir)) {
  stop("Biodiversity data code directory not found: ", code_dir, call. = FALSE)
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(code_dir)

message("Starting raw biodiversity data downloads...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

message("Sourcing 03_download_predicts.R")
source("03_download_predicts.R")

message("Sourcing 04_download_biotime.R")
source("04_download_biotime.R")

message("Raw biodiversity data download step complete.")
