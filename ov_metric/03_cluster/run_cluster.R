# =====================================================
# Run the clustering pipeline
# =====================================================

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

if (length(file_arg) > 0) {
  cluster_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  frame_files <- unlist(lapply(sys.frames(), `[[`, "ofile"), use.names = FALSE)

  cluster_dir <- if (length(frame_files) > 0) {
    dirname(normalizePath(frame_files[[length(frame_files)]]))
  } else {
    normalizePath(getwd())
  }
}

code_dir <- file.path(cluster_dir, "code")

if (!dir.exists(code_dir)) {
  stop("Cluster code directory not found: ", code_dir, call. = FALSE)
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(code_dir)

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
