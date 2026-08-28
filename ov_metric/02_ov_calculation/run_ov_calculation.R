# =====================================================
# Run full OV pipeline: build PD, then calculate OV/AEZ tables
# =====================================================

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "run_ov_calculation.R"
ov_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)

message("Running OV build step")
setwd(file.path(ov_dir, "build", "code"))
source("run_build.R")

message("Running OV calculation step")
setwd(file.path(ov_dir, "calculation", "code"))
source("run_calculation.R")

message("Full OV pipeline complete.")
