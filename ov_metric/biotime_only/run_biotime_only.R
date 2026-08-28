#!/usr/bin/env Rscript

script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
workflow_dir <- dirname(normalizePath(script_path))
repo_root <- normalizePath(file.path(workflow_dir, "..", ".."), mustWork = TRUE)

run_stage <- function(path) {
  message("\n=====================================================")
  message("Running: ", path)
  message("=====================================================")
  status <- system2("Rscript", file.path(repo_root, path))
  if (!identical(status, 0L)) {
    stop("Stage failed: ", path, call. = FALSE)
  }
}

run_stage("ov_metric/biotime_only/data_cleaning/code/run_data_cleaning.R")
run_stage("ov_metric/biotime_only/deforestation_matching/code/run_deforestation_matching.R")
run_stage("ov_metric/biotime_only/regression/code/run_regression.R")
run_stage("ov_metric/biotime_only/visualization/code/run_visualization.R")

message("BioTIME-only workflow complete.")
