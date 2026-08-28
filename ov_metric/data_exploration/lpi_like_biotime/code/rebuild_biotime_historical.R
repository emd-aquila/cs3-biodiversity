#!/usr/bin/env Rscript
# Rebuild only the raw-BioTIME branch of the integration pipeline for the
# requested historical window, then recalculate BioTIME PD and OV scores.

args <- commandArgs(trailingOnly = TRUE)
min_year <- if (length(args)) suppressWarnings(as.integer(args[[1]])) else 1970L
if (!is.finite(min_year) || min_year < 1800L || min_year > 2023L) {
  stop("Usage: Rscript rebuild_biotime_historical.R [min_year], with 1800 <= min_year <= 2023.", call. = FALSE)
}
Sys.setenv(BIOTIME_MIN_YEAR = as.character(min_year), OV_REBUILD_PD_DATASETS = "biotime")

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
code_dir <- dirname(normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE))
topic_dir <- normalizePath(file.path(code_dir, ".."), mustWork = TRUE)
repo_root <- normalizePath(file.path(topic_dir, "..", "..", ".."), mustWork = TRUE)

integration_dir <- file.path(repo_root, "ov_metric", "01_biodiversity_data_integration")
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(file.path(integration_dir, "code"))
source("00_libraries.R")
source("01_config.R")
source("02_helpers.R")
message("Rebuilding raw BioTIME integration from ", min_year, " through ", format(date_max, "%Y"))
source("04_process_biotime.R")
source("05_write_combined_outputs.R")

message("Recomputing BioTIME PD and OV scores")
ov_dir <- file.path(repo_root, "ov_metric", "02_ov_calculation")
setwd(file.path(ov_dir, "build", "code"))
source("run_build.R")
setwd(file.path(ov_dir, "calculation", "code"))
source("run_calculation.R")
message("Historical BioTIME rebuild complete.")
