#!/usr/bin/env Rscript

workflow_dir <- normalizePath(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), mustWork = TRUE)
local_lib <- file.path(workflow_dir, ".Rlib")
dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(local_lib, .libPaths()))

required_packages <- c(
  "ape",
  "BioTIMEr",
  "picante",
  "tibble",
  "tidyr",
  "lme4",
  "broom.mixed"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    lib = local_lib,
    repos = "https://cloud.r-project.org"
  )
}

message("R dependency check complete. Local library: ", local_lib)
