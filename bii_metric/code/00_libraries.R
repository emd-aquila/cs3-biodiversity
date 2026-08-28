# Packages shared by the PREDICTS BII workflow.
suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(stringr)
})

local_ov_lib <- normalizePath(
  file.path(getwd(), "..", "..", "ov_metric", "biotime_only", ".Rlib"),
  winslash = "/", mustWork = FALSE
)
if (dir.exists(local_ov_lib)) .libPaths(c(local_ov_lib, .libPaths()))

has_lme4 <- requireNamespace("lme4", quietly = TRUE)
if (!has_lme4) {
  warning(
    "Package 'lme4' is not installed. The workflow will use fixed-effect models, " ,
    "which are useful for smoke tests but are not the De Palma-style mixed models. ",
    "Install lme4 before producing scientific results.",
    call. = FALSE
  )
}
