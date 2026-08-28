local_lib <- normalizePath(file.path(getwd(), "..", "..", ".Rlib"), mustWork = FALSE)
if (dir.exists(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

required_packages <- c(
  "dplyr",
  "readr",
  "tidyr",
  "purrr",
  "stringr",
  "ggplot2",
  "broom",
  "broom.mixed",
  "lme4",
  "performance",
  "mgcv"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  stop(
    "Missing required regression packages: ",
    paste(missing_packages, collapse = ", "),
    "\nRun: Rscript ov_metric/biotime_only/install_dependencies.R",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(ggplot2)
  library(broom)
  library(broom.mixed)
  library(lme4)
  library(performance)
  library(mgcv)
})
