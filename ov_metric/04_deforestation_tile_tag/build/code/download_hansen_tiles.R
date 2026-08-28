#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
})

options(timeout = max(3600, getOption("timeout")))

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop(
    paste0(
      "Usage:\n",
      "  Rscript ov_metric/04_deforestation_tile_tag/build/code/download_hansen_tiles.R ",
      "ov_metric/04_deforestation_tile_tag/build/output/<database>/<method>/radius_<radius>/hansen_gee/<manifest>.csv"
    ),
    call. = FALSE
  )
}

manifest_path <- args[[1]]

if (!file.exists(manifest_path)) {
  stop("Manifest does not exist: ", manifest_path, call. = FALSE)
}

manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)

required_cols <- c("layer", "tile_id", "url", "local_dir", "local_path")
missing_cols <- setdiff(required_cols, names(manifest))

if (length(missing_cols) > 0) {
  stop(
    "Manifest is missing required columns: ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

download_one <- function(url, local_path, max_attempts = 3L) {
  dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(local_path) && file.size(local_path) > 0) {
    message("Already present: ", local_path)
    return(invisible(local_path))
  }

  tmp_path <- paste0(local_path, ".part")
  if (file.exists(tmp_path)) {
    file.remove(tmp_path)
  }

  for (attempt in seq_len(max_attempts)) {
    message("Downloading: ", url)
    if (attempt > 1L) {
      message("  retry attempt ", attempt, " of ", max_attempts)
    }

    status <- tryCatch(
      utils::download.file(
        url = url,
        destfile = tmp_path,
        mode = "wb",
        quiet = FALSE
      ),
      error = function(err) err
    )

    if (!inherits(status, "error") && file.exists(tmp_path) && file.size(tmp_path) > 0) {
      file.rename(tmp_path, local_path)
      return(invisible(local_path))
    }

    if (file.exists(tmp_path)) {
      file.remove(tmp_path)
    }
  }

  stop("Download failed after ", max_attempts, " attempts: ", url, call. = FALSE)
}

manifest %>%
  distinct(url, local_path) %>%
  pwalk(function(url, local_path) {
    download_one(url, local_path)
  })

message("Hansen download complete.")
