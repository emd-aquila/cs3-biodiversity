library(predictsr)
library(dplyr)
library(readr)
library(lubridate)

script_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", script_args, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  getwd()
}

data_dir <- file.path(script_dir, "data")
output_dir <- file.path(script_dir, "output")

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

extract_years <- c(2016, 2022)
date_min <- as.Date("2000-01-01")
date_max <- as.Date("2024-12-31")

predicts_rds_path <- file.path(data_dir, "predicts.rds")

predicts_filtered_path <- file.path(
  output_dir,
  paste0(
    "predicts_filtered_",
    format(date_min, "%Y"), "_",
    format(date_max, "%Y"),
    ".csv"
  )
)

if (file.exists(predicts_rds_path)) {
  cached_predicts <- tryCatch(readRDS(predicts_rds_path), error = function(e) NULL)
  if (!is.data.frame(cached_predicts) || nrow(cached_predicts) == 0) {
    file.remove(predicts_rds_path)
    aux_path <- paste0(predicts_rds_path, ".aux.json")
    if (file.exists(aux_path)) file.remove(aux_path)
    cat("Removed invalid PREDICTS cache: ", predicts_rds_path, "\n")
  }
}

predicts_raw <- LoadPredictsData(file_predicts = predicts_rds_path, extract = extract_years)

if (!is.data.frame(predicts_raw) || nrow(predicts_raw) == 0) {
  stop("PREDICTS loaded as empty or invalid.", call. = FALSE)
}

required_cols <- c("Diversity_metric_type", "Sample_midpoint")
missing_cols <- setdiff(required_cols, names(predicts_raw))
if (length(missing_cols) > 0) {
  stop(
    "PREDICTS data is missing required columns: ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

predicts_filtered <- predicts_raw %>%
  mutate(
    Sample_midpoint = as.Date(Sample_midpoint),
    Year = lubridate::year(Sample_midpoint)
  ) %>%
  filter(
    Diversity_metric_type == "Abundance",
    !is.na(Sample_midpoint),
    Sample_midpoint >= date_min,
    Sample_midpoint <= date_max
  )

write_csv(predicts_filtered, predicts_filtered_path)

cat("\nPREDICTS filtered.\n")
cat("Raw rows: ", nrow(predicts_raw), "\n")
cat("Rows: ", nrow(predicts_filtered), "\n")
cat("Columns: ", ncol(predicts_filtered), "\n\n")
cat("Wrote: ", predicts_filtered_path, "\n")
