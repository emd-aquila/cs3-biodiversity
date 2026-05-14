library(readr)

script_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", script_args, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  getwd()
}

data_dir <- file.path(script_dir, "data")
output_dir <- file.path(script_dir, "output")

biotime_raw_path <- file.path(data_dir, "biotime_data.csv")
biotime_metadata_path <- file.path(data_dir, "biotime_metadata.csv")
biotime_references_path <- file.path(data_dir, "biotime_references.csv")
studies_filtered_out_path <- file.path(output_dir, "biotime_studies_filtered_out.csv")
studies_remaining_path <- file.path(output_dir, "biotime_studies_remaining.csv")

for (path in c(
  biotime_raw_path,
  biotime_metadata_path,
  biotime_references_path,
  studies_filtered_out_path,
  studies_remaining_path
)) {
  if (!file.exists(path)) {
    stop("File not found: ", path, "\nRun load_biotime.R first.", call. = FALSE)
  }
}

biotime_raw_cols <- read_csv(biotime_raw_path, n_max = 0, show_col_types = FALSE)
biotime_raw_head <- read_csv(biotime_raw_path, n_max = 5, show_col_types = FALSE)
biotime_metadata <- read_csv(biotime_metadata_path, show_col_types = FALSE)
biotime_references <- read_csv(biotime_references_path, show_col_types = FALSE)
studies_filtered_out <- read_csv(studies_filtered_out_path, show_col_types = FALSE)
studies_remaining <- read_csv(studies_remaining_path, show_col_types = FALSE)

cat("\nBioTIME raw observations\n")
cat("Input: ", biotime_raw_path, "\n")
cat("Rows: not counted during inspection; load_biotime.R preserves all rows.\n")
cat("Columns: ", ncol(biotime_raw_cols), "\n\n")

cat("Raw BioTIME column names:\n")
print(names(biotime_raw_cols))

cat("\nRaw BioTIME head(5):\n")
print(biotime_raw_head)

cat("\n\nBioTIME metadata\n")
cat("Rows: ", nrow(biotime_metadata), "\n")
cat("Columns: ", ncol(biotime_metadata), "\n\n")

cat("BioTIME metadata column names:\n")
print(names(biotime_metadata))

cat("\nBioTIME metadata head(5):\n")
print(head(biotime_metadata, 5))

cat("\n\nBioTIME references\n")
cat("Rows: ", nrow(biotime_references), "\n")
cat("Columns: ", ncol(biotime_references), "\n\n")

cat("BioTIME references column names:\n")
print(names(biotime_references))

cat("\nBioTIME references head(5):\n")
print(head(biotime_references, 5))

cat("\n\nBioTIME study filtering outputs\n")
cat("Filtered out studies: ", nrow(studies_filtered_out), "\n")
cat("Remaining studies: ", nrow(studies_remaining), "\n\n")

cat("Filtered out reason counts are non-exclusive:\n")
cat("  Non-terrestrial Realm: ", sum(studies_filtered_out$non_terrestrial_realm), "\n")
cat("  END_YEAR < 2000: ", sum(studies_filtered_out$end_year_before_2000), "\n")
cat("  Not abundance-based: ", sum(studies_filtered_out$not_abundance_based), "\n\n")

cat("Remaining studies with DOI extracted from BIB: ")
cat(sum(!is.na(studies_remaining$DOI) & nzchar(studies_remaining$DOI)), "\n")

cat("\nRemaining studies head(5):\n")
print(head(studies_remaining, 5))
