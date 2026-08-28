library(readr)
library(stringr)

script_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", script_args, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  getwd()
}

output_dir <- file.path(script_dir, "output")
candidate_paths <- c(
  file.path(output_dir, "predicts_filtered_2000_2024.csv"),
  file.path(script_dir, "data", "predicts_filtered_2000_2024.csv"),
  file.path(
    dirname(dirname(script_dir)),
    "predicts_ov_table",
    "build",
    "output",
    "predicts_filtered_2000_2024.csv"
  ),
  file.path(
    dirname(script_dir),
    "old",
    "build_predicts",
    "output",
    "predicts_filtered_2000_2024.csv"
  )
)

existing_paths <- candidate_paths[file.exists(candidate_paths)]

if (length(existing_paths) == 0) {
  stop(
    "Could not find predicts_filtered_2000_2024.csv in any expected location:\n",
    paste(candidate_paths, collapse = "\n"),
    call. = FALSE
  )
}

input_path <- existing_paths[[1]]

predicts <- read_csv(input_path, show_col_types = FALSE)

if (!is.data.frame(predicts) || nrow(predicts) == 0) {
  stop("Loaded PREDICTS file is empty or invalid.", call. = FALSE)
}

extract_dois <- function(x) {
  matches <- str_extract_all(
    x,
    regex("10\\.[0-9]{4,9}/[-._;()/:A-Za-z0-9]+", ignore_case = TRUE)
  )
  vapply(matches, function(values) {
    values <- unique(na.omit(values))
    values <- values[nzchar(values)]
    if (length(values) == 0) return(NA_character_)
    paste(values, collapse = " | ")
  }, character(1))
}

cat("\nPREDICTS (filtered 2000–2024)\n")
cat("Input: ", input_path, "\n")
cat("Rows: ", nrow(predicts), "\n")
cat("Columns: ", ncol(predicts), "\n\n")

cat("Column names:\n")
print(names(predicts))

cat("\nHead(5):\n")
print(head(predicts, 5))

candidate_cols <- grep(
  "doi|bib|ref|source|citation|paper|study",
  names(predicts),
  ignore.case = TRUE,
  value = TRUE
)

cat("\nReference/DOI candidate columns:\n")
print(candidate_cols)

if ("Reference" %in% names(predicts)) {
  predicts_dois <- extract_dois(predicts$Reference)
  cat("\nRows with DOI-like text in Reference: ")
  cat(sum(!is.na(predicts_dois) & nzchar(predicts_dois)), "\n")

  cat("\nDistinct Reference examples:\n")
  print(head(unique(predicts$Reference), 10))
}
