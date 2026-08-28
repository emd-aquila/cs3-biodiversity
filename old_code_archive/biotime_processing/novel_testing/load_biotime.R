library(dplyr)
library(purrr)
library(readr)
library(stringr)

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

download_if_missing <- function(url, destfile) {
  if (!file.exists(destfile)) {
    download.file(url, destfile, mode = "wb")
  }
  destfile
}

canonicalize_csv <- function(url, canonical_path, legacy_path = NULL) {
  if (!file.exists(canonical_path)) {
    if (!is.null(legacy_path) && file.exists(legacy_path)) {
      file.rename(legacy_path, canonical_path)
    } else {
      download_if_missing(url, canonical_path)
    }
  }

  if (!is.null(legacy_path) && file.exists(legacy_path) && legacy_path != canonical_path) {
    invisible(file.remove(legacy_path))
  }

  canonical_path
}

detect_first_existing_col <- function(df, candidates) {
  hits <- intersect(candidates, names(df))
  if (length(hits) == 0) return(NA_character_)
  hits[[1]]
}

collapse_unique <- function(x) {
  x <- unique(na.omit(x))
  x <- x[nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = " | ")
}

extract_dois <- function(x) {
  matches <- str_extract_all(
    x,
    regex("10\\.[0-9]{4,9}/[-._;()/:A-Za-z0-9]+", ignore_case = TRUE)
  )
  vapply(matches, collapse_unique, character(1))
}

biotime_query_url <- "https://zenodo.org/records/15222193/files/biotime_v2_query_15April25.rds?download=1"
biotime_metadata_url <- "https://zenodo.org/records/15222193/files/biotime_v2_metadata_15April25.csv?download=1"
biotime_references_url <- "https://zenodo.org/records/15222193/files/references_biotime_v2_15April25.csv?download=1"

biotime_data_path <- file.path(data_dir, "biotime_data.csv")
biotime_metadata_path <- file.path(data_dir, "biotime_metadata.csv")
biotime_references_path <- file.path(data_dir, "biotime_references.csv")

legacy_query_path <- file.path(data_dir, "biotime_v2_query_15April25.rds")
legacy_metadata_path <- file.path(data_dir, "biotime_v2_metadata_15April25.csv")
legacy_references_path <- file.path(data_dir, "references_biotime_v2_15April25.csv")

biotime_metadata_path <- canonicalize_csv(
  biotime_metadata_url,
  biotime_metadata_path,
  legacy_metadata_path
)
biotime_references_path <- canonicalize_csv(
  biotime_references_url,
  biotime_references_path,
  legacy_references_path
)

raw_observation_rows <- NA_integer_

if (!file.exists(biotime_data_path)) {
  download_if_missing(biotime_query_url, legacy_query_path)
  biotime_raw <- readRDS(legacy_query_path)

  if (!is.data.frame(biotime_raw) || nrow(biotime_raw) == 0) {
    stop("BioTIME query table loaded as empty or invalid.", call. = FALSE)
  }

  raw_observation_rows <- nrow(biotime_raw)
  write_csv(biotime_raw, biotime_data_path)
}

if (file.exists(legacy_query_path)) {
  invisible(file.remove(legacy_query_path))
}

ds_store_path <- file.path(data_dir, ".DS_Store")
if (file.exists(ds_store_path)) {
  invisible(file.remove(ds_store_path))
}

biotime_metadata <- read_csv(biotime_metadata_path, show_col_types = FALSE)
biotime_references <- read_csv(biotime_references_path, show_col_types = FALSE)

if (!file.exists(biotime_data_path)) {
  stop("BioTIME data CSV was not created: ", biotime_data_path, call. = FALSE)
}

required_metadata_cols <- c("STUDY_ID", "REALM", "END_YEAR", "AB_BIO")
missing_metadata_cols <- setdiff(required_metadata_cols, names(biotime_metadata))
if (length(missing_metadata_cols) > 0) {
  stop(
    "BioTIME metadata is missing required columns: ",
    paste(missing_metadata_cols, collapse = ", "),
    call. = FALSE
  )
}

required_reference_cols <- c("STUDY_ID", "CITATION_ID", "BIB")
missing_reference_cols <- setdiff(required_reference_cols, names(biotime_references))
if (length(missing_reference_cols) > 0) {
  stop(
    "BioTIME references are missing required columns: ",
    paste(missing_reference_cols, collapse = ", "),
    call. = FALSE
  )
}

title_col <- detect_first_existing_col(biotime_metadata, c("TITLE", "Title", "title"))

reference_summary <- biotime_references %>%
  mutate(DOI = extract_dois(BIB)) %>%
  group_by(STUDY_ID) %>%
  summarise(
    CITATION_ID = collapse_unique(as.character(CITATION_ID)),
    DOI = collapse_unique(DOI),
    BIB = collapse_unique(BIB),
    .groups = "drop"
  )

study_filters <- biotime_metadata %>%
  mutate(
    STUDY_ID = as.integer(STUDY_ID),
    END_YEAR = as.integer(END_YEAR),
    non_terrestrial_realm = REALM %in% c("Freshwater", "Marine"),
    end_year_before_2000 = !is.na(END_YEAR) & END_YEAR < 2000,
    not_abundance_based = is.na(AB_BIO) | !str_detect(str_to_upper(AB_BIO), "A"),
    filtered_out = non_terrestrial_realm | end_year_before_2000 | not_abundance_based,
    filter_reason = pmap_chr(
      list(non_terrestrial_realm, end_year_before_2000, not_abundance_based),
      function(non_terrestrial, old_end_year, not_abundance) {
        reasons <- c(
          if (non_terrestrial) "non_terrestrial_realm",
          if (old_end_year) "end_year_before_2000",
          if (not_abundance) "not_abundance_based"
        )
        if (length(reasons) == 0) return("")
        paste(reasons, collapse = ";")
      }
    )
  ) %>%
  left_join(reference_summary, by = "STUDY_ID")

output_cols <- c(
  "STUDY_ID",
  title_col,
  "REALM",
  "END_YEAR",
  "AB_BIO",
  "ABUNDANCE_TYPE",
  "non_terrestrial_realm",
  "end_year_before_2000",
  "not_abundance_based",
  "filter_reason",
  "CITATION_ID",
  "DOI",
  "BIB"
)
output_cols <- output_cols[!is.na(output_cols)]

studies_filtered_out <- study_filters %>%
  filter(filtered_out) %>%
  select(any_of(output_cols)) %>%
  arrange(STUDY_ID)

studies_remaining <- study_filters %>%
  filter(!filtered_out) %>%
  select(any_of(output_cols)) %>%
  arrange(STUDY_ID)

write_csv(studies_filtered_out, file.path(output_dir, "biotime_studies_filtered_out.csv"))
write_csv(studies_remaining, file.path(output_dir, "biotime_studies_remaining.csv"))

cat("\nBioTIME canonical files loaded.\n")
cat("Data CSV: ", biotime_data_path, "\n")
if (!is.na(raw_observation_rows)) {
  cat("Raw observations written: ", raw_observation_rows, "\n")
}
cat("Metadata studies: ", nrow(biotime_metadata), "\n")
cat("Reference rows: ", nrow(biotime_references), "\n\n")
cat("Studies filtered out: ", nrow(studies_filtered_out), "\n")
cat("Studies remaining: ", nrow(studies_remaining), "\n")
cat("Filter counts are non-exclusive:\n")
cat("  Non-terrestrial Realm: ", sum(study_filters$non_terrestrial_realm), "\n")
cat("  END_YEAR < 2000: ", sum(study_filters$end_year_before_2000), "\n")
cat("  Not abundance-based: ", sum(study_filters$not_abundance_based), "\n")
cat("\nWrote:\n")
cat("  ", file.path(output_dir, "biotime_studies_filtered_out.csv"), "\n")
cat("  ", file.path(output_dir, "biotime_studies_remaining.csv"), "\n")
