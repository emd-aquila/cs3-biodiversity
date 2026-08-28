# =====================================================
# Download and prepare PREDICTS inputs.
# =====================================================

source("01_config.R")

required_packages <- c("predictsr", "data.table", "readr", "stringi")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Install required packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

write_csv_safe <- function(data, path) {
  readr::write_csv(data, path)
  message("Wrote: ", path)
  invisible(path)
}

collapse_unique <- function(x, separator = " | ") {
  x <- unique(trimws(as.character(x)))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = separator)
}

normalize_key <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

as_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(as.character(x))
}

ascii_transliterate <- function(x) {
  x <- as.character(x)
  replacements <- c(
    "\u03b1" = "alpha",
    "\u0391" = "Alpha",
    "\u03b2" = "beta",
    "\u0392" = "Beta",
    "\u03b3" = "gamma",
    "\u0393" = "Gamma",
    "\u03b4" = "delta",
    "\u0394" = "Delta",
    "\u2018" = "'",
    "\u2019" = "'",
    "\u201c" = "\"",
    "\u201d" = "\"",
    "\u2013" = "-",
    "\u2014" = "-",
    "\u2212" = "-",
    "\u00d7" = "x"
  )
  for (pattern in names(replacements)) {
    x <- gsub(pattern, replacements[[pattern]], x, fixed = TRUE)
  }
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  gsub("[^ -~]", "", x)
}

ascii_normalize_table <- function(dt) {
  dt <- data.table::copy(dt)
  char_cols <- names(dt)[vapply(dt, is.character, logical(1))]
  for (col in char_cols) {
    data.table::set(dt, j = col, value = ascii_transliterate(dt[[col]]))
  }
  dt
}

assert_nonempty_df <- function(x, label) {
  if (!is.data.frame(x) || nrow(x) == 0 || ncol(x) == 0) {
    stop(label, " is empty or invalid.", call. = FALSE)
  }
}

load_predicts_checked <- function(path, extract) {
  predicts <- predictsr::LoadPredictsData(
    file_predicts = path,
    extract = extract
  )
  if (!is.data.frame(predicts) || nrow(predicts) == 0 || ncol(predicts) == 0) {
    warning("PREDICTS cache was empty or invalid; forcing a fresh download.", call. = FALSE)
    predicts <- predictsr::LoadPredictsData(
      file_predicts = path,
      extract = extract,
      force_refresh = TRUE
    )
  }
  assert_nonempty_df(predicts, paste("PREDICTS extract", paste(extract, collapse = ",")))
  predicts
}

summarize_column <- function(x) {
  data.table::data.table(
    r_type = class(x)[[1]],
    non_missing_rows = sum(!is.na(x)),
    missing_rows = sum(is.na(x)),
    distinct_values = data.table::uniqueN(x, na.rm = TRUE)
  )
}

remove_empty_rows <- function(dt, ignore_cols = character()) {
  value_cols <- setdiff(names(dt), ignore_cols)
  if (length(value_cols) == 0) return(dt)
  is_empty <- dt[
    ,
    Reduce(
      `&`,
      lapply(.SD, function(x) {
        x <- trimws(as.character(x))
        is.na(x) | !nzchar(x)
      })
    ),
    .SDcols = value_cols
  ]
  dt[!is_empty]
}

combine_reference_tables <- function(refs_2016, refs_2022) {
  refs_2016 <- data.table::copy(refs_2016)
  refs_2022 <- data.table::copy(refs_2022)
  refs_2016 <- remove_empty_rows(refs_2016, ignore_cols = "_id")
  refs_2022 <- remove_empty_rows(refs_2022, ignore_cols = "_id")
  if ("_id" %in% names(refs_2016) && "_id" %in% names(refs_2022)) {
    start_id <- max(as.integer(refs_2016[["_id"]]), na.rm = TRUE) + 1L
    original_order <- order(as.integer(refs_2022[["_id"]]))
    refs_2022 <- refs_2022[original_order]
    refs_2022[, `_id` := start_id + seq_len(.N) - 1L]
  }
  refs_2016[, data_release_year := 2016L]
  refs_2022[, data_release_year := 2022L]

  ordered_cols <- c(
    setdiff(names(refs_2016), "data_release_year"),
    setdiff(names(refs_2022), c(names(refs_2016), "data_release_year")),
    "data_release_year"
  )

  combined <- data.table::rbindlist(
    list(refs_2016, refs_2022),
    fill = TRUE,
    use.names = TRUE
  )
  data.table::setcolorder(combined, ordered_cols)
  remove_empty_rows(combined, ignore_cols = "data_release_year")
}

screen_reference_overlap <- function(refs_2016, refs_2022) {
  keys <- intersect(
    c("_id", "Source_ID", "DOI", "BibTeX_reference", "Reference", "Title"),
    intersect(names(refs_2016), names(refs_2022))
  )

  overlap_rows <- list()
  summary_rows <- lapply(keys, function(key) {
    left <- normalize_key(refs_2016[[key]])
    right <- normalize_key(refs_2022[[key]])
    left <- left[!is.na(left) & nzchar(left)]
    right <- right[!is.na(right) & nzchar(right)]
    overlaps <- intersect(left, right)

    if (length(overlaps) > 0) {
      overlap_rows[[key]] <<- data.table::data.table(
        overlap_key = key,
        overlap_value = overlaps
      )
    }

    data.table::data.table(
      overlap_key = key,
      n_overlap_values = length(overlaps),
      blocks_combining = key != "_id" && length(overlaps) > 0,
      note = if (key == "_id") {
        "_id is a resource-local NHM row identifier; overlaps are expected and ignored."
      } else {
        "Meaningful reference overlap screen."
      }
    )
  })

  list(
    summary = data.table::rbindlist(summary_rows),
    details = if (length(overlap_rows) == 0) {
      data.table::data.table(overlap_key = character(), overlap_value = character())
    } else {
      data.table::rbindlist(overlap_rows)
    }
  )
}

message("Downloading/loading combined raw PREDICTS extract.")
predicts <- load_predicts_checked(predicts_raw_rds_path, predicts_release_years)
predicts_dt <- data.table::as.data.table(predicts)
message("Combined rows: ", nrow(predicts_dt), "; columns: ", ncol(predicts_dt))

message("Downloading PREDICTS column descriptions with predictsr::GetColumnDescriptions().")
columns <- predictsr::GetColumnDescriptions()
assert_nonempty_df(columns, "PREDICTS column descriptions")

message("Downloading/loading PREDICTS site-level summaries.")
if (file.exists(predicts_sites_rds_path)) {
  sites <- readRDS(predicts_sites_rds_path)
} else {
  sites <- predictsr::GetSitelevelSummaries(extract = predicts_release_years)
  saveRDS(sites, predicts_sites_rds_path, compress = "gzip")
  message("Wrote: ", predicts_sites_rds_path)
}
assert_nonempty_df(sites, "PREDICTS site summaries")

message("Screening manually downloaded NHM reference tables.")
if (!file.exists(predicts_reference_2016_path) || !file.exists(predicts_reference_2022_path)) {
  stop(
    "Missing manually downloaded PREDICTS reference CSVs. Expected:\n",
    predicts_reference_2016_path,
    "\n",
    predicts_reference_2022_path,
    call. = FALSE
  )
}
refs_2016 <- data.table::fread(predicts_reference_2016_path, encoding = "UTF-8")
refs_2022 <- data.table::fread(predicts_reference_2022_path, encoding = "UTF-8")

overlap <- screen_reference_overlap(refs_2016, refs_2022)
write_csv_safe(overlap$summary, predicts_reference_overlap_path)
blocking_overlap_details <- overlap$details[overlap_key != "_id"]
if (nrow(blocking_overlap_details) > 0) {
  details_path <- sub("\\.csv$", "_details.csv", predicts_reference_overlap_path)
  write_csv_safe(blocking_overlap_details, details_path)
  stop(
    "Reference overlap detected between 2016 and 2022 tables. Review: ",
    details_path,
    call. = FALSE
  )
}
references <- ascii_normalize_table(combine_reference_tables(refs_2016, refs_2022))

message("Filtering PREDICTS to abundance records with sampling starts dated 2000-2024.")
predicts_dt[, Sample_start_earliest := as_date_safe(Sample_start_earliest)]
predicts_filtered <- predicts_dt[
  Diversity_metric_type == "Abundance" &
    !is.na(Sample_start_earliest) &
    Sample_start_earliest >= date_min &
    Sample_start_earliest <= date_max
]
predicts_filtered_cols_available <- intersect(predicts_filtered_cols, names(predicts_filtered))
predicts_filtered <- predicts_filtered[, ..predicts_filtered_cols_available]
predicts_filtered <- ascii_normalize_table(predicts_filtered)
saveRDS(as.data.frame(predicts_filtered), predicts_filtered_rds_path, compress = "gzip")
message("Wrote: ", predicts_filtered_rds_path)

write_csv_safe(columns, predicts_columns_path)
write_csv_safe(references, predicts_references_path)
write_csv_safe(utils::head(predicts_filtered, preview_n_rows), predicts_database_preview_path)
write_csv_safe(utils::head(data.table::as.data.table(sites), preview_n_rows), predicts_sites_preview_path)

message("Writing PREDICTS column inventory.")
column_stats <- data.table::rbindlist(
  lapply(names(predicts_filtered), function(col_name) {
    cbind(
      data.table::data.table(column = col_name),
      summarize_column(predicts_filtered[[col_name]])
    )
  })
)

if ("Column" %in% names(columns)) {
  columns_dt <- data.table::as.data.table(columns)
  data.table::setnames(columns_dt, "Column", "column")
  column_inventory <- merge(column_stats, columns_dt, by = "column", all.x = TRUE, sort = FALSE)
} else {
  column_inventory <- column_stats
}
write_csv_safe(column_inventory, predicts_column_inventory_path)

rds_outputs <- data.table::data.table(
  output_name = c(
    "predicts_database_raw",
    "predicts_database_filtered",
    "predicts_site_summaries"
  ),
  path = c(
    predicts_raw_rds_path,
    predicts_filtered_rds_path,
    predicts_sites_rds_path
  ),
  rows = c(
    nrow(predicts_dt),
    nrow(predicts_filtered),
    nrow(data.table::as.data.table(sites))
  ),
  columns = c(
    ncol(predicts_dt),
    ncol(predicts_filtered),
    ncol(data.table::as.data.table(sites))
  ),
  description = c(
    "Raw combined PREDICTS extract from predictsr, downloaded with extract = c(2016, 2022).",
    "Abundance PREDICTS records with Sample_start_earliest from 2000-01-01 through 2024-12-31, using the configured retained columns.",
    "Combined PREDICTS site-level summaries from predictsr."
  )
)
write_csv_safe(rds_outputs, rds_outputs_manifest_path)

summary_table <- data.table::data.table(
  artifact = c(
    "raw_combined_database_rds",
    "filtered_database_rds",
    "column_descriptions_csv",
    "combined_reference_table_csv",
    "database_preview_csv",
    "site_summaries_rds",
    "site_summaries_preview_csv",
    "column_inventory_csv",
    "reference_overlap_screen_csv",
    "rds_outputs_manifest_csv"
  ),
  path = c(
    predicts_raw_rds_path,
    predicts_filtered_rds_path,
    predicts_columns_path,
    predicts_references_path,
    predicts_database_preview_path,
    predicts_sites_rds_path,
    predicts_sites_preview_path,
    predicts_column_inventory_path,
    predicts_reference_overlap_path,
    rds_outputs_manifest_path
  ),
  rows = c(
    nrow(predicts_dt),
    nrow(predicts_filtered),
    nrow(columns),
    nrow(references),
    min(preview_n_rows, nrow(predicts_filtered)),
    nrow(data.table::as.data.table(sites)),
    min(preview_n_rows, nrow(data.table::as.data.table(sites))),
    nrow(column_inventory),
    nrow(overlap$summary),
    nrow(rds_outputs)
  ),
  columns = c(
    ncol(predicts_dt),
    ncol(predicts_filtered),
    ncol(columns),
    ncol(references),
    ncol(predicts_filtered),
    ncol(data.table::as.data.table(sites)),
    ncol(data.table::as.data.table(sites)),
    ncol(column_inventory),
    ncol(overlap$summary),
    ncol(rds_outputs)
  )
)
write_csv_safe(summary_table, predicts_summary_path)

readme_lines <- c(
  "# PREDICTS Inputs",
  "",
  "Generated by `code/02_get_predicts.R`.",
  "",
  "Downloads follow the `predictsr` package workflow: `LoadPredictsData()`,",
  "`GetSitelevelSummaries()`, and `GetColumnDescriptions()`.",
  "",
  "The database preview is filtered to abundance records with `Sample_start_earliest`",
  "from 2000-01-01 through 2024-12-31.",
  "",
  "## Human-readable CSVs",
  "",
  paste0("- Reference table: `", predicts_references_path, "`"),
  paste0("- Database preview: `", predicts_database_preview_path, "`"),
  paste0("- Data extract columns: `", predicts_columns_path, "`"),
  paste0("- Column inventory: `", predicts_column_inventory_path, "`"),
  paste0("- Reference overlap screen: `", predicts_reference_overlap_path, "`"),
  "",
  "## RDS Outputs",
  "",
  paste0("- RDS manifest: `", rds_outputs_manifest_path, "`"),
  paste0("- Filtered database RDS: `", predicts_filtered_rds_path, "`"),
  "",
  "## Summary",
  "",
  paste0("- Raw combined database rows: ", format(nrow(predicts_dt), big.mark = ",")),
  paste0("- Filtered database rows: ", format(nrow(predicts_filtered), big.mark = ",")),
  paste0("- Combined reference rows: ", format(nrow(references), big.mark = ",")),
  paste0("- Site summary rows: ", format(nrow(data.table::as.data.table(sites)), big.mark = ","))
)
writeLines(readme_lines, predicts_readme_path)
message("Wrote: ", predicts_readme_path)

message("PREDICTS download/prep complete.")
