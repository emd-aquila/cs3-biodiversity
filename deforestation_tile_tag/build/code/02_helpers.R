# =====================================================
# Helper functions
# =====================================================

assert_has_cols <- function(data, cols, data_name) {
  missing_cols <- setdiff(cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        data_name,
        " is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

assert_no_duplicate_keys <- function(data, keys, data_name) {
  duplicate_rows <- data %>%
    count(across(all_of(keys)), name = "n") %>%
    filter(n > 1)
  
  if (nrow(duplicate_rows) > 0) {
    stop(
      paste0(
        data_name,
        " has duplicate key rows for: ",
        paste(keys, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

read_or_build <- function(path, build_fn, rebuild = FALSE) {
  if (!rebuild && file.exists(path)) {
    message("Loading cached object: ", basename(path))
    return(readRDS(path))
  }
  
  message("Building object: ", basename(path))
  value <- build_fn()
  saveRDS(value, path)
  value
}

write_csv_safe <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, path)
  message("Wrote: ", path)
}

write_gpkg_safe <- function(data, path, delete_dsn = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(data, dsn = path, delete_dsn = delete_dsn, quiet = TRUE)
  message("Wrote: ", path)
}