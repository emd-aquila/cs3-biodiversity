# =====================================================
# Helpers for the PREDICTS build pipeline
# =====================================================

assert_exists <- function(path) {
  if (!file.exists(path)) {
    stop("Missing required file: ", path, call. = FALSE)
  }
}

write_csv_safe <- function(df, path) {
  readr::write_csv(df, path)
  message("Wrote: ", path)
}
