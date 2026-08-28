# =====================================================
# Helpers for the PREDICTS analysis pipeline
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

minmax01 <- function(x) {
  r <- range(x, na.rm = TRUE)

  if (!all(is.finite(r)) || r[1] == r[2]) {
    return(rep(NA_real_, length(x)))
  }

  (x - r[1]) / (r[2] - r[1])
}
