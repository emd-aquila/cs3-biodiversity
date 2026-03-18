# =====================================================
# Helper functions for analysis pipeline
# =====================================================

# -----------------------
# Assertions
# -----------------------

assert_exists <- function(path) {
  if (!file.exists(path)) {
    stop("Missing required file: ", path, call. = FALSE)
  }
}

assert_has_cols <- function(data, cols, data_name = deparse(substitute(data))) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        data_name, " is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

# -----------------------
# Safe writers
# -----------------------

write_csv_safe <- function(df, path) {
  readr::write_csv(df, path)
  message("Wrote: ", path)
}

write_rds_safe <- function(object, path) {
  saveRDS(object, path)
  message("Wrote: ", path)
}

write_plot_safe <- function(plot_obj, path, width = 10, height = 6, dpi = 300) {
  ggplot2::ggsave(
    filename = path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
  message("Wrote: ", path)
}

# -----------------------
# Ordering helpers
# -----------------------

standardize_aez_order <- function(x) {
  x_chr <- as.character(x)
  x_num <- readr::parse_number(x_chr)
  factor(x_chr, levels = unique(x_chr[order(x_num, x_chr)]))
}

# -----------------------
# Summary helpers
# -----------------------

calc_minmax <- function(x, na.rm = TRUE) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  max(x, na.rm = na.rm) - min(x, na.rm = na.rm)
}

calc_iqr <- function(x, na.rm = TRUE) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  stats::IQR(x, na.rm = na.rm)
}

round_numeric_cols <- function(df, digits = 1) {
  df %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, digits)
      )
    )
}