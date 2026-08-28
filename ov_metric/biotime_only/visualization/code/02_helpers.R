assert_file_exists <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " does not exist: ", path, call. = FALSE)
  }
}

assert_has_cols <- function(data, cols, label) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

save_plot <- function(plot, filename, width = 10, height = 7, subdir = NULL) {
  path <- if (is.null(subdir)) {
    file.path(figure_dir, filename)
  } else {
    file.path(figure_dir, subdir, filename)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
  message("Wrote: ", path)
  invisible(path)
}

base_theme <- function() {
  ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey92", color = "grey70"),
      legend.position = "bottom"
    )
}

sanitize_filename <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "unknown")
}

ordered_aez_levels <- function(x) {
  values <- unique(as.character(x))
  numeric_key <- suppressWarnings(as.integer(gsub("[^0-9]+", "", values)))
  values[order(is.na(numeric_key), numeric_key, values)]
}

set_aez_order <- function(data) {
  if ("AEZ" %in% names(data)) {
    data$AEZ <- factor(as.character(data$AEZ), levels = ordered_aez_levels(data$AEZ))
  }
  data
}

plot_limits <- function(data) {
  x <- data[[predictor_col]]
  y <- data[[response_col]]
  x_range <- range(x[is.finite(x)], na.rm = TRUE)
  y_range <- range(y[is.finite(y)], na.rm = TRUE)
  if (!all(is.finite(x_range)) || x_range[1] == x_range[2]) {
    x_range <- x_range + c(-0.5, 0.5)
  }
  if (!all(is.finite(y_range)) || y_range[1] == y_range[2]) {
    y_range <- y_range + c(-0.5, 0.5)
  }
  list(x = x_range, y = y_range)
}

annotation_position <- function(limits) {
  data.frame(
    label_x = limits$x[1] + 0.025 * diff(limits$x),
    label_y = limits$y[2] - 0.06 * diff(limits$y)
  )
}

format_r2_label <- function(value, prefix = "R^2") {
  ifelse(is.finite(value), paste0(prefix, " = ", sprintf("%.3f", value)), paste0(prefix, " = NA"))
}

r2_annotation_layer <- function(annotation_data) {
  ggplot2::geom_label(
    data = annotation_data,
    ggplot2::aes(x = label_x, y = label_y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 3.2,
    color = "#1F2933",
    fill = "white",
    alpha = 0.88,
    linewidth = 0.18,
    label.r = grid::unit(0.08, "lines")
  )
}
