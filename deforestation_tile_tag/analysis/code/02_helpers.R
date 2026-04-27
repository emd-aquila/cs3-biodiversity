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

round_numeric_cols_except <- function(df, digits = 1, exclude = character()) {
  df %>%
    mutate(
      across(
        where(is.numeric) & !any_of(exclude),
        ~ round(.x, digits)
      )
    )
}

log_buffer_run <- function(buffer_km) {
  message("\n==============================")
  message("Running analysis for buffer_km = ", buffer_km)
  message("==============================")
}

buffer_key <- function(buffer_km) {
  paste0("buf_", buffer_km, "km")
}

log_cluster_run <- function(cluster_method, cluster_radius_km) {
  message("\n======================================")
  message(
    "Running analysis for cluster config: ",
    cluster_method,
    " / ",
    sprintf("%.1f", cluster_radius_km),
    " km"
  )
  message("======================================")
}

assert_objects_exist <- function(object_names, script_name = "script") {
  missing_objects <- object_names[!vapply(object_names, exists, logical(1))]
  
  if (length(missing_objects) > 0) {
    stop(
      paste0(
        script_name,
        " is missing required objects: ",
        paste(missing_objects, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

get_available_ov_score_specs <- function(cluster_year_ov) {
  assert_has_cols(
    cluster_year_ov,
    c("AEZ", "cluster_id", "year", "median_ov_year", "n_sites_year"),
    "cluster_year_ov"
  )

  specs <- ov_score_specs %>%
    filter(cluster_year_col %in% names(cluster_year_ov))

  if (!"ov_full" %in% specs$ov_method) {
    stop("cluster_year_ov is missing the required full-OV column: median_ov_year", call. = FALSE)
  }

  missing_variant_cols <- setdiff(ov_score_specs$cluster_year_col, names(cluster_year_ov))
  if (length(missing_variant_cols) > 0) {
    warning(
      "Some OV variant columns are not available in cluster_year_ov and will be skipped: ",
      paste(missing_variant_cols, collapse = ", "),
      call. = FALSE
    )
  }

  specs
}

build_year_pair_ov_table <- function(cluster_year_ov, ov_specs) {
  base <- cluster_year_ov %>%
    arrange(AEZ, cluster_id, year) %>%
    group_by(AEZ, cluster_id) %>%
    mutate(
      year_t2 = lead(year),
      n_sites_t2 = lead(n_sites_year)
    ) %>%
    ungroup() %>%
    filter(!is.na(year_t2)) %>%
    transmute(
      AEZ,
      cluster_id,
      year_t1 = year,
      year_t2 = as.integer(year_t2),
      year_gap = year_t2 - year_t1,
      n_sites_t1 = n_sites_year,
      n_sites_t2 = n_sites_t2
    )

  variant_tables <- purrr::map(
    seq_len(nrow(ov_specs)),
    function(i) {
      spec <- ov_specs[i, ]
      source_col <- spec$cluster_year_col[[1]]
      t1_col <- spec$t1_col[[1]]
      t2_col <- spec$t2_col[[1]]
      delta_col <- spec$delta_col[[1]]
      annualized_col <- spec$annualized_col[[1]]

      cluster_year_ov %>%
        arrange(AEZ, cluster_id, year) %>%
        group_by(AEZ, cluster_id) %>%
        mutate(
          year_t2 = lead(year),
          ov_next = lead(.data[[source_col]])
        ) %>%
        ungroup() %>%
        filter(!is.na(year_t2)) %>%
        transmute(
          AEZ,
          cluster_id,
          year_t1 = year,
          !!t1_col := .data[[source_col]],
          !!t2_col := ov_next,
          !!delta_col := ov_next - .data[[source_col]],
          !!annualized_col := (ov_next - .data[[source_col]]) / (year_t2 - year)
        )
    }
  )

  purrr::reduce(variant_tables, left_join, .init = base, by = c("AEZ", "cluster_id", "year_t1"))
}

build_whole_cluster_ov_table <- function(cluster_year_ov, ov_specs) {
  base <- cluster_year_ov %>%
    arrange(AEZ, cluster_id, year) %>%
    group_by(AEZ, cluster_id) %>%
    summarise(
      year_start = first(year),
      year_final = last(year),
      year_gap = year_final - year_start,
      n_sites_start = first(n_sites_year),
      n_sites_final = last(n_sites_year),
      n_years_observed = n(),
      .groups = "drop"
    ) %>%
    filter(n_years_observed >= 2L)

  variant_tables <- purrr::map(
    seq_len(nrow(ov_specs)),
    function(i) {
      spec <- ov_specs[i, ]
      source_col <- spec$cluster_year_col[[1]]
      t1_col <- spec$t1_col[[1]]
      t2_col <- spec$t2_col[[1]]
      delta_col <- spec$delta_col[[1]]
      annualized_col <- spec$annualized_col[[1]]

      cluster_year_ov %>%
        arrange(AEZ, cluster_id, year) %>%
        group_by(AEZ, cluster_id) %>%
        summarise(
          year_start = first(year),
          year_final = last(year),
          year_gap = year_final - year_start,
          n_years_observed = n(),
          !!t1_col := first(.data[[source_col]]),
          !!t2_col := last(.data[[source_col]]),
          !!delta_col := last(.data[[source_col]]) - first(.data[[source_col]]),
          !!annualized_col := dplyr::if_else(
            year_gap > 0,
            (last(.data[[source_col]]) - first(.data[[source_col]])) / year_gap,
            NA_real_
          ),
          .groups = "drop"
        ) %>%
        filter(n_years_observed >= 2L) %>%
        select(AEZ, cluster_id, all_of(c(t1_col, t2_col, delta_col, annualized_col)))
    }
  )

  purrr::reduce(variant_tables, left_join, .init = base, by = c("AEZ", "cluster_id"))
}
