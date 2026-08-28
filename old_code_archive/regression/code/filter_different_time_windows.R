# =====================================================
# filter_different_time_windows.R
# Explore inclusive OV-observation windows from 1-2 through 6-7 years.
# =====================================================

message("Starting time-window regression exploration...")

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))))
  }

  normalizePath(getwd())
}

code_dir <- get_script_dir()
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(code_dir)

source("00_libraries.R")
source("01_config.R")
source("02_helpers.R")

window_output_dir <- normalizePath(
  file.path(regression_dir, "filter_different_time_windows"),
  winslash = "/",
  mustWork = FALSE
)
dir.create(window_output_dir, recursive = TRUE, showWarnings = FALSE)

window_plot_output_dir <- file.path(window_output_dir, "noteworthy_window_plots")

window_grid <- tidyr::crossing(
  min_year_gap = 1:6,
  max_year_gap = 2:7
) %>%
  filter(min_year_gap < max_year_gap) %>%
  mutate(window_label = paste0(min_year_gap, "-", max_year_gap, " years"))

parse_path_value <- function(path, pattern) {
  match <- stringr::str_match(path, pattern)
  if (is.na(match[, 2])) NA_character_ else match[, 2]
}

parse_transition_path <- function(path) {
  tibble::tibble(
    path = path,
    cluster_method = parse_path_value(path, "/output/([^/]+)/radius_"),
    cluster_radius_km = as.numeric(parse_path_value(path, "/radius_([0-9.]+)km/")),
    buffer_km = as.numeric(parse_path_value(path, "/buf_([0-9.]+)km/")),
    delta_ov_approach = dplyr::case_when(
      grepl("/ov_year_pair/", path) ~ "year_pair_delta_ov",
      grepl("/ov_whole_cluster/", path) ~ "whole_cluster_delta_ov",
      TRUE ~ NA_character_
    ),
    single_tile_collapse_mode = dplyr::case_when(
      grepl("/collapse_off/", path) ~ "keep_single_tile_clusters",
      grepl("/collapse_on/", path) ~ "collapse_single_tile_clusters",
      TRUE ~ NA_character_
    )
  )
}

build_exposure_specs <- function(data) {
  available <- list(
    baseline_deforestation = "delta_defor_treecover_share_raw",
    cutoff_defor = "delta_defor_treecover_share_raw_cutoff",
    lagged_deforestation = "delta_defor_treecover_share_raw_lagged"
  )

  # Legacy outputs used the lagged suffix for the cutoff treatment. Only treat
  # it as a true lag after the new explicit cutoff column proves the new schema
  # is present.
  if (!"delta_defor_treecover_share_raw_cutoff" %in% names(data)) {
    available <- available["baseline_deforestation"]
  }

  available[vapply(available, function(col) col %in% names(data), logical(1))]
}

prepare_window_data <- function(data, defor_col, min_year_gap, max_year_gap) {
  data %>%
    mutate(
      aez = standardize_aez_order(aez),
      year_gap = as.integer(year_gap),
      delta_ov_annualized_selected = as.numeric(delta_ov) / year_gap,
      delta_defor_annualized_selected = as.numeric(.data[[defor_col]]) / year_gap,
      year_gap_label = factor(
        paste0(year_gap, " year", if_else(year_gap == 1L, "", "s")),
        levels = paste0(1:7, " year", if_else(1:7 == 1L, "", "s"))
      )
    ) %>%
    filter(
      year_gap >= min_year_gap,
      year_gap <= max_year_gap,
      is.finite(delta_ov_annualized_selected),
      is.finite(delta_defor_annualized_selected),
      !is.na(aez)
    )
}

fit_window_model <- function(data, defor_col, min_year_gap, max_year_gap) {
  data_window <- prepare_window_data(
    data,
    defor_col,
    min_year_gap,
    max_year_gap
  )

  if (
    nrow(data_window) < 10 ||
      n_distinct(data_window$delta_ov_annualized_selected) < 2 ||
      n_distinct(data_window$delta_defor_annualized_selected) < 2
  ) {
    return(tibble::tibble(
      n_obs = nrow(data_window),
      n_clusters = n_distinct(data_window$cluster_id),
      n_aez = n_distinct(data_window$aez),
      correlation = NA_real_,
      slope = NA_real_,
      p_value = NA_real_,
      r_squared = NA_real_
    ))
  }

  model <- stats::lm(
    delta_ov_annualized_selected ~ delta_defor_annualized_selected + aez,
    data = data_window
  )
  coef_table <- summary(model)$coefficients
  regressor_row <- coef_table["delta_defor_annualized_selected", , drop = FALSE]

  tibble::tibble(
    n_obs = nrow(data_window),
    n_clusters = n_distinct(data_window$cluster_id),
    n_aez = n_distinct(data_window$aez),
    correlation = stats::cor(
      data_window$delta_defor_annualized_selected,
      data_window$delta_ov_annualized_selected
    ),
    slope = unname(regressor_row[1, "Estimate"]),
    p_value = unname(regressor_row[1, "Pr(>|t|)"]),
    r_squared = summary(model)$r.squared
  )
}

format_path_number <- function(value) {
  format(value, trim = TRUE, scientific = FALSE)
}

build_noteworthy_plot_path <- function(result_row) {
  file.path(
    window_plot_output_dir,
    result_row$cluster_method,
    paste0("radius_", format_path_number(result_row$cluster_radius_km), "km"),
    paste0("buf_", format_path_number(result_row$buffer_km), "km"),
    result_row$delta_ov_approach,
    result_row$single_tile_collapse_mode,
    result_row$defor_bin,
    paste0("window_", result_row$min_year_gap, "_", result_row$max_year_gap, "_years"),
    "ols.png"
  )
}

summarize_aez_plot_data <- function(data) {
  data %>%
    group_by(aez) %>%
    summarise(
      n_obs = n(),
      n_clusters = n_distinct(cluster_id),
      correlation = if (
        n() >= 2 &&
          n_distinct(delta_defor_annualized_selected) >= 2 &&
          n_distinct(delta_ov_annualized_selected) >= 2
      ) {
        stats::cor(
          delta_defor_annualized_selected,
          delta_ov_annualized_selected
        )
      } else {
        NA_real_
      },
      facet_label = paste0(
        "n = ", n_obs,
        "\nclusters = ", n_clusters,
        "\nr = ", if_else(is.finite(correlation), sprintf("%.3f", correlation), "n/a")
      ),
      .groups = "drop"
    )
}

write_noteworthy_window_plot <- function(transition_data, result_row) {
  plot_data <- prepare_window_data(
    transition_data,
    result_row$defor_source_col,
    result_row$min_year_gap,
    result_row$max_year_gap
  )

  aez_summary <- summarize_aez_plot_data(plot_data)
  fitted_line_aez <- aez_summary %>%
    filter(n_obs >= 2, is.finite(correlation)) %>%
    pull(aez) %>%
    as.character()
  fitted_line_data <- plot_data %>%
    filter(as.character(aez) %in% fitted_line_aez)

  plot_path <- build_noteworthy_plot_path(result_row)
  dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)

  plot_obj <- ggplot(
    plot_data,
    aes(
      x = delta_defor_annualized_selected,
      y = delta_ov_annualized_selected,
      color = year_gap_label
    )
  ) +
    geom_hline(yintercept = 0, color = "grey75", linewidth = 0.3) +
    geom_vline(xintercept = 0, color = "grey75", linewidth = 0.3) +
    geom_point(alpha = 0.55, size = 1.5) +
    geom_smooth(
      data = fitted_line_data,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "steelblue4",
      linewidth = 0.8
    ) +
    geom_label(
      data = aez_summary,
      aes(label = facet_label),
      x = -Inf,
      y = Inf,
      hjust = -0.05,
      vjust = 1.05,
      size = 2.8,
      linewidth = 0.2,
      fill = "white",
      alpha = 0.85,
      color = "grey15",
      inherit.aes = FALSE
    ) +
    facet_wrap(
      ~ aez,
      scales = "free",
      axes = "all",
      axis.labels = "all"
    ) +
    scale_color_viridis_d(name = "OV reading gap") +
    labs(
      title = paste0(
        "AEZ-specific annualized OV-deforestation patterns | ",
        result_row$window_label
      ),
      subtitle = stringr::str_wrap(
        paste(
          paste0("radius_", format_path_number(result_row$cluster_radius_km), "km"),
          paste0("buf_", format_path_number(result_row$buffer_km), "km"),
          result_row$delta_ov_approach,
          result_row$defor_bin,
          paste0(
            "overall r = ",
            if_else(
              is.finite(result_row$correlation),
              sprintf("%.3f", result_row$correlation),
              "n/a"
            )
          ),
          paste0(
            "AEZ-adjusted slope = ",
            if_else(is.finite(result_row$slope), sprintf("%.3f", result_row$slope), "n/a")
          ),
          paste0(
            "p = ",
            if_else(is.finite(result_row$p_value), format.pval(result_row$p_value, digits = 3), "n/a")
          ),
          sep = " | "
        ),
        width = 115
      ),
      x = "Annualized deforestation share of baseline tree cover",
      y = "Annualized delta OV",
      caption = "Points are cluster transitions. Blue lines are separate descriptive OLS fits within each AEZ; screening p-values use the pooled AEZ-adjusted model."
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  n_facets <- max(1L, nrow(aez_summary))
  ggplot2::ggsave(
    filename = plot_path,
    plot = plot_obj,
    width = 14,
    height = max(6, 2.8 * ceiling(n_facets / 4)),
    dpi = 200
  )

  plot_path
}

write_noteworthy_window_plots <- function(noteworthy_windows) {
  if (nrow(noteworthy_windows) == 0) {
    return(tibble::tibble())
  }

  dir.create(window_plot_output_dir, recursive = TRUE, showWarnings = FALSE)
  path_groups <- split(noteworthy_windows, noteworthy_windows$path)
  plot_index <- 0L

  purrr::map_dfr(
    path_groups,
    function(path_rows) {
      transition_data <- readr::read_csv(path_rows$path[[1]], show_col_types = FALSE) %>%
        normalize_cluster_deltas_schema()

      purrr::pmap_dfr(
        path_rows,
        function(...) {
          result_row <- tibble::tibble(...)
          plot_index <<- plot_index + 1L

          if (plot_index %% 25L == 0L || plot_index == nrow(noteworthy_windows)) {
            message("Writing noteworthy AEZ plot ", plot_index, "/", nrow(noteworthy_windows))
          }

          result_row %>%
            mutate(plot_path = write_noteworthy_window_plot(transition_data, result_row))
        }
      )
    }
  )
}

cluster_delta_paths <- list.files(
  file.path(repo_root, "deforestation_tile_tag", "analysis", "output"),
  pattern = "^cluster_deltas\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(cluster_delta_paths) == 0) {
  stop("No cluster_deltas.csv files were found. Run deforestation tagging analysis first.", call. = FALSE)
}

window_results <- purrr::map_dfr(
  cluster_delta_paths,
  function(path) {
    path_metadata <- parse_transition_path(path)
    transition_data <- readr::read_csv(path, show_col_types = FALSE) %>%
      normalize_cluster_deltas_schema()
    exposure_specs <- build_exposure_specs(transition_data)

    purrr::imap_dfr(
      exposure_specs,
      function(defor_col, defor_bin) {
        purrr::pmap_dfr(
          window_grid,
          function(min_year_gap, max_year_gap, window_label) {
            fit_window_model(
              transition_data,
              defor_col,
              min_year_gap,
              max_year_gap
            ) %>%
              mutate(
                defor_bin = defor_bin,
                defor_source_col = defor_col,
                min_year_gap = min_year_gap,
                max_year_gap = max_year_gap,
                window_label = window_label,
                .before = 1
              )
          }
        )
      }
    ) %>%
      bind_cols(path_metadata[rep(1, nrow(.)), ])
  }
) %>%
  mutate(
    pattern_flag = case_when(
      is.finite(p_value) & p_value < 0.05 ~ "p < 0.05",
      is.finite(correlation) & abs(correlation) >= 0.10 ~ "|correlation| >= 0.10",
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(
    cluster_method,
    cluster_radius_km,
    buffer_km,
    delta_ov_approach,
    single_tile_collapse_mode,
    defor_bin
  ) %>%
  arrange(
    cluster_radius_km,
    buffer_km,
    delta_ov_approach,
    defor_bin,
    min_year_gap,
    max_year_gap
  )

noteworthy_windows <- window_results %>%
  filter(!is.na(pattern_flag)) %>%
  arrange(p_value, desc(abs(correlation)))

readr::write_csv(
  window_results,
  file.path(window_output_dir, "time_window_regression_results.csv")
)
readr::write_csv(
  noteworthy_windows,
  file.path(window_output_dir, "noteworthy_time_windows.csv")
)

noteworthy_window_plot_manifest <- write_noteworthy_window_plots(noteworthy_windows)
readr::write_csv(
  noteworthy_window_plot_manifest,
  file.path(window_output_dir, "noteworthy_window_plot_manifest.csv")
)

message("Wrote time-window results: ", file.path(window_output_dir, "time_window_regression_results.csv"))
message("Flagged noteworthy rows: ", nrow(noteworthy_windows))
message("Wrote noteworthy AEZ plot manifest: ", file.path(window_output_dir, "noteworthy_window_plot_manifest.csv"))
