# =====================================================
# 07_master_output_table.R
# Build one master regression report table from fit-stats outputs.
# =====================================================

message("Building master regression output table...")

fit_stats_files <- list.files(
  output_dir,
  pattern = "^fit_stats(?:__.*)?\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

fit_stats_files <- fit_stats_files[!grepl("fit_stats__comparison\\.csv$", fit_stats_files)]

if (length(fit_stats_files) == 0) {
  warning("No fit_stats CSV files found under regression/output. Skipping master output table.")
} else {
  format_readable_stat <- function(x, digits = 4) {
    dplyr::if_else(
      is.na(x),
      NA_character_,
      formatC(signif(x, digits), digits = digits, format = "fg", flag = "#")
    )
  }

  format_integerish <- function(x) {
    dplyr::if_else(
      is.na(x),
      NA_character_,
      as.character(as.integer(round(x)))
    )
  }

  fit_stats_manifest <- tibble::tibble(path = fit_stats_files) %>%
    mutate(
      path_normalized = normalizePath(path, winslash = "/", mustWork = FALSE),
      rel_path = stringr::str_remove(
        path_normalized,
        paste0("^", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "/")
      ),
      path_parts = stringr::str_split(rel_path, "/"),
      n_parts = purrr::map_int(path_parts, length),
      cluster_method = purrr::map_chr(path_parts, ~ if (length(.x) >= 1) .x[[1]] else NA_character_),
      cluster_radius_dir = purrr::map_chr(path_parts, ~ if (length(.x) >= 2) .x[[2]] else NA_character_),
      buffer_dir = purrr::map_chr(path_parts, ~ if (length(.x) >= 3) .x[[3]] else NA_character_),
      regression_scale = purrr::map_chr(
        path_parts,
        ~ if (length(.x) >= 8) .x[[4]] else "non_annualized"
      ),
      ov_approach = purrr::map_chr(
        path_parts,
        ~ if (length(.x) >= 8) .x[[5]] else if (length(.x) >= 4) .x[[4]] else NA_character_
      ),
      defor_approach = purrr::map_chr(
        path_parts,
        ~ if (length(.x) >= 8) .x[[6]] else if (length(.x) >= 5) .x[[5]] else NA_character_
      ),
      defor_transform = purrr::map_chr(
        path_parts,
        ~ if (length(.x) >= 8) .x[[7]] else if (length(.x) >= 6) .x[[6]] else NA_character_
      ),
      filename = basename(path_normalized),
      buffer_km = readr::parse_number(buffer_dir),
      cluster_radius_km = readr::parse_number(cluster_radius_dir),
      sample_tag = dplyr::case_when(
        defor_transform == "rlm" ~ "rlm",
        TRUE ~ "ols"
      ),
      defor_approach_order = match(defor_approach, defor_approaches),
      defor_transform_order = match(defor_transform, defor_transforms),
      regression_scale_order = match(regression_scale, regression_scales),
      sample_order = dplyr::case_when(
        sample_tag == "ols" ~ 1,
        sample_tag == "rlm" ~ 2,
        TRUE ~ 99
      )
    ) %>%
    filter(
      n_parts >= 7,
      cluster_method == "clara",
      buffer_km %in% c(1, 5, 10)
    )

  master_fit_stats <- purrr::map2_dfr(
    fit_stats_manifest$path,
    seq_len(nrow(fit_stats_manifest)),
    ~ {
      manifest_row <- fit_stats_manifest[.y, ]
      fit_stats_df <- readr::read_csv(.x, show_col_types = FALSE)

      fit_stats_df %>%
        mutate(
          cluster_method = manifest_row$cluster_method,
          cluster_radius_km = manifest_row$cluster_radius_km,
          buffer_km = manifest_row$buffer_km,
          regression_scale = manifest_row$regression_scale,
          ov_approach = manifest_row$ov_approach,
          defor_approach = manifest_row$defor_approach,
          defor_transform = manifest_row$defor_transform,
          sample_tag = manifest_row$sample_tag,
          source_file = manifest_row$rel_path,
          defor_approach_order = manifest_row$defor_approach_order,
          defor_transform_order = manifest_row$defor_transform_order,
          regression_scale_order = manifest_row$regression_scale_order,
          sample_order = manifest_row$sample_order
        )
    }
  ) %>%
    mutate(
      AEZ_num = readr::parse_number(as.character(AEZ)),
      cluster_radius_km = format_integerish(cluster_radius_km),
      buffer_km = format_integerish(buffer_km),
      n_obs = format_integerish(n_obs),
      n_unique_regressor = format_integerish(n_unique_regressor),
      slope = format_readable_stat(slope),
      r_squared = format_readable_stat(r_squared),
      adj_r_squared = format_readable_stat(adj_r_squared),
      correlation = format_readable_stat(correlation)
    ) %>%
    arrange(
      cluster_method,
      cluster_radius_km,
      buffer_km,
      regression_scale_order,
      ov_approach,
      defor_approach_order,
      defor_transform_order,
      sample_order,
      AEZ_num,
      AEZ,
      model
    ) %>%
    dplyr::select(
      cluster_method,
      cluster_radius_km,
      buffer_km,
      regression_scale,
      ov_approach,
      defor_approach,
      defor_transform,
      model,
      AEZ,
      n_obs,
      n_unique_regressor,
      slope,
      r_squared,
      adj_r_squared,
      correlation
    )

  readr::write_csv(
    master_fit_stats,
    file.path(output_dir, "master_fit_stats_report.csv")
  )

  message("Wrote master regression report table: ", file.path(output_dir, "master_fit_stats_report.csv"))
}
