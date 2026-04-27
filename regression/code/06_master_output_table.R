# =====================================================
# 06_master_output_table.R
# Build one master regression report table from fit-stats outputs.
# Supports the current folder layout:
# cluster_method / radius / buffer / grouping / ov_calculation / exposure / scale / ov / summing / data / model_family / transform
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
  format_readable_stat <- function(x, digits = 3) {
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

  parse_fit_stats_manifest <- function(path_vector) {
    output_root <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
    defor_approach_lookup <- purrr::imap_dfr(
      defor_approach_specs,
      ~ tibble::tibble(
        defor_approach = .y,
        defor_summing = .x$summing_dir,
        defor_data_type = .x$data_dir
      )
    )
    grouping_lookup <- purrr::imap_dfr(
      regression_grouping_specs,
      ~ tibble::tibble(grouping_level = .y, grouping_dir = .x$output_dir)
    )
    exposure_lookup <- purrr::imap_dfr(
      defor_exposure_mode_specs,
      ~ tibble::tibble(defor_exposure_mode = .y, exposure_dir = .x$output_dir)
    )
    get_path_part <- function(parts, idx) {
      if (length(parts) >= idx) parts[[idx]] else NA_character_
    }

    tibble::tibble(path = path_vector) %>%
      mutate(
        path_normalized = normalizePath(path, winslash = "/", mustWork = FALSE),
        rel_path = stringr::str_remove(
          path_normalized,
          paste0("^", output_root, "/")
        ),
        path_parts = stringr::str_split(rel_path, "/"),
        n_parts = purrr::map_int(path_parts, length),
        cluster_method = purrr::map_chr(path_parts, ~ get_path_part(.x, 1)),
        cluster_radius_dir = purrr::map_chr(path_parts, ~ get_path_part(.x, 2)),
        buffer_dir = purrr::map_chr(path_parts, ~ get_path_part(.x, 3)),
        grouping_dir = purrr::map_chr(path_parts, ~ get_path_part(.x, 4)),
        ov_calculation_method = purrr::map_chr(path_parts, ~ get_path_part(.x, 5)),
        exposure_dir = purrr::map_chr(path_parts, ~ get_path_part(.x, 6)),
        regression_scale = purrr::map_chr(path_parts, ~ get_path_part(.x, 7)),
        ov_approach = purrr::map_chr(path_parts, ~ get_path_part(.x, 8)),
        defor_summing = purrr::map_chr(path_parts, ~ get_path_part(.x, 9)),
        defor_data_type = purrr::map_chr(path_parts, ~ get_path_part(.x, 10)),
        regression_model_family = purrr::map_chr(path_parts, ~ get_path_part(.x, 11)),
        defor_transform = purrr::map_chr(path_parts, ~ get_path_part(.x, 12)),
        filename = basename(path_normalized),
        buffer_km = readr::parse_number(buffer_dir),
        cluster_radius_km = readr::parse_number(cluster_radius_dir),
        sample_tag = regression_model_family,
        ov_calculation_order = match(ov_calculation_method, ov_calculation_methods),
        regression_scale_order = match(regression_scale, regression_scales),
        ov_approach_order = match(ov_approach, ov_approaches),
        defor_summing_order = match(defor_summing, unique(purrr::map_chr(defor_approach_specs, "summing_dir"))),
        defor_data_order = match(defor_data_type, unique(purrr::map_chr(defor_approach_specs, "data_dir"))),
        regression_model_family_order = match(regression_model_family, regression_model_families),
        defor_transform_order = match(defor_transform, defor_transforms),
        sample_order = regression_model_family_order
      ) %>%
      left_join(
        defor_approach_lookup,
        by = c("defor_summing", "defor_data_type")
      ) %>%
      left_join(
        grouping_lookup,
        by = "grouping_dir"
      ) %>%
      left_join(
        exposure_lookup,
        by = "exposure_dir"
      ) %>%
      mutate(
        grouping_level_order = match(grouping_level, names(regression_grouping_specs)),
        defor_exposure_order = match(defor_exposure_mode, names(defor_exposure_mode_specs))
      ) %>%
      filter(
        n_parts >= 13,
        cluster_method %in% cluster_methods,
        buffer_km %in% buffers
      )
  }

  fit_stats_manifest <- parse_fit_stats_manifest(fit_stats_files)

  if (nrow(fit_stats_manifest) == 0) {
    warning("No fit_stats CSV files matched the current output layout. Skipping master output table.")
  } else {
    master_fit_stats <- purrr::map2_dfr(
      fit_stats_manifest$path,
      seq_len(nrow(fit_stats_manifest)),
      ~ {
        manifest_row <- fit_stats_manifest[.y, ]
        fit_stats_df <- readr::read_csv(.x, show_col_types = FALSE)

        for (col_name in c(
          "cluster_method",
          "cluster_radius_km",
          "buffer_km",
          "grouping_level",
          "ov_calculation_method",
          "defor_exposure_mode",
          "regression_scale",
          "ov_approach",
          "defor_summing",
          "defor_data_type",
          "defor_approach",
          "regression_model_family",
          "defor_transform"
        )) {
          if (!col_name %in% names(fit_stats_df)) {
            fit_stats_df[[col_name]] <- NA
          }
        }

        fit_stats_df %>%
          mutate(
            cluster_method = dplyr::coalesce(cluster_method, manifest_row$cluster_method),
            cluster_radius_km = dplyr::coalesce(cluster_radius_km, manifest_row$cluster_radius_km),
            buffer_km = dplyr::coalesce(buffer_km, manifest_row$buffer_km),
            grouping_level = dplyr::coalesce(grouping_level, manifest_row$grouping_level),
            ov_calculation_method = dplyr::coalesce(ov_calculation_method, manifest_row$ov_calculation_method),
            defor_exposure_mode = dplyr::coalesce(defor_exposure_mode, manifest_row$defor_exposure_mode),
            regression_scale = dplyr::coalesce(regression_scale, manifest_row$regression_scale),
            ov_approach = dplyr::coalesce(ov_approach, manifest_row$ov_approach),
            defor_summing = dplyr::coalesce(defor_summing, manifest_row$defor_summing),
            defor_data_type = dplyr::coalesce(defor_data_type, manifest_row$defor_data_type),
            defor_approach = dplyr::coalesce(defor_approach, manifest_row$defor_approach),
            regression_model_family = dplyr::coalesce(regression_model_family, manifest_row$regression_model_family),
            defor_transform = dplyr::coalesce(defor_transform, manifest_row$defor_transform),
            sample_tag = manifest_row$sample_tag,
            source_file = manifest_row$rel_path,
            grouping_level_order = manifest_row$grouping_level_order,
            ov_calculation_order = manifest_row$ov_calculation_order,
            defor_exposure_order = manifest_row$defor_exposure_order,
            regression_scale_order = manifest_row$regression_scale_order,
            ov_approach_order = manifest_row$ov_approach_order,
            defor_summing_order = manifest_row$defor_summing_order,
            defor_data_order = manifest_row$defor_data_order,
            regression_model_family_order = manifest_row$regression_model_family_order,
            defor_transform_order = manifest_row$defor_transform_order,
            sample_order = manifest_row$sample_order
          )
      }
    ) %>%
      mutate(
        group_value = as.character(group_value)
      ) %>%
      mutate(
        group_value_num = NA_real_,
        group_value_num = replace(
          group_value_num,
        grouping_level == "aez",
          readr::parse_number(group_value[grouping_level == "aez"])
        ),
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
        grouping_level_order,
        ov_calculation_order,
        defor_exposure_order,
        regression_scale_order,
        ov_approach_order,
        defor_summing_order,
        defor_data_order,
        regression_model_family_order,
        defor_transform_order,
        sample_order,
        group_value_num,
        group_value,
        model
      ) %>%
      dplyr::select(
        cluster_method,
        cluster_radius_km,
        buffer_km,
        grouping_level,
        ov_calculation_method,
        defor_exposure_mode,
        regression_scale,
        ov_approach,
        defor_summing,
        defor_data_type,
        defor_approach,
        regression_model_family,
        defor_transform,
        model,
        group_value,
        n_obs,
        n_unique_regressor,
        slope,
        r_squared,
        adj_r_squared,
        correlation,
        source_file
      )

    readr::write_csv(
      master_fit_stats,
      file.path(output_dir, "master_fit_stats_report.csv")
    )

    message("Wrote master regression report table: ", file.path(output_dir, "master_fit_stats_report.csv"))
  }
}
