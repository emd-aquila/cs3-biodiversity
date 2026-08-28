# =====================================================
# 06_master_output_table.R
# Build one master regression report table from fit-stats outputs.
# Supports the conditional folder layout. Axes with one selected option may be
# omitted as folders and recovered from 01_config.R.
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
  warning("No fit_stats CSV files found under configured regression output dir. Skipping master output table.")
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

  format_numberish <- function(x) {
    dplyr::case_when(
      is.na(x) ~ NA_character_,
      abs(x - round(x)) < 1e-9 ~ as.character(as.integer(round(x))),
      TRUE ~ as.character(x)
    )
  }

  parse_fit_stats_manifest <- function(path_vector) {
    output_root <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
    cluster_database_lookup <- tibble::tibble(
      cluster_database = cluster_databases,
      cluster_database_dir = cluster_databases
    )
    cluster_method_lookup <- tibble::tibble(
      cluster_method = cluster_methods,
      cluster_method_dir = cluster_methods
    )
    cluster_radius_lookup <- tibble::tibble(
      cluster_radius_km = cluster_radii,
      cluster_radius_dir = paste0("radius_", sprintf("%.1fkm", cluster_radii))
    )
    buffer_lookup <- tibble::tibble(
      buffer_km = buffers,
      buffer_dir = paste0("buf_", buffers, "km")
    )
    defor_approach_lookup <- purrr::imap_dfr(
      defor_approach_specs,
      ~ tibble::tibble(
        defor_approach = .y,
        defor_tile_sum_dir = .x$tile_sum_dir
      )
    )
    grouping_lookup <- purrr::imap_dfr(
      regression_group_specs,
      ~ tibble::tibble(regression_group = .y, grouping_dir = .x$output_dir)
    )
    annualization_lookup <- purrr::imap_dfr(
      annualization_mode_specs,
      ~ tibble::tibble(annualization_mode = .y, annualization_dir = .x$output_dir)
    )
    exposure_lookup <- purrr::imap_dfr(
      defor_bin_specs,
      ~ tibble::tibble(defor_bin = .y, exposure_dir = .x$output_dir)
    )
    delta_ov_lookup <- purrr::imap_dfr(
      delta_ov_approach_specs,
      ~ tibble::tibble(delta_ov_approach = .y, delta_ov_approach_dir = .x$output_dir)
    )
    single_tile_collapse_lookup <- purrr::imap_dfr(
      single_tile_collapse_mode_specs,
      ~ tibble::tibble(single_tile_collapse_mode = .y, single_tile_collapse_dir = .x$output_dir)
    )
    ov_calculation_lookup <- tibble::tibble(
      ov_calculation_method = ov_calculation_methods,
      ov_calculation_dir = purrr::map_chr(ov_calculation_methods, ~ ov_calculation_specs[[.x]]$output_dir)
    )
    ov_change_lookup <- purrr::imap_dfr(
      ov_change_mode_specs,
      ~ tibble::tibble(ov_change_mode = .y, ov_change_dir = .x$output_dir)
    )
    starting_ov_adjustment_lookup <- purrr::imap_dfr(
      starting_ov_adjustment_specs,
      ~ tibble::tibble(starting_ov_adjustment_mode = .y, starting_ov_adjustment_dir = .x$output_dir)
    )
    defor_transform_lookup <- purrr::imap_dfr(
      defor_transform_specs,
      ~ tibble::tibble(defor_transform = .y, defor_transform_dir = .x$output_dir)
    )

    regression_model_lookup <- tibble::tibble(
      regression_model = regression_models,
      regression_model_dir = regression_models
    )

    resolve_axis <- function(parts,
                             lookup,
                             option_col,
                             dir_col,
                             selected_options) {
      matched <- lookup[lookup[[dir_col]] %in% parts, , drop = FALSE]

      if (nrow(matched) > 0) {
        return(matched[[option_col]][[1]])
      }

      if (length(selected_options) == 1) {
        return(selected_options[[1]])
      }

      NA
    }

    resolve_required_axis <- function(parts,
                                      lookup,
                                      option_col,
                                      dir_col) {
      matched <- lookup[lookup[[dir_col]] %in% parts, , drop = FALSE]

      if (nrow(matched) > 0) {
        return(matched[[option_col]][[1]])
      }

      NA
    }

    tibble::tibble(path = path_vector) %>%
      mutate(
        path_normalized = normalizePath(path, winslash = "/", mustWork = FALSE),
        rel_path = stringr::str_remove(
          path_normalized,
          paste0("^", output_root, "/")
        ),
        path_parts = stringr::str_split(rel_path, "/"),
        cluster_database = purrr::map_chr(
          path_parts,
          ~ resolve_axis(.x, cluster_database_lookup, "cluster_database", "cluster_database_dir", cluster_databases)
        ),
        cluster_method = purrr::map_chr(
          path_parts,
          ~ resolve_axis(.x, cluster_method_lookup, "cluster_method", "cluster_method_dir", cluster_methods)
        ),
        cluster_radius_km = purrr::map_dbl(
          path_parts,
          ~ resolve_axis(.x, cluster_radius_lookup, "cluster_radius_km", "cluster_radius_dir", cluster_radii)
        ),
        buffer_km = purrr::map_dbl(
          path_parts,
          ~ resolve_axis(.x, buffer_lookup, "buffer_km", "buffer_dir", buffers)
        ),
        regression_group = purrr::map_chr(
          path_parts,
          ~ resolve_axis(.x, grouping_lookup, "regression_group", "grouping_dir", regression_groups)
        ),
        annualization_mode = purrr::map_chr(
          path_parts,
          ~ resolve_axis(.x, annualization_lookup, "annualization_mode", "annualization_dir", annualization_modes)
        ),
        defor_bin = purrr::map_chr(
          path_parts,
          ~ resolve_axis(.x, exposure_lookup, "defor_bin", "exposure_dir", defor_bins)
        ),
        delta_ov_approach = purrr::map_chr(
          path_parts,
          ~ resolve_axis(.x, delta_ov_lookup, "delta_ov_approach", "delta_ov_approach_dir", delta_ov_approaches)
        ),
        single_tile_collapse_mode = purrr::map_chr(
          path_parts,
          ~ resolve_axis(
            .x,
            single_tile_collapse_lookup,
            "single_tile_collapse_mode",
            "single_tile_collapse_dir",
            single_tile_collapse_modes
          )
        ),
        ov_calculation_method = purrr::map_chr(
          path_parts,
          ~ resolve_axis(
            .x,
            ov_calculation_lookup,
            "ov_calculation_method",
            "ov_calculation_dir",
            ov_calculation_methods
          )
        ),
        ov_change_mode = purrr::map_chr(
          path_parts,
          ~ resolve_axis(.x, ov_change_lookup, "ov_change_mode", "ov_change_dir", ov_change_modes)
        ),
        starting_ov_adjustment_mode = purrr::map_chr(
          path_parts,
          ~ resolve_required_axis(
            .x,
            starting_ov_adjustment_lookup,
            "starting_ov_adjustment_mode",
            "starting_ov_adjustment_dir"
          )
        ),
        defor_approach = purrr::map_chr(
          path_parts,
          ~ resolve_axis(.x, defor_approach_lookup, "defor_approach", "defor_tile_sum_dir", defor_tile_sum_methods)
        ),
        regression_model = purrr::map_chr(
          path_parts,
          ~ resolve_axis(.x, regression_model_lookup, "regression_model", "regression_model_dir", regression_models)
        ),
        defor_transform = purrr::map_chr(
          path_parts,
          ~ resolve_axis(.x, defor_transform_lookup, "defor_transform", "defor_transform_dir", defor_transforms)
        ),
        filename = basename(path_normalized),
        sample_tag = regression_model
      ) %>%
      mutate(
        ov_calculation_order = match(ov_calculation_method, ov_calculation_methods),
        annualization_mode_order = match(annualization_mode, annualization_modes),
        delta_ov_approach_order = match(delta_ov_approach, delta_ov_approaches),
        single_tile_collapse_mode_order = match(single_tile_collapse_mode, single_tile_collapse_modes),
        ov_change_mode_order = match(ov_change_mode, ov_change_modes),
        starting_ov_adjustment_mode_order = match(starting_ov_adjustment_mode, starting_ov_adjustment_modes),
        defor_tile_sum = defor_approach,
        defor_tile_sum_order = match(defor_tile_sum, defor_tile_sum_methods),
        regression_model_order = match(regression_model, regression_models),
        defor_transform_order = match(defor_transform, defor_transforms),
        sample_order = regression_model_order,
        regression_group_order = match(regression_group, regression_groups),
        defor_bin_order = match(defor_bin, defor_bins)
      ) %>%
      filter(
        cluster_database %in% cluster_databases,
        cluster_method %in% cluster_methods,
        cluster_radius_km %in% cluster_radii,
        buffer_km %in% buffers,
        regression_group %in% regression_groups,
        ov_calculation_method %in% ov_calculation_methods,
        annualization_mode %in% annualization_modes,
        defor_bin %in% defor_bins,
        delta_ov_approach %in% delta_ov_approaches,
        single_tile_collapse_mode %in% single_tile_collapse_modes,
        ov_change_mode %in% ov_change_modes,
        starting_ov_adjustment_mode %in% starting_ov_adjustment_modes,
        defor_tile_sum %in% defor_tile_sum_methods,
        regression_model %in% regression_models,
        defor_transform %in% defor_transforms
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
          "cluster_database",
          "cluster_method",
          "cluster_radius_km",
          "buffer_km",
          "regression_group",
          "ov_calculation_method",
          "defor_bin",
          "annualization_mode",
          "delta_ov_approach",
          "single_tile_collapse_mode",
          "ov_change_mode",
          "starting_ov_adjustment_mode",
          "defor_tile_sum",
          "defor_approach",
          "regression_model",
          "defor_transform"
        )) {
          if (!col_name %in% names(fit_stats_df)) {
            fit_stats_df[[col_name]] <- NA
          }
        }

        for (numeric_col_name in c("starting_ov_slope", "defor_starting_ov_interaction")) {
          if (!numeric_col_name %in% names(fit_stats_df)) {
            fit_stats_df[[numeric_col_name]] <- NA_real_
          }
        }

        fit_stats_df %>%
          mutate(
            cluster_database = dplyr::coalesce(cluster_database, manifest_row$cluster_database),
            cluster_method = dplyr::coalesce(cluster_method, manifest_row$cluster_method),
            cluster_radius_km = dplyr::coalesce(cluster_radius_km, manifest_row$cluster_radius_km),
            buffer_km = dplyr::coalesce(buffer_km, manifest_row$buffer_km),
            regression_group = dplyr::coalesce(regression_group, manifest_row$regression_group),
            ov_calculation_method = dplyr::coalesce(ov_calculation_method, manifest_row$ov_calculation_method),
            defor_bin = dplyr::coalesce(defor_bin, manifest_row$defor_bin),
            annualization_mode = dplyr::coalesce(annualization_mode, manifest_row$annualization_mode),
            delta_ov_approach = dplyr::coalesce(delta_ov_approach, manifest_row$delta_ov_approach),
            single_tile_collapse_mode = dplyr::coalesce(single_tile_collapse_mode, manifest_row$single_tile_collapse_mode),
            ov_change_mode = dplyr::coalesce(ov_change_mode, manifest_row$ov_change_mode),
            starting_ov_adjustment_mode = dplyr::coalesce(starting_ov_adjustment_mode, manifest_row$starting_ov_adjustment_mode),
            defor_tile_sum = dplyr::coalesce(defor_tile_sum, manifest_row$defor_tile_sum),
            defor_approach = dplyr::coalesce(defor_approach, manifest_row$defor_approach),
            regression_model = dplyr::coalesce(regression_model, manifest_row$regression_model),
            defor_transform = dplyr::coalesce(defor_transform, manifest_row$defor_transform),
            sample_tag = manifest_row$sample_tag,
            source_file = manifest_row$rel_path,
            regression_group_order = manifest_row$regression_group_order,
            ov_calculation_order = manifest_row$ov_calculation_order,
            defor_bin_order = manifest_row$defor_bin_order,
            annualization_mode_order = manifest_row$annualization_mode_order,
            delta_ov_approach_order = manifest_row$delta_ov_approach_order,
            single_tile_collapse_mode_order = manifest_row$single_tile_collapse_mode_order,
            ov_change_mode_order = manifest_row$ov_change_mode_order,
            starting_ov_adjustment_mode_order = manifest_row$starting_ov_adjustment_mode_order,
            defor_tile_sum_order = manifest_row$defor_tile_sum_order,
            regression_model_order = manifest_row$regression_model_order,
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
        regression_group == "by_aez",
          readr::parse_number(group_value[regression_group == "by_aez"])
        ),
        cluster_radius_km = format_numberish(cluster_radius_km),
        buffer_km = format_numberish(buffer_km),
        n_obs = format_integerish(n_obs),
        n_unique_regressor = format_integerish(n_unique_regressor),
        slope = format_readable_stat(slope),
        starting_ov_slope = format_readable_stat(starting_ov_slope),
        defor_starting_ov_interaction = format_readable_stat(defor_starting_ov_interaction),
        r_squared = format_readable_stat(r_squared),
        adj_r_squared = format_readable_stat(adj_r_squared),
        correlation = format_readable_stat(correlation)
      ) %>%
      arrange(
        cluster_database,
        cluster_method,
        cluster_radius_km,
        buffer_km,
        regression_group_order,
        annualization_mode_order,
        defor_bin_order,
        delta_ov_approach_order,
        single_tile_collapse_mode_order,
        ov_calculation_order,
        ov_change_mode_order,
        starting_ov_adjustment_mode_order,
        defor_tile_sum_order,
        regression_model_order,
        defor_transform_order,
        sample_order,
        group_value_num,
        group_value,
        model
      ) %>%
      dplyr::select(
        cluster_database,
        cluster_method,
        cluster_radius_km,
        buffer_km,
        regression_group,
        annualization_mode,
        defor_bin,
        delta_ov_approach,
        single_tile_collapse_mode,
        ov_calculation_method,
        ov_change_mode,
        starting_ov_adjustment_mode,
        defor_tile_sum,
        defor_approach,
        regression_model,
        defor_transform,
        model,
        group_value,
        n_obs,
        n_unique_regressor,
        slope,
        r_squared,
        adj_r_squared,
        correlation,
        starting_ov_slope,
        defor_starting_ov_interaction,
        source_file
      )

    readr::write_csv(
      master_fit_stats,
      file.path(output_dir, "master_fit_stats_report.csv")
    )

    message("Wrote master regression report table: ", file.path(output_dir, "master_fit_stats_report.csv"))
  }
}
