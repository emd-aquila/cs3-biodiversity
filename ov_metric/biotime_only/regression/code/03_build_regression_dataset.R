assert_file_exists(year_pair_defor_path, "BioTIME year-pair deforestation table")

year_pair_defor <- readr::read_csv(year_pair_defor_path, show_col_types = FALSE)
assert_has_cols(
  year_pair_defor,
  c(
    "time_series_id",
    "AEZ",
    "taxon_group",
    "year_t1",
    "year_t2",
    "year_gap",
    "ov_t1",
    "delta_ov",
    response_col,
    predictor_col,
    "has_complete_hansen_interval"
  ),
  "BioTIME year-pair deforestation table"
)

base_regression_data <- year_pair_defor |>
  dplyr::mutate(
    AEZ = as.factor(AEZ),
    taxon_group = as.factor(taxon_group),
    starting_ov = ov_t1,
    log_total_abundance_t1 = log1p(total_abundance_t1),
    observation_effort_t1 = n_observation_rows_t1
  ) |>
  dplyr::filter(
    has_complete_hansen_interval == TRUE,
    is.finite(.data[[response_col]]),
    is.finite(.data[[predictor_col]]),
    !is.na(AEZ),
    !is.na(taxon_group),
    year_gap > 0
  )

build_variant_dataset <- function(data, variant_key, variant_label, filter_positive_delta_ov) {
  variant_data <- data
  if (isTRUE(filter_positive_delta_ov)) {
    variant_data <- variant_data |>
      dplyr::filter(delta_ov <= 0)
  }

  retained_aez <- level_has_enough_rows(variant_data, "AEZ", min_rows_per_random_level)
  retained_taxa <- level_has_enough_rows(variant_data, "taxon_group", min_rows_per_random_level)

  variant_data <- variant_data |>
    dplyr::filter(AEZ %in% retained_aez, taxon_group %in% retained_taxa) |>
    droplevels()
  variant_data <- set_aez_order(variant_data)

  variant_dir <- file.path(output_dir, variant_key)
  dir.create(variant_dir, recursive = TRUE, showWarnings = FALSE)
  variant_dataset_path <- file.path(variant_dir, "biotime_regression_dataset.csv")
  write_csv_safe(variant_data, variant_dataset_path)
  write_csv_safe(variant_data, file.path(processed_data_dir, paste0("biotime_regression_dataset_", variant_key, ".csv")))

  message(
    variant_label,
    " regression dataset rows: ", nrow(variant_data),
    "; AEZ levels: ", dplyr::n_distinct(variant_data$AEZ),
    "; Taxon levels: ", dplyr::n_distinct(variant_data$taxon_group)
  )

  list(
    data = variant_data,
    summary = tibble::tibble(
      variant_key = variant_key,
      variant_label = variant_label,
      filter_positive_delta_ov = filter_positive_delta_ov,
      n_rows = nrow(variant_data),
      n_time_series = dplyr::n_distinct(variant_data$time_series_id),
      n_aez = dplyr::n_distinct(variant_data$AEZ),
      n_taxon_groups = dplyr::n_distinct(variant_data$taxon_group),
      positive_delta_ov_rows = sum(variant_data$delta_ov > 0, na.rm = TRUE),
      nonpositive_delta_ov_rows = sum(variant_data$delta_ov <= 0, na.rm = TRUE),
      dataset_path = variant_dataset_path
    )
  )
}

variant_results <- purrr::pmap(
  regression_variants,
  ~ build_variant_dataset(
    data = base_regression_data,
    variant_key = ..1,
    variant_label = ..2,
    filter_positive_delta_ov = ..3
  )
)

variant_summaries <- purrr::map_dfr(variant_results, "summary")
write_csv_safe(variant_summaries, variant_summary_path)

all_variant_index <- which(regression_variants$variant_key == "all_delta_ov")[1]
if (!is.na(all_variant_index)) {
  write_csv_safe(variant_results[[all_variant_index]]$data, regression_dataset_path)
  write_csv_safe(variant_results[[all_variant_index]]$data, file.path(processed_data_dir, "biotime_regression_dataset.csv"))
}
