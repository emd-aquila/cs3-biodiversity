code_dir <- getwd()
stage_dir <- normalizePath(file.path(code_dir, ".."), mustWork = TRUE)
workflow_dir <- normalizePath(file.path(stage_dir, ".."), mustWork = TRUE)

output_dir <- file.path(stage_dir, "output")
model_dir <- file.path(output_dir, "models")
table_dir <- file.path(output_dir, "tables")
prediction_dir <- file.path(output_dir, "predictions")
tmp_dir <- file.path(stage_dir, "tmp")
processed_data_dir <- file.path(workflow_dir, "data", "processed")

dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(prediction_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_data_dir, recursive = TRUE, showWarnings = FALSE)

year_pair_defor_path <- file.path(workflow_dir, "deforestation_matching", "output", "tabular", "biotime_year_pair_deforestation.csv")
regression_dataset_path <- file.path(output_dir, "biotime_regression_dataset.csv")
model_fit_stats_path <- file.path(table_dir, "model_fit_stats.csv")
model_parameters_path <- file.path(table_dir, "model_parameters.csv")
aez_gam_comparison_path <- file.path(table_dir, "aez_gam_linear_comparison.csv")
variant_summary_path <- file.path(table_dir, "regression_variant_summary.csv")

regression_variants <- data.frame(
  variant_key = c("all_delta_ov", "nonpositive_delta_ov"),
  variant_label = c("All delta OV", "Nonpositive delta OV"),
  filter_positive_delta_ov = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

response_col <- "delta_ov"
predictor_col <- "delta_defor_pct"
min_rows_per_random_level <- 3L
min_gam_rows <- 12L
min_gam_unique_x <- 5L
