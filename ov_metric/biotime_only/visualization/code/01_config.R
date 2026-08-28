code_dir <- getwd()
stage_dir <- normalizePath(file.path(code_dir, ".."), mustWork = TRUE)
workflow_dir <- normalizePath(file.path(stage_dir, ".."), mustWork = TRUE)

output_dir <- file.path(stage_dir, "output")
figure_dir <- file.path(output_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

regression_output_dir <- file.path(workflow_dir, "regression", "output")
clean_sample_year_path <- file.path(workflow_dir, "data_cleaning", "output", "biotime_clean_sample_year.csv")
regression_variants <- data.frame(
  variant_key = c("all_delta_ov", "nonpositive_delta_ov"),
  variant_label = c("All delta OV", "Nonpositive delta OV"),
  stringsAsFactors = FALSE
)

response_col <- "delta_ov"
predictor_col <- "delta_defor_pct"
x_label <- "Deforestation within 1 km buffer during BioTIME interval (% land area)"
y_label <- "Delta composite OV during BioTIME interval"
