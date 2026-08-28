code_dir <- getwd()
stage_dir <- normalizePath(file.path(code_dir, ".."), mustWork = TRUE)
workflow_dir <- normalizePath(file.path(stage_dir, ".."), mustWork = TRUE)
ov_metric_dir <- normalizePath(file.path(workflow_dir, ".."), mustWork = TRUE)
project_dir <- normalizePath(file.path(ov_metric_dir, ".."), mustWork = TRUE)

local_lib <- file.path(workflow_dir, ".Rlib")
if (dir.exists(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

output_dir <- file.path(stage_dir, "output")
spatial_output_dir <- file.path(output_dir, "spatial")
tabular_output_dir <- file.path(output_dir, "tabular")
hansen_gee_dir <- file.path(output_dir, "hansen_gee")
tmp_dir <- file.path(stage_dir, "tmp")
processed_data_dir <- file.path(workflow_dir, "data", "processed")

dir.create(spatial_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tabular_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(hansen_gee_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_data_dir, recursive = TRUE, showWarnings = FALSE)

clean_sample_year_path <- file.path(workflow_dir, "data_cleaning", "output", "biotime_clean_sample_year.csv")
clean_timeseries_path <- file.path(workflow_dir, "data_cleaning", "output", "biotime_timeseries.csv")

analysis_crs <- 6933
buffer_km <- 1
hansen_years <- 2001:2025
hansen_asset_id <- "UMD/hansen/global_forest_change_2025_v1_13"

site_buffer_gpkg_path <- file.path(spatial_output_dir, "biotime_timeseries_1km_buffers.gpkg")
site_buffer_geojson_path <- file.path(hansen_gee_dir, "biotime_timeseries_1km_buffers.geojson")
hansen_site_year_defor_path <- file.path(hansen_gee_dir, "hansen_biotime_site_year_defor.csv")

matched_tiles_path <- file.path(tabular_output_dir, "matched_timeseries_hansen_tiles.csv")
site_year_defor_output_path <- file.path(tabular_output_dir, "biotime_hansen_site_year_defor.csv")
year_pair_defor_path <- file.path(tabular_output_dir, "biotime_year_pair_deforestation.csv")
defor_summary_path <- file.path(tabular_output_dir, "biotime_deforestation_matching_summary.csv")

export_script_path <- file.path(code_dir, "04_export_hansen_biotime_stats.py")
earthengine_project <- Sys.getenv("EARTHENGINE_PROJECT", unset = "")
run_hansen_export <- tolower(Sys.getenv("BIOTIME_ONLY_RUN_HANSEN_EXPORT", unset = "true")) %in% c("1", "true", "yes")
force_hansen_export <- tolower(Sys.getenv("BIOTIME_ONLY_FORCE_HANSEN_EXPORT", unset = "false")) %in% c("1", "true", "yes")
hansen_export_chunk_size <- as.integer(Sys.getenv("BIOTIME_ONLY_HANSEN_CHUNK_SIZE", unset = "100"))
