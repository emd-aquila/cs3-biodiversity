# =====================================================
# Configuration of paths and analysis settings
# =====================================================

# -----------------------
# Analysis settings
# -----------------------

# Equal-area CRS used for buffering and area calculations
analysis_crs <- 6933
buffer_km_vals <- c(0, 0.5, 1, 2.5, 5)

hansen_years <- 2001:2025
hansen_version <- "GFC-2025-v1.13"
hansen_asset_id <- "UMD/hansen/global_forest_change_2025_v1_13"
hansen_base_url <- paste0(
  "https://storage.googleapis.com/earthenginepartners-hansen/",
  hansen_version
)
hansen_layers <- c("lossyear", "datamask", "treecover2000")

ov_score_cols <- c(
  "ov_score",
  "ov_obs_only",
  "ov_no_HQI",
  "ov_no_HANPP",
  "ov_no_MSA",
  "ov_no_PD",
  "ov_no_Shannon"
)

# -----------------------
# Cluster input selection grid
# -----------------------

cluster_databases <- c("predicts", "biotime", "combined")
cluster_methods <- c("greedy_cover") # can choose clara, pam, greedy_cover
cluster_radius_km_vals <- c(2.5, 5, 7.5, 10)

cluster_run_grid <- crossing(
  cluster_database = cluster_databases,
  cluster_method = cluster_methods,
  cluster_radius_km = cluster_radius_km_vals
) %>% 
  mutate(
    cluster_radius = sprintf("%.1fkm", cluster_radius_km),
    cluster_radius_dir = paste0("radius_", cluster_radius),
    cluster_stub = paste0(cluster_method, "_rad_", cluster_radius),
    cluster_file = "model_df_clustered.csv"
  )

# -----------------------
# Directory structure and file locations
# -----------------------

# Base directories relative to build/code/
build_dir <- ".."
ov_metric_dir <- normalizePath(file.path(build_dir, ".."), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(ov_metric_dir, ".."), winslash = "/", mustWork = TRUE)

build_output_dir <- file.path(build_dir, "output")
dir.create(build_output_dir, recursive = TRUE, showWarnings = FALSE)

build_tmp_dir <- file.path(build_dir, "tmp")
dir.create(build_tmp_dir, recursive = TRUE, showWarnings = FALSE)

# External source data locations
hansen_spatial_data_dir <- file.path(repo_root, "00_spatial_data", "hansen_global_forest_change")
hansen_raster_dir <- file.path(hansen_spatial_data_dir, "rasters")
hansen_vrt_dir <- file.path(hansen_spatial_data_dir, "vrt")

dir.create(hansen_spatial_data_dir, recursive = TRUE, showWarnings = FALSE)
hansen_spatial_data_dir <- normalizePath(hansen_spatial_data_dir, mustWork = TRUE)

# Pipeline helper scripts and generated Hansen exchange files live inside
# 04_deforestation_tile_tag, not 00_spatial_data.
hansen_scripts_dir <- file.path(build_dir, "code")
hansen_scripts_dir <- normalizePath(hansen_scripts_dir, mustWork = TRUE)

country_boundaries_dir <- file.path(repo_root, "00_spatial_data", "naturalearth_10m_admin_0_countries")
country_boundaries_file <- file.path(
  country_boundaries_dir,
  "ne_10m_admin_0_countries.shp"
)

cluster_dir <- file.path(ov_metric_dir, "03_cluster", "output")

# -----------------------
# Cache files that are slow to rebuild because large
# -----------------------

defor_tiles_cache <- file.path(build_tmp_dir, "hansen_cluster_buffer_source_sf_v1.rds")

# -----------------------
# Helper for run-specific paths
# -----------------------

set_cluster_run_paths <- function(cluster_database, cluster_method, cluster_radius_km) {
  cluster_radius_dir <- paste0(
    "radius_",
    sprintf("%.1fkm", cluster_radius_km)
  )
  cluster_stub <- paste0(
    cluster_method,
    "_rad_",
    sprintf("%.1fkm", cluster_radius_km)
  )
  
  cluster_file_path <<- file.path(
    cluster_dir,
    cluster_database,
    cluster_radius_dir,
    cluster_method,
    "model_df_clustered.csv"
  )
  
  run_output_dir <<- file.path(build_output_dir, cluster_database, cluster_method, cluster_radius_dir)
  canonical_tabular_dir <<- file.path(run_output_dir, "canonical_tabular")
  canonical_spatial_dir <<- file.path(run_output_dir, "canonical_spatial")
  hansen_exchange_dir <<- file.path(run_output_dir, "hansen_gee")
  
  dir.create(run_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(canonical_tabular_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(canonical_spatial_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(hansen_exchange_dir, recursive = TRUE, showWarnings = FALSE)
  hansen_exchange_dir <<- normalizePath(hansen_exchange_dir, winslash = "/", mustWork = TRUE)
  
  cluster_footprints_cache <<- file.path(
    build_tmp_dir,
    paste0(cluster_database, "_", cluster_stub, "_cluster_footprints_hull_buffer_hansen_sf_v1.rds")
  )

  hansen_cluster_buffer_geojson <<- file.path(
    hansen_exchange_dir,
    "cluster_buffers.geojson"
  )

  hansen_cluster_year_defor_file <<- file.path(
    hansen_exchange_dir,
    "hansen_cluster_year_defor.csv"
  )

  hansen_tile_manifest_file <<- file.path(
    hansen_exchange_dir,
    "hansen_tile_manifest.csv"
  )
}
