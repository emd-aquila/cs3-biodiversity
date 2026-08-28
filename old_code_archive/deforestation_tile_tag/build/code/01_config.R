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

cluster_methods <- c("clara") # can choose clara, pam, greedy_cover
# cluster_methods <- c("clara", "pam", "greedy_cover")

# Tag completed clustering runs only. Add pending radii to the active vector
# after their cluster outputs have finished.
cluster_radius_km_vals <- c(2.5, 10)
pending_cluster_radius_km_vals <- c(5, 7.5)

cluster_run_grid <- crossing(
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

build_output_dir <- file.path(build_dir, "output")
dir.create(build_output_dir, recursive = TRUE, showWarnings = FALSE)

build_tmp_dir <- file.path(build_dir, "tmp")
dir.create(build_tmp_dir, recursive = TRUE, showWarnings = FALSE)

# External source data locations
hansen_spatial_data_dir <- file.path(
  "..",
  "..",
  "..",
  "spatial_data",
  "hansen_global_forest_change"
)
hansen_raster_dir <- file.path(hansen_spatial_data_dir, "rasters")
hansen_vrt_dir <- file.path(hansen_spatial_data_dir, "vrt")

dir.create(hansen_spatial_data_dir, recursive = TRUE, showWarnings = FALSE)
hansen_spatial_data_dir <- normalizePath(hansen_spatial_data_dir, mustWork = TRUE)

# Pipeline helper scripts and generated Hansen exchange files live inside
# deforestation_tile_tag, not spatial_data.
hansen_scripts_dir <- file.path(build_dir, "scripts")
dir.create(hansen_scripts_dir, recursive = TRUE, showWarnings = FALSE)
hansen_scripts_dir <- normalizePath(hansen_scripts_dir, mustWork = TRUE)

country_boundaries_dir <- file.path(
  "..",
  "..",
  "..",
  "spatial_data",
  "naturalearth_10m_admin_0_countries"
)
country_boundaries_file <- file.path(
  country_boundaries_dir,
  "ne_10m_admin_0_countries.shp"
)

cluster_dir <- file.path("..", "..", "..", "cluster", "output")

# -----------------------
# Cache files that are slow to rebuild because large
# -----------------------

defor_tiles_cache <- file.path(build_tmp_dir, "hansen_cluster_buffer_source_sf_v1.rds")

# -----------------------
# Helper for run-specific paths
# -----------------------

set_cluster_run_paths <- function(cluster_method, cluster_radius_km) {
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
    cluster_radius_dir,
    cluster_method,
    "model_df_clustered.csv"
  )
  
  run_output_dir <<- file.path(build_output_dir, cluster_radius_dir, cluster_method)
  canonical_tabular_dir <<- file.path(run_output_dir, "canonical_tabular")
  canonical_spatial_dir <<- file.path(run_output_dir, "canonical_spatial")
  hansen_exchange_dir <<- file.path(run_output_dir, "hansen_gee")
  
  dir.create(run_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(canonical_tabular_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(canonical_spatial_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(hansen_exchange_dir, recursive = TRUE, showWarnings = FALSE)
  
  cluster_footprints_cache <<- file.path(
    build_tmp_dir,
    paste0(cluster_stub, "_cluster_footprints_hull_buffer_hansen_sf_v1.rds")
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
