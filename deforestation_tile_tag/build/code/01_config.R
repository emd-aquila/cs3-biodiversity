# =====================================================
# Configuration of paths and analysis settings
# =====================================================

# -----------------------
# Analysis settings
# -----------------------

# Equal-area CRS used for buffering and area calculations
analysis_crs <- 6933
buffer_km_vals <- c(1, 5, 10)

# -----------------------
# Cluster input selection grid
# -----------------------

cluster_methods <- c("clara", "pam", "greedy_cover")
cluster_radius_km_vals <- c(10.0, 12.5, 17.5, 20.0, 25.0)

cluster_run_grid <- crossing(
  cluster_method = cluster_methods,
  cluster_radius_km = cluster_radius_km_vals
) %>% 
  mutate(
    cluster_radius = sprintf("%.1fkm", cluster_radius_km),
    cluster_stub = paste0(cluster_method, "_rad_", cluster_radius),
    cluster_file = paste0(cluster_stub, "_model_df_clustered.csv")
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

# External data locations and files
defor_spatial_dir <- file.path("..", "..", "..", "spatial_data", "deforestation", "spatial")
defor_spatial_file <- file.path(defor_spatial_dir,"deforestation_50km.shp")

defor_tabular_dir <- file.path("..", "..", "..", "spatial_data", "deforestation", "tabular")
defor_tabular_file <- file.path(defor_tabular_dir, "deforestation_tile_total_ha_SPAM2020_2001_2024.csv")

cluster_dir <- file.path("..", "..", "..", "cluster", "output")

# -----------------------
# Cache files that are slow to rebuild because large
# -----------------------

defor_tiles_cache <- file.path(build_tmp_dir, "defor_tiles_sf.rds")

# -----------------------
# Helper for run-specific paths
# -----------------------

set_cluster_run_paths <- function(cluster_method, cluster_radius_km) {
  cluster_stub <- paste0(
    cluster_method,
    "_rad_",
    sprintf("%.1fkm", cluster_radius_km)
  )
  
  cluster_file_path <<- file.path(
    cluster_dir,
    paste0(cluster_stub, "_model_df_clustered.csv")
  )
  
  run_output_dir <<- file.path(build_output_dir, cluster_stub)
  canonical_tabular_dir <<- file.path(run_output_dir, "canonical_tabular")
  canonical_spatial_dir <<- file.path(run_output_dir, "canonical_spatial")
  
  dir.create(run_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(canonical_tabular_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(canonical_spatial_dir, recursive = TRUE, showWarnings = FALSE)
  
  cluster_footprints_cache <<- file.path(
    build_tmp_dir,
    paste0(cluster_stub, "_cluster_footprints_sf.rds")
  )
}