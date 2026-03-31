# =====================================================
# Configuration for regression pipeline
# =====================================================

#TODO:
# we need to track these things: buffer, clustering method, clustering radius, regression type, 
# transformation of deltaDefor, 


# subdivision = c("aez", "aez_canopy")
# model_family = c("ols")



# ------------------------------
# Regression setting
# ------------------------------

# Minimum sample size required to run AEZ-specific regressions.
min_observations_per_aez_regression <- 5

# optional filter to only negative deltaOV values;
# keep FALSE for the default baseline analysis.
filter_neg_delta_ov <- TRUE

# Current clustering scope for regression - add/remove as desired
cluster_methods <- c("clara")                     # select from clara, pam, greedy_cover
cluster_radii <- c(10.0, 12.5, 17.5)   # select from 10.0, 12.5, 17.5, 20.0, 25.0
buffers <- c(1, 5, 10)                    # select from 1, 5, 10
defor_transform_vals = c("raw", "log1p")

# ------------------------------
# Regression run grid - to be looped over in 04_ols_regression
# ------------------------------

regression_run_grid <- crossing(
  cluster_method = cluster_methods,
  cluster_radius_km = cluster_radii,
  buffer_km = buffers,
  defor_transform = defor_transform_vals
) %>%
  mutate(
    cluster_stub = paste0(
      cluster_method,
      "_rad_",
      sprintf("%.1fkm", cluster_radius_km)
    ),
    buffer_stub = paste0("buf_", buffer_km, "km"),
    run_stub = paste0(buffer_stub, "__", cluster_stub, "__defor_", defor_transform)
  )

# ------------------------------
# Directories and file paths
# ------------------------------

# Assumes working directory is regression/code
repo_root <- file.path("..", "..")

regression_dir <- file.path(repo_root, "regression")
code_dir <- file.path(regression_dir, "code")
output_dir <- file.path(regression_dir, "output")
tmp_dir <- file.path(regression_dir, "tmp")

analysis_output_dir <- file.path(repo_root, "deforestation_tile_tag", "analysis", "output")
build_output_dir <- file.path(repo_root, "deforestation_tile_tag", "build", "output")

# Create high-level regression directories
for (dir_path in c(output_dir, tmp_dir)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

# ------------------------------
# Current-run placeholders
# These are assigned inside run_regressions.R
# ------------------------------

current_cluster_method <- NA_character_
current_cluster_radius_km <- NA_real_
current_buffer_km <- NA_real_
current_defor_transform <- NA_character_

current_cluster_stub <- NA_character_
current_buffer_stub <- NA_character_
current_run_stub <- NA_character_

cluster_deltas_path <- NULL
canonical_tabular_dir <- NULL
canonical_spatial_dir <- NULL

run_output_dir <- NULL
tables_dir <- NULL
models_dir <- NULL
charts_dir <- NULL
run_tmp_dir <- NULL

# ------------------------------
# Helper: build one cluster stub
# ------------------------------

cluster_stub_from_run <- function(cluster_method, cluster_radius_km) {
  paste0(
    cluster_method,
    "_rad_",
    sprintf("%.1fkm", cluster_radius_km)
  )
}

# ------------------------------
# Helper: build one buffer stub
# ------------------------------

buffer_stub_from_run <- function(buffer_km) {
  paste0("buf_", buffer_km, "km")
}

# ------------------------------
# Helper: set paths for the current regression run
# ------------------------------

set_regression_run_paths <- function(cluster_method, 
                                     cluster_radius_km, 
                                     buffer_km, 
                                     defor_transform) {
  
  current_cluster_method <<- cluster_method
  current_cluster_radius_km <<- cluster_radius_km
  current_buffer_km <<- buffer_km
  current_defor_transform <<- defor_transform
  
  current_cluster_stub <<- cluster_stub_from_run(cluster_method, cluster_radius_km)
  current_buffer_stub <<- buffer_stub_from_run(buffer_km)
  current_run_stub <<- paste0(current_buffer_stub, "__", current_cluster_stub, 
                              "__defor_", current_defor_transform)
  
  # Input from deforestation_tile_tag/analysis
  cluster_deltas_path <<- file.path(
    analysis_output_dir,
    current_buffer_stub,
    current_cluster_stub,
    "tables",
    "cluster_deltas.csv"
  )
  
  # Optional access to canonical build outputs for maps / exploration
  canonical_tabular_dir <<- file.path(
    build_output_dir,
    current_cluster_stub,
    "canonical_tabular"
  )
  
  canonical_spatial_dir <<- file.path(
    build_output_dir,
    current_cluster_stub,
    "canonical_spatial"
  )
  
  # Regression output directories for this run
  run_output_dir <<- file.path(output_dir, current_buffer_stub, current_cluster_stub,
                               paste0("defor_", current_defor_transform))
  tables_dir <<- file.path(run_output_dir, "tables")
  models_dir <<- file.path(run_output_dir, "models")
  charts_dir <<- file.path(run_output_dir, "charts")
  run_tmp_dir <<- file.path(run_output_dir, "tmp")
  
  for (dir_path in c(run_output_dir, tables_dir, models_dir, charts_dir, run_tmp_dir)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  invisible(
    list(
      cluster_deltas_path = cluster_deltas_path,
      canonical_tabular_dir = canonical_tabular_dir,
      canonical_spatial_dir = canonical_spatial_dir,
      run_output_dir = run_output_dir,
      tables_dir = tables_dir,
      models_dir = models_dir,
      charts_dir = charts_dir,
      run_tmp_dir = run_tmp_dir
    )
  )
}

# ------------------------------
# Required columns in cluster_deltas
# ------------------------------

cluster_deltas_required_cols <- c(
  "AEZ",
  "cluster_id",
  "buffer_km",
  "medoid_latitude",
  "medoid_longitude",
  "year_t1",
  "year_t2",
  "year_gap",
  "ov_t1",
  "ov_t2",
  "delta_ov",
  "delta_ov_annualized",
  "n_sites_t1",
  "n_sites_t2",
  "delta_defor_ha",
  "delta_defor_ha_annualized",
  "inverse_change"
)