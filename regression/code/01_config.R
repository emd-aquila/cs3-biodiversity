# =====================================================
# Configuration for regression pipeline
# =====================================================

# ------------------------------
# Regression settings
# ------------------------------

# Minimum sample size required to run AEZ-specific regressions.
min_observations_per_aez_regression <- 3

# Filter: TRUE to only keep negative deltaOV, FALSE to include positive
filter_neg_delta_ov <- TRUE

# Winsorization cap settings: currently p90
winsorization_threshold <- 0.90

# Output families for regression variants
defor_transforms <- c("raw", "rlm", "log1p", "p90", "winsorized")
regression_scales <- c("non_annualized", "annualized")
ov_approaches <- c("ov_year_pair", "ov_whole_cluster")
defor_approaches <- c(
  "defor_tile_total_ha_total",
  "defor_tile_total_ha_crop",
  "defor_tile_avg_ha_total",
  "defor_tile_avg_ha_crop",
  "defor_tile_rel_pct_ha_total",
  "defor_tile_rel_pct_ha_crop"
)

defor_approach_specs <- list(
  defor_tile_total_ha_total = list(
    source_col = "delta_defor_ha_total_raw",
    label = "total raw tile deforestation (ha_total)"
  ),
  defor_tile_total_ha_crop = list(
    source_col = "delta_defor_ha_crops_raw",
    label = "total raw tile deforestation (ha_crop)"
  ),
  defor_tile_avg_ha_total = list(
    source_col = "delta_defor_ha_total_avg",
    label = "average tile deforestation (ha_total)"
  ),
  defor_tile_avg_ha_crop = list(
    source_col = "delta_defor_ha_crops_avg",
    label = "average tile deforestation (ha_crop)"
  ),
  defor_tile_rel_pct_ha_total = list(
    source_col = "delta_defor_ha_total_rel_pct",
    label = "overlap-weighted tile deforestation (ha_total)"
  ),
  defor_tile_rel_pct_ha_crop = list(
    source_col = "delta_defor_ha_crops_rel_pct",
    label = "overlap-weighted tile deforestation (ha_crop)"
  )
)

# Current scope of regression - add as desired
cluster_methods <- c("clara")           # select from clara, pam, greedy_cover
cluster_radii <- c(10.0)                # select from 10.0, 12.5, 17.5, 20.0, 25.0
buffers <- c(1, 10)                     # select from 1, 5, 10

# ------------------------------
# Required columns in cluster_deltas and for regressions
# ------------------------------

required_cluster_deltas_cols <- c(
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
  "n_defor_years",
  "delta_defor_ha_total_raw",
  "delta_defor_ha_total_avg",
  "delta_defor_ha_total_rel_pct",
  "delta_defor_ha_crops_raw",
  "delta_defor_ha_crops_avg",
  "delta_defor_ha_crops_rel_pct",
  "delta_defor_ha",
  "delta_defor_ha_annualized",
  "inverse_change"
)

required_regression_cols <- c(
  "AEZ",
  "delta_ov",
  "delta_defor_ha",
  "log1p_delta_defor_ha"
)

# ------------------------------
# Regression run grid - to be looped over in 04_ols_regression
# contains all the methods, radii, buffers specified above
# ------------------------------

regression_run_grid <- crossing(
  cluster_method = cluster_methods,
  cluster_radius_km = cluster_radii,
  buffer_km = buffers
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
build_output_dir <- file.path(repo_root, "deforestation_tile_tag", "build", "output")

# Create output and tmp directories
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

current_cluster_stub <- NA_character_
current_buffer_stub <- NA_character_
current_run_label <- NA_character_
current_regression_scale <- NA_character_
current_ov_approach <- NA_character_
current_defor_approach <- NA_character_
current_defor_source_col <- NA_character_
current_defor_transform <- NA_character_

cluster_deltas_path <- NULL
canonical_tabular_dir <- NULL
canonical_spatial_dir <- NULL
analysis_output_dir_current <- NULL

regression_scale_output_dir <- NULL
ov_output_dir <- NULL
defor_approach_output_dir <- NULL
output_dirs_by_transform <- NULL
