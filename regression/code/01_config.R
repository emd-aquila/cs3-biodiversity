# =====================================================
# Configuration for regression pipeline
# =====================================================

# Current scope of regression - add as desired

# ------------------------------
# Regression settings
# ------------------------------

# "clara", "pam", "greedy_cover"
cluster_methods <- c("clara")

# 10.0, 12.5, 17.5, 20.0, 25.0
cluster_radii <- c(10.0)

# 1, 5, 10
buffers <- c(1, 10)

# Minimum sample size required to run group-specific regressions.
min_observations_per_group_regression <- 3

# Filter: TRUE to only keep negative deltaOV, FALSE to include positive
filter_neg_delta_ov <- TRUE

# Output families for regression variants.
regression_model_families <- c("ols", "robust_linear", "polynomial", "gam_spline")

# "raw", "log1p", "p90"
defor_transforms <- c("raw", "log1p", "p90")

# "non-annualized", "annualized"
regression_scales <- c("non_annualized")

# "ov_year_pair", "ov_whole_cluster"
ov_approaches <- c("ov_year_pair", "ov_whole_cluster")

# "ov_full", "ov_obs_only", "ov_no_hqi", "ov_no_hanpp", "ov_no_msa", "ov_no_pd", "ov_no_shannon"
ov_calculation_methods <- c(
  "ov_full",
  "ov_obs_only",
  "ov_no_hqi",
  "ov_no_hanpp"
)

# "aez", "climate_zone"
regression_grouping_levels <- c("aez")

# "baseline", "lagged"
defor_exposure_modes <- c("baseline")

# "defor_tile_total_ha_total", "defor_tile_avg_ha_total", "defor_tile_rel_pct_ha_total"
# "defor_tile_total_ha_crop", "defor_tile_avg_ha_crop", "defor_tile_rel_pct_ha_crop"
defor_approaches <- c(
  "defor_tile_total_ha_total",
  "defor_tile_avg_ha_total",
  "defor_tile_rel_pct_ha_total"
)

# ------------------------------
# Directory and label name specification
# ------------------------------

regression_grouping_specs <- list(
  aez = list(
    group_col = "AEZ",
    output_dir = "aez",
    label = "AEZ"
  ),
  climate_zone = list(
    group_col = "climate_zone",
    output_dir = "climate_zone",
    label = "climate zone"
  )
)

defor_exposure_mode_specs <- list(
  baseline = list(
    output_dir = "baseline_deforestation",
    label = "baseline"
  ),
  lagged = list(
    output_dir = "lagged_deforestation",
    label = "lagged"
  )
)

ov_calculation_specs <- list(
  ov_full = list(
    delta_col = "delta_ov",
    annualized_col = "delta_ov_annualized",
    output_dir = "ov_full",
    label = "full OV"
  ),
  ov_obs_only = list(
    delta_col = "delta_ov_obs_only",
    annualized_col = "delta_ov_obs_only_annualized",
    output_dir = "ov_obs_only",
    label = "observed biodiversity only"
  ),
  ov_no_hqi = list(
    delta_col = "delta_ov_no_HQI",
    annualized_col = "delta_ov_no_hqi_annualized",
    output_dir = "ov_no_hqi",
    label = "OV without HQI"
  ),
  ov_no_hanpp = list(
    delta_col = "delta_ov_no_hanpp",
    annualized_col = "delta_ov_no_hanpp_annualized",
    output_dir = "ov_no_hanpp",
    label = "OV without HANPP"
  ),
  ov_no_msa = list(
    delta_col = "delta_ov_no_msa",
    annualized_col = "delta_ov_no_msa_annualized",
    output_dir = "ov_no_msa",
    label = "OV without MSA"
  ),
  ov_no_pd = list(
    delta_col = "delta_ov_no_pd",
    annualized_col = "delta_ov_no_pds_annualized",
    output_dir = "ov_no_pd",
    label = "OV without PD"
  ),
  ov_no_shannon = list(
    delta_col = "delta_ov_no_shannon",
    annualized_col = "delta_ov_no_shannon_annualized",
    output_dir = "ov_no_shannon",
    label = "OV without Shannon"
  )
)

# ------------------------------
# Table writing toggle settings
# ------------------------------
write_model_tables <- FALSE
write_diagnostic_plots <- TRUE
build_master_output_report <- TRUE
build_comparison_canvas <- FALSE


defor_approach_specs <- list(
  defor_tile_total_ha_total = list(
    source_cols = list(
      baseline = "delta_defor_ha_total_raw",
      lagged = "delta_defor_ha_total_raw_lagged"
    ),
    summing_dir = "tile_total",
    data_dir = "ha_total",
    label = "total raw tile deforestation (ha_total)"
  ),
  defor_tile_total_ha_crop = list(
    source_cols = list(
      baseline = "delta_defor_ha_crops_raw",
      lagged = "delta_defor_ha_crops_raw_lagged"
    ),
    summing_dir = "tile_total",
    data_dir = "ha_crop",
    label = "total raw tile deforestation (ha_crop)"
  ),
  defor_tile_avg_ha_total = list(
    source_cols = list(
      baseline = "delta_defor_ha_total_avg",
      lagged = "delta_defor_ha_total_avg_lagged"
    ),
    summing_dir = "tile_avg",
    data_dir = "ha_total",
    label = "average tile deforestation (ha_total)"
  ),
  defor_tile_avg_ha_crop = list(
    source_cols = list(
      baseline = "delta_defor_ha_crops_avg",
      lagged = "delta_defor_ha_crops_avg_lagged"
    ),
    summing_dir = "tile_avg",
    data_dir = "ha_crop",
    label = "average tile deforestation (ha_crop)"
  ),
  defor_tile_rel_pct_ha_total = list(
    source_cols = list(
      baseline = "delta_defor_ha_total_rel_pct",
      lagged = "delta_defor_ha_total_rel_pct_lagged"
    ),
    summing_dir = "tile_rel_pct",
    data_dir = "ha_total",
    label = "overlap-weighted tile deforestation (ha_total)"
  ),
  defor_tile_rel_pct_ha_crop = list(
    source_cols = list(
      baseline = "delta_defor_ha_crops_rel_pct",
      lagged = "delta_defor_ha_crops_rel_pct_lagged"
    ),
    summing_dir = "tile_rel_pct",
    data_dir = "ha_crop",
    label = "overlap-weighted tile deforestation (ha_crop)"
  )
)

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
  "inverse_change"
)

required_regression_cols <- c(
  "group_value",
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
current_ov_calculation_method <- NA_character_
current_ov_calculation_label <- NA_character_
current_delta_ov_source_col <- NA_character_
current_delta_ov_annualized_source_col <- NA_character_
current_regression_model_family <- NA_character_
current_grouping_level <- NA_character_
current_group_col <- NA_character_
current_group_label <- NA_character_
current_defor_approach <- NA_character_
current_defor_exposure_mode <- NA_character_
current_defor_summing <- NA_character_
current_defor_data_type <- NA_character_
current_defor_source_col <- NA_character_
current_defor_transform <- NA_character_

cluster_deltas_path <- NULL
canonical_tabular_dir <- NULL
canonical_spatial_dir <- NULL
analysis_output_dir_current <- NULL

regression_scale_output_dir <- NULL
ov_calculation_output_dir <- NULL
ov_output_dir <- NULL
defor_approach_output_dir <- NULL
model_family_output_dir <- NULL
output_dirs_by_transform <- NULL
