# =====================================================
# Configuration for regression pipeline
# =====================================================

# ------------------------------
# Regression settings
# ------------------------------

# Output hierarchy axes:
# cluster_methods / cluster_radii / buffers / regression_groups /
# annualization_modes / defor_bins / delta_ov_approaches /
# single_tile_collapse_modes / ov_calculation_methods / ov_change_modes /
# starting_ov_adjustment_modes / defor_tile_sum_methods /
# regression_models / defor_transforms
#
# Only axes with more than one selected option are created as output folders,
# except starting_ov_adjustment_modes, which is always kept as a folder so
# baseline-adjusted outputs cannot mix with unadjusted outputs.
# Other axes with one selected option are recorded in the run README instead.


# "predicts", "biotime", "combined"
cluster_databases <- c("predicts", "biotime", "combined")

# "clara", "pam", "greedy_cover"
cluster_methods <- c("greedy_cover")

cluster_radii <- c(2.5, 5, 7.5, 10)

# Hansen 30 m raster buffers in km
buffers <- c(0, 0.5, 1, 2.5, 5)

# "by_aez", "by_climate_zone", "by_country"
regression_groups <- c("by_aez")

# "raw_delta", "annualized_delta"
annualization_modes <- c("annualized_delta")

# "baseline_deforestation", "cutoff_defor", "lagged_deforestation"
defor_bins <- c("baseline_deforestation", "cutoff_defor", "lagged_deforestation")

# "year_pair_delta_ov", "whole_cluster_delta_ov"
delta_ov_approaches <- c("year_pair_delta_ov", "whole_cluster_delta_ov")

# Hansen uses raster exposure inside the cluster buffer, so single-tile collapse is not meaningful.
single_tile_collapse_modes <- c("keep_single_tile_clusters")

# "ov_full", "ov_obs_only", "ov_no_hqi", "ov_no_hanpp", "ov_no_msa", "ov_no_pd", "ov_no_shannon"
ov_calculation_methods <- c("ov_full", "ov_obs_only")

# "linear_delta_ov", "thresholded_linear_delta_ov", "percent_delta_ov", "thresholded_percent_delta_ov"
ov_change_modes <- c(
  "linear_delta_ov",
  "thresholded_linear_delta_ov",
  "percent_delta_ov"
)

# "no_starting_ov_adjustment", "starting_ov_control", "starting_ov_interaction"
# This is a formula-specification toggle. The interaction mode estimates the
# deforestation slope at average starting OV within each regression group and
# allows that slope to vary with starting OV.
starting_ov_adjustment_modes <- c("starting_ov_interaction")

# Starting-OV cutoffs used by thresholded_linear_delta_ov and thresholded_percent_delta_ov.
# These are intentionally centralized so threshold sensitivity runs only need
# config changes. Current values are the more aggressive cutoffs.
ov_thresholds <- list(
  ov_full = 1.50,
  ov_obs_only = 0.25,
  default = 1.50
)

# "hansen_total_deforestation", "hansen_land_share_deforestation",
# "hansen_treecover_share_deforestation"
defor_tile_sum_methods <- c(
  "hansen_treecover_share_deforestation"
)

# "ols", "robust_linear", "gam_spline"
regression_models <- c("ols")

# "defor_raw", "defor_log1p", "defor_p90_trimmed"
defor_transforms <- c("defor_raw", "defor_log1p")

# Minimum sample size required to run group-specific regressions.
min_observations_per_group_regression <- 3

# Filter: TRUE to only keep negative ∆OV, FALSE to include positive
filter_neg_delta_ov <- TRUE

# ------------------------------
# Directory and label name specification
# ------------------------------

regression_group_specs <- list(
  by_aez = list(
    group_col = "aez",
    output_dir = "by_aez",
    label = "AEZ"
  ),
  by_climate_zone = list(
    group_col = "climate_zone",
    output_dir = "by_climate_zone",
    label = "climate zone"
  ),
  by_country = list(
    group_col = "country",
    output_dir = "by_country",
    label = "country"
  )
)

annualization_mode_specs <- list(
  raw_delta = list(
    output_dir = "raw_delta",
    source_mode = "non_annualized",
    label = "raw delta"
  ),
  annualized_delta = list(
    output_dir = "annualized_delta",
    source_mode = "annualized",
    label = "annualized delta"
  )
)

defor_bin_specs <- list(
  baseline_deforestation = list(
    output_dir = "baseline_deforestation",
    source_mode = "baseline",
    label = "baseline"
  ),
  cutoff_defor = list(
    output_dir = "cutoff_defor",
    source_mode = "cutoff",
    label = "two-year cutoff"
  ),
  lagged_deforestation = list(
    output_dir = "lagged_deforestation",
    source_mode = "lagged",
    label = "prior-year lag"
  )
)

delta_ov_approach_specs <- list(
  year_pair_delta_ov = list(
    output_dir = "year_pair_delta_ov",
    source_dir = "ov_year_pair",
    label = "year-pair delta OV"
  ),
  whole_cluster_delta_ov = list(
    output_dir = "whole_cluster_delta_ov",
    source_dir = "ov_whole_cluster",
    label = "whole-cluster delta OV"
  )
)

single_tile_collapse_mode_specs <- list(
  collapse_single_tile_clusters = list(
    output_dir = "collapse_single_tile_clusters",
    source_dir = "collapse_on",
    label = "single-tile collapse on"
  ),
  keep_single_tile_clusters = list(
    output_dir = "keep_single_tile_clusters",
    source_dir = "collapse_off",
    label = "single-tile collapse off"
  )
)

ov_calculation_specs <- list(
  ov_full = list(
    delta_col = "delta_ov",
    annualized_col = "delta_ov_annualized",
    initial_col = "ov_t1",
    output_dir = "ov_full",
    label = "full OV"
  ),
  ov_obs_only = list(
    delta_col = "delta_ov_obs_only",
    annualized_col = "delta_ov_obs_only_annualized",
    initial_col = "ov_obs_only_t1",
    output_dir = "ov_obs_only",
    label = "observed biodiversity only"
  ),
  ov_no_hqi = list(
    delta_col = "delta_ov_no_hqi",
    annualized_col = "delta_ov_no_hqi_annualized",
    initial_col = "ov_no_hqi_t1",
    output_dir = "ov_no_hqi",
    label = "OV without HQI"
  ),
  ov_no_hanpp = list(
    delta_col = "delta_ov_no_hanpp",
    annualized_col = "delta_ov_no_hanpp_annualized",
    initial_col = "ov_no_hanpp_t1",
    output_dir = "ov_no_hanpp",
    label = "OV without HANPP"
  ),
  ov_no_msa = list(
    delta_col = "delta_ov_no_msa",
    annualized_col = "delta_ov_no_msa_annualized",
    initial_col = "ov_no_msa_t1",
    output_dir = "ov_no_msa",
    label = "OV without MSA"
  ),
  ov_no_pd = list(
    delta_col = "delta_ov_no_pd",
    annualized_col = "delta_ov_no_pd_annualized",
    initial_col = "ov_no_pd_t1",
    output_dir = "ov_no_pd",
    label = "OV without PD"
  ),
  ov_no_shannon = list(
    delta_col = "delta_ov_no_shannon",
    annualized_col = "delta_ov_no_shannon_annualized",
    initial_col = "ov_no_shannon_t1",
    output_dir = "ov_no_shannon",
    label = "OV without Shannon"
  )
)

ov_change_mode_specs <- list(
  linear_delta_ov = list(
    output_dir = "linear_delta_ov",
    label = "plain delta OV",
    apply_threshold = FALSE,
    use_percent_change = FALSE
  ),
  thresholded_linear_delta_ov = list(
    output_dir = "thresholded_linear_delta_ov",
    label = "starting OV threshold cutoff",
    apply_threshold = TRUE,
    use_percent_change = FALSE
  ),
  percent_delta_ov = list(
    output_dir = "percent_delta_ov",
    label = "percentage OV change",
    apply_threshold = FALSE,
    use_percent_change = TRUE
  ),
  thresholded_percent_delta_ov = list(
    output_dir = "thresholded_percent_delta_ov",
    label = "threshold cutoff and percentage OV change",
    apply_threshold = TRUE,
    use_percent_change = TRUE
  )
)

starting_ov_adjustment_specs <- list(
  no_starting_ov_adjustment = list(
    output_dir = "no_starting_ov_adjustment",
    label = "no starting OV adjustment",
    formula_mode = "none"
  ),
  starting_ov_control = list(
    output_dir = "starting_ov_control",
    label = "starting OV linear control",
    formula_mode = "control"
  ),
  starting_ov_interaction = list(
    output_dir = "starting_ov_interaction",
    label = "starting OV control and interaction",
    formula_mode = "interaction"
  )
)

# ------------------------------
# Table writing toggle settings
# ------------------------------
write_model_tables <- FALSE
write_regression_plots <- TRUE
write_histogram_plots <- FALSE
build_master_output_report <- TRUE
verbose_console_output <- FALSE


defor_tile_sum_specs <- list(
  hansen_total_deforestation = list(
    output_dir = "hansen_total_deforestation",
    source_cols = list(
      baseline = "delta_defor_ha_total_raw",
      cutoff = "delta_defor_ha_total_raw_cutoff",
      lagged = "delta_defor_ha_total_raw_lagged"
    ),
    label = "Hansen raster deforestation hectares",
    outcome_label = "all deforested hectares"
  ),
  hansen_land_share_deforestation = list(
    output_dir = "hansen_land_share_deforestation",
    source_cols = list(
      baseline = "delta_defor_land_share_raw",
      cutoff = "delta_defor_land_share_raw_cutoff",
      lagged = "delta_defor_land_share_raw_lagged"
    ),
    label = "Hansen deforestation share of buffer land",
    outcome_label = "deforested hectares per buffer land hectare"
  ),
  hansen_treecover_share_deforestation = list(
    output_dir = "hansen_treecover_share_deforestation",
    source_cols = list(
      baseline = "delta_defor_treecover_share_raw",
      cutoff = "delta_defor_treecover_share_raw_cutoff",
      lagged = "delta_defor_treecover_share_raw_lagged"
    ),
    label = "Hansen deforestation share of baseline tree cover",
    outcome_label = "deforested hectares per baseline tree-cover-equivalent hectare"
  ),
  total_tagged_tile_deforestation = list(
    output_dir = "total_tagged_tile_deforestation",
    source_suffix = "raw",
    label = "total tile deforestation"
  ),
  mean_tagged_tile_deforestation = list(
    output_dir = "mean_tagged_tile_deforestation",
    source_suffix = "avg",
    label = "average tile deforestation"
  ),
  overlap_weighted_deforestation = list(
    output_dir = "overlap_weighted_deforestation",
    source_suffix = "rel_pct",
    label = "overlap-weighted tile deforestation"
  )
)

defor_transform_specs <- list(
  defor_raw = list(
    output_dir = "defor_raw",
    source_transform = "raw",
    label = "raw deforestation"
  ),
  defor_log1p = list(
    output_dir = "defor_log1p",
    source_transform = "log1p",
    label = "log1p deforestation"
  ),
  defor_p90_trimmed = list(
    output_dir = "defor_p90_trimmed",
    source_transform = "p90",
    label = "p90-trimmed deforestation"
  )
)

build_defor_approach_specs <- function(tile_sum_methods = defor_tile_sum_methods) {
  # Regressions always use total deforested hectares. Crop-hectare variants are
  # intentionally not part of the config or output hierarchy.
  purrr::map(
    tile_sum_methods,
    function(tile_sum_method) {
      tile_sum_spec <- defor_tile_sum_specs[[tile_sum_method]]
      if ("source_cols" %in% names(tile_sum_spec)) {
        source_cols <- tile_sum_spec$source_cols
      } else {
        source_col <- paste0("delta_defor_ha_total_", tile_sum_spec$source_suffix)
        source_cols <- list(
          baseline = source_col,
          cutoff = paste0(source_col, "_cutoff"),
          lagged = paste0(source_col, "_lagged")
        )
      }

      list(
        source_cols = source_cols,
        tile_sum_dir = tile_sum_spec$output_dir,
        tile_sum_label = tile_sum_spec$label,
        label = paste(
          tile_sum_spec$label,
          "-",
          if ("outcome_label" %in% names(tile_sum_spec)) {
            tile_sum_spec$outcome_label
          } else {
            "all deforested hectares"
          }
        )
      )
    }
  ) |>
    rlang::set_names(tile_sum_methods)
}

defor_approach_specs <- build_defor_approach_specs()

# ------------------------------
# Required columns in cluster_deltas and for regressions
# ------------------------------

required_cluster_deltas_cols <- c(
  "cluster_database",
  "cluster_method",
  "cluster_radius_km",
  "aez",
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
  "primary_country_id",
  "primary_country_name",
  "n_defor_years",
  "delta_defor_ha_total_raw",
  "delta_defor_ha_total_avg",
  "delta_defor_ha_total_rel_pct",
  "delta_defor_land_share_raw",
  "delta_defor_treecover_share_raw",
  "delta_defor_ha",
  "inverse_change"
)

required_regression_cols <- c(
  "group_value",
  "starting_ov",
  "starting_ov_centered",
  "delta_ov",
  "delta_defor_ha",
  "log1p_delta_defor_ha"
)

# ------------------------------
# Regression run grid - to be looped over in 04_ols_regression
# contains all the methods, radii, buffers specified above
# ------------------------------

regression_run_grid <- crossing(
  cluster_database = cluster_databases,
  cluster_method = cluster_methods,
  cluster_radius_km = cluster_radii,
  buffer_km = buffers
)

# ------------------------------
# Directories and file paths
# ------------------------------

# Assumes working directory is ov_metric/05_regression/code
repo_root <- normalizePath(file.path("..", "..", ".."), winslash = "/", mustWork = TRUE)
ov_metric_dir <- file.path(repo_root, "ov_metric")

regression_dir <- file.path(ov_metric_dir, "05_regression")
code_dir <- file.path(regression_dir, "code")
output_dir <- file.path(regression_dir, "output")
tmp_dir <- file.path(regression_dir, "tmp")
build_output_dir <- file.path(ov_metric_dir, "04_deforestation_tile_tag", "build", "output")

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
current_cluster_database <- NA_character_

current_cluster_stub <- NA_character_
current_buffer_stub <- NA_character_
current_run_label <- NA_character_
current_annualization_mode <- NA_character_
current_delta_ov_approach <- NA_character_
current_single_tile_collapse_mode <- NA_character_
current_single_tile_collapse_label <- NA_character_
current_ov_calculation_method <- NA_character_
current_ov_calculation_label <- NA_character_
current_delta_ov_source_col <- NA_character_
current_delta_ov_annualized_source_col <- NA_character_
current_starting_ov_source_col <- NA_character_
current_ov_change_mode <- NA_character_
current_ov_change_label <- NA_character_
current_ov_threshold <- NA_real_
current_starting_ov_adjustment_mode <- NA_character_
current_starting_ov_adjustment_label <- NA_character_
current_starting_ov_adjustment_formula_mode <- NA_character_
current_regression_model <- NA_character_
current_regression_group <- NA_character_
current_group_col <- NA_character_
current_group_label <- NA_character_
current_defor_approach <- NA_character_
current_defor_bin <- NA_character_
current_defor_tile_sum <- NA_character_
current_defor_source_col <- NA_character_
current_defor_transform <- NA_character_

cluster_deltas_path <- NULL
canonical_tabular_dir <- NULL
canonical_spatial_dir <- NULL
analysis_output_dir_current <- NULL

annualization_mode_output_dir <- NULL
ov_calculation_output_dir <- NULL
ov_output_dir <- NULL
single_tile_collapse_output_dir <- NULL
ov_change_output_dir <- NULL
starting_ov_adjustment_output_dir <- NULL
defor_approach_output_dir <- NULL
regression_model_output_dir <- NULL
output_dirs_by_transform <- NULL
