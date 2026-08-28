# =====================================================
# Configuration of paths and analysis settings
# =====================================================

# -----------------------
# Analysis settings
# -----------------------

# Workflows to run when sourcing run_analysis.R; wrappers can override this option to run a subset.
analysis_modes_to_run <- getOption("defor_analysis_modes", c("ov_year_pair", "ov_whole_cluster"))
single_tile_collapse_modes_to_run <- getOption(
  "defor_single_tile_collapse_modes",
  c("collapse_off")
)

ov_score_cols <- c(
  "ov_score",
  "ov_obs_only",
  "ov_no_HQI",
  "ov_no_HANPP",
  "ov_no_MSA",
  "ov_no_PD",
  "ov_no_Shannon"
)

ov_score_specs <- tibble::tribble(
  ~ov_method,        ~site_col,        ~cluster_year_col,              ~t1_col,             ~t2_col,             ~delta_col,               ~annualized_col,
  "ov_full",         "ov_score",       "median_ov_year",              "ov_t1",             "ov_t2",             "delta_ov",               "delta_ov_annualized",
  "ov_obs_only",     "ov_obs_only",    "median_ov_obs_only_year",     "ov_obs_only_t1",    "ov_obs_only_t2",    "delta_ov_obs_only",      "delta_ov_obs_only_annualized",
  "ov_no_hqi",       "ov_no_HQI",      "median_ov_no_HQI_year",       "ov_no_HQI_t1",      "ov_no_HQI_t2",      "delta_ov_no_HQI",        "delta_ov_no_HQI_annualized",
  "ov_no_hanpp",     "ov_no_HANPP",    "median_ov_no_HANPP_year",     "ov_no_HANPP_t1",    "ov_no_HANPP_t2",    "delta_ov_no_HANPP",      "delta_ov_no_HANPP_annualized",
  "ov_no_msa",       "ov_no_MSA",      "median_ov_no_MSA_year",       "ov_no_MSA_t1",      "ov_no_MSA_t2",      "delta_ov_no_MSA",        "delta_ov_no_MSA_annualized",
  "ov_no_pd",        "ov_no_PD",       "median_ov_no_PD_year",        "ov_no_PD_t1",       "ov_no_PD_t2",       "delta_ov_no_PD",         "delta_ov_no_PD_annualized",
  "ov_no_shannon",   "ov_no_Shannon",  "median_ov_no_Shannon_year",   "ov_no_Shannon_t1",  "ov_no_Shannon_t2",  "delta_ov_no_Shannon",    "delta_ov_no_Shannon_annualized"
)

# clustering method, radius, buffer combinations to analyze
cluster_databases <- c("predicts", "biotime", "combined")
cluster_methods <- c("greedy_cover") # can choose clara, greedy_cover, pam
cluster_radius_km_vals <- c(2.5, 5, 7.5, 10)

buffer_km_focus <- c(0, 0.5, 1, 2.5, 5) # Hansen 30 m raster buffers

single_tile_collapse_mode_specs <- list(
  collapse_on = list(
    output_dir = "collapse_on",
    collapse_single_tile_clusters = TRUE,
    label = "single-tile collapse on"
  ),
  collapse_off = list(
    output_dir = "collapse_off",
    collapse_single_tile_clusters = FALSE,
    label = "single-tile collapse off"
  )
)

# Current default. run_analysis.R will set this for each paired output mode.
collapse_single_tile_clusters <- TRUE

cluster_run_grid <- crossing(
  cluster_database = cluster_databases,
  cluster_method = cluster_methods,
  cluster_radius_km = cluster_radius_km_vals
) %>%
  mutate(
    cluster_stub = paste0(
      cluster_method,
      "_rad_",
      sprintf("%.1fkm", cluster_radius_km)
    )
  )

# -----------------------
# Base directory structure
# -----------------------

# analysis/code assumed as working directory
analysis_dir <- ".."
ov_metric_dir <- normalizePath(file.path(analysis_dir, "..", ".."), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(ov_metric_dir, ".."), winslash = "/", mustWork = TRUE)

analysis_output_root_dir <- file.path(analysis_dir, "output")
dir.create(analysis_output_root_dir, recursive = TRUE, showWarnings = FALSE)

analysis_tmp_root_dir <- file.path(analysis_dir, "tmp")
dir.create(analysis_tmp_root_dir, recursive = TRUE, showWarnings = FALSE)

build_output_dir <- file.path("..", "..", "build", "output")
aez_path <- file.path(repo_root, "00_spatial_data", "aez", "AEZ_shp_file.shp")

# -----------------------
# Naming helpers
# -----------------------

buffer_dir_name <- function(buffer_km) {
  paste0("buf_", buffer_km, "km")
}

cluster_stub_from_run <- function(cluster_method, cluster_radius_km) {
  paste0(
    cluster_method,
    "_rad_",
    sprintf("%.1fkm", cluster_radius_km)
  )
}

validate_analysis_mode <- function(analysis_mode) {
  valid_modes <- c("ov_year_pair", "ov_whole_cluster")
  if (!analysis_mode %in% valid_modes) {
    stop(
      "Unknown analysis_mode: ",
      analysis_mode,
      ". Valid modes are: ",
      paste(valid_modes, collapse = ", "),
      call. = FALSE
    )
  }
}

walk(analysis_modes_to_run, validate_analysis_mode)

validate_single_tile_collapse_mode <- function(single_tile_collapse_mode) {
  valid_modes <- names(single_tile_collapse_mode_specs)
  if (!single_tile_collapse_mode %in% valid_modes) {
    stop(
      "Unknown single_tile_collapse_mode: ",
      single_tile_collapse_mode,
      ". Valid modes are: ",
      paste(valid_modes, collapse = ", "),
      call. = FALSE
    )
  }
}

walk(single_tile_collapse_modes_to_run, validate_single_tile_collapse_mode)

set_single_tile_collapse_mode <- function(single_tile_collapse_mode) {
  validate_single_tile_collapse_mode(single_tile_collapse_mode)

  spec <- single_tile_collapse_mode_specs[[single_tile_collapse_mode]]

  current_single_tile_collapse_mode <<- single_tile_collapse_mode
  current_single_tile_collapse_output_dir <<- spec$output_dir
  current_single_tile_collapse_label <<- spec$label
  collapse_single_tile_clusters <<- spec$collapse_single_tile_clusters

  invisible(
    list(
      current_single_tile_collapse_mode = current_single_tile_collapse_mode,
      current_single_tile_collapse_output_dir = current_single_tile_collapse_output_dir,
      current_single_tile_collapse_label = current_single_tile_collapse_label,
      collapse_single_tile_clusters = collapse_single_tile_clusters
    )
  )
}

set_analysis_mode_paths <- function(analysis_mode) {
  validate_analysis_mode(analysis_mode)

  current_ov_approach <<- analysis_mode
  analysis_output_dir <<- analysis_output_root_dir
  analysis_tmp_dir <<- file.path(analysis_tmp_root_dir, analysis_mode)

  dir.create(analysis_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(analysis_tmp_dir, recursive = TRUE, showWarnings = FALSE)

  invisible(
    list(
      analysis_output_dir = analysis_output_dir,
      analysis_tmp_dir = analysis_tmp_dir
    )
  )
}

# -----------------------
# Cluster-run path setter
# -----------------------

set_analysis_run_paths <- function(cluster_database, cluster_method, cluster_radius_km) {
  current_cluster_database <<- cluster_database
  current_cluster_method <<- cluster_method
  current_cluster_radius_km <<- cluster_radius_km
  current_cluster_stub <<- cluster_stub_from_run(cluster_method, cluster_radius_km)
  current_cluster_run_label <<- file.path(cluster_database, current_cluster_stub)

  canonical_tabular_dir <<- file.path(
    build_output_dir,
    cluster_database,
    cluster_method,
    paste0("radius_", sprintf("%.1fkm", cluster_radius_km)),
    "canonical_tabular"
  )

  canonical_spatial_dir <<- file.path(
    build_output_dir,
    cluster_database,
    cluster_method,
    paste0("radius_", sprintf("%.1fkm", cluster_radius_km)),
    "canonical_spatial"
  )
}

# -----------------------
# Buffer-run path setter
# -----------------------

# Build output folders in the order database -> cluster method -> radius -> buffer ->
# delta-OV approach -> single-tile collapse mode.
build_analysis_output_dirs <- function(buffer_km,
                                       analysis_mode = current_ov_approach,
                                       single_tile_collapse_mode = current_single_tile_collapse_mode,
                                       cluster_database = current_cluster_database,
                                       cluster_method = current_cluster_method,
                                       cluster_radius_km = current_cluster_radius_km,
                                       base_dir = analysis_output_root_dir) {
  validate_single_tile_collapse_mode(single_tile_collapse_mode)

  database_dir <- cluster_database
  cluster_method_dir <- cluster_method
  cluster_radius_dir <- paste0("radius_", sprintf("%.1fkm", cluster_radius_km))
  buffer_dir <- buffer_dir_name(buffer_km)
  analysis_mode_dir <- analysis_mode
  single_tile_collapse_dir <- single_tile_collapse_mode_specs[[single_tile_collapse_mode]]$output_dir
  run_dir <- file.path(
    base_dir,
    database_dir,
    cluster_method_dir,
    cluster_radius_dir,
    buffer_dir,
    analysis_mode_dir,
    single_tile_collapse_dir
  )

  list(
    database_dir = database_dir,
    cluster_method_dir = cluster_method_dir,
    cluster_radius_dir = cluster_radius_dir,
    buffer_dir = buffer_dir,
    analysis_mode_dir = analysis_mode_dir,
    single_tile_collapse_dir = single_tile_collapse_dir,
    run_dir = run_dir,
    tables_dir = file.path(run_dir, "tables"),
    figures_dir = file.path(run_dir, "figures"),
    tmp_dir = file.path(run_dir, "tmp")
  )
}

set_buffer_output_dirs <- function(buffer_km, cluster_stub = current_cluster_stub) {
  current_buffer_km <<- buffer_km
  current_buffer_key <<- buffer_dir_name(buffer_km)

  current_output_dirs <<- build_analysis_output_dirs(
    buffer_km = buffer_km,
    analysis_mode = current_ov_approach,
    single_tile_collapse_mode = current_single_tile_collapse_mode,
    cluster_database = current_cluster_database,
    cluster_method = current_cluster_method,
    cluster_radius_km = current_cluster_radius_km,
    base_dir = analysis_output_dir
  )

  dir.create(current_output_dirs$run_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(current_output_dirs$tables_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(current_output_dirs$figures_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(current_output_dirs$tmp_dir, recursive = TRUE, showWarnings = FALSE)

  analysis_tables_dir_current <<- current_output_dirs$tables_dir
  analysis_figures_dir_current <<- current_output_dirs$figures_dir
  analysis_tmp_dir_current <<- current_output_dirs$tmp_dir

  invisible(current_output_dirs)
}

# -----------------------
# Current-run placeholders
# -----------------------

current_ov_approach <- NA_character_
analysis_output_dir <- NULL
analysis_tmp_dir <- NULL

current_single_tile_collapse_mode <- "collapse_on"
current_single_tile_collapse_output_dir <- "collapse_on"
current_single_tile_collapse_label <- "single-tile collapse on"

current_cluster_method <- NA_character_
current_cluster_radius_km <- NA_real_
current_cluster_stub <- NA_character_
current_cluster_database <- NA_character_
current_cluster_run_label <- NA_character_

current_buffer_km <- NA_real_
current_buffer_key <- NA_character_

current_output_dirs <- NULL
analysis_tables_dir_current <- NULL
analysis_figures_dir_current <- NULL
analysis_tmp_dir_current <- NULL
