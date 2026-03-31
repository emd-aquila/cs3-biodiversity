# =====================================================
# Configuration of paths and analysis settings
# =====================================================

# -----------------------
# Analysis settings
# -----------------------

# workflows to run when sourcing run_analysis.R
analysis_modes_to_run <- c("year_pair", "whole_cluster")

# buffers to analyze from the canonical build outputs
buffer_km_focus <- c(1, 5, 10)

# clustering method and radius combinations to analyze
cluster_methods <- c("clara", "greedy_cover", "pam")
cluster_radius_km_vals <- c(10.0, 12.5, 17.5, 20.0, 25.0)

cluster_run_grid <- crossing(
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

analysis_output_root_dir <- file.path(analysis_dir, "output")
dir.create(analysis_output_root_dir, recursive = TRUE, showWarnings = FALSE)

analysis_tmp_root_dir <- file.path(analysis_dir, "tmp")
dir.create(analysis_tmp_root_dir, recursive = TRUE, showWarnings = FALSE)

build_output_dir <- file.path("..", "..", "build", "output")
aez_path <- file.path("..", "..", "..", "spatial_data", "aez", "AEZ_shp_file.shp")

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
  valid_modes <- c("year_pair", "whole_cluster")
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

set_analysis_mode_paths <- function(analysis_mode) {
  validate_analysis_mode(analysis_mode)

  current_analysis_mode <<- analysis_mode
  analysis_output_dir <<- file.path(analysis_output_root_dir, analysis_mode)
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

set_analysis_run_paths <- function(cluster_method, cluster_radius_km) {
  current_cluster_method <<- cluster_method
  current_cluster_radius_km <<- cluster_radius_km
  current_cluster_stub <<- cluster_stub_from_run(cluster_method, cluster_radius_km)

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
}

# -----------------------
# Buffer-run path setter
# -----------------------

set_buffer_output_dirs <- function(buffer_km, cluster_stub = current_cluster_stub) {
  current_buffer_km <<- buffer_km
  current_buffer_key <<- buffer_dir_name(buffer_km)

  current_output_dirs <<- list(
    buffer_dir = file.path(analysis_output_dir, current_buffer_key),
    run_dir = file.path(analysis_output_dir, current_buffer_key, cluster_stub),
    tables_dir = file.path(analysis_output_dir, current_buffer_key, cluster_stub, "tables"),
    figures_dir = file.path(analysis_output_dir, current_buffer_key, cluster_stub, "figures"),
    tmp_dir = file.path(analysis_output_dir, current_buffer_key, cluster_stub, "tmp")
  )

  dir.create(current_output_dirs$buffer_dir, recursive = TRUE, showWarnings = FALSE)
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

current_analysis_mode <- NA_character_
analysis_output_dir <- NULL
analysis_tmp_dir <- NULL

current_cluster_method <- NA_character_
current_cluster_radius_km <- NA_real_
current_cluster_stub <- NA_character_

current_buffer_km <- NA_real_
current_buffer_key <- NA_character_

current_output_dirs <- NULL
analysis_tables_dir_current <- NULL
analysis_figures_dir_current <- NULL
analysis_tmp_dir_current <- NULL
