# =====================================================
# Configuration of paths and run settings for the clustering pipeline
# =====================================================

# -----------------------
# Directory structure
# -----------------------

# Base directories relative to code/
cluster_dir <- ".."
cluster_output_dir <- file.path(cluster_dir, "output")
cluster_tmp_dir <- file.path(cluster_dir, "tmp")

# Upstream sources relative to code/
ov_metric_dir <- normalizePath(file.path(cluster_dir, ".."), winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(file.path(ov_metric_dir, ".."), winslash = "/", mustWork = TRUE)
ov_output_dir <- file.path(ov_metric_dir, "02_ov_calculation", "calculation", "output")
aez_dir <- file.path(project_dir, "00_spatial_data", "aez")

# Dataset-specific inputs and output roots.
cluster_dataset_specs <- tibble::tibble(
  dataset_key = c("combined", "predicts", "biotime"),
  dataset_label = c("Combined", "PREDICTS", "BioTIME"),
  tagged_sites_file = file.path(
    ov_output_dir,
    c("combined_ov_scores.csv", "predicts_ov_scores.csv", "biotime_ov_scores.csv")
  ),
  output_dir = file.path(cluster_output_dir, dataset_key),
  tmp_dir = file.path(cluster_tmp_dir, dataset_key),
  checkpoint_root = file.path(tmp_dir, "checkpoints"),
  summary_dir = file.path(output_dir, "summary")
)

# -----------------------
# Create directories if they do not exist
# -----------------------

dirs_to_create <- c(
  cluster_output_dir,
  cluster_tmp_dir,
  cluster_dataset_specs$output_dir,
  cluster_dataset_specs$tmp_dir,
  cluster_dataset_specs$checkpoint_root,
  cluster_dataset_specs$summary_dir
)

for (d in dirs_to_create) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

# -----------------------
# Input file paths
# -----------------------

# AEZ shapefile
aez_file <- file.path(aez_dir, "AEZ_shp_file.shp")

# -----------------------
# Basic file checks
# -----------------------

missing_tagged_files <- cluster_dataset_specs$tagged_sites_file[
  !file.exists(cluster_dataset_specs$tagged_sites_file)
]
if (length(missing_tagged_files) > 0) {
  stop(
    "Tagged OV score file(s) not found: ",
    paste(normalizePath(missing_tagged_files, mustWork = FALSE), collapse = ", "),
    call. = FALSE
  )
}

if (!file.exists(aez_file)) {
  stop("AEZ shapefile not found: ", normalizePath(aez_file, mustWork = FALSE))
}

# -----------------------
# Building a config grid for clustering pipeline
# -----------------------
# Greedy Cover is used as the default because for the combined BioTIME + PREDICTS datasets,
# CLARA takes far too long on the large AEZs 10, 11, and 12 (it did not finish after 9 days).

# I updated the CLARA approach to start with higher k, so am running it again in the background
# to see how it works out.
clustering_methods <- c("CLARA") # PAM, GREEDY, CLARA
# clustering_methods <- c("GREEDY")
clustering_radii_km <- c(2.5, 5, 7.5, 10)

# Restart behavior - if TRUE, a dataset/radius/method run is skipped when its
# saved clustering outputs already exist.
skip_existing_cluster_outputs <- TRUE

# Parallelization 
available_cores <- as.integer(future::availableCores())
if (is.na(available_cores)) available_cores <- 1L

# Number of AEZ tasks to run at once. The queue is one layer deep:
# dataset/method/radius/AEZ tasks are layered, so another radius can keep going
# while a few large AEZs are still running.
clustering_parallel_workers <- max(1L, available_cores - 1L)

# CLARA radius search settings. To make CLARA faster, I first run GREEDY to estimate
# a plausible k (number of clusters) and start CLARA there instead of k=2 and increasing
clara_k_max <- .Machine$integer.max
clara_samples <- 5L
clara_sample_k_multiplier <- 10L
clara_k_start_strategy <- "greedy_cover" # "greedy_cover" or "fixed"
clara_greedy_start_fraction <- 1 # this could be set to 0.75 
clara_refine_to_min_k <- FALSE

run_grid <- tidyr::expand_grid(
  method = clustering_methods,
  radius_km = clustering_radii_km
) %>%
  mutate(radius_m = radius_km * 1000)
