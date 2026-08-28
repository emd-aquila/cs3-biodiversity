# =====================================================
# Configuration of paths and run settings for the clustering pipeline
# =====================================================

# -----------------------
# Directory structure
# -----------------------

# Base directories relative to code/
cluster_dir <- ".."
output_dir <- file.path(cluster_dir, "output")
tmp_dir <- file.path(cluster_dir, "tmp")

# Upstream sources relative to code/
predicts_output_dir <- file.path("..", "..", "predicts_ov_table", "analysis", "output")
aez_dir <- file.path("..", "..", "spatial_data", "aez")

# Internal subdirectories
checkpoint_root <- file.path(tmp_dir, "checkpoints")
summary_dir <- file.path(output_dir, "summary")

# -----------------------
# Create directories if they do not exist
# -----------------------

dirs_to_create <- c(output_dir, tmp_dir, checkpoint_root, summary_dir)

for (d in dirs_to_create) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

# -----------------------
# Input file paths
# -----------------------

# Read tagged site observations directly from the upstream project output.
tagged_sites_file <- file.path(predicts_output_dir, "site_ov_aez.csv")

# AEZ shapefile
aez_file <- file.path(aez_dir, "AEZ_shp_file.shp")

# -----------------------
# Basic file checks
# -----------------------

if (!file.exists(tagged_sites_file)) {
  stop("Tagged sites file not found: ", normalizePath(tagged_sites_file, mustWork = FALSE))
}

if (!file.exists(aez_file)) {
  stop("AEZ shapefile not found: ", normalizePath(aez_file, mustWork = FALSE))
}

# -----------------------
# Building a config grid for clustering pipeline
# -----------------------
clustering_methods <- c("GREEDY", "CLARA") # PAM, GREEDY, CLARA
clustering_radii_km <- c(2.5, 5, 7.5)

# Parallelize independent AEZ clustering jobs within each method-radius run.
# Set to 1 to force serial execution.
available_cores <- as.integer(future::availableCores())
if (is.na(available_cores)) available_cores <- 1L
clustering_parallel_workers <- max(1L, available_cores - 1L)

# CLARA radius search settings. Smaller radii often need many clusters, so the
# search uses exponential bracketing plus binary refinement instead of trying
# every k from 2 upward. Leave clara_k_max uncapped by default so every AEZ can
# reach the requested radius when necessary.
clara_k_max <- .Machine$integer.max
clara_samples <- 5L
clara_sample_k_multiplier <- 10L

run_grid <- tidyr::crossing(
  method = clustering_methods,
  radius_km = clustering_radii_km
) %>%
  mutate(radius_m = radius_km * 1000)
