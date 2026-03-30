# =====================================================
# Configuration of paths and analysis settings
# =====================================================

# -----------------------
# Directory structure
# -----------------------

# Base directories relative to code/
code_dir <- "code"
input_dir <- "input"
output_dir <- "output"
tmp_dir <- "tmp"

# External project folders relative to cluster/
predicts_out_dir <- file.path("..", "predicts_ov_table", "analysis", "output")
aez_dir <- file.path("..", "spatial_data", "aez")

# Internal subdirectories
checkpoint_root <- file.path(tmp_dir, "checkpoints")
summary_dir <- file.path(output_dir, "summary")

# -----------------------
# Create directories if they do not exist
# -----------------------

dirs_to_create <- c(output_dir, tmp_dir, checkpoint_root, summary_dir)

for (d in dirs_to_create) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# -----------------------
# Input file paths
# -----------------------

# Read directly from predicts_ov_table output rather than copying into input/
tagged_sites_file <- file.path(predicts_out_dir, "ov_AEZ_tag.csv")

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
method = c("PAM", "GREEDY", "CLARA")
radius_km = c(10, 12.5, 17.5, 20, 25)
radius_m <- radius_km * 1000

run_grid <- tidyr::crossing(
  method,
  radius_km
) %>%
  mutate(radius_m = radius_km * 1000)