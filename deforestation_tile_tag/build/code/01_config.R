# =====================================================
# Configuration of paths and analysis settings
# =====================================================

# -----------------------
# Directory structure
# -----------------------

# Base directories relative to build/code/
input_dir <- file.path("..", "..", "input")
build_dir <- ".."
build_output_dir <- file.path(build_dir, "output")
build_tmp_dir <- file.path(build_dir, "tmp")

canonical_tabular_dir <- file.path(build_output_dir, "canonical_tabular")
canonical_spatial_dir <- file.path(build_output_dir, "canonical_spatial")

# External data locations
defor_spatial_dir <- file.path("..", "..", "..", "spatial_data", "deforestation", "spatial")
defor_tabular_dir <- file.path("..", "..", "..", "spatial_data", "deforestation", "tabular")
cluster_dir <- file.path("..", "..", "..", "clustering", "output")

# Ensure build directories exist
dir.create(build_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(build_tmp_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------
# Analysis settings
# -----------------------

# Equal-area CRS used for buffering and area calculations
analysis_crs <- 6933
buffer_km_vals <- c(1, 3, 5, 10)

# -----------------------
# Cluster input selection
# -----------------------

cluster_method <- "clara"
cluster_radius <- "12.5km"

cluster_file <- paste0(
  cluster_method,
  "_rad_",
  cluster_radius,
  "_model_df_clustered.csv"
)

# -----------------------
# Input file paths
# -----------------------

defor_spatial_file <- file.path(
  defor_spatial_dir,
  "deforestation_50km.shp"
)

defor_tabular_file <- file.path(
  defor_tabular_dir,
  "deforestation_tile_total_ha_SPAM2020_2001_2024.csv"
)

cluster_file_path <- file.path(
  cluster_dir,
  cluster_file
)


# -----------------------
# Output file paths
# -----------------------

canonical_tabular_dir <- file.path(build_output_dir, "canonical_tabular")
canonical_spatial_dir <- file.path(build_output_dir, "canonical_spatial")

dir.create(canonical_tabular_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(canonical_spatial_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------
# Cache files
# -----------------------

# Cached spatial objects that are slow to rebuild
defor_tiles_cache <- file.path(build_tmp_dir, "defor_tiles_sf.rds")
cluster_footprints_cache <- file.path(build_tmp_dir, "cluster_footprints_sf.rds")