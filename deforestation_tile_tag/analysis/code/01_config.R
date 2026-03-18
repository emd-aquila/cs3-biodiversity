# =====================================================
# Configuration of paths and analysis settings
# =====================================================

# -----------------------
# Directory structure
# -----------------------

# Base directories relative to analysis/code/
analysis_dir <- ".."
analysis_output_dir <- file.path(analysis_dir, "output")
analysis_tmp_dir <- file.path(analysis_dir, "tmp")

# Build outputs (inputs to analysis)
build_dir <- file.path("..", "..", "build")
build_output_dir <- file.path(build_dir, "output")

canonical_tabular_dir <- file.path(build_output_dir, "canonical_tabular")
canonical_spatial_dir <- file.path(build_output_dir, "canonical_spatial")

# Ensure analysis directories exist
dir.create(analysis_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(analysis_tmp_dir, recursive = TRUE, showWarnings = FALSE)

analysis_tables_dir <- file.path(analysis_output_dir, "tables")
analysis_figures_dir <- file.path(analysis_output_dir, "figures")

dir.create(analysis_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(analysis_figures_dir, recursive = TRUE, showWarnings = FALSE)

aez_path <- file.path("..", "..", "..", "spatial_data", "aez", "AEZ_shp_file.shp")

# -----------------------
# Analysis settings
# -----------------------

buffer_km_focus <- c(1,3,5,10)