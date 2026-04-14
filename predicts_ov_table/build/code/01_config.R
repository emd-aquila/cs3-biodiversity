# =====================================================
# Configuration for the PREDICTS build pipeline
# =====================================================

# -----------------------
# Directories
# -----------------------

build_dir <- ".."
input_dir <- file.path(build_dir, "input")
tmp_dir <- file.path(build_dir, "tmp")
output_dir <- file.path(build_dir, "output")

dirs_to_create <- c(tmp_dir, output_dir)

for (dir_path in dirs_to_create) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
}

# -----------------------
# Run settings
# -----------------------

extract_years <- c(2016, 2022)
date_min <- as.Date("2000-01-01")
date_max <- as.Date("2024-12-31")

# -----------------------
# File paths
# -----------------------

predicts_rds_path <- file.path(tmp_dir, "predicts.rds")
predicts_filtered_path <- file.path(
  output_dir,
  paste0(
    "predicts_filtered_",
    format(date_min, "%Y"), "_",
    format(date_max, "%Y"),
    ".csv"
  )
)

family_present_path <- file.path(tmp_dir, "family_names_present.txt")
family_unmatched_path <- file.path(tmp_dir, "family_names_unmatched.txt")
pd_output_path <- file.path(output_dir, "pd_result.csv")
tree_file <- file.path(input_dir, "iphylo_tree.nwk")

if (!file.exists(tree_file)) {
  stop("Phylogenetic tree file not found: ", normalizePath(tree_file, mustWork = FALSE))
}
