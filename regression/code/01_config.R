# =====================================================
# Configuration for regression pipeline
# =====================================================

# ------------------------------
# Parameters + scenario grid information
# update these as I examine more things
# ------------------------------

min_observations_per_aez_regression <- 5

buffer_km = c(1, 5, 10)
defor_transform = c("raw", "log1p")
subdivision = c("aez", "aez_canopy")
model_family = c("ols")
fixed_effects = c("none","cluster")

scenario_grid <- crossing(
  buffer_km,
  defor_transform,
  subdivision,
  model_family,
  fixed_effects
) %>% 
  filter(
    !(fixed_effects == "cluster" & model_family != "ols")
  )

# function to name scenario IDs depending on options selected
scenario_id_from_row <- function(buffer_km,
                                 defor_transform,
                                 subdivision = "aez",
                                 model_family = "ols",
                                 fixed_effects = "none",
                                 weighting = "none") {
  parts <- c(
    paste0("buf_", buffer_km, "km"),
    if (subdivision != "aez") subdivision else NULL,
    paste0("defor_", defor_transform),
    model_family,
    if (fixed_effects != "none") paste0("fe_", fixed_effects) else NULL,
    if (weighting != "none") paste0("w_", weighting) else NULL
  )
  
  paste(parts, collapse = "__")
}


# ------------------------------
# Directories and file paths
# ------------------------------

# high-level
repo_root <- file.path("..", "..")
regression_dir <- file.path(repo_root, "regression")
code_dir <- file.path(regression_dir, "code")
output_dir <- file.path(regression_dir, "output")
tmp_dir <- file.path(regression_dir, "tmp")

# scenario-level

tables_dir <- file.path(output_dir, "tables")
models_dir <- file.path(output_dir, "models")
charts_dir <- file.path(output_dir, "charts")

# Upstream analysis directories and input files
analysis_output_dir <- file.path(repo_root, "deforestation_tile_tag", "analysis", "output")

# TODO: need to realign this to have multiple cluster_deltas
# cluster_deltas_path <- file.path(analysis_tables_dir, "cluster_deltas.csv")




cluster_deltas_required_cols <- c(
  "AEZ",
  "cluster_id",
  "year_t1",
  "year_t2",
  "year_gap",
  "ov_t1",
  "ov_t2",
  "delta_ov",
  "n_sites_t1",
  "n_sites_t2",
  "delta_defor_ha",
  "delta_defor_ha_annualized"
)

# Create the regression directories in case they don't yet exist
for (dir_path in c(
  output_dir,
  tables_dir,
  models_dir,
  tmp_dir,
  charts_dir
)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}