# =====================================================
# Load raw input data (deforestation and clusters)
# read source files and perform minimal cleaning
# =====================================================

# -----------------------
# Read raw inputs
# -----------------------

defor_spatial_raw <- read_sf(defor_spatial_file) %>% 
  st_make_valid() %>% 
  mutate(id = trimws(as.character(id)))

defor_tabular_raw <- read_csv(defor_tabular_file, show_col_types = FALSE) %>% 
  mutate(id = trimws(as.character(id)))

cluster_raw <- read_csv(cluster_file_path, show_col_types = FALSE)

# -----------------------
# Validate required columns
# -----------------------

assert_has_cols(
  defor_spatial_raw,
  c("id", "geometry"),
  "defor_spatial_raw"
)

assert_has_cols(
  defor_tabular_raw,
  c("id", "lossyear_mode", "total_deforested_ha"),
  "defor_tabular_raw"
)

assert_has_cols(
  cluster_raw,
  c(
    "sample_id",
    "AEZ",
    "cluster_id",
    "Latitude",
    "Longitude",
    "year",
    "ov_score"
  ),
  "cluster_raw"
)

# -----------------------
# Make sure files are loaded
# -----------------------

message("Loaded raw inputs:")
message("  defor_spatial_raw rows: ", nrow(defor_spatial_raw))
message("  defor_tabular_raw rows: ", nrow(defor_tabular_raw))
message("  cluster_raw rows: ", nrow(cluster_raw))