# =====================================================
# Load raw input data (deforestation only)
# read source files and perform minimal cleaning
# =====================================================

# -----------------------
# Read raw deforestation inputs
# -----------------------

defor_spatial_raw <- read_sf(defor_spatial_file) %>% 
  st_make_valid() %>% 
  mutate(id = trimws(as.character(id)))

defor_tabular_raw <- read_csv(defor_tabular_file, show_col_types = FALSE) %>% 
  mutate(id = trimws(as.character(id)))

country_boundaries_raw <- read_sf(country_boundaries_file) %>%
  st_make_valid()

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
  c("id", "lossyear_mode", 
    "total_deforested_ha", 
    "total_deforested_ha_crops", 
    "total_deforested_ha_livestock"
    ),
  "defor_tabular_raw"
)

assert_has_cols(
  country_boundaries_raw,
  c("ADMIN", "ADM0_A3", "ISO_A3", "SOVEREIGNT", "geometry"),
  "country_boundaries_raw"
)

message("Loaded raw deforestation inputs:")
message("  defor_spatial_raw rows: ", nrow(defor_spatial_raw))
message("  defor_tabular_raw rows: ", nrow(defor_tabular_raw))
message("  country_boundaries_raw rows: ", nrow(country_boundaries_raw))
