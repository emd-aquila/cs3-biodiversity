library(sf)
library(dplyr)

aez_dir <- "aez_outputs"

aez_paths <- file.path(aez_dir, paste0("AEZ_", 1:18, ".gpkg"))
names(aez_paths) <- paste0("AEZ_", 1:18)

# --- Read, combine, write whole geopackage -----------------
# st_layers() on the first file to confirm layer names
st_layers(aez_paths[["AEZ_1"]])

aez_list <- lapply(seq_along(aez_paths), function(i) {
  path  <- aez_paths[[i]]
  layer <- paste0("AEZ_", i)          # layer name matches file name
  message("Reading ", layer, " ...")
  st_read(path, layer = layer, quiet = TRUE)
})

# Row-bind all AEZs into a single sf object
all_aez <- do.call(rbind, aez_list)

# remove leading X in column names
names(all_aez) <- gsub("^X", "", names(all_aez))

# Mutate to categorize Canopy Height label as low (<5m), medium (5-25m), high (25m+)
all_aez <- all_aez %>% 
  mutate(AEZ = as.integer(AEZ),
         CH_label = case_when(
           CH_mean < 5  ~ "low",
           CH_mean < 25 ~ "medium",
           TRUE         ~ "high"
         ))

# coerce AEZ column to integer for easier filtering/sorting, then reorder
all_aez <- all_aez[, c("cell_id", "AEZ", "CH_mean", "CH_label", setdiff(names(all_aez), c("cell_id", "AEZ", "CH_mean", "CH_label")))]

# Write back to a single combined GeoPackage
st_write(all_aez, "canopy_height_combined.gpkg", layer = "all_aez", delete_layer = TRUE)

# --- Create separate geopackages by year -----------------
by_year_dir <- "by_year"
dir.create(by_year_dir)

for (yr in 2014:2023) {
  all_aez %>%
    dplyr::select(AEZ, cell_id, CH_mean, CH_label, starts_with(as.character(yr))) %>%
    st_write(paste0("by_year/ch_deforestation_", yr, ".gpkg"), delete_layer = TRUE)
}