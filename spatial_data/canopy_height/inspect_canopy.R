library(sf)

gpkg_path <- "canopy_height.gpkg"
st_layers(gpkg_path)

data <- st_read(gpkg_path, layer = "dataset_cleaned")
print(names(data), max = 600)