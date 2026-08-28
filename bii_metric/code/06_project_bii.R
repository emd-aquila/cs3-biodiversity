# Project every fitted PREDICTS BII response function onto prepared Landsat
# fractions, producing global, regional, country, and taxon-specific outputs.
assert_file_exists(landsat_prepared_path, "prepared Landsat BII land use")
assert_file_exists(response_table_path, "BII response table")
landsat_landuse <- readRDS(landsat_prepared_path)
landsat_projection <- project_bii_landuse(landsat_landuse, "landsat")
