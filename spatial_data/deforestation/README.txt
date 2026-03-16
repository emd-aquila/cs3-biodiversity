Dataset: Deforestation within 50 km tiles (2001–2024)

Format:
ESRI Shapefile + CSV attribute dataset

Files included:

Spatial data (geometry):
- deforestation_50km.shp  (geometry)
- deforestation_50km.shx  (shape index)
- deforestation_50km.dbf  (attribute table)
- deforestation_50km.prj  (coordinate reference system)
- deforestation_50km.cpg  (character encoding)

Tabular data (attributes):
- deforestation_tile_attributes_SPAM2020_2001_2024.csv

Description:
This dataset contains spatial tiles representing 50 km analysis units
used to measure and analyze deforestation patterns between 2001 and 2024.
The shapefile defines the spatial geometry of each tile, while the CSV
file contains annual deforestation estimates (in hectares) disaggregated
by end-use sector.

The CSV dataset includes sector-specific and total deforestation values
derived using SPAM2020 classifications and is linked to the spatial
dataset via a shared tile identifier.

Data linkage:
Each record in the CSV corresponds to a spatial tile in the shapefile.
Datasets can be joined using:

    id  (or equivalent unique identifier field)

Users should perform an attribute join between the shapefile and CSV
using this identifier to associate deforestation statistics with
spatial geometries.

Temporal coverage:
2001–2024 (annual observations)

Units:
Deforested area measured in hectares (ha).

Coordinate Reference System (CRS):

Source data:
- SPAM2020 agricultural classification data

Processing performed:
- Spatial tiles generated at 50 km resolution
- Annual deforestation aggregated to tile level
- Sectoral allocation based on SPAM2020 classifications
- Tabular outputs exported to CSV format
- Dataset prepared for spatial analysis in the cs3-biodiversity project

Date created:
2026-02-26

Date accessed (source data):
YYYY-MM-DD

Prepared by:
Eli Duggan, MIT, CS3

Project:
cs3-biodiversity

Notes:
All shapefile components must remain together in the same directory
for proper functionality. The CSV file must be joined to the shapefile
using the tile identifier to reproduce analytical results.

Recommended workflow:
1. Load shapefile into GIS or analysis software.
2. Import CSV file.
3. Join CSV to shapefile using tile_id.
4. Perform spatial analysis or visualization.

Dataset archived at:
https://doi.org/10.5281/zenodo.18795067