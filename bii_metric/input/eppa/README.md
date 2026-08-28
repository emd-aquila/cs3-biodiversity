# EPPA land-use input

Create `eppa_landuse_long.csv` here. Required columns are `scenario`, `year`,
`region`, `source_class`, and `area_km2`. One row represents the area of one
EPPA land-use sector in one scenario/year/region. The adapter creates shares
and an `EPPA:<region>` projection cell before applying the crosswalk.
