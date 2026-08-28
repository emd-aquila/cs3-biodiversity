# LPD--LUH2 land-use transition analysis

This workflow is separate from `lpd_hansen/`; it does not overwrite Hansen
tables or figures. It samples the LUH2 0.25-degree grid at each native
terrestrial LPD population coordinate, aggregates LUH2 states to the broad
PREDICTS-compatible classes, and links start/end shares to adjacent population
observations from 2000--2020.

The five classes are primary (`primf + primn`), secondary (`secdf + secdn`),
cropland (all LUH2 crop states), pasture (`pastr + range`), and urban. The
fractional-change regression estimates associations per 10 percentage-point
increase in every non-primary class, relative to primary vegetation.

## Run

```sh
Rscript lpd_luh2/run_lpd_luh2.R prepare
bii_metric/tmp/luh2_venv/bin/python lpd_luh2/code/extract_luh2_population_states.py \
  --intervals lpd_luh2/output/tables/lpd_native_terrestrial_intervals_2000_2020.csv \
  --output lpd_luh2/output/luh2/luh2_predicts_landuse_by_location_year.csv
Rscript lpd_luh2/run_lpd_luh2.R analyse
```

The extractor uses historical LUH2 v2h through 2015 and the bundled
SSP2-RCP4.5 MESSAGE-GLOBIOM future state file for 2016--2020. Results spanning
2015 are therefore explicitly tagged as historical-to-scenario transitions;
they are not an observed post-2015 land-use history.

`dominant_landuse_transition_matrix.csv` is the categorical matrix of start
and end dominant classes. `fractional_landuse_regression_coefficients.csv` is
the more informative compositional analysis, because LUH2 cells contain land
use fractions and can change without their dominant category changing.
