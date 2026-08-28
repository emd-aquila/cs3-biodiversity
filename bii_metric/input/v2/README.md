# V2 pressure inputs

V2 is optional. It implements a more published-style PREDICTS BII model while
leaving V1 unchanged. It needs pressure data that are *not* in the raw PREDICTS
extract or in LUH2 state fractions.

## Required input 1: PREDICTS site pressures

Create `predicts_site_pressures.csv`, with one row per `(study_id, site_id)` in
the PREDICTS data. `study_id` is PREDICTS `SS`; `site_id` is `SSBS`.

| Column | Unit / definition |
| --- | --- |
| `study_id`, `site_id` | PREDICTS keys. |
| `human_population_density` | People per km², sampled for the survey year or nearest available year. |
| `road_density_1km_km_per_km2` | Required only when `v2_include_roads <- TRUE`; road length (km) per km² in a 1 km neighbourhood. |
| `road_density_50km_km_per_km2` | Required only when `v2_include_roads <- TRUE`; road length (km) per km² in a 50 km neighbourhood. |
| `secondary_age_years` | Optional; required only when `v2_include_secondary_age <- TRUE`. Use `0` for non-secondary sites. |

## Required input 2: projection pressures

Create `landsat_spatial_pressures.csv` and/or `eppa_spatial_pressures.csv`.
Each has one row per `(scenario, cell_id, year)` matching the land-use input,
and human population density (plus the road columns only when roads are enabled). V2 joins these values
to the intensity-resolved land-use fractions before projecting BII.
When secondary age is enabled, provide the age of secondary vegetation in that
cell and use `0` where it is absent; V2 applies the age term only to secondary
land-use shares.

## Recommended data sources and workflow

1. Derive land-use intensity fractions using the published PREDICTS strategy:
   reclassify a Global Land Systems intensity reference map, then model each
   land-use/intensity share from land-use extent, population density, UN
   subregion and interactions. Apply that fitted allocator to annual land use.
   The exact Newbold et al. (2015) class conversion is versioned in
   `../crosswalks/global_land_systems_to_predicts_intensity.csv`. Do not infer
   intensity directly from a LUH2 class label.
2. Use a historical population grid such as GPWv4 or WorldPop to extract the
   PREDICTS site covariate; use a compatible SSP population grid for future
   EPPA/LUH-style scenarios.
3. Optionally calculate static 1 km and 50 km road density from a globally consistent
   road vector such as gROADS or GRIP. A time-varying road scenario can replace
   the static map when one is defensible, but should use the same units.
4. Keep plantation forest separate where a management/plantation layer allows
   it. If that is impossible, document whether it is allocated to secondary
   vegetation; do not silently call it primary forest.
5. For a strict match to the published 1970-2050 PREDICTS scenario framework,
   calculate *fractional* young (<30 years), intermediate (30-50 years), and
   mature (>50 years) secondary vegetation from LUH2 transition histories.
   Treat LUH2 `secma` only as a mean-age diagnostic, not a substitute for those
   shares.

V2 applies `log1p` to human population density and cube-root transformation to
road density, then centers and scales each predictor using the PREDICTS
training data. Projection values outside the observed transformed range are
capped, rather than extrapolated. The generated
`bii_v2_predictor_scaling.csv` records the fitted bounds.

The abundance model also calculates a study-level mean population-density
control from the supplied site data, following the published treatment of
spatial sampling bias. It is a fitting control only: gridded projections are
set to the training-centred value rather than treating it as a landscape
pressure.

V2 does not yet include the environmental covariates used in the published
compositional-similarity model or its full selection/bootstrap workflow.
Those additions need a reviewed site-level environmental extraction and should
be implemented as a further opt-in extension, not silently assumed here.

For comparison with the published global 1970–2050 PREDICTS projection, roads
are off by default: that projection omitted roads because comparable historical
and future road grids were unavailable. Set `v2_include_roads <- TRUE` only
for a road-informed sensitivity analysis with complete site and grid inputs.

## Switch

```sh
CS3_BII_VERSION=v2 Rscript bii_metric/run_bii_analysis.R
```

Without `CS3_BII_VERSION=v2`, the workflow stays on V1. V2 outputs use their
own `bii_v2_*` names and never overwrite V1 results.
