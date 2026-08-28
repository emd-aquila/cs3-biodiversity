# BioTIME-Only Biodiversity And Hansen Deforestation Workflow

This folder contains a BioTIME-only alternative to the clustered PREDICTS/BioTIME
pipeline. It uses raw BioTIME inputs, BioTIMEr assemblage construction, the AEZ
shapefile, the family-level phylogenetic tree and taxonomy lookups used by the
original OV workflow, and Hansen Global Forest Change via the same Earth Engine
summary pattern used elsewhere in this repository.

## Design Notes

- Time series are defined by BioTIMEr `assemblageID`, then exposed downstream as
  `time_series_id = assemblage_id`.
- BioTIME records are filtered to terrestrial, abundance-based studies, years
  from 2000 onward, valid coordinates, finite abundance values, and usable
  taxonomic names. Zero-abundance rows are retained through BioTIMEr gridding;
  positive abundance is used inside diversity and PD calculations.
- BioTIMEr gridding and deterministic sample-event rarefaction create comparable
  assemblage-year samples.
- The primary `OV` is the original composite OV:
  `scaled Shannon + scaled log(mean species abundance + 1) + scaled
  phylogenetic diversity`.
- Time series must have at least two retained post-2000 composite OV sample
  years.
- AEZ is assigned by point-in-polygon; points outside AEZ polygons are assigned
  to the nearest AEZ and flagged.
- Deforestation exposure is based on a 1 km buffer around each time-series point.
  Hansen loss is accumulated for years after `year_t1` through `year_t2`,
  and the current regression uses cumulative interval deforestation rather than
  annualized deforestation.
- The mixed-effect models use `lme4` random intercepts and random slopes. The
  GAM/spline models use `mgcv`.
- Regressions and figures are produced for both `all_delta_ov` and
  `nonpositive_delta_ov`, where the latter filters out positive OV changes.
- Figures include R^2 annotations. Mixed-effect facet labels use prediction-based
  R^2 by facet; GAM facet labels use the fitted GAM R^2, with linear fallback
  where a GAM cannot be fit.
- AEZ facets are ordered numerically by AEZ label.

Projection-ready predictors should be direct EPPA outputs or deterministic
transforms/crosswalks from EPPA outputs. Candidate next predictors include EPPA
land-cover or land-use area shares, changes in those shares, managed/unmanaged
forest share, cropland and pasture expansion, harvested/secondary/regrowth forest
states if available, and cumulative or lagged land-use change. BioTIME-only
sampling controls can be used diagnostically, but they are not projection
predictors unless they can be supplied for EPPA scenarios.

## Run Order

```sh
Rscript 11_biotime_only/install_dependencies.R
Rscript 11_biotime_only/data_cleaning/code/run_data_cleaning.R
Rscript 11_biotime_only/deforestation_matching/code/run_deforestation_matching.R
Rscript 11_biotime_only/regression/code/run_regression.R
Rscript 11_biotime_only/visualization/code/run_visualization.R
```

The deforestation step writes
`11_biotime_only/deforestation_matching/output/hansen_gee/biotime_timeseries_1km_buffers.geojson`
and then calls the Earth Engine export script to create
`hansen_biotime_site_year_defor.csv`. If Earth Engine is not authenticated,
run the printed command after authenticating, then rerun the deforestation step.
This sends BioTIME-derived coordinates and 1 km buffer geometries to Google
Earth Engine so Hansen pixels can be summarized over those buffers.

To force a new Earth Engine export after changing the BioTIME buffer set:

```sh
EARTHENGINE_PROJECT=biodiversity-deforestation-cs3 \
BIOTIME_ONLY_FORCE_HANSEN_EXPORT=true \
Rscript 11_biotime_only/deforestation_matching/code/run_deforestation_matching.R
```
