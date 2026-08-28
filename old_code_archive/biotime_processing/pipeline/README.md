# Structured BioTIME Pipeline

This folder is the organized, sequential replacement path for the exploratory
BioTIME notebooks. The files under `biotime_processing/old/` and
`biotime_processing/novel_testing/` remain untouched for reference.

## Layout

- `code/`: sequential R scripts and the pipeline runner.
- `input/`: small, versionable trial fixtures. Large canonical downloads remain
  in `../novel_testing/data/` to avoid duplicating gigabytes of data.
- `output/`: generated filtered BioTIME outputs and overlap diagnostics.
- `tmp/`: replaceable caches.

## Run

From this folder:

```sh
cd code
Rscript run_pipeline.R
```

The scripts run in this order:

1. `03_filter_predicts.R`: keeps abundance-based PREDICTS studies starting in
   2000 or later and writes a slim extract for OV calculation and geotagging.
2. `04_prepare_trial_predicts.R`: writes a 10-row PREDICTS fixture for trial work.
3. `05_filter_biotime.R`: keeps terrestrial, abundance-based BioTIME studies
   starting in 2000 or later and writes a compressed slim observation subset.
4. `06_compare_biotime_predicts.R`: compares eligible BioTIME and PREDICTS data
   at the study level using citation and spatiotemporal overlap signals.
5. `07_write_schema_inventory.R`: records the original fields, metadata fields,
   and slim retained fields in human- and machine-readable formats.

## Canonical Sources

- BioTIME 2.0 static files:
  <https://zenodo.org/records/15222193>
- BioTIME usage guidance:
  <https://biotime.st-andrews.ac.uk/usageGuidelines.php>
- BioTIMEr package:
  <https://biotimehub.github.io/BioTIMEr/>
- `predictsr` package documentation:
  <https://cran.r-project.org/package=predictsr>

The comparison is a screening tool. A citation or nearby spatiotemporal match
is a candidate for manual review, not proof that two database records are
duplicates.

See `comparison_method.md` for the full reasoning, limitations, and recommended
merge pilot.
