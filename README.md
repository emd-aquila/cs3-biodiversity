
# BioTIME Data Processing Pipeline

BioTIME data processing pipeline to filter unusable data, assign genus/species names to bird/plant code entries, and assign taxonomic class.
This prepares the data to be merged with the PREDICTS database for clustering (June 2025).

## Overview

This pipeline performs:
- Species name resolution
- Bird code decoding
- BONAP-style plant code decoding
- Taxonomic class assignment
- Final data merging for analysis

## Folder Structure

biotime_pipeline/
├── data/
│   ├── biotime_v2_query_15April25.rds
│   ├── IBP-AOS-LIST24.csv
│   ├── backbone.csv  <-- GBIF backbone (large file, not version-controlled)
│   ├── plant_code_lookup.csv
│   ├── filtered_resolved_biotimes.csv
│   ├── unique_genus_species_biotime.csv
│   ├── class_assigned.csv
│   ├── unclassified_species.csv
│   └── filtered_resolved_biotimes_with_class.csv
│
├── cache/
│   └── classification_cache.rds
│
└── scripts/
    ├── BioTIME_build_plant_lookup.Rmd <-- only need to run once
    ├── BioTIME_data_processing.Rmd
    ├── BioTIME_taxonomic_classification.Rmd
    └── BioTIME_class_merge.Rmd

## Dependencies

The scripts automatically install required R packages.

## Full Pipeline Run Order

Rscript -e "rmarkdown::render('scripts/BioTIME_build_plant_lookup.Rmd')"
Rscript -e "rmarkdown::render('scripts/BioTIME_data_processing.Rmd')"
Rscript -e "rmarkdown::render('scripts/BioTIME_taxonomic_classification.Rmd')"
Rscript -e "rmarkdown::render('scripts/BioTIME_class_merge.Rmd')"

## Notes

- `backbone.csv` (~600MB) is not version-controlled.
- Only `plant_code_lookup.csv` is version-controlled.
