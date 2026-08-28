# Biodiversity Data Integration

Sequential scripts for processing already-downloaded PREDICTS and BioTIME
source tables for later OV calculation and database integration. Raw downloads
are prepared first in `00_biodiversity_data`.

Run from the `01_biodiversity_data_integration` folder:

```sh
cd 01_biodiversity_data_integration
Rscript run_integration.R
```

Script sequence:

- `01_config.R`: shared paths, filters, raw data locations, and lookup-table
  locations.
- `03_process_predicts.R`: combines the manually downloaded reference tables,
  filters to abundance records sampled from 2000-01-01 through 2024-12-31, and
  writes full data/metadata/reference products plus an OV-ready handoff table.
- `04_process_biotime.R`: filters terrestrial abundance observations from
  2000-2024, rebuilds cached taxonomy assignments using GBIF, bird, and plant
  lookup tables, applies BioTIMEr gridding plus sample-event rarefaction, and
  writes BioTIME info products.
- `05_write_combined_outputs.R`: writes the individual and combined OV-ready
  PREDICTS/BioTIME CSV and RDS handoff tables for `02_ov_calculation`. Before
  writing the combined handoff, it screens retained PREDICTS and BioTIME
  references by DOI and removes overlapping BioTIME studies from the combined
  table only.

Folder roles:

- `../00_biodiversity_data/predicts/`: raw downloaded PREDICTS source files.
- `../00_biodiversity_data/biotime/`: raw downloaded BioTIME source files.
- `predicts_info/`: full filtered PREDICTS data, metadata, and references as
  CSV and RDS.
- `biotime_info/`: full filtered BioTIME data, metadata, and references as CSV
  and RDS, plus `biotime_filter_summary.csv`.
- `.tmp/`: internal diagnostics and taxonomy cache/resolution artifacts.
- `overlap_info/`: DOI-based PREDICTS/BioTIME overlap screen and BioTIME
  exclusions used for the combined handoff.
- `output/`: downstream handoff files for the next pipeline stage.
