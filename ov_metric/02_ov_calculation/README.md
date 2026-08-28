# OV Calculation

This stage is split into two sequential substeps.

1. `build/`: reads the OV-ready databases from `01_biodiversity_data_integration/output`, builds family-level community matrices, compares observed family names with `build/input/iphylo_tree.nwk`, and writes `*_pd_result.csv` files to `build/output`.
2. `calculation/`: reads the same databases plus the build `pd_result` files, calculates Shannon, MSA, and observation-only OV scores, assigns AEZ labels, and writes final `*_ov_scores.csv` handoff tables to `calculation/output`.

Run the full stage from this folder:

```sh
Rscript run_ov_calculation.R
```

Or run the steps separately:

```sh
cd build/code
Rscript run_build.R

cd ../../calculation/code
Rscript run_calculation.R
```
