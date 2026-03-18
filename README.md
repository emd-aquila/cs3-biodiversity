# cs3-biodiversity

A research-oriented R project for building biodiversity indicators, clustering study sites, and linking those sites to deforestation patterns and agro-ecological zones (AEZs).

## Overview

This repository contains three connected analysis pipelines:

1. **`predicts_ov_table/`** computes biodiversity metrics and produces OV/AEZ-tagged site outputs.
2. **`cluster/`** clusters biodiversity sampling sites across multiple methods and radii, then summarizes OV spread at cluster and AEZ levels.
3. **`deforestation_tile_tag/`** builds canonical deforestation and cluster spatial/tabular outputs, then analyzes transitions, buffer sensitivity, and AEZ summaries.

Supporting geospatial inputs live in **`spatial_data/`**, including AEZ boundaries and deforestation shapefiles.

## Repository structure

```text
cs3-biodiversity/
├── cs3-biodiversity.Rproj              # RStudio project file
├── README.md                           # Project overview and repository guide
├── LICENSE                             # MIT license
├── cluster/                            # Site clustering pipeline
│   ├── run_cluster.R                   # Entry point for clustering workflow
│   └── code/                           # Clustering scripts and notebook
├── deforestation_tile_tag/             # Deforestation tagging build + analysis pipeline
│   ├── run_tag.R                       # Entry point for full tagging workflow
│   ├── build/code/                     # Canonical build scripts and outputs prep
│   └── analysis/code/                  # Analysis, summaries, and figure generation
├── predicts_ov_table/                  # OV calculation and biodiversity metrics pipeline
│   ├── build/code/                     # PREDICTS import/preparation notebooks
│   └── analysis/code/                  # OV calculation notebook
└── spatial_data/                       # AEZ and deforestation spatial reference data
    ├── aez/
    └── deforestation/
```

## Typical workflow

- Run the **PREDICTS/OV** workflow to generate biodiversity-derived outputs.
- Run the **clustering** workflow to assign cluster IDs and derive AEZ-level summaries.
- Run the **deforestation tagging** workflow to combine cluster footprints with deforestation layers and generate analysis-ready outputs and figures.

## Requirements

This repository is primarily written in **R** and uses script- and notebook-based workflows. The code references packages such as `tidyverse`, `sf`, `ape`, `picante`, and other analysis helpers, so an R environment with those dependencies installed is required.

## License

This project is licensed under the **MIT License**. See [LICENSE](LICENSE) for the full license text.
