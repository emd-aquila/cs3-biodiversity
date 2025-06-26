# CS3 Biodiversity

This repository contains workflows and scripts developed at the **MIT Center for Sustainability Science and Strategy (CS3)** to support global biodiversity synthesis. It focuses on processing and integrating biodiversity time series data, including taxonomic resolution, trait enrichment, and metadata harmonization.

This work is part of a Master's research project conducted within CS3.

## Overview

The project enables:

- Standardization of biodiversity datasets (e.g., BioTIME)
- Taxonomic name resolution and harmonization
- Integration of ecological traits and study-level metadata
- Scalable analysis of biodiversity change over time

The repository is modular, reproducible, and designed for collaborative use across synthesis efforts.

## Repository Structure

```
cs3-biodiversity/
├── biotime/                  # BioTIME data cleaning, taxonomy, metadata integration
│   └── README.md             # Details for the BioTIME pipeline
├── predicts/                # [planned] Workflows for PREDICTS database
├── traits/                  # [planned] Scripts for trait mapping and lookup
├── workflows/               # [planned] End-to-end workflow automation
├── data/                    # Raw and processed reference files (partial tracking)
└── README.md                # This file
```

## Tools & Dependencies

- Languages: R, Python  
- Key packages: `tidyverse`, `taxize`, `pandas`, `openpyxl`, `jupyter`  
- Data sources: BioTIME, IBP-AOS bird list, plant trait databases (e.g., TRY, BIEN)

## Getting Started

1. Clone the repository:  
   `git clone https://github.com/emd-aquila/cs3-biodiversity.git`
2. Navigate into a dataset-specific folder (e.g., `biotime/`)
3. Follow the instructions in that folder’s `README.md` to run the workflow
4. Environment and dependency setup will be included in future updates

## Project Status

- BioTIME data processing implemented (R + Python)
- Trait integration and additional datasets planned
- Pipeline automation under development

## Contributions

Contributions and collaborations are welcome. Please open an issue or submit a pull request to get involved.

## License

MIT License. See [LICENSE](LICENSE) for more information.
