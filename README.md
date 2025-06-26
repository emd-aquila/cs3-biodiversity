
# cs3‑biodiversity

## Overview
A toolkit for biodiversity data processing, focused on integrating species records, spatial analysis, and interactive dashboards.

## Architecture
- **Processing Pipeline**:
  1. **Data Cleaning & Validation**  
     - `BioTIME_data_processing.Rmd`: standardizes scientific names of the BioTIME input data.
  2. **Taxonomic Assignment**  
     - `BioTIME_taxonomic_assignment.Rmd`: Queries taxonomic databases to provide taxonomic classification for the unique species identified by 'BioTIME_data_processing.Rmd'.
  3. **Merge assignment files**  
     - `BioTIME_taxonomic_merge.Rmd`: Brings together the classification.rds files and outputs a single .rds file of unique species with their taxonomic information. 

## Project Structure
```
cs3-biodiversity/
├── biotime_pipeline/                 # BioTIME-specific processing pipeline
│   ├── BioTIME_data_processing.Rmd      # Main data cleaning and formatting
│   ├── BioTIME_taxonomic_assignment.ipynb # Automated taxonomic matching
│   ├── BioTIME_taxonomic_merge.Rmd       # Taxonomy harmonization and final merge
│   ├── BioTIME_build_plant_lookup.Rmd    # Trait lookup preparation (e.g., TRY)
│   └── input_files/                  # Raw input data and lookup resources
│       ├── biotime_v2_query_15April25.rds  # Original BioTIME data
│       ├── bird_codes.rds                  # Bird 4/6 letter alpha codes corresponding to species names
│       ├── plant_codes.rds                 # Plant BONAP-style codes
│       └── taxon.rds                       # Taxonomy database downloaded from NCBI to create BONAP-style codes
├── README.md                         
└── LICENSE
```

## Contributions
Contributions and collaborations are welcome.

## License
MIT License. See LICENSE for more information.