
# cs3‑biodiversity

## Overview
A toolkit for biodiversity data processing, focused on integrating species records, spatial analysis, and interactive dashboards.

## Architecture
- **Data Input**: Accepts CSVs, GeoJSON, and XLSX—ingests species observations, conservation status, and location metadata.
- **Processing Pipeline**:
  1. **Data Cleaning & Validation**  
     - `clean_data.py`: standardizes scientific names, dates, coordinate formats.
     - `validate_schema.py`: validates required fields via JSON schema and flags anomalies.
  2. **Spatial Analysis**  
     - `spa_analysis.py`: calculates species richness, hotspot identification, and range overlap using GeoPandas.
     - `env_overlap.py`: quantifies biodiversity overlap with protected areas / land use maps.
  3. **Statistical Summary**  
     - `stats_summary.py`: produces breakdowns (species by status, region, trends) and exports PDF/CSV.
  4. **Dashboard Generation**  
     - `dashboard_builder.py`: builds an interactive Plotly Dash app showing species distribution maps, charts, and filters.
- **Pipeline Orchestration**  
  Use `main.py` or `Makefile` to run steps sequentially or in isolation with flags (e.g. `--no-dashboard`).

## Installation & Usage
1. **Clone & install**:
    ```bash
    git clone https://github.com/emd-aquila/cs3-biodiversity.git
    cd cs3-biodiversity
    pip install -r requirements.txt
    ```

2. **Run full pipeline**:
    ```bash
    python main.py \
      --input observations.csv \
      --regions shapefile/regions.geojson \
      --build-dashboard \
      --output results/
    ```

3. **Run individual steps**:
    - `python clean_data.py --input raw.csv --out cleaned.csv`
    - `python spa_analysis.py --occurrences cleaned.csv --regions regions.geojson`
    - `python dashboard_builder.py --stats stats.csv --geo results.geojson --port 8050`

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

## Examples & Tests
- Use `data/sample_dataset.zip` to run a demo pipeline end‑to‑end.
- Automated validation in `tests/`—run:
  ```bash
  pytest -q
  ```

## Roadmap
- Core cleaning, spatial, and stats modules  
- Automated dashboard generation  

## Contributions
Contributions and collaborations are welcome.

## License
MIT License. See LICENSE for more information.