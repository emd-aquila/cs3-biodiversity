
# 🧬 cs3‑biodiversity

## 🔍 Overview
A toolkit for biodiversity data processing, focused on integrating species records, spatial analysis, and interactive dashboards.

## 🧱 Architecture
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

## ⚙️ Installation & Usage
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

## 📁 Project Structure
```
cs3-biodiversity/
├── data/                     # Sample data
├── scripts/
│   ├── clean_data.py
│   ├── spa_analysis.py
│   ├── stats_summary.py
│   └── dashboard_builder.py
├── main.py                   # Entry point & orchestration
├── requirements.txt
└── results/                  # Pipeline outputs: CSV, JSON, Dashboard assets
```

## 🧪 Examples & Tests
- Use `data/sample_dataset.zip` to run a demo pipeline end‑to‑end.
- Automated validation in `tests/`—run:
  ```bash
  pytest -q
  ```

## 🚀 Roadmap
- ✅ Core cleaning, spatial, and stats modules  
- ✅ Automated dashboard generation  
- 🔜 Add trend-analysis scripts  
- 🔜 Integrate remote-access backend (e.g., AWS S3, cloud dashboard deployment)  
- 🔜 Enhance multi-region support

## ✨ Contribution & Style Guide
- Follow `scripts/style-guide.md` (flask-black-precommit).
- Submit PRs for new features or bug fixes.
- Use issue templates and unit tests for new modules.
