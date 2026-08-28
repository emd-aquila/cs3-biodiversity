# Data Exploration

Standalone exploratory analyses live here instead of inside numbered pipeline
steps. Each topic has its own `code/` and `output/` folders so exploration code
and products stay together without becoming part of the main production
pipeline.

Current topics:

- `site_maps/`: maps PREDICTS, BioTIME, and combined OV sites on country
  boundaries and writes AEZ site-count tables.
- `meeting_tables/`: meeting-ready summary tables of filtered entries, sites,
  and entries per AEZ across PREDICTS, BioTIME, and combined.
- `ov_vs_year_by_aez/`: sample-level OV-vs-year plots by individual AEZ and
  broad tropical/temperate/cold AEZ group.
- `ov_over_time/`: migrated OV-over-time regression exploration outputs.
- `brazil_exploration/`: migrated Brazil-focused regression exploration
  outputs.
- `starting_ov_vs_delta_ov/`: migrated starting-OV diagnostic outputs.
- `whole_cluster_delta_ov_maps/`: migrated whole-cluster delta-OV map outputs.
