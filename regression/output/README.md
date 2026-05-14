# Regression Output README

Generated: 2026-05-08 11:45:26 EDT
Output root: /Users/eliduggan/Projects/cs3-biodiversity/regression/output

This directory contains regression outputs for the options selected in `regression/code/01_config.R` at run time.
Folder levels are only created for hierarchy axes with more than one selected option. Fixed single-option choices are recorded below instead of adding one-option folders.

## Folder Hierarchy Created For This Run
1. Cluster buffer size (`buffer`): `buf_1km`, `buf_10km`
2. Grouping used for group-specific regressions (`regression_group`): `by_aez`, `by_country`
3. Deforestation exposure window (`defor_bin`): `baseline_deforestation`, `lagged_deforestation`
4. How delta OV is calculated within clusters (`delta_ov_approach`): `year_pair_delta_ov`, `whole_cluster_delta_ov`
5. OV calculation method (`ov_calculation_method`): `ov_full`, `ov_obs_only`
6. How low starting OV and percent-change outcomes are handled (`ov_change_mode`): `linear_delta_ov`, `thresholded_linear_delta_ov`, `percent_delta_ov`
7. How tagged-tile deforestation is summarized for each cluster (`defor_tile_sum`): `total_tagged_tile_deforestation`, `mean_tagged_tile_deforestation`, `overlap_weighted_deforestation`
8. Deforestation regressor transformation (`defor_transform`): `defor_raw`, `defor_log1p`

## Fixed Choices Saved Only In This README
- Clustering method (`cluster_methods`): clara [no folder level]
- Clustering radius (`cluster_radii`): 10.0 km [no folder level]
- Whether delta OV and deforestation are raw totals or annualized by year_gap (`annualization_modes`): annualized_delta [no folder level]
- Whether same-single-tile clusters are collapsed (`single_tile_collapse_modes`): collapse_single_tile_clusters [no folder level]
- Regression model family (`regression_models`): ols [no folder level]

## Selected Options
- Clustering method (`cluster_methods`): clara -> folders `clara`
- Clustering radius (`cluster_radii`): 10.0 km -> folders `radius_10.0km`
- Cluster buffer size (`buffers`): 1 km, 10 km -> folders `buf_1km`, `buf_10km`
- Grouping used for group-specific regressions (`regression_groups`): by_aez, by_country -> folders `by_aez`, `by_country`
- Whether delta OV and deforestation are raw totals or annualized by year_gap (`annualization_modes`): annualized_delta -> folders `annualized_delta`
- Deforestation exposure window (`defor_bins`): baseline_deforestation, lagged_deforestation -> folders `baseline_deforestation`, `lagged_deforestation`
- How delta OV is calculated within clusters (`delta_ov_approaches`): year_pair_delta_ov, whole_cluster_delta_ov -> folders `year_pair_delta_ov`, `whole_cluster_delta_ov`
- Whether same-single-tile clusters are collapsed (`single_tile_collapse_modes`): collapse_single_tile_clusters -> folders `collapse_single_tile_clusters`
- OV calculation method (`ov_calculation_methods`): ov_full, ov_obs_only -> folders `ov_full`, `ov_obs_only`
- How low starting OV and percent-change outcomes are handled (`ov_change_modes`): linear_delta_ov, thresholded_linear_delta_ov, percent_delta_ov -> folders `linear_delta_ov`, `thresholded_linear_delta_ov`, `percent_delta_ov`
- How tagged-tile deforestation is summarized for each cluster (`defor_tile_sum_methods`): total_tagged_tile_deforestation, mean_tagged_tile_deforestation, overlap_weighted_deforestation -> folders `total_tagged_tile_deforestation`, `mean_tagged_tile_deforestation`, `overlap_weighted_deforestation`
- Regression model family (`regression_models`): ols -> folders `ols`
- Deforestation regressor transformation (`defor_transforms`): defor_raw, defor_log1p -> folders `defor_raw`, `defor_log1p`

## Starting OV Thresholds
- ov_full: 1.5
- ov_obs_only: 0.25
- default: 1.5

## Output Toggles
- Model tables: FALSE
- Regression OLS plots: TRUE
- Histogram plots: FALSE
- Master output report: TRUE

## Notes
- `raw_delta` uses raw delta OV and raw deforestation totals.
- `annualized_delta` divides delta OV and deforestation by `year_gap`.
- `baseline_deforestation` uses all deforestation between the first and last delta-OV years, inclusive.
- `lagged_deforestation` uses the lagged deforestation columns produced by the upstream deforestation-tagging workflow.
- Thresholded OV-change modes remove rows where starting OV is below the configured threshold for that OV calculation method.
- Percent OV-change modes replace linear delta OV with `100 * delta_ov / starting_ov` after the selected raw or annualized delta is chosen.
