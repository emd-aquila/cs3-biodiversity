# Configuration for the PREDICTS BII workflow. All paths are anchored at the
# repository root so this stage remains independent from ov_metric/.
bii_dir <- normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(bii_dir, ".."), winslash = "/", mustWork = TRUE)

input_dir <- file.path(bii_dir, "input")
output_dir <- file.path(bii_dir, "output")
tmp_dir <- file.path(bii_dir, "tmp")
for (path in c(input_dir, output_dir, tmp_dir)) dir.create(path, recursive = TRUE, showWarnings = FALSE)

# Source data already managed by the repository's shared data folders.
predicts_raw_path <- file.path(repo_root, "00_biodiversity_data", "predicts", "predicts_database_raw.rds")
published_bii_path <- file.path(
  repo_root, "00_biodiversity_data", "bii", "1970_to_2050_projections", "long_data.csv"
)

# User-supplied spatial land-use data. The README specifies this long format.
landsat_landuse_path <- file.path(input_dir, "landsat_landuse", "landsat_landuse_long.csv")
landsat_crosswalk_path <- file.path(input_dir, "crosswalks", "landsat_to_bii_crosswalk.csv")

# User-supplied EPPA scenario data and its explicit crosswalk.
eppa_landuse_path <- file.path(input_dir, "eppa", "eppa_landuse_long.csv")
eppa_crosswalk_path <- file.path(input_dir, "crosswalks", "eppa_to_bii_crosswalk.csv")
eppa_intensity_path <- file.path(input_dir, "crosswalks", "eppa_intensity_scenarios.csv")
eppa_intensity_scenario <- "central" # used only for model_variant = "land_use_intensity"

# LUH2 inputs used for the current historical/future demonstration.  LUH2 is
# read directly from NetCDF rather than expanded into a very large long CSV.
# The 2030 file is the SSP2-RCP4.5 MESSAGE-GLOBIOM pathway, selected as an
# explicit default for comparison with the bundled published BII series.
luh2_dir <- file.path(input_dir, "luh2")
luh2_historical_path <- file.path(luh2_dir, "raw", "luh2_v2h_states_850-2015.nc")
luh2_future_path <- file.path(luh2_dir, "raw", "luh2_v2f_ssp245_states_2015-2100.nc")
luh2_static_path <- file.path(luh2_dir, "raw", "luh2_v2h_static_quarterdeg.nc")
luh2_country_boundaries_path <- file.path(
  luh2_dir, "reference", "natural_earth_50m", "ne_50m_admin_0_countries.shp"
)
luh2_grid_lookup_path <- file.path(luh2_dir, "derived", "luh2_grid_country_lookup.csv")
luh2_scenario_2030 <- "ssp2rcp4p5messageglobiom"
# Default comparison years for the original NHM 1970--2050 benchmark.  The
# values can be overridden without changing V1 code, e.g.
# CS3_BII_LUH2_YEARS=2000,2005,2010,2015,2020.
luh2_projection_years <- Sys.getenv("CS3_BII_LUH2_YEARS", unset = "2010,2030")
luh2_output_prefix <- Sys.getenv("CS3_BII_LUH2_OUTPUT_PREFIX", unset = "bii_luh2")

# NHM v2.1.1 is a separate, raster-based published benchmark. Download the
# public ZIP manually when required and keep it outside source control.
nhm_v211_dir <- file.path(repo_root, "00_biodiversity_data", "bii", "v2_1_1")
nhm_v211_zip_path <- file.path(nhm_v211_dir, "raw", "bii-v2-1-1-nhm-data-portal.zip")
nhm_v211_extract_dir <- file.path(nhm_v211_dir, "extracted")
nhm_v211_years <- c(2000L, 2005L, 2010L, 2015L, 2020L)
nhm_v211_projection_prefix <- "bii_luh2_v211_years"

# Hill et al. (2018) LUH2/PREDICTS site-level re-curation. The rules are
# intentionally versioned and only classify sites when their retained PREDICTS
# metadata supplies evidence. Ambiguous sites are exported for review rather
# than assigned an invented LUH2 class.
hill2018_rules_path <- file.path(input_dir, "crosswalks", "hill2018_recuration_text_rules.csv")
hill2018_site_recuration_path <- file.path(tmp_dir, "predicts_hill2018_site_recuration.rds")
hill2018_site_recuration_csv_path <- file.path(output_dir, "predicts_hill2018_site_recuration.csv")
hill2018_recuration_coverage_path <- file.path(output_dir, "predicts_hill2018_recuration_coverage.csv")
hill2018_recuration_review_path <- file.path(output_dir, "predicts_hill2018_recuration_review_queue.csv")
hill2018_recuration_metadata_path <- file.path(output_dir, "predicts_hill2018_recuration_metadata.csv")

# -----------------------------------------------------------------------------
# V3: Hill (2018)-style re-curated PREDICTS/LUH2 workflow
# -----------------------------------------------------------------------------
# V3 is deliberately independent of V1 and V2.  It retains only the strict
# site re-curation, preserves the detailed LUH2 classes, and represents use
# intensity as the empirical class-by-intensity distribution in that subset.
# Secondary age is derived from annual LUH2 states using a documented net-change
# proxy unless an exact LUH2 transitions workflow is supplied later.
v3_reference_pressure_class <- "primary__minimal"
v3_model_bundle_path <- file.path(output_dir, "bii_v3_model_bundle.rds")
v3_predictor_spec_path <- file.path(output_dir, "bii_v3_predictor_scaling.csv")
v3_pairs_path <- file.path(tmp_dir, "predicts_v3_composition_pairs.rds")
v3_site_path <- file.path(tmp_dir, "predicts_v3_site_abundance.rds")
v3_composition_path <- file.path(tmp_dir, "predicts_v3_composition_records.rds")
v3_intensity_mixture_path <- file.path(output_dir, "bii_v3_intensity_mixture.csv")
v3_luh2_landuse_path <- file.path(tmp_dir, "luh2_v3_landuse_1deg.csv")
v3_luh2_pressure_path <- file.path(tmp_dir, "luh2_v3_spatial_pressures_1deg.csv")
v3_population_2010_path <- file.path(input_dir, "v2", "raw", "ssp2_2010.nc")
v3_population_2020_path <- file.path(input_dir, "v2", "raw", "ssp2_2020.nc")
v3_population_2030_path <- file.path(input_dir, "v2", "raw", "ssp2_2030.nc")
v3_projection_years <- c(2010L, 2020L, 2030L)
v3_minimum_sites_per_scope <- 200L
v3_minimum_pairs_per_scope <- 500L

# Outputs used between workflow stages.
predicts_site_path <- file.path(tmp_dir, "predicts_site_abundance.rds")
predicts_composition_path <- file.path(tmp_dir, "predicts_composition_records.rds")
composition_pairs_path <- file.path(tmp_dir, "predicts_composition_pairs_broad_taxa.rds")
model_bundle_path <- file.path(output_dir, "bii_model_bundle.rds")
response_table_path <- file.path(output_dir, "bii_response_table.csv")
landsat_prepared_path <- file.path(tmp_dir, "landsat_bii_landuse.rds")
eppa_prepared_path <- file.path(tmp_dir, "eppa_bii_landuse.rds")

# The default follows the accessible De Palma tutorial: minimally used primary
# vegetation is the reference, and secondary vegetation is one broad class.
# Set to "land_use_intensity" only when the land-use input has defensible
# fractions for each intensity stratum.
model_variant <- "land_use" # "land_use" or "land_use_intensity"
reference_pressure_class <- "primary_minimal"
similarity_adjustment <- 0.001

# -----------------------------------------------------------------------------
# V2: published-style pressure model (OFF by default)
# -----------------------------------------------------------------------------
# V1 above remains the stable land-use-only workflow. Set CS3_BII_VERSION=v2
# when the reviewed site-level and gridded pressure inputs described in
# input/v2/README.md are available. V2 never overwrites V1 model artefacts.
bii_version <- tolower(Sys.getenv("CS3_BII_VERSION", unset = "v1"))
if (!bii_version %in% c("v1", "v2")) {
  stop("CS3_BII_VERSION must be either 'v1' or 'v2'.", call. = FALSE)
}
bii_v2_enabled <- identical(bii_version, "v2")
v2_input_dir <- file.path(input_dir, "v2")
v2_site_pressure_path <- file.path(v2_input_dir, "predicts_site_pressures.csv")
v2_landsat_pressure_path <- file.path(v2_input_dir, "landsat_spatial_pressures.csv")
v2_eppa_pressure_path <- file.path(v2_input_dir, "eppa_spatial_pressures.csv")
v2_model_bundle_path <- file.path(output_dir, "bii_v2_model_bundle.rds")
v2_predictor_spec_path <- file.path(output_dir, "bii_v2_predictor_scaling.csv")
v2_pairs_path <- file.path(tmp_dir, "predicts_v2_composition_pairs.rds")
v2_luh2_population_2010_path <- file.path(v2_input_dir, "raw", "ssp2_2010.nc")
v2_luh2_population_2030_path <- file.path(v2_input_dir, "raw", "ssp2_2030.nc")
v2_luh2_landuse_path <- file.path(tmp_dir, "luh2_v2_landuse_0p5deg.csv")
v2_luh2_pressure_path <- file.path(v2_input_dir, "luh2_spatial_pressures.csv")
v2_luh2_crosswalk_path <- file.path(input_dir, "crosswalks", "luh2_to_bii_v2_crosswalk.csv")
# The published 1970--2050 PREDICTS projections omitted roads because no
# harmonised historical/future road series was available. Keep that setting as
# the V2 default for direct comparison with those results; opt in only for a
# road-informed analysis with reviewed 1 km and 50 km grids.
v2_include_roads <- FALSE
v2_include_secondary_age <- FALSE # turn on only with adequate age coverage
v2_include_study_mean_hpd_control <- TRUE # published-style control for spatial sampling bias
v2_max_missing_site_pressure_share <- 0.01 # small geolocation/data-coverage loss only
v2_minimum_sites_per_scope <- 200L
v2_minimum_pairs_per_scope <- 500L

# Filtering and model-fit safeguards. PREDICTS is terrestrial where a biome is
# reported; marine/unclassified records are excluded rather than guessed.
minimum_sites_per_scope <- 40L
minimum_pairs_per_scope <- 80L
minimum_taxon_sites <- 80L
minimum_region_sites <- 100L
fit_taxon_models <- TRUE
fit_region_models <- TRUE
# The taxonomic reporting groups requested for this analysis. Broad groups are
# constructed from PREDICTS Kingdom and Class fields in 02_helpers.R.
taxon_groups_to_fit <- c(
  "Mammals", "Birds", "Amphibians", "Reptiles", "Invertebrates", "Plants", "Fungi"
)
region_values_to_fit <- character() # empty = every adequately represented UN_region
max_pairs_per_study <- Inf # set finite only for an explicit speed sensitivity run
random_seed <- 42L

# A spatial input must contain this area field for published-style area-weighted
# aggregates. Set FALSE only for an explicitly equal-cell exploratory analysis.
require_area_weight <- TRUE

# Published benchmark settings. The bundled NHM file contains a historical
# global record identified by area_code == "global".
published_scenario <- "historical"
published_global_area_code <- "global"
