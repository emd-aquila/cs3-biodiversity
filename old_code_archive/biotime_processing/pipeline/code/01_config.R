# =====================================================
# Configuration for the structured BioTIME pipeline.
# =====================================================

code_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
pipeline_dir <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = TRUE)
biotime_processing_dir <- normalizePath(
  file.path(pipeline_dir, ".."),
  winslash = "/",
  mustWork = TRUE
)

input_dir <- file.path(pipeline_dir, "input")
output_dir <- file.path(pipeline_dir, "output")
tmp_dir <- file.path(pipeline_dir, "tmp")
legacy_data_dir <- file.path(biotime_processing_dir, "novel_testing", "data")

for (dir_path in c(input_dir, output_dir, tmp_dir)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

# Static official BioTIME 2.0 files. The local copies are reused when present.
biotime_query_url <- "https://zenodo.org/records/15222193/files/biotime_v2_query_15April25.rds?download=1"
biotime_metadata_url <- "https://zenodo.org/records/15222193/files/biotime_v2_metadata_15April25.csv?download=1"
biotime_references_url <- "https://zenodo.org/records/15222193/files/references_biotime_v2_15April25.csv?download=1"

first_existing_path <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) return(paths[[1]])
  normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
}

biotime_data_path <- first_existing_path(c(
  file.path(legacy_data_dir, "biotime_data.csv"),
  file.path(input_dir, "biotime_data.csv")
))
biotime_metadata_path <- first_existing_path(c(
  file.path(legacy_data_dir, "biotime_metadata.csv"),
  file.path(input_dir, "biotime_metadata.csv")
))
biotime_references_path <- first_existing_path(c(
  file.path(legacy_data_dir, "biotime_references.csv"),
  file.path(input_dir, "biotime_references.csv")
))
predicts_full_rds_path <- first_existing_path(c(
  file.path(biotime_processing_dir, "old", "build_predicts", "tmp", "predicts.rds"),
  file.path(tmp_dir, "predicts.rds")
))

# Comparison settings.
minimum_year <- 2000L
strong_spatial_match_km <- 1
possible_spatial_match_km <- 10

# Filtered PREDICTS outputs.
predicts_filtered_observations_path <- file.path(
  output_dir,
  "predicts_observations_abundance_study_start_2000.rds"
)
predicts_filter_summary_path <- file.path(output_dir, "predicts_filter_summary.csv")

# Small fixture for trial work.
predicts_trial_n_rows <- 10L
predicts_trial_path <- file.path(
  input_dir,
  "predicts_abundance_study_start_2000_top_10.csv"
)

# Filtered BioTIME outputs.
biotime_eligible_studies_path <- file.path(output_dir, "biotime_studies_eligible.csv")
biotime_excluded_studies_path <- file.path(output_dir, "biotime_studies_excluded.csv")
biotime_filtered_observations_path <- file.path(
  output_dir,
  "biotime_observations_terrestrial_abundance_study_start_2000.rds"
)
biotime_study_summary_path <- file.path(output_dir, "biotime_study_summary.csv")
biotime_filter_summary_path <- file.path(output_dir, "biotime_filter_summary.csv")

# Comparison outputs.
predicts_study_summary_path <- file.path(output_dir, "predicts_study_summary.csv")
reference_overlap_path <- file.path(output_dir, "reference_overlap_candidates.csv")
spatiotemporal_overlap_path <- file.path(output_dir, "spatiotemporal_overlap_candidates.csv")
comparison_summary_path <- file.path(output_dir, "database_comparison_summary.csv")
comparison_report_path <- file.path(output_dir, "comparison_report.md")
schema_inventory_csv_path <- file.path(output_dir, "schema_inventory.csv")
schema_inventory_md_path <- file.path(output_dir, "schema_inventory.md")
