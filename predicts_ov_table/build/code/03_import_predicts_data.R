# =====================================================
# Download and filter PREDICTS data
# =====================================================

predicts_raw <- LoadPredictsData(predicts_rds_path, extract = extract_years)

# Filter to abundance metrics and 2000-2024 date range
predicts_filtered <- predicts_raw %>%
  mutate(
    Sample_midpoint = as.Date(Sample_midpoint),
    Year = lubridate::year(Sample_midpoint)
  ) %>%
  filter(
    Diversity_metric_type == "Abundance",
    !is.na(Sample_midpoint),
    Sample_midpoint >= date_min,
    Sample_midpoint <= date_max
  )

# Select relevant columns to keep
cols_keep <- c(
  "Reference", "Study_number", "Site_name", "Site_number", "Source_ID", "SSBS",
  "Country",
  "Biome", "Predominant_land_use", "Use_intensity",
  "Years_since_fragmentation_or_conversion",
  "UN_subregion",
  "Hotspot",
  "Latitude", "Longitude",
  "Kingdom", "Phylum", "Class", "Order", "Family", "Genus",
  "Diversity_metric_unit",
  "Effort_corrected_measurement", "Measurement",
  "Sampling_effort", "Sampling_effort_unit",
  "Sample_date_resolution", "Sample_start_earliest", "Sample_end_latest", "Sample_midpoint"
)

predicts_clean <- predicts_filtered %>%
  select(any_of(cols_keep))

write_csv_safe(predicts_clean, predicts_filtered_path)
