# =====================================================
# Build meeting-ready biodiversity database summary tables
# =====================================================

# Importing libraries
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(scales)
library(this.path)
library(stringr)

# Directory paths within data_exploration
code_dir <- normalizePath(this.path::this.dir())
topic_dir <- normalizePath(file.path(code_dir, ".."), mustWork = TRUE)
output_dir <- normalizePath(file.path(topic_dir, "output"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Pipeline directory and database paths used during analysis
repo_root <- normalizePath(file.path(topic_dir, "..", "..", ".."), mustWork = TRUE)
ov_metric_dir <- file.path(repo_root, "ov_metric")
integration_output_dir <- file.path(ov_metric_dir, "01_biodiversity_data_integration", "output")
ov_output_dir <- file.path(ov_metric_dir, "02_ov_calculation", "calculation", "output")

dataset_specs <- tibble::tibble(
  dataset = c("predicts", "biotime", "combined"),
  label = c("PREDICTS", "BioTIME", "Combined"),
  database_path = file.path(
    integration_output_dir,
    c("predicts_database.rds", "biotime_database.rds", "combined_database.rds")
  ),
  ov_path = file.path(
    ov_output_dir,
    c("predicts_ov_scores.csv", "biotime_ov_scores.csv", "combined_ov_scores.csv")
  )
)

missing_inputs <- c(dataset_specs$database_path, dataset_specs$ov_path)
missing_inputs <- missing_inputs[!file.exists(missing_inputs)]
if (length(missing_inputs) > 0) {
  stop("Missing input file(s): ", paste(missing_inputs, collapse = ", "), call. = FALSE)
}


# Helper functions to format and read data from databases
format_count_table <- function(dat, skip_cols = character()) {
  dat %>%
    mutate(
      across(
        where(is.numeric) & !all_of(skip_cols),
        ~ scales::comma(.x, accuracy = if (any(.x %% 1 != 0, na.rm = TRUE)) 0.1 else 1)
      )
    )
}

read_database_with_aez <- function(database_path, ov_path, label) {
  db <- readRDS(database_path)
  
  ov_lookup <- readr::read_csv(ov_path, show_col_types = FALSE) %>%
    transmute(
      sample_id,
      AEZ,
      ov_latitude = as.numeric(latitude),
      ov_longitude = as.numeric(longitude)
    ) %>%
    distinct(sample_id, .keep_all = TRUE)
  
  db %>%
    mutate(
      sample_id = as.character(sample_id),
      sample_year = as.integer(sample_year),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      abundance = as.numeric(abundance),
      effort_corrected_measurement = as.numeric(effort_corrected_measurement)
    ) %>%
    left_join(ov_lookup, by = "sample_id") %>%
    mutate(
      label = label,
      site_latitude = round(coalesce(ov_latitude, latitude), 4),
      site_longitude = round(coalesce(ov_longitude, longitude), 4),
      raw_site_key = paste(site_latitude, site_longitude, sep = "__"),
      site_key = paste(AEZ, site_latitude, site_longitude, sep = "__")
    )
}

summarize_database <- function(database_path, ov_path, label) {
  dat <- read_database_with_aez(database_path, ov_path, label)
  
  entries_by_site <- dat %>%
    filter(!is.na(site_latitude), !is.na(site_longitude)) %>%
    count(raw_site_key, name = "raw_entries")
  
  samples_by_site <- dat %>%
    filter(!is.na(site_latitude), !is.na(site_longitude)) %>%
    group_by(raw_site_key) %>%
    summarise(samples = n_distinct(sample_id), .groups = "drop")
  
  tibble::tibble(
    Labels = label,
    raw_entries_passing_filters = nrow(dat),
    sites = n_distinct(dat$raw_site_key[!is.na(dat$site_latitude) & !is.na(dat$site_longitude)]),
    aez_tagged_sites_in_final_ov_table = n_distinct(dat$site_key[!is.na(dat$AEZ)]),
    raw_entries_with_final_aez_assignment = sum(!is.na(dat$AEZ)),
    raw_entries_without_final_aez_assignment = sum(is.na(dat$AEZ)),
    samples = n_distinct(dat$sample_id),
    studies_or_sources = n_distinct(dat$standard_source_id),
    sample_year_min = min(dat$sample_year, na.rm = TRUE),
    sample_year_max = max(dat$sample_year, na.rm = TRUE),
    taxa = n_distinct(dat$taxon_name),
    families = n_distinct(dat$Family, na.rm = TRUE),
    entries_per_site_average = round(mean(entries_by_site$raw_entries), 1),
    entries_per_site_median = round(median(entries_by_site$raw_entries), 1),
    entries_per_site_minimum = min(entries_by_site$raw_entries),
    entries_per_site_maximum = max(entries_by_site$raw_entries),
    samples_per_site_average = round(mean(samples_by_site$samples), 1),
    samples_per_site_median = round(median(samples_by_site$samples), 1),
    samples_per_site_minimum = min(samples_by_site$samples),
    samples_per_site_maximum = max(samples_by_site$samples)
  )
}

entries_by_aez_one_database <- function(database_path, ov_path, label) {
  read_database_with_aez(database_path, ov_path, label) %>%
    mutate(AEZ = tidyr::replace_na(AEZ, "No_AEZ_or_no_final_OV")) %>%
    count(AEZ, name = label)
}

# Use functions to analyze databases and output tables
entries_and_sites <- purrr::pmap_dfr(
  dataset_specs,
  function(dataset, label, database_path, ov_path) {
    summarize_database(database_path, ov_path, label)
  }
)

entries_per_aez <- purrr::pmap(
  dataset_specs,
  function(dataset, label, database_path, ov_path) {
    entries_by_aez_one_database(database_path, ov_path, label)
  }
) %>%
  purrr::reduce(full_join, by = "AEZ") %>%
  mutate(across(c(PREDICTS, BioTIME, Combined), ~ tidyr::replace_na(.x, 0L))) %>%
  mutate(AEZ = factor(AEZ, levels = stringr::str_sort(unique(AEZ), numeric = TRUE))) %>%
  arrange(AEZ) %>%
  mutate(AEZ = as.character(AEZ))

entries_and_sites %>%
  format_count_table(skip_cols = c("sample_year_min", "sample_year_max")) %>%
  write_csv(file.path(output_dir, "entries_and_sites_by_db.csv"))

entries_per_aez %>%
  format_count_table() %>%
  write_csv(file.path(output_dir, "entries_per_aez.csv"))

message("Wrote meeting tables to: ", normalizePath(output_dir, mustWork = FALSE))
