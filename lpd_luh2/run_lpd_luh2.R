#!/usr/bin/env Rscript

# Native-terrestrial LPD population intervals linked to the LUH2 0.25-degree
# cell containing each population coordinate. LUH2 fractions are aggregated to
# the five broad PREDICTS-compatible land-use classes in the companion Python
# extractor. Results are independent of lpd_hansen/.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(sf)
  library(ggplot2)
  library(fixest)
})

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args)) tolower(args[[1]]) else "all"
if (!mode %in% c("prepare", "analyse", "all")) stop("Usage: Rscript lpd_luh2/run_lpd_luh2.R [prepare|analyse|all]", call. = FALSE)

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else "lpd_luh2/run_lpd_luh2.R"
workflow_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
repo_root <- normalizePath(file.path(workflow_dir, ".."), mustWork = TRUE)
input_path <- file.path(repo_root, "00_biodiversity_data", "living_planet", "LPD_2024_public.csv")
aez_path <- file.path(repo_root, "00_spatial_data", "aez", "AEZ_shp_file.shp")
output_dir <- file.path(workflow_dir, "output")
table_dir <- file.path(output_dir, "tables")
figure_dir <- file.path(output_dir, "figures")
luh2_dir <- file.path(output_dir, "luh2")
walk(c(table_dir, figure_dir, luh2_dir), dir.create, recursive = TRUE, showWarnings = FALSE)

year_min <- 2000L; year_max <- 2020L
land_classes <- c("primary", "secondary", "cropland", "pasture", "urban")
predictor_classes <- setdiff(land_classes, "primary")
minimum_aez_intervals <- 30L; minimum_aez_locations <- 10L
paths <- list(
  intervals = file.path(table_dir, "lpd_native_terrestrial_intervals_2000_2020.csv"),
  locations = file.path(table_dir, "lpd_native_terrestrial_locations_aez.csv"),
  states = file.path(luh2_dir, "luh2_predicts_landuse_by_location_year.csv"),
  transitions = file.path(table_dir, "lpd_luh2_interval_transitions.csv")
)

check <- function(condition, message) if (!isTRUE(condition)) stop(message, call. = FALSE)
write_csv_safe <- function(data, path) readr::write_csv(data, path, na = "")

assign_aez <- function(locations) {
  sf::sf_use_s2(FALSE)
  aez <- sf::read_sf(aez_path, quiet = TRUE) |> sf::st_make_valid() |> select(AEZ_id = Id, AEZ)
  points <- locations |> st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |> st_transform(st_crs(aez))
  st_join(points, aez, join = st_within, left = TRUE) |> st_drop_geometry() |>
    group_by(location_id) |>
    summarise(across(-c(AEZ_id, AEZ), first),
              AEZ_id = first(AEZ_id[!is.na(AEZ_id)], default = NA_real_),
              AEZ = first(AEZ[!is.na(AEZ)], default = NA_character_), .groups = "drop") |>
    mutate(AEZ_assignment = if_else(is.na(AEZ), "unassigned", "within_polygon"))
}

prepare_data <- function() {
  raw <- readr::read_csv(input_path, show_col_types = FALSE, name_repair = "unique")
  check(nrow(raw) == 35996L, "Unexpected LPD row count; expected 35,996 rows.")
  year_cols <- as.character(year_min:year_max)
  cohort <- raw |> filter(System == "Terrestrial", Native == "1") |>
    transmute(population_id = as.character(ID), binomial = Binomial, taxon_class = Class, country = Country,
              latitude = suppressWarnings(as.numeric(Latitude)), longitude = suppressWarnings(as.numeric(Longitude)),
              units = Units, lpr2024_included = `Included in LPR2024`, across(all_of(year_cols)))
  check(nrow(cohort) == 11702L, "Unexpected native terrestrial LPD cohort size; expected 11,702.")
  observations <- cohort |> pivot_longer(all_of(year_cols), names_to = "year", values_to = "raw_value") |>
    mutate(year = as.integer(year), abundance = suppressWarnings(as.numeric(raw_value))) |>
    filter(is.finite(abundance), abundance >= 0, is.finite(latitude), is.finite(longitude)) |>
    select(-raw_value) |> arrange(population_id, year)
  check(!anyDuplicated(observations[c("population_id", "year")]), "Duplicate population-year values in LPD input.")
  intervals <- observations |> group_by(population_id) |>
    mutate(positive_mean = mean(abundance[abundance > 0], na.rm = TRUE), replacement = 0.01 * positive_mean,
           log_value = if_else(abundance == 0, replacement, abundance), zero_replaced = abundance == 0,
           interval_end_year = lead(year), abundance_end = lead(abundance), log_value_end = lead(log_value),
           zero_replaced_end = lead(zero_replaced)) |> ungroup() |>
    filter(!is.na(interval_end_year), is.finite(log_value), is.finite(log_value_end), log_value > 0, log_value_end > 0) |>
    transmute(interval_id = paste(population_id, year, interval_end_year, sep = "__"), population_id,
              location_id = paste(format(latitude, trim = TRUE, scientific = FALSE), format(longitude, trim = TRUE, scientific = FALSE), sep = "__"),
              binomial, taxon_class, country, latitude, longitude, units, lpr2024_included,
              interval_start_year = year, interval_end_year = as.integer(interval_end_year),
              interval_duration = interval_end_year - interval_start_year, abundance_start = abundance, abundance_end,
              zero_replaced_start = zero_replaced, zero_replaced_end,
              log_abundance_change = log(log_value_end) - log(log_value)) |>
    filter(interval_duration > 0)
  check(!anyDuplicated(intervals$interval_id), "Duplicate LPD interval IDs.")
  locations <- intervals |> distinct(location_id, latitude, longitude, country) |> assign_aez()
  write_csv_safe(intervals, paths$intervals); write_csv_safe(locations, paths$locations)
  write_csv_safe(tibble(metric = c("native_terrestrial_populations", "intervals", "populations_with_intervals", "locations", "unassigned_aez_locations"),
                         value = c(nrow(cohort), nrow(intervals), n_distinct(intervals$population_id), nrow(locations), sum(is.na(locations$AEZ)))),
                 file.path(table_dir, "prepare_audit.csv"))
  message("Prepared ", nrow(intervals), " LPD intervals at ", nrow(locations), " locations.")
}

term_row <- function(model, term, group) {
  tab <- fixest::coeftable(model)
  if (!term %in% rownames(tab)) return(tibble())
  estimate <- unname(tab[term, "Estimate"]); standard_error <- unname(tab[term, "Std. Error"])
  tibble(group = group, term = term, estimate = estimate, std_error = standard_error,
         conf_low = estimate - 1.96 * standard_error, conf_high = estimate + 1.96 * standard_error,
         statistic = unname(tab[term, "t value"]), p_value = unname(tab[term, "Pr(>|t|)"]), n_intervals = nobs(model))
}

fit_fractional_model <- function(data, group = "global") {
  terms <- paste0("delta_", predictor_classes, "_per_10pp")
  model <- fixest::feols(as.formula(paste0("log_abundance_change ~ ", paste(c(terms, "interval_duration"), collapse = " + "),
                                          " | population_id + interval_start_year")),
                        data = data, cluster = ~population_id + location_id, warn = FALSE)
  result <- bind_rows(lapply(terms, function(term) term_row(model, term, group))) |>
    mutate(n_populations = n_distinct(data$population_id), n_locations = n_distinct(data$location_id), n_countries = n_distinct(data$country))
  list(model = model, result = result)
}

dominant_class <- function(data, suffix) {
  matrix <- as.matrix(data[paste0("share_", land_classes, suffix)])
  land_classes[max.col(matrix, ties.method = "first")]
}

analyse_data <- function() {
  if (!all(file.exists(unlist(paths[c("intervals", "locations", "states")]))) ) {
    message("LUH2 state extraction is required before analysis. Run:\n  bii_metric/tmp/luh2_venv/bin/python lpd_luh2/code/extract_luh2_population_states.py --intervals ", paths$intervals,
            " --output ", paths$states)
    return(invisible(FALSE))
  }
  intervals <- read_csv(paths$intervals, show_col_types = FALSE)
  locations <- read_csv(paths$locations, show_col_types = FALSE)
  states <- read_csv(paths$states, show_col_types = FALSE)
  required <- c("location_id", "year", "luh2_source", paste0("share_", land_classes))
  check(all(required %in% names(states)), "LUH2 state output has an invalid schema.")
  check(!anyDuplicated(states[c("location_id", "year")]), "LUH2 state output has duplicate location-year rows.")
  start <- states |> rename_with(~paste0(.x, "_start"), -c(location_id, year)) |> rename(interval_start_year = year)
  end <- states |> rename_with(~paste0(.x, "_end"), -c(location_id, year)) |> rename(interval_end_year = year)
  data <- intervals |> left_join(locations |> select(location_id, AEZ, AEZ_id, AEZ_assignment), by = "location_id") |>
    left_join(start, by = c("location_id", "interval_start_year")) |> left_join(end, by = c("location_id", "interval_end_year"))
  share_columns <- c(paste0("share_", land_classes, "_start"), paste0("share_", land_classes, "_end"))
  data <- data |> filter(if_all(all_of(share_columns), is.finite))
  check(nrow(data) > 0, "No LPD intervals could be matched to finite LUH2 land-use states.")
  for (class_name in land_classes) {
    data[[paste0("delta_", class_name)]] <- data[[paste0("share_", class_name, "_end")]] - data[[paste0("share_", class_name, "_start")]]
  }
  for (class_name in predictor_classes) data[[paste0("delta_", class_name, "_per_10pp")]] <- data[[paste0("delta_", class_name)]] / 0.1
  data$from_land_use <- dominant_class(data, "_start")
  data$to_land_use <- dominant_class(data, "_end")
  data <- data |> mutate(dominant_land_use_changed = from_land_use != to_land_use,
                         lu_source_pair = paste(luh2_source_start, luh2_source_end, sep = "->"))
  write_csv_safe(data, paths$transitions)

  matrix <- data |> filter(dominant_land_use_changed) |> count(from_land_use, to_land_use, name = "n_intervals") |>
    complete(from_land_use = land_classes, to_land_use = land_classes, fill = list(n_intervals = 0)) |>
    arrange(from_land_use, to_land_use)
  matrix_summary <- data |> filter(dominant_land_use_changed) |> group_by(from_land_use, to_land_use) |>
    summarise(n_intervals = n(), n_populations = n_distinct(population_id), n_locations = n_distinct(location_id),
              mean_log_abundance_change = mean(log_abundance_change), median_log_abundance_change = median(log_abundance_change), .groups = "drop")
  write_csv_safe(matrix, file.path(table_dir, "dominant_landuse_transition_matrix.csv"))
  write_csv_safe(matrix_summary, file.path(table_dir, "dominant_landuse_transition_summary.csv"))

  global <- fit_fractional_model(data)
  aez_data <- data |> filter(!is.na(AEZ)) |> group_by(AEZ) |>
    filter(n() >= minimum_aez_intervals, n_distinct(location_id) >= minimum_aez_locations) |> ungroup()
  aez <- split(aez_data, aez_data$AEZ) |> map(~tryCatch(fit_fractional_model(.x, unique(.x$AEZ)), error = function(e) NULL)) |> compact()
  results <- bind_rows(global$result, bind_rows(map(aez, "result")))
  models <- c(global = list(global$model), map(aez, "model"))
  write_csv_safe(results, file.path(table_dir, "fractional_landuse_regression_coefficients.csv"))
  saveRDS(models, file.path(table_dir, "fractional_landuse_regression_models.rds"), compress = "gzip")
  write_csv_safe(data |> summarise(n_intervals = n(), n_populations = n_distinct(population_id), n_locations = n_distinct(location_id),
                                   n_changed_dominant_landuse = sum(dominant_land_use_changed), historical_historical = sum(lu_source_pair == "historical->historical"),
                                   historical_future = sum(lu_source_pair == "historical->ssp245_future"), future_future = sum(lu_source_pair == "ssp245_future->ssp245_future")),
                 file.path(table_dir, "analysis_coverage.csv"))

  heatmap <- ggplot(matrix, aes(to_land_use, from_land_use, fill = n_intervals)) + geom_tile(colour = "white") +
    geom_text(aes(label = n_intervals)) + scale_fill_viridis_c() + coord_equal() +
    labs(title = "Dominant LUH2 land-use transitions at LPD locations", x = "End land use", y = "Start land use") + theme_minimal(base_size = 11)
  ggsave(file.path(figure_dir, "dominant_landuse_transition_matrix.png"), heatmap, width = 7, height = 6, dpi = 220)
  global_plot <- results |> filter(group == "global") |>
    ggplot(aes(reorder(term, estimate), estimate)) + geom_hline(yintercept = 0, colour = "grey60") +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.15) + geom_point() + coord_flip() +
    labs(title = "Global LPD association with LUH2 fractional land-use change", subtitle = "Coefficient per 10 percentage-point change, relative to primary vegetation",
         x = NULL, y = "Change in log abundance") + theme_minimal(base_size = 11)
  ggsave(file.path(figure_dir, "global_fractional_landuse_coefficients.png"), global_plot, width = 8, height = 5, dpi = 220)
  message("LUH2 analysis complete: ", table_dir)
}

if (!identical(Sys.getenv("LPD_LUH2_DEFINE_ONLY"), "1")) {
  if (mode %in% c("prepare", "all")) prepare_data()
  if (mode %in% c("analyse", "all")) analyse_data()
}
