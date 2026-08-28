# LPI-compatible, within-assemblage BioTIME OV analysis.
#
# This script deliberately aggregates annual *within-assemblage* changes.  It
# never pools OV values across assemblages and never inserts a value of one when
# a new assemblage first appears.  The implementation follows the unweighted
# LPI logic where applicable: GAMs for >=6 observations, log-linear chain
# interpolation otherwise, an LPI-style zero replacement, capped annual ratios,
# and geometric aggregation of annual log changes.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(sf)
  library(tidyr)
  library(purrr)
  library(mgcv)
  library(scales)
})

file_arg <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grepl("^--file=", commandArgs(trailingOnly = FALSE))])
code_dir <- if (length(file_arg)) dirname(normalizePath(file_arg[[1]], mustWork = TRUE)) else getwd()
topic_dir <- normalizePath(file.path(code_dir, ".."), mustWork = TRUE)
repo_root <- normalizePath(file.path(topic_dir, "..", "..", ".."), mustWork = TRUE)
input_dir <- file.path(topic_dir, "input")
output_dir <- file.path(topic_dir, "output")
csv_dir <- file.path(output_dir, "csv")
png_dir <- file.path(output_dir, "png")

output_folders <- c(
  "global", "continents", "taxa", "protected_status", "diagnostics",
  "crosswalks", "published_lpi_comparison"
)
walk(c(csv_dir, png_dir), ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))
walk(file.path(csv_dir, output_folders), ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))
walk(file.path(png_dir, output_folders), ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

ov_path <- file.path(repo_root, "ov_metric", "02_ov_calculation", "calculation", "output", "biotime_ov_scores.csv")
taxon_path <- file.path(repo_root, "ov_metric", "biotime_only", "data_cleaning", "output", "biotime_timeseries.csv")
raw_metadata_path <- file.path(repo_root, "00_biodiversity_data", "biotime", "biotime_metadata_raw.rds")
countries_path <- file.path(repo_root, "00_spatial_data", "naturalearth_10m_admin_0_countries", "ne_10m_admin_0_countries.shp")
lpi_path <- file.path(input_dir, "living_planet_index_by_region.csv")
pa_paths <- c(
  file.path(input_dir, "wdpca_site_membership_global_candidate.csv"),
  file.path(input_dir, "wdpca_site_membership.csv")
)
stopifnot(file.exists(ov_path), file.exists(taxon_path), file.exists(countries_path))

metrics <- c(
  ov_score = "OV",
  msa = "MSA",
  phylo_div = "PD",
  shannon = "Shannon diversity"
)
metric_order <- names(metrics)
metric_palette <- c(
  ov_score = "#D73027",
  msa = "#9ECAE1",
  phylo_div = "#3182BD",
  shannon = "#08519C"
)
continent_order <- c("Africa", "Asia", "Europe", "North America", "Oceania", "South America", "Unassigned")
display_taxa <- c("Amphibians", "Birds", "Invertebrates", "Mammals", "Plants")
index_start_year <- suppressWarnings(as.integer(Sys.getenv("LPI_INDEX_START_YEAR", unset = "2000")))
if (!is.finite(index_start_year) || index_start_year < 1800L || index_start_year > 2020L) stop("LPI_INDEX_START_YEAR must be between 1800 and 2020.", call. = FALSE)
index_end_year <- 2023L
years <- index_start_year:index_end_year
comparison_years <- index_start_year:2020
bootstrap_reps <- as.integer(Sys.getenv("LPI_BOOTSTRAPS", unset = "100"))
set.seed(20260818)

write_out <- function(data, folder, name) {
  write_csv(data, file.path(csv_dir, folder, name))
}

theme_lpi <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

metric_scale <- function() {
  scale_colour_manual(values = metric_palette, breaks = metric_order, labels = unname(metrics[metric_order]))
}

metric_fill_scale <- function() {
  scale_fill_manual(values = metric_palette, breaks = metric_order, labels = unname(metrics[metric_order]))
}

# Keep the visual and left-to-right bar order identical across every chart.
# This is especially important for dodged continent/taxon bars, where a
# character grouping would otherwise be ordered alphabetically.
order_metrics_for_plot <- function(data) {
  data |> mutate(metric = factor(as.character(metric), levels = metric_order))
}

safe_first <- function(x, default = NA_character_) {
  x <- x[!is.na(x)]
  if (length(x)) x[[1]] else default
}

# Geographic and taxonomic metadata -------------------------------------------------
ov <- read_csv(ov_path, show_col_types = FALSE) |>
  filter(database == "BioTIME", is.finite(sample_year), !is.na(assemblage_id)) |>
  mutate(sample_year = as.integer(sample_year))

taxa <- read_csv(taxon_path, show_col_types = FALSE) |>
  transmute(assemblage_id, taxon_group_clean = taxon_group) |>
  distinct(assemblage_id, .keep_all = TRUE)
raw_metadata <- readRDS(raw_metadata_path)
study_taxa <- tibble(
  study_id = as.character(raw_metadata$STUDY_ID),
  taxa_raw = as.character(raw_metadata$TAXA), organisms_raw = as.character(raw_metadata$ORGANISMS)
) |>
  mutate(taxon_group_metadata = case_when(
    grepl("bird", taxa_raw, ignore.case = TRUE) ~ "Birds",
    grepl("mammal", taxa_raw, ignore.case = TRUE) ~ "Mammals",
    grepl("amphib", taxa_raw, ignore.case = TRUE) ~ "Amphibians",
    grepl("reptile", taxa_raw, ignore.case = TRUE) & grepl("lizard", organisms_raw, ignore.case = TRUE) ~ "Lizards",
    grepl("reptile", taxa_raw, ignore.case = TRUE) ~ "Reptiles",
    grepl("plant", taxa_raw, ignore.case = TRUE) ~ "Plants",
    grepl("invertebrate", taxa_raw, ignore.case = TRUE) ~ "Invertebrates",
    grepl("fung", taxa_raw, ignore.case = TRUE) ~ "Fungi",
    grepl("multiple", taxa_raw, ignore.case = TRUE) ~ "Multiple",
    TRUE ~ "Unclassified"
  )) |>
  select(study_id, taxon_group_metadata) |>
  distinct(study_id, .keep_all = TRUE)

countries <- read_sf(countries_path, quiet = TRUE) |>
  st_make_valid() |>
  select(continent = CONTINENT, large_region = REGION_UN)
sites <- ov |>
  distinct(assemblage_id, longitude, latitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
site_regions <- st_join(sites, countries, join = st_within, left = TRUE) |>
  st_drop_geometry() |>
  group_by(assemblage_id) |>
  summarise(
    continent = safe_first(continent),
    large_region = safe_first(large_region),
    .groups = "drop"
  )

site_meta <- ov |>
  group_by(assemblage_id) |>
  summarise(
    study_id = first(study_id),
    longitude = first(longitude), latitude = first(latitude),
    first_year = min(sample_year), last_year = max(sample_year),
    n_observations = n_distinct(sample_year), year_span = last_year - first_year,
    .groups = "drop"
  ) |>
  mutate(study_id = as.character(study_id)) |>
  left_join(site_regions, by = "assemblage_id") |>
  left_join(taxa, by = "assemblage_id") |>
  left_join(study_taxa, by = "study_id") |>
  mutate(
    continent = coalesce(continent, "Unassigned"),
    large_region = coalesce(large_region, "Unassigned"),
    taxon_group = coalesce(taxon_group_clean, taxon_group_metadata, "Unclassified")
  ) |>
  select(-taxon_group_clean, -taxon_group_metadata)

pa_path <- pa_paths[file.exists(pa_paths)][1]
if (is.na(pa_path) || !length(pa_path)) {
  stop("Missing WDPCA membership file. Run prepare_wdpca_membership_global.R before this analysis.", call. = FALSE)
}
pa <- read_csv(pa_path, show_col_types = FALSE) |>
  select(assemblage_id, protected_area, first_year, country_iso3, everything()) |>
  distinct(assemblage_id, .keep_all = TRUE)
if (n_distinct(pa$assemblage_id) != nrow(site_meta)) {
  stop(
    "WDPCA membership is incomplete (", n_distinct(pa$assemblage_id), " of ", nrow(site_meta),
    " assemblages). Finish the protected/conserved classification before reporting PA results.",
    call. = FALSE
  )
}
site_meta <- site_meta |>
  select(-any_of(c("protected_area", "country_iso3"))) |>
  left_join(pa |> select(-any_of(c("first_year", "country_iso3"))), by = "assemblage_id") |>
  mutate(protected_area = coalesce(protected_area, "Not classified"))
write_out(site_meta, "crosswalks", "assemblage_continent_taxon_lookup.csv")
write_out(pa, "protected_status", "assemblage_protection_audit.csv")

# LPI-compatible annual changes -------------------------------------------------------
# Return one predicted value per intervening calendar year. Long series receive a
# GAM fit on log values. Other series receive the LPI chain method: straight lines
# on the log scale, which imply a constant annual proportional change in gaps.
annualise_one_series <- function(data, metric) {
  dat <- data |>
    select(assemblage_id, sample_year, value = all_of(metric)) |>
    filter(is.finite(value), sample_year >= min(years), sample_year <= max(years)) |>
    group_by(sample_year) |>
    summarise(value = mean(value), .groups = "drop") |>
    arrange(sample_year)
  if (nrow(dat) < 2 || max(dat$sample_year) <= min(dat$sample_year) || any(dat$value < 0)) return(tibble())

  zero_n <- sum(dat$value == 0)
  zero_offset <- if (zero_n) 0.01 * mean(dat$value, na.rm = TRUE) else NA_real_
  if (zero_n && (!is.finite(zero_offset) || zero_offset <= 0)) return(tibble())
  dat <- dat |> mutate(value_lpi = if_else(value == 0, zero_offset, value))

  prediction_years <- seq.int(min(dat$sample_year), max(dat$sample_year))
  fit_method <- "chain_interpolation"
  fit_ok <- FALSE
  predicted_log <- rep(NA_real_, length(prediction_years))
  if (nrow(dat) >= 6) {
    k <- min(10L, max(3L, nrow(dat) - 1L))
    fit <- try(
      mgcv::gam(log(value_lpi) ~ s(sample_year, k = k), data = dat, method = "REML"),
      silent = TRUE
    )
    if (!inherits(fit, "try-error")) {
      candidate <- try(predict(fit, newdata = tibble(sample_year = prediction_years)), silent = TRUE)
      if (!inherits(candidate, "try-error") && all(is.finite(candidate))) {
        predicted_log <- as.numeric(candidate)
        fit_method <- "GAM"
        fit_ok <- TRUE
      }
    }
  }
  if (!fit_ok) {
    predicted_log <- approx(
      x = dat$sample_year, y = log(dat$value_lpi), xout = prediction_years,
      method = "linear", rule = 1
    )$y
  }
  pred <- tibble(sample_year = prediction_years, predicted_value = exp(predicted_log)) |>
    mutate(
      previous_value = lag(predicted_value),
      annual_ratio_raw = predicted_value / previous_value,
      annual_ratio = pmin(10, pmax(0.1, annual_ratio_raw)),
      capped_annual_change = !is.na(annual_ratio_raw) & annual_ratio != annual_ratio_raw,
      log_change = log(annual_ratio)
    ) |>
    filter(!is.na(previous_value), is.finite(log_change)) |>
    mutate(
      assemblage_id = data$assemblage_id[[1]], metric = metric,
      fit_method = fit_method, zero_replacements = zero_n, zero_offset = zero_offset,
      n_observed_values = nrow(dat)
    )
  pred
}

annual_cache <- file.path(csv_dir, "diagnostics", "assemblage_annual_changes.csv")
if (file.exists(annual_cache) && Sys.getenv("LPI_REBUILD_ANNUAL", unset = "0") != "1") {
  message("Reusing cached annual LPI-compatible changes: ", annual_cache)
  annual_changes <- read_csv(annual_cache, show_col_types = FALSE)
} else {
  message("Building annual LPI-compatible changes for ", length(metric_order), " metrics.")
  annual_changes <- bind_rows(lapply(metric_order, function(metric) {
    message("  ", metrics[[metric]])
    pieces <- split(ov |> select(assemblage_id, sample_year, all_of(metric)), ov$assemblage_id)
    bind_rows(lapply(pieces, annualise_one_series, metric = metric))
  })) |>
    left_join(site_meta, by = "assemblage_id") |>
    mutate(metric_label = unname(metrics[metric]))
  write_out(annual_changes, "diagnostics", "assemblage_annual_changes.csv")
}

model_audit <- annual_changes |>
  distinct(assemblage_id, metric, fit_method, zero_replacements, zero_offset, n_observed_values) |>
  left_join(site_meta |> select(assemblage_id, first_year, last_year, year_span, taxon_group, continent, protected_area), by = "assemblage_id")
write_out(model_audit, "diagnostics", "model_and_zero_handling_audit.csv")

cohort_membership <- site_meta |>
  transmute(
    assemblage_id,
    all_valid = n_observations >= 2,
    span_3_years = year_span >= 3,
    span_5_years = year_span >= 5,
    start_2000_2005_span_3 = first_year >= 2000 & first_year <= 2005 & year_span >= 3,
    fixed_coverage_2000_2005_to_2018_2020 = first_year >= 2000 & first_year <= 2005 & last_year >= 2018 & last_year <= 2020,
    protected_all = n_observations >= 2 & protected_area == "Inside",
    unprotected_all = n_observations >= 2 & protected_area == "Outside",
    protected_start_2000_2005_span_3 = first_year >= 2000 & first_year <= 2005 & year_span >= 3 & protected_area == "Inside",
    unprotected_start_2000_2005_span_3 = first_year >= 2000 & first_year <= 2005 & year_span >= 3 & protected_area == "Outside"
  )
cohort_names <- names(cohort_membership)[-1]
cohort_labels <- c(
  all_valid = "All valid series",
  span_3_years = "≥3-year span",
  span_5_years = "≥5-year span",
  start_2000_2005_span_3 = "Starts 2000–05 + ≥3 years",
  fixed_coverage_2000_2005_to_2018_2020 = "Fixed coverage: starts 2000–05, observed 2018–20",
  protected_all = "Protected/conserved, all valid",
  unprotected_all = "Unprotected, all valid",
  protected_start_2000_2005_span_3 = "Protected/conserved, 2000–05 + ≥3 years",
  unprotected_start_2000_2005_span_3 = "Unprotected, 2000–05 + ≥3 years"
)
write_out(
  cohort_membership |> pivot_longer(-assemblage_id, names_to = "cohort", values_to = "eligible") |>
    mutate(cohort_label = unname(cohort_labels[cohort])),
  "diagnostics", "cohort_membership.csv"
)

# Geometric chain and annual-change bootstrap. The bootstrap resamples annual
# assemblage changes within each interval, matching the interval bootstrap used
# in the original chain LPI while retaining the changing contributor set.
chain_index <- function(data, weighted_continents = FALSE, reps = 0L) {
  empty <- tibble(
    sample_year = years, n_contributors = 0L, annual_log_change = 0,
    annual_geometric_change = 1, index = 1,
    index_ci_low = NA_real_, index_ci_high = NA_real_
  )
  if (!nrow(data)) return(empty)

  interval <- if (weighted_continents) {
    data |>
      filter(continent != "Unassigned") |>
      group_by(sample_year, continent) |>
      summarise(log_change = mean(log_change), n_assemblages = n_distinct(assemblage_id), .groups = "drop") |>
      group_by(sample_year) |>
      summarise(
        annual_log_change = mean(log_change),
        n_contributors = sum(n_assemblages),
        n_continents = n(),
        .groups = "drop"
      )
  } else {
    data |>
      group_by(sample_year) |>
      summarise(annual_log_change = mean(log_change), n_contributors = n_distinct(assemblage_id), .groups = "drop")
  }
  out <- tibble(sample_year = years) |>
    left_join(interval, by = "sample_year") |>
    mutate(
      n_contributors = coalesce(n_contributors, 0L),
      annual_log_change = coalesce(annual_log_change, 0),
      annual_geometric_change = exp(annual_log_change),
      index = exp(cumsum(annual_log_change)),
      index_ci_low = NA_real_, index_ci_high = NA_real_
    )
  out$index[out$sample_year == index_start_year] <- 1

  if (reps > 0 && !weighted_continents) {
    boot_log <- matrix(0, nrow = reps, ncol = length(years))
    for (j in seq_along(years)) {
      yr <- years[[j]]
      values <- data$log_change[data$sample_year == yr]
      if (length(values) > 1) {
        boot_log[, j] <- replicate(reps, mean(sample(values, length(values), replace = TRUE)))
      } else if (length(values) == 1) {
        boot_log[, j] <- values
      }
    }
    boot_index <- exp(t(apply(boot_log, 1, cumsum)))
    boot_index[, years == index_start_year] <- 1
    out$index_ci_low <- apply(boot_index, 2, quantile, probs = 0.025, na.rm = TRUE)
    out$index_ci_high <- apply(boot_index, 2, quantile, probs = 0.975, na.rm = TRUE)
  }
  out
}

make_indices <- function(group_var = NULL, include_balanced = FALSE) {
  group_values <- if (is.null(group_var)) site_meta |> transmute(assemblage_id, group = "Global") else site_meta |>
    transmute(assemblage_id, group = .data[[group_var]]) |>
    distinct()
  bind_rows(lapply(cohort_names, function(cohort) {
    cohort_id <- cohort
    ids <- cohort_membership |> filter(.data[[cohort_id]]) |> pull(assemblage_id)
    dat <- annual_changes |> filter(assemblage_id %in% ids) |>
      inner_join(group_values, by = "assemblage_id")
    groups <- sort(unique(dat$group))
    bind_rows(lapply(groups, function(group) {
      bind_rows(lapply(metric_order, function(metric) {
        metric_id <- metric
        metric_label_value <- unname(metrics[[metric_id]])
        x <- dat |> filter(group == !!group, metric == !!metric_id)
        idx <- chain_index(x, reps = bootstrap_reps)
        idx |> mutate(
          geography = group, cohort = cohort_id, cohort_label = unname(cohort_labels[[cohort_id]]),
          metric = metric_id, metric_label = metric_label_value, aggregation = "Unweighted assemblages"
        )
      }))
    }))
  }))
}

message("Chaining global, continental, regional, taxonomic, and PA indices.")
global_indices <- make_indices()
continent_indices <- make_indices("continent")
region_indices <- make_indices("large_region")
taxon_indices <- make_indices("taxon_group")
pa_indices <- make_indices("protected_area")

# Equal-continent weighting is a global-only sensitivity result.
balanced_indices <- bind_rows(lapply(cohort_names, function(cohort) {
  cohort_id <- cohort
  ids <- cohort_membership |> filter(.data[[cohort_id]]) |> pull(assemblage_id)
  bind_rows(lapply(metric_order, function(metric) {
    metric_id <- metric
    metric_label_value <- unname(metrics[[metric_id]])
    x <- annual_changes |> filter(assemblage_id %in% ids, metric == !!metric_id)
    chain_index(x, weighted_continents = TRUE, reps = 0L) |>
      mutate(
        geography = "Global", cohort = cohort_id, cohort_label = unname(cohort_labels[[cohort_id]]),
        metric = metric_id, metric_label = metric_label_value, aggregation = "Equal-continent weighted"
      )
  }))
}))

summarise_change <- function(indices) {
  indices |>
    group_by(geography, cohort, cohort_label, metric, metric_label, aggregation) |>
    summarise(
      index_1970 = index[sample_year == 1970][1],
      index_2000 = index[sample_year == 2000][1],
      index_2020 = index[sample_year == 2020][1],
      index_2023 = index[sample_year == 2023][1],
      percent_change_1970_2020 = 100 * (index_2020 / index_1970 - 1),
      percent_change_1970_2023 = 100 * (index_2023 / index_1970 - 1),
      percent_change_2000_2020 = 100 * (index_2020 / index_2000 - 1),
      percent_change_2000_2023 = 100 * (index_2023 / index_2000 - 1),
      ci_low_1970_2020 = 100 * (index_ci_low[sample_year == 2020][1] / index_1970 - 1),
      ci_high_1970_2020 = 100 * (index_ci_high[sample_year == 2020][1] / index_1970 - 1),
      ci_low_2000_2020 = 100 * (index_ci_low[sample_year == 2020][1] / index_2000 - 1),
      ci_high_2000_2020 = 100 * (index_ci_high[sample_year == 2020][1] / index_2000 - 1),
      n_eligible = n_distinct(annual_changes$assemblage_id[annual_changes$metric == first(metric)]),
      n_contributors_2000 = n_contributors[sample_year == 2000][1],
      n_contributors_2020 = n_contributors[sample_year == 2020][1],
      n_contributors_2023 = n_contributors[sample_year == 2023][1],
      .groups = "drop"
    )
}

global_change <- summarise_change(global_indices)
continent_change <- summarise_change(continent_indices)
region_change <- summarise_change(region_indices)
taxon_change <- summarise_change(taxon_indices)
pa_change <- summarise_change(pa_indices)
balanced_change <- summarise_change(balanced_indices)

# Add exact eligible counts, sparse flags, and model diagnostics to summaries.
eligible_counts <- cohort_membership |>
  pivot_longer(-assemblage_id, names_to = "cohort", values_to = "eligible") |>
  filter(eligible) |>
  left_join(site_meta |> select(assemblage_id, continent, large_region, taxon_group, protected_area), by = "assemblage_id")
count_for <- function(summary, by_var = NULL) {
  if (is.null(by_var)) {
    counts <- eligible_counts |> count(cohort, name = "n_eligible") |> mutate(geography = "Global")
  } else {
    counts <- eligible_counts |> count(cohort, geography = .data[[by_var]], name = "n_eligible")
  }
  summary |> select(-n_eligible) |> left_join(counts, by = c("cohort", "geography"))
}
global_change <- count_for(global_change)
continent_change <- count_for(continent_change, "continent")
region_change <- count_for(region_change, "large_region")
taxon_change <- count_for(taxon_change, "taxon_group")
pa_change <- count_for(pa_change, "protected_area")
balanced_change <- count_for(balanced_change)

continent_coverage <- eligible_counts |> count(cohort, continent, name = "n_eligible") |>
  mutate(sparse = n_eligible < 30)
taxon_coverage <- eligible_counts |> count(cohort, taxon_group, name = "n_eligible") |>
  mutate(sparse = n_eligible < 30)
write_out(continent_coverage, "continents", "continent_coverage_and_sparse_flags.csv")
write_out(taxon_coverage, "taxa", "taxon_coverage_and_sparse_flags.csv")

# A wide global matrix is the primary table requested by the user.
make_global_matrix <- function(change_col) {
  global_change |>
    filter(aggregation == "Unweighted assemblages") |>
    select(metric, metric_label, cohort_label, percent_change = all_of(change_col)) |>
    pivot_wider(names_from = cohort_label, values_from = percent_change) |>
    mutate(metric = factor(metric, levels = metric_order)) |>
    arrange(metric) |>
    select(Metric = metric_label, all_of(unname(cohort_labels[cohort_names])))
}
global_matrix_2000 <- make_global_matrix("percent_change_2000_2020")
write_out(global_matrix_2000, "global", "lpi_change_by_scenario_2000_2020.csv")
write_out(make_global_matrix("percent_change_2000_2023"), "global", "lpi_change_by_scenario_2000_2023.csv")
if (index_start_year <= 1970L) {
  global_matrix_1970 <- make_global_matrix("percent_change_1970_2020")
  post_2000_cols <- c(
    "Starts 2000–05 + ≥3 years",
    "Fixed coverage: starts 2000–05, observed 2018–20",
    "Protected/conserved, 2000–05 + ≥3 years",
    "Unprotected, 2000–05 + ≥3 years"
  )
  names(global_matrix_1970)[match(post_2000_cols, names(global_matrix_1970))] <- paste0(post_2000_cols, " [post-2000 sensitivity]")
  write_out(global_matrix_1970, "global", "lpi_change_by_scenario_1970_2020.csv")
  write_out(make_global_matrix("percent_change_1970_2023"), "global", "lpi_change_by_scenario_1970_2023.csv")
}
write_out(global_indices, "global", paste0("lpi_index_annual_", index_start_year, "_2023.csv"))
write_out(global_change, "global", "lpi_change_summary_long.csv")
write_out(balanced_indices, "global", "lpi_index_equal_continent_weighted.csv")
write_out(balanced_change, "global", "lpi_change_equal_continent_weighted.csv")
write_out(continent_indices, "continents", "lpi_index_by_continent_year.csv")
write_out(continent_change, "continents", "lpi_change_by_continent.csv")
write_out(region_indices, "continents", "lpi_index_by_large_region_year.csv")
write_out(region_change, "continents", "lpi_change_by_large_region.csv")
write_out(taxon_indices, "taxa", "lpi_index_by_taxon_year.csv")
write_out(taxon_change, "taxa", "lpi_change_by_taxon.csv")
write_out(pa_indices, "protected_status", "lpi_index_by_protection_year.csv")
write_out(pa_change, "protected_status", "lpi_change_by_protection.csv")
write_out(bind_rows(global_indices, continent_indices, region_indices, taxon_indices, pa_indices) |>
            select(geography, cohort, metric, sample_year, n_contributors, annual_log_change, annual_geometric_change),
          "diagnostics", "contributor_counts_by_year.csv")

scenario_notes <- tibble(
  baseline = c("1970–2020", "1970–2020", "2000–2020", "Both"),
  cohort_or_method = c(
    "Starts 2000–05 and fixed-coverage columns",
    "Baseline-aligned fixed coverage: starts 1970–75 and observed 2018–20",
    "Fixed coverage: starts 2000–05 and observed 2018–20",
    "GAM-fitted annual changes"
  ),
  interpretation = c(
    "These cohorts contain no annual changes before 2000, so their 1970–2020 values should not be compared with a 1970-baseline LPI; use them only as post-2000 sensitivities.",
    "Only 2 assemblages meet this definition; it is too sparse to report as a robust fixed-coverage result.",
    "Useful for testing entrant/attrition effects from 2000 onward, but it does not require complete annual observations.",
    "GAMs interpolate/smooth between observed years. Fixed cohort membership controls composition, but does not turn interpolated years into direct observations."
  )
)
write_out(scenario_notes, "global", "scenario_table_interpretation_notes.csv")

if (Sys.getenv("LPI_TABLES_ONLY", unset = "0") == "1") {
  message("Completed requested table outputs only in: ", normalizePath(output_dir))
  quit(save = "no", status = 0)
}

# Endpoint and observed-pair companion analyses --------------------------------------
endpoint_one_metric <- function(metric) {
  ov |>
    select(assemblage_id, sample_year, value = all_of(metric)) |>
    arrange(assemblage_id, sample_year) |>
    group_by(assemblage_id) |>
    summarise(
      first_year = first(sample_year), last_year = last(sample_year), n_observations = n(),
      year_span = last_year - first_year, start_value = first(value), end_value = last(value), .groups = "drop"
    ) |>
    mutate(
      metric = metric, delta = end_value - start_value,
      percent_change = if_else(start_value > 0, 100 * delta / start_value, NA_real_),
      ratio = if_else(start_value > 0 & end_value > 0, end_value / start_value, NA_real_),
      annual_ratio = if_else(!is.na(ratio) & year_span > 0, ratio^(1 / year_span), NA_real_)
    )
}
pair_one_metric <- function(metric) {
  ov |>
    select(assemblage_id, sample_year, value = all_of(metric)) |>
    arrange(assemblage_id, sample_year) |>
    group_by(assemblage_id) |>
    mutate(year_t1 = lag(sample_year), value_t1 = lag(value)) |>
    filter(!is.na(year_t1)) |>
    ungroup() |>
    transmute(
      assemblage_id, metric = metric, year_t1, year_t2 = sample_year, year_gap = sample_year - year_t1,
      value_t1, value_t2 = value, delta = value - value_t1,
      percent_change = if_else(value_t1 > 0, 100 * delta / value_t1, NA_real_),
      ratio = if_else(value_t1 > 0 & value > 0, value / value_t1, NA_real_),
      annual_ratio = if_else(!is.na(ratio) & year_gap > 0, ratio^(1 / year_gap), NA_real_)
    )
}
endpoints <- bind_rows(lapply(metric_order, endpoint_one_metric)) |>
  left_join(site_meta, by = "assemblage_id") |>
  mutate(metric_label = unname(metrics[metric]))
pairs <- bind_rows(lapply(metric_order, pair_one_metric)) |>
  left_join(site_meta, by = "assemblage_id") |>
  mutate(metric_label = unname(metrics[metric]))

endpoint_summary <- endpoints |>
  inner_join(cohort_membership |> pivot_longer(-assemblage_id, names_to = "cohort", values_to = "eligible") |> filter(eligible), by = "assemblage_id") |>
  group_by(metric, metric_label, cohort) |>
  summarise(
    n_total = n(), n_geometric_valid = sum(is.finite(ratio) & ratio > 0),
    arithmetic_mean_percent_change = mean(percent_change, na.rm = TRUE),
    median_percent_change = median(percent_change, na.rm = TRUE),
    geometric_mean_percent_change = 100 * (exp(mean(log(ratio[is.finite(ratio) & ratio > 0]))) - 1),
    annualized_geometric_mean_percent_change = 100 * (exp(mean(log(annual_ratio[is.finite(annual_ratio) & annual_ratio > 0]))) - 1),
    negative_n = sum(delta < 0, na.rm = TRUE), negative_share = mean(delta < 0, na.rm = TRUE), .groups = "drop"
  )
pair_summary <- pairs |>
  group_by(metric, metric_label) |>
  summarise(
    n_pairs = n(), n_geometric_valid = sum(is.finite(ratio) & ratio > 0),
    arithmetic_mean_percent_change = mean(percent_change, na.rm = TRUE),
    geometric_mean_percent_change = 100 * (exp(mean(log(ratio[is.finite(ratio) & ratio > 0]))) - 1),
    annualized_geometric_mean_percent_change = 100 * (exp(mean(log(annual_ratio[is.finite(annual_ratio) & annual_ratio > 0]))) - 1),
    negative_n = sum(delta < 0, na.rm = TRUE), negative_share = mean(delta < 0, na.rm = TRUE), .groups = "drop"
  )
write_out(endpoints, "diagnostics", "assemblage_first_last_changes.csv")
write_out(pairs, "diagnostics", "assemblage_consecutive_observation_pairs.csv")
write_out(bind_rows(mutate(endpoint_summary, change_type = "first_to_last"), mutate(pair_summary, change_type = "consecutive_observation_pairs")), "global", "endpoint_and_pairwise_summary.csv")

# Published-LPI comparison ------------------------------------------------------------
if (file.exists(lpi_path)) {
  lpi <- read_csv(lpi_path, show_col_types = FALSE)
  value_col <- setdiff(names(lpi), c("Entity", "Code", "Year"))[[1]]
  published_global <- lpi |>
    filter(Entity %in% c("World", "Global"), Year %in% comparison_years) |>
    transmute(sample_year = Year, index = .data[[value_col]], series = "Published global LPI") |>
    group_by(series) |> mutate(index = index / index[sample_year == index_start_year][1]) |> ungroup()
  biotime_global <- global_indices |>
    filter(cohort == "all_valid", metric == "ov_score", aggregation == "Unweighted assemblages", sample_year %in% comparison_years) |>
    transmute(sample_year, index, series = "BioTIME OV")
  comparison <- bind_rows(biotime_global, published_global)
  write_out(comparison, "published_lpi_comparison", "published_lpi_comparison.csv")
  fig <- ggplot(comparison, aes(sample_year, index, colour = series)) +
    geom_hline(yintercept = 1, colour = "grey60", linewidth = 0.35) +
    geom_line(linewidth = 1) + geom_point(size = 1.5) +
    scale_colour_manual(values = c("BioTIME OV" = metric_palette[["ov_score"]], "Published global LPI" = "#444444")) +
    labs(title = "BioTIME OV and published global LPI", subtitle = paste0("Both rebased to 1 in ", index_start_year, "; indicators and sampling frames differ."), x = NULL, y = paste0("Index (", index_start_year, " = 1)")) + theme_lpi()
  ggsave(file.path(png_dir, "published_lpi_comparison", "global_ov_vs_published_lpi.png"), fig, width = 10, height = 6, dpi = 300)
}

# Figures -----------------------------------------------------------------------------
global_plot_data <- global_indices |>
  filter(cohort == "all_valid", aggregation == "Unweighted assemblages", sample_year <= 2020) |>
  order_metrics_for_plot()
fig_global <- ggplot(global_plot_data, aes(sample_year, index, colour = metric, fill = metric)) +
  geom_ribbon(aes(ymin = index_ci_low, ymax = index_ci_high), alpha = 0.12, colour = NA, show.legend = FALSE) +
  geom_hline(yintercept = 1, colour = "grey60", linewidth = 0.35) +
  geom_line(linewidth = 1) + metric_scale() + metric_fill_scale() +
  labs(title = "Global BioTIME LPI-style index", subtitle = paste0("Unweighted annual geometric change; ", index_start_year, " = 1."), x = NULL, y = "Index") + theme_lpi()
ggsave(file.path(png_dir, "global", "global_four_metric_index_2000_2020.png"), fig_global, width = 10.5, height = 6.5, dpi = 300)

fig_extension <- ggplot(global_indices |> filter(cohort == "all_valid", aggregation == "Unweighted assemblages") |> order_metrics_for_plot(), aes(sample_year, index, colour = metric, fill = metric)) +
  geom_ribbon(aes(ymin = index_ci_low, ymax = index_ci_high), alpha = 0.12, colour = NA, show.legend = FALSE) +
  geom_hline(yintercept = 1, colour = "grey60", linewidth = 0.35) + geom_line(linewidth = 1) + metric_scale() + metric_fill_scale() +
  labs(title = "Global BioTIME LPI-style index, extension to 2023", subtitle = paste0("BioTIME-only extension; ", index_start_year, " = 1."), x = NULL, y = "Index") + theme_lpi()
ggsave(file.path(png_dir, "global", "global_four_metric_index_2000_2023.png"), fig_extension, width = 10.5, height = 6.5, dpi = 300)

weight_compare <- bind_rows(
  global_indices |> filter(cohort == "all_valid", metric == "ov_score", aggregation == "Unweighted assemblages") |> select(sample_year, index, aggregation),
  balanced_indices |> filter(cohort == "all_valid", metric == "ov_score") |> select(sample_year, index, aggregation)
)
fig_weight <- ggplot(weight_compare, aes(sample_year, index, colour = aggregation)) +
  geom_hline(yintercept = 1, colour = "grey60", linewidth = 0.35) + geom_line(linewidth = 1) +
  scale_colour_manual(values = c("Unweighted assemblages" = metric_palette[["ov_score"]], "Equal-continent weighted" = "#333333")) +
  labs(title = "Global OV sensitivity to continental weighting", subtitle = paste0(index_start_year, " = 1."), x = NULL, y = "OV index") + theme_lpi()
ggsave(file.path(png_dir, "global", "ov_unweighted_vs_equal_continent_weighted.png"), fig_weight, width = 10, height = 6, dpi = 300)

fig_contributors <- ggplot(global_indices |> filter(cohort == "all_valid", aggregation == "Unweighted assemblages") |> order_metrics_for_plot(), aes(sample_year, n_contributors, colour = metric)) +
  geom_line(linewidth = 1) + metric_scale() +
  labs(title = "Assemblages contributing annual LPI-style changes", x = NULL, y = "Contributing assemblages") + theme_lpi()
ggsave(file.path(png_dir, "diagnostics", "global_annual_contributor_counts.png"), fig_contributors, width = 10, height = 6, dpi = 300)

coverage_long <- site_meta |>
  select(assemblage_id, first_year, year_span) |>
  pivot_longer(-assemblage_id, names_to = "measure", values_to = "value")
fig_coverage <- ggplot(coverage_long, aes(value)) +
  geom_histogram(bins = 24, fill = "#666666", colour = "white") +
  facet_wrap(~ measure, scales = "free_x", labeller = as_labeller(c(first_year = "Assemblage first year", year_span = "First-to-last span (years)"))) +
  labs(title = "BioTIME assemblage temporal coverage", x = NULL, y = "Assemblages") + theme_lpi()
ggsave(file.path(png_dir, "diagnostics", "assemblage_start_year_and_span_distribution.png"), fig_coverage, width = 11, height = 5.5, dpi = 300)

fig_endpoint <- ggplot(endpoints |> filter(is.finite(percent_change)) |> order_metrics_for_plot(), aes(metric, percent_change, fill = metric)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.35) + geom_boxplot(outlier.alpha = 0.16) + metric_fill_scale() +
  scale_x_discrete(limits = metric_order, labels = unname(metrics[metric_order])) +
  labs(title = "Individual assemblage first-to-last change", subtitle = "Companion endpoint analysis; not the chained LPI-style headline.", x = NULL, y = "Percent change") + theme_lpi()
ggsave(file.path(png_dir, "global", "endpoint_change_distribution_by_metric.png"), fig_endpoint, width = 10.5, height = 6.5, dpi = 300)

continent_plot_data <- continent_indices |>
  filter(cohort == "all_valid", aggregation == "Unweighted assemblages", geography %in% continent_order) |>
  order_metrics_for_plot()
fig_continent <- ggplot(continent_plot_data, aes(sample_year, index, colour = metric)) +
  geom_hline(yintercept = 1, colour = "grey70", linewidth = 0.3) + geom_line(linewidth = 0.85) + metric_scale() +
  facet_wrap(~ geography, scales = "free_y") + labs(title = "LPI-style indices by continent", subtitle = paste0("All valid assemblages; ", index_start_year, " = 1."), x = NULL, y = "Index") + theme_lpi(10)
ggsave(file.path(png_dir, "continents", "four_metric_indices_by_continent.png"), fig_continent, width = 13, height = 9, dpi = 300)

for (cont in setdiff(continent_order, "Unassigned")) {
  dat <- continent_plot_data |> filter(geography == cont)
  if (!nrow(dat)) next
  fig <- ggplot(dat, aes(sample_year, index, colour = metric)) +
    geom_hline(yintercept = 1, colour = "grey70", linewidth = 0.3) + geom_line(linewidth = 1) + metric_scale() +
    labs(title = paste("LPI-style indices —", cont), subtitle = paste0("All valid assemblages; ", index_start_year, " = 1."), x = NULL, y = "Index") + theme_lpi()
  ggsave(file.path(png_dir, "continents", paste0("four_metric_index_", gsub(" ", "_", tolower(cont)), ".png")), fig, width = 10, height = 6, dpi = 300)
}

fig_cont_change <- ggplot(continent_change |> filter(cohort == "all_valid", aggregation == "Unweighted assemblages", geography != "Unassigned") |> order_metrics_for_plot(), aes(geography, percent_change_2000_2020, fill = metric, group = metric)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.35) +
  geom_col(width = 0.72, position = position_dodge2(width = 0.88, preserve = "single", padding = 0.08)) + metric_fill_scale() +
  labs(title = "Final LPI-style change by continent", subtitle = "2000–2020; all valid assemblages.", x = NULL, y = "Percent change") + theme_lpi() + theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(png_dir, "continents", "final_change_by_continent_2000_2020.png"), fig_cont_change, width = 12, height = 6.5, dpi = 300)

region_plot_data <- region_indices |> filter(cohort == "all_valid", aggregation == "Unweighted assemblages", geography != "Unassigned") |> order_metrics_for_plot()
fig_region <- ggplot(region_plot_data, aes(sample_year, index, colour = metric)) +
  geom_hline(yintercept = 1, colour = "grey70", linewidth = 0.3) + geom_line(linewidth = 0.85) + metric_scale() +
  facet_wrap(~ geography, scales = "free_y") + labs(title = "LPI-style indices by large region", subtitle = paste0("All valid assemblages; ", index_start_year, " = 1."), x = NULL, y = "Index") + theme_lpi(10)
ggsave(file.path(png_dir, "continents", "four_metric_indices_by_large_region.png"), fig_region, width = 13, height = 8, dpi = 300)

taxon_plot_data <- taxon_indices |> filter(cohort == "all_valid", aggregation == "Unweighted assemblages", geography %in% display_taxa) |> order_metrics_for_plot()
fig_taxon <- ggplot(taxon_plot_data, aes(sample_year, index, colour = metric)) +
  geom_hline(yintercept = 1, colour = "grey70", linewidth = 0.3) + geom_line(linewidth = 0.85) + metric_scale() +
  facet_wrap(~ geography, scales = "free_y") + labs(title = "LPI-style indices by taxon", subtitle = paste0("Taxon groups with at least 30 assemblages; ", index_start_year, " = 1."), x = NULL, y = "Index") + theme_lpi(10)
ggsave(file.path(png_dir, "taxa", "four_metric_indices_by_taxon.png"), fig_taxon, width = 13, height = 8.5, dpi = 300)

fig_taxon_change <- ggplot(taxon_change |> filter(cohort == "all_valid", aggregation == "Unweighted assemblages", geography %in% display_taxa) |> order_metrics_for_plot(), aes(geography, percent_change_2000_2020, fill = metric, group = metric)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.35) +
  geom_col(width = 0.72, position = position_dodge2(width = 0.88, preserve = "single", padding = 0.08)) + metric_fill_scale() +
  labs(title = "Final LPI-style change by taxon", subtitle = "2000–2020; taxon groups with at least 30 assemblages.", x = NULL, y = "Percent change") + theme_lpi() + theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(png_dir, "taxa", "final_change_by_taxon_2000_2020.png"), fig_taxon_change, width = 12, height = 6.5, dpi = 300)

taxon_endpoint <- endpoints |> filter(taxon_group %in% display_taxa, is.finite(percent_change)) |> order_metrics_for_plot()
fig_taxon_endpoint <- ggplot(taxon_endpoint, aes(taxon_group, percent_change, fill = metric)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.35) + geom_boxplot(outlier.alpha = 0.12) + metric_fill_scale() +
  labs(title = "Individual endpoint changes by taxon", subtitle = "Companion first-to-last analysis; taxon groups with at least 30 assemblages.", x = NULL, y = "Percent change") +
  theme_lpi() + theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(png_dir, "taxa", "endpoint_change_distribution_by_taxon.png"), fig_taxon_endpoint, width = 12, height = 6.5, dpi = 300)

pa_plot_data <- pa_indices |> filter(cohort %in% c("protected_all", "unprotected_all"), geography %in% c("Inside", "Outside"), aggregation == "Unweighted assemblages") |>
  mutate(geography = recode(geography, Inside = "Protected/conserved", Outside = "Unprotected")) |>
  order_metrics_for_plot()
fig_pa <- ggplot(pa_plot_data, aes(sample_year, index, colour = metric)) +
  geom_hline(yintercept = 1, colour = "grey70", linewidth = 0.3) + geom_line(linewidth = 0.9) + metric_scale() +
  facet_wrap(~ geography, scales = "free_y") + labs(title = "LPI-style indices by protected/conserved status", subtitle = paste0("All valid assemblages; protection status at first sampled year; ", index_start_year, " = 1."), x = NULL, y = "Index") + theme_lpi()
ggsave(file.path(png_dir, "protected_status", "four_metric_indices_by_protection_all_valid.png"), fig_pa, width = 12, height = 6.5, dpi = 300)

pa_start_plot_data <- pa_indices |> filter(cohort %in% c("protected_start_2000_2005_span_3", "unprotected_start_2000_2005_span_3"), geography %in% c("Inside", "Outside"), aggregation == "Unweighted assemblages") |>
  mutate(geography = recode(geography, Inside = "Protected/conserved", Outside = "Unprotected")) |>
  order_metrics_for_plot()
fig_pa_start <- ggplot(pa_start_plot_data, aes(sample_year, index, colour = metric)) +
  geom_hline(yintercept = 1, colour = "grey70", linewidth = 0.3) + geom_line(linewidth = 0.9) + metric_scale() +
  facet_wrap(~ geography, scales = "free_y") + labs(title = "Protected/conserved status — stable-start cohort", subtitle = "Assemblages starting 2000–05 with a ≥3-year span; 2000 = 1.", x = NULL, y = "Index") + theme_lpi()
ggsave(file.path(png_dir, "protected_status", "four_metric_indices_by_protection_2000_2005_start.png"), fig_pa_start, width = 12, height = 6.5, dpi = 300)

# Readme is deliberately generated with the results so paths and definitions cannot drift.
readme_lines <- c(
  "# BioTIME OV LPI-style output index", "",
  "## Start here", "",
  "- `csv/global/lpi_change_by_scenario_2000_2020.csv`: primary 2000–2020 chained-index matrix across all requested cohorts.",
  "- `csv/global/lpi_change_by_scenario_2000_2023.csv`: BioTIME-only extension through 2023.",
  "- `png/global/global_four_metric_index_2000_2020.png`: primary global graphic.",
  "- `csv/continents/` and `png/continents/`: continent and large-region results.",
  "- `csv/taxa/` and `png/taxa/`: taxon-group results.",
  "- `csv/protected_status/` and `png/protected_status/`: PA/OECM analyses.",
  "- `csv/diagnostics/`: annual changes, contributor counts, model fitting, endpoint, and pairwise results.", "",
  "## Method", "",
  "The headline index chains geometric means of annual within-assemblage changes. Assemblages enter only when they contribute an annual change; no entrant is re-based to one in an aggregate calendar-year index.",
  "Series with >=6 observations use GAM predictions on the log scale; shorter/failed fits use log-linear chain interpolation. Zero observations are replaced by 1% of the series mean for the log calculation; annual ratios are capped between 0.1 and 10.",
  "The global primary index weights assemblages equally. The equal-continent-weighted global OV result is a sensitivity analysis.", "",
  "## Interpretation", "",
  "Arithmetic endpoint means average percentages directly and can be driven by extreme gains. Geometric endpoint means average multiplicative ratios and are the appropriate LPI-like counterpart. The endpoint outputs are companion analyses, not the headline chained index.",
  paste0("Bootstrap intervals use ", bootstrap_reps, " annual-change resamples per index interval. Sparse taxonomic and continental strata are flagged in coverage CSVs.")
)
writeLines(readme_lines, file.path(output_dir, "README.md"))

message("Completed LPI-style outputs in: ", normalizePath(output_dir))
