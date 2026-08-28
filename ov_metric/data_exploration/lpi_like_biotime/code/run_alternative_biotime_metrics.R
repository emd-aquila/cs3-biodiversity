# Diagnostic alternative to the OV components: within-assemblage richness
# standardised to each assemblage's minimum observed individual count.
#
# BioTIME processing equalises the number of sample events per year but not the
# number of individuals recorded in those events.  This script therefore
# compares observed richness with individual-based, sample-size-standardised
# expected richness.  It uses the same annual within-assemblage LPI chain as
# the main analysis and never compares raw scores among assemblages.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
})

file_arg <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grepl("^--file=", commandArgs(trailingOnly = FALSE))])
code_dir <- if (length(file_arg)) dirname(normalizePath(file_arg[[1]], mustWork = TRUE)) else getwd()
topic_dir <- normalizePath(file.path(code_dir, ".."), mustWork = TRUE)
repo_root <- normalizePath(file.path(topic_dir, "..", "..", ".."), mustWork = TRUE)
input_path <- file.path(repo_root, "ov_metric", "01_biodiversity_data_integration", "output", "biotime_database.rds")
out_csv <- file.path(topic_dir, "output", "csv", "alternative_metrics")
out_png <- file.path(topic_dir, "output", "png", "alternative_metrics")
dir.create(out_csv, recursive = TRUE, showWarnings = FALSE)
dir.create(out_png, recursive = TRUE, showWarnings = FALSE)

start_year <- 1970L
end_year <- 2023L
years <- start_year:end_year
minimum_reference_individuals <- 20L

# Expected richness after rarefaction to n individuals. Uses the exact
# hypergeometric expectation, avoiding a stochastic resampling step.
rarefied_richness <- function(counts, n) {
  counts <- as.numeric(counts[counts > 0])
  total <- sum(counts)
  if (!length(counts) || !is.finite(total) || total < n || n < 1) return(NA_real_)
  not_seen <- numeric(length(counts))
  feasible <- (total - counts) >= n
  not_seen[feasible] <- exp(lchoose(total - counts[feasible], n) - lchoose(total, n))
  sum(1 - not_seen)
}

message("Reading standardised BioTIME observations")
raw <- as.data.table(readRDS(input_path))
raw <- raw[
  sample_year >= start_year & sample_year <= end_year &
    is.finite(effort_corrected_measurement) & effort_corrected_measurement > 0 &
    !is.na(taxon_name) & nzchar(taxon_name),
  .(assemblage_id, sample_year = as.integer(sample_year), taxon_name,
    count = as.numeric(effort_corrected_measurement))
]

taxon_year <- raw[, .(count = sum(count)), by = .(assemblage_id, sample_year, taxon_name)]
year_totals <- taxon_year[, .(
  total_individuals = sum(count),
  observed_richness = uniqueN(taxon_name)
), by = .(assemblage_id, sample_year)]
reference <- year_totals[, .(
  n_observed_years = .N,
  reference_individuals = min(total_individuals)
), by = assemblage_id]
eligible <- reference[n_observed_years >= 3L & reference_individuals >= minimum_reference_individuals]
message("Eligible assemblages for individual rarefaction: ", nrow(eligible))

taxon_year <- merge(taxon_year, eligible[, .(assemblage_id, reference_individuals)], by = "assemblage_id", all = FALSE)
rarefied <- taxon_year[, .(
  rarefied_richness = rarefied_richness(count, unique(reference_individuals))
), by = .(assemblage_id, sample_year)]
metric_values <- merge(
  year_totals[assemblage_id %in% eligible$assemblage_id],
  rarefied,
  by = c("assemblage_id", "sample_year"), all = FALSE
) |>
  as_tibble() |>
  left_join(as_tibble(eligible), by = "assemblage_id")
write_csv(metric_values, file.path(out_csv, "assemblage_year_observed_and_rarefied_richness.csv"))

annualise <- function(data, metric, label) {
  values <- data |>
    transmute(assemblage_id, sample_year, value = .data[[metric]]) |>
    filter(is.finite(value), value > 0) |>
    arrange(assemblage_id, sample_year)
  pieces <- split(values, values$assemblage_id)
  bind_rows(lapply(pieces, function(dat) {
    assemblage_id <- dat$assemblage_id[[1]]
    dat <- dat |> group_by(sample_year) |> summarise(value = mean(value), .groups = "drop") |> arrange(sample_year)
    if (nrow(dat) < 2) return(tibble())
    prediction_years <- seq.int(min(dat$sample_year), max(dat$sample_year))
    # This diagnostic deliberately uses only the LPI log-linear chain. It
    # keeps the alternative-metric comparison transparent and avoids allowing
    # a smoothing model to manufacture a trend from sparse observations.
    fit_method <- "chain_interpolation"
    predicted_log <- approx(dat$sample_year, log(dat$value), xout = prediction_years)$y
    predicted <- exp(predicted_log)
    ratio_raw <- predicted[-1] / predicted[-length(predicted)]
    ratio <- pmin(10, pmax(0.1, ratio_raw))
    tibble(
      assemblage_id = assemblage_id, sample_year = prediction_years[-1],
      metric = label, fit_method = fit_method, annual_ratio_raw = ratio_raw,
      annual_ratio = ratio, capped = ratio != ratio_raw, log_change = log(ratio)
    )
  }))
}

annual_changes <- bind_rows(
  annualise(metric_values, "observed_richness", "Observed richness"),
  annualise(metric_values, "rarefied_richness", "Individual-rarefied richness")
)
write_csv(annual_changes, file.path(out_csv, "assemblage_annual_changes_observed_and_rarefied_richness.csv"))

indices <- annual_changes |>
  group_by(metric, sample_year) |>
  summarise(
    annual_log_change = mean(log_change),
    n_contributors = n_distinct(assemblage_id),
    capped_changes = sum(capped),
    .groups = "drop"
) |>
  complete(metric, sample_year = years, fill = list(annual_log_change = 0, n_contributors = 0L, capped_changes = 0L)) |>
  arrange(metric, sample_year) |>
  group_by(metric) |>
  mutate(index_1970 = exp(cumsum(annual_log_change))) |>
  ungroup() |>
  group_by(metric) |>
  mutate(index_2000 = index_1970 / index_1970[sample_year == 2000][1]) |>
  ungroup()
write_csv(indices, file.path(out_csv, "lpi_style_index_observed_and_rarefied_richness.csv"))

summary <- indices |>
  group_by(metric) |>
  summarise(
    n_eligible_assemblages = nrow(eligible),
    reference_individuals_minimum = minimum_reference_individuals,
    index_at_1970 = first(index_1970[sample_year == 1970]),
    index_at_2000 = first(index_1970[sample_year == 2000]),
    index_at_2020 = first(index_1970[sample_year == 2020]),
    index_at_2023 = first(index_1970[sample_year == 2023]),
    change_1970_2020_percent = 100 * (first(index_1970[sample_year == 2020]) - 1),
    change_2000_2020_percent = 100 * (first(index_2000[sample_year == 2020]) - 1),
    change_2000_2023_percent = 100 * (first(index_2000[sample_year == 2023]) - 1),
    contributors_2000 = n_contributors[sample_year == 2000][1],
    contributors_2020 = n_contributors[sample_year == 2020][1],
    contributors_2023 = n_contributors[sample_year == 2023][1],
    .groups = "drop"
  )
write_csv(summary, file.path(out_csv, "summary_observed_and_rarefied_richness.csv"))

palette <- c("Observed richness" = "#3182BD", "Individual-rarefied richness" = "#08519C")
plot <- ggplot(indices, aes(sample_year, index_2000, colour = metric)) +
  geom_hline(yintercept = 1, linewidth = 0.35, colour = "grey55") +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  scale_colour_manual(values = palette) +
  labs(
    title = "BioTIME richness trend after individual-count standardisation",
    subtitle = paste0("LPI-style chained annual changes; assemblages with ≥3 years and ≥", minimum_reference_individuals, " individuals in every observed year"),
    x = "Year", y = "Index (2000 = 1)", colour = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
ggsave(file.path(out_png, "observed_vs_individual_rarefied_richness_2000_baseline.png"), plot, width = 8.5, height = 5.2, dpi = 180)

message("Alternative richness diagnostic complete.")
