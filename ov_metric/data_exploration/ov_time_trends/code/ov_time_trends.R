# =====================================================
# Plot OV vs. year by AEZ and broad AEZ type
# =====================================================

library(dplyr)
library(readr)
library(ggplot2)
library(purrr)
library(stringr)
library(scales)
library(this.path)

# Directory and file paths referred to during script
code_dir <- normalizePath(this.path::this.dir())
topic_dir <- normalizePath(file.path(code_dir, ".."), mustWork = TRUE)
repo_root <- normalizePath(file.path(topic_dir, "..", "..", ".."), mustWork = TRUE)
ov_metric_dir <- file.path(repo_root, "ov_metric")
output_dir <- file.path(topic_dir, "output")
aez_plot_dir <- file.path(output_dir, "individual_aez")
region_plot_dir <- file.path(output_dir, "regional")
dir.create(aez_plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(region_plot_dir, recursive = TRUE, showWarnings = FALSE)
summary_path <- file.path(output_dir, "summary.csv")

combined_ov_path <- file.path(
  ov_metric_dir,
  "02_ov_calculation",
  "calculation",
  "output",
  "combined_ov_scores.csv"
)

if (!file.exists(combined_ov_path)) {
  stop("Missing combined OV table: ", combined_ov_path, call. = FALSE)
}

classify_region <- function(aez) {
  aez_number <- as.integer(stringr::str_extract(aez, "[0-9]+"))
  dplyr::case_when(
    aez_number >= 1 & aez_number <= 6 ~ "Tropical",
    aez_number >= 7 & aez_number <= 12 ~ "Temperate",
    aez_number >= 13 & aez_number <= 18 ~ "Cold",
    TRUE ~ NA_character_
  )
}

sanitize_file_id <- function(x) {
  tolower(gsub("[^A-Za-z0-9_-]", "_", x))
}

trend_summary <- function(dat, group_cols) {
  dat %>%
    group_by(across(all_of(group_cols))) %>%
    group_modify(function(.x, .y) {
      if (nrow(.x) < 3 || n_distinct(.x$sample_year) < 2) {
        return(tibble(
          n_samples = nrow(.x),
          first_year = min(.x$sample_year, na.rm = TRUE),
          last_year = max(.x$sample_year, na.rm = TRUE),
          slope_ov_per_year = NA_real_,
          p_value = NA_real_,
          r_squared = NA_real_
        ))
      }

      fit <- lm(ov_score ~ sample_year, data = .x)
      fit_summary <- summary(fit)
      tibble(
        n_samples = nrow(.x),
        first_year = min(.x$sample_year, na.rm = TRUE),
        last_year = max(.x$sample_year, na.rm = TRUE),
        slope_ov_per_year = round(unname(coef(fit)[["sample_year"]]), 5),
        p_value = signif(fit_summary$coefficients["sample_year", "Pr(>|t|)"], 4),
        r_squared = round(fit_summary$r.squared, 4)
      )
    }) %>%
    ungroup()
}

plot_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey30"),
      legend.position = "bottom"
    )
}

save_aez_plot <- function(aez_value) {
  plot_dat <- ov_dat %>% filter(AEZ == aez_value)
  if (nrow(plot_dat) == 0) return(invisible(NULL))

  plot_obj <- ggplot(plot_dat, aes(x = sample_year, y = ov_score)) +
    geom_point(aes(color = source_database), alpha = 0.42, size = 1.1) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black", linewidth = 0.8) +
    scale_color_manual(values = c(PREDICTS = "#0072B2", BioTIME = "#D55E00"), name = "Source") +
    labs(
      title = paste0("OV vs. year: ", aez_value),
      subtitle = paste0(unique(plot_dat$region), " AEZ group; black line is a linear trend across all samples."),
      x = "Sample year",
      y = "OV score"
    ) +
    plot_theme()

  ggsave(
    file.path(aez_plot_dir, paste0("ov_vs_year_", sanitize_file_id(aez_value), ".png")),
    plot_obj,
    width = 9,
    height = 6,
    dpi = 300
  )
}

save_region_plot <- function(region_value) {
  plot_dat <- ov_dat %>% filter(region == region_value)
  if (nrow(plot_dat) == 0) return(invisible(NULL))

  plot_obj <- ggplot(plot_dat, aes(x = sample_year, y = ov_score)) +
    geom_point(aes(color = source_database), alpha = 0.35, size = 0.9) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black", linewidth = 0.85) +
    scale_color_manual(values = c(PREDICTS = "#0072B2", BioTIME = "#D55E00"), name = "Source") +
    labs(
      title = paste0("OV vs. year: ", region_value, " AEZs"),
      subtitle = "Black line is a linear trend across all samples in the broad AEZ group.",
      x = "Sample year",
      y = "OV score"
    ) +
    plot_theme()

  ggsave(
    file.path(region_plot_dir, paste0("ov_vs_year_", sanitize_file_id(region_value), ".png")),
    plot_obj,
    width = 9,
    height = 6,
    dpi = 300
  )
}

ov_dat <- readr::read_csv(combined_ov_path, show_col_types = FALSE) %>%
  transmute(
    sample_id,
    AEZ,
    sample_year = as.integer(sample_year),
    ov_score = as.numeric(ov_score),
    source_database = dplyr::case_when(
      database == "PREDICTS" ~ "PREDICTS",
      database == "BioTIME" ~ "BioTIME",
      TRUE ~ as.character(database)
    ),
    region = classify_region(AEZ)
  ) %>%
  filter(
    !is.na(AEZ),
    !is.na(region),
    !is.na(sample_year),
    is.finite(ov_score),
    source_database %in% c("PREDICTS", "BioTIME")
  ) %>%
  mutate(
    aez_number = as.integer(stringr::str_extract(AEZ, "[0-9]+")),
    AEZ = factor(AEZ, levels = paste0("AEZ", sort(unique(aez_number)))),
    region = factor(region, levels = c("Tropical", "Temperate", "Cold")),
    source_database = factor(source_database, levels = c("PREDICTS", "BioTIME"))
  )

aez_trends <- trend_summary(ov_dat, c("AEZ", "region")) %>%
  arrange(region, AEZ)

region_trends <- trend_summary(ov_dat, c("region")) %>%
  arrange(region)

aez_trends_display <- aez_trends %>%
  mutate(n_samples = scales::comma(n_samples, accuracy = 1))

region_trends_display <- region_trends %>%
  mutate(n_samples = scales::comma(n_samples, accuracy = 1))

readr::write_csv(aez_trends_display, file.path(aez_plot_dir, "ov_year_trends_by_aez.csv"))
readr::write_csv(region_trends_display, file.path(region_plot_dir, "ov_year_trends_by_region.csv"))

purrr::walk(levels(droplevels(ov_dat$AEZ)), save_aez_plot)
purrr::walk(levels(droplevels(ov_dat$region)), save_region_plot)

facet_plot <- ggplot(ov_dat, aes(x = sample_year, y = ov_score)) +
  geom_point(aes(color = source_database), alpha = 0.28, size = 0.75) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black", linewidth = 0.65) +
  facet_wrap(~ AEZ, scales = "free_y") +
  scale_color_manual(values = c(PREDICTS = "#0072B2", BioTIME = "#D55E00"), name = "Source") +
  labs(
    title = "OV vs. year by AEZ",
    subtitle = "Each panel is one AEZ; black lines are within-AEZ linear trends.",
    x = "Sample year",
    y = "OV score"
  ) +
  plot_theme()

ggsave(
  file.path(aez_plot_dir, "ov_vs_year_all_aez_faceted.png"),
  facet_plot,
  width = 14,
  height = 10,
  dpi = 300
)

type_facet_plot <- ggplot(ov_dat, aes(x = sample_year, y = ov_score)) +
  geom_point(aes(color = source_database), alpha = 0.28, size = 0.75) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black", linewidth = 0.75) +
  facet_wrap(~ region, scales = "free_y") +
  scale_color_manual(values = c(PREDICTS = "#0072B2", BioTIME = "#D55E00"), name = "Source") +
  labs(
    title = "OV vs. year by broad AEZ type",
    subtitle = "Tropical = AEZ1-6; Temperate = AEZ7-12; Cold = AEZ13-18.",
    x = "Sample year",
    y = "OV score"
  ) +
  plot_theme()

ggsave(
  file.path(region_plot_dir, "ov_vs_year_region_faceted.png"),
  type_facet_plot,
  width = 12,
  height = 5.5,
  dpi = 300
)

message("Wrote OV-vs-year AEZ outputs to: ", normalizePath(output_dir, mustWork = FALSE))
