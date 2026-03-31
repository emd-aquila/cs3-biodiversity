# =====================================================
# 05_diagnostics.R
# Create summaries and plots for AEZ-specific OLS models
# for the current regression run.
# =====================================================

# -----------------------------------------------------
# Preconditions
# -----------------------------------------------------

required_objects <- c(
  "regression_data",
  "models_aez_raw_ols",
  "models_aez_iqr_trimmed_ols",
  "models_aez_top5_trimmed_ols",
  "eligible_aez_raw",
  "eligible_aez_iqr_trimmed",
  "eligible_aez_top5_trimmed",
  "regression_data_aez_raw",
  "regression_data_aez_iqr_trimmed",
  "regression_data_aez_top5_trimmed",
  "current_defor_transform",
  "current_run_stub",
  "tables_dir",
  "charts_dir"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    paste0(
      "05_diagnostics.R is missing required objects: ",
      paste(missing_objects, collapse = ", ")
    ),
    call. = FALSE
  )
}

# -----------------------------------------------------
# Choose regressor column for current run
# -----------------------------------------------------

regressor_col <- dplyr::case_when(
  current_defor_transform == "raw" ~ "delta_defor_ha",
  current_defor_transform == "log1p" ~ "log1p_delta_defor_ha",
  TRUE ~ NA_character_
)

if (is.na(regressor_col)) {
  stop("Unknown current_defor_transform: ", current_defor_transform, call. = FALSE)
}

transform_label <- paste0("defor_", current_defor_transform)

assert_has_cols(
  regression_data,
  c("AEZ", "delta_ov", regressor_col),
  "regression_data"
)

# -----------------------------------------------------
# Helper to coerce fixest fit statistics to plain numeric
# -----------------------------------------------------

extract_fitstat_numeric <- function(model, stat_name) {
  unname(as.numeric(fixest::fitstat(model, stat_name)))
}

# -----------------------------------------------------
# AEZ-level fit summary helper
# -----------------------------------------------------

build_aez_fit_summary <- function(models_list, data_used, model_label, regressor_col) {
  if (length(models_list) == 0) {
    return(tibble::tibble())
  }
  
  fit_summary <- purrr::imap_dfr(
    models_list,
    ~ {
      model_coef <- stats::coef(.x)
      
      tibble::tibble(
        AEZ = .y,
        model = model_label,
        regressor = regressor_col,
        n_obs = stats::nobs(.x),
        r_squared = extract_fitstat_numeric(.x, "r2"),
        adj_r_squared = extract_fitstat_numeric(.x, "ar2"),
        slope = if (regressor_col %in% names(model_coef)) {
          unname(model_coef[[regressor_col]])
        } else {
          NA_real_
        }
      )
    }
  )
  
  correlations <- data_used %>%
    dplyr::group_by(AEZ) %>%
    dplyr::summarise(
      n_obs_data = dplyr::n(),
      n_unique_regressor = dplyr::n_distinct(.data[[regressor_col]]),
      correlation = if (
        dplyr::n() >= 2 &&
        dplyr::n_distinct(.data[[regressor_col]]) >= 2 &&
        dplyr::n_distinct(delta_ov) >= 2
      ) {
        stats::cor(.data[[regressor_col]], delta_ov)
      } else {
        NA_real_
      },
      .groups = "drop"
    )
  
  fit_summary %>%
    dplyr::left_join(correlations, by = "AEZ")
}

# -----------------------------------------------------
# AEZ-level fit summaries
# -----------------------------------------------------

summary_aez_raw_ols <- build_aez_fit_summary(
  models_aez_raw_ols,
  regression_data_aez_raw,
  "ols_raw",
  regressor_col
)

summary_aez_iqr_trimmed_ols <- build_aez_fit_summary(
  models_aez_iqr_trimmed_ols,
  regression_data_aez_iqr_trimmed,
  "ols_iqr_trimmed",
  regressor_col
)

summary_aez_top5_trimmed_ols <- build_aez_fit_summary(
  models_aez_top5_trimmed_ols,
  regression_data_aez_top5_trimmed,
  "ols_top5_trimmed",
  regressor_col
)

if (nrow(summary_aez_raw_ols) > 0) {
  readr::write_csv(
    summary_aez_raw_ols,
    file.path(tables_dir, paste0("fit_statistics_raw__", transform_label, ".csv"))
  )
}

if (nrow(summary_aez_iqr_trimmed_ols) > 0) {
  readr::write_csv(
    summary_aez_iqr_trimmed_ols,
    file.path(tables_dir, paste0("fit_statistics_iqr_trimmed__", transform_label, ".csv"))
  )
}

if (nrow(summary_aez_top5_trimmed_ols) > 0) {
  readr::write_csv(
    summary_aez_top5_trimmed_ols,
    file.path(tables_dir, paste0("fit_statistics_top5_trimmed__", transform_label, ".csv"))
  )
}

# -----------------------------------------------------
# Combined AEZ comparison summary
# -----------------------------------------------------

summary_aez_comparison <- dplyr::bind_rows(
  summary_aez_raw_ols,
  summary_aez_iqr_trimmed_ols,
  summary_aez_top5_trimmed_ols
) %>%
  dplyr::arrange(readr::parse_number(as.character(AEZ)), AEZ, model)

if (nrow(summary_aez_comparison) > 0) {
  readr::write_csv(
    summary_aez_comparison,
    file.path(tables_dir, paste0("summary_aez_fit_statistics_comparison__", transform_label, ".csv"))
  )
}

# -----------------------------------------------------
# Main fitted scatterplots
# -----------------------------------------------------

if (length(models_aez_raw_ols) > 0) {
  plot_aez_raw_ols <- ggplot(
    regression_data_aez_raw,
    aes(x = .data[[regressor_col]], y = delta_ov)
  ) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~ AEZ, scales = "free") +
    labs(
      x = regressor_col,
      y = "delta_ov",
      title = paste0("AEZ-Specific Baseline OLS (", current_defor_transform, ")")
    ) +
    theme_minimal()
  
  ggsave(
    filename = file.path(charts_dir, paste0("aez_raw_ols__", transform_label, ".png")),
    plot = plot_aez_raw_ols,
    width = 12,
    height = 8,
    dpi = 300
  )
}

if (length(models_aez_iqr_trimmed_ols) > 0) {
  plot_aez_iqr_trimmed_ols <- ggplot(
    regression_data_aez_iqr_trimmed,
    aes(x = .data[[regressor_col]], y = delta_ov)
  ) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~ AEZ, scales = "free") +
    labs(
      x = regressor_col,
      y = "delta_ov",
      title = paste0("AEZ-Specific OLS after Within-AEZ IQR Trimming (", current_defor_transform, ")")
    ) +
    theme_minimal()
  
  ggsave(
    filename = file.path(charts_dir, paste0("aez_iqr_trimmed_ols__", transform_label, ".png")),
    plot = plot_aez_iqr_trimmed_ols,
    width = 12,
    height = 8,
    dpi = 300
  )
}

if (length(models_aez_top5_trimmed_ols) > 0) {
  plot_aez_top5_trimmed_ols <- ggplot(
    regression_data_aez_top5_trimmed,
    aes(x = .data[[regressor_col]], y = delta_ov)
  ) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~ AEZ, scales = "free") +
    labs(
      x = regressor_col,
      y = "delta_ov",
      title = paste0("AEZ-Specific OLS after Within-AEZ Top-5% Trimming (", current_defor_transform, ")")
    ) +
    theme_minimal()
  
  ggsave(
    filename = file.path(charts_dir, paste0("aez_top5_trimmed_ols__", transform_label, ".png")),
    plot = plot_aez_top5_trimmed_ols,
    width = 12,
    height = 8,
    dpi = 300
  )
}

# -----------------------------------------------------
# Distribution plots for current run
# -----------------------------------------------------

plot_hist_regressor <- ggplot(
  regression_data,
  aes(x = .data[[regressor_col]])
) +
  geom_histogram(bins = 40) +
  labs(
    x = regressor_col,
    y = "Count",
    title = paste0("Distribution of ", regressor_col)
  ) +
  theme_minimal()

plot_hist_delta_ov <- ggplot(
  regression_data,
  aes(x = delta_ov)
) +
  geom_histogram(bins = 40) +
  labs(
    x = "delta_ov",
    y = "Count",
    title = "Distribution of delta_ov"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(charts_dir, paste0("hist_regressor__", transform_label, ".png")),
  plot = plot_hist_regressor,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(charts_dir, paste0("hist_delta_ov__", transform_label, ".png")),
  plot = plot_hist_delta_ov,
  width = 8,
  height = 6,
  dpi = 300
)

message("Finished 05_diagnostics.R")
message("  current run: ", current_run_stub)
message("  regressor column: ", regressor_col)