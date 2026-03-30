# =====================================================
# 05_diagnostics.R
# Create summaries and plots for OLS models. 
# =====================================================

# -----------------------------------------------------
# Check whether required objects exist
# -----------------------------------------------------

required_objects <- c(
  "regression_data",,
  "models_aez_raw_ols",
  "models_aez_iqr_trimmed_ols",
  "models_aez_top5_trimmed_ols",
  "eligible_aez_raw",
  "eligible_aez_iqr_trimmed",
  "eligible_aez_top5_trimmed",
  "regression_data_aez_raw",
  "regression_data_aez_iqr_trimmed",
  "regression_data_aez_top5_trimmed"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    paste0(
      "The following required objects were not created in 04_ols_regression.R: ",
      paste(missing_objects, collapse = ", ")
    ),
    call. = FALSE
  )
}

# -----------------------------------------------------
# Helper to coerce fixest fit statistics to plain numeric
# -----------------------------------------------------

extract_fitstat_numeric <- function(model, stat_name) {
  unname(as.numeric(fitstat(model, stat_name)))
}

# -----------------------------------------------------
# AEZ-level fit summary helper
# -----------------------------------------------------

build_aez_fit_summary <- function(models_list, data_used, model_label) {
  if (length(models_list) == 0) {
    return(tibble())
  }
  
  fit_summary <- imap_dfr(
    models_list,
    ~ {
      model_coef <- coef(.x)
      
      tibble(
        AEZ = .y,
        model = model_label,
        n_obs = nobs(.x),
        r_squared = extract_fitstat_numeric(.x, "r2"),
        adj_r_squared = extract_fitstat_numeric(.x, "ar2"),
        slope_delta_defor_ha = if ("delta_defor_ha" %in% names(model_coef)) {
          unname(model_coef[["delta_defor_ha"]])
        } else {
          NA_real_
        }
      )
    }
  )
  
  correlations <- data_used %>%
    group_by(AEZ) %>%
    summarise(
      n_obs_data = n(),
      n_unique_delta_defor_ha = n_distinct(delta_defor_ha),
      correlation = if (n() >= 2 && n_distinct(delta_defor_ha) >= 2 && n_distinct(delta_ov) >= 2) {
        cor(delta_defor_ha, delta_ov)
      } else {
        NA_real_
      },
      .groups = "drop"
    )
  
  fit_summary %>%
    left_join(correlations, by = "AEZ")
}

# -----------------------------------------------------
# AEZ-level fit summaries
# -----------------------------------------------------

summary_aez_raw_ols <- build_aez_fit_summary(
  models_aez_raw_ols,
  regression_data_aez_raw,
  "ols_raw"
)

summary_aez_iqr_trimmed_ols <- build_aez_fit_summary(
  models_aez_iqr_trimmed_ols,
  regression_data_aez_iqr_trimmed,
  "ols_iqr_trimmed"
)

summary_aez_top5_trimmed_ols <- build_aez_fit_summary(
  models_aez_top5_trimmed_ols,
  regression_data_aez_top5_trimmed,
  "ols_top5_trimmed"
)

if (nrow(summary_aez_raw_ols) > 0) {
  write_csv(
    summary_aez_raw_ols,
    file.path(tables_dir, "fit_statistics_raw.csv")
  )
}

if (nrow(summary_aez_iqr_trimmed_ols) > 0) {
  write_csv(
    summary_aez_iqr_trimmed_ols,
    file.path(tables_dir, "fit_statistics_iqr_trimmed.csv")
  )
}

if (nrow(summary_aez_top5_trimmed_ols) > 0) {
  write_csv(
    summary_aez_top5_trimmed_ols,
    file.path(tables_dir, "fit_statistics_top5_trimmed.csv")
  )
}

# -----------------------------------------------------
# Combined AEZ comparison summary
# -----------------------------------------------------

summary_aez_comparison <- bind_rows(
  summary_aez_raw_ols,
  summary_aez_iqr_trimmed_ols,
  summary_aez_top5_trimmed_ols
) %>%
  arrange(as.numeric(str_extract(AEZ, "\\d+")))

if (nrow(summary_aez_comparison) > 0) {
  write_csv(
    summary_aez_comparison,
    file.path(tables_dir, "summary_aez_fit_statistics_comparison.csv")
  )
}

# -----------------------------------------------------
# Main fitted scatterplots
# -----------------------------------------------------

if (length(models_aez_raw_ols) > 0) {
  plot_aez_raw_ols <- ggplot(
    regression_data_aez_raw,
    aes(x = delta_defor_ha, y = delta_ov)
  ) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~ AEZ, scales = "free") +
    labs(
      x = "Change in deforestation (ha)",
      y = "Change in OV",
      title = "AEZ-Specific Raw OLS"
    )
  
  ggsave(
    filename = file.path(charts_dir, "aez_raw_ols.png"),
    plot = plot_aez_raw_ols,
    width = 12,
    height = 8,
    dpi = 300
  )
}

if (length(models_aez_iqr_trimmed_ols) > 0) {
  plot_aez_iqr_trimmed_ols <- ggplot(
    regression_data_aez_iqr_trimmed,
    aes(x = delta_defor_ha, y = delta_ov)
  ) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~ AEZ, scales = "free") +
    labs(
      x = "Change in deforestation (ha)",
      y = "Change in OV",
      title = "AEZ-Specific Raw OLS after Within-AEZ IQR Trimming"
    )
  
  ggsave(
    filename = file.path(charts_dir, "aez_iqr_trimmed_ols.png"),
    plot = plot_aez_iqr_trimmed_ols,
    width = 12,
    height = 8,
    dpi = 300
  )
}

if (length(models_aez_top5_trimmed_ols) > 0) {
  plot_aez_top5_trimmed_ols <- ggplot(
    regression_data_aez_top5_trimmed,
    aes(x = delta_defor_ha, y = delta_ov)
  ) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~ AEZ, scales = "free") +
    labs(
      x = "Change in deforestation (ha)",
      y = "Change in OV",
      title = "AEZ-Specific Raw OLS after Within-AEZ Top-5% Trimming"
    )
  
  ggsave(
    filename = file.path(charts_dir, "aez_top5_trimmed_ols.png"),
    plot = plot_aez_top5_trimmed_ols,
    width = 12,
    height = 8,
    dpi = 300
  )
}

# -----------------------------------------------------
# Distribution plots for baseline raw data
# -----------------------------------------------------

plot_hist_delta_defor_ha <- ggplot(
  regression_data_raw_complete,
  aes(x = delta_defor_ha)
) +
  geom_histogram(bins = 40) +
  labs(
    x = "Change in deforestation (ha)",
    y = "Count",
    title = "Distribution of Change in Deforestation"
  )

plot_hist_delta_ov <- ggplot(
  regression_data_raw_complete,
  aes(x = delta_ov)
) +
  geom_histogram(bins = 40) +
  labs(
    x = "Change in OV",
    y = "Count",
    title = "Distribution of Change in OV"
  )

ggsave(
  filename = file.path(charts_dir, "hist_delta_defor_ha.png"),
  plot = plot_hist_delta_defor_ha,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(charts_dir, "hist_delta_ov.png"),
  plot = plot_hist_delta_ov,
  width = 8,
  height = 6,
  dpi = 300
)

# -----------------------------------------------------
# Residual diagnostics for pooled raw OLS
# -----------------------------------------------------

plot_residuals_vs_fitted_raw_ols <- ggplot(
  pooled_raw_augmented,
  aes(x = .fitted, y = .resid)
) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted: Pooled Raw OLS"
  )

plot_residuals_hist_raw_ols <- ggplot(
  pooled_raw_augmented,
  aes(x = std_resid)
) +
  geom_histogram(bins = 40) +
  labs(
    x = "Standardized residuals",
    y = "Count",
    title = "Distribution of Standardized Residuals: Pooled Raw OLS"
  )

ggsave(
  filename = file.path(charts_dir, "residuals_vs_fitted_raw_ols.png"),
  plot = plot_residuals_vs_fitted_raw_ols,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(charts_dir, "residuals_hist_raw_ols.png"),
  plot = plot_residuals_hist_raw_ols,
  width = 8,
  height = 6,
  dpi = 300
)

message("Finished 05_diagnostics.R")