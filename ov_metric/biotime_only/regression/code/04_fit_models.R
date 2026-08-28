fit_models_for_variant <- function(variant_key, variant_label) {
  variant_dir <- file.path(output_dir, variant_key)
  variant_model_dir <- file.path(variant_dir, "models")
  variant_table_dir <- file.path(variant_dir, "tables")
  variant_prediction_dir <- file.path(variant_dir, "predictions")
  dir.create(variant_model_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(variant_table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(variant_prediction_dir, recursive = TRUE, showWarnings = FALSE)

  dataset_path <- file.path(variant_dir, "biotime_regression_dataset.csv")
  assert_file_exists(dataset_path, paste(variant_label, "regression dataset"))

  regression_data <- readr::read_csv(dataset_path, show_col_types = FALSE) |>
    dplyr::mutate(
      AEZ = as.factor(AEZ),
      taxon_group = as.factor(taxon_group)
    )
  regression_data <- set_aez_order(regression_data)

  if (nrow(regression_data) < 5) {
    stop(variant_label, " has too few regression rows to fit requested models.", call. = FALSE)
  }

  model_formula <- stats::as.formula(paste(response_col, "~", predictor_col))
  mixed_aez_formula <- stats::as.formula(paste(response_col, "~", predictor_col, "+ (1 +", predictor_col, "|| AEZ)"))
  mixed_taxon_formula <- stats::as.formula(paste(response_col, "~", predictor_col, "+ (1 +", predictor_col, "|| taxon_group)"))
  mixed_aez_taxon_formula <- stats::as.formula(paste(response_col, "~", predictor_col, "+ (1 +", predictor_col, "|| AEZ) + (1 +", predictor_col, "|| taxon_group)"))

  message("Fitting ", variant_label, ": Global Linear model")
  global_linear <- stats::lm(model_formula, data = regression_data)

  message("Fitting ", variant_label, ": Mixed AEZ model")
  mixed_aez <- safe_lmer(mixed_aez_formula, regression_data, paste(variant_label, "Mixed AEZ"))

  message("Fitting ", variant_label, ": Mixed Taxon model")
  mixed_taxon <- safe_lmer(mixed_taxon_formula, regression_data, paste(variant_label, "Mixed Taxon"))

  message("Fitting ", variant_label, ": Mixed AEZ Taxon model")
  mixed_aez_taxon <- safe_lmer(mixed_aez_taxon_formula, regression_data, paste(variant_label, "Mixed AEZ Taxon"))

  model_objects <- list(
    global_linear = global_linear,
    mixed_aez = mixed_aez,
    mixed_taxon = mixed_taxon,
    mixed_aez_taxon = mixed_aez_taxon
  )
  model_objects <- model_objects[!vapply(model_objects, is.null, logical(1))]

  purrr::iwalk(model_objects, function(model, name) {
    saveRDS(model, file.path(variant_model_dir, paste0(name, ".rds")))
  })

  message("Fitting ", variant_label, ": by-AEZ GAM/spline models")
  gam_results <- regression_data |>
    dplyr::group_split(AEZ) |>
    purrr::map(function(df) {
      aez_value <- as.character(df$AEZ[1])
      n_unique_x <- dplyr::n_distinct(df[[predictor_col]])

      linear_model <- stats::lm(model_formula, data = df)

      if (nrow(df) < min_gam_rows || n_unique_x < min_gam_unique_x) {
        return(list(
          AEZ = aez_value,
          data = df,
          linear = linear_model,
          gam = NULL,
          note = "Insufficient rows or unique deforestation values for GAM"
        ))
      }

      k_value <- max(3L, min(6L, n_unique_x - 1L, floor(nrow(df) / 4L)))
      gam_formula <- stats::as.formula(paste(response_col, "~ s(", predictor_col, ", k = ", k_value, ")", sep = ""))
      gam_model <- tryCatch(
        mgcv::gam(gam_formula, data = df, method = "REML"),
        error = function(err) {
          warning("GAM failed for ", variant_label, " / ", aez_value, ": ", conditionMessage(err))
          NULL
        }
      )

      list(
        AEZ = aez_value,
        data = df,
        linear = linear_model,
        gam = gam_model,
        note = if (is.null(gam_model)) "GAM failed" else NA_character_
      )
    })

  names(gam_results) <- vapply(gam_results, `[[`, character(1), "AEZ")
  saveRDS(gam_results, file.path(variant_model_dir, "gam_aez_models.rds"))

  fit_stats <- list(
    fit_stats_lm(global_linear, "Global Linear")
  )

  if (!is.null(mixed_aez)) {
    fit_stats <- c(fit_stats, list(fit_stats_lmer(mixed_aez, "Mixed AEZ")))
  }
  if (!is.null(mixed_taxon)) {
    fit_stats <- c(fit_stats, list(fit_stats_lmer(mixed_taxon, "Mixed Taxon")))
  }
  if (!is.null(mixed_aez_taxon)) {
    fit_stats <- c(fit_stats, list(fit_stats_lmer(mixed_aez_taxon, "Mixed AEZ Taxon")))
  }

  aez_gam_comparison <- purrr::map_dfr(gam_results, function(result) {
    linear_stats <- fit_stats_lm(result$linear, paste0("Linear AEZ ", result$AEZ))
    if (is.null(result$gam)) {
      return(
        tibble::tibble(
          AEZ = result$AEZ,
          n_obs = model_nobs(result$linear),
          linear_aic = stats::AIC(result$linear),
          gam_aic = NA_real_,
          delta_aic_gam_minus_linear = NA_real_,
          linear_rmse = rmse(result$linear),
          gam_rmse = NA_real_,
          linear_r2 = linear_stats$r2,
          gam_r2 = NA_real_,
          gam_edf = NA_real_,
          note = result$note
        )
      )
    }

    gam_summary <- summary(result$gam)
    tibble::tibble(
      AEZ = result$AEZ,
      n_obs = model_nobs(result$linear),
      linear_aic = stats::AIC(result$linear),
      gam_aic = stats::AIC(result$gam),
      delta_aic_gam_minus_linear = stats::AIC(result$gam) - stats::AIC(result$linear),
      linear_rmse = rmse(result$linear),
      gam_rmse = rmse(result$gam),
      linear_r2 = linear_stats$r2,
      gam_r2 = gam_summary$r.sq,
      gam_edf = sum(gam_summary$edf),
      note = result$note
    )
  })

  gam_fit_stats <- purrr::map_dfr(gam_results, function(result) {
    if (is.null(result$gam)) {
      return(tibble::tibble())
    }
    fit_stats_gam(result$gam, paste0("GAM AEZ ", result$AEZ))
  })

  all_fit_stats <- dplyr::bind_rows(fit_stats, gam_fit_stats) |>
    dplyr::mutate(
      variant_key = variant_key,
      variant_label = variant_label,
      .before = 1
    )

  all_parameters <- purrr::imap_dfr(model_objects, function(model, name) {
    label <- dplyr::case_when(
      name == "global_linear" ~ "Global Linear",
      name == "mixed_aez" ~ "Mixed AEZ",
      name == "mixed_taxon" ~ "Mixed Taxon",
      name == "mixed_aez_taxon" ~ "Mixed AEZ Taxon",
      TRUE ~ name
    )
    tidy_model_parameters(model, label)
  })

  gam_parameters <- purrr::map_dfr(gam_results, function(result) {
    if (is.null(result$gam)) {
      return(tibble::tibble())
    }
    tidy_model_parameters(result$gam, paste0("GAM AEZ ", result$AEZ))
  })

  all_parameters <- dplyr::bind_rows(all_parameters, gam_parameters) |>
    dplyr::mutate(
      variant_key = variant_key,
      variant_label = variant_label,
      .before = 1
    )

  aez_gam_comparison <- aez_gam_comparison |>
    dplyr::mutate(
      variant_key = variant_key,
      variant_label = variant_label,
      .before = 1
    )

  write_csv_safe(all_fit_stats, file.path(variant_table_dir, "model_fit_stats.csv"))
  write_csv_safe(all_parameters, file.path(variant_table_dir, "model_parameters.csv"))
  write_csv_safe(aez_gam_comparison, file.path(variant_table_dir, "aez_gam_linear_comparison.csv"))

  global_predictions <- predict_line_lm(global_linear, regression_data)
  write_csv_safe(global_predictions, file.path(variant_prediction_dir, "global_linear_predictions.csv"))

  plot_r2 <- list(
    model_prediction_r2(global_linear, regression_data, "Global Linear")
  )

  if (!is.null(mixed_aez)) {
    mixed_aez_predictions <- predict_line_lm(mixed_aez, regression_data, group_cols = "AEZ")
    write_csv_safe(mixed_aez_predictions, file.path(variant_prediction_dir, "mixed_aez_predictions.csv"))
    plot_r2 <- c(plot_r2, list(model_prediction_r2(mixed_aez, regression_data, "Mixed AEZ", group_col = "AEZ")))
  }

  if (!is.null(mixed_taxon)) {
    mixed_taxon_predictions <- predict_line_lm(mixed_taxon, regression_data, group_cols = "taxon_group")
    write_csv_safe(mixed_taxon_predictions, file.path(variant_prediction_dir, "mixed_taxon_predictions.csv"))
    plot_r2 <- c(plot_r2, list(model_prediction_r2(mixed_taxon, regression_data, "Mixed Taxon", group_col = "taxon_group")))
  }

  gam_predictions <- purrr::map_dfr(gam_results, function(result) {
    model <- result$gam %||% result$linear
    df <- result$data
    x_values <- seq(
      min(df[[predictor_col]], na.rm = TRUE),
      max(df[[predictor_col]], na.rm = TRUE),
      length.out = 100
    )
    newdata <- tibble::tibble(
      AEZ = rep(result$AEZ, length(x_values))
    )
    newdata[[predictor_col]] <- x_values
    newdata$prediction <- as.numeric(stats::predict(model, newdata = newdata))
    newdata$model_type <- if (is.null(result$gam)) "linear_fallback" else "gam"
    newdata
  })
  write_csv_safe(gam_predictions, file.path(variant_prediction_dir, "gam_aez_predictions.csv"))

  gam_plot_r2 <- aez_gam_comparison |>
    dplyr::transmute(
      model = "GAM AEZ",
      group_type = "AEZ",
      group = as.character(AEZ),
      prediction_r2 = dplyr::coalesce(gam_r2, linear_r2)
    )
  plot_r2 <- dplyr::bind_rows(plot_r2, gam_plot_r2) |>
    dplyr::mutate(
      variant_key = variant_key,
      variant_label = variant_label,
      .before = 1
    )
  write_csv_safe(plot_r2, file.path(variant_table_dir, "plot_r2_annotations.csv"))

  if (identical(variant_key, "all_delta_ov")) {
    purrr::iwalk(model_objects, function(model, name) {
      saveRDS(model, file.path(model_dir, paste0(name, ".rds")))
    })
    saveRDS(gam_results, file.path(model_dir, "gam_aez_models.rds"))
    write_csv_safe(all_fit_stats, model_fit_stats_path)
    write_csv_safe(all_parameters, model_parameters_path)
    write_csv_safe(aez_gam_comparison, aez_gam_comparison_path)
    write_csv_safe(global_predictions, file.path(prediction_dir, "global_linear_predictions.csv"))
    if (exists("mixed_aez_predictions", inherits = FALSE)) {
      write_csv_safe(mixed_aez_predictions, file.path(prediction_dir, "mixed_aez_predictions.csv"))
    }
    if (exists("mixed_taxon_predictions", inherits = FALSE)) {
      write_csv_safe(mixed_taxon_predictions, file.path(prediction_dir, "mixed_taxon_predictions.csv"))
    }
    write_csv_safe(gam_predictions, file.path(prediction_dir, "gam_aez_predictions.csv"))
    write_csv_safe(plot_r2, file.path(table_dir, "plot_r2_annotations.csv"))
  }

  invisible(TRUE)
}

purrr::pwalk(
  regression_variants[, c("variant_key", "variant_label")],
  ~ fit_models_for_variant(..1, ..2)
)

message("Regression modeling complete.")
