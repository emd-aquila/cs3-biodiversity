assert_file_exists <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " does not exist: ", path, call. = FALSE)
  }
}

assert_has_cols <- function(data, cols, label) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

write_csv_safe <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, path)
  message("Wrote: ", path)
  invisible(path)
}

rmse <- function(model) {
  sqrt(mean(stats::residuals(model)^2, na.rm = TRUE))
}

model_nobs <- function(model) {
  out <- tryCatch(stats::nobs(model), error = function(err) NA_integer_)
  if (is.na(out)) {
    out <- length(stats::residuals(model))
  }
  as.integer(out)
}

fit_stats_lm <- function(model, model_name) {
  fit <- model
  glance <- broom::glance(fit)
  tibble::tibble(
    model = model_name,
    model_class = paste(class(fit), collapse = ";"),
    n_obs = model_nobs(fit),
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    logLik = as.numeric(stats::logLik(fit)),
    rmse = rmse(fit),
    r2 = glance$r.squared %||% NA_real_,
    adj_r2 = glance$adj.r.squared %||% NA_real_,
    r2_marginal = NA_real_,
    r2_conditional = NA_real_,
    singular = NA,
    notes = NA_character_
  )
}

fit_stats_lmer <- function(model, model_name) {
  fit <- model
  r2_values <- tryCatch(performance::r2(fit), error = function(err) NULL)
  r2_marginal <- NA_real_
  r2_conditional <- NA_real_
  if (!is.null(r2_values)) {
    r2_marginal <- as.numeric(r2_values$R2_marginal[1])
    r2_conditional <- as.numeric(r2_values$R2_conditional[1])
  }

  tibble::tibble(
    model = model_name,
    model_class = paste(class(fit), collapse = ";"),
    n_obs = model_nobs(fit),
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    logLik = as.numeric(stats::logLik(fit)),
    rmse = rmse(fit),
    r2 = NA_real_,
    adj_r2 = NA_real_,
    r2_marginal = r2_marginal,
    r2_conditional = r2_conditional,
    singular = lme4::isSingular(fit),
    notes = NA_character_
  )
}

fit_stats_gam <- function(model, model_name) {
  fit <- model
  summary_model <- summary(fit)
  tibble::tibble(
    model = model_name,
    model_class = paste(class(fit), collapse = ";"),
    n_obs = model_nobs(fit),
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    logLik = as.numeric(stats::logLik(fit)),
    rmse = rmse(fit),
    r2 = summary_model$r.sq,
    adj_r2 = summary_model$r.sq,
    r2_marginal = NA_real_,
    r2_conditional = NA_real_,
    singular = NA,
    notes = NA_character_
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

safe_lmer <- function(formula, data, model_name) {
  tryCatch(
    lme4::lmer(
      formula,
      data = data,
      REML = FALSE,
      control = lme4::lmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)
      )
    ),
    error = function(err) {
      warning(model_name, " failed: ", conditionMessage(err))
      NULL
    }
  )
}

level_has_enough_rows <- function(data, group_col, min_rows) {
  data |>
    dplyr::count(.data[[group_col]], name = "n") |>
    dplyr::filter(!is.na(.data[[group_col]]), n >= min_rows) |>
    dplyr::pull(.data[[group_col]])
}

ordered_aez_levels <- function(x) {
  values <- unique(as.character(x))
  numeric_key <- suppressWarnings(as.integer(gsub("[^0-9]+", "", values)))
  values[order(is.na(numeric_key), numeric_key, values)]
}

set_aez_order <- function(data) {
  if ("AEZ" %in% names(data)) {
    data$AEZ <- factor(as.character(data$AEZ), levels = ordered_aez_levels(data$AEZ))
  }
  data
}

tidy_model_parameters <- function(model, model_name) {
  if (inherits(model, "lm") && !inherits(model, "gam")) {
    return(
      broom::tidy(model, conf.int = TRUE) |>
        dplyr::mutate(model = model_name, component = "fixed") |>
        dplyr::select(model, component, dplyr::everything())
    )
  }

  if (inherits(model, "merMod")) {
    out <- broom.mixed::tidy(model, effects = c("fixed", "ran_pars"), conf.int = FALSE)
    out$component <- out$effect
    out$model <- model_name
    return(
      out |>
        dplyr::select(model, component, dplyr::everything(), -effect)
    )
  }

  if (inherits(model, "gam")) {
    parametric <- broom::tidy(model, parametric = TRUE) |>
      dplyr::mutate(model = model_name, component = "parametric")
    smooth <- broom::tidy(model, parametric = FALSE) |>
      dplyr::mutate(model = model_name, component = "smooth")

    return(
      dplyr::bind_rows(parametric, smooth) |>
        dplyr::select(model, component, dplyr::everything())
    )
  }

  tibble::tibble(model = model_name, component = "unknown")
}

predict_line_lm <- function(model, data, group_cols = NULL, n = 100L) {
  if (is.null(group_cols)) {
    newdata <- tibble::tibble(
      .predictor_value = seq(
        min(data[[predictor_col]], na.rm = TRUE),
        max(data[[predictor_col]], na.rm = TRUE),
        length.out = n
      )
    )
    names(newdata)[names(newdata) == ".predictor_value"] <- predictor_col
    newdata$prediction <- as.numeric(stats::predict(model, newdata = newdata))
    return(newdata)
  }

  grid <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      x_min = min(.data[[predictor_col]], na.rm = TRUE),
      x_max = max(.data[[predictor_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(.predictor_value = list(seq(x_min, x_max, length.out = n))) |>
    tidyr::unnest(.predictor_value) |>
    dplyr::select(-x_min, -x_max) |>
    dplyr::ungroup()
  names(grid)[names(grid) == ".predictor_value"] <- predictor_col

  grid$prediction <- as.numeric(stats::predict(model, newdata = grid, allow.new.levels = TRUE))
  grid
}

prediction_r2 <- function(observed, predicted) {
  ok <- is.finite(observed) & is.finite(predicted)
  observed <- observed[ok]
  predicted <- predicted[ok]
  if (length(observed) < 2 || stats::var(observed, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  1 - sum((observed - predicted)^2, na.rm = TRUE) /
    sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
}

model_prediction_r2 <- function(model, data, model_name, group_col = NULL) {
  fitted <- tryCatch(
    as.numeric(stats::predict(model, newdata = data, allow.new.levels = TRUE)),
    error = function(err) as.numeric(stats::predict(model, newdata = data))
  )
  df <- data |>
    dplyr::mutate(.prediction = fitted)

  if (is.null(group_col)) {
    return(
      tibble::tibble(
        model = model_name,
        group_type = "global",
        group = "global",
        prediction_r2 = prediction_r2(df[[response_col]], df$.prediction)
      )
    )
  }

  df |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarise(
      prediction_r2 = prediction_r2(.data[[response_col]], .prediction),
      .groups = "drop"
    ) |>
    dplyr::transmute(
      model = model_name,
      group_type = group_col,
      group = as.character(.data[[group_col]]),
      prediction_r2
    )
}
