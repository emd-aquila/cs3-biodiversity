# =====================================================
# Helper functions for regression pipeline
# =====================================================

assert_exists <- function(path) {
  if (!file.exists(path)) {
    stop("Missing required file: ", path, call. = FALSE)
  }
}

assert_has_cols <- function(data, cols, data_name = deparse(substitute(data))) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        data_name,
        " is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

save_model_rds <- function(model, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(model, path)
  message("Wrote model: ", path)
}

standardize_aez_order <- function(x) {
  x_chr <- as.character(x)
  x_num <- readr::parse_number(x_chr)
  
  ord <- order(is.na(x_num), x_num, x_chr)
  factor(x_chr, levels = unique(x_chr[ord]))
}

sanitize_filename <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
}

coerce_cluster_deltas_types <- function(df) {
  assert_has_cols(df, cluster_deltas_required_cols, "cluster_deltas")
  
  if (!"n_matched_tiles_with_ha" %in% names(df)) {
    df$n_matched_tiles_with_ha <- NA_real_
  }
  
  if (!"medoid_latitude" %in% names(df)) {
    df$medoid_latitude <- NA_real_
  }
  
  if (!"medoid_longitude" %in% names(df)) {
    df$medoid_longitude <- NA_real_
  }
  
  df %>%
    mutate(
      AEZ = as.character(AEZ),
      cluster_id = as.character(cluster_id),
      year_t1 = as.integer(year_t1),
      year_t2 = as.integer(year_t2),
      year_gap = as.integer(year_gap),
      ov_t1 = as.numeric(ov_t1),
      ov_t2 = as.numeric(ov_t2),
      delta_ov = as.numeric(delta_ov),
      n_sites_t1 = as.numeric(n_sites_t1),
      n_sites_t2 = as.numeric(n_sites_t2),
      delta_defor_ha = as.numeric(delta_defor_ha),
      delta_defor_ha_annualized = as.numeric(delta_defor_ha_annualized),
      n_matched_tiles_with_ha = as.numeric(n_matched_tiles_with_ha),
      medoid_latitude = as.numeric(medoid_latitude),
      medoid_longitude = as.numeric(medoid_longitude)
    )
}

build_regression_data <- function(cluster_deltas) {
  assert_has_cols(cluster_deltas, cluster_deltas_required_cols, "cluster_deltas")
  
  cluster_deltas %>%
    coerce_cluster_deltas_types() %>%
    mutate(
      AEZ = standardize_aez_order(AEZ),
      delta_ov_annualized = if_else(
        !is.na(year_gap) & year_gap > 0,
        delta_ov / year_gap,
        NA_real_
      ),
      mean_n_sites = (n_sites_t1 + n_sites_t2) / 2
    ) %>%
    arrange(
      AEZ, cluster_id, year_t1, year_t2,
      ov_t1, ov_t2, delta_ov, delta_defor_ha,
      delta_defor_ha_annualized, inverse_change,
      n_sites_t1, n_sites_t2, n_matched_tiles_with_ha,
      medoid_latitude, medoid_longitude
    )
}


fit_ols_by_aez_current <- function(data, regressor_col) {
  assert_has_cols(data, c("AEZ", "delta_ov", regressor_col), "data")
  
  data_split <- data %>%
    group_by(AEZ) %>%
    group_split(.keep = TRUE)
  
  aez_names <- data %>%
    group_by(AEZ) %>%
    group_keys() %>%
    pull(AEZ) %>%
    as.character()
  
  model_formula <- stats::as.formula(
    paste("delta_ov ~", regressor_col)
  )
  
  map(
    rlang::set_names(data_split, aez_names),
    ~ feols(model_formula, data = .x)
  )
}

fit_annualized_ols <- function(data) {
  required_cols <- c("delta_ov_annualized", "delta_defor_ha_annualized")
  assert_has_cols(data, required_cols, "data")
  
  feols(
    delta_ov_annualized ~ delta_defor_ha_annualized,
    data = data
  )
}

fit_annualized_ols_by_aez <- function(data) {
  required_cols <- c("AEZ", "delta_ov_annualized", "delta_defor_ha_annualized")
  assert_has_cols(data, required_cols, "data")
  
  data_filtered <- data %>%
    filter(
      !is.na(AEZ),
      !is.na(delta_ov_annualized),
      !is.na(delta_defor_ha_annualized)
    )
  
  data_split <- data_filtered %>%
    group_by(AEZ) %>%
    group_split(.keep = TRUE)
  
  aez_names <- data_filtered %>%
    group_by(AEZ) %>%
    group_keys() %>%
    pull(AEZ) %>%
    as.character()
  
  map(
    set_names(data_split, aez_names),
    ~ feols(delta_ov_annualized ~ delta_defor_ha_annualized, data = .x)
  )
}

# functions to save models and summary tables
save_model_rds <- function(model, path) {
  saveRDS(model, path)
  message("Wrote model: ", path)
}

save_modelsummary_table <- function(models, path, title = NULL, ...) {
  modelsummary::modelsummary(
    models,
    output = path,
    title = title,
    gof_omit = "IC|Log|Adj|Within|Pseudo|Std.Errors",
    ...
  )
  
  message("Wrote table: ", path)
}