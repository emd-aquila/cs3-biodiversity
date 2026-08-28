# =====================================================
# Helper functions for analysis pipeline
# =====================================================

# -----------------------
# Assertions
# -----------------------

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
        data_name, " is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

# -----------------------
# Safe writers
# -----------------------

write_csv_safe <- function(df, path) {
  readr::write_csv(df, path)
  message("Wrote: ", path)
}

write_rds_safe <- function(object, path) {
  saveRDS(object, path)
  message("Wrote: ", path)
}

write_plot_safe <- function(plot_obj, path, width = 10, height = 6, dpi = 300) {
  ggplot2::ggsave(
    filename = path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
  message("Wrote: ", path)
}

# -----------------------
# Ordering helpers
# -----------------------

standardize_aez_order <- function(x) {
  x_chr <- as.character(x)
  x_num <- readr::parse_number(x_chr)
  factor(x_chr, levels = unique(x_chr[order(x_num, x_chr)]))
}

# -----------------------
# Summary helpers
# -----------------------

calc_minmax <- function(x, na.rm = TRUE) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  max(x, na.rm = na.rm) - min(x, na.rm = na.rm)
}

calc_iqr <- function(x, na.rm = TRUE) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  stats::IQR(x, na.rm = na.rm)
}

round_numeric_cols <- function(df, digits = 1) {
  df %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, digits)
      )
    )
}

round_numeric_cols_except <- function(df, digits = 1, exclude = character()) {
  df %>%
    mutate(
      across(
        where(is.numeric) & !any_of(exclude),
        ~ round(.x, digits)
      )
    )
}

log_buffer_run <- function(buffer_km) {
  message("\n==============================")
  message("Running analysis for buffer_km = ", buffer_km)
  message("==============================")
}

buffer_key <- function(buffer_km) {
  paste0("buf_", buffer_km, "km")
}

log_cluster_run <- function(cluster_method, cluster_radius_km) {
  message("\n======================================")
  message(
    "Running analysis for cluster config: ",
    cluster_method,
    " / ",
    sprintf("%.1f", cluster_radius_km),
    " km"
  )
  message("======================================")
}

assert_objects_exist <- function(object_names, script_name = "script") {
  missing_objects <- object_names[!vapply(object_names, exists, logical(1))]
  
  if (length(missing_objects) > 0) {
    stop(
      paste0(
        script_name,
        " is missing required objects: ",
        paste(missing_objects, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

get_available_ov_score_specs <- function(cluster_year_ov) {
  assert_has_cols(
    cluster_year_ov,
    c("AEZ", "cluster_id", "year", "median_ov_year", "n_sites_year"),
    "cluster_year_ov"
  )

  specs <- ov_score_specs %>%
    filter(cluster_year_col %in% names(cluster_year_ov))

  if (!"ov_full" %in% specs$ov_method) {
    stop("cluster_year_ov is missing the required full-OV column: median_ov_year", call. = FALSE)
  }

  missing_variant_cols <- setdiff(ov_score_specs$cluster_year_col, names(cluster_year_ov))
  if (length(missing_variant_cols) > 0) {
    warning(
      "Some OV variant columns are not available in cluster_year_ov and will be skipped: ",
      paste(missing_variant_cols, collapse = ", "),
      call. = FALSE
    )
  }

  specs
}

build_analysis_unit_tables <- function(cluster_sites,
                                       cluster_buffer_this,
                                       cluster_buffer_tile_this,
                                       cluster_buffer_country_this,
                                       cluster_buffer_year_defor_this,
                                       cluster_medoids,
                                       defor_tile_year,
                                       ov_score_specs,
                                       collapse_single_tile_clusters = TRUE) {
  cluster_buffer_meta_original <- cluster_buffer_this %>%
    st_drop_geometry() %>%
    distinct(
      AEZ,
      cluster_id,
      buffer_km,
      n_sites,
      n_matched_tiles,
      n_matched_tiles_with_ha,
      n_matched_tiles_missing_ha,
      tagged_any_tile,
      tagged_ha_tile,
      hansen_land_area_ha,
      hansen_treecover2000_equiv_ha,
      hansen_treecover2000_mean_pct
    ) %>%
    mutate(
      original_cluster_id = as.character(cluster_id)
    )

  cluster_tile_summary <- cluster_buffer_tile_this %>%
    group_by(AEZ, cluster_id, buffer_km) %>%
    summarise(
      n_all_matched_tiles = n_distinct(tile_id),
      n_ha_matched_tiles = n_distinct(tile_id[has_ha_info %in% TRUE]),
      single_tile_id = dplyr::if_else(
        n_distinct(tile_id) == 1L,
        dplyr::first(as.character(tile_id)),
        NA_character_
      ),
      single_tile_has_ha = dplyr::if_else(
        n_distinct(tile_id) == 1L,
        dplyr::first(has_ha_info) %in% TRUE,
        FALSE
      ),
      .groups = "drop"
    ) %>%
    mutate(
      original_cluster_id = as.character(cluster_id)
    ) %>%
    dplyr::select(-cluster_id)

  analysis_unit_membership <- cluster_buffer_meta_original %>%
    left_join(
      cluster_tile_summary,
      by = c("AEZ", "original_cluster_id", "buffer_km")
    ) %>%
    mutate(
      n_all_matched_tiles = replace_na(n_all_matched_tiles, 0L),
      n_ha_matched_tiles = replace_na(n_ha_matched_tiles, 0L),
      single_tile_has_ha = replace_na(single_tile_has_ha, FALSE),
      collapse_to_single_tile = isTRUE(collapse_single_tile_clusters) &
        n_all_matched_tiles == 1L,
      analysis_unit_type = dplyr::if_else(
        collapse_to_single_tile,
        "single_tile",
        "cluster"
      ),
      collapse_tile_id = dplyr::if_else(
        collapse_to_single_tile,
        single_tile_id,
        NA_character_
      ),
      analysis_unit_id = dplyr::if_else(
        collapse_to_single_tile,
        paste0(as.character(AEZ), "_TILE_", collapse_tile_id),
        original_cluster_id
      )
    ) %>%
    dplyr::select(
      AEZ,
      buffer_km,
      original_cluster_id,
      analysis_unit_id,
      analysis_unit_type,
      collapse_tile_id,
      collapse_to_single_tile
    )

  analysis_unit_buffer_country <- cluster_buffer_country_this %>%
    mutate(original_cluster_id = as.character(cluster_id)) %>%
    inner_join(
      analysis_unit_membership %>%
        dplyr::select(AEZ, buffer_km, original_cluster_id, analysis_unit_id),
      by = c("AEZ", "buffer_km", "original_cluster_id")
    ) %>%
    group_by(
      AEZ,
      cluster_id = analysis_unit_id,
      buffer_km,
      country_id,
      country_iso3,
      country_name,
      country_name_long,
      sovereign_name
    ) %>%
    summarise(
      country_intersection_area_ha = sum(country_intersection_area_ha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(AEZ, cluster_id, buffer_km) %>%
    mutate(
      normalized_country_overlap_share =
        country_intersection_area_ha / sum(country_intersection_area_ha, na.rm = TRUE)
    ) %>%
    ungroup()

  analysis_unit_country_summary <- analysis_unit_buffer_country %>%
    arrange(
      AEZ,
      cluster_id,
      buffer_km,
      desc(normalized_country_overlap_share),
      country_name
    ) %>%
    group_by(AEZ, cluster_id, buffer_km) %>%
    summarise(
      n_matched_countries = n_distinct(country_id),
      primary_country_id = dplyr::first(country_id),
      primary_country_iso3 = dplyr::first(country_iso3),
      primary_country_name = dplyr::first(country_name),
      primary_country_name_long = dplyr::first(country_name_long),
      primary_sovereign_name = dplyr::first(sovereign_name),
      primary_country_overlap_share = dplyr::first(normalized_country_overlap_share),
      matched_country_ids = paste(sort(unique(country_id)), collapse = ";"),
      matched_country_names = paste(sort(unique(country_name)), collapse = ";"),
      .groups = "drop"
    )

  analysis_unit_summary <- analysis_unit_membership %>%
    group_by(AEZ, cluster_id = analysis_unit_id, buffer_km) %>%
    summarise(
      analysis_unit_type = dplyr::first(analysis_unit_type),
      collapse_tile_id = dplyr::first(collapse_tile_id),
      n_member_clusters = n_distinct(original_cluster_id),
      member_cluster_ids = paste(sort(unique(original_cluster_id)), collapse = ";"),
      .groups = "drop"
    ) %>%
    left_join(
      analysis_unit_country_summary,
      by = c("AEZ", "cluster_id", "buffer_km")
    )

  analysis_unit_sites <- cluster_sites %>%
    mutate(original_cluster_id = as.character(cluster_id)) %>%
    inner_join(
      analysis_unit_membership %>%
        dplyr::select(AEZ, original_cluster_id, analysis_unit_id, analysis_unit_type, collapse_tile_id),
      by = c("AEZ", "original_cluster_id")
    ) %>%
    mutate(
      cluster_id = analysis_unit_id
    )

  available_site_specs <- ov_score_specs %>%
    filter(site_col %in% names(analysis_unit_sites))

  if (!"ov_full" %in% available_site_specs$ov_method) {
    stop("cluster_sites is missing the required full-OV site column for analysis-unit medians.", call. = FALSE)
  }

  median_exprs <- purrr::map(
    available_site_specs$site_col,
    ~ rlang::expr(median(.data[[!!.x]], na.rm = TRUE))
  )
  names(median_exprs) <- available_site_specs$cluster_year_col

  analysis_unit_year_ov <- analysis_unit_sites %>%
    st_drop_geometry() %>%
    group_by(AEZ, cluster_id, year) %>%
    summarise(
      !!!median_exprs,
      n_sites_year = n(),
      .groups = "drop"
    ) %>%
    arrange(AEZ, cluster_id, year)

  analysis_unit_buffer_meta <- cluster_buffer_meta_original %>%
    dplyr::select(
      AEZ,
      buffer_km,
      original_cluster_id,
      n_sites,
      n_matched_tiles,
      n_matched_tiles_with_ha,
      n_matched_tiles_missing_ha,
      tagged_any_tile,
      tagged_ha_tile,
      hansen_land_area_ha,
      hansen_treecover2000_equiv_ha,
      hansen_treecover2000_mean_pct
    ) %>%
    inner_join(
      analysis_unit_membership %>%
        dplyr::select(
          AEZ,
          buffer_km,
          original_cluster_id,
          analysis_unit_id,
          analysis_unit_type,
          collapse_tile_id
        ),
      by = c("AEZ", "buffer_km", "original_cluster_id")
    ) %>%
    group_by(AEZ, cluster_id = analysis_unit_id, buffer_km) %>%
    summarise(
      n_sites = sum(n_sites, na.rm = TRUE),
      n_matched_tiles = dplyr::if_else(
        dplyr::first(analysis_unit_type) == "single_tile",
        1L,
        dplyr::first(n_matched_tiles)
      ),
      n_matched_tiles_with_ha = dplyr::if_else(
        dplyr::first(analysis_unit_type) == "single_tile",
        dplyr::first(n_matched_tiles_with_ha),
        dplyr::first(n_matched_tiles_with_ha)
      ),
      n_matched_tiles_missing_ha = dplyr::if_else(
        dplyr::first(analysis_unit_type) == "single_tile",
        dplyr::first(n_matched_tiles_missing_ha),
        dplyr::first(n_matched_tiles_missing_ha)
      ),
      tagged_any_tile = any(tagged_any_tile, na.rm = TRUE),
      tagged_ha_tile = any(tagged_ha_tile, na.rm = TRUE),
      hansen_land_area_ha = sum(hansen_land_area_ha, na.rm = TRUE),
      hansen_treecover2000_equiv_ha = sum(hansen_treecover2000_equiv_ha, na.rm = TRUE),
      hansen_treecover2000_mean_pct = dplyr::if_else(
        hansen_land_area_ha > 0,
        100 * hansen_treecover2000_equiv_ha / hansen_land_area_ha,
        NA_real_
      ),
      analysis_unit_type = dplyr::first(analysis_unit_type),
      collapse_tile_id = dplyr::first(collapse_tile_id),
      n_member_clusters = n_distinct(original_cluster_id),
      member_cluster_ids = paste(sort(unique(original_cluster_id)), collapse = ";"),
      .groups = "drop"
    ) %>%
    left_join(
      analysis_unit_country_summary,
      by = c("AEZ", "cluster_id", "buffer_km")
    ) %>%
    mutate(
      n_matched_countries = replace_na(n_matched_countries, 0L)
    )

  noncollapsed_tile <- cluster_buffer_tile_this %>%
    mutate(original_cluster_id = as.character(cluster_id)) %>%
    inner_join(
      analysis_unit_membership %>%
        filter(!collapse_to_single_tile) %>%
        dplyr::select(AEZ, buffer_km, original_cluster_id, analysis_unit_id, analysis_unit_type, collapse_tile_id),
      by = c("AEZ", "buffer_km", "original_cluster_id")
    ) %>%
    mutate(cluster_id = analysis_unit_id) %>%
    dplyr::select(
      AEZ,
      cluster_id,
      buffer_km,
      tile_id,
      country_name,
      has_ha_info,
      intersection_area_ha,
      normalized_overlap_share,
      analysis_unit_type,
      collapse_tile_id
    )

  collapsed_tile <- cluster_buffer_tile_this %>%
    mutate(original_cluster_id = as.character(cluster_id)) %>%
    inner_join(
      analysis_unit_membership %>%
        filter(collapse_to_single_tile) %>%
        dplyr::select(AEZ, buffer_km, original_cluster_id, analysis_unit_id, analysis_unit_type, collapse_tile_id),
      by = c("AEZ", "buffer_km", "original_cluster_id")
    ) %>%
    group_by(AEZ, cluster_id = analysis_unit_id, buffer_km, tile_id, country_name, has_ha_info) %>%
    summarise(
      intersection_area_ha = sum(intersection_area_ha, na.rm = TRUE),
      normalized_overlap_share = 1,
      analysis_unit_type = dplyr::first(analysis_unit_type),
      collapse_tile_id = dplyr::first(collapse_tile_id),
      .groups = "drop"
    )

  analysis_unit_buffer_tile <- bind_rows(noncollapsed_tile, collapsed_tile) %>%
    arrange(AEZ, cluster_id, buffer_km, tile_id)

  noncollapsed_year_defor <- cluster_buffer_year_defor_this %>%
    mutate(original_cluster_id = as.character(cluster_id)) %>%
    inner_join(
      analysis_unit_membership %>%
        filter(!collapse_to_single_tile) %>%
        dplyr::select(AEZ, buffer_km, original_cluster_id, analysis_unit_id),
      by = c("AEZ", "buffer_km", "original_cluster_id")
    ) %>%
    mutate(cluster_id = analysis_unit_id) %>%
    dplyr::select(
      AEZ,
      cluster_id,
      buffer_km,
      year,
      n_tiles_with_ha,
      n_tiles,
      defor_ha_total_raw,
      defor_ha_total_avg,
      defor_ha_total_rel_pct,
      defor_ha_crops_raw,
      defor_ha_crops_avg,
      defor_ha_crops_rel_pct
    )

  collapsed_year_defor <- analysis_unit_buffer_tile %>%
    filter(analysis_unit_type == "single_tile") %>%
    distinct(AEZ, cluster_id, buffer_km, tile_id, has_ha_info) %>%
    left_join(defor_tile_year, by = "tile_id", relationship = "many-to-many") %>%
    group_by(AEZ, cluster_id, buffer_km, year) %>%
    summarise(
      n_tiles = n_distinct(tile_id),
      n_tiles_with_ha = n_distinct(tile_id[has_ha_info %in% TRUE]),
      defor_ha_total_raw = sum(defor_total_ha, na.rm = TRUE),
      defor_ha_total_avg = defor_ha_total_raw / n_tiles,
      defor_ha_total_rel_pct = sum(defor_total_ha, na.rm = TRUE),
      defor_ha_crops_raw = sum(defor_crops_ha, na.rm = TRUE),
      defor_ha_crops_avg = defor_ha_crops_raw / n_tiles,
      defor_ha_crops_rel_pct = sum(defor_crops_ha, na.rm = TRUE),
      .groups = "drop"
    )

  analysis_unit_year_defor <- bind_rows(noncollapsed_year_defor, collapsed_year_defor) %>%
    left_join(
      analysis_unit_buffer_meta %>%
        dplyr::select(
          AEZ,
          cluster_id,
          buffer_km,
          hansen_land_area_ha,
          hansen_treecover2000_equiv_ha,
          hansen_treecover2000_mean_pct
        ),
      by = c("AEZ", "cluster_id", "buffer_km")
    ) %>%
    arrange(AEZ, cluster_id, buffer_km, year)

  analysis_unit_medoids <- cluster_medoids %>%
    mutate(original_cluster_id = as.character(cluster_id)) %>%
    inner_join(
      analysis_unit_membership %>%
        dplyr::select(AEZ, original_cluster_id, analysis_unit_id),
      by = c("AEZ", "original_cluster_id")
    ) %>%
    group_by(AEZ, cluster_id = analysis_unit_id) %>%
    summarise(
      medoid_latitude = mean(medoid_latitude, na.rm = TRUE),
      medoid_longitude = mean(medoid_longitude, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    membership = analysis_unit_membership,
    summary = analysis_unit_summary,
    cluster_sites = analysis_unit_sites,
    cluster_year_ov = analysis_unit_year_ov,
    cluster_buffer_meta = analysis_unit_buffer_meta,
    cluster_buffer_tile = analysis_unit_buffer_tile,
    cluster_buffer_country = analysis_unit_buffer_country,
    cluster_buffer_year_defor = analysis_unit_year_defor,
    cluster_medoids = analysis_unit_medoids
  )
}

build_year_pair_ov_table <- function(cluster_year_ov, ov_specs) {
  base <- cluster_year_ov %>%
    arrange(AEZ, cluster_id, year) %>%
    group_by(AEZ, cluster_id) %>%
    mutate(
      year_t2 = lead(year),
      n_sites_t2 = lead(n_sites_year)
    ) %>%
    ungroup() %>%
    filter(!is.na(year_t2)) %>%
    transmute(
      AEZ,
      cluster_id,
      year_t1 = year,
      year_t2 = as.integer(year_t2),
      year_gap = year_t2 - year_t1,
      n_sites_t1 = n_sites_year,
      n_sites_t2 = n_sites_t2
    )

  variant_tables <- purrr::map(
    seq_len(nrow(ov_specs)),
    function(i) {
      spec <- ov_specs[i, ]
      source_col <- spec$cluster_year_col[[1]]
      t1_col <- spec$t1_col[[1]]
      t2_col <- spec$t2_col[[1]]
      delta_col <- spec$delta_col[[1]]
      annualized_col <- spec$annualized_col[[1]]

      cluster_year_ov %>%
        arrange(AEZ, cluster_id, year) %>%
        group_by(AEZ, cluster_id) %>%
        mutate(
          year_t2 = lead(year),
          ov_next = lead(.data[[source_col]])
        ) %>%
        ungroup() %>%
        filter(!is.na(year_t2)) %>%
        transmute(
          AEZ,
          cluster_id,
          year_t1 = year,
          !!t1_col := .data[[source_col]],
          !!t2_col := ov_next,
          !!delta_col := ov_next - .data[[source_col]],
          !!annualized_col := (ov_next - .data[[source_col]]) / (year_t2 - year)
        )
    }
  )

  purrr::reduce(variant_tables, left_join, .init = base, by = c("AEZ", "cluster_id", "year_t1"))
}

build_whole_cluster_ov_table <- function(cluster_year_ov, ov_specs) {
  base <- cluster_year_ov %>%
    arrange(AEZ, cluster_id, year) %>%
    group_by(AEZ, cluster_id) %>%
    summarise(
      year_start = first(year),
      year_final = last(year),
      year_gap = year_final - year_start,
      n_sites_start = first(n_sites_year),
      n_sites_final = last(n_sites_year),
      n_years_observed = n(),
      .groups = "drop"
    ) %>%
    filter(n_years_observed >= 2L)

  variant_tables <- purrr::map(
    seq_len(nrow(ov_specs)),
    function(i) {
      spec <- ov_specs[i, ]
      source_col <- spec$cluster_year_col[[1]]
      t1_col <- spec$t1_col[[1]]
      t2_col <- spec$t2_col[[1]]
      delta_col <- spec$delta_col[[1]]
      annualized_col <- spec$annualized_col[[1]]

      cluster_year_ov %>%
        arrange(AEZ, cluster_id, year) %>%
        group_by(AEZ, cluster_id) %>%
        summarise(
          year_start = first(year),
          year_final = last(year),
          year_gap = year_final - year_start,
          n_years_observed = n(),
          !!t1_col := first(.data[[source_col]]),
          !!t2_col := last(.data[[source_col]]),
          !!delta_col := last(.data[[source_col]]) - first(.data[[source_col]]),
          !!annualized_col := dplyr::if_else(
            year_gap > 0,
            (last(.data[[source_col]]) - first(.data[[source_col]])) / year_gap,
            NA_real_
          ),
          .groups = "drop"
        ) %>%
        filter(n_years_observed >= 2L) %>%
        select(AEZ, cluster_id, all_of(c(t1_col, t2_col, delta_col, annualized_col)))
    }
  )

  purrr::reduce(variant_tables, left_join, .init = base, by = c("AEZ", "cluster_id"))
}
