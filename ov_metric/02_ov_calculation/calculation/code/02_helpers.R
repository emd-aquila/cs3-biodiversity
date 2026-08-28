# =====================================================
# General helpers
# =====================================================

write_csv_safe <- function(df, path) {
  readr::write_csv(df, path)
  message("Wrote: ", path)
  invisible(path)
}

minmax01 <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || r[1] == r[2]) {
    return(rep(NA_real_, length(x)))
  }
  (x - r[1]) / (r[2] - r[1])
}

dataset_tmp_path <- function(database_key, suffix) {
  file.path(tmp_dir, paste0(database_key, "_", suffix))
}

dataset_output_path <- function(database_key, suffix) {
  file.path(output_dir, paste0(database_key, "_", suffix))
}

# =====================================================
# Input preparation helpers
# =====================================================

prepare_database_table <- function(dt, database_key) {
  dt <- data.table::as.data.table(dt)
  required_cols <- c(
    "database", "sample_id", "site_id", "sample_midpoint", "latitude",
    "longitude", "taxon_name", "Family", "Genus", "effort_corrected_measurement"
  )
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(
      database_key,
      " database is missing required OV columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  dt[
    ,
    `:=`(
      sample_midpoint = as.Date(sample_midpoint),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      effort_corrected_measurement = as.numeric(effort_corrected_measurement),
      taxon_name = dplyr::coalesce(as.character(taxon_name), paste(Family, Genus, sep = "_"))
    )
  ]
  dt
}

# =====================================================
# OV component helpers
# =====================================================

calculate_ov_scores_for_database <- function(dt, pd_result, database_key) {
  taxon_abundance <- dt[
    effort_corrected_measurement > 0 &
      !is.na(taxon_name) &
      nzchar(as.character(taxon_name)),
    .(abundance = round(sum(effort_corrected_measurement, na.rm = TRUE), 0)),
    by = .(sample_id, taxon_name)
  ]

  shannon <- taxon_abundance[
    ,
    {
      p <- abundance / sum(abundance)
      .(shannon = -sum(p * log(p), na.rm = TRUE))
    },
    by = sample_id
  ]

  msa <- taxon_abundance[
    ,
    .(
      site_total_abundance = sum(abundance),
      site_total_taxa = data.table::uniqueN(taxon_name),
      msa = round(sum(abundance) / data.table::uniqueN(taxon_name), 0)
    ),
    by = sample_id
  ]

  sample_lookup <- unique(
    dt[
      ,
      .(
        database,
        sample_id,
        site_id,
        assemblage_id,
        study_id,
        source_id,
        standard_source_id,
        reference,
        doi,
        sample_year,
        sample_midpoint,
        latitude,
        longitude
      )
    ],
    by = "sample_id"
  )

  components <- data.table::as.data.table(pd_result)
  components <- merge(components, shannon, by = "sample_id", all.x = TRUE, sort = FALSE)
  components <- merge(components, msa, by = "sample_id", all.x = TRUE, sort = FALSE)
  components <- merge(components, sample_lookup, by = "sample_id", all.x = TRUE, sort = FALSE)
  components <- components[
    !is.na(shannon) &
      !is.nan(shannon) &
      !is.na(msa) &
      !is.na(phylo_div)
  ]

  if (nrow(components) > 0) {
    components[
      ,
      `:=`(
        log_msa = log(msa + 1),
        shannon_scaled = minmax01(shannon),
        phylo_scaled = minmax01(phylo_div)
      )
    ]
    components[, log_msa_scaled := minmax01(log_msa)]
    components[
      ,
      `:=`(
        ov_obs_only = shannon_scaled + log_msa_scaled + phylo_scaled,
        ov_score = shannon_scaled + log_msa_scaled + phylo_scaled
      )
    ]
    numeric_cols <- names(components)[vapply(components, is.numeric, logical(1))]
    round_cols <- setdiff(numeric_cols, c("latitude", "longitude", "sample_year"))
    components[, (round_cols) := lapply(.SD, round, 3), .SDcols = round_cols]
  }

  write_csv_safe(components, dataset_tmp_path(database_key, "component_scores.csv"))
  components
}

# =====================================================
# AEZ tagging helpers
# =====================================================

assign_aez_to_scores <- function(ov_scores, database_key) {
  if (nrow(ov_scores) == 0) {
    write_csv_safe(ov_scores, dataset_output_path(database_key, "ov_scores.csv"))
    return(ov_scores)
  }

  sf::sf_use_s2(FALSE)
  aez <- sf::read_sf(aez_path) |>
    sf::st_make_valid() |>
    dplyr::select(Id, AEZ)

  sites_sf <- ov_scores |>
    dplyr::filter(!is.na(longitude), !is.na(latitude)) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
    sf::st_transform(sf::st_crs(aez))

  tagged <- sf::st_join(sites_sf, aez, join = sf::st_within, left = TRUE) |>
    sf::st_drop_geometry()

  missing_idx <- which(is.na(tagged$AEZ))
  if (length(missing_idx) > 0) {
    nearest_idx <- sf::st_nearest_feature(sites_sf[missing_idx, ], aez)
    tagged$AEZ[missing_idx] <- aez$AEZ[nearest_idx]
  }

  tagged <- tagged |>
    dplyr::mutate(AEZ_assigned_by_nearest = dplyr::row_number() %in% missing_idx) |>
    dplyr::relocate(AEZ, .after = sample_id) |>
    dplyr::relocate(AEZ_assigned_by_nearest, .after = AEZ)

  write_csv_safe(tagged, dataset_output_path(database_key, "ov_scores.csv"))
  tagged
}

# =====================================================
# Manifest helpers
# =====================================================

manifest_row <- function(output_name, path, data, description) {
  data.table::data.table(
    output_name = output_name,
    path = path,
    rows = nrow(data),
    columns = ncol(data),
    description = description
  )
}
