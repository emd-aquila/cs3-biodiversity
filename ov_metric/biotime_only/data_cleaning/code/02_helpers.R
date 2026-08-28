`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

if (exists("integration_helper_path") && file.exists(integration_helper_path)) {
  source(integration_helper_path)
}

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

valid_taxon_name <- function(x) {
  x <- as.character(x)
  invalid_pattern <- paste(
    c(
      "morphospecies", "unknown", "undetermined", "unidentif",
      "sp\\.", "spp\\.", "sp$", "cf\\.", "indet\\."
    ),
    collapse = "|"
  )
  !is.na(x) & nzchar(x) & !grepl(invalid_pattern, x, ignore.case = TRUE)
}

clean_taxon_group <- function(taxa, organisms = NA_character_) {
  taxa <- stringr::str_squish(as.character(taxa))
  organisms <- stringr::str_squish(as.character(organisms))
  dplyr::case_when(
    grepl("bird", taxa, ignore.case = TRUE) ~ "Birds",
    grepl("mammal", taxa, ignore.case = TRUE) ~ "Mammals",
    grepl("amphib", taxa, ignore.case = TRUE) ~ "Amphibians",
    grepl("reptile", taxa, ignore.case = TRUE) & grepl("lizard", organisms, ignore.case = TRUE) ~ "Lizards",
    grepl("reptile", taxa, ignore.case = TRUE) ~ "Reptiles",
    grepl("plant", taxa, ignore.case = TRUE) ~ "Plants",
    grepl("invertebrate", taxa, ignore.case = TRUE) ~ "Invertebrates",
    grepl("fung", taxa, ignore.case = TRUE) ~ "Fungi",
    grepl("multiple", taxa, ignore.case = TRUE) ~ "Multiple",
    is.na(taxa) | !nzchar(taxa) ~ "Unknown",
    TRUE ~ taxa
  )
}

calc_shannon <- function(abundance) {
  abundance <- abundance[is.finite(abundance) & abundance > 0]
  if (length(abundance) == 0 || sum(abundance) <= 0) {
    return(NA_real_)
  }
  p <- abundance / sum(abundance)
  -sum(p * log(p), na.rm = TRUE)
}

minmax01 <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || r[1] == r[2]) {
    return(rep(NA_real_, length(x)))
  }
  (x - r[1]) / (r[2] - r[1])
}

clean_family_name <- function(x) {
  x <- as.character(x)
  x <- gsub("^f__", "", x)
  trimws(x)
}

write_lines_safe <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(x), path)
  message("Wrote: ", path)
  invisible(path)
}

calculate_pd_for_biotime <- function(dt, phylo_tree) {
  dt <- data.table::as.data.table(dt)
  required_cols <- c("sample_id", "Family", "effort_corrected_measurement")
  assert_has_cols(dt, required_cols, "BioTIME standardized rows")

  family_labels <- clean_family_name(phylo_tree$tip.label)
  observed_families <- dt[
    effort_corrected_measurement > 0 &
      !is.na(Family) &
      nzchar(as.character(Family)),
    sort(unique(clean_family_name(Family)))
  ]
  matched_families <- intersect(observed_families, family_labels)
  unmatched_families <- setdiff(observed_families, matched_families)

  write_lines_safe(matched_families, pd_matched_families_path)
  write_lines_safe(unmatched_families, pd_unmatched_families_path)

  if (length(matched_families) == 0) {
    warning("No BioTIME family names matched the phylogenetic tree.", call. = FALSE)
    pd_result <- tibble::tibble(sample_id = character(), phylo_div = numeric(), SR = numeric())
    write_csv_safe(pd_result, pd_result_path)
    return(pd_result)
  }

  pd_long <- dt[
    effort_corrected_measurement > 0 &
      clean_family_name(Family) %in% matched_families,
    .(abundance = round(sum(effort_corrected_measurement, na.rm = TRUE), 0)),
    by = .(sample_id, Family = clean_family_name(Family))
  ]

  comm <- pd_long |>
    tibble::as_tibble() |>
    tidyr::pivot_wider(names_from = Family, values_from = abundance, values_fill = 0)

  comm_mat <- comm |>
    tibble::column_to_rownames("sample_id") |>
    as.data.frame()

  tree_tip_lookup <- data.frame(
    tip_label = phylo_tree$tip.label,
    family = family_labels,
    stringsAsFactors = FALSE
  )
  keep_tips <- tree_tip_lookup$tip_label[tree_tip_lookup$family %in% colnames(comm_mat)]
  pruned_tree <- ape::drop.tip(phylo_tree, setdiff(phylo_tree$tip.label, keep_tips))
  tree_family_order <- tree_tip_lookup$family[match(pruned_tree$tip.label, tree_tip_lookup$tip_label)]

  comm_mat <- comm_mat[, tree_family_order, drop = FALSE]
  colnames(comm_mat) <- pruned_tree$tip.label

  pd_out <- picante::pd(samp = comm_mat, tree = pruned_tree, include.root = TRUE)
  pd_result <- pd_out |>
    tibble::rownames_to_column("sample_id") |>
    dplyr::rename(phylo_div = PD) |>
    tibble::as_tibble()

  write_csv_safe(pd_result, pd_result_path)
  pd_result
}

calculate_composite_ov_scores <- function(dt, pd_result) {
  dt <- data.table::as.data.table(dt)
  pd_result <- data.table::as.data.table(pd_result)
  required_cols <- c(
    "sample_id", "site_id", "assemblage_id", "study_id", "sample_year",
    "sample_midpoint", "latitude", "longitude", "taxon_name",
    "Family", "Genus", "effort_corrected_measurement"
  )
  assert_has_cols(dt, required_cols, "BioTIME standardized rows")

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
        sample_start_date,
        sample_end_date,
        sample_midpoint,
        latitude,
        longitude,
        sampling_effort,
        sampling_effort_unit,
        raw_sample_events_available,
        sample_events_selected,
        rarefaction_seed,
        biotime_grid_resolution
      )
    ],
    by = "sample_id"
  )

  sample_effort <- dt[
    ,
    .(
      n_standardized_taxon_rows = .N,
      n_observation_rows = sum(as.integer(n_observation_rows), na.rm = TRUE),
      n_families = data.table::uniqueN(Family[!is.na(Family) & nzchar(as.character(Family))]),
      n_genera = data.table::uniqueN(Genus[!is.na(Genus) & nzchar(as.character(Genus))])
    ),
    by = sample_id
  ]

  components <- data.table::copy(pd_result)
  components <- merge(components, shannon, by = "sample_id", all.x = TRUE, sort = FALSE)
  components <- merge(components, msa, by = "sample_id", all.x = TRUE, sort = FALSE)
  components <- merge(components, sample_lookup, by = "sample_id", all.x = TRUE, sort = FALSE)
  components <- merge(components, sample_effort, by = "sample_id", all.x = TRUE, sort = FALSE)
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
        ov_score = shannon_scaled + log_msa_scaled + phylo_scaled,
        effective_species = exp(shannon),
        richness = site_total_taxa,
        total_abundance = site_total_abundance
      )
    ]
  }

  write_csv_safe(components, ov_component_scores_path)
  components
}

assign_aez_to_points <- function(points_df, aez_path, analysis_crs) {
  sf::sf_use_s2(FALSE)

  aez <- sf::read_sf(aez_path) |>
    sf::st_make_valid() |>
    dplyr::select(Id, AEZ) |>
    sf::st_transform(analysis_crs)

  points_sf <- points_df |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
    sf::st_transform(analysis_crs)

  tagged <- sf::st_join(points_sf, aez, join = sf::st_within, left = TRUE)
  tagged$AEZ_assigned_by_nearest <- is.na(tagged$AEZ)

  missing_idx <- which(is.na(tagged$AEZ))
  if (length(missing_idx) > 0) {
    nearest_idx <- sf::st_nearest_feature(tagged[missing_idx, ], aez)
    tagged$AEZ[missing_idx] <- aez$AEZ[nearest_idx]
    tagged$Id[missing_idx] <- aez$Id[nearest_idx]
  }

  tagged |>
    sf::st_drop_geometry() |>
    dplyr::rename(AEZ_id = Id)
}
