# =====================================================
# Helpers for PD build step
# =====================================================

write_csv_safe <- function(df, path) {
  readr::write_csv(df, path)
  message("Wrote: ", path)
  invisible(path)
}

write_lines_safe <- function(x, path) {
  writeLines(as.character(x), path)
  message("Wrote: ", path)
  invisible(path)
}

clean_family_name <- function(x) {
  x <- as.character(x)
  x <- gsub("^f__", "", x)
  trimws(x)
}

dataset_tmp_path <- function(database_key, suffix) {
  file.path(tmp_dir, paste0(database_key, "_", suffix))
}

dataset_output_path <- function(database_key, suffix) {
  file.path(output_dir, paste0(database_key, "_", suffix))
}

prepare_database_table <- function(dt, database_key) {
  dt <- data.table::as.data.table(dt)
  required_cols <- c("sample_id", "Family", "effort_corrected_measurement")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(
      database_key,
      " database is missing required PD columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  dt[, effort_corrected_measurement := as.numeric(effort_corrected_measurement)]
  dt
}

calculate_pd_for_database <- function(dt, database_key, phylo_tree) {
  family_labels <- clean_family_name(phylo_tree$tip.label)
  observed_families <- dt[
    effort_corrected_measurement > 0 &
      !is.na(Family) &
      nzchar(as.character(Family)),
    sort(unique(clean_family_name(Family)))
  ]
  matched_families <- intersect(observed_families, family_labels)
  unmatched_families <- setdiff(observed_families, matched_families)

  write_lines_safe(matched_families, dataset_tmp_path(database_key, "family_names_present.txt"))
  write_lines_safe(unmatched_families, dataset_tmp_path(database_key, "family_names_unmatched.txt"))

  if (length(matched_families) == 0) {
    warning("No family names in ", database_key, " matched the phylogenetic tree.", call. = FALSE)
    pd_result <- tibble::tibble(sample_id = character(), phylo_div = numeric(), SR = numeric())
    write_csv_safe(pd_result, dataset_output_path(database_key, "pd_result.csv"))
    return(pd_result)
  }

  pd_long <- dt[
    effort_corrected_measurement > 0 &
      clean_family_name(Family) %in% matched_families,
    .(abundance = round(sum(effort_corrected_measurement, na.rm = TRUE), 0)),
    by = .(sample_id, Family = clean_family_name(Family))
  ]

  comm <- pd_long |>
    as_tibble() |>
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
    as_tibble()

  write_csv_safe(pd_result, dataset_output_path(database_key, "pd_result.csv"))
  pd_result
}

manifest_row <- function(output_name, path, data, description) {
  data.table::data.table(
    output_name = output_name,
    path = path,
    rows = nrow(data),
    columns = ncol(data),
    description = description
  )
}
