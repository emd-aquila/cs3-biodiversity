# =====================================================
# Build family community matrices and calculate PD
# =====================================================

required_objects <- c("database_tables")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]
if (length(missing_objects) > 0) {
  stop(
    "Missing required objects for 04_calculate_phylogenetic_diversity.R: ",
    paste(missing_objects, collapse = ", "),
    call. = FALSE
  )
}

message("Reading phylogenetic tree: ", tree_file)
phylo_tree <- ape::read.tree(tree_file)

pd_tables <- setNames(vector("list", length(database_tables)), names(database_tables))
for (database_key in names(database_tables)) {
  message("Calculating phylogenetic diversity for ", database_key)
  pd_path <- dataset_output_path(database_key, "pd_result.csv")
  if (isTRUE(reuse_existing_pd_outputs) && file.exists(pd_path) && database_key != "combined" && !(database_key %in% rebuild_pd_datasets)) {
    message("Using existing PD output: ", pd_path)
    pd_tables[[database_key]] <- readr::read_csv(pd_path, show_col_types = FALSE)
  } else if (
    database_key == "combined" &&
      all(c("predicts", "biotime") %in% names(pd_tables)) &&
      !is.null(pd_tables$predicts) &&
      !is.null(pd_tables$biotime)
  ) {
    family_labels <- clean_family_name(phylo_tree$tip.label)
    observed_families <- database_tables[[database_key]][
      effort_corrected_measurement > 0 &
        !is.na(Family) &
        nzchar(as.character(Family)),
      sort(unique(clean_family_name(Family)))
    ]
    write_lines_safe(
      intersect(observed_families, family_labels),
      dataset_tmp_path(database_key, "family_names_present.txt")
    )
    write_lines_safe(
      setdiff(observed_families, family_labels),
      dataset_tmp_path(database_key, "family_names_unmatched.txt")
    )
    pd_tables[[database_key]] <- dplyr::bind_rows(pd_tables$predicts, pd_tables$biotime) |>
      dplyr::filter(sample_id %in% unique(database_tables[[database_key]]$sample_id))
    write_csv_safe(pd_tables[[database_key]], pd_path)
  } else {
    pd_tables[[database_key]] <- calculate_pd_for_database(
      dt = database_tables[[database_key]],
      database_key = database_key,
      phylo_tree = phylo_tree
    )
  }
}
