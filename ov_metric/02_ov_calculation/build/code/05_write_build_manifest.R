# =====================================================
# Write PD build manifest
# =====================================================

required_objects <- c("pd_tables")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]
if (length(missing_objects) > 0) {
  stop(
    "Missing required objects for 05_write_build_manifest.R: ",
    paste(missing_objects, collapse = ", "),
    call. = FALSE
  )
}

build_manifest <- data.table::rbindlist(lapply(names(pd_tables), function(database_key) {
  manifest_row(
    output_name = paste0(database_key, "_pd_result"),
    path = dataset_output_path(database_key, "pd_result.csv"),
    data = pd_tables[[database_key]],
    description = paste0("Family-level phylogenetic diversity build output for ", database_key, ".")
  )
}))

write_csv_safe(build_manifest, build_manifest_path)
message("PD build manifest rows: ", nrow(build_manifest))
