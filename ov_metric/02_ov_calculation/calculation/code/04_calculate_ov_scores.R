# =====================================================
# Calculate observation-based OV scores from databases and PD outputs
# =====================================================

required_objects <- c("database_tables", "pd_tables")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]
if (length(missing_objects) > 0) {
  stop(
    "Missing required objects for 04_calculate_ov_scores.R: ",
    paste(missing_objects, collapse = ", "),
    call. = FALSE
  )
}

ov_score_tables <- setNames(vector("list", length(database_tables)), names(database_tables))
for (database_key in names(database_tables)) {
  message("Calculating OV scores for ", database_key)
  ov_score_tables[[database_key]] <- calculate_ov_scores_for_database(
    dt = database_tables[[database_key]],
    pd_result = pd_tables[[database_key]],
    database_key = database_key
  )
}
