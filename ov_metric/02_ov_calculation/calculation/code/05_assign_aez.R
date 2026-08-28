# =====================================================
# Assign AEZ labels to OV scores
# =====================================================

required_objects <- c("ov_score_tables")

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]
if (length(missing_objects) > 0) {
  stop(
    "Missing required objects for 05_assign_aez.R: ",
    paste(missing_objects, collapse = ", "),
    call. = FALSE
  )
}

ov_score_aez_tables <- setNames(vector("list", length(ov_score_tables)), names(ov_score_tables))
for (database_key in names(ov_score_tables)) {
  message("Assigning AEZ labels for ", database_key)
  
  ov_score_aez_tables[[database_key]] <- assign_aez_to_scores(
    ov_scores = ov_score_tables[[database_key]],
    database_key = database_key
  )
}
