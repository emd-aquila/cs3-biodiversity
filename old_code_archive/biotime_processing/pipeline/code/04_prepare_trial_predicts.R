# =====================================================
# Create a small PREDICTS fixture for fast trial work.
# =====================================================

assert_file_exists(predicts_filtered_observations_path, "filtered slim PREDICTS RDS")

predicts_trial <- readRDS(predicts_filtered_observations_path)
predicts_trial <- utils::head(predicts_trial, predicts_trial_n_rows)

if (nrow(predicts_trial) != predicts_trial_n_rows) {
  stop(
    "Expected ",
    predicts_trial_n_rows,
    " PREDICTS trial rows but found ",
    nrow(predicts_trial),
    ".",
    call. = FALSE
  )
}

write_csv_safe(predicts_trial, predicts_trial_path)
