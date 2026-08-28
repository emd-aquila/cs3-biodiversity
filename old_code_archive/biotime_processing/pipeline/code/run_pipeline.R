# =====================================================
# Run the structured BioTIME pipeline sequentially.
# =====================================================

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))))
  }
  normalizePath(getwd())
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(get_script_dir())

message("Starting structured BioTIME pipeline...")

scripts <- c(
  "00_libraries.R",
  "01_config.R",
  "02_helpers.R",
  "03_filter_predicts.R",
  "04_prepare_trial_predicts.R",
  "05_filter_biotime.R",
  "06_compare_biotime_predicts.R",
  "07_write_schema_inventory.R"
)

for (script in scripts) {
  message("Sourcing ", script)
  source(script)
}

message("Structured BioTIME pipeline complete.")
