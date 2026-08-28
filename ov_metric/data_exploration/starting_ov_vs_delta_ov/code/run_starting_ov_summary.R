# =====================================================
# Summarize migrated starting-OV diagnostic outputs
# =====================================================

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  normalizePath(getwd())
}
topic_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
output_dir <- file.path(topic_dir, "output")
summary_path <- file.path(output_dir, "starting_ov_vs_delta_ov__summary.csv")

if (!file.exists(summary_path)) {
  stop("Missing migrated starting-OV summary: ", summary_path, call. = FALSE)
}

summary_dat <- read.csv(summary_path)

message("Starting-OV diagnostic summary rows: ", nrow(summary_dat))
if ("delta_ov_approach" %in% names(summary_dat)) {
  message("Delta-OV approaches: ", paste(unique(summary_dat$delta_ov_approach), collapse = ", "))
}
