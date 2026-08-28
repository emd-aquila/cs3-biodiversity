# =====================================================
# Summarize migrated whole-cluster delta-OV map outputs
# =====================================================

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  normalizePath(getwd())
}
topic_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
output_dir <- file.path(topic_dir, "output")
summary_path <- file.path(output_dir, "whole_cluster_delta_ov_map_summary.csv")

if (!file.exists(summary_path)) {
  stop("Missing migrated whole-cluster map summary: ", summary_path, call. = FALSE)
}

summary_dat <- read.csv(summary_path)
map_files <- list.files(output_dir, pattern = "[.]png$", full.names = FALSE)

message("Whole-cluster delta-OV map summary rows: ", nrow(summary_dat))
message("Map PNG files: ", paste(map_files, collapse = ", "))
