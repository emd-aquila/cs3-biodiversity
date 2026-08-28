# =====================================================
# Summarize migrated Brazil exploration outputs
# =====================================================

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  normalizePath(getwd())
}
topic_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
output_dir <- file.path(topic_dir, "output")
summary_path <- file.path(output_dir, "brazil_summary.csv")
points_path <- file.path(output_dir, "brazil_10_points.csv")

if (!file.exists(summary_path)) {
  stop("Missing migrated Brazil summary: ", summary_path, call. = FALSE)
}
if (!file.exists(points_path)) {
  stop("Missing migrated Brazil point table: ", points_path, call. = FALSE)
}

summary_dat <- read.csv(summary_path)
points_dat <- read.csv(points_path)

message("Brazil summary rows: ", nrow(summary_dat))
message("Illustrative Brazil clusters: ", nrow(points_dat))
