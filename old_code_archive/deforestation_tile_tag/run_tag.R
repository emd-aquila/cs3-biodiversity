# =====================================================
# Run full build + analysis pipeline
# =====================================================

# -----------------------
# Workflow toggles
# -----------------------

run_whole_cluster <- TRUE
run_year_pair <- TRUE

if (!dir.exists("build") || !dir.exists("analysis")) {
  stop("Run this script from the cluster_deforestation_tag project root.")
}

analysis_modes_to_run <- c(
  if (isTRUE(run_year_pair)) "ov_year_pair",
  if (isTRUE(run_whole_cluster)) "ov_whole_cluster"
)

if (length(analysis_modes_to_run) == 0) {
  stop(
    "No analysis workflows selected. Set run_whole_cluster and/or run_year_pair to TRUE.",
    call. = FALSE
  )
}

project_root <- normalizePath(getwd())
old_options <- options(defor_analysis_modes = analysis_modes_to_run)
on.exit(options(old_options), add = TRUE)

setwd(file.path(project_root, "build", "code"))
source("run_build.R")

setwd(file.path(project_root, "analysis", "code"))
source("run_analysis.R")

setwd(project_root)

message("Full pipeline completed successfully.")
