# =====================================================
# Run full build + analysis pipeline
# =====================================================

if (!dir.exists("build") || !dir.exists("analysis")) {
  stop("Run this script from the cluster_deforestation_tag project root.")
}

project_root <- normalizePath(getwd())

setwd(file.path(project_root, "build", "code"))
source("run_build.R")

setwd(file.path(project_root, "analysis", "code"))
source("run_analysis.R")

setwd(project_root)

message("Full pipeline completed successfully.")
