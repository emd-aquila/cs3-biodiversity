# Build the AEZ x land-use-transition biodiversity response matrix
# ("X additional ha of deforestation in AEZ a -> Y % change in biodiversity")
# from the fitted PREDICTS intactness responses. Run from any directory:
#
#   Rscript bii_metric/run_aez_deforestation_matrix.R
#
# Requires the prepared PREDICTS site table from 03_prepare_predicts.R (it is
# built here if missing). Optional inputs and environment variables are
# documented at the top of code/22_aez_deforestation_matrix.R.
launcher_code_dir <- Sys.getenv("CS3_BII_CODE_DIR", unset = "")
if (nzchar(launcher_code_dir)) {
  code_dir <- normalizePath(launcher_code_dir)
} else {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else NA_character_
  code_dir <- if (!is.na(script_path)) file.path(dirname(normalizePath(script_path)), "code") else normalizePath(getwd())
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(code_dir)

source("00_libraries.R")
source("01_config.R")
source("02_helpers.R")
if (!file.exists(predicts_site_path)) {
  message("Prepared PREDICTS site table not found; running 03_prepare_predicts.R")
  source("03_prepare_predicts.R")
}
source("22_aez_deforestation_matrix.R")

run_aez_deforestation_matrix()
