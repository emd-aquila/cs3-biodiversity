# Draw the figures for an existing AEZ deforestation matrix run
# (output/aez_matrix/*.csv from run_aez_deforestation_matrix.R):
#
#   Rscript bii_metric/run_aez_matrix_figures.R
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
aez_matrix_output_dir <- file.path(output_dir, "aez_matrix")
source("23_aez_matrix_figures.R")

plot_aez_matrix_figures(aez_matrix_output_dir)
