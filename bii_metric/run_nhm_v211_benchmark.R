# Summarise the published NHM BII v2.1.1 maps after their ZIP archive has been
# downloaded manually to the configured shared-data location.

launcher_code_dir <- Sys.getenv("CS3_BII_CODE_DIR", unset = "")
if (nzchar(launcher_code_dir)) {
  code_dir <- normalizePath(launcher_code_dir)
} else {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else NA_character_
  code_dir <- if (!is.na(script_path)) {
    dirname(normalizePath(file.path(dirname(script_path), "code", "16_summarise_nhm_v211.R")))
  } else {
    normalizePath(file.path(getwd(), "code"))
  }
}
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(code_dir)

source("00_libraries.R")
source("01_config.R")
source("02_helpers.R")
source("16_summarise_nhm_v211.R")
