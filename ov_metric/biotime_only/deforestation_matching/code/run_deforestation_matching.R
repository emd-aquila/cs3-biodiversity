#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
code_dir <- dirname(normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE))
setwd(code_dir)

source("00_libraries.R")
source("01_config.R")
source("02_helpers.R")
source("03_build_site_buffers.R")

if (force_hansen_export || !file.exists(hansen_site_year_defor_path)) {
  if (isTRUE(run_hansen_export)) {
    run_hansen_export_command()
  } else {
    stop(
      paste0(
        "Missing Hansen export: ", hansen_site_year_defor_path, "\n",
        "Run:\n",
        "  python3 ", shQuote(export_script_path),
        " --input ", shQuote(site_buffer_geojson_path),
        " --output ", shQuote(hansen_site_year_defor_path), "\n",
        "Then rerun this R script."
      ),
      call. = FALSE
    )
  }
}

source("05_build_hansen_matches.R")
