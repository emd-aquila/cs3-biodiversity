# =====================================================
# Load libraries for the structured BioTIME pipeline.
# =====================================================

required_packages <- c("data.table", "readr", "stringr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install required packages before running the BioTIME pipeline: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(readr)
  library(stringr)
})
