#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
code_dir <- dirname(normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE))
setwd(code_dir)

source("00_libraries.R")
source("01_config.R")
source("02_helpers.R")
source("03_clean_biotime.R")

