#!/usr/bin/env Rscript

# Create the evidence-based Hill 2018 LUH2-compatible PREDICTS site mapping.
script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
code_path <- file.path(dirname(normalizePath(script_path)), "code", "18_recurate_predicts_hill2018.R")
old_bii_code_dir <- Sys.getenv("CS3_BII_CODE_DIR", unset = "")
Sys.setenv(CS3_BII_CODE_DIR = dirname(code_path))
on.exit(Sys.setenv(CS3_BII_CODE_DIR = old_bii_code_dir), add = TRUE)

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(dirname(code_path))
source("00_libraries.R")
source("01_config.R")
source("02_helpers.R")
source("18_recurate_predicts_hill2018.R")
