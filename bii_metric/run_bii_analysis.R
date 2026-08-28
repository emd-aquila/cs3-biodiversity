#!/usr/bin/env Rscript

script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
code_path <- file.path(dirname(normalizePath(script_path)), "code", "run_bii_analysis.R")
old_bii_code_dir <- Sys.getenv("CS3_BII_CODE_DIR", unset = "")
Sys.setenv(CS3_BII_CODE_DIR = dirname(code_path))
on.exit(Sys.setenv(CS3_BII_CODE_DIR = old_bii_code_dir), add = TRUE)
source(code_path)
