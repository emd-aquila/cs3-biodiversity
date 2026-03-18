# =====================================================
# Load required libraries for biodiversity site clustering
# This script loads all packages used throughout the pipeline.
# Run this script before executing other code scripts.
# =====================================================

# tidy data manipulation and plotting
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(ggplot2)

# spatial data handling
library(sf)

# clustering algorithms and parallelism
library(cluster)
library(furrr)
library(progressr)
library(future)