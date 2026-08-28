# =====================================================
# Load build outputs needed for OV calculation
# =====================================================

assert_exists(predicts_file)
assert_exists(pd_file)

predicts <- read_csv(predicts_file, show_col_types = FALSE) %>%
  mutate(
    Sample_midpoint = as.Date(Sample_midpoint),
    sample_id = paste(SSBS, Sample_midpoint, sep = "__")
  )

pd <- read_csv(pd_file, show_col_types = FALSE)

message("Loaded predicts file rows: ", nrow(predicts))
message("Loaded phylogenetic diversity rows: ", nrow(pd))
