# =====================================================
# Load databases and PD build outputs
# =====================================================

message("Loading databases and PD build outputs")
database_tables <- setNames(vector("list", nrow(database_specs)), database_specs$database_key)
pd_tables <- setNames(vector("list", nrow(database_specs)), database_specs$database_key)

for (i in seq_len(nrow(database_specs))) {
  spec <- database_specs[i]
  database_tables[[spec$database_key]] <- prepare_database_table(readRDS(spec$input_rds), spec$database_key)
  pd_tables[[spec$database_key]] <- readr::read_csv(spec$pd_csv, show_col_types = FALSE)
  message(
    spec$database_label,
    " database rows: ", nrow(database_tables[[spec$database_key]]),
    "; PD rows: ", nrow(pd_tables[[spec$database_key]])
  )
}
