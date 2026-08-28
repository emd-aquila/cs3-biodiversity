# =====================================================
# Load OV-ready databases from biodiversity integration
# =====================================================

message("Loading OV-ready databases from 01_biodiversity_data_integration")
database_tables <- setNames(vector("list", nrow(database_specs)), database_specs$database_key)

for (i in seq_len(nrow(database_specs))) {
  spec <- database_specs[i]
  dt <- prepare_database_table(readRDS(spec$input_rds), spec$database_key)
  database_tables[[spec$database_key]] <- dt
  message(spec$database_label, " rows: ", nrow(dt), "; columns: ", ncol(dt))
}

input_summary <- data.table::rbindlist(lapply(names(database_tables), function(database_key) {
  dt <- database_tables[[database_key]]
  data.table::data.table(
    database_key = database_key,
    rows = nrow(dt),
    samples = data.table::uniqueN(dt$sample_id),
    families = data.table::uniqueN(dt$Family[!is.na(dt$Family)])
  )
}))
write_csv_safe(input_summary, file.path(tmp_dir, "input_database_summary.csv"))
