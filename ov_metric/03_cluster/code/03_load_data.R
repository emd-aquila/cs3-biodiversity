# =====================================================
# Load and prepare tagged OV data for all clustering datasets
# =====================================================

cluster_inputs <- stats::setNames(vector("list", nrow(cluster_dataset_specs)), cluster_dataset_specs$dataset_key)

for (i in seq_len(nrow(cluster_dataset_specs))) {
  spec <- cluster_dataset_specs[i, ]
  loaded <- load_cluster_dataset(spec$tagged_sites_file)
  cluster_inputs[[spec$dataset_key]] <- loaded

  message("Loaded ", spec$dataset_label, " tagged OV data:")
  message("  rows: ", nrow(loaded$model_df_tagged))
  message("  unique clustering sites: ", nrow(loaded$sites_tbl))
}
