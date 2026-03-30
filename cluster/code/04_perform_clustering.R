# =====================================================
# Run clustering methods and export results
# Run all method-radius combinations in the config grid.
# =====================================================

stopifnot(exists("sites_tbl"), exists("model_df_tagged"))
stopifnot(exists("checkpoint_root"), exists("output_dir"))

aez_order <- sites_tbl %>%
  count(AEZ, name = "n_sites") %>%
  arrange(n_sites) %>%
  pull(AEZ) %>%
  as.character()

for (i in seq_len(nrow(run_grid))) {
  run_one_clustering_config(
    method = run_grid$method[i],
    radius_m = run_grid$radius_m[i],
    sites_tbl = sites_tbl,
    model_df_tagged = model_df_tagged,
    aez_order = aez_order
  )
}

message("Finished clustering runs. Bundles saved to output directory.")