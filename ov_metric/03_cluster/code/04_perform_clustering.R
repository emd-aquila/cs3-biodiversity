# =====================================================
# Run all method-radius clustering combinations by database
# =====================================================

stopifnot(exists("cluster_inputs"), exists("cluster_dataset_specs"))

message("AEZ clustering workers: ", clustering_parallel_workers)
message("Flat queue mode: dataset/method/radius/AEZ checkpoint tasks")

for (dataset_i in seq_len(nrow(cluster_dataset_specs))) {
  dataset_spec <- cluster_dataset_specs[dataset_i, ]
  dataset_key <- dataset_spec$dataset_key
  dataset_input <- cluster_inputs[[dataset_key]]

  run_clustering_for_dataset(
    dataset_spec = dataset_spec,
    dataset_input = dataset_input,
    run_grid = run_grid,
    parallel_workers = clustering_parallel_workers
  )
}

message("Finished clustering runs. Bundles saved under output/<dataset>/radius_<km>/<method>.")
