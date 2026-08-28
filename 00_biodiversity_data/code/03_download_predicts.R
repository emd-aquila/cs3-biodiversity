# =====================================================
# Download/cache PREDICTS inputs
# =====================================================

options(timeout = max(600, getOption("timeout", 60)))

message("Downloading/loading combined raw PREDICTS extract")
predicts <- load_predicts_checked(predicts_raw_rds_path, predicts_release_years)
message("Combined raw PREDICTS rows: ", nrow(predicts), "; columns: ", ncol(predicts))

message("Downloading/loading PREDICTS site-level summaries")
if (file.exists(predicts_metadata_raw_rds_path)) {
  sites <- data.table::as.data.table(readRDS(predicts_metadata_raw_rds_path))
} else {
  sites <- tryCatch(
    data.table::as.data.table(predictsr::GetSitelevelSummaries(extract = predicts_release_years)),
    error = function(err) {
      if (file.exists(predicts_metadata_rds_path)) {
        warning(
          "Could not download PREDICTS site summaries; seeding raw cache from existing retained metadata: ",
          predicts_metadata_rds_path,
          call. = FALSE
        )
        return(data.table::as.data.table(readRDS(predicts_metadata_rds_path)))
      }
      stop(err)
    }
  )
  if ((!is.data.frame(sites) || nrow(sites) == 0) && file.exists(predicts_metadata_rds_path)) {
    warning(
      "PREDICTS site-summary download returned empty data; seeding raw cache from existing retained metadata: ",
      predicts_metadata_rds_path,
      call. = FALSE
    )
    sites <- data.table::as.data.table(readRDS(predicts_metadata_rds_path))
  }
}
assert_nonempty_df(sites, "PREDICTS site summaries")
saveRDS(as.data.frame(sites), predicts_metadata_raw_rds_path, compress = "gzip")
message("Wrote: ", predicts_metadata_raw_rds_path)

if (!file.exists(predicts_reference_2016_path) || !file.exists(predicts_reference_2022_path)) {
  stop(
    "Missing manually downloaded PREDICTS reference CSVs. Expected:\n",
    predicts_reference_2016_path,
    "\n",
    predicts_reference_2022_path,
    call. = FALSE
  )
}

message("PREDICTS download/cache step complete")
