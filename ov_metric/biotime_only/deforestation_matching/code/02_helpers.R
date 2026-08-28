assert_file_exists <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " does not exist: ", path, call. = FALSE)
  }
}

assert_has_cols <- function(data, cols, label) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

write_csv_safe <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, path)
  message("Wrote: ", path)
  invisible(path)
}

write_gpkg_safe <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(data, dsn = path, delete_dsn = TRUE, quiet = TRUE)
  message("Wrote: ", path)
  invisible(path)
}

run_hansen_export_command <- function() {
  args <- c(
    export_script_path,
    "--input", site_buffer_geojson_path,
    "--output", hansen_site_year_defor_path,
    "--chunk-size", as.character(hansen_export_chunk_size)
  )

  if (nzchar(earthengine_project)) {
    args <- c(args, "--project", earthengine_project)
  }

  message("Running Hansen Earth Engine export:")
  message("  python3 ", paste(shQuote(args), collapse = " "))
  status <- system2("python3", args)

  if (!identical(status, 0L)) {
    stop(
      paste0(
        "Hansen Earth Engine export failed.\n",
        "After installing/authenticating Earth Engine, rerun:\n",
        "  python3 ", paste(shQuote(args), collapse = " "), "\n",
        "Then rerun:\n",
        "  Rscript ov_metric/biotime_only/deforestation_matching/code/run_deforestation_matching.R"
      ),
      call. = FALSE
    )
  }
}

complete_interval_years <- function(year_t1, year_t2, hansen_years) {
  hansen_years[hansen_years > year_t1 & hansen_years <= year_t2]
}
