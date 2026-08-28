# =====================================================
# Download/cache helpers for raw biodiversity data
# =====================================================

assert_nonempty_df <- function(x, label) {
  if (!is.data.frame(x) || nrow(x) == 0 || ncol(x) == 0) {
    stop(label, " is empty or invalid.", call. = FALSE)
  }
}

assert_file_exists <- function(path, label) {
  if (!file.exists(path)) stop("Missing ", label, ": ", path, call. = FALSE)
}

download_if_missing <- function(url, destfile) {
  if (!file.exists(destfile)) {
    message("Downloading: ", url)
    partial_path <- paste0(destfile, ".part")
    if (file.exists(partial_path)) unlink(partial_path)
    tryCatch(
      {
        download.file(url, partial_path, mode = "wb", quiet = FALSE)
        if (!file.exists(partial_path) || file.size(partial_path) == 0) {
          stop("Downloaded file is empty: ", partial_path, call. = FALSE)
        }
        file.rename(partial_path, destfile)
      },
      error = function(err) {
        if (file.exists(partial_path)) unlink(partial_path)
        stop(err)
      }
    )
    message("Wrote: ", destfile)
  }
  destfile
}

download_csv_as_rds_if_missing <- function(url, destfile) {
  if (!file.exists(destfile)) {
    csv_path <- file.path(tmp_dir, paste0(basename(destfile), ".csv"))
    download_if_missing(url, csv_path)
    dt <- data.table::fread(csv_path, showProgress = FALSE)
    if (!is.data.frame(dt) || nrow(dt) == 0) {
      stop("Downloaded CSV loaded as empty or invalid: ", url, call. = FALSE)
    }
    saveRDS(as.data.frame(dt), destfile, compress = "gzip")
    message("Wrote: ", destfile)
    if (file.exists(csv_path)) unlink(csv_path)
  }
  destfile
}

load_predicts_checked <- function(path, extract) {
  predicts <- predictsr::LoadPredictsData(file_predicts = path, extract = extract)
  if (!is.data.frame(predicts) || nrow(predicts) == 0 || ncol(predicts) == 0) {
    warning("PREDICTS cache was empty or invalid; forcing a fresh download.", call. = FALSE)
    predicts <- predictsr::LoadPredictsData(
      file_predicts = path,
      extract = extract,
      force_refresh = TRUE
    )
  }
  assert_nonempty_df(predicts, paste("PREDICTS extract", paste(extract, collapse = ",")))
  saveRDS(as.data.frame(predicts), path, compress = "gzip")
  message("Wrote: ", path)
  predicts
}

ensure_gbif_taxon_rds <- function() {
  if (file.exists(taxon_path)) {
    message("Using existing GBIF taxon lookup: ", taxon_path)
    return(invisible(taxon_path))
  }

  download_if_missing(gbif_backbone_url, gbif_backbone_zip_path)
  dir.create(gbif_taxon_extract_dir, recursive = TRUE, showWarnings = FALSE)

  backbone_files <- utils::unzip(gbif_backbone_zip_path, list = TRUE)
  taxon_member <- backbone_files$Name[
    basename(backbone_files$Name) %in% c("Taxon.tsv", "Taxon.txt", "taxon.tsv", "taxon.txt")
  ][1]
  if (is.na(taxon_member) || !nzchar(taxon_member)) {
    stop("Could not find Taxon.tsv or Taxon.txt in GBIF backbone archive.", call. = FALSE)
  }

  extracted <- utils::unzip(
    gbif_backbone_zip_path,
    files = taxon_member,
    exdir = gbif_taxon_extract_dir,
    overwrite = TRUE
  )
  if (length(extracted) == 0 || !file.exists(extracted[1])) {
    stop("GBIF taxon file extraction failed.", call. = FALSE)
  }

  message("Reading GBIF backbone taxon file. This can take several minutes.")
  taxon <- data.table::fread(extracted[1], sep = "\t", quote = "", showProgress = TRUE)
  assert_nonempty_df(taxon, "GBIF backbone taxon table")
  saveRDS(as.data.frame(taxon), taxon_path, compress = "gzip")
  message("Wrote: ", taxon_path)
  invisible(taxon_path)
}
