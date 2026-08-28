# =====================================================
# Shared table, text, and validation helpers
# =====================================================

write_csv_safe <- function(data, path) {
  readr::write_csv(data, path)
  message("Wrote: ", path)
  invisible(path)
}

collapse_unique <- function(x, separator = " | ") {
  x <- unique(trimws(as.character(x)))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = separator)
}

normalize_key <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

as_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(as.character(x))
}

ascii_transliterate <- function(x) {
  x <- as.character(x)
  replacements <- c(
    "\u03b1" = "alpha",
    "\u0391" = "Alpha",
    "\u03b2" = "beta",
    "\u0392" = "Beta",
    "\u03b3" = "gamma",
    "\u0393" = "Gamma",
    "\u03b4" = "delta",
    "\u0394" = "Delta",
    "\u2018" = "'",
    "\u2019" = "'",
    "\u201c" = "\"",
    "\u201d" = "\"",
    "\u2013" = "-",
    "\u2014" = "-",
    "\u2212" = "-",
    "\u00d7" = "x"
  )
  for (pattern in names(replacements)) {
    x <- gsub(pattern, replacements[[pattern]], x, fixed = TRUE)
  }
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  gsub("[^ -~]", "", x)
}

ascii_normalize_table <- function(dt) {
  dt <- data.table::copy(dt)
  char_cols <- names(dt)[vapply(dt, is.character, logical(1))]
  for (col in char_cols) {
    data.table::set(dt, j = col, value = ascii_transliterate(dt[[col]]))
  }
  dt
}

assert_nonempty_df <- function(x, label) {
  if (!is.data.frame(x) || nrow(x) == 0 || ncol(x) == 0) {
    stop(label, " is empty or invalid.", call. = FALSE)
  }
}

assert_file_exists <- function(path, label) {
  if (!file.exists(path)) stop("Missing ", label, ": ", path, call. = FALSE)
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

extract_doi <- function(x) {
  doi_pattern <- stringr::regex(
    "(?:https?://(?:dx\\.)?doi\\.org/|doi\\s*[:=]\\s*)?(10\\.[0-9]{4,9}/[^\\s\"'<>}\\]\\),;]+)",
    ignore_case = TRUE
  )
  matches <- stringr::str_match_all(as.character(x), doi_pattern)
  vapply(
    matches,
    function(match_table) {
      if (nrow(match_table) == 0) return(NA_character_)
      doi <- unique(trimws(match_table[, 2]))
      doi <- gsub("^https?://(?:dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
      doi <- gsub("[[:space:]]+", "", doi)
      doi <- gsub("[\\.\\),;]+$", "", doi)
      doi <- doi[!is.na(doi) & nzchar(doi)]
      if (length(doi) == 0) return(NA_character_)
      paste(tolower(doi), collapse = " | ")
    },
    character(1)
  )
}

extract_bib_field <- function(x, field) {
  if (tolower(field) == "doi") {
    field_pattern <- stringr::regex(
      "\\bdoi\\s*=\\s*[\\{\\\"]([^\\}\\\"]*)[\\}\\\"]",
      ignore_case = TRUE,
      multiline = TRUE
    )
    field_text <- stringr::str_match(as.character(x), field_pattern)[, 2]
    doi <- extract_doi(field_text)
    missing_doi <- is.na(doi) | !nzchar(doi)
    if (any(missing_doi)) {
      doi[missing_doi] <- extract_doi(as.character(x)[missing_doi])
    }
    return(doi)
  }
  
  pattern <- stringr::regex(
    paste0("\\b", field, "\\s*=\\s*\\{([^}]*)\\}"),
    ignore_case = TRUE,
    multiline = TRUE
  )
  stringr::str_match(as.character(x), pattern)[, 2]
}


# =====================================================
# PREDICTS-specific helpers
# =====================================================

remove_empty_rows <- function(dt, ignore_cols = character()) {
  value_cols <- setdiff(names(dt), ignore_cols)
  if (length(value_cols) == 0) return(dt)
  is_empty <- dt[
    ,
    Reduce(`&`, lapply(.SD, function(x) {
      x <- trimws(as.character(x))
      is.na(x) | !nzchar(x)
    })),
    .SDcols = value_cols
  ]
  dt[!is_empty]
}

combine_reference_tables <- function(refs_2016, refs_2022) {
  refs_2016 <- remove_empty_rows(data.table::copy(refs_2016), ignore_cols = "_id")
  refs_2022 <- remove_empty_rows(data.table::copy(refs_2022), ignore_cols = "_id")

  if ("_id" %in% names(refs_2016) && "_id" %in% names(refs_2022)) {
    start_id <- max(as.integer(refs_2016[["_id"]]), na.rm = TRUE) + 1L
    refs_2022 <- refs_2022[order(as.integer(refs_2022[["_id"]]))]
    refs_2022[, `_id` := start_id + seq_len(.N) - 1L]
  }

  refs_2016[, data_release_year := 2016L]
  refs_2022[, data_release_year := 2022L]
  ordered_cols <- c(
    setdiff(names(refs_2016), "data_release_year"),
    setdiff(names(refs_2022), c(names(refs_2016), "data_release_year")),
    "data_release_year"
  )
  combined <- data.table::rbindlist(list(refs_2016, refs_2022), fill = TRUE, use.names = TRUE)
  data.table::setcolorder(combined, ordered_cols)
  remove_empty_rows(combined, ignore_cols = "data_release_year")
}

screen_reference_overlap <- function(refs_2016, refs_2022) {
  keys <- intersect(
    c("_id", "Source_ID", "DOI", "BibTeX_reference", "Reference", "Title"),
    intersect(names(refs_2016), names(refs_2022))
  )
  overlap_rows <- list()
  summary_rows <- lapply(keys, function(key) {
    left <- normalize_key(refs_2016[[key]])
    right <- normalize_key(refs_2022[[key]])
    left <- left[!is.na(left) & nzchar(left)]
    right <- right[!is.na(right) & nzchar(right)]
    overlaps <- intersect(left, right)
    if (length(overlaps) > 0) {
      overlap_rows[[key]] <<- data.table::data.table(overlap_key = key, overlap_value = overlaps)
    }
    data.table::data.table(
      overlap_key = key,
      n_overlap_values = length(overlaps),
      blocks_combining = key != "_id" && length(overlaps) > 0,
      note = if (key == "_id") {
        "_id is a resource-local NHM row identifier; overlaps are expected and ignored."
      } else {
        "Meaningful reference overlap screen."
      }
    )
  })
  list(
    summary = data.table::rbindlist(summary_rows),
    details = if (length(overlap_rows) == 0) {
      data.table::data.table(overlap_key = character(), overlap_value = character())
    } else {
      data.table::rbindlist(overlap_rows)
    }
  )
}

# =====================================================
# BioTIME taxonomy helpers
# =====================================================

build_plant_lookup_if_missing <- function(force = FALSE) {
  if (file.exists(plant_codes_path) && !force) {
    message("Using existing plant code lookup: ", plant_codes_path)
    return(invisible(plant_codes_path))
  }

  assert_file_exists(taxon_path, "GBIF taxon lookup")
  message("Building BONAP-style plant code lookup from GBIF taxon table.")
  taxon <- data.table::as.data.table(readRDS(taxon_path))
  assert_has_cols(
    taxon,
    c("taxonID", "taxonRank", "phylum", "genus", "specificEpithet"),
    "GBIF taxon lookup"
  )

  plant_species <- taxon[
    taxonRank == "species" &
      phylum == "Tracheophyta" &
      !is.na(genus) & nzchar(genus) &
      !is.na(specificEpithet) & nzchar(specificEpithet)
  ]
  plant_species <- unique(plant_species, by = c("genus", "specificEpithet"))
  plant_species[
    ,
    `:=`(
      code8 = paste0(
        toupper(stringr::str_pad(substr(genus, 1, 4), 4, pad = "X")),
        toupper(stringr::str_pad(substr(specificEpithet, 1, 4), 4, pad = "X"))
      ),
      code7_3_4 = paste0(
        toupper(substr(genus, 1, 3)),
        toupper(substr(specificEpithet, 1, 4))
      ),
      code7_4_3 = paste0(
        toupper(substr(genus, 1, 4)),
        toupper(substr(specificEpithet, 1, 3))
      ),
      plant_binomial = paste(genus, specificEpithet)
    )
  ]
  plant_lookup <- unique(plant_species, by = "code8")
  plant_lookup <- unique(plant_lookup, by = "code7_3_4")
  plant_lookup <- unique(plant_lookup, by = "code7_4_3")
  plant_lookup <- plant_lookup[, .(plant_binomial, code8, code7_3_4, code7_4_3, taxonID)]
  saveRDS(as.data.frame(plant_lookup), plant_codes_path, compress = "gzip")
  message("Wrote: ", plant_codes_path)
  invisible(plant_codes_path)
}

sample_date_from_parts <- function(year, month, day) {
  year <- suppressWarnings(as.integer(year))
  month <- suppressWarnings(as.integer(month))
  day <- suppressWarnings(as.integer(day))
  month[is.na(month) | month < 1L | month > 12L] <- 6L
  day[is.na(day) | day < 1L | day > 31L] <- 15L
  date_text <- sprintf("%04d-%02d-%02d", year, month, day)
  date <- suppressWarnings(as.Date(date_text))
  bad <- is.na(date) & !is.na(year)
  if (any(bad)) {
    date[bad] <- as.Date(sprintf("%04d-%02d-15", year[bad], month[bad]))
  }
  date
}

check_biotimer_schema <- function(observation_cols, metadata_cols) {
  if (!requireNamespace("BioTIMEr", quietly = TRUE)) {
    message("BioTIMEr is not installed; skipping optional packaged-subset schema check.")
    return(invisible(FALSE))
  }
  
  message("Checking BioTIMEr packaged subset schema.")
  biotimer_env <- new.env(parent = emptyenv())
  utils::data("BTsubset_data", package = "BioTIMEr", envir = biotimer_env)
  utils::data("BTsubset_meta", package = "BioTIMEr", envir = biotimer_env)
  
  missing_observation_cols <- setdiff(observation_cols, names(biotimer_env$BTsubset_data))
  missing_metadata_cols <- setdiff(metadata_cols, names(biotimer_env$BTsubset_meta))
  
  if (length(missing_observation_cols) > 0 || length(missing_metadata_cols) > 0) {
    warning(
      "BioTIMEr packaged subset schema differs from expected full BioTIME schema. ",
      "Observation columns missing from subset: ",
      paste(missing_observation_cols, collapse = ", "),
      "; metadata columns missing from subset: ",
      paste(missing_metadata_cols, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

load_biotime_observations <- function(path, select_cols) {
  dt <- data.table::as.data.table(readRDS(path))
  assert_has_cols(dt, select_cols, "BioTIME observations")
  dt[, ..select_cols]
}

apply_lookup <- function(dt, input_col, lookup, lookup_col, result_col, method) {
  unresolved <- is.na(dt$resolution_method)
  if (!any(unresolved)) return(invisible(dt))
  lookup <- lookup[!is.na(get(lookup_col)) & nzchar(get(lookup_col))]
  lookup <- unique(lookup, by = lookup_col)
  row_idx <- which(unresolved)
  hits <- match(dt[[input_col]][row_idx], lookup[[lookup_col]])
  matched <- !is.na(hits)
  if (any(matched)) {
    dt[
      row_idx[matched],
      `:=`(
        resolved_name = lookup[[result_col]][hits[matched]],
        resolution_method = method
      )
    ]
  }
  invisible(dt)
}

prepare_taxonomy_lookup <- function(taxon_path, ranks = c("species", "genus")) {
  taxon <- data.table::as.data.table(readRDS(taxon_path))
  taxon <- taxon[taxonRank %in% ranks]
  taxon <- taxon[
    (taxonRank == "species" & !is.na(genus) & nzchar(genus) &
      !is.na(specificEpithet) & nzchar(specificEpithet)) |
      (taxonRank == "genus" & !is.na(genus) & nzchar(genus))
  ]
  taxon[
    ,
    resolved_name := data.table::fifelse(
      !is.na(canonicalName) & nzchar(canonicalName),
      canonicalName,
      data.table::fifelse(
        taxonRank == "species",
        paste(genus, specificEpithet),
        genus
      )
    )
  ]
  taxon[, rank_priority := match(taxonRank, c("species", "genus"))]
  taxon <- taxon[
    ,
    .(
      resolved_name,
      resolved_key = tolower(trimws(resolved_name)),
      taxonID,
      taxonRank,
      rank_priority,
      taxonomicStatus,
      Kingdom = kingdom,
      Phylum = phylum,
      Class = class,
      Order = order,
      Family = family,
      Genus = genus,
      Species = data.table::fifelse(taxonRank == "species", specificEpithet, NA_character_)
    )
  ]
  data.table::setorder(taxon, resolved_key, rank_priority, taxonomicStatus)
  unique(taxon, by = "resolved_key")
}

taxonomy_cols <- function() {
  c("taxonID", "taxonRank", "taxonomicStatus", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
}

load_taxonomy_cache <- function(cache_path) {
  required_cols <- c("valid_name_original", "resolved_name", "resolution_method", taxonomy_cols())
  if (!file.exists(cache_path)) {
    return(data.table::data.table(
      valid_name_original = character(),
      resolved_name = character(),
      resolution_method = character(),
      taxonID = character(),
      taxonRank = character(),
      taxonomicStatus = character(),
      Kingdom = character(),
      Phylum = character(),
      Class = character(),
      Order = character(),
      Family = character(),
      Genus = character(),
      Species = character()
    ))
  }
  
  cache <- data.table::as.data.table(readRDS(cache_path))
  missing_cols <- setdiff(required_cols, names(cache))
  if ("taxonRank" %in% missing_cols) {
    cache[, taxonRank := NA_character_]
    missing_cols <- setdiff(required_cols, names(cache))
  }
  if (length(missing_cols) > 0) {
    stop("Taxonomy cache is missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  unique(cache[, ..required_cols], by = "valid_name_original")
}

save_taxonomy_cache <- function(cache, cache_path) {
  cache <- unique(cache, by = "valid_name_original")
  saveRDS(as.data.frame(cache), cache_path, compress = "gzip")
  message("Wrote: ", cache_path)
  invisible(cache)
}

resolve_biotime_taxonomy_names <- function(names_dt) {
  names_dt <- data.table::copy(names_dt)
  names_dt[
    ,
    `:=`(
      resolved_name = NA_character_,
      resolution_method = NA_character_
    )
  ]
  
  message("Loading GBIF/taxon lookup table.")
  taxon_lookup <- prepare_taxonomy_lookup(taxon_path)
  names_dt[, valid_name_key := tolower(trimws(as.character(valid_name_original)))]
  direct_hits <- match(names_dt$valid_name_key, taxon_lookup$resolved_key)
  matched <- !is.na(direct_hits)
  names_dt[
    matched,
    `:=`(
      resolved_name = taxon_lookup$resolved_name[direct_hits[matched]],
      resolution_method = "taxon_direct_species"
    )
  ]
  
  if (file.exists(bird_codes_path)) {
    message("Resolving BioTIME bird codes/common names.")
    bird <- data.table::as.data.table(readRDS(bird_codes_path))
    bird[
      ,
      `:=`(
        four_code = toupper(trimws(SPEC)),
        six_code = toupper(trimws(SPEC6)),
        common_clean = tolower(gsub("[^a-z]", "", COMMONNAME)),
        scientific_name = trimws(SCINAME)
      )
    ]
    names_dt[
      ,
      `:=`(
        input_upper = toupper(trimws(as.character(valid_name_original))),
        input_clean = tolower(gsub("[^a-z]", "", as.character(valid_name_original)))
      )
    ]
    apply_lookup(names_dt, "input_upper", bird, "four_code", "scientific_name", "bird_4letter")
    apply_lookup(names_dt, "input_upper", bird, "six_code", "scientific_name", "bird_6letter")
    apply_lookup(names_dt, "input_clean", bird, "common_clean", "scientific_name", "bird_common")
  }
  
  if (file.exists(plant_codes_path)) {
    message("Resolving BioTIME plant codes.")
    plant <- data.table::as.data.table(readRDS(plant_codes_path))
    plant <- plant[!is.na(plant_binomial) & nzchar(plant_binomial)]
    plant[
      ,
      `:=`(
        code8 = toupper(trimws(code8)),
        code7_3_4 = toupper(trimws(code7_3_4)),
        code7_4_3 = toupper(trimws(code7_4_3))
      )
    ]
    names_dt[
      ,
      code_input := data.table::fifelse(
        nchar(as.character(valid_name_original)) %in% c(7L, 8L) &
          grepl("^[A-Z]+$", as.character(valid_name_original)),
        as.character(valid_name_original),
        NA_character_
      )
    ]
    apply_lookup(names_dt, "code_input", plant, "code8", "plant_binomial", "plant_8char")
    apply_lookup(names_dt, "code_input", plant, "code7_3_4", "plant_binomial", "plant_7char_3_4")
    apply_lookup(names_dt, "code_input", plant, "code7_4_3", "plant_binomial", "plant_7char_4_3")
  }
  
  names_dt[, resolved_key := tolower(trimws(resolved_name))]
  hits <- match(names_dt$resolved_key, taxon_lookup$resolved_key)
  matched <- !is.na(hits)
  for (col in taxonomy_cols()) {
    names_dt[, (col) := NA_character_]
    data.table::set(names_dt, i = which(matched), j = col, value = as.character(taxon_lookup[[col]][hits[matched]]))
  }
  
  cleanup_cols <- intersect(
    c("valid_name_key", "input_upper", "input_clean", "code_input", "resolved_key"),
    names(names_dt)
  )
  if (length(cleanup_cols) > 0) names_dt[, (cleanup_cols) := NULL]
  names_dt
}

resolve_biotime_taxonomy <- function(dt, cache_path = biotime_taxonomy_cache_rds_path) {
  assert_file_exists(taxon_path, "curated taxon lookup")
  assert_file_exists(bird_codes_path, "curated bird code lookup")
  assert_file_exists(plant_codes_path, "curated plant code lookup")
  
  dt <- data.table::copy(dt)
  dt[, valid_name_original := as.character(valid_name)]
  
  unique_names <- unique(dt[!is.na(valid_name_original) & nzchar(valid_name_original), .(valid_name_original)])
  cache <- load_taxonomy_cache(cache_path)
  names_to_resolve <- unique_names[!valid_name_original %in% cache$valid_name_original]
  
  message("BioTIME taxonomy cache rows: ", nrow(cache))
  message("BioTIME names needing taxonomy resolution: ", nrow(names_to_resolve))
  
  if (nrow(names_to_resolve) > 0) {
    resolved_new <- resolve_biotime_taxonomy_names(names_to_resolve)
    cache <- data.table::rbindlist(list(cache, resolved_new), fill = TRUE)
    cache <- save_taxonomy_cache(cache, cache_path)
  }
  
  dt <- merge(dt, cache, by = "valid_name_original", all.x = TRUE, sort = FALSE)
  dt
}

# =====================================================
# Integrated output and overlap helpers
# =====================================================

split_doi_values <- function(x) {
  doi <- extract_doi(x)
  if (is.na(doi) || !nzchar(doi)) return(character())
  unique(unlist(strsplit(doi, "\\s*\\|\\s*")))
}

expand_reference_dois <- function(dt, doi_col) {
  dt <- data.table::copy(data.table::as.data.table(dt))
  if (!doi_col %in% names(dt)) {
    dt[, (doi_col) := NA_character_]
  }
  dt[, row_id := .I]
  doi_long <- dt[, .(doi = split_doi_values(get(doi_col))), by = row_id]
  doi_long <- doi_long[!is.na(doi) & nzchar(doi)]
  if (nrow(doi_long) == 0) {
    dt[, row_id := NULL]
    return(data.table::data.table())
  }
  out <- merge(doi_long, dt, by = "row_id", all.x = TRUE, sort = FALSE)
  out[, row_id := NULL]
  out
}

summarize_predicts_reference_dois <- function(predicts_references) {
  p <- expand_reference_dois(predicts_references, "DOI")
  if (nrow(p) == 0) {
    return(data.table::data.table(
      doi = character(),
      predicts_source_ids = character(),
      predicts_references = character(),
      predicts_titles = character()
    ))
  }
  p[
    ,
    .(
      predicts_source_ids = collapse_unique(if ("Source_ID" %in% names(.SD)) Source_ID else NA_character_),
      predicts_references = collapse_unique(if ("Reference" %in% names(.SD)) Reference else NA_character_),
      predicts_titles = collapse_unique(if ("Title" %in% names(.SD)) Title else NA_character_)
    ),
    by = doi
  ]
}

summarize_biotime_reference_dois <- function(biotime_references) {
  b <- expand_reference_dois(biotime_references, "DOI")
  if (nrow(b) == 0) {
    return(data.table::data.table(
      doi = character(),
      biotime_study_ids = character(),
      biotime_citation_ids = character(),
      biotime_titles = character()
    ))
  }
  b[
    ,
    .(
      biotime_study_ids = collapse_unique(if ("STUDY_ID" %in% names(.SD)) STUDY_ID else NA_character_),
      biotime_citation_ids = collapse_unique(if ("CITATION_ID" %in% names(.SD)) CITATION_ID else NA_character_),
      biotime_titles = collapse_unique(if ("reference_title" %in% names(.SD)) reference_title else NA_character_)
    ),
    by = doi
  ]
}

screen_biotime_predicts_doi_overlap <- function(predicts_references, biotime_references, biotime_output) {
  p <- summarize_predicts_reference_dois(predicts_references)
  b <- summarize_biotime_reference_dois(biotime_references)
  candidates <- merge(b, p, by = "doi", all = FALSE, sort = FALSE)

  b_long <- expand_reference_dois(biotime_references, "DOI")
  excluded_studies <- if (nrow(candidates) == 0 || nrow(b_long) == 0) {
    data.table::data.table(
      study_id = character(),
      doi = character(),
      citation_id = character(),
      reference_title = character()
    )
  } else {
    unique(
      b_long[
        doi %in% candidates$doi,
        .(
          study_id = as.character(STUDY_ID),
          doi,
          citation_id = as.character(CITATION_ID),
          reference_title = if ("reference_title" %in% names(b_long)) reference_title else NA_character_
        )
      ]
    )
  }

  excluded_ids <- unique(excluded_studies$study_id)
  excluded_rows <- data.table::as.data.table(biotime_output)[study_id %in% excluded_ids, .N]
  summary <- data.table::data.table(
    metric = c(
      "predicts_unique_dois",
      "biotime_unique_dois",
      "overlap_unique_dois",
      "biotime_studies_excluded_from_combined",
      "biotime_rows_excluded_from_combined"
    ),
    value = c(
      nrow(p),
      nrow(b),
      data.table::uniqueN(candidates$doi),
      length(excluded_ids),
      excluded_rows
    )
  )

  write_csv_safe(candidates, biotime_reference_overlap_path)
  write_csv_safe(excluded_studies, biotime_overlap_excluded_studies_path)
  write_csv_safe(summary, biotime_predicts_overlap_summary_path)

  list(
    candidates = candidates,
    excluded_studies = excluded_studies,
    excluded_study_ids = excluded_ids,
    summary = summary
  )
}

select_integrated_cols <- function(dt) {
  dt <- data.table::copy(data.table::as.data.table(dt))
  missing_cols <- setdiff(integrated_database_cols, names(dt))
  for (col in missing_cols) {
    dt[, (col) := NA]
  }
  data.table::setcolorder(dt, c(integrated_database_cols, setdiff(names(dt), integrated_database_cols)))
  dt[, ..integrated_database_cols]
}

build_predicts_output_database <- function(predicts_filtered, predicts_references) {
  predicts_dt <- data.table::copy(data.table::as.data.table(predicts_filtered))
  predicts_refs <- data.table::copy(data.table::as.data.table(predicts_references))
  if ("Source_ID" %in% names(predicts_refs)) {
    ref_lookup <- predicts_refs[
      ,
      .(
        doi = collapse_unique(if ("DOI" %in% names(.SD)) DOI else NA_character_),
        reference_title = collapse_unique(if ("Title" %in% names(.SD)) Title else NA_character_)
      ),
      by = Source_ID
    ]
    predicts_dt <- merge(predicts_dt, ref_lookup, by = "Source_ID", all.x = TRUE, sort = FALSE)
  } else {
    predicts_dt[, `:=`(doi = NA_character_, reference_title = NA_character_)]
  }

  predicts_dt[
    ,
    `:=`(
      database = "PREDICTS",
      sample_id = paste("PREDICTS", SSBS, Sample_midpoint, sep = "__"),
      site_id = paste0("PREDICTS:", SSBS),
      assemblage_id = paste0("PREDICTS:", SSBS),
      study_id = as.character(Study_number),
      source_id = as.character(Source_ID),
      standard_source_id = paste0("PREDICTS:", Source_ID),
      reference = data.table::fifelse(
        !is.na(reference_title) & nzchar(reference_title),
        reference_title,
        as.character(Reference)
      ),
      sample_year = as.integer(format(as.Date(Sample_midpoint), "%Y")),
      sample_start_date = as.Date(Sample_start_earliest),
      sample_end_date = as.Date(Sample_end_latest),
      sample_midpoint = as.Date(Sample_midpoint),
      latitude = as.numeric(Latitude),
      longitude = as.numeric(Longitude),
      taxon_name = paste(Family, Genus, sep = "_"),
      resolved_name = data.table::fifelse(
        !is.na(Genus) & nzchar(as.character(Genus)),
        as.character(Genus),
        as.character(Family)
      ),
      Species = NA_character_,
      measurement = as.numeric(Measurement),
      abundance = as.numeric(Effort_corrected_measurement),
      effort_corrected_measurement = as.numeric(Effort_corrected_measurement),
      sampling_effort = as.numeric(Sampling_effort),
      sampling_effort_unit = as.character(Sampling_effort_unit),
      effort_standardized = !is.na(Effort_corrected_measurement),
      effort_standardization_method = "PREDICTS Effort_corrected_measurement",
      raw_sample_events_available = NA_integer_,
      sample_events_selected = NA_integer_,
      n_observation_rows = NA_integer_,
      rarefaction_seed = NA_integer_,
      biotime_grid_resolution = NA_integer_
    )
  ]
  select_integrated_cols(predicts_dt)
}

standardize_biotime_sampling <- function(
  biotime_filtered,
  biotime_metadata,
  biotime_reference_table,
  grid_resolution = biotime_grid_resolution,
  rarefaction_seed = biotime_rarefaction_seed
) {
  dt <- data.table::copy(data.table::as.data.table(biotime_filtered))
  meta <- data.table::copy(data.table::as.data.table(biotime_metadata))
  refs <- data.table::copy(data.table::as.data.table(biotime_reference_table))

  assert_has_cols(
    dt,
    c(
      "STUDY_ID", "SAMPLE_DESC", "LATITUDE", "LONGITUDE", "YEAR", "MONTH",
      "DAY", "taxon", "valid_name", "resolution", "ABUNDANCE", "BIOMASS",
      "sample_event_id", "Family", "Genus"
    ),
    "Filtered BioTIME observations"
  )
  assert_has_cols(
    meta,
    c(
      "STUDY_ID", "NUMBER_LAT_LONG", "AREA_SQ_KM", "CENT_LONG", "CENT_LAT",
      "REALM", "CLIMATE", "TAXA", "ABUNDANCE_TYPE", "BIOMASS_TYPE"
    ),
    "BioTIME metadata"
  )

  btf_for_gridding <- data.table::copy(dt)
  btf_for_gridding[is.na(BIOMASS), BIOMASS := 0]
  btf_for_gridding[is.na(ABUNDANCE), ABUNDANCE := 0]

  message("Assigning BioTIME rows to BioTIMEr assemblage grid cells.")
  gridded <- BioTIMEr::gridding(
    meta = as.data.frame(meta[STUDY_ID %in% unique(dt$STUDY_ID)]),
    btf = as.data.frame(btf_for_gridding),
    res = grid_resolution,
    verbose = TRUE
  )
  gridded <- data.table::as.data.table(gridded)
  if (nrow(gridded) == 0) {
    stop("BioTIME gridding returned no rows.", call. = FALSE)
  }
  gridded[
    ,
    sample_event_id := paste(
      STUDY_ID, SAMPLE_DESC, LATITUDE, LONGITUDE, YEAR, MONTH, DAY,
      sep = "__"
    )
  ]
  event_lookup <- unique(
    gridded[
      ,
      .(
        sample_event_id,
        STUDY_ID,
        YEAR,
        assemblageID,
        cell,
        StudyMethod
      )
    ],
    by = "sample_event_id"
  )
  saveRDS(as.data.frame(event_lookup), biotime_gridded_event_lookup_rds_path, compress = "gzip")
  message("Wrote: ", biotime_gridded_event_lookup_rds_path)

  dt <- merge(
    dt,
    event_lookup[, .(sample_event_id, assemblageID, cell, StudyMethod)],
    by = "sample_event_id",
    all = FALSE,
    sort = FALSE
  )
  if (nrow(dt) == 0) {
    stop("No BioTIME rows remained after gridded assemblage assignment.", call. = FALSE)
  }

  event_counts <- unique(dt[, .(assemblageID, YEAR, sample_event_id)])
  year_counts <- event_counts[
    ,
    .(raw_sample_events_available = data.table::uniqueN(sample_event_id)),
    by = .(assemblageID, YEAR)
  ]
  assemblage_targets <- year_counts[
    ,
    .(
      n_years = .N,
      min_sample_events_per_year = min(raw_sample_events_available, na.rm = TRUE)
    ),
    by = assemblageID
  ]
  assemblage_targets <- assemblage_targets[n_years >= 2L & min_sample_events_per_year > 0L]
  if (nrow(assemblage_targets) == 0) {
    stop("No BioTIME assemblages had at least two years for rarefaction.", call. = FALSE)
  }

  set.seed(rarefaction_seed)
  selectable_events <- merge(
    event_counts,
    assemblage_targets[, .(assemblageID, min_sample_events_per_year)],
    by = "assemblageID",
    all = FALSE,
    sort = FALSE
  )
  data.table::setorder(selectable_events, assemblageID, YEAR, sample_event_id)
  selectable_events[, rarefaction_order := runif(.N)]
  selected_events <- selectable_events[
    order(rarefaction_order),
    utils::head(.SD, min_sample_events_per_year[1]),
    by = .(assemblageID, YEAR)
  ]
  selected_events[, rarefaction_order := NULL]
  selected_events <- merge(
    selected_events,
    year_counts,
    by = c("assemblageID", "YEAR"),
    all.x = TRUE,
    sort = FALSE
  )
  selected_events[, sample_events_selected := min_sample_events_per_year]
  saveRDS(as.data.frame(selected_events), biotime_rarefied_events_rds_path, compress = "gzip")
  message("Wrote: ", biotime_rarefied_events_rds_path)

  selected <- merge(
    dt,
    selected_events[
      ,
      .(
        assemblageID, YEAR, sample_event_id,
        raw_sample_events_available, sample_events_selected
      )
    ],
    by = c("assemblageID", "YEAR", "sample_event_id"),
    all = FALSE,
    sort = FALSE
  )
  selected[, sample_date := sample_date_from_parts(YEAR, MONTH, DAY)]
  selected[
    ,
    taxon_name := data.table::fifelse(
      !is.na(resolved_name) & nzchar(resolved_name),
      resolved_name,
      as.character(valid_name_original)
    )
  ]

  refs_summary <- refs[
    ,
    .(
      doi = collapse_unique(if ("DOI" %in% names(.SD)) DOI else NA_character_),
      reference = collapse_unique(
        if ("reference_title" %in% names(.SD)) reference_title else
          if ("TITLE" %in% names(.SD)) TITLE else
            if ("BIB" %in% names(.SD)) BIB else NA_character_
      )
    ),
    by = STUDY_ID
  ]
  meta_summary <- meta[
    ,
    .(
      metadata_title = collapse_unique(if ("TITLE" %in% names(.SD)) TITLE else NA_character_)
    ),
    by = STUDY_ID
  ]

  group_cols <- c(
    "STUDY_ID", "assemblageID", "cell", "StudyMethod", "YEAR", "taxon_name",
    "resolved_name", taxonomy_cols(), "raw_sample_events_available",
    "sample_events_selected"
  )
  group_cols <- intersect(group_cols, names(selected))
  standardized <- selected[
    ,
    .(
      measurement = sum(as.numeric(ABUNDANCE), na.rm = TRUE),
      abundance = sum(as.numeric(ABUNDANCE), na.rm = TRUE),
      effort_corrected_measurement = sum(as.numeric(ABUNDANCE), na.rm = TRUE),
      n_observation_rows = .N,
      sample_start_date = min(sample_date, na.rm = TRUE),
      sample_end_date = max(sample_date, na.rm = TRUE),
      latitude = mean(as.numeric(LATITUDE), na.rm = TRUE),
      longitude = mean(as.numeric(LONGITUDE), na.rm = TRUE)
    ),
    by = group_cols
  ]
  standardized[
    ,
    sample_midpoint := as.Date(
      as.numeric(sample_start_date) +
        floor((as.numeric(sample_end_date) - as.numeric(sample_start_date)) / 2),
      origin = "1970-01-01"
    )
  ]
  standardized <- merge(standardized, refs_summary, by = "STUDY_ID", all.x = TRUE, sort = FALSE)
  standardized <- merge(standardized, meta_summary, by = "STUDY_ID", all.x = TRUE, sort = FALSE)
  standardized[
    ,
    reference := data.table::fifelse(
      !is.na(reference) & nzchar(reference),
      reference,
      metadata_title
    )
  ]
  standardized[
    ,
    `:=`(
      database = "BioTIME",
      sample_id = paste("BioTIME", assemblageID, YEAR, "rarefied", sep = "__"),
      site_id = paste0("BioTIME:", assemblageID),
      assemblage_id = paste0("BioTIME:", assemblageID),
      study_id = as.character(STUDY_ID),
      source_id = paste0("BioTIME_STUDY_", STUDY_ID),
      standard_source_id = paste0("BioTIME:", STUDY_ID),
      sample_year = as.integer(YEAR),
      sampling_effort = as.numeric(sample_events_selected),
      sampling_effort_unit = "sample_events_per_assemblage_year",
      effort_standardized = TRUE,
      effort_standardization_method = paste0(
        "BioTIMEr gridding res=", grid_resolution,
        " plus deterministic sample-event rarefaction"
      ),
      rarefaction_seed = as.integer(rarefaction_seed),
      biotime_grid_resolution = as.integer(grid_resolution)
    )
  ]
  standardized[, metadata_title := NULL]
  select_integrated_cols(standardized)
}

write_output_manifest <- function(rows) {
  output <- data.table::rbindlist(rows, fill = TRUE)
  write_csv_safe(output, output_manifest_path)
  invisible(output)
}

manifest_row <- function(output_name, path, data, description) {
  data.table::data.table(
    output_name = output_name,
    path = path,
    rows = nrow(data),
    columns = ncol(data),
    description = description
  )
}
