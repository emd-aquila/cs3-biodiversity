# =====================================================
# Shared helpers for the structured BioTIME pipeline.
# =====================================================

assert_file_exists <- function(path, label = "required file") {
  if (!file.exists(path)) {
    stop("Missing ", label, ": ", path, call. = FALSE)
  }
}

assert_has_cols <- function(data, cols, label = "data") {
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

normalize_text <- function(x) {
  x <- stringr::str_to_lower(as.character(x))
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", " ")
  stringr::str_squish(x)
}

extract_dois <- function(x) {
  matches <- stringr::str_extract_all(
    as.character(x),
    stringr::regex("10\\.[0-9]{4,9}/[-._;()/:A-Za-z0-9]+", ignore_case = TRUE)
  )

  vapply(matches, collapse_unique, character(1))
}

extract_bib_field <- function(x, field) {
  pattern <- stringr::regex(
    paste0("\\b", field, "\\s*=\\s*\\{([^}]*)\\}"),
    ignore_case = TRUE,
    multiline = TRUE
  )
  stringr::str_match(as.character(x), pattern)[, 2]
}

extract_first_author_surname <- function(author_text) {
  vapply(
    as.character(author_text),
    function(author) {
      if (is.na(author) || !nzchar(author)) return(NA_character_)
      first_author <- stringr::str_split(author, "\\s+and\\s+", n = 2)[[1]][[1]]
      first_author <- stringr::str_replace_all(first_author, "[{}]", "")
      first_author <- stringr::str_squish(first_author)

      if (stringr::str_detect(first_author, ",")) {
        surname <- stringr::str_split(first_author, ",", n = 2)[[1]][[1]]
      } else {
        surname <- tail(stringr::str_split(first_author, "\\s+")[[1]], 1)
      }

      normalize_text(surname)
    },
    character(1)
  )
}

build_biotime_author_year_key <- function(bib) {
  author <- extract_bib_field(bib, "Author")
  year <- extract_bib_field(bib, "Year")
  surname <- extract_first_author_surname(author)
  ifelse(
    !is.na(surname) & nzchar(surname) & !is.na(year) & nzchar(year),
    paste(surname, year, sep = "_"),
    NA_character_
  )
}

build_predicts_author_year_key <- function(reference) {
  reference <- as.character(reference)
  surname <- stringr::str_extract(reference, "^[[:alpha:]'-]+")
  year <- stringr::str_extract(reference, "(19|20)[0-9]{2}")
  surname <- normalize_text(surname)
  ifelse(
    !is.na(surname) & nzchar(surname) & !is.na(year),
    paste(surname, year, sep = "_"),
    NA_character_
  )
}

haversine_km <- function(lat1, lon1, lat2, lon2) {
  earth_radius_km <- 6371.0088
  to_radians <- pi / 180
  lat1 <- lat1 * to_radians
  lon1 <- lon1 * to_radians
  lat2 <- lat2 * to_radians
  lon2 <- lon2 * to_radians
  delta_lat <- lat2 - lat1
  delta_lon <- lon2 - lon1
  a <- sin(delta_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
  2 * earth_radius_km * atan2(sqrt(a), sqrt(1 - a))
}

format_count <- function(x) {
  format(as.numeric(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}
