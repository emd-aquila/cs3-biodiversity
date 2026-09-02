# =============================================================================
# AEZ x land-use-transition biodiversity response matrix
# =============================================================================
#
# Purpose
# -------
# Produce the coefficient EPPA needs:
#
#     "X additional hectares of deforestation (forest class f -> land use l)
#      in AEZ a changes the AEZ's biodiversity by Y %"
#
# using the PREDICTS Biodiversity-Intactness logic already implemented in this
# stage (De Palma / Newbold structure): local abundance of a land-use class
# relative to minimally used primary vegetation, estimated *within study*.
#
# Definitions
# -----------
#   B[a, l]      intactness of land-use class l in AEZ a (1 = primary-minimal),
#                = squared prediction of the sqrt(relative abundance) mixed
#                model, divided by the reference prediction; optionally
#                multiplied by the compositional-similarity response
#                (bii_class_response, as in model_response_table()).
#   Local effect of converting one hectare from f to l:
#                local_change_pct[a, f->l] = 100 * (B[a, l] / B[a, f] - 1)
#   AEZ-mean effect (area-weighted mean intactness of the AEZ):
#                delta_pct_per_ha[a, f->l] = 100 * (B[a, l] - B[a, f]) / A[a]
#                where A[a] is the AEZ land area in hectares, so that
#                Y % = X ha * delta_pct_per_ha.
#
# The AEZ-mean coefficient is linear in hectares and is the marginal effect
# for small changes. For EPPA's own region x AEZ areas, supply a table
# (see aez_region_area_path) and the matrix is also written per region.
#
# Estimation and pooling
# ----------------------
# For each AEZ the abundance model
#     sqrt_relative_abundance ~ pressure_class + (1 | study_id) + (1 | block_id)
# is fitted on the PREDICTS sites falling inside that AEZ.  A class response
# is taken from the individual-AEZ model only when the AEZ has enough sites
# and studies overall AND enough sites in that class and in the reference
# class; otherwise it falls back to the AEZ climate group (tropical AEZ1-6,
# temperate AEZ7-12, boreal/cold AEZ13-18) and then to the global model.  The
# provenance of every cell is recorded.  Confidence intervals come from a
# study-level (cluster) bootstrap, which respects the PREDICTS design.
#
# Inputs (all produced by the existing workflow)
# ----------------------------------------------
#   predicts_site_path            bii_metric/tmp/predicts_site_abundance.rds (03_prepare_predicts.R)
#   composition_pairs_path        optional, for the compositional component
#   00_spatial_data/aez/AEZ_shp_file.shp
#   optional: input/eppa/eppa_aez_area_ha.csv  (region, aez, area_ha)
#   optional: input/eppa/eppa_aez_transitions.csv (scenario, year, region, aez,
#             from_class, to_class, area_ha) to evaluate a scenario directly.
#
# Outputs (bii_metric/output/aez_matrix/)
# ---------------------------------------
#   aez_site_coverage.csv                sites/studies per AEZ x class
#   aez_class_response.csv               B[a, l] with CI, n, provenance
#   aez_transition_local_effect.csv      100*(B_to/B_from - 1) with CI
#   aez_deforestation_matrix.csv         % of AEZ-mean intactness per ha / per 1000 ha
#   aez_deforestation_matrix_wide.csv    AEZ x transition, % per 1000 ha
#   aez_region_deforestation_matrix.csv  same per EPPA region x AEZ (if areas supplied)
#   aez_scenario_evaluation.csv          delta % per scenario/year/region/AEZ (if transitions supplied)
#   aez_matrix_model_diagnostics.csv     convergence / singularity per fitted scope
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
})

# ----------------------------------------------------------------------------
# Settings (override with environment variables where indicated)
# ----------------------------------------------------------------------------
aez_matrix_output_dir <- file.path(output_dir, "aez_matrix")
dir.create(aez_matrix_output_dir, recursive = TRUE, showWarnings = FALSE)

aez_shapefile_path <- file.path(repo_root, "00_spatial_data", "aez", "AEZ_shp_file.shp")
aez_region_area_path <- file.path(input_dir, "eppa", "eppa_aez_area_ha.csv")
aez_transitions_path <- file.path(input_dir, "eppa", "eppa_aez_transitions.csv")

# Minimum data for an AEZ-specific estimate; below these the class falls back
# to the AEZ group, then to the global model.
aez_min_sites_total <- as.integer(Sys.getenv("CS3_AEZ_MIN_SITES", unset = "150"))
aez_min_studies_total <- as.integer(Sys.getenv("CS3_AEZ_MIN_STUDIES", unset = "8"))
aez_min_sites_per_class <- as.integer(Sys.getenv("CS3_AEZ_MIN_CLASS_SITES", unset = "25"))
aez_min_studies_per_class <- as.integer(Sys.getenv("CS3_AEZ_MIN_CLASS_STUDIES", unset = "3"))

# Study-level bootstrap replicates for confidence intervals (0 = point estimates only).
aez_n_boot <- as.integer(Sys.getenv("CS3_AEZ_NBOOT", unset = "200"))
aez_ci_level <- 0.95
aez_boot_seed <- 20260902L

# Multiply the abundance response by the compositional-similarity response
# (full BII logic) when the cached pair table exists. Abundance-only otherwise.
aez_include_composition <- tolower(Sys.getenv("CS3_AEZ_INCLUDE_COMPOSITION", unset = "auto")) # auto | true | false

# Transitions to report. "from" classes are the forest/natural classes EPPA
# can convert; "to" classes are the destinations.
aez_from_classes <- c("primary_minimal", "primary_other", "secondary")
aez_to_classes <- c("cropland", "pasture", "plantation", "urban", "secondary")

# Taxon scopes: "all" pools every taxon group (the headline matrix); add
# BII taxon groups (e.g. "Birds", "Plants") for group-specific matrices.
aez_taxon_scopes <- strsplit(Sys.getenv("CS3_AEZ_TAXON_SCOPES", unset = "all"), ",", fixed = TRUE)[[1]]
aez_taxon_scopes <- trimws(aez_taxon_scopes[nzchar(trimws(aez_taxon_scopes))])

equal_area_crs <- 6933

# ----------------------------------------------------------------------------
# AEZ helpers
# ----------------------------------------------------------------------------
aez_number <- function(aez_label) {
  as.integer(stringr::str_extract(as.character(aez_label), "[0-9]+"))
}

aez_group_of <- function(aez_label) {
  n <- aez_number(aez_label)
  data.table::fcase(
    n >= 1L & n <= 6L, "tropical",
    n >= 7L & n <= 12L, "temperate",
    n >= 13L & n <= 18L, "boreal",
    default = NA_character_
  )
}

read_aez_polygons <- function(path = aez_shapefile_path) {
  assert_file_exists(path, "AEZ shapefile")
  sf::sf_use_s2(FALSE)
  aez <- sf::read_sf(path)
  aez <- sf::st_make_valid(aez)
  aez <- aez[, intersect(c("Id", "AEZ"), names(aez))]
  aez
}

# Land area of every AEZ in hectares (from the shapefile, equal-area CRS).
aez_area_table <- function(aez_sf) {
  projected <- sf::st_transform(aez_sf, equal_area_crs)
  areas <- data.table(
    aez = as.character(projected$AEZ),
    area_ha = as.numeric(sf::st_area(projected)) / 1e4
  )
  areas[, .(aez_area_ha = sum(area_ha, na.rm = TRUE)), by = aez][order(aez_number(aez))]
}

# Point-in-polygon AEZ assignment with nearest-polygon fallback and a row-count
# guard (a duplicated join would silently mis-index the fallback).
assign_sites_to_aez <- function(site, aez_sf) {
  pts <- site[is.finite(longitude) & is.finite(latitude)]
  if (nrow(pts) == 0) stop("No PREDICTS sites with coordinates.", call. = FALSE)
  pts_sf <- sf::st_as_sf(as.data.frame(pts[, .(study_id, site_id, longitude, latitude)]),
                         coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  pts_sf <- sf::st_transform(pts_sf, sf::st_crs(aez_sf))
  joined <- sf::st_join(pts_sf, aez_sf["AEZ"], join = sf::st_within, left = TRUE)
  if (nrow(joined) != nrow(pts_sf)) {
    # keep the first polygon per site if any polygons overlap
    joined <- joined[!duplicated(paste(joined$study_id, joined$site_id)), ]
  }
  missing <- which(is.na(joined$AEZ))
  if (length(missing) > 0) {
    nearest <- sf::st_nearest_feature(pts_sf[missing, ], aez_sf)
    joined$AEZ[missing] <- aez_sf$AEZ[nearest]
  }
  out <- data.table(
    study_id = joined$study_id,
    site_id = joined$site_id,
    aez = as.character(joined$AEZ),
    aez_assigned_by_nearest = seq_len(nrow(joined)) %in% missing
  )
  merged <- merge(site, out, by = c("study_id", "site_id"), all.x = FALSE, sort = FALSE)
  merged[, aez_group := aez_group_of(aez)]
  merged
}

# ----------------------------------------------------------------------------
# Model fitting (same structure as fit_scope_models(); abundance always,
# composition optional) and class-response extraction
# ----------------------------------------------------------------------------
fit_abundance_model <- function(abundance) {
  abundance <- copy(abundance)
  abundance[, pressure_class := relevel(factor(pressure_class), ref = reference_pressure_class)]
  if (has_lme4) {
    n_blocks <- uniqueN(abundance$block_id)
    formula_used <- if (n_blocks > uniqueN(abundance$study_id)) {
      sqrt_relative_abundance ~ pressure_class + (1 | study_id) + (1 | block_id)
    } else {
      sqrt_relative_abundance ~ pressure_class + (1 | study_id)
    }
    model <- suppressMessages(suppressWarnings(
      lme4::lmer(formula_used, data = abundance, REML = TRUE)
    ))
  } else {
    model <- stats::lm(sqrt_relative_abundance ~ pressure_class, data = abundance)
  }
  model
}

fit_composition_model <- function(pairs) {
  pairs <- copy(pairs)
  pairs[, pressure_class := relevel(factor(pressure_class), ref = reference_pressure_class)]
  if (has_lme4) {
    model <- suppressMessages(suppressWarnings(lme4::lmer(
      logit_similarity ~ pressure_class + log10_geographic_distance + (1 | study_id) + (1 | target_site_id),
      data = pairs, REML = TRUE
    )))
  } else {
    model <- stats::lm(logit_similarity ~ pressure_class + log10_geographic_distance, data = pairs)
  }
  model
}

# Relative abundance (and composition) per class from fitted models; returns
# a data.table with pressure_class, abundance_relative, composition_relative,
# class_response (= product when composition is used, abundance otherwise).
class_responses_from_models <- function(abundance_model, composition_model = NULL) {
  ab_classes <- levels(stats::model.frame(abundance_model)$pressure_class)
  new_ab <- data.frame(pressure_class = factor(ab_classes, levels = ab_classes))
  ab_pred <- predict_no_random_effects(abundance_model, new_ab)^2
  ref <- match(reference_pressure_class, ab_classes)
  if (is.na(ref) || !is.finite(ab_pred[[ref]]) || ab_pred[[ref]] <= 0) return(NULL)
  out <- data.table(
    pressure_class = ab_classes,
    abundance_relative = ab_pred / ab_pred[[ref]],
    composition_relative = NA_real_
  )
  if (!is.null(composition_model)) {
    co_classes <- levels(stats::model.frame(composition_model)$pressure_class)
    new_co <- data.frame(
      pressure_class = factor(co_classes, levels = co_classes),
      log10_geographic_distance = 0
    )
    co_pred <- inverse_adjusted_logit(predict_no_random_effects(composition_model, new_co))
    co_ref <- match(reference_pressure_class, co_classes)
    if (!is.na(co_ref) && is.finite(co_pred[[co_ref]]) && co_pred[[co_ref]] > 0) {
      co <- data.table(pressure_class = co_classes, composition_relative = co_pred / co_pred[[co_ref]])
      out[co, composition_relative := i.composition_relative, on = "pressure_class"]
    }
  }
  out[, class_response := fifelse(is.finite(composition_relative),
                                  abundance_relative * composition_relative,
                                  abundance_relative)]
  out
}

# Fit a scope (a subset of sites and pairs) once; return responses + diagnostics.
fit_scope_once <- function(abundance, pairs, use_composition) {
  abundance_model <- fit_abundance_model(abundance)
  composition_model <- NULL
  if (isTRUE(use_composition) && !is.null(pairs) && nrow(pairs) >= minimum_pairs_per_scope &&
      reference_pressure_class %in% pairs$pressure_class) {
    composition_model <- tryCatch(fit_composition_model(pairs), error = function(e) NULL)
  }
  responses <- class_responses_from_models(abundance_model, composition_model)
  diagnostics <- data.table(
    n_sites = nrow(abundance),
    n_studies = uniqueN(abundance$study_id),
    n_pairs = if (is.null(composition_model)) 0L else nrow(pairs),
    fit_type = if (inherits(abundance_model, "merMod")) "mixed_effects_lme4" else "fixed_effect_fallback",
    abundance_singular = if (inherits(abundance_model, "merMod")) lme4::isSingular(abundance_model, tol = 1e-4) else NA,
    abundance_convergence = if (inherits(abundance_model, "merMod")) {
      paste(unlist(abundance_model@optinfo$conv$lme4$messages), collapse = " | ")
    } else NA_character_,
    composition_used = !is.null(composition_model)
  )
  list(responses = responses, diagnostics = diagnostics)
}

# Study-level (cluster) bootstrap of the class responses for one scope.
bootstrap_scope <- function(abundance, pairs, use_composition, n_boot, seed) {
  if (n_boot <= 0) return(NULL)
  set.seed(seed)
  studies <- unique(abundance$study_id)
  ab_by_study <- split(abundance, abundance$study_id)
  pairs_by_study <- if (!is.null(pairs)) split(pairs, pairs$study_id) else NULL
  draws <- vector("list", n_boot)
  for (b in seq_len(n_boot)) {
    sampled <- sample(studies, length(studies), replace = TRUE)
    # Give resampled copies of the same study distinct ids so random effects
    # treat them as separate clusters.
    ab_b <- rbindlist(lapply(seq_along(sampled), function(k) {
      d <- copy(ab_by_study[[sampled[[k]]]])
      d[, `:=`(study_id = paste0(study_id, "#", k), block_id = paste0(block_id, "#", k))]
      d
    }))
    pr_b <- NULL
    if (isTRUE(use_composition) && !is.null(pairs_by_study)) {
      pr_b <- rbindlist(lapply(seq_along(sampled), function(k) {
        d <- pairs_by_study[[sampled[[k]]]]
        if (is.null(d)) return(NULL)
        d <- copy(d)
        d[, `:=`(study_id = paste0(study_id, "#", k), target_site_id = paste0(target_site_id, "#", k))]
        d
      }))
    }
    if (!reference_pressure_class %in% ab_b$pressure_class) next
    result <- tryCatch(fit_scope_once(ab_b, pr_b, use_composition)$responses, error = function(e) NULL)
    if (is.null(result)) next
    draws[[b]] <- result[, .(pressure_class, class_response, abundance_relative, boot_id = b)]
  }
  rbindlist(draws, fill = TRUE)
}

# ----------------------------------------------------------------------------
# Main estimation: individual AEZ -> AEZ group -> global, with per-class rules
# ----------------------------------------------------------------------------
estimate_aez_class_responses <- function(site_aez, pairs_aez, use_composition, taxon_scope = "all") {
  sites <- if (identical(taxon_scope, "all")) copy(site_aez) else site_aez[taxon_group == taxon_scope]
  pairs <- if (is.null(pairs_aez)) NULL else if (identical(taxon_scope, "all")) copy(pairs_aez) else pairs_aez[taxon_group == taxon_scope]
  if (nrow(sites) < minimum_sites_per_scope) {
    warning("Taxon scope '", taxon_scope, "' has fewer than ", minimum_sites_per_scope, " sites; skipped.", call. = FALSE)
    return(NULL)
  }

  scopes <- rbindlist(list(
    data.table(scope_level = "global", scope_value = "global"),
    data.table(scope_level = "aez_group", scope_value = sort(unique(na.omit(sites$aez_group)))),
    data.table(scope_level = "aez", scope_value = unique(sites$aez))
  ))
  scopes <- scopes[!is.na(scope_value)]
  scopes[, order_key := fifelse(scope_level == "aez", aez_number(scope_value), NA_integer_)]
  setorder(scopes, scope_level, order_key, na.last = TRUE)

  fitted <- vector("list", nrow(scopes))
  diagnostics <- vector("list", nrow(scopes))
  for (i in seq_len(nrow(scopes))) {
    lvl <- scopes$scope_level[[i]]
    val <- scopes$scope_value[[i]]
    ab <- switch(lvl,
      global = sites,
      aez_group = sites[aez_group == val],
      aez = sites[aez == val]
    )
    pr <- if (is.null(pairs)) NULL else switch(lvl,
      global = pairs,
      aez_group = pairs[aez_group == val],
      aez = pairs[aez == val]
    )
    # Data sufficiency for the scope as a whole
    enough <- nrow(ab) >= (if (lvl == "aez") aez_min_sites_total else minimum_sites_per_scope) &&
      uniqueN(ab$study_id) >= (if (lvl == "aez") aez_min_studies_total else 2L) &&
      sum(ab$pressure_class == reference_pressure_class) >= aez_min_sites_per_class
    if (!enough) {
      diagnostics[[i]] <- data.table(taxon_scope = taxon_scope, scope_level = lvl, scope_value = val,
                                     n_sites = nrow(ab), n_studies = uniqueN(ab$study_id),
                                     fitted = FALSE, reason = "insufficient sites/studies/reference sites")
      next
    }
    message("  fitting ", lvl, " = ", val, " (", nrow(ab), " sites, ", uniqueN(ab$study_id), " studies)")
    fit <- tryCatch(fit_scope_once(ab, pr, use_composition), error = function(e) NULL)
    if (is.null(fit) || is.null(fit$responses)) {
      diagnostics[[i]] <- data.table(taxon_scope = taxon_scope, scope_level = lvl, scope_value = val,
                                     n_sites = nrow(ab), n_studies = uniqueN(ab$study_id),
                                     fitted = FALSE, reason = "model failed")
      next
    }
    class_counts <- ab[, .(n_sites_class = .N, n_studies_class = uniqueN(study_id)), by = pressure_class]
    responses <- merge(fit$responses, class_counts, by = "pressure_class", all.x = TRUE)
    responses[, `:=`(taxon_scope = taxon_scope, scope_level = lvl, scope_value = val)]
    # Per-class sufficiency (the reference class is always kept)
    responses[, class_sufficient := pressure_class == reference_pressure_class |
                (n_sites_class >= aez_min_sites_per_class & n_studies_class >= aez_min_studies_per_class)]

    boot <- bootstrap_scope(ab, pr, use_composition, aez_n_boot, aez_boot_seed + i)
    if (!is.null(boot) && nrow(boot) > 0) {
      alpha <- (1 - aez_ci_level) / 2
      ci <- boot[, .(
        class_response_lower = stats::quantile(class_response, alpha, na.rm = TRUE),
        class_response_upper = stats::quantile(class_response, 1 - alpha, na.rm = TRUE),
        n_boot_ok = sum(is.finite(class_response))
      ), by = pressure_class]
      responses <- merge(responses, ci, by = "pressure_class", all.x = TRUE)
      fit$boot <- boot[, `:=`(taxon_scope = taxon_scope, scope_level = lvl, scope_value = val)]
    } else {
      responses[, `:=`(class_response_lower = NA_real_, class_response_upper = NA_real_, n_boot_ok = 0L)]
    }
    fitted[[i]] <- list(responses = responses, boot = fit$boot)
    diagnostics[[i]] <- cbind(
      data.table(taxon_scope = taxon_scope, scope_level = lvl, scope_value = val, fitted = TRUE, reason = ""),
      fit$diagnostics
    )
  }
  responses_all <- rbindlist(lapply(fitted, function(f) if (is.null(f)) NULL else f$responses), fill = TRUE)
  boot_all <- rbindlist(lapply(fitted, function(f) if (is.null(f)) NULL else f$boot), fill = TRUE)
  if (nrow(responses_all) == 0) stop("No scope could be fitted for taxon scope ", taxon_scope, call. = FALSE)

  # Resolve every AEZ x class to the most specific sufficient estimate.
  classes <- unique(c(reference_pressure_class, aez_from_classes, aez_to_classes,
                      unique(responses_all$pressure_class)))
  aez_values <- sort(unique(sites$aez), na.last = TRUE)
  aez_values <- aez_values[order(aez_number(aez_values))]
  resolved <- CJ(aez = aez_values, pressure_class = classes, unique = TRUE)
  resolved[, aez_group := aez_group_of(aez)]
  pick <- function(a, g, cls) {
    cand <- responses_all[pressure_class == cls & class_sufficient == TRUE &
                            ((scope_level == "aez" & scope_value == a) |
                             (scope_level == "aez_group" & scope_value == g) |
                             (scope_level == "global"))]
    if (nrow(cand) == 0) return(NULL)
    cand[, priority := match(scope_level, c("aez", "aez_group", "global"))]
    cand[order(priority)][1]
  }
  rows <- vector("list", nrow(resolved))
  for (i in seq_len(nrow(resolved))) {
    p <- pick(resolved$aez[[i]], resolved$aez_group[[i]], resolved$pressure_class[[i]])
    if (is.null(p)) next
    rows[[i]] <- data.table(
      aez = resolved$aez[[i]], aez_group = resolved$aez_group[[i]], pressure_class = resolved$pressure_class[[i]],
      class_response = p$class_response, class_response_lower = p$class_response_lower,
      class_response_upper = p$class_response_upper, abundance_relative = p$abundance_relative,
      composition_relative = p$composition_relative,
      provenance = p$scope_level, provenance_scope = p$scope_value,
      n_sites_class = p$n_sites_class, n_studies_class = p$n_studies_class
    )
  }
  class_table <- rbindlist(rows, fill = TRUE)
  class_table[, taxon_scope := taxon_scope]
  list(class_table = class_table, scope_responses = responses_all, boot = boot_all,
       diagnostics = rbindlist(diagnostics, fill = TRUE))
}

# ----------------------------------------------------------------------------
# Transition matrices
# ----------------------------------------------------------------------------
# Bootstrap draws are resolved with the same provenance as the point estimates
# so that the CI of a difference/ratio uses paired draws.
resolve_boot_draws <- function(class_table, boot) {
  if (is.null(boot) || nrow(boot) == 0) return(NULL)
  key <- unique(class_table[, .(aez, pressure_class, provenance, provenance_scope)])
  merged <- merge(
    key,
    boot[, .(scope_level, scope_value, pressure_class, boot_id, class_response)],
    by.x = c("provenance", "provenance_scope", "pressure_class"),
    by.y = c("scope_level", "scope_value", "pressure_class"),
    allow.cartesian = TRUE
  )
  merged[, .(aez, pressure_class, boot_id, class_response)]
}

build_transition_tables <- function(class_table, boot, aez_areas) {
  draws <- resolve_boot_draws(class_table, boot)
  alpha <- (1 - aez_ci_level) / 2
  transitions <- CJ(from_class = aez_from_classes, to_class = aez_to_classes, unique = TRUE)[from_class != to_class]
  out <- vector("list", 0)
  for (a in unique(class_table$aez)) {
    ct <- class_table[aez == a]
    for (j in seq_len(nrow(transitions))) {
      f <- transitions$from_class[[j]]; t <- transitions$to_class[[j]]
      bf <- ct[pressure_class == f]; bt <- ct[pressure_class == t]
      if (nrow(bf) == 0 || nrow(bt) == 0) next
      row <- data.table(
        aez = a, aez_group = bf$aez_group[[1]], from_class = f, to_class = t,
        b_from = bf$class_response, b_to = bt$class_response,
        local_change_pct = 100 * (bt$class_response / bf$class_response - 1),
        intactness_diff = bt$class_response - bf$class_response,
        provenance_from = bf$provenance, provenance_to = bt$provenance,
        n_sites_from = bf$n_sites_class, n_sites_to = bt$n_sites_class
      )
      if (!is.null(draws)) {
        d <- merge(draws[aez == a & pressure_class == f, .(boot_id, bf = class_response)],
                   draws[aez == a & pressure_class == t, .(boot_id, bt = class_response)], by = "boot_id")
        if (nrow(d) > 0) {
          row[, `:=`(
            local_change_pct_lower = 100 * stats::quantile(d$bt / d$bf - 1, alpha, na.rm = TRUE),
            local_change_pct_upper = 100 * stats::quantile(d$bt / d$bf - 1, 1 - alpha, na.rm = TRUE),
            intactness_diff_lower = stats::quantile(d$bt - d$bf, alpha, na.rm = TRUE),
            intactness_diff_upper = stats::quantile(d$bt - d$bf, 1 - alpha, na.rm = TRUE),
            n_boot_pairs = nrow(d)
          )]
        }
      }
      out[[length(out) + 1L]] <- row
    }
  }
  local <- rbindlist(out, fill = TRUE)
  if (nrow(local) == 0) stop("No transition could be evaluated.", call. = FALSE)
  matrix <- merge(local, aez_areas, by = "aez", all.x = TRUE, sort = FALSE)
  # Y % = X ha * delta_pct_per_ha  (percent of the AEZ-mean intactness)
  matrix[, `:=`(
    delta_pct_per_ha = 100 * intactness_diff / aez_area_ha,
    delta_pct_per_1000ha = 1e5 * intactness_diff / aez_area_ha
  )]
  if ("intactness_diff_lower" %in% names(matrix)) {
    matrix[, `:=`(
      delta_pct_per_1000ha_lower = 1e5 * intactness_diff_lower / aez_area_ha,
      delta_pct_per_1000ha_upper = 1e5 * intactness_diff_upper / aez_area_ha
    )]
  }
  matrix <- matrix[order(aez_number(aez), from_class, to_class)]
  list(local = local, matrix = matrix)
}

wide_matrix <- function(matrix, value_col = "delta_pct_per_1000ha") {
  m <- copy(matrix)[, transition := paste(from_class, to_class, sep = "->")]
  w <- dcast(m, taxon_scope + aez + aez_area_ha ~ transition, value.var = value_col)
  w[order(aez_number(aez))]
}

# Per EPPA region x AEZ areas (optional): same intactness differences, diluted
# by the region's own AEZ area.
region_matrix <- function(local, region_areas) {
  assert_has_cols(region_areas, c("region", "aez", "area_ha"), "EPPA region x AEZ area table")
  m <- merge(local, region_areas[, .(region, aez, region_aez_area_ha = as.numeric(area_ha))],
             by = "aez", allow.cartesian = TRUE)
  m[, `:=`(
    delta_pct_per_ha = 100 * intactness_diff / region_aez_area_ha,
    delta_pct_per_1000ha = 1e5 * intactness_diff / region_aez_area_ha
  )]
  if ("intactness_diff_lower" %in% names(m)) {
    m[, `:=`(
      delta_pct_per_1000ha_lower = 1e5 * intactness_diff_lower / region_aez_area_ha,
      delta_pct_per_1000ha_upper = 1e5 * intactness_diff_upper / region_aez_area_ha
    )]
  }
  setorder(m, region, aez, from_class, to_class)
  m
}

# Evaluate a scenario table of transitions (scenario, year, region, aez,
# from_class, to_class, area_ha) -> delta % of region x AEZ mean intactness,
# and the absolute intactness-hectares lost (area_ha * intactness_diff), which
# aggregates across AEZs/regions without needing areas.
evaluate_transitions <- function(local, transitions, region_areas = NULL) {
  assert_has_cols(transitions, c("aez", "from_class", "to_class", "area_ha"), "transition table")
  for (col in c("scenario", "year", "region")) if (!col %in% names(transitions)) transitions[, (col) := NA]
  ev <- merge(transitions, local[, .(aez, from_class, to_class, intactness_diff, local_change_pct,
                                     intactness_diff_lower = if ("intactness_diff_lower" %in% names(local)) intactness_diff_lower else NA_real_,
                                     intactness_diff_upper = if ("intactness_diff_upper" %in% names(local)) intactness_diff_upper else NA_real_)],
              by = c("aez", "from_class", "to_class"), all.x = TRUE)
  ev[, `:=`(
    intactness_ha_change = area_ha * intactness_diff,
    intactness_ha_change_lower = area_ha * intactness_diff_lower,
    intactness_ha_change_upper = area_ha * intactness_diff_upper
  )]
  if (!is.null(region_areas)) {
    ev <- merge(ev, region_areas[, .(region, aez, region_aez_area_ha = as.numeric(area_ha))],
                by = c("region", "aez"), all.x = TRUE)
    ev[, delta_pct_region_aez := 100 * intactness_ha_change / region_aez_area_ha]
  }
  ev
}

# ----------------------------------------------------------------------------
# Workflow entry point
# ----------------------------------------------------------------------------
run_aez_deforestation_matrix <- function() {
  assert_file_exists(predicts_site_path, "prepared PREDICTS site table (run 03_prepare_predicts.R first)")
  site <- as.data.table(readRDS(predicts_site_path))
  assert_has_cols(site, c("study_id", "site_id", "block_id", "pressure_class", "taxon_group",
                          "longitude", "latitude", "sqrt_relative_abundance"), "PREDICTS site table")

  aez_sf <- read_aez_polygons()
  aez_areas <- aez_area_table(aez_sf)
  write_csv_safe(aez_areas, file.path(aez_matrix_output_dir, "aez_land_area_ha.csv"))

  message("Assigning ", nrow(site), " PREDICTS sites to AEZs")
  site_aez <- assign_sites_to_aez(site, aez_sf)

  use_composition <- switch(aez_include_composition,
    "true" = TRUE, "false" = FALSE,
    file.exists(composition_pairs_path)
  )
  pairs_aez <- NULL
  if (isTRUE(use_composition)) {
    if (!file.exists(composition_pairs_path)) {
      stop("CS3_AEZ_INCLUDE_COMPOSITION=true but no cached pair table; run 04_fit_bii_models.R first.", call. = FALSE)
    }
    pairs <- as.data.table(readRDS(composition_pairs_path))
    pairs_aez <- merge(pairs, site_aez[, .(study_id, target_site_id = site_id, aez, aez_group)],
                       by = c("study_id", "target_site_id"), all.x = FALSE)
    message("Compositional-similarity component included (", nrow(pairs_aez), " pairs with AEZ).")
  } else {
    message("Abundance-only responses (set CS3_AEZ_INCLUDE_COMPOSITION=true for the full BII product).")
  }

  coverage <- site_aez[, .(n_sites = .N, n_studies = uniqueN(study_id)),
                       by = .(aez, aez_group, pressure_class, taxon_group)][order(aez_number(aez), pressure_class)]
  write_csv_safe(coverage, file.path(aez_matrix_output_dir, "aez_site_coverage.csv"))

  all_class <- list(); all_local <- list(); all_matrix <- list(); all_diag <- list(); all_scope <- list()
  for (scope in aez_taxon_scopes) {
    message("Taxon scope: ", scope)
    est <- estimate_aez_class_responses(site_aez, pairs_aez, use_composition, scope)
    if (is.null(est)) next
    tables <- build_transition_tables(est$class_table, est$boot, aez_areas)
    tables$local[, taxon_scope := scope]
    tables$matrix[, taxon_scope := scope]
    all_class[[scope]] <- est$class_table
    all_local[[scope]] <- tables$local
    all_matrix[[scope]] <- tables$matrix
    all_diag[[scope]] <- est$diagnostics
    all_scope[[scope]] <- est$scope_responses
  }
  class_table <- rbindlist(all_class, fill = TRUE)
  local <- rbindlist(all_local, fill = TRUE)
  matrix <- rbindlist(all_matrix, fill = TRUE)

  write_csv_safe(class_table, file.path(aez_matrix_output_dir, "aez_class_response.csv"))
  write_csv_safe(rbindlist(all_scope, fill = TRUE), file.path(aez_matrix_output_dir, "aez_scope_responses.csv"))
  write_csv_safe(local, file.path(aez_matrix_output_dir, "aez_transition_local_effect.csv"))
  write_csv_safe(matrix, file.path(aez_matrix_output_dir, "aez_deforestation_matrix.csv"))
  write_csv_safe(wide_matrix(matrix), file.path(aez_matrix_output_dir, "aez_deforestation_matrix_wide.csv"))
  write_csv_safe(rbindlist(all_diag, fill = TRUE), file.path(aez_matrix_output_dir, "aez_matrix_model_diagnostics.csv"))

  region_areas <- NULL
  if (file.exists(aez_region_area_path)) {
    region_areas <- as.data.table(readr::read_csv(aez_region_area_path, show_col_types = FALSE))
    write_csv_safe(region_matrix(local, region_areas),
                   file.path(aez_matrix_output_dir, "aez_region_deforestation_matrix.csv"))
  } else {
    message("No EPPA region x AEZ area table at ", aez_region_area_path, "; region matrix skipped.")
  }
  if (file.exists(aez_transitions_path)) {
    transitions <- as.data.table(readr::read_csv(aez_transitions_path, show_col_types = FALSE))
    write_csv_safe(evaluate_transitions(local, transitions, region_areas),
                   file.path(aez_matrix_output_dir, "aez_scenario_evaluation.csv"))
  }

  message("AEZ deforestation matrix written to ", aez_matrix_output_dir)
  invisible(list(class_table = class_table, local = local, matrix = matrix))
}
