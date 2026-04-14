# =====================================================
# Build an HTML comparison canvas for selected regression outputs
# =====================================================

message("Building regression comparison canvas...")
message("Working directory: ", getwd())

script_args <- commandArgs(trailingOnly = FALSE)
script_arg <- script_args[grepl("^--file=", script_args)]

script_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
} else if (file.exists("07_compare_canvas.R")) {
  normalizePath(getwd())
} else if (file.exists(file.path("regression", "code", "07_compare_canvas.R"))) {
  normalizePath(file.path(getwd(), "regression", "code"))
} else {
  stop(
    "Could not determine the regression/code directory. Run this script from ",
    "regression/code or from the repository root.",
    call. = FALSE
  )
}

setwd(script_dir)
message("Script directory: ", script_dir)

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

# ------------------------------
# User settings
# ------------------------------

cluster_method_target <- if (exists("canvas_cluster_method_target")) {
  canvas_cluster_method_target
} else {
  "clara"
}

cluster_radius_km_target <- if (exists("canvas_cluster_radius_km_target")) {
  canvas_cluster_radius_km_target
} else {
  10.0
}

buffer_km_target <- if (exists("canvas_buffer_km_target")) {
  canvas_buffer_km_target
} else {
  1
}

regression_scale_target <- if (exists("canvas_regression_scale_target")) {
  canvas_regression_scale_target
} else {
  "non_annualized"
}

ov_approaches_target <- if (exists("canvas_ov_approaches_target")) {
  canvas_ov_approaches_target
} else {
  c("ov_whole_cluster", "ov_year_pair")
}

defor_approaches_target <- if (exists("canvas_defor_approaches_target")) {
  canvas_defor_approaches_target
} else {
  c(
    "defor_tile_total_ha_total",
    "defor_tile_avg_ha_total",
    "defor_tile_rel_pct_ha_total"
  )
}

defor_transforms_target <- if (exists("canvas_defor_transforms_target")) {
  canvas_defor_transforms_target
} else {
  c("raw", "log1p", "p90")
}

# ------------------------------
# Helpers
# ------------------------------

# Copy one image into the bundle assets directory and return its path relative to index.html.
copy_bundle_asset <- function(source_path, bundle_assets_dir, asset_stub) {
  file_ext <- tools::file_ext(source_path)
  asset_filename <- paste0(asset_stub, ".", file_ext)
  target_path <- file.path(bundle_assets_dir, asset_filename)

  ok <- file.copy(source_path, target_path, overwrite = TRUE)
  if (!isTRUE(ok)) {
    stop("Failed to copy asset into bundle: ", source_path, call. = FALSE)
  }

  file.path("assets", asset_filename)
}

# Assign each AEZ to one non-overlapping, project-specific correlation bucket.
correlation_interpretation <- function(correlation_value) {
  dplyr::case_when(
    is.na(correlation_value) ~ "missing",
    correlation_value < -0.4 ~ "strong inverse (< -0.4)",
    correlation_value < -0.1 ~ "weak inverse (-0.4 to -0.1)",
    correlation_value < 0.1 ~ "inconclusive (-0.1 to 0.1)",
    TRUE ~ "correlated (> 0.1)"
  )
}

# Build one HTML note block listing all AEZs grouped by correlation category
build_aez_notes_html <- function(fit_stats_df) {
  bucket_order <- c(
    "strong inverse (< -0.4)",
    "weak inverse (-0.4 to -0.1)",
    "inconclusive (-0.1 to 0.1)",
    "correlated (> 0.1)"
  )

  note_df <- fit_stats_df %>%
    filter(model == "ols") %>%
    mutate(
      AEZ = standardize_aez_order(AEZ),
      bucket = correlation_interpretation(correlation)
    ) %>%
    arrange(AEZ)

  bucket_sections <- purrr::map_chr(
      bucket_order,
      function(bucket_name) {
        bucket_rows <- note_df %>%
        filter(bucket == bucket_name)

      if (nrow(bucket_rows) == 0) {
        return(
          paste0(
            "<div class='bucket-block'>",
            "<div class='bucket-title'>", bucket_name, "</div>",
            "<p class='empty-note'>No AEZs in this category.</p>",
            "</div>"
          )
        )
      }

      note_items <- purrr::pmap_chr(
        list(
          as.character(bucket_rows$AEZ),
          bucket_rows$correlation,
          bucket_rows$r_squared,
          bucket_rows$slope
        ),
        function(AEZ, correlation, r_squared, slope) {
          paste0(
            "<li><strong>", AEZ, "</strong>: ",
            "r = ", sprintf("%.3f", correlation),
            ", R² = ", sprintf("%.3f", r_squared),
            ", slope = ",
            formatC(signif(slope, 4), digits = 4, format = "fg", flag = "#"),
            "</li>"
          )
        }
      )

      paste0(
        "<div class='bucket-block'>",
        "<div class='bucket-title'>", bucket_name, "</div>",
        "<ul>", paste(note_items, collapse = ""), "</ul>",
        "</div>"
      )
    }
  )

  paste(bucket_sections, collapse = "")
}

# Turn a technical identifier into a friendlier section label.
display_label <- function(value) {
  dplyr::case_when(
    identical(value, "ov_whole_cluster") ~ "OV Whole Cluster",
    identical(value, "ov_year_pair") ~ "OV Year Pair",
    identical(value, "defor_tile_total_ha_total") ~ "Tile Total | ha_total",
    identical(value, "defor_tile_avg_ha_total") ~ "Tile Average | ha_total",
    identical(value, "defor_tile_rel_pct_ha_total") ~ "Tile Relative Share | ha_total",
    identical(value, "raw") ~ "Raw",
    identical(value, "log1p") ~ "log1p",
    identical(value, "p90") ~ "p90",
    TRUE ~ value
  )
}

# Build one card for a single OV approach x deforestation approach x transform combination.
build_combo_card_html <- function(ov_approach,
                                  defor_approach,
                                  defor_transform,
                                  run_root,
                                  bundle_assets_dir) {
  combo_dir <- file.path(run_root, ov_approach, defor_approach, defor_transform)
  fit_stats_path <- file.path(combo_dir, "fit_stats.csv")
  ols_png_path <- file.path(combo_dir, "ols.png")
  regressor_hist_path <- file.path(combo_dir, "hist_regressor.png")
  delta_ov_hist_path <- file.path(combo_dir, "hist_delta_ov.png")

  for (path in c(fit_stats_path, ols_png_path, regressor_hist_path, delta_ov_hist_path)) {
    if (!file.exists(path)) {
      stop("Missing required canvas input: ", path, call. = FALSE)
    }
  }

  fit_stats_df <- readr::read_csv(fit_stats_path, show_col_types = FALSE)
  asset_prefix <- paste(ov_approach, defor_approach, defor_transform, sep = "__")
  ols_img_rel <- copy_bundle_asset(ols_png_path, bundle_assets_dir, paste0(asset_prefix, "__ols"))
  reg_hist_rel <- copy_bundle_asset(regressor_hist_path, bundle_assets_dir, paste0(asset_prefix, "__hist_regressor"))
  delta_ov_hist_rel <- copy_bundle_asset(delta_ov_hist_path, bundle_assets_dir, paste0(asset_prefix, "__hist_delta_ov"))

  notes_html <- build_aez_notes_html(fit_stats_df)
  inverse_count <- fit_stats_df %>%
    filter(model == "ols", correlation < -0.01) %>%
    nrow()

  strongest_inverse <- fit_stats_df %>%
    filter(model == "ols", correlation < 0) %>%
    arrange(correlation) %>%
    slice_head(n = 1)

  strongest_inverse_html <- if (nrow(strongest_inverse) == 0) {
    "<p class='summary-line'>Best inverse signal: none.</p>"
  } else {
    paste0(
      "<p class='summary-line'>Best inverse signal: <strong>",
      strongest_inverse$AEZ[[1]],
      "</strong> (r = ",
      sprintf("%.3f", strongest_inverse$correlation[[1]]),
      ", ",
      correlation_interpretation(strongest_inverse$correlation[[1]]),
      ").</p>"
    )
  }

  paste0(
    "<div class='transform-card'>",
    "<div class='card-header'>", display_label(defor_transform), "</div>",
    "<div class='card-body'>",
    "<div class='notes-pane'>",
    "<p class='summary-line'>AEZs with inverse direction: <strong>", inverse_count, "</strong></p>",
    strongest_inverse_html,
    notes_html,
    "</div>",
    "<div class='images-pane'>",
    "<div class='main-chart'>",
    "<div class='chart-label'>AEZ Regression Fits</div>",
    "<img src='", ols_img_rel, "' alt='OLS fit chart'>",
    "</div>",
    "<div class='small-charts'>",
    "<div class='small-chart'>",
    "<div class='chart-label'>Deforestation Distribution</div>",
    "<img src='", reg_hist_rel, "' alt='Regressor histogram'>",
    "</div>",
    "<div class='small-chart'>",
    "<div class='chart-label'>Delta OV Distribution</div>",
    "<img src='", delta_ov_hist_rel, "' alt='Delta OV histogram'>",
    "</div>",
    "</div>",
    "</div>",
    "</div>",
    "</div>"
  )
}

# ------------------------------
# Paths
# ------------------------------

cluster_radius_dir <- paste0("radius_", sprintf("%.1fkm", cluster_radius_km_target))
buffer_dir <- paste0("buf_", buffer_km_target, "km")

run_root <- file.path(
  output_dir,
  cluster_method_target,
  cluster_radius_dir,
  buffer_dir,
  regression_scale_target
)

if (!dir.exists(run_root) && identical(regression_scale_target, "non_annualized")) {
  legacy_run_root <- file.path(
    output_dir,
    cluster_method_target,
    cluster_radius_dir,
    buffer_dir
  )

  if (dir.exists(legacy_run_root)) {
    warning(
      "Using legacy comparison inputs without a regression_scale folder: ",
      legacy_run_root
    )
    run_root <- legacy_run_root
  }
}

bundle_dir <- file.path(
  run_root,
  "comparison_bundle__tile_total_avg_rel__raw_log1p_p90__ha_total"
)
bundle_assets_dir <- file.path(bundle_dir, "assets")
canvas_path <- file.path(bundle_dir, "index.html")

if (dir.exists(bundle_dir)) {
  unlink(bundle_dir, recursive = TRUE, force = TRUE)
}

dir.create(bundle_assets_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------
# Build HTML
# ------------------------------

ov_sections_html <- purrr::map_chr(
  ov_approaches_target,
  function(ov_approach) {
    approach_sections <- purrr::map_chr(
      defor_approaches_target,
      function(defor_approach) {
        transform_cards <- purrr::map_chr(
          defor_transforms_target,
          ~ build_combo_card_html(
                      ov_approach = ov_approach,
                      defor_approach = defor_approach,
                      defor_transform = .x,
                      run_root = run_root,
                      bundle_assets_dir = bundle_assets_dir
                    )
        )

        paste0(
          "<section class='defor-section'>",
          "<h3>", display_label(defor_approach), "</h3>",
          "<div class='transform-grid'>",
          paste(transform_cards, collapse = ""),
          "</div>",
          "</section>"
        )
      }
    )

    paste0(
      "<section class='ov-section'>",
      "<h2>", display_label(ov_approach), "</h2>",
      paste(approach_sections, collapse = ""),
      "</section>"
    )
  }
)

html_lines <- c(
  "<!DOCTYPE html>",
  "<html lang='en'>",
  "<head>",
  "<meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width, initial-scale=1'>",
  "<title>Regression Comparison Canvas - ", regression_scale_target, "</title>",
  "<style>",
  "@page { size: 17in 11in; margin: 0.35in; }",
  "body { font-family: Georgia, 'Times New Roman', serif; margin: 0; background: #f4f1ea; color: #1e1e1b; }",
  ".page { max-width: 1800px; margin: 0 auto; padding: 20px; }",
  "h1 { margin: 0 0 6px 0; font-size: 28px; }",
  ".intro { margin: 0 0 16px 0; font-size: 13px; line-height: 1.35; max-width: 1350px; }",
  ".ov-section { margin-top: 18px; padding: 14px; background: #fbfaf7; border-top: 5px solid #275d63; box-shadow: 0 6px 18px rgba(0,0,0,0.08); }",
  ".ov-section h2 { margin: 0 0 12px 0; font-size: 23px; }",
  ".defor-section { margin-top: 12px; padding: 12px; background: #f0ece2; border-left: 5px solid #b06c49; }",
  ".defor-section h3 { margin: 0 0 10px 0; font-size: 18px; }",
  ".transform-grid { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 18px; align-items: start; }",
  ".transform-card { background: white; border: 1px solid #d8d0c2; box-shadow: 0 4px 14px rgba(0,0,0,0.06); }",
  ".card-header { padding: 9px 12px; font-size: 17px; font-weight: 700; background: #d9e4dd; border-bottom: 1px solid #c7d5cd; }",
  ".card-body { display: grid; grid-template-columns: 0.95fr 1.05fr; min-height: 500px; }",
  ".notes-pane { padding: 12px 14px; border-right: 1px solid #e5dfd3; overflow-wrap: anywhere; font-size: 12px; }",
  ".images-pane { padding: 10px; display: grid; grid-template-rows: 1.2fr 0.9fr; gap: 10px; }",
  ".main-chart, .small-chart { background: #faf8f3; border: 1px solid #dfd7ca; padding: 8px; }",
  ".small-charts { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; }",
  ".chart-label { font-size: 11px; font-weight: 700; letter-spacing: 0.03em; text-transform: uppercase; margin-bottom: 6px; color: #5d5446; }",
  "img { width: 100%; height: auto; display: block; background: white; }",
  ".summary-line { margin: 0 0 8px 0; font-size: 12px; line-height: 1.3; }",
  ".bucket-block { margin-top: 10px; padding-top: 8px; border-top: 1px solid #e8e2d7; }",
  ".bucket-title { font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.03em; color: #544a3c; }",
  ".empty-note { margin: 8px 0 0 0; font-style: italic; color: #675f53; }",
  "ul { margin: 8px 0 0 16px; padding: 0; }",
  "li { margin: 0 0 5px 0; line-height: 1.2; }",
  "@media (max-width: 1500px) { .transform-grid { grid-template-columns: 1fr; } .card-body { grid-template-columns: 1fr; } .notes-pane { border-right: none; border-bottom: 1px solid #e5dfd3; } }",
  "</style>",
  "</head>",
  "<body>",
  "<div class='page'>",
  "<h1>Regression Comparison Canvas: ", display_label(regression_scale_target), "</h1>",
  "<p class='intro'>",
  "Scope: <strong>", cluster_method_target, "</strong>, radius <strong>", sprintf("%.1f", cluster_radius_km_target), " km</strong>, buffer <strong>", buffer_km_target, " km</strong>, regression scale <strong>", regression_scale_target, "</strong>. Included outputs are <strong>ha_total only</strong>, covering <strong>tile_total</strong>, <strong>tile_avg</strong>, and <strong>tile_rel</strong>, and limited to the <strong>raw</strong>, <strong>log1p</strong>, and <strong>p90</strong> deforestation transforms. This bundle is meant to be shared as a folder: keep <code>index.html</code> and the <code>assets/</code> subfolder together.",
  "</p>",
  paste(ov_sections_html, collapse = ""),
  "</div>",
  "</body>",
  "</html>"
)

writeLines(html_lines, canvas_path)

message("Wrote comparison bundle: ", bundle_dir)
message("  index: ", canvas_path)
