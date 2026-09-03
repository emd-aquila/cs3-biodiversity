# =============================================================================
# Figures for the AEZ x transition deforestation matrix
# =============================================================================
# Reads the CSV outputs of 22_aez_deforestation_matrix.R from
# output/aez_matrix/ and writes PNG figures next to them:
#
#   fig1_local_effect_heatmap_<scope>.png   AEZ x transition, % change per converted hectare
#   fig2_class_response_<scope>.png         intactness B[a, l] per AEZ with CI, by class
#   fig3_coverage_<scope>.png               PREDICTS studies per AEZ x class (data behind the fit)
#   fig4_aez_mean_effect_<scope>.png        % of AEZ-mean intactness per 1,000 ha
#
# Conventions: red = biodiversity loss, blue = gain, grey = no change; cells
# or points drawn from a pooled (AEZ-group / global) model are marked with an
# open symbol or "(p)"; when bootstrap CIs exist, an asterisk marks effects
# whose 95 % CI excludes zero.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

aez_fig_palette <- list(
  loss = "#d03b3b", neutral = "#f0efec", gain = "#2a78d6",
  text = "#0b0b0b", text_secondary = "#52514e", muted = "#a8a7a1",
  seq_low = "#cde2fb", seq_high = "#0d366b", surface = "#fcfcfb"
)

aez_number_fig <- function(x) as.integer(stringr::str_extract(as.character(x), "[0-9]+"))

class_short <- function(x) {
  data.table::fcase(
    x == "primary_minimal", "primary (min. use)",
    x == "primary_other", "primary (used)",
    default = as.character(x)
  )
}

transition_label <- function(from_class, to_class) {
  lab <- paste0(class_short(from_class), "\n-> ", to_class)
  order_key <- order(match(from_class, c("primary_minimal", "primary_other", "secondary")),
                     match(to_class, c("cropland", "pasture", "plantation", "urban", "secondary")))
  factor(lab, levels = unique(lab[order_key]))
}

aez_levels <- function(x) {
  u <- unique(as.character(x))
  u[order(aez_number_fig(u))]
}

theme_aez <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = aez_fig_palette$surface, colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#e6e5e1", linewidth = 0.3),
      plot.title = element_text(face = "bold", colour = aez_fig_palette$text),
      plot.subtitle = element_text(colour = aez_fig_palette$text_secondary, size = base_size - 1),
      plot.caption = element_text(colour = aez_fig_palette$text_secondary, size = base_size - 2, hjust = 0),
      axis.text = element_text(colour = aez_fig_palette$text),
      strip.text = element_text(face = "bold", colour = aez_fig_palette$text),
      legend.position = "bottom"
    )
}

read_matrix_outputs <- function(dir) {
  paths <- list(
    class = file.path(dir, "aez_class_response.csv"),
    local = file.path(dir, "aez_transition_local_effect.csv"),
    matrix = file.path(dir, "aez_deforestation_matrix.csv"),
    coverage = file.path(dir, "aez_site_coverage.csv")
  )
  missing <- names(paths)[!file.exists(unlist(paths))]
  if (length(missing) > 0) {
    stop("Missing matrix outputs in ", dir, ": ", paste(missing, collapse = ", "),
         ". Run run_aez_deforestation_matrix.R first.", call. = FALSE)
  }
  lapply(paths, function(p) as.data.table(readr::read_csv(p, show_col_types = FALSE)))
}

# --- Figure 1: local effect heatmap -----------------------------------------
plot_local_effect_heatmap <- function(local, scope, clamp = 60) {
  d <- copy(local)[taxon_scope == scope]
  if (nrow(d) == 0) return(NULL)
  d[, transition := transition_label(from_class, to_class)]
  d[, aez := factor(aez, levels = rev(aez_levels(aez)))]
  d[, pooled := provenance_from != "aez" | provenance_to != "aez"]
  has_ci <- all(c("local_change_pct_lower", "local_change_pct_upper") %in% names(d))
  d[, significant := if (has_ci) !is.na(local_change_pct_lower) &
      (local_change_pct_lower > 0 | local_change_pct_upper < 0) else NA]
  d[, fill_value := pmax(-clamp, pmin(clamp, local_change_pct))]
  d[, label := paste0(formatC(local_change_pct, format = "f", digits = 0),
                      fifelse(!is.na(significant) & significant, "*", ""),
                      fifelse(pooled, " (p)", ""))]
  d[, label_colour := fifelse(abs(fill_value) > clamp * 0.6, "#ffffff", aez_fig_palette$text)]

  caption <- paste0(
    "Values: % change in local intactness (abundance relative to minimally used primary vegetation) ",
    "when one hectare moves from the 'from' class to the 'to' class.\n",
    "(p) = at least one side of the contrast comes from a pooled (AEZ-group or global) model, not this AEZ. ",
    if (has_ci) "* = 95 % bootstrap CI excludes zero. " else "No CIs (run with CS3_AEZ_NBOOT > 0 for them). ",
    "Colour clamped at +/-", clamp, " %."
  )
  ggplot(d, aes(x = transition, y = aez, fill = fill_value)) +
    geom_tile(colour = aez_fig_palette$surface, linewidth = 1.5) +
    geom_text(aes(label = label, colour = label_colour), size = 3.1) +
    scale_colour_identity() +
    scale_fill_gradient2(
      low = aez_fig_palette$loss, mid = aez_fig_palette$neutral, high = aez_fig_palette$gain,
      midpoint = 0, limits = c(-clamp, clamp),
      name = "% change per converted hectare"
    ) +
    scale_x_discrete(position = "top") +
    labs(
      title = paste0("Local biodiversity effect of land-use transitions by AEZ (taxon scope: ", scope, ")"),
      subtitle = "Red = loss, blue = gain, grey = no change. Rows: AEZ (1-6 tropical, 7-12 temperate, 13-18 boreal).",
      x = NULL, y = NULL, caption = caption
    ) +
    theme_aez() +
    theme(axis.text.x = element_text(size = 8.5, lineheight = 0.9), legend.key.width = unit(1.6, "cm"))
}

# --- Figure 2: class responses with CI -----------------------------------------
plot_class_response <- function(class_table, scope) {
  d <- copy(class_table)[taxon_scope == scope & pressure_class != "primary_minimal"]
  if (nrow(d) == 0) return(NULL)
  d[, aez := factor(aez, levels = rev(aez_levels(aez)))]
  d[, pooled := provenance != "aez"]
  d[, pressure_class := factor(pressure_class,
                               levels = c("primary_other", "secondary", "plantation", "pasture", "cropland", "urban"))]
  has_ci <- all(c("class_response_lower", "class_response_upper") %in% names(d)) &&
    any(is.finite(d$class_response_lower))
  p <- ggplot(d, aes(x = class_response, y = aez)) +
    geom_vline(xintercept = 1, colour = aez_fig_palette$muted, linewidth = 0.5) +
    facet_wrap(~ pressure_class, nrow = 1) +
    labs(
      title = paste0("Intactness of each land-use class by AEZ (taxon scope: ", scope, ")"),
      subtitle = "B = local abundance relative to minimally used primary vegetation (= 1). Filled = estimated in this AEZ; open = pooled from AEZ group or global model.",
      x = "Intactness relative to primary-minimal", y = NULL,
      caption = if (has_ci) "Bars: 95 % study-level bootstrap CI." else "No CIs in this run (CS3_AEZ_NBOOT = 0)."
    ) +
    theme_aez()
  if (has_ci) {
    p <- p + geom_errorbar(aes(xmin = class_response_lower, xmax = class_response_upper), orientation = "y",
                            width = 0, colour = aez_fig_palette$text_secondary, linewidth = 0.5)
  }
  p + geom_point(aes(shape = pooled), size = 2.6, colour = aez_fig_palette$gain, fill = aez_fig_palette$gain) +
    scale_shape_manual(values = c(`FALSE` = 21, `TRUE` = 1), labels = c("AEZ-specific", "pooled"), name = NULL)
}

# --- Figure 3: coverage -----------------------------------------------------------
plot_coverage <- function(coverage, scope) {
  d <- copy(coverage)
  if (!identical(scope, "all")) d <- d[taxon_group == scope]
  if (nrow(d) == 0) return(NULL)
  d <- d[, .(n_studies = sum(n_studies), n_sites = sum(n_sites)), by = .(aez, pressure_class)]
  d[, aez := factor(aez, levels = rev(aez_levels(aez)))]
  d[, pressure_class := factor(pressure_class,
                               levels = c("primary_minimal", "primary_other", "secondary", "plantation", "pasture", "cropland", "urban"))]
  ggplot(d, aes(x = pressure_class, y = aez, fill = n_studies)) +
    geom_tile(colour = aez_fig_palette$surface, linewidth = 1.5) +
    geom_text(aes(label = paste0(n_studies, "\n(", n_sites, ")"),
                  colour = fifelse(n_studies > max(n_studies) * 0.55, "#ffffff", aez_fig_palette$text)), size = 2.7) +
    scale_colour_identity() +
    scale_fill_gradient(low = aez_fig_palette$seq_low, high = aez_fig_palette$seq_high, name = "PREDICTS studies", trans = "sqrt") +
    scale_x_discrete(position = "top") +
    labs(
      title = paste0("Data behind each estimate: PREDICTS studies (sites) per AEZ and land-use class (", scope, ")"),
      subtitle = "AEZ-specific estimates need >= 3 studies / 25 sites in the class and >= 8 studies / 150 sites in the AEZ (defaults).",
      x = NULL, y = NULL
    ) +
    theme_aez() + theme(legend.key.width = unit(1.6, "cm"))
}

# --- Figure 4: AEZ-mean effect per 1,000 ha ----------------------------------------
plot_aez_mean_effect <- function(matrix, scope) {
  d <- copy(matrix)[taxon_scope == scope]
  if (nrow(d) == 0) return(NULL)
  d[, transition := transition_label(from_class, to_class)]
  d[, aez := factor(aez, levels = rev(aez_levels(aez)))]
  d[, value := delta_pct_per_1000ha * 1e3] # % per million ha, more readable
  lim <- max(abs(d$value), na.rm = TRUE)
  ggplot(d, aes(x = transition, y = aez, fill = value)) +
    geom_tile(colour = aez_fig_palette$surface, linewidth = 1.5) +
    geom_text(aes(label = formatC(value, format = "f", digits = 2)), size = 2.8, colour = aez_fig_palette$text) +
    scale_fill_gradient2(low = aez_fig_palette$loss, mid = aez_fig_palette$neutral, high = aez_fig_palette$gain,
                         midpoint = 0, limits = c(-lim, lim), name = "% of AEZ-mean intactness per million ha") +
    scale_x_discrete(position = "top") +
    labs(
      title = paste0("AEZ-mean effect: % change of the AEZ's mean intactness per million ha converted (", scope, ")"),
      subtitle = "= local effect x (converted area / AEZ land area). Divide by 1,000 for the per-1,000-ha coefficient in aez_deforestation_matrix.csv.",
      x = NULL, y = NULL,
      caption = "The AEZ areas come from the AEZ shapefile; supply input/eppa/eppa_aez_area_ha.csv for EPPA's own region x AEZ areas."
    ) +
    theme_aez() + theme(axis.text.x = element_text(size = 8.5, lineheight = 0.9), legend.key.width = unit(1.6, "cm"))
}

save_fig <- function(plot, path, width, height) {
  if (is.null(plot)) return(invisible(NULL))
  ggsave(path, plot, width = width, height = height, dpi = 200, bg = aez_fig_palette$surface)
  message("Wrote: ", path)
  invisible(path)
}

plot_aez_matrix_figures <- function(dir = aez_matrix_output_dir) {
  x <- read_matrix_outputs(dir)
  scopes <- unique(x$local$taxon_scope)
  for (scope in scopes) {
    tag <- make.names(scope)
    save_fig(plot_local_effect_heatmap(x$local, scope), file.path(dir, paste0("fig1_local_effect_heatmap_", tag, ".png")), 15, 8)
    save_fig(plot_class_response(x$class, scope), file.path(dir, paste0("fig2_class_response_", tag, ".png")), 13, 6)
    save_fig(plot_coverage(x$coverage, scope), file.path(dir, paste0("fig3_coverage_", tag, ".png")), 9, 7)
    save_fig(plot_aez_mean_effect(x$matrix, scope), file.path(dir, paste0("fig4_aez_mean_effect_", tag, ".png")), 15, 8)
  }
  invisible(dir)
}
