# Fit PREDICTS abundance and compositional-similarity models, then convert their
# predictions to relative response functions for every fitted scope.
assert_file_exists(predicts_site_path, "prepared PREDICTS site table")
assert_file_exists(predicts_composition_path, "prepared PREDICTS composition table")
site <- as.data.table(readRDS(predicts_site_path))
composition <- as.data.table(readRDS(predicts_composition_path))

if (file.exists(composition_pairs_path)) {
  message("Reading cached compositional-similarity pairs: ", composition_pairs_path)
  composition_pairs <- as.data.table(readRDS(composition_pairs_path))
} else {
  message("Constructing balanced Bray-Curtis PREDICTS site pairs.")
  composition_pairs <- build_composition_pairs(site, composition)
  saveRDS(composition_pairs, composition_pairs_path, compress = "gzip")
  message("Wrote: ", composition_pairs_path)
}

pair_summary <- composition_pairs[, .(
  n_pairs = .N,
  mean_similarity = mean(similarity),
  median_distance_km = median(geographic_distance_km)
), by = .(pressure_class, taxon_group, region)]
write_csv_safe(pair_summary, file.path(output_dir, "predicts_composition_pair_coverage.csv"))

scopes <- make_model_scopes(site, composition_pairs)
model_records <- vector("list", nrow(scopes))
for (i in seq_len(nrow(scopes))) {
  scope_row <- scopes[i]
  message("Fitting BII model: ", scope_row$model_id)
  model_records[[i]] <- tryCatch(
    fit_scope_models(site, composition_pairs, scope_row),
    error = function(error) {
      warning("Skipping ", scope_row$model_id, ": ", conditionMessage(error), call. = FALSE)
      NULL
    }
  )
}
model_records <- Filter(Negate(is.null), model_records)
if (length(model_records) == 0) {
  stop("No BII models could be fitted; inspect PREDICTS coverage outputs.", call. = FALSE)
}

responses <- rbindlist(lapply(model_records, model_response_table), fill = TRUE)
write_csv_safe(responses, response_table_path)
saveRDS(model_records, model_bundle_path, compress = "gzip")
message("Wrote: ", model_bundle_path)

model_summary <- rbindlist(lapply(model_records, function(model_record) {
  data.table(
    model_id = model_record$model_id,
    scope_type = model_record$scope_type,
    scope_value = model_record$scope_value,
    fit_type = model_record$fit_type,
    n_sites = model_record$n_sites,
    n_pairs = model_record$n_pairs
  )
}), fill = TRUE)
write_csv_safe(model_summary, file.path(output_dir, "bii_model_summary.csv"))

model_diagnostics <- rbindlist(lapply(model_records, function(model_record) {
  describe_component <- function(model, component) {
    if (!inherits(model, "merMod")) {
      return(data.table(
        model_id = model_record$model_id,
        scope_type = model_record$scope_type,
        scope_value = model_record$scope_value,
        component = component,
        fit_type = model_record$fit_type,
        singular = NA,
        convergence_messages = NA_character_
      ))
    }
    messages <- model@optinfo$conv$lme4$messages
    data.table(
      model_id = model_record$model_id,
      scope_type = model_record$scope_type,
      scope_value = model_record$scope_value,
      component = component,
      fit_type = model_record$fit_type,
      singular = lme4::isSingular(model, tol = 1e-4),
      convergence_messages = if (length(messages)) paste(messages, collapse = "; ") else NA_character_
    )
  }
  rbindlist(list(
    describe_component(model_record$abundance_model, "relative_abundance"),
    describe_component(model_record$composition_model, "compositional_similarity")
  ))
}), fill = TRUE)
write_csv_safe(model_diagnostics, file.path(output_dir, "bii_model_diagnostics.csv"))

response_plot <- ggplot(responses, aes(pressure_class, bii_class_response, fill = scope_type)) +
  geom_col(position = position_dodge()) +
  coord_flip() +
  labs(
    title = "PREDICTS BII class responses",
    subtitle = "Abundance and compositional responses relative to minimally used primary vegetation",
    x = NULL, y = "Relative BII class response", fill = "Model scope"
  ) +
  theme_minimal(base_size = 11)
ggsave(file.path(output_dir, "bii_response_functions.png"), response_plot, width = 11, height = 7, dpi = 250)
