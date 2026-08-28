# Prepare terrestrial PREDICTS abundance records for the two BII components.
assert_file_exists(predicts_raw_path, "raw PREDICTS extract")
message("Reading raw PREDICTS data: ", predicts_raw_path)
predicts_raw <- readRDS(predicts_raw_path)
prepared_predicts <- prepare_predicts_records(predicts_raw)

saveRDS(prepared_predicts$site, predicts_site_path, compress = "gzip")
saveRDS(prepared_predicts$composition, predicts_composition_path, compress = "gzip")
message("Wrote: ", predicts_site_path)
message("Wrote: ", predicts_composition_path)

coverage <- prepared_predicts$site[, .(
  n_sites = .N,
  n_studies = uniqueN(study_id),
  mean_relative_abundance = mean(relative_abundance)
), by = .(pressure_class, land_use, intensity, taxon_group, region)]
write_csv_safe(coverage, file.path(output_dir, "predicts_bii_coverage.csv"))

summary <- data.table(
  metric = c("raw_rows", "terrestrial_abundance_rows", "sites", "studies", "pressure_classes", "taxon_groups", "regions"),
  value = c(
    nrow(predicts_raw),
    nrow(prepared_predicts$composition),
    nrow(prepared_predicts$site),
    uniqueN(prepared_predicts$site$study_id),
    uniqueN(prepared_predicts$site$pressure_class),
    uniqueN(prepared_predicts$site$taxon_group),
    uniqueN(prepared_predicts$site$region)
  )
)
write_csv_safe(summary, file.path(output_dir, "predicts_bii_input_summary.csv"))
