# =====================================================
# Build site-level OV scores from component scores
# =====================================================

required_objects <- c("combined_component_scores")

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    "Missing required objects for 05_build_ov_scores.R: ",
    paste(missing_objects, collapse = ", "),
    call. = FALSE
  )
}

model_df <- combined_component_scores %>%
  mutate(
    log_msa = log(msa + 1),
    shannon_scaled = minmax01(shannon),
    phylo_scaled = minmax01(phylo_div),
    log_msa_scaled = minmax01(log_msa),
    ov_score =
      hq_score +
      shannon_scaled +
      phylo_scaled +
      log_msa_scaled +
      0.2 * hanpphigh +
      0.8 * hanpplow
  ) %>%
  select(
    sample_id, Latitude, Longitude, Sample_midpoint,
    hanpp, msa, hq_score, shannon, phylo_div,
    hanpphigh, hanpplow, log_msa,
    shannon_scaled, phylo_scaled, log_msa_scaled, ov_score
  )

write_csv_safe(model_df, file.path(tmp_dir, "ov_scores_sitelevel.csv"))
