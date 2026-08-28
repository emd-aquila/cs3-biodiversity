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
    hanpp_score = 0.2 * hanpphigh + 0.8 * hanpplow,
    
    ov_score =
      hq_score +
      shannon_scaled +
      phylo_scaled +
      log_msa_scaled +
      hanpp_score,
    
    ov_obs_only =
      shannon_scaled +
      log_msa_scaled +
      phylo_scaled,
    
    ov_no_HQI = ov_score - hq_score,
    ov_no_HANPP = ov_score - hanpp_score,
    ov_no_MSA = ov_score - log_msa_scaled,
    ov_no_PD = ov_score - phylo_scaled,
    ov_no_Shannon = ov_score - shannon_scaled
  ) %>%
  select(
    sample_id, Latitude, Longitude, Sample_midpoint,
    hanpp, msa, hq_score, shannon, phylo_div,
    hanpphigh, hanpplow, hanpp_score, log_msa,
    shannon_scaled, phylo_scaled, log_msa_scaled,
    ov_score, ov_obs_only,
    ov_no_HQI, ov_no_HANPP, ov_no_MSA, ov_no_PD, ov_no_Shannon
  ) %>%
  mutate(
    across(
      where(is.numeric) & !any_of(c("Latitude", "Longitude")),
      ~ round(.x, 3)
    )
  )

write_csv_safe(model_df, file.path(tmp_dir, "ov_scores_sitelevel.csv"))
