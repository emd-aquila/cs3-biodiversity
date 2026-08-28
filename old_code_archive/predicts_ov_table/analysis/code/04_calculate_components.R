# =====================================================
# Calculate site-level OV components
# =====================================================

taxon_ab <- predicts %>%
  filter(Effort_corrected_measurement > 0) %>%
  mutate(taxon = paste(Family, Genus, sep = "_")) %>%
  group_by(sample_id, taxon) %>%
  summarise(ab = sum(Effort_corrected_measurement), .groups = "drop") %>%
  mutate(ab = round(ab, 0))

shann_index <- taxon_ab %>%
  group_by(sample_id) %>%
  mutate(p = ab / sum(ab)) %>%
  summarise(shannon = -sum(p * log(p), na.rm = TRUE), .groups = "drop")

msa <- taxon_ab %>%
  group_by(sample_id) %>%
  summarise(
    site_total_ab = sum(ab),
    site_total_spp = n_distinct(taxon),
    msa = round(site_total_ab / site_total_spp, 0),
    .groups = "drop"
  )

hqi_score <- predicts %>%
  distinct(sample_id, Biome, Predominant_land_use, Use_intensity) %>%
  filter(
    Predominant_land_use != "Cannot decide",
    Use_intensity != "Cannot decide"
  ) %>%
  mutate(
    lu_score = if_else(
      Predominant_land_use %in% c(
        "Mature secondary vegetation",
        "Secondary vegetation (indeterminate age)",
        "Intermediate secondary vegetation",
        "Primary vegetation",
        "Young secondary vegetation"
      ),
      3,
      1
    ),
    ui_score = case_when(
      Use_intensity == "Minimal use" ~ -0.1,
      Use_intensity == "Light use" ~ -0.3,
      Use_intensity == "Intense use" ~ -0.5,
      TRUE ~ NA_real_
    ),
    hq_score = lu_score + ui_score
  ) %>%
  select(sample_id, hq_score)

hanpp_score <- predicts %>%
  distinct(sample_id, Biome, Predominant_land_use, Use_intensity) %>%
  mutate(
    hanpp = case_when(
      Predominant_land_use == "Urban" ~ "high",
      Predominant_land_use %in% c(
        "Primary vegetation",
        "Mature secondary vegetation",
        "Intermediate secondary vegetation",
        "Young secondary vegetation",
        "Secondary vegetation (indeterminate age)"
      ) ~ "low",
      Predominant_land_use %in% c("Cropland", "Plantation forest", "Pasture") &
        Use_intensity == "Intense use" ~ "high",
      Predominant_land_use %in% c("Cropland", "Plantation forest", "Pasture") &
        Use_intensity %in% c("Minimal use", "Light use") ~ "low",
      Predominant_land_use == "Cannot decide" | Use_intensity == "Cannot decide" ~ NA_character_,
      TRUE ~ NA_character_
    ),
    hanpphigh = as.integer(hanpp == "high"),
    hanpplow = as.integer(hanpp == "low")
  ) %>%
  select(sample_id, hanpp, hanpphigh, hanpplow)

baseline_primary <- "Primary vegetation"
baseline_secondary <- c(
  "Young secondary vegetation",
  "Intermediate secondary vegetation",
  "Mature secondary vegetation",
  "Secondary vegetation (indeterminate age)"
)
impact_uses <- c("Cropland", "Pasture", "Plantation forest", "Urban")

biome_lookup <- predicts %>%
  filter(!is.na(sample_id), !is.na(Biome)) %>%
  count(sample_id, Biome, name = "n") %>%
  slice_max(order_by = n, n = 1, by = sample_id, with_ties = FALSE) %>%
  select(sample_id, Biome) %>%
  rename(Biome_resolved = Biome)

pdf_base <- predicts %>%
  select(sample_id, Class, Predominant_land_use, Effort_corrected_measurement) %>%
  left_join(biome_lookup, by = "sample_id") %>%
  filter(
    !is.na(sample_id),
    !is.na(Class),
    !is.na(Biome_resolved),
    Predominant_land_use != "Cannot decide",
    Effort_corrected_measurement > 0
  ) %>%
  group_by(sample_id, Class, Biome_resolved, Predominant_land_use) %>%
  summarise(total_ab = sum(Effort_corrected_measurement), .groups = "drop") %>%
  mutate(total_ab = round(total_ab, 2))

baseline_tbl <- pdf_base %>%
  mutate(
    baseline_type = case_when(
      Predominant_land_use == baseline_primary ~ "primary_b",
      Predominant_land_use %in% baseline_secondary ~ "secondary_b",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(baseline_type)) %>%
  group_by(sample_id, Class, Biome_resolved, baseline_type) %>%
  summarise(baseline = mean(total_ab, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = baseline_type, values_from = baseline)

impact_tbl <- pdf_base %>%
  filter(Predominant_land_use %in% impact_uses) %>%
  select(sample_id, Class, Biome_resolved, Predominant_land_use, total_ab) %>%
  pivot_wider(
    names_from = Predominant_land_use,
    values_from = total_ab,
    values_fill = 0
  )

eps <- 1e-7

pdf_component <- baseline_tbl %>%
  left_join(impact_tbl, by = c("sample_id", "Class", "Biome_resolved")) %>%
  filter(!(is.na(primary_b) & is.na(secondary_b))) %>%
  mutate(
    cropland_pri = 100 * (Cropland - primary_b) / (primary_b + eps),
    pasture_pri = 100 * (Pasture - primary_b) / (primary_b + eps),
    plantation_pri = 100 * (`Plantation forest` - primary_b) / (primary_b + eps),
    urban_pri = 100 * (Urban - primary_b) / (primary_b + eps),
    cropland_sec = 100 * (Cropland - secondary_b) / (secondary_b + eps),
    pasture_sec = 100 * (Pasture - secondary_b) / (secondary_b + eps),
    plantation_sec = 100 * (`Plantation forest` - secondary_b) / (secondary_b + eps),
    urban_sec = 100 * (Urban - secondary_b) / (secondary_b + eps)
  ) %>%
  mutate(
    across(
      c(
        cropland_pri, pasture_pri, plantation_pri, urban_pri,
        cropland_sec, pasture_sec, plantation_sec, urban_sec
      ),
      ~ round(.x, 0)
    )
  ) %>%
  mutate(
    mean_avg = rowMeans(
      select(
        .,
        cropland_pri, pasture_pri, plantation_pri, urban_pri,
        cropland_sec, pasture_sec, plantation_sec, urban_sec
      ),
      na.rm = TRUE
    ),
    mean_avg = ifelse(is.nan(mean_avg), 0, mean_avg)
  ) %>%
  rename(Biome = Biome_resolved) %>%
  select(
    sample_id, Class, Biome,
    primary_b, secondary_b,
    Cropland, Pasture, `Plantation forest`, Urban,
    cropland_pri, pasture_pri, plantation_pri, urban_pri,
    cropland_sec, pasture_sec, plantation_sec, urban_sec,
    mean_avg
  )

stopifnot(
  nrow(pdf_component) ==
    n_distinct(paste(pdf_component$sample_id, pdf_component$Class))
)

site_date_lookup <- predicts %>%
  distinct(sample_id, SSBS, Sample_midpoint)

coords_lookup <- predicts %>%
  filter(!is.na(SSBS), !is.na(Latitude), !is.na(Longitude)) %>%
  distinct(SSBS, Latitude, Longitude)

stopifnot(
  coords_lookup %>%
    count(SSBS) %>%
    summarise(all(n == 1)) %>%
    pull()
)

pdf_sample <- pdf_component %>%
  group_by(sample_id) %>%
  summarise(mean_pct_change = mean(mean_avg, na.rm = TRUE), .groups = "drop")

combined_component_scores <- pd %>%
  select(sample_id, phylo_div, SR) %>%
  left_join(pdf_sample, by = "sample_id") %>%
  left_join(shann_index, by = "sample_id") %>%
  left_join(msa, by = "sample_id") %>%
  left_join(hqi_score, by = "sample_id") %>%
  left_join(hanpp_score, by = "sample_id") %>%
  left_join(site_date_lookup, by = "sample_id") %>%
  left_join(coords_lookup, by = "SSBS") %>%
  filter(!is.na(hanpp), !is.na(shannon), !is.nan(shannon))

stopifnot(
  nrow(combined_component_scores) == n_distinct(combined_component_scores$sample_id)
)

write_csv_safe(
  combined_component_scores,
  file.path(tmp_dir, "combined_component_scores_sitelevel.csv")
)
