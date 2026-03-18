# =====================================================
# Build master clustering dataset and AEZ-level OV summaries
# Read clustered full-data outputs, assemble a wide master table
# across all method-radius combinations, and calculate
# cluster- and AEZ-level OV spread metrics.
# =====================================================

# -----------------------
# Locate clustered full-data outputs
# -----------------------

clustered_paths <- list.files(
  output_dir,
  pattern = "_model_df_clustered\\.csv$",
  full.names = TRUE
)

if (length(clustered_paths) == 0) {
  stop("No clustered model files found in: ", normalizePath(output_dir, mustWork = FALSE))
}

target_radii <- sort(unique(run_grid$radius_km))
target_methods <- sort(unique(run_grid$method))

# -----------------------
# Build one cluster-id table per file
# -----------------------

cluster_cols_list <- map(
  clustered_paths,
  function(path) {
    radius_km <- parse_radius_km(path)
    method <- parse_method(path)
    
    if (is.na(radius_km) || !(radius_km %in% target_radii)) {
      return(NULL)
    }
    
    if (!(method %in% target_methods)) {
      return(NULL)
    }
    
    cluster_col <- paste0("cluster_id_", method, "_", radius_km, "km")
    
    dat <- read_csv(path, show_col_types = FALSE)
    
    out <- dat %>%
      select(sample_id, cluster_id) %>%
      rename(!!cluster_col := cluster_id)
    
    if (anyDuplicated(out$sample_id)) {
      stop("Duplicated sample_id values found in: ", basename(path))
    }
    
    out
  }
) %>%
  compact()

if (length(cluster_cols_list) == 0) {
  stop("No valid clustered files were parsed into cluster columns.")
}

expected_n <- nrow(run_grid)
if (length(cluster_cols_list) != expected_n) {
  warning(
    "Expected ", expected_n, " clustered files from run_grid, but found ",
    length(cluster_cols_list), ". Check output naming and completed runs."
  )
}

# -----------------------
# Build base table from first clustered output
# -----------------------

base <- read_csv(clustered_paths[[1]], show_col_types = FALSE) %>%
  select(
    sample_id,
    AEZ,
    Latitude,
    Longitude,
    Sample_midpoint,
    ov_score,
    Id,
    lat_r,
    lon_r,
    year
  ) %>%
  distinct(sample_id, .keep_all = TRUE)

if (anyDuplicated(base$sample_id)) {
  stop("Base table still contains duplicated sample_id values.")
}

# -----------------------
# Join all cluster-id columns onto base
# -----------------------

clusters_wide <- reduce(cluster_cols_list, left_join, by = "sample_id")

master <- base %>%
  left_join(clusters_wide, by = "sample_id")

# -----------------------
# Add globally min-max scaled OV score
# -----------------------

ov_rng <- range(master$ov_score, na.rm = TRUE)

master <- master %>%
  mutate(
    ov_minmax_global = if (ov_rng[2] == ov_rng[1]) {
      NA_real_
    } else {
      (ov_score - ov_rng[1]) / (ov_rng[2] - ov_rng[1])
    }
  )

write_csv(
  master,
  file.path(summary_dir, "predicts_method_radius_clusters.csv")
)

# -----------------------
# Reshape to long format for comparison metrics
# -----------------------

cluster_cols <- names(master)[str_detect(names(master), "^cluster_id_")]

long <- master %>%
  pivot_longer(
    cols = all_of(cluster_cols),
    names_to = "cluster_key",
    values_to = "cluster_id"
  ) %>%
  mutate(
    method = str_match(cluster_key, "^cluster_id_([^_]+)_")[, 2],
    radius_km = as.numeric(str_match(cluster_key, "_([0-9]+(?:\\.[0-9]+)?)km$")[, 2])
  ) %>%
  filter(!is.na(method), !is.na(radius_km)) %>%
  filter(!is.na(cluster_id)) %>%
  filter(!is.na(AEZ), !is.na(ov_score), !is.na(ov_minmax_global))

# -----------------------
# Calculate cluster-level OV spread metrics
# -----------------------

cluster_level_information <- long %>%
  group_by(method, radius_km, AEZ, cluster_id) %>%
  summarise(
    n_obs = n(),
    raw_min = min(ov_score, na.rm = TRUE),
    raw_max = max(ov_score, na.rm = TRUE),
    raw_range = raw_max - raw_min,
    raw_p25 = quantile(ov_score, 0.25, na.rm = TRUE),
    raw_p75 = quantile(ov_score, 0.75, na.rm = TRUE),
    raw_iqr = raw_p75 - raw_p25,
    mm_min = min(ov_minmax_global, na.rm = TRUE),
    mm_max = max(ov_minmax_global, na.rm = TRUE),
    mm_range = mm_max - mm_min,
    mm_p25 = quantile(ov_minmax_global, 0.25, na.rm = TRUE),
    mm_p75 = quantile(ov_minmax_global, 0.75, na.rm = TRUE),
    mm_iqr = mm_p75 - mm_p25,
    .groups = "drop"
  )

write_csv(
  cluster_level_information,
  file.path(summary_dir, "cluster_level_information.csv")
)

# -----------------------
# Calculate AEZ-level summaries from cluster-level metrics
# -----------------------

aez_level_information <- cluster_level_information %>%
  group_by(method, radius_km, AEZ) %>%
  summarise(
    n_clusters_in_aez = n(),
    max_ov_range_raw = max(raw_range, na.rm = TRUE),
    max_ov_range_minmax = max(mm_range, na.rm = TRUE),
    median_cluster_raw_iqr = median(raw_iqr, na.rm = TRUE),
    median_cluster_mm_iqr = median(mm_iqr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(method, radius_km, AEZ)

write_csv(
  aez_level_information,
  file.path(summary_dir, "aez_level_information.csv")
)

message("Finished master dataset and AEZ-level OV summaries.")