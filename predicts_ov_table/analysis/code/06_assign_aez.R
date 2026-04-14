# =====================================================
# Assign AEZ labels to site-level OV scores
# =====================================================

required_objects <- c("model_df")

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    "Missing required objects for 06_assign_aez.R: ",
    paste(missing_objects, collapse = ", "),
    call. = FALSE
  )
}

sf_use_s2(FALSE)

aez <- read_sf(aez_path) %>%
  st_make_valid() %>%
  select(Id, AEZ)

sites_sf <- model_df %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(aez))

tagged <- st_join(sites_sf, aez, join = st_within, left = TRUE) %>%
  st_drop_geometry()

missing_idx <- which(is.na(tagged$AEZ))

if (length(missing_idx) > 0) {
  nearest_idx <- st_nearest_feature(sites_sf[missing_idx, ], aez)
  tagged$AEZ[missing_idx] <- aez$AEZ[nearest_idx]
}

tagged <- tagged %>%
  mutate(AEZ_assigned_by_nearest = row_number() %in% missing_idx) %>%
  relocate(AEZ, .after = sample_id) %>%
  relocate(AEZ_assigned_by_nearest, .after = AEZ)

write_csv_safe(tagged, file.path(output_dir, "ov_AEZ_tag.csv"))
