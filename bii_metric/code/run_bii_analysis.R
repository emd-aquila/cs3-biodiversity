# Run the complete PREDICTS BII workflow from any working directory.
launcher_code_dir <- Sys.getenv("CS3_BII_CODE_DIR", unset = "")
if (nzchar(launcher_code_dir)) {
  code_dir <- normalizePath(launcher_code_dir)
} else {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else NA_character_
  code_dir <- if (!is.na(script_path)) dirname(normalizePath(script_path)) else normalizePath(getwd())
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(code_dir)

source("00_libraries.R")
source("01_config.R")
source("02_helpers.R")
source("03_prepare_predicts.R")

# -----------------------------------------------------------------------------
# V2 optional branch. V1 below is retained as the stable default workflow.
# -----------------------------------------------------------------------------
if (isTRUE(bii_v2_enabled)) {
  source("12_bii_v2.R")
  fit_bii_v2_models()
  source("09_export_published_benchmarks.R")
  projection_status <- data.table(
    component = c("predicts_v2_pressure_models", "landsat_v2_projection", "eppa_v2_projection"),
    status = c("complete", "awaiting_input", "awaiting_input"),
    detail = c(
      "Fitted PREDICTS V2 models with land-use intensity and human population density; roads are an optional extension.",
      "Requires both Landsat land-use fractions with intensity mappings and input/v2/landsat_spatial_pressures.csv.",
      "Requires both EPPA land-use fractions with intensity mappings and input/v2/eppa_spatial_pressures.csv."
    )
  )
  if (file.exists(landsat_landuse_path) && file.exists(v2_landsat_pressure_path)) {
    source("05_prepare_landsat_landuse.R")
    project_bii_landuse_v2(readRDS(landsat_prepared_path), v2_landsat_pressure_path, "landsat")
    projection_status[component == "landsat_v2_projection", `:=`(
      status = "complete",
      detail = "Projected V2 BII from Landsat land-use/intensity and gridded pressure inputs."
    )]
  }
  if (file.exists(eppa_landuse_path) && file.exists(v2_eppa_pressure_path)) {
    source("08_prepare_eppa_inputs.R")
    project_bii_landuse_v2(readRDS(eppa_prepared_path), v2_eppa_pressure_path, "eppa")
    projection_status[component == "eppa_v2_projection", `:=`(
      status = "complete",
      detail = "Projected V2 BII from EPPA land-use/intensity and gridded pressure inputs."
    )]
  }
  write_csv_safe(projection_status, file.path(output_dir, "bii_v2_workflow_status.csv"))
  message("BII V2 workflow complete: ", output_dir)
} else {
source("04_fit_bii_models.R")
source("09_export_published_benchmarks.R")

# The PREDICTS response models can be fitted independently of a spatial
# projection.  Do not manufacture an area-weighted BII from the non-random
# PREDICTS sampling sites when a compatible land-use layer is absent.
projection_status <- data.table(
  component = c("predicts_response_models", "landsat_projection", "published_validation", "eppa_projection"),
  status = c("complete", "awaiting_input", "not_run", "awaiting_input"),
  detail = c(
    "Fitted terrestrial PREDICTS abundance and compositional-similarity response models.",
    "Provide bii_metric/input/landsat_landuse/landsat_landuse_long.csv before calculating area-weighted BII.",
    "Runs only after a Landsat projection with matching historical years is available.",
    "Provide bii_metric/input/eppa/eppa_landuse_long.csv before calculating EPPA BII."
  )
)

if (file.exists(landsat_landuse_path)) {
  source("05_prepare_landsat_landuse.R")
  source("06_project_bii.R")
  source("07_validate_bii.R")
  projection_status[component %in% c("landsat_projection", "published_validation"), `:=`(
    status = "complete",
    detail = c(
      "Projected area-weighted BII from the supplied Landsat land-use input.",
      "Compared matching historical global and UN-region years with the bundled NHM BII series."
    )
  )]
} else {
  message("Landsat input not found; fitted PREDICTS response models only. See bii_metric/input/landsat_landuse/README.md.")
}

if (file.exists(eppa_landuse_path)) {
  source("08_prepare_eppa_inputs.R")
  project_bii_landuse(readRDS(eppa_prepared_path), "eppa")
  projection_status[component == "eppa_projection", `:=`(
    status = "complete",
    detail = "Projected area-weighted BII from the supplied EPPA land-use input."
  )]
} else {
  message("EPPA input not found; skipped EPPA adaptation. See bii_metric/input/eppa/README.md.")
}
write_csv_safe(projection_status, file.path(output_dir, "bii_workflow_status.csv"))

message("BII workflow complete: ", output_dir)
}
