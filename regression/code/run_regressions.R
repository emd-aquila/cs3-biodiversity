# =====================================================
# Run regression pipeline over all configured runs
# =====================================================

message("Starting regression pipeline...")
message("Working directory: ", getwd())

message("Sourcing 00_libraries.R")
source("00_libraries.R")

message("Sourcing 01_config.R")
source("01_config.R")

message("Sourcing 02_helpers.R")
source("02_helpers.R")

for (regression_scale in regression_scales) {
  set_regression_scale(regression_scale)

  for (ov_approach in ov_approaches) {
    set_delta_ov_approach(ov_approach)

    message("\n######################################")
    message("Running regression workflow: ", current_regression_scale, " / ", current_ov_approach)
    message("Analysis input root: ", analysis_output_dir_current)
    message("Regression output root: ", file.path(output_dir, current_regression_scale, current_ov_approach))
    message("######################################")

    for (defor_approach in defor_approaches) {
      set_defor_approach(defor_approach)

      for (i in seq_len(nrow(regression_run_grid))) {
        cluster_method_i <- regression_run_grid$cluster_method[i]
        cluster_radius_km_i <- regression_run_grid$cluster_radius_km[i]
        buffer_km_i <- regression_run_grid$buffer_km[i]

        message("\n======================================")
        message("Running regression spec:")
        message("  regression_scale: ", current_regression_scale)
        message("  ov_approach: ", current_ov_approach)
        message("  defor_approach: ", current_defor_approach)
        message("  defor_source_col: ", current_defor_source_col)
        message("  cluster_method: ", cluster_method_i)
        message("  cluster_radius_km: ", sprintf("%.1f", cluster_radius_km_i))
        message("  buffer_km: ", buffer_km_i)
        message("  defor_transforms: ", paste(defor_transforms, collapse = ", "))
        message("======================================")

        set_regression_run_paths(
          cluster_method = cluster_method_i,
          cluster_radius_km = cluster_radius_km_i,
          buffer_km = buffer_km_i
        )

        message("Current cluster_deltas path: ", cluster_deltas_path)

        message("Sourcing 03_load_data.R")
        source("03_load_data.R")

        message("Sourcing 04_ols_regression.R")
        source("04_ols_regression.R")

        message("Sourcing 05_diagnostics.R")
        source("05_diagnostics.R")
      }
    }
  }
}

message("Sourcing 06_master_output_table.R")
source("06_master_output_table.R")

message("Sourcing 07_compare_canvas.R for configured regression scales and runs")
for (regression_scale in regression_scales) {
  for (i in seq_len(nrow(regression_run_grid))) {
    canvas_regression_scale_target <- regression_scale
    canvas_cluster_method_target <- regression_run_grid$cluster_method[i]
    canvas_cluster_radius_km_target <- regression_run_grid$cluster_radius_km[i]
    canvas_buffer_km_target <- regression_run_grid$buffer_km[i]

    message("\n======================================")
    message("Building comparison canvas:")
    message("  regression_scale: ", canvas_regression_scale_target)
    message("  cluster_method: ", canvas_cluster_method_target)
    message("  cluster_radius_km: ", sprintf("%.1f", canvas_cluster_radius_km_target))
    message("  buffer_km: ", canvas_buffer_km_target)
    message("======================================")

    tryCatch(
      source("07_compare_canvas.R"),
      error = function(e) {
        warning(
          "Skipping comparison canvas for ",
          canvas_regression_scale_target,
          " / ",
          canvas_cluster_method_target,
          " radius ",
          sprintf("%.1f", canvas_cluster_radius_km_target),
          " buffer ",
          canvas_buffer_km_target,
          ": ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
}

message("Regression pipeline complete.")
