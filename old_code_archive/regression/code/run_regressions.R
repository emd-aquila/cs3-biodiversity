# =====================================================
# Run regression pipeline over all configured runs
# =====================================================

message("Starting regression pipeline...")
log_start_time <- Sys.time()

source("00_libraries.R")
source("01_config.R")
source("02_helpers.R")

total_base_runs <- length(regression_groups) *
  length(annualization_modes) *
  length(defor_bins) *
  length(delta_ov_approaches) *
  length(single_tile_collapse_modes) *
  length(ov_calculation_methods) *
  length(ov_change_modes) *
  length(starting_ov_adjustment_modes) *
  length(defor_approach_specs) *
  length(regression_models) *
  nrow(regression_run_grid)

run_counter <- 0L
message("Configured base runs: ", total_base_runs)
message("Deforestation transforms per base run: ", length(defor_transforms))
message("Regression fit plots: ", write_regression_plots)
message("Histogram plots: ", write_histogram_plots)
message("Run README: ", write_output_readme())

for (regression_group in regression_groups) {
  set_regression_group(regression_group)

  for (annualization_mode in annualization_modes) {
    set_annualization_mode(annualization_mode)

      for (defor_bin in defor_bins) {
        set_defor_bin(defor_bin)

        for (delta_ov_approach in delta_ov_approaches) {
          set_delta_ov_approach(delta_ov_approach)

          for (single_tile_collapse_mode in single_tile_collapse_modes) {
            set_single_tile_collapse_mode(single_tile_collapse_mode)

            for (ov_calculation_method in ov_calculation_methods) {
              set_ov_calculation_method(ov_calculation_method)

              for (ov_change_mode in ov_change_modes) {
                set_ov_change_mode(ov_change_mode)

                for (starting_ov_adjustment_mode in starting_ov_adjustment_modes) {
                  set_starting_ov_adjustment_mode(starting_ov_adjustment_mode)

          log_verbose("\n######################################")
          log_verbose(
            "Running regression workflow: ",
            current_regression_group,
            " / ",
            current_annualization_mode,
            " / ",
            current_defor_bin,
            " / ",
            current_delta_ov_approach,
            " / ",
            current_single_tile_collapse_mode,
            " / ",
            current_ov_calculation_method,
            " / ",
            current_ov_change_mode,
            " / ",
            current_starting_ov_adjustment_mode
          )
          log_verbose("Analysis input root: ", analysis_output_dir_current)
          log_verbose("Regression output root: ", output_dir)
          log_verbose("######################################")

          for (defor_approach in names(defor_approach_specs)) {
            set_defor_approach(defor_approach)

            for (regression_model in regression_models) {
              set_regression_model(regression_model)

              for (i in seq_len(nrow(regression_run_grid))) {
                cluster_method_i <- regression_run_grid$cluster_method[i]
                cluster_radius_km_i <- regression_run_grid$cluster_radius_km[i]
                buffer_km_i <- regression_run_grid$buffer_km[i]

                log_verbose("\n======================================")
                log_verbose("Running regression spec:")
                log_verbose("  annualization_mode: ", current_annualization_mode)
                log_verbose("  regression_group: ", current_regression_group)
                log_verbose("  group_col: ", current_group_col)
                log_verbose("  ov_calculation_method: ", current_ov_calculation_method)
                log_verbose("  ov_delta_source_col: ", current_delta_ov_source_col)
                log_verbose("  defor_bin: ", current_defor_bin)
                log_verbose("  delta_ov_approach: ", current_delta_ov_approach)
                log_verbose("  single_tile_collapse_mode: ", current_single_tile_collapse_mode)
                log_verbose("  ov_change_mode: ", current_ov_change_mode)
                log_verbose("  starting_ov_source_col: ", current_starting_ov_source_col)
                log_verbose("  starting_ov_adjustment_mode: ", current_starting_ov_adjustment_mode)
                log_verbose(
                  "  ov_threshold: ",
                  ifelse(is.na(current_ov_threshold), "none", current_ov_threshold)
                )
                log_verbose("  defor_approach: ", current_defor_approach)
                log_verbose("  defor_tile_sum: ", current_defor_tile_sum)
                log_verbose("  defor_source_col: ", current_defor_source_col)
                log_verbose("  regression_model: ", current_regression_model)
                log_verbose("  cluster_method: ", cluster_method_i)
                log_verbose("  cluster_radius_km: ", sprintf("%.1f", cluster_radius_km_i))
                log_verbose("  buffer_km: ", buffer_km_i)
                log_verbose("  defor_transforms: ", paste(defor_transforms, collapse = ", "))
                log_verbose("======================================")

                set_regression_run_paths(
                  cluster_method = cluster_method_i,
                  cluster_radius_km = cluster_radius_km_i,
                  buffer_km = buffer_km_i
                )

                run_counter <- run_counter + 1L
                message(
                  sprintf("[%03d/%03d] ", run_counter, total_base_runs),
                  paste(
                    c(
                      current_regression_group,
                      paste0("buf_", current_buffer_km, "km"),
                      current_delta_ov_approach,
                      current_ov_calculation_method,
                      current_ov_change_mode,
                      current_starting_ov_adjustment_mode,
                      current_defor_tile_sum
                    ),
                    collapse = " | "
                  )
                )

                log_verbose("Current cluster_deltas path: ", cluster_deltas_path)
                log_verbose("Sourcing 03_load_data.R")
                source("03_load_data.R")

                log_verbose("Sourcing 04_regression.R")
                source("04_regression.R")

                log_verbose("Sourcing 05_diagnostics.R")
                source("05_diagnostics.R")
              }
            }
          }
          }
          }
          }
          }
      }
    }
  }
}

if (isTRUE(build_master_output_report)) {
  message("Sourcing 06_master_output_table.R")
  source("06_master_output_table.R")
}

message("Regression pipeline complete. Elapsed: ", round(difftime(Sys.time(), log_start_time, units = "mins"), 2), " minutes")
