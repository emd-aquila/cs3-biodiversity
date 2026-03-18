# =====================================================
# Evaluate clustering tradeoffs across methods and radii
# =====================================================

# =====================================================
# Evaluate clustering tradeoffs across methods and radii
# Run pre- and post-screen summaries for each method-radius
# combination in the configuration grid.
# =====================================================

tradeoff_results <- map_dfr(
  seq_len(nrow(run_grid)),
  function(i) {
    method <- run_grid$method[i]
    radius_m <- run_grid$radius_m[i]
    
    spec <- get_method_spec(method, radius_m)
    
    eval_params <- spec$engine_params
    eval_params$radius_m <- NULL
    
    evaluate_radius_pre_post(
      sites_tbl = sites_tbl,
      radius_m = radius_m,
      engine_fun = spec$engine_fun,
      method_name = method,
      engine_params = eval_params,
      radius_param = "radius_m",
      year_single_col = "years_single"
    )
  }
) %>%
  arrange(method, radius_km, stage)

write_csv(
  tradeoff_results,
  file.path(summary_dir, "radius_tradeoff_all_methods.csv")
)

message("Finished radius evaluation.")