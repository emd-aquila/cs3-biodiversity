# =====================================================
# Helper functions for biodiversity site clustering
# Defines utility functions, clustering engines, wrappers,
# post-processing helpers, and export routines.
# =====================================================

# -----------------------
# Cluster input preparation helpers
# -----------------------

load_cluster_dataset <- function(tagged_sites_file) {
  model_df_tagged <- readr::read_csv(tagged_sites_file, show_col_types = FALSE)

  if (!"Latitude" %in% names(model_df_tagged) && "latitude" %in% names(model_df_tagged)) {
    model_df_tagged <- model_df_tagged %>% dplyr::mutate(Latitude = latitude)
  }
  if (!"Longitude" %in% names(model_df_tagged) && "longitude" %in% names(model_df_tagged)) {
    model_df_tagged <- model_df_tagged %>% dplyr::mutate(Longitude = longitude)
  }
  if (!"Sample_midpoint" %in% names(model_df_tagged) && "sample_midpoint" %in% names(model_df_tagged)) {
    model_df_tagged <- model_df_tagged %>% dplyr::mutate(Sample_midpoint = sample_midpoint)
  }

  model_df_tagged <- model_df_tagged %>%
    dplyr::mutate(
      lat_r = round(Latitude, 4),
      lon_r = round(Longitude, 4),
      year  = as.integer(substr(Sample_midpoint, 1, 4))
    )

  unique_sites <- model_df_tagged %>%
    dplyr::distinct(AEZ, lat_r, lon_r, .keep_all = TRUE) %>%
    dplyr::arrange(AEZ)

  unique_sf <- unique_sites %>%
    sf::st_as_sf(
      coords = c("lon_r", "lat_r"),
      crs = 4326,
      remove = FALSE,
      na.fail = FALSE
    )

  sites_m <- sf::st_transform(unique_sf, 6933)
  xy <- sf::st_coordinates(sites_m)

  sites_tbl <- sites_m %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      AEZ = as.character(AEZ),
      x = xy[, 1],
      y = xy[, 2]
    )

  stopifnot(nrow(sites_tbl) == nrow(unique_sites))

  site_years <- model_df_tagged %>%
    dplyr::group_by(AEZ, lat_r, lon_r) %>%
    dplyr::summarize(
      n_years = dplyr::n_distinct(year),
      years_single = dplyr::if_else(n_years == 1L, dplyr::first(year), NA_integer_),
      .groups = "drop"
    )

  sites_tbl <- sites_tbl %>%
    dplyr::left_join(site_years, by = c("AEZ", "lat_r", "lon_r")) %>%
    dplyr::select(
      AEZ,
      lat_r,
      lon_r,
      x,
      y,
      n_years,
      years_single
    )

  list(
    model_df_tagged = model_df_tagged,
    sites_tbl = sites_tbl
  )
}

# -----------------------
# Basic distance and assignment helpers
# -----------------------

# Euclidean distance between coordinates (meters)
dist2 <- function(x1, y1, x2, y2) sqrt((x1 - x2)^2 + (y1 - y2)^2)

# Choose a medoid index from a set of candidate indices by minimizing total distance.
choose_medoid <- function(x, y, idx) {
  if (length(idx) == 1) return(idx[1])
  xx <- x[idx]; yy <- y[idx]
  dx <- outer(xx, xx, "-")
  dy <- outer(yy, yy, "-")
  D  <- sqrt(dx^2 + dy^2)
  idx[which.min(rowSums(D))]
}

# Assign each point to its nearest medoid and return cluster and distance.
assign_to_medoids <- function(x, y, medoids_xy) {
  k <- nrow(medoids_xy)
  if (k == 1) {
    d <- dist2(x, y, medoids_xy[1,1], medoids_xy[1,2])
    return(list(cluster = rep(1L, length(x)), dist = d))
  }
  D <- sapply(seq_len(k), function(j) dist2(x, y, medoids_xy[j,1], medoids_xy[j,2]))
  cl <- max.col(-D)
  d  <- D[cbind(seq_along(x), cl)]
  list(cluster = as.integer(cl), dist = as.numeric(d))
}

# -----------------------
# Clustering engine functions
# -----------------------

# Greedy cover algorithm: form clusters by iteratively selecting uncovered points as seeds.
engine_greedy_cover <- function(df, radius_m = 12500) {
  stopifnot(all(c("x","y") %in% names(df)))
  n <- nrow(df)
  x <- df$x; y <- df$y

  cluster <- integer(n)
  dist_to_medoid <- rep(NA_real_, n)

  uncovered <- rep(TRUE, n)
  k <- 0L

  for (i in seq_len(n)) {
    if (!uncovered[i]) next
    k <- k + 1L

    d <- dist2(x, y, x[i], y[i])
    members <- which(uncovered & d <= radius_m)

    med <- choose_medoid(x, y, members)
    d2 <- dist2(x, y, x[med], y[med])
    members2 <- which(uncovered & d2 <= radius_m)

    cluster[members2] <- k
    dist_to_medoid[members2] <- d2[members2]
    uncovered[members2] <- FALSE
  }

  list(
    cluster = cluster,
    dist_to_medoid = dist_to_medoid,
    k = if (k > 0) k else 0L,
    max_cluster_radius = if (k > 0) max(dist_to_medoid, na.rm = TRUE) else 0,
    method = "greedy_cover",
    params = list(radius_m = radius_m)
  )
}

# Partitioning around medoids (PAM) with a maximum radius constraint.
engine_pam_radius <- function(df, radius_m = 12500, k_start = 2L, k_max = 200L) {
  stopifnot(all(c("x","y") %in% names(df)))
  n <- nrow(df)
  x <- df$x; y <- df$y

  if (n <= 1) {
    return(list(
      cluster = if (n == 1) 1L else integer(0),
      dist_to_medoid = if (n == 1) 0 else numeric(0),
      k = if (n == 1) 1L else 0L,
      max_cluster_radius = 0,
      method = "pam_radius",
      params = list(radius_m = radius_m, k_start = k_start, k_max = k_max)
    ))
  }

  D <- as.matrix(stats::dist(cbind(x, y), method = "euclidean"))
  best <- NULL
  for (k in seq(from = k_start, to = min(k_max, n))) {
    pam_res <- cluster::pam(D, k = k, diss = TRUE)

    cl <- as.integer(pam_res$clustering)
    medoid_idx <- as.integer(pam_res$id.med)

    dist_to_medoid <- rep(NA_real_, n)
    max_rad <- 0
    for (j in seq_len(k)) {
      idx <- which(cl == j)
      med <- medoid_idx[j]
      d <- D[idx, med]
      dist_to_medoid[idx] <- d
      max_rad <- max(max_rad, max(d))
    }

    best <- list(
      cluster = cl,
      dist_to_medoid = dist_to_medoid,
      k = k,
      max_cluster_radius = max_rad,
      method = "pam_radius",
      params = list(radius_m = radius_m, k_start = k_start, k_max = k_max)
    )

    if (max_rad <= radius_m) break
  }

  best
}

# Run one CLARA fit and standardize the assignment by nearest returned medoid.
fit_clara_at_k <- function(X, k, samples, sampsize, seed) {
  set.seed(seed)
  clara_res <- cluster::clara(X, k = k, samples = samples, sampsize = sampsize)
  med_xy <- as.matrix(clara_res$medoids)
  assigned <- assign_to_medoids(X[,1], X[,2], med_xy)
  list(
    cluster = assigned$cluster,
    dist_to_medoid = assigned$dist,
    k = k,
    max_cluster_radius = max(assigned$dist)
  )
}

# CLARA (Clustering LARge Applications) engine: scalable medoid approximation.
engine_clara <- function(df, radius_m = 12500, k_start = 2L, k_max = 500L,
                         samples = 5L, sampsize = NULL, seed = 1L,
                         sample_k_multiplier = 10L,
                         k_start_strategy = "greedy_cover",
                         greedy_start_fraction = 1,
                         refine_to_min_k = FALSE) {
  stopifnot(all(c("x","y") %in% names(df)))
  n <- nrow(df)
  X <- as.matrix(df[, c("x","y")])
  requested_k_start <- max(1L, as.integer(k_start))
  greedy_seed_k <- NA_integer_

  if (n <= 1) {
    return(list(
      cluster = if (n == 1) 1L else integer(0),
      dist_to_medoid = if (n == 1) 0 else numeric(0),
      k = if (n == 1) 1L else 0L,
      max_cluster_radius = 0,
      method = "clara_radius",
      params = list(
        radius_m = radius_m,
        k_start = k_start,
        k_max = k_max,
        samples = samples,
        sampsize = sampsize,
        seed = seed,
        sample_k_multiplier = sample_k_multiplier,
        k_start_strategy = k_start_strategy,
        greedy_seed_k = greedy_seed_k,
        greedy_start_fraction = greedy_start_fraction,
        refine_to_min_k = refine_to_min_k
      )
    ))
  }

  k_start <- requested_k_start
  if (!is.finite(k_max)) {
    k_max <- n
  } else {
    k_max <- min(as.integer(k_max), n)
  }
  sample_k_multiplier <- max(1L, as.integer(sample_k_multiplier))
  greedy_start_fraction <- max(0, as.numeric(greedy_start_fraction))

  make_singleton_result <- function() {
    list(
      cluster = seq_len(n),
      dist_to_medoid = rep(0, n),
      k = n,
      max_cluster_radius = 0,
      method = "singleton_radius",
      params = list(
        radius_m = radius_m,
        k_start = k_start,
        k_max = k_max,
        samples = samples,
        sampsize = n,
        seed = seed,
        sample_k_multiplier = sample_k_multiplier,
        requested_k_start = requested_k_start,
        effective_k_start = k_start,
        k_start_strategy = k_start_strategy,
        greedy_seed_k = greedy_seed_k,
        greedy_start_fraction = greedy_start_fraction,
        refine_to_min_k = refine_to_min_k,
        search = "singleton",
        reached_radius = TRUE
      )
    )
  }

  clara_k_max <- min(k_max, n - 1L)
  if (clara_k_max < 1L || k_start >= n) return(make_singleton_result())

  if (identical(k_start_strategy, "greedy_cover") && is.finite(radius_m) && radius_m > 0) {
    greedy_seed <- engine_greedy_cover(df, radius_m = radius_m)
    greedy_seed_k <- suppressWarnings(max(greedy_seed$cluster, na.rm = TRUE))
    if (is.finite(greedy_seed_k) && greedy_seed_k > 0) {
      k_start <- max(k_start, as.integer(ceiling(greedy_seed_k * greedy_start_fraction)))
    }
  } else if (!identical(k_start_strategy, "fixed")) {
    stop("Unknown CLARA k_start_strategy: ", k_start_strategy, call. = FALSE)
  }

  if (k_start >= n) return(make_singleton_result())
  if (k_start > clara_k_max) k_start <- clara_k_max

  sampsize_for_k <- function(k) {
    if (!is.null(sampsize)) {
      return(min(n, max(as.integer(sampsize), k + 1L)))
    }
    min(n, max(40L, sample_k_multiplier * k, k + 1L))
  }

  make_result <- function(fit, reached_radius) {
    list(
      cluster = as.integer(fit$cluster),
      dist_to_medoid = as.numeric(fit$dist_to_medoid),
      k = fit$k,
      max_cluster_radius = fit$max_cluster_radius,
      method = "clara_radius",
      params = list(
        radius_m = radius_m,
        k_start = k_start,
        k_max = k_max,
        samples = samples,
        sampsize = sampsize_for_k(fit$k),
        seed = seed,
        sample_k_multiplier = sample_k_multiplier,
        requested_k_start = requested_k_start,
        effective_k_start = k_start,
        k_start_strategy = k_start_strategy,
        greedy_seed_k = greedy_seed_k,
        greedy_start_fraction = greedy_start_fraction,
        refine_to_min_k = refine_to_min_k,
        search = if (isTRUE(refine_to_min_k)) "seeded_exponential_binary" else "seeded_exponential",
        reached_radius = reached_radius
      )
    )
  }

  fit_cache <- new.env(parent = emptyenv())
  run_k <- function(k) {
    key <- as.character(k)
    if (!exists(key, envir = fit_cache, inherits = FALSE)) {
      fit_cache[[key]] <- fit_clara_at_k(
        X = X,
        k = k,
        samples = samples,
        sampsize = sampsize_for_k(k),
        seed = seed
      )
    }
    fit_cache[[key]]
  }

  best_fit <- NULL
  passing_fit <- NULL
  previous_fail <- k_start - 1L

  if (isTRUE(refine_to_min_k) && k_start > requested_k_start) {
    low_anchor_k <- min(requested_k_start, clara_k_max)
    low_anchor_fit <- run_k(low_anchor_k)
    best_fit <- low_anchor_fit
    if (low_anchor_fit$max_cluster_radius <= radius_m) {
      passing_fit <- low_anchor_fit
      previous_fail <- low_anchor_k - 1L
    } else {
      previous_fail <- low_anchor_k
    }
  }

  if (is.null(passing_fit)) {
    k <- k_start
    repeat {
      fit <- run_k(k)
      if (is.null(best_fit) || fit$max_cluster_radius < best_fit$max_cluster_radius) {
        best_fit <- fit
      }
      if (fit$max_cluster_radius <= radius_m) {
        passing_fit <- fit
        break
      }
      previous_fail <- k
      if (k >= clara_k_max) break
      k <- min(clara_k_max, max(k + 1L, k * 2L))
    }
  }

  if (!is.null(passing_fit)) {
    if (!isTRUE(refine_to_min_k)) {
      return(make_result(passing_fit, reached_radius = TRUE))
    }

    lo <- previous_fail + 1L
    hi <- passing_fit$k - 1L
    while (lo <= hi) {
      mid <- floor((lo + hi) / 2L)
      fit <- run_k(mid)
      if (is.null(best_fit) || fit$max_cluster_radius < best_fit$max_cluster_radius) {
        best_fit <- fit
      }
      if (fit$max_cluster_radius <= radius_m) {
        passing_fit <- fit
        hi <- mid - 1L
      } else {
        lo <- mid + 1L
      }
    }
    return(make_result(passing_fit, reached_radius = TRUE))
  }

  if (k_max >= n) return(make_singleton_result())
  make_result(best_fit, reached_radius = FALSE)
}

# -----------------------
# Method-specific clustering settings
# -----------------------

get_method_spec <- function(method, radius_m) {
  if (method == "PAM") {
    return(list(
      method = "PAM",
      file_stub = "pam",
      engine_fun = engine_pam_radius,
      engine_params = list(
        radius_m = radius_m,
        k_start  = 2L,
        k_max    = 200L
      ),
      export_params = list(
        method = "PAM",
        primary_engine = "engine_pam_radius",
        radius_m = radius_m
      )
    ))
  }

  if (method == "GREEDY") {
    return(list(
      method = "GREEDY",
      file_stub = "greedy_cover",
      engine_fun = engine_greedy_cover,
      engine_params = list(
        radius_m = radius_m
      ),
      export_params = list(
        method = "GREEDY",
        engine = "engine_greedy_cover",
        radius_m = radius_m
      )
    ))
  }

  if (method == "CLARA") {
    clara_k_max_cfg <- get0("clara_k_max", ifnotfound = .Machine$integer.max)
    clara_samples_cfg <- get0("clara_samples", ifnotfound = 5L)
    clara_sample_k_multiplier_cfg <- get0("clara_sample_k_multiplier", ifnotfound = 10L)
    clara_k_start_strategy_cfg <- get0("clara_k_start_strategy", ifnotfound = "greedy_cover")
    clara_greedy_start_fraction_cfg <- get0("clara_greedy_start_fraction", ifnotfound = 1)
    clara_refine_to_min_k_cfg <- get0("clara_refine_to_min_k", ifnotfound = FALSE)
    return(list(
      method = "CLARA",
      file_stub = "clara",
      engine_fun = engine_clara,
      engine_params = list(
        radius_m = radius_m,
        k_start  = 2L,
        k_max    = clara_k_max_cfg,
        samples  = clara_samples_cfg,
        sampsize = NULL,
        seed     = 1L,
        sample_k_multiplier = clara_sample_k_multiplier_cfg,
        k_start_strategy = clara_k_start_strategy_cfg,
        greedy_start_fraction = clara_greedy_start_fraction_cfg,
        refine_to_min_k = clara_refine_to_min_k_cfg
      ),
      export_params = list(
        method = "CLARA",
        primary_engine = "engine_clara",
        radius_m = radius_m
      )
    ))
  }

  stop("Unknown method: ", method)
}

# -----------------------
# Run one clustering configuration and export results
# -----------------------

build_cluster_output_dir <- function(output_root, file_stub, radius_str) {
  file.path(
    output_root,
    paste0("radius_", radius_str, "km"),
    file_stub
  )
}

cluster_progress_log_name <- function(method, radius_str) {
  paste0(method, "_", radius_str, "km_progress.log")
}

cluster_output_exists <- function(out_dir) {
  required_files <- c(
    "bundle.rds",
    "model_df_clustered.csv",
    "sites_tbl_clustered.csv",
    "clusters_meta.csv"
  )
  all(file.exists(file.path(out_dir, required_files)))
}

run_one_clustering_config <- function(method, radius_m, sites_tbl, model_df_tagged, aez_order,
                                      parallel_workers = get0("clustering_parallel_workers", ifnotfound = 1L),
                                      output_root = get0("output_dir", ifnotfound = NULL),
                                      checkpoint_root_dir = get0("checkpoint_root", ifnotfound = NULL)) {
  if (is.null(output_root)) {
    stop("Missing output_root for clustering run.", call. = FALSE)
  }
  if (is.null(checkpoint_root_dir)) {
    stop("Missing checkpoint_root_dir for clustering run.", call. = FALSE)
  }

  spec <- get_method_spec(method, radius_m)

  radius_km <- radius_m / 1000
  radius_str <- formatC(radius_km, format = "f", digits = 1)

  checkpoint_dir <- file.path(
    checkpoint_root_dir,
    paste0(spec$file_stub, "_rad_", radius_str, "km")
  )

  out_dir <- build_cluster_output_dir(
    output_root = output_root,
    file_stub = spec$file_stub,
    radius_str = radius_str
  )

  if (isTRUE(get0("skip_existing_cluster_outputs", ifnotfound = TRUE)) && cluster_output_exists(out_dir)) {
    message(
      "Skipping existing ",
      method,
      " at ",
      radius_str,
      " km: ",
      normalizePath(out_dir, mustWork = FALSE)
    )
    return(invisible(NULL))
  }

  run <- cluster_within_aez(
    sites_tbl      = sites_tbl,
    engine_fun     = spec$engine_fun,
    method_tag     = spec$method,
    engine_params  = spec$engine_params,
    min_n          = 2L,
    aez_order      = aez_order,
    checkpoint_dir = checkpoint_dir,
    log_file       = cluster_progress_log_name(method, radius_str),
    fail_fast      = TRUE,
    parallel_workers    = parallel_workers
  )

  stopifnot("dist_to_medoid" %in% names(run$sites))

  sites_final <- run$sites %>%
    enforce_year_rule() %>%
    finalize_cluster_ids(method_tag = spec$method)

  clusters_final <- summarize_clusters(
    sites_final,
    method_tag = spec$method
  )

  model_df_final <- join_clusters_back(
    model_df_tagged     = model_df_tagged,
    sites_tbl_clustered = sites_final,
    join_by             = c("AEZ", "lat_r", "lon_r")
  )

  export_bundle(
    out_dir = out_dir,
    model_df_clustered  = model_df_final,
    sites_tbl_clustered = sites_final,
    clusters_meta       = clusters_final,
    params = c(
      spec$export_params,
      list(
        engine_params = run$run_meta$engine_params,
        checkpoint_dir = run$run_meta$checkpoint_dir,
        log_path = run$run_meta$log_path
      )
    )
  )

  message("Finished ", method, " at ", radius_str, " km.")
}

build_cluster_run_context <- function(method, radius_m, output_root, checkpoint_root_dir) {
  spec <- get_method_spec(method, radius_m)
  radius_km <- radius_m / 1000
  radius_str <- formatC(radius_km, format = "f", digits = 1)
  checkpoint_dir <- file.path(
    checkpoint_root_dir,
    paste0(spec$file_stub, "_rad_", radius_str, "km")
  )
  out_dir <- build_cluster_output_dir(
    output_root = output_root,
    file_stub = spec$file_stub,
    radius_str = radius_str
  )
  list(
    spec = spec,
    method = method,
    radius_m = radius_m,
    radius_km = radius_km,
    radius_str = radius_str,
    checkpoint_dir = checkpoint_dir,
    log_path = file.path(checkpoint_dir, cluster_progress_log_name(method, radius_str)),
    out_dir = out_dir
  )
}

build_flat_aez_task_queue <- function(dataset_spec, dataset_input, run_grid) {
  sites_tbl <- dataset_input$sites_tbl
  output_root <- dataset_spec$output_dir
  checkpoint_root_dir <- dataset_spec$checkpoint_root

  aez_order <- sites_tbl %>%
    dplyr::count(AEZ, name = "n_sites") %>%
    dplyr::arrange(dplyr::desc(n_sites)) %>%
    dplyr::pull(AEZ) %>%
    as.character()

  aez_launch_order <- sites_tbl %>%
    dplyr::count(AEZ, name = "n_sites") %>%
    dplyr::arrange(n_sites) %>%
    dplyr::pull(AEZ) %>%
    as.character()

  sites_tbl <- sites_tbl %>%
    dplyr::mutate(AEZ = factor(AEZ, levels = aez_order)) %>%
    dplyr::arrange(AEZ)

  aez_list <- split(sites_tbl, sites_tbl$AEZ, drop = TRUE)
  tasks <- list()
  run_contexts <- list()

  for (i in seq_len(nrow(run_grid))) {
    context <- build_cluster_run_context(
      method = run_grid$method[i],
      radius_m = run_grid$radius_m[i],
      output_root = output_root,
      checkpoint_root_dir = checkpoint_root_dir
    )

    if (isTRUE(get0("skip_existing_cluster_outputs", ifnotfound = TRUE)) &&
        cluster_output_exists(context$out_dir)) {
      message(
        "Skipping existing ",
        dataset_spec$dataset_label,
        " ",
        context$method,
        " at ",
        context$radius_str,
        " km: ",
        normalizePath(context$out_dir, mustWork = FALSE)
      )
      next
    }

    dir.create(context$checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
    run_contexts[[length(run_contexts) + 1L]] <- context
  }

  if (length(run_contexts) == 0L) {
    return(list(tasks = tasks, aez_order = aez_order))
  }

  for (context in run_contexts) {
    for (aez in aez_launch_order) {
      df_aez <- aez_list[[aez]]
      ckpt <- checkpoint_path(context$checkpoint_dir, aez, context$spec$method)

      if (file.exists(ckpt)) {
        obj <- readRDS(ckpt)
        if (checkpoint_matches_config(
          obj = obj,
          df_aez = df_aez,
          method_tag = context$spec$method,
          engine_params = context$spec$engine_params
        )) {
          next
        }
      }

      task_key <- paste(
        dataset_spec$dataset_key,
        context$spec$file_stub,
        context$radius_str,
        aez,
        sep = "::"
      )
      tasks[[task_key]] <- list(
        dataset_key = dataset_spec$dataset_key,
        dataset_label = dataset_spec$dataset_label,
        aez = as.character(aez),
        df_aez = df_aez,
        method = context$method,
        radius_m = context$radius_m,
        radius_str = context$radius_str,
        checkpoint_path = ckpt,
        log_path = context$log_path
      )
    }
  }

  list(
    tasks = tasks,
    aez_order = aez_order
  )
}

task_report_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

format_task_radius <- function(radius_str) {
  paste0(radius_str, " km")
}

format_task_report <- function(action, task) {
  paste0(
    "[", task_report_timestamp(), "] ",
    action,
    " Task: ",
    "[", task$dataset_label, "]",
    "[", task$method, "]",
    "[", format_task_radius(task$radius_str), "]",
    "[", task$aez, "]"
  )
}

run_one_flat_aez_task <- function(task) {
  spec <- get_method_spec(task$method, task$radius_m)
  logf <- make_logger(task$log_path, echo_console = FALSE)
  dir.create(dirname(task$checkpoint_path), showWarnings = FALSE, recursive = TRUE)

  if (file.exists(task$checkpoint_path)) {
    obj <- readRDS(task$checkpoint_path)
    if (checkpoint_matches_config(
      obj = obj,
      df_aez = task$df_aez,
      method_tag = spec$method,
      engine_params = spec$engine_params
    )) {
      logf(
        "INFO",
        paste0(format_task_report("Skipping", task), " checkpoint=", basename(task$checkpoint_path))
      )
      return(invisible(task$checkpoint_path))
    }
    logf(
      "INFO",
      paste0(format_task_report("Rebuilding", task), " checkpoint=", basename(task$checkpoint_path))
    )
  }

  logf("INFO", format_task_report("Starting", task))

  res <- cluster_one_aez(
    df_aez = task$df_aez,
    engine_fun = spec$engine_fun,
    method_tag = spec$method,
    engine_params = spec$engine_params,
    min_n = 2L,
    logf = logf
  )
  saveRDS(res, task$checkpoint_path)
  logf(
    "INFO",
    paste0(format_task_report("Finished", task), " checkpoint=", basename(task$checkpoint_path))
  )
  invisible(task$checkpoint_path)
}

run_flat_aez_task_queue <- function(tasks, parallel_workers) {
  if (length(tasks) == 0L) {
    message("No missing AEZ checkpoint tasks to run.")
    return(invisible(NULL))
  }

  parallel_workers <- max(1L, min(as.integer(parallel_workers), length(tasks)))
  message("Running ", length(tasks), " flat AEZ checkpoint task(s) with ", parallel_workers, " worker(s).")

  if (parallel_workers == 1L) {
    purrr::iwalk(tasks, function(task, task_key) {
      message(format_task_report("Starting", task))
      run_one_flat_aez_task(task)
      message(format_task_report("Finished", task))
    })
    return(invisible(NULL))
  }

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multisession, workers = parallel_workers)

  pending <- names(tasks)
  active <- list()

  launch_next <- function() {
    if (length(pending) == 0L) return(invisible(NULL))
    task_key <- pending[[1]]
    pending <<- pending[-1]
    task <- tasks[[task_key]]
    message(format_task_report("Starting", task))
    active[[task_key]] <<- future::future(
      run_one_flat_aez_task(task),
      globals = c(
        "task",
        "run_one_flat_aez_task",
        "format_task_report",
        "format_task_radius",
        "task_report_timestamp",
        "get_method_spec",
        "engine_clara",
        "fit_clara_at_k",
        "engine_greedy_cover",
        "engine_pam_radius",
        "assign_to_medoids",
        "dist2",
        "choose_medoid",
        "cluster_one_aez",
        "standardize_engine_output",
        "make_logger",
        "checkpoint_matches_config",
        "clara_k_max",
        "clara_samples",
        "clara_sample_k_multiplier",
        "clara_k_start_strategy",
        "clara_greedy_start_fraction",
        "clara_refine_to_min_k"
      ),
      seed = TRUE
    )
    invisible(NULL)
  }

  for (i in seq_len(min(parallel_workers, length(pending)))) {
    launch_next()
  }

  while (length(active) > 0L) {
    resolved <- vapply(active, future::resolved, logical(1))
    if (!any(resolved)) {
      Sys.sleep(1)
      next
    }

    for (task_key in names(active)[resolved]) {
      future::value(active[[task_key]])
      message(format_task_report("Finished", tasks[[task_key]]))
      active[[task_key]] <- NULL
      launch_next()
    }
  }

  invisible(NULL)
}

run_clustering_for_dataset <- function(dataset_spec,
                                       dataset_input,
                                       run_grid,
                                       parallel_workers) {
  model_df_tagged <- dataset_input$model_df_tagged
  sites_tbl <- dataset_input$sites_tbl
  output_root <- dataset_spec$output_dir
  checkpoint_root_dir <- dataset_spec$checkpoint_root

  dir.create(output_root, showWarnings = FALSE, recursive = TRUE)
  dir.create(checkpoint_root_dir, showWarnings = FALSE, recursive = TRUE)

  queue <- build_flat_aez_task_queue(
    dataset_spec = dataset_spec,
    dataset_input = dataset_input,
    run_grid = run_grid
  )
  run_flat_aez_task_queue(queue$tasks, parallel_workers = parallel_workers)

  message("Assembling clustering outputs for ", dataset_spec$dataset_label, ".")
  for (i in seq_len(nrow(run_grid))) {
    run_one_clustering_config(
      method = run_grid$method[i],
      radius_m = run_grid$radius_m[i],
      sites_tbl = sites_tbl,
      model_df_tagged = model_df_tagged,
      aez_order = queue$aez_order,
      parallel_workers = 1L,
      output_root = output_root,
      checkpoint_root_dir = checkpoint_root_dir
    )
  }

  invisible(NULL)
}

# -----------------------
# Engine output standardization
# -----------------------

# Standardize engine output to a consistent structure.
standardize_engine_output <- function(res, n, method = NA_character_, params = list()) {
  if (is.null(res) || !is.list(res)) stop("Engine must return a list.")
  if (is.null(res$cluster)) stop("Engine output is missing `$cluster`.")
  if (length(res$cluster) != n) stop("Engine `$cluster` length (", length(res$cluster), ") != n (", n, ").")

  cluster <- res$cluster
  if (is.factor(cluster)) cluster <- as.character(cluster)
  cluster <- suppressWarnings(as.integer(cluster))
  cluster[is.na(cluster)] <- 0L
  if (any(cluster < 0L)) stop("Engine `$cluster` must be >= 0 (0 = unassigned).")

  dist_to_medoid <- res$dist_to_medoid
  if (is.null(dist_to_medoid)) {
    dist_to_medoid <- rep(NA_real_, n)
  } else {
    if (length(dist_to_medoid) != n) {
      stop("Engine `$dist_to_medoid` length (", length(dist_to_medoid), ") != n (", n, ").")
    }
    dist_to_medoid <- suppressWarnings(as.numeric(dist_to_medoid))
  }

  k <- res$k
  if (is.null(k)) {
    k <- NA_integer_
  } else {
    if (length(k) != 1L) stop("Engine `$k` must be a scalar.")
    k <- suppressWarnings(as.integer(k))
  }

  max_cluster_radius <- res$max_cluster_radius
  if (is.null(max_cluster_radius)) {
    max_cluster_radius <- NA_real_
  } else {
    if (length(max_cluster_radius) != 1L) stop("Engine `$max_cluster_radius` must be a scalar.")
    max_cluster_radius <- suppressWarnings(as.numeric(max_cluster_radius))
  }

  out_method <- res$method
  if (is.null(out_method) || is.na(out_method) || !nzchar(out_method)) {
    out_method <- method
  }
  if (is.null(out_method) || is.na(out_method) || !nzchar(out_method)) {
    out_method <- "unknown_engine"
  }

  out_params <- res$params
  if (is.null(out_params) || !is.list(out_params)) out_params <- params

  if (is.na(k)) {
    k <- length(setdiff(unique(cluster), 0L))
  }

  if (is.na(max_cluster_radius) && any(!is.na(dist_to_medoid))) {
    max_cluster_radius <- suppressWarnings(max(dist_to_medoid, na.rm = TRUE))
    if (!is.finite(max_cluster_radius)) max_cluster_radius <- NA_real_
  }

  list(
    cluster = cluster,
    dist_to_medoid = dist_to_medoid,
    k = k,
    max_cluster_radius = max_cluster_radius,
    method = out_method,
    params = out_params
  )
}

# -----------------------
# Evaluation helper
# -----------------------

# Evaluate performance of an engine before and after year rule screening.
evaluate_radius_pre_post <- function(sites_tbl,
                                     radius_m,
                                     engine_fun,
                                     method_name = "method",
                                     engine_params = list(),
                                     radius_param = "radius_m",
                                     year_single_col = "years_single") {
  stopifnot(all(c("AEZ","x","y","n_years") %in% names(sites_tbl)))
  stopifnot(year_single_col %in% names(sites_tbl))

  params <- engine_params
  params[[radius_param]] <- radius_m

  raw <- do.call(engine_fun, c(list(df = sites_tbl), params))
  std <- standardize_engine_output(raw, n = nrow(sites_tbl), method = method_name, params = params)

  tmp <- sites_tbl
  tmp$cluster_within_aez <- std$cluster
  tmp$dist_to_medoid     <- std$dist_to_medoid

  cluster_years_summary <- function(df_sites) {
    df_sites |>
      dplyr::filter(cluster_within_aez > 0L) |>
      dplyr::group_by(AEZ, cluster_within_aez) |>
      dplyr::summarise(
        n_sites = dplyr::n(),
        has_multi_year_site = any(n_years > 1, na.rm = TRUE),
        distinct_single_years = dplyr::n_distinct(df_sites[[year_single_col]][n_years == 1], na.rm = TRUE),
        est_distinct_years =
          ifelse(has_multi_year_site,
                 pmax(2, distinct_single_years),
                 distinct_single_years),
        max_dist_to_medoid = {
          m <- suppressWarnings(max(dist_to_medoid, na.rm = TRUE))
          if (is.finite(m)) m else NA_real_
        },
        .groups = "drop"
      )
  }

  pct_sites_in_2plus <- function(df_sites, cl_tbl) {
    if (nrow(cl_tbl) == 0) return(0)
    df_sites |>
      dplyr::left_join(
        cl_tbl |>
          dplyr::mutate(cluster_2plus = est_distinct_years >= 2L) |>
          dplyr::select(AEZ, cluster_within_aez, cluster_2plus),
        by = c("AEZ","cluster_within_aez")
      ) |>
      dplyr::mutate(cluster_2plus = dplyr::coalesce(cluster_2plus, FALSE)) |>
      dplyr::summarise(pct = 100 * mean(cluster_2plus)) |>
      dplyr::pull(pct)
  }

  stage_metrics <- function(df_sites, stage_label) {
    cl <- cluster_years_summary(df_sites)
    tibble::tibble(
      method = method_name,
      radius_km = radius_m / 1000,
      stage = stage_label,

      pct_clustered = 100 * mean(df_sites$cluster_within_aez > 0L),
      n_clusters = nrow(cl),
      n_aez_with_clusters = dplyr::n_distinct(df_sites$AEZ[df_sites$cluster_within_aez > 0L]),

      median_years_per_cluster = if (nrow(cl) == 0) NA_real_ else median(cl$est_distinct_years),
      pct_clusters_2plus_years = if (nrow(cl) == 0) NA_real_ else 100 * mean(cl$est_distinct_years >= 2L),

      pct_sites_in_2plus_year_clusters = pct_sites_in_2plus(df_sites, cl),

      median_cluster_size = if (nrow(cl) == 0) NA_real_ else median(cl$n_sites),
      max_radius_observed = if (nrow(cl) == 0) NA_real_ else max(cl$max_dist_to_medoid, na.rm = TRUE),

      effective_cluster_years = if (nrow(cl) == 0) NA_real_ else sum(cl$est_distinct_years, na.rm = TRUE)
    )
  }

  pre_row  <- stage_metrics(tmp, "pre_screen")
  tmp_post <- enforce_year_rule(tmp)
  post_row <- stage_metrics(tmp_post, "post_screen")
  dplyr::bind_rows(pre_row, post_row)
}

# -----------------------
# Logging helpers and checkpoint path
# -----------------------

make_logger <- function(log_path, echo_console = TRUE) {
  force(log_path)
  force(echo_console)
  dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)
  function(level = "INFO", msg) {
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    line <- sprintf("%s [%s] %s\n", ts, level, msg)
    cat(line, file = log_path, append = TRUE)
    if (isTRUE(echo_console)) message(trimws(line))
    invisible(NULL)
  }
}

# Build a stable checkpoint filepath for a given AEZ + method tag.
checkpoint_path <- function(checkpoint_dir, aez, method_tag) {
  file.path(checkpoint_dir, sprintf("%s_%s.rds", as.character(aez), method_tag))
}

checkpoint_matches_config <- function(obj,
                                      df_aez,
                                      method_tag,
                                      engine_params) {
  if (!is.list(obj) || is.null(obj$sites) || is.null(obj$engine_meta)) return(FALSE)
  if (nrow(obj$sites) != nrow(df_aez)) return(FALSE)
  if (!identical(as.character(obj$sites$AEZ), as.character(df_aez$AEZ))) return(FALSE)
  if (!identical(as.character(obj$sites$lat_r), as.character(df_aez$lat_r))) return(FALSE)
  if (!identical(as.character(obj$sites$lon_r), as.character(df_aez$lon_r))) return(FALSE)

  meta <- obj$engine_meta
  if (!identical(meta$method_tag, method_tag)) return(FALSE)

  expected_params <- engine_params
  observed_params <- if (!is.null(meta$config_params)) meta$config_params else meta$params_used
  if (is.null(observed_params) || !is.list(observed_params)) return(FALSE)

  param_names <- setdiff(names(expected_params), "sampsize")
  all(vapply(
    param_names,
    function(param_name) {
      observed <- observed_params[[param_name]]
      expected <- expected_params[[param_name]]
      if (is.null(observed)) return(FALSE)
      isTRUE(all.equal(observed, expected, check.attributes = FALSE))
    },
    logical(1)
  ))
}

# Summarize cluster-level metadata from a sites table.
summarize_clusters <- function(sites_tbl, method_tag) {
  stopifnot(all(c("AEZ", "cluster_within_aez") %in% names(sites_tbl)))
  has_dist <- "dist_to_medoid" %in% names(sites_tbl)
  clusters <- sites_tbl |>
    dplyr::filter(cluster_within_aez > 0L) |>
    dplyr::group_by(AEZ, cluster_within_aez) |>
    dplyr::summarise(
      n_sites = dplyr::n(),
      max_dist_to_medoid = if (has_dist) {
        m <- suppressWarnings(max(dist_to_medoid, na.rm = TRUE))
        if (is.finite(m)) m else NA_real_
      } else {
        NA_real_
      },
      .groups = "drop"
    ) |>
    dplyr::mutate(
      method = method_tag,
      cluster_id = paste0(AEZ, "_", method_tag, "_", cluster_within_aez)
    )
  clusters
}

# Run clustering within a single AEZ, returning standardized sites-level output and engine metadata.
cluster_one_aez <- function(df_aez,
                            engine_fun,
                            method_tag,
                            engine_params = list(),
                            min_n = 2L,
                            logf = NULL) {
  stopifnot(is.data.frame(df_aez))
  n <- nrow(df_aez)
  if (!all(c("x","y") %in% names(df_aez))) stop("df_aez must contain columns x and y.")
  if (is.null(logf)) logf <- function(level, msg) invisible(NULL)

  if (n < min_n) {
    df_aez$cluster_within_aez <- 0L
    df_aez$dist_to_medoid <- NA_real_
    return(list(
      sites = df_aez,
      engine_meta = list(
        method_tag = method_tag,
        engine_method_used = NA_character_,
        config_params = engine_params,
        params_used = engine_params,
        n_sites = n,
        k = 0L,
        max_cluster_radius = 0
      )
    ))
  }

  raw <- do.call(engine_fun, c(list(df = df_aez), engine_params))
  std <- standardize_engine_output(
    raw,
    n = n,
    method = method_tag,
    params = engine_params
  )
  df_aez$cluster_within_aez <- std$cluster
  df_aez$dist_to_medoid     <- std$dist_to_medoid
  if (!is.null(engine_params$radius_m) &&
      is.finite(engine_params$radius_m) &&
      is.finite(std$max_cluster_radius) &&
      std$max_cluster_radius > engine_params$radius_m) {
    logf(
      "WARN",
      paste0(
        "[RAD  ] max_cluster_radius=",
        round(std$max_cluster_radius, 2),
        " exceeds radius_m=",
        round(engine_params$radius_m, 2),
        " for AEZ n=",
        n
      )
    )
  }

  list(
    sites = df_aez,
    engine_meta = list(
      method_tag = method_tag,
      engine_method_used = std$method,
      config_params = engine_params,
      params_used = std$params,
      n_sites = n,
      k = std$k,
      max_cluster_radius = std$max_cluster_radius
    )
  )
}

# Main wrapper: loop over AEZs with logging and per-AEZ checkpointing.
cluster_within_aez <- function(sites_tbl,
                               engine_fun,
                               method_tag,
                               engine_params = list(),
                               min_n = 2L,
                               aez_order = NULL,
                               checkpoint_dir,
                               log_file = "progress.log",
                               fail_fast = TRUE,
                               parallel_workers = 1L) {
  stopifnot(is.data.frame(sites_tbl))
  stopifnot("AEZ" %in% names(sites_tbl))
  dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)

  log_path <- file.path(checkpoint_dir, log_file)
  logf <- make_logger(log_path)

  if (is.null(aez_order)) {
    aez_order <- unique(as.character(sites_tbl$AEZ))
  }

  sites_tbl <- sites_tbl |>
    dplyr::mutate(AEZ = factor(AEZ, levels = aez_order)) |>
    dplyr::arrange(AEZ)

  aez_list <- split(sites_tbl, sites_tbl$AEZ, drop = TRUE)

  parallel_workers <- max(1L, min(as.integer(parallel_workers), length(aez_list)))
  aez_sizes <- purrr::map_int(aez_list, nrow)
  logf(
    "INFO",
    paste0(
      "[PLAN ] ", length(aez_list), " AEZ jobs; workers=", parallel_workers,
      "; method=", method_tag
    )
  )
  purrr::iwalk(
    aez_sizes,
    function(n_sites, aez) {
      logf("INFO", paste0("[QUEUE] ", aez, " n=", n_sites))
    }
  )

  process_one_aez <- function(aez) {
    job_logf <- if (parallel_workers > 1L) {
      make_logger(log_path, echo_console = FALSE)
    } else {
      logf
    }
    ckpt <- checkpoint_path(checkpoint_dir, aez, method_tag)
    df_aez <- aez_list[[aez]]

    if (file.exists(ckpt)) {
      obj <- readRDS(ckpt)
      if (checkpoint_matches_config(
        obj = obj,
        df_aez = df_aez,
        method_tag = method_tag,
        engine_params = engine_params
      )) {
        job_logf("INFO", paste0("[SKIP ] ", aez, " (checkpoint current: ", basename(ckpt), ")"))
        return(obj)
      }
      job_logf("INFO", paste0("[STALE] ", aez, " (checkpoint will be rebuilt: ", basename(ckpt), ")"))
    }

    job_logf("INFO", paste0("[START] ", aez, " n=", nrow(df_aez)))

    tryCatch(
      {
        res <- cluster_one_aez(
          df_aez = df_aez,
          engine_fun = engine_fun,
          method_tag = method_tag,
          engine_params = engine_params,
          min_n = min_n,
          logf = job_logf
        )
        saveRDS(res, ckpt)
        job_logf("INFO", paste0("[DONE ] ", aez, " -> ", basename(ckpt),
                                " | engine_used=", res$engine_meta$engine_method_used))
        res
      },
      error = function(e) {
        job_logf("ERROR", paste0("[FAIL ] ", aez, ": ", conditionMessage(e)))
        if (fail_fast) stop(e)
        df_aez$cluster_within_aez <- 0L
        df_aez$dist_to_medoid <- NA_real_
        list(
          sites = df_aez,
          engine_meta = list(
            method_tag = method_tag,
            engine_method_used = NA_character_,
            config_params = engine_params,
            params_used = engine_params,
            n_sites = nrow(df_aez),
            k = NA_integer_,
            max_cluster_radius = NA_real_
          )
        )
      }
    )
  }

  if (parallel_workers > 1L) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = parallel_workers)
    aez_names <- names(aez_list)
    out <- vector("list", length(aez_names))
    names(out) <- aez_names
    pending <- aez_names
    active <- list()

    launch_next <- function() {
      if (length(pending) == 0L) return(invisible(NULL))
      aez <- pending[[1]]
      pending <<- pending[-1]
      logf("INFO", paste0("[START] ", aez, " n=", aez_sizes[[aez]], " (main)"))
      active[[aez]] <<- future::future(
        process_one_aez(aez),
        seed = TRUE
      )
      invisible(NULL)
    }

    for (i in seq_len(min(parallel_workers, length(pending)))) {
      launch_next()
    }

    while (length(active) > 0L) {
      resolved <- vapply(active, future::resolved, logical(1))
      if (!any(resolved)) {
        Sys.sleep(1)
        next
      }

      for (aez in names(active)[resolved]) {
        out[[aez]] <- future::value(active[[aez]])
        logf("INFO", paste0("[DONE ] ", aez, " (main)"))
        active[[aez]] <- NULL
        launch_next()
      }
    }
  } else {
    out <- purrr::map(names(aez_list), process_one_aez)
    names(out) <- names(aez_list)
  }

  out_sites <- purrr::map(out, "sites")
  out_meta  <- purrr::map(out, "engine_meta")

  sites_out <- dplyr::bind_rows(out_sites)
  clusters_out <- summarize_clusters(sites_out, method_tag = method_tag)
  run_meta <- list(
    method_tag = method_tag,
    engine_params = engine_params,
    checkpoint_dir = checkpoint_dir,
    log_path = log_path,
    aez_order = aez_order,
    parallel_workers = parallel_workers
  )
  list(
    sites = sites_out,
    clusters = clusters_out,
    engine_meta_by_aez = out_meta,
    run_meta = run_meta
  )
}

# -----------------------
# Post-processing helpers
# -----------------------

# Enforce year-rule on cluster assignments. Invalid clusters are set to 0.
enforce_year_rule <- function(sites_tbl) {
  req <- c("AEZ", "cluster_within_aez", "n_years", "years_single")
  missing <- setdiff(req, names(sites_tbl))
  if (length(missing) > 0) stop("sites_tbl missing required columns: ", paste(missing, collapse = ", "))
  validity <- sites_tbl |>
    dplyr::filter(cluster_within_aez > 0L) |>
    dplyr::group_by(AEZ, cluster_within_aez) |>
    dplyr::summarise(
      has_multi_year_site = any(n_years > 1, na.rm = TRUE),
      n_distinct_single_years = dplyr::n_distinct(years_single[n_years == 1], na.rm = TRUE),
      year_rule_ok = has_multi_year_site | (n_distinct_single_years >= 2L),
      .groups = "drop"
    ) |>
    dplyr::select(AEZ, cluster_within_aez, year_rule_ok)
  out <- sites_tbl |>
    dplyr::left_join(validity, by = c("AEZ","cluster_within_aez")) |>
    dplyr::mutate(
      year_rule_ok = dplyr::if_else(cluster_within_aez == 0L, TRUE, dplyr::coalesce(year_rule_ok, FALSE)),
      cluster_within_aez = dplyr::if_else(year_rule_ok, cluster_within_aez, 0L)
    )
  out
}

# Assign final cluster identifiers and missing flag.
finalize_cluster_ids <- function(sites_tbl, method_tag) {
  stopifnot(all(c("AEZ","cluster_within_aez") %in% names(sites_tbl)))
  sites_tbl |>
    dplyr::mutate(
      method = method_tag,
      cluster_id = dplyr::if_else(
        cluster_within_aez > 0L,
        paste0(AEZ, "_", method_tag, "_", cluster_within_aez),
        NA_character_
      ),
      cluster_missing = (cluster_within_aez == 0L)
    )
}

# Join cluster labels back onto the full time-series dataset.
join_clusters_back <- function(model_df_tagged,
                               sites_tbl_clustered,
                               join_by = c("AEZ","lat_r","lon_r")) {
  stopifnot(is.data.frame(model_df_tagged), is.data.frame(sites_tbl_clustered))
  stopifnot(all(join_by %in% names(model_df_tagged)))
  stopifnot(all(join_by %in% names(sites_tbl_clustered)))
  keep_cols <- c(join_by, "cluster_within_aez", "cluster_id", "method", "cluster_missing")
  if ("dist_to_medoid" %in% names(sites_tbl_clustered)) keep_cols <- c(keep_cols, "dist_to_medoid")
  if ("year_rule_ok" %in% names(sites_tbl_clustered))  keep_cols <- c(keep_cols, "year_rule_ok")
  rhs <- sites_tbl_clustered |>
    dplyr::select(dplyr::all_of(unique(keep_cols)))
  model_df_tagged |>
    dplyr::left_join(rhs, by = join_by) |>
    dplyr::mutate(cluster_missing = dplyr::coalesce(cluster_missing, TRUE))
}

# Export one clustering run as a bundle plus companion csv files.
export_bundle <- function(out_dir,
                          model_df_clustered,
                          sites_tbl_clustered,
                          clusters_meta,
                          params = list()) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  bundle <- list(
    model_df_clustered = model_df_clustered,
    sites_tbl_clustered = sites_tbl_clustered,
    clusters_meta = clusters_meta,
    params = params
  )
  saveRDS(bundle, file.path(out_dir, "bundle.rds"))
  readr::write_csv(model_df_clustered, file.path(out_dir, "model_df_clustered.csv"))
  readr::write_csv(sites_tbl_clustered, file.path(out_dir, "sites_tbl_clustered.csv"))
  readr::write_csv(clusters_meta, file.path(out_dir, "clusters_meta.csv"))
  invisible(bundle)
}

# -----------------------
# Additional utilities for diagnostics and summarisation
# -----------------------

# Parse radius from a filename (expects pattern like "12.5km").
parse_radius_km <- function(path) {
  x <- paste(strsplit(normalizePath(path, mustWork = FALSE), .Platform$file.sep, fixed = TRUE)[[1]], collapse = "/")
  m <- stringr::str_match(x, "([0-9]+(?:\\.[0-9]+)?)km")[, 2]
  as.numeric(m)
}

# Parse method (PAM, CLARA, GREEDY) from a saved output path.
parse_method <- function(path) {
  x <- tolower(paste(strsplit(normalizePath(path, mustWork = FALSE), .Platform$file.sep, fixed = TRUE)[[1]], collapse = "/"))
  if (stringr::str_detect(x, "clara"))  return("CLARA")
  if (stringr::str_detect(x, "pam"))    return("PAM")
  if (stringr::str_detect(x, "greedy")) return("GREEDY")
  "UNKNOWN"
}

path_matches_cluster_grid <- function(path, target_radii, target_methods) {
  radius_km <- parse_radius_km(path)
  method <- parse_method(path)
  radius_ok <- !is.na(radius_km) && (is.null(target_radii) || radius_km %in% target_radii)
  method_ok <- method != "UNKNOWN" && (is.null(target_methods) || method %in% target_methods)
  radius_ok && method_ok
}

summarize_bundle_path <- function(path, target_radii, target_methods) {
  if (!path_matches_cluster_grid(path, target_radii, target_methods)) {
    return(NULL)
  }
  summarize_bundle(
    bundle = readRDS(path),
    method = parse_method(path),
    radius_km = parse_radius_km(path),
    year_single_col = "years_single"
  )
}

clusters_by_aez_path <- function(path, target_radii, target_methods) {
  if (!path_matches_cluster_grid(path, target_radii, target_methods)) {
    return(NULL)
  }
  clusters_by_aez_one_bundle(
    bundle = readRDS(path),
    method = parse_method(path),
    radius_km = parse_radius_km(path)
  )
}

cluster_column_from_path <- function(path, target_radii, target_methods) {
  if (!path_matches_cluster_grid(path, target_radii, target_methods)) {
    return(NULL)
  }

  radius_km <- parse_radius_km(path)
  method <- parse_method(path)
  cluster_col <- paste0("cluster_id_", method, "_", radius_km, "km")

  out <- readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::select(sample_id, cluster_id) %>%
    dplyr::rename(!!cluster_col := cluster_id)

  if (anyDuplicated(out$sample_id)) {
    stop("Duplicated sample_id values found in: ", basename(path), call. = FALSE)
  }

  out
}

first_clustered_path_for_grid <- function(clustered_paths, target_radii, target_methods) {
  matching_paths <- clustered_paths[
    vapply(clustered_paths, path_matches_cluster_grid, logical(1), target_radii, target_methods)
  ]
  if (length(matching_paths) == 0) {
    stop("No clustered model files matched the available method-radius outputs.", call. = FALSE)
  }
  matching_paths[[1]]
}

aez_sort_key <- function(aez) {
  readr::parse_number(as.character(aez))
}

# Summarise cluster year information.
cluster_years_summary <- function(sites, year_single_col = "years_single") {
  stopifnot(all(c("AEZ", "cluster_within_aez", "n_years") %in% names(sites)))
  stopifnot(year_single_col %in% names(sites))
  sites |>
    dplyr::filter(cluster_within_aez > 0L) |>
    dplyr::group_by(AEZ, cluster_within_aez) |>
    dplyr::summarise(
      n_sites = dplyr::n(),
      has_multi_year_site = any(n_years > 1, na.rm = TRUE),
      distinct_single_years = dplyr::n_distinct(sites[[year_single_col]][n_years == 1], na.rm = TRUE),
      est_distinct_years = ifelse(has_multi_year_site, pmax(2, distinct_single_years), distinct_single_years),
      max_dist_to_medoid = if ("dist_to_medoid" %in% names(sites)) {
        m <- suppressWarnings(max(dist_to_medoid, na.rm = TRUE))
        if (is.finite(m)) m else NA_real_
      } else NA_real_,
      .groups = "drop"
    )
}

# Summarise a clustering bundle into a single row of metrics.
summarize_bundle <- function(bundle, method, radius_km, year_single_col = "years_single") {
  sites <- bundle$sites_tbl_clustered
  stopifnot(!is.null(sites))
  if (!("cluster_missing" %in% names(sites))) {
    sites <- sites |>
      dplyr::mutate(cluster_missing = cluster_within_aez == 0L | is.na(cluster_within_aez))
  }
  n_total_sites <- nrow(sites)
  n_clustered_sites <- sum(sites$cluster_within_aez > 0L, na.rm = TRUE)
  cl <- cluster_years_summary(sites, year_single_col = year_single_col)
  tibble::tibble(
    method = method,
    radius_km = radius_km,
    n_sites_total = n_total_sites,
    pct_sites_clustered = 100 * n_clustered_sites / n_total_sites,
    n_clusters = nrow(cl),
    n_clusters_2plus_years = sum(cl$est_distinct_years >= 2, na.rm = TRUE),
    pct_clusters_2plus_years = if (nrow(cl) == 0) NA_real_ else 100 * mean(cl$est_distinct_years >= 2, na.rm = TRUE),
    n_aez_with_clusters = dplyr::n_distinct(sites$AEZ[sites$cluster_within_aez > 0L]),
    effective_cluster_years = if (nrow(cl) == 0) 0 else sum(cl$est_distinct_years, na.rm = TRUE),
    median_cluster_size = if (nrow(cl) == 0) NA_real_ else median(cl$n_sites, na.rm = TRUE),
    p90_cluster_size = if (nrow(cl) == 0) NA_real_ else as.numeric(quantile(cl$n_sites, 0.9, na.rm = TRUE)),
    max_cluster_size = if (nrow(cl) == 0) NA_real_ else max(cl$n_sites, na.rm = TRUE),
    max_radius_observed_km = if (nrow(cl) == 0) NA_real_ else max(cl$max_dist_to_medoid, na.rm = TRUE) / 1000
  )
}

# Compute number of clusters per AEZ for one bundle.
clusters_by_aez_one_bundle <- function(bundle, method, radius_km) {
  sites <- bundle$sites_tbl_clustered
  stopifnot(!is.null(sites))
  stopifnot(all(c("AEZ","cluster_within_aez") %in% names(sites)))
  all_aez <- sort(unique(sites$AEZ))
  counts <- sites |>
    dplyr::filter(cluster_within_aez > 0L) |>
    dplyr::mutate(cluster_key = paste(AEZ, cluster_within_aez, sep = "::")) |>
    dplyr::group_by(AEZ) |>
    dplyr::summarise(n_clusters = dplyr::n_distinct(cluster_key), .groups = "drop")
  tibble::tibble(AEZ = all_aez) |>
    dplyr::left_join(counts, by = "AEZ") |>
    dplyr::mutate(
      n_clusters = tidyr::replace_na(n_clusters, 0L),
      method = method,
      radius_km = radius_km
    ) |>
    dplyr::select(method, radius_km, AEZ, n_clusters)
}
