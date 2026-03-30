# =====================================================
# Helper functions for biodiversity site clustering
# Defines utility functions, clustering engines, wrappers,
# post-processing helpers, and export routines.
# =====================================================

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

# CLARA (Clustering LARge Applications) engine: scalable medoid approximation.
engine_clara <- function(df, radius_m = 12500, k_start = 2L, k_max = 500L,
                         samples = 5L, sampsize = NULL, seed = 1L) {
  stopifnot(all(c("x","y") %in% names(df)))
  n <- nrow(df)
  X <- as.matrix(df[, c("x","y")])
  
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
        seed = seed
      )
    ))
  }
  
  if (is.null(sampsize)) {
    sampsize <- min(n, max(40L, 10L * k_start))
  }
  
  best <- NULL
  
  for (k in seq(from = k_start, to = min(k_max, n))) {
    set.seed(seed)
    clara_res <- cluster::clara(X, k = k, samples = samples, sampsize = sampsize)
    
    med_xy <- as.matrix(clara_res$medoids)
    assigned <- assign_to_medoids(X[,1], X[,2], med_xy)
    cl <- assigned$cluster
    d  <- assigned$dist
    
    max_rad <- max(d)
    best <- list(
      cluster = as.integer(cl),
      dist_to_medoid = as.numeric(d),
      k = k,
      max_cluster_radius = max_rad,
      method = "clara_radius",
      params = list(
        radius_m = radius_m,
        k_start = k_start,
        k_max = k_max,
        samples = samples,
        sampsize = sampsize,
        seed = seed
      )
    )
    
    if (max_rad <= radius_m) break
    sampsize <- min(n, max(sampsize, 10L * k))
  }
  
  best
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
      switch_n_threshold = 1500L,
      fallback_engine_fun = engine_greedy_cover,
      fallback_params = list(
        radius_m = radius_m
      ),
      export_params = list(
        method = "PAM",
        primary_engine = "engine_pam_radius",
        fallback_engine = "engine_greedy_cover",
        switch_n_threshold = 1500L,
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
      switch_n_threshold = NULL,
      fallback_engine_fun = NULL,
      fallback_params = list(),
      export_params = list(
        method = "GREEDY",
        engine = "engine_greedy_cover",
        radius_m = radius_m
      )
    ))
  }
  
  if (method == "CLARA") {
    return(list(
      method = "CLARA",
      file_stub = "clara",
      engine_fun = engine_clara,
      engine_params = list(
        radius_m = radius_m,
        k_start  = 2L,
        k_max    = 800L,
        samples  = 5L,
        sampsize = NULL,
        seed     = 1L
      ),
      switch_n_threshold = 2000L,
      fallback_engine_fun = engine_greedy_cover,
      fallback_params = list(),
      export_params = list(
        method = "CLARA",
        primary_engine = "engine_clara",
        fallback_engine = "engine_greedy_cover",
        switch_n_threshold = 2000L,
        radius_m = radius_m
      )
    ))
  }
  
  stop("Unknown method: ", method)
}

# -----------------------
# Run one clustering configuration and export results
# -----------------------

run_one_clustering_config <- function(method, radius_m, sites_tbl, model_df_tagged, aez_order) {
  spec <- get_method_spec(method, radius_m)
  
  radius_km <- radius_m / 1000
  radius_str <- formatC(radius_km, format = "f", digits = 1)
  
  checkpoint_dir <- file.path(
    checkpoint_root,
    paste0(spec$file_stub, "_rad_", radius_str, "km")
  )
  
  out_prefix <- file.path(
    output_dir,
    paste0(spec$file_stub, "_rad_", radius_str, "km")
  )
  
  run <- cluster_within_aez(
    sites_tbl      = sites_tbl,
    engine_fun     = spec$engine_fun,
    method_tag     = spec$method,
    engine_params  = spec$engine_params,
    min_n          = 2L,
    aez_order      = aez_order,
    checkpoint_dir = checkpoint_dir,
    log_file       = "progress.log",
    fail_fast      = TRUE,
    switch_n_threshold  = spec$switch_n_threshold,
    fallback_engine_fun = spec$fallback_engine_fun,
    fallback_params     = spec$fallback_params
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
    out_prefix = out_prefix,
    model_df_clustered  = model_df_final,
    sites_tbl_clustered = sites_final,
    clusters_meta       = clusters_final,
    params = c(
      spec$export_params,
      list(
        engine_params = run$run_meta$engine_params,
        fallback_params = run$run_meta$fallback_params,
        checkpoint_dir = run$run_meta$checkpoint_dir,
        log_path = run$run_meta$log_path
      )
    )
  )
  
  message("Finished ", method, " at ", radius_str, " km.")
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

make_logger <- function(log_path) {
  force(log_path)
  dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)
  function(level = "INFO", msg) {
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    line <- sprintf("%s [%s] %s\n", ts, level, msg)
    cat(line, file = log_path, append = TRUE)
    invisible(NULL)
  }
}

# Build a stable checkpoint filepath for a given AEZ + method tag.
checkpoint_path <- function(checkpoint_dir, aez, method_tag) {
  file.path(checkpoint_dir, sprintf("aez=%s__method=%s.rds", as.character(aez), method_tag))
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
                            switch_n_threshold = NULL,
                            fallback_engine_fun = NULL,
                            fallback_params = list(),
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
        params_used = engine_params,
        fallback_used = FALSE,
        fallback_reason = NA_character_,
        n_sites = n,
        k = 0L,
        max_cluster_radius = 0
      )
    ))
  }
  
  use_engine_fun <- engine_fun
  use_params     <- engine_params
  fallback_used  <- FALSE
  fallback_reason <- NA_character_
  
  if (!is.null(switch_n_threshold) &&
      is.finite(switch_n_threshold) &&
      n > switch_n_threshold &&
      !is.null(fallback_engine_fun)) {
    use_engine_fun <- fallback_engine_fun
    use_params     <- fallback_params
    fallback_used  <- TRUE
    fallback_reason <- paste0("n_aez=", n, " > switch_n_threshold=", switch_n_threshold)
    logf("INFO", paste0("[FALLB] switching to fallback engine for AEZ n=", n,
                        " (", fallback_reason, ")"))
  }
  
  raw <- do.call(use_engine_fun, c(list(df = df_aez), use_params))
  std <- standardize_engine_output(
    raw,
    n = n,
    method = method_tag,
    params = use_params
  )
  df_aez$cluster_within_aez <- std$cluster
  df_aez$dist_to_medoid     <- std$dist_to_medoid
  
  list(
    sites = df_aez,
    engine_meta = list(
      method_tag = method_tag,
      engine_method_used = std$method,
      params_used = std$params,
      fallback_used = fallback_used,
      fallback_reason = fallback_reason,
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
                               switch_n_threshold = NULL,
                               fallback_engine_fun = NULL,
                               fallback_params = list()) {
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
  
  out_sites <- vector("list", length(aez_list))
  out_meta  <- vector("list", length(aez_list))
  names(out_sites) <- names(aez_list)
  names(out_meta)  <- names(aez_list)
  
  for (aez in names(aez_list)) {
    ckpt <- checkpoint_path(checkpoint_dir, aez, method_tag)
    if (file.exists(ckpt)) {
      logf("INFO", paste0("[SKIP ] ", aez, " (checkpoint exists: ", basename(ckpt), ")"))
      obj <- readRDS(ckpt)
      out_sites[[aez]] <- obj$sites
      out_meta[[aez]]  <- obj$engine_meta
      next
    }
    
    df_aez <- aez_list[[aez]]
    logf("INFO", paste0("[START] ", aez, " n=", nrow(df_aez)))
    
    obj <- tryCatch(
      {
        res <- cluster_one_aez(
          df_aez = df_aez,
          engine_fun = engine_fun,
          method_tag = method_tag,
          engine_params = engine_params,
          min_n = min_n,
          switch_n_threshold = switch_n_threshold,
          fallback_engine_fun = fallback_engine_fun,
          fallback_params = fallback_params,
          logf = logf
        )
        saveRDS(res, ckpt)
        logf("INFO", paste0("[DONE ] ", aez, " -> ", basename(ckpt),
                            " | engine_used=", res$engine_meta$engine_method_used,
                            if (isTRUE(res$engine_meta$fallback_used)) " (fallback)" else ""))
        res
      },
      error = function(e) {
        logf("ERROR", paste0("[FAIL ] ", aez, ": ", conditionMessage(e)))
        if (fail_fast) stop(e)
        df_aez$cluster_within_aez <- 0L
        df_aez$dist_to_medoid <- NA_real_
        list(
          sites = df_aez,
          engine_meta = list(
            method_tag = method_tag,
            engine_method_used = NA_character_,
            params_used = engine_params,
            fallback_used = FALSE,
            fallback_reason = NA_character_,
            n_sites = nrow(df_aez),
            k = NA_integer_,
            max_cluster_radius = NA_real_
          )
        )
      }
    )
    
    out_sites[[aez]] <- obj$sites
    out_meta[[aez]]  <- obj$engine_meta
  }
  
  sites_out <- dplyr::bind_rows(out_sites)
  clusters_out <- summarize_clusters(sites_out, method_tag = method_tag)
  run_meta <- list(
    method_tag = method_tag,
    engine_params = engine_params,
    checkpoint_dir = checkpoint_dir,
    log_path = log_path,
    aez_order = aez_order,
    switch_n_threshold = switch_n_threshold,
    fallback_engine = if (!is.null(fallback_engine_fun)) deparse(substitute(fallback_engine_fun)) else NULL,
    fallback_params = fallback_params
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

# Export bundle: save cluster results and metadata.
export_bundle <- function(out_prefix,
                          model_df_clustered,
                          sites_tbl_clustered,
                          clusters_meta,
                          params = list()) {
  dir.create(dirname(out_prefix), showWarnings = FALSE, recursive = TRUE)
  bundle <- list(
    model_df_clustered = model_df_clustered,
    sites_tbl_clustered = sites_tbl_clustered,
    clusters_meta = clusters_meta,
    params = params
  )
  saveRDS(bundle, paste0(out_prefix, "_bundle.rds"))
  readr::write_csv(model_df_clustered, paste0(out_prefix, "_model_df_clustered.csv"))
  readr::write_csv(sites_tbl_clustered, paste0(out_prefix, "_sites_tbl_clustered.csv"))
  readr::write_csv(clusters_meta, paste0(out_prefix, "_clusters_meta.csv"))
  invisible(bundle)
}

# -----------------------
# Additional utilities for diagnostics and summarisation
# -----------------------

# Parse radius from a filename (expects pattern like "12.5km").
parse_radius_km <- function(path) {
  x <- basename(path)
  m <- stringr::str_match(x, "([0-9]+(?:\\.[0-9]+)?)km")[, 2]
  as.numeric(m)
}

# Parse method (PAM, CLARA, GREEDY) from a filename.
parse_method <- function(path) {
  x <- tolower(basename(path))
  if (stringr::str_detect(x, "clara"))  return("CLARA")
  if (stringr::str_detect(x, "pam"))    return("PAM")
  if (stringr::str_detect(x, "greedy")) return("GREEDY")
  "UNKNOWN"
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