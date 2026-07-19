# =============================================================================
# 08_model_comparison.R
# Behavioural Segmentation Toolkit
#
# Purpose:
#   - Load candidate feature sets and retained distance objects
#   - Compare PAM, hierarchical clustering and k-means
#   - Evaluate candidate values of k across all feature sets
#   - Calculate silhouette, cluster balance and variance-separation metrics
#   - Rank candidate models for downstream stability validation
#
# Important:
#   This script does NOT declare the final segmentation model.
#   The highest-ranked candidates must still be assessed for bootstrap
#   stability, interpretability and profile distinctness.
#
# Expected inputs:
#   data/processed/06_candidate_feature_sets.rds
#   outputs/models/07_distance_objects.rds
#
# Main outputs:
#   outputs/tables/08_model_quality.csv
#   outputs/tables/08_ranked_model_candidates.csv
#   outputs/tables/08_best_model_by_method.csv
#   outputs/tables/08_best_model_by_feature_set.csv
#   outputs/tables/08_model_comparison_summary.csv
#   outputs/models/08_cluster_models.rds
#   outputs/models/08_cluster_assignments.rds
#   outputs/figures/08_model_silhouette_comparison.png
#   outputs/figures/08_top_model_candidates.png
#   outputs/figures/08_cluster_balance_comparison.png
#   outputs/logs/08_model_comparison.log
# =============================================================================


# =============================================================================
# 0. Environment
# =============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  warn = 1,
  scipen = 999
)


# =============================================================================
# 1. Packages
# =============================================================================

required_packages <- c(
  "dplyr",
  "readr",
  "tidyr",
  "purrr",
  "tibble",
  "ggplot2",
  "cluster"
)

missing_packages <- setdiff(
  required_packages,
  rownames(installed.packages())
)

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    dependencies = TRUE
  )
}

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)


# =============================================================================
# 2. Settings
# =============================================================================

identifier_variable <- "respondent_id"

k_values <- 2:8

retained_distance_methods <- c(
  "manhattan",
  "gower"
)

hierarchical_linkage <- "average"

kmeans_nstart <- 100
kmeans_iter_max <- 200

random_seed <- 123

# Preliminary composite score weights.
# Stability and interpretability are intentionally excluded at this stage.
weight_silhouette <- 0.45
weight_balance <- 0.20
weight_minimum_cluster_share <- 0.15
weight_between_ss_ratio <- 0.20

minimum_acceptable_cluster_share <- 0.05

number_top_models_to_plot <- 20


# =============================================================================
# 3. Helper functions
# =============================================================================

find_project_root <- function(start_dir = getwd()) {

  current <- normalizePath(
    start_dir,
    winslash = "/",
    mustWork = TRUE
  )

  for (i in seq_len(10)) {

    if (
      dir.exists(file.path(current, "data")) &&
      dir.exists(file.path(current, "outputs"))
    ) {
      return(current)
    }

    parent <- dirname(current)

    if (identical(parent, current)) {
      break
    }

    current <- parent
  }

  stop(
    "Project root not found. ",
    "The project must contain data/ and outputs/ folders."
  )
}


create_directory <- function(path) {

  if (!dir.exists(path)) {
    dir.create(
      path,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  invisible(path)
}


write_log <- function(..., log_file) {

  text <- paste0(...)

  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " | ",
    text,
    "\n",
    file = log_file,
    append = TRUE,
    sep = ""
  )

  message(text)
}


normalise_01 <- function(x) {

  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }

  minimum <- min(x, na.rm = TRUE)
  maximum <- max(x, na.rm = TRUE)

  if (
    !is.finite(minimum) ||
    !is.finite(maximum) ||
    maximum == minimum
  ) {
    return(rep(1, length(x)))
  }

  (x - minimum) / (maximum - minimum)
}


standardise_numeric_data <- function(data) {

  scaled <- scale(data)

  if (any(!is.finite(scaled))) {
    stop(
      "Non-finite values were produced during standardisation."
    )
  }

  as.matrix(scaled)
}


cluster_size_metrics <- function(cluster_assignment) {

  cluster_sizes <- table(
    cluster_assignment
  )

  cluster_shares <- as.numeric(
    cluster_sizes
  ) / sum(cluster_sizes)

  entropy <- -sum(
    cluster_shares *
      log(cluster_shares)
  )

  normalised_entropy <- ifelse(
    length(cluster_shares) <= 1,
    0,
    entropy / log(length(cluster_shares))
  )

  tibble::tibble(
    minimum_cluster_size =
      min(cluster_sizes),
    maximum_cluster_size =
      max(cluster_sizes),
    minimum_cluster_share =
      min(cluster_shares),
    maximum_cluster_share =
      max(cluster_shares),
    cluster_size_ratio = ifelse(
      min(cluster_sizes) == 0,
      NA_real_,
      max(cluster_sizes) /
        min(cluster_sizes)
    ),
    balance_score = 1 -
      (
        max(cluster_shares) -
          min(cluster_shares)
      ),
    cluster_size_entropy =
      normalised_entropy
  )
}


variance_separation_metrics <- function(
    scaled_data,
    cluster_assignment
) {

  total_mean <- colMeans(
    scaled_data
  )

  total_ss <- sum(
    sweep(
      scaled_data,
      2,
      total_mean,
      "-"
    )^2
  )

  unique_clusters <- sort(
    unique(cluster_assignment)
  )

  within_ss <- 0

  for (cluster_id in unique_clusters) {

    cluster_data <- scaled_data[
      cluster_assignment == cluster_id,
      ,
      drop = FALSE
    ]

    cluster_mean <- colMeans(
      cluster_data
    )

    within_ss <- within_ss +
      sum(
        sweep(
          cluster_data,
          2,
          cluster_mean,
          "-"
        )^2
      )
  }

  between_ss <- total_ss -
    within_ss

  n_observations <- nrow(
    scaled_data
  )

  k <- length(
    unique_clusters
  )

  calinski_harabasz <- ifelse(
    k > 1 &&
      n_observations > k &&
      within_ss > 0,
    (
      between_ss /
        (k - 1)
    ) /
      (
        within_ss /
          (n_observations - k)
      ),
    NA_real_
  )

  tibble::tibble(
    total_ss = total_ss,
    within_ss = within_ss,
    between_ss = between_ss,
    between_ss_ratio = ifelse(
      total_ss == 0,
      NA_real_,
      between_ss / total_ss
    ),
    calinski_harabasz =
      calinski_harabasz
  )
}


evaluate_assignment <- function(
    cluster_assignment,
    silhouette_distance,
    scaled_data,
    feature_set,
    feature_count,
    method,
    distance_method,
    k,
    model_id,
    elapsed_seconds
) {

  silhouette_result <- cluster::silhouette(
    cluster_assignment,
    silhouette_distance
  )

  size_metrics <- cluster_size_metrics(
    cluster_assignment
  )

  separation_metrics <-
    variance_separation_metrics(
      scaled_data,
      cluster_assignment
    )

  dplyr::bind_cols(
    tibble::tibble(
      model_id = model_id,
      feature_set = feature_set,
      feature_count = feature_count,
      method = method,
      distance_method =
        distance_method,
      k = k,
      average_silhouette = mean(
        silhouette_result[
          ,
          "sil_width"
        ]
      ),
      minimum_silhouette = min(
        silhouette_result[
          ,
          "sil_width"
        ]
      ),
      negative_silhouette_share = mean(
        silhouette_result[
          ,
          "sil_width"
        ] < 0
      ),
      elapsed_seconds =
        elapsed_seconds
    ),
    size_metrics,
    separation_metrics
  )
}


fit_pam_model <- function(
    distance_object,
    k
) {

  cluster::pam(
    x = distance_object,
    k = k,
    diss = TRUE,
    cluster.only = FALSE
  )
}


fit_hierarchical_model <- function(
    distance_object,
    k,
    linkage
) {

  tree <- stats::hclust(
    distance_object,
    method = linkage
  )

  assignment <- stats::cutree(
    tree,
    k = k
  )

  list(
    tree = tree,
    assignment = assignment
  )
}


fit_kmeans_model <- function(
    scaled_data,
    k,
    seed
) {

  set.seed(seed)

  stats::kmeans(
    x = scaled_data,
    centers = k,
    nstart = kmeans_nstart,
    iter.max = kmeans_iter_max
  )
}


# =============================================================================
# 4. Paths
# =============================================================================

project_dir <- find_project_root()

data_processed_dir <- file.path(
  project_dir,
  "data",
  "processed"
)

tables_dir <- file.path(
  project_dir,
  "outputs",
  "tables"
)

figures_dir <- file.path(
  project_dir,
  "outputs",
  "figures"
)

models_dir <- file.path(
  project_dir,
  "outputs",
  "models"
)

logs_dir <- file.path(
  project_dir,
  "outputs",
  "logs"
)

invisible(
  lapply(
    c(
      tables_dir,
      figures_dir,
      models_dir,
      logs_dir
    ),
    create_directory
  )
)

feature_sets_file <- file.path(
  data_processed_dir,
  "06_candidate_feature_sets.rds"
)

distance_objects_file <- file.path(
  models_dir,
  "07_distance_objects.rds"
)

log_file <- file.path(
  logs_dir,
  "08_model_comparison.log"
)

if (file.exists(log_file)) {
  file.remove(log_file)
}


# =============================================================================
# 5. Load inputs
# =============================================================================

if (!file.exists(feature_sets_file)) {
  stop(
    "Candidate feature sets not found: ",
    feature_sets_file,
    "\nRun 06_feature_selection.R first."
  )
}

if (!file.exists(distance_objects_file)) {
  stop(
    "Distance objects not found: ",
    distance_objects_file,
    "\nRun 07_distance_comparison.R first."
  )
}

candidate_feature_sets <- readRDS(
  feature_sets_file
)

distance_objects <- readRDS(
  distance_objects_file
)

if (!is.list(candidate_feature_sets)) {
  stop(
    "06_candidate_feature_sets.rds must contain a named list."
  )
}

if (!is.list(distance_objects)) {
  stop(
    "07_distance_objects.rds must contain a nested named list."
  )
}

write_log(
  "Candidate feature sets loaded: ",
  length(candidate_feature_sets),
  ".",
  log_file = log_file
)


# =============================================================================
# 6. Validate feature sets and distance objects
# =============================================================================

for (feature_set_name in names(candidate_feature_sets)) {

  feature_data <- candidate_feature_sets[[feature_set_name]]

  if (!identifier_variable %in%
      names(feature_data)) {
    stop(
      "Identifier variable missing from ",
      feature_set_name,
      "."
    )
  }

  model_data <- feature_data %>%
    dplyr::select(
      -dplyr::all_of(
        identifier_variable
      )
    )

  non_numeric_variables <- names(
    model_data
  )[
    !vapply(
      model_data,
      is.numeric,
      logical(1)
    )
  ]

  if (length(non_numeric_variables) > 0) {
    stop(
      "Non-numeric features found in ",
      feature_set_name,
      ": ",
      paste(
        non_numeric_variables,
        collapse = ", "
      )
    )
  }

  if (!feature_set_name %in%
      names(distance_objects)) {
    stop(
      "Distance objects missing for ",
      feature_set_name,
      "."
    )
  }

  missing_distances <- setdiff(
    retained_distance_methods,
    names(distance_objects[[feature_set_name]])
  )

  if (length(missing_distances) > 0) {
    stop(
      "Required distances missing for ",
      feature_set_name,
      ": ",
      paste(
        missing_distances,
        collapse = ", "
      )
    )
  }
}


# =============================================================================
# 7. Fit all candidate models
# =============================================================================

model_quality_results <- list()

cluster_models <- list()

cluster_assignments <- list()

for (feature_set_name in names(candidate_feature_sets)) {

  write_log(
    "Evaluating models for ",
    feature_set_name,
    "...",
    log_file = log_file
  )

  feature_data <- candidate_feature_sets[[feature_set_name]]

  respondent_ids <- feature_data[[identifier_variable]]

  model_data <- feature_data %>%
    dplyr::select(
      -dplyr::all_of(
        identifier_variable
      )
    )

  feature_count <- ncol(
    model_data
  )

  scaled_data <- standardise_numeric_data(
    model_data
  )

  euclidean_distance <- stats::dist(
    scaled_data,
    method = "euclidean"
  )

  cluster_models[[feature_set_name]] <- list()

  cluster_assignments[[feature_set_name]] <- list()

  # ---------------------------------------------------------------------------
  # PAM and hierarchical clustering with retained distances
  # ---------------------------------------------------------------------------

  for (
    distance_method in
    retained_distance_methods
  ) {

    distance_object <- distance_objects[[feature_set_name]][[distance_method]]

    cluster_models[[feature_set_name]][[distance_method]] <- list()

    cluster_assignments[[feature_set_name]][[distance_method]] <- list()

    for (k in k_values) {

      # PAM
      pam_model_id <- paste(
        feature_set_name,
        "pam",
        distance_method,
        paste0("k", k),
        sep = "__"
      )

      start_time <- proc.time()[["elapsed"]]

      pam_model <- fit_pam_model(
        distance_object =
          distance_object,
        k = k
      )

      pam_elapsed <- proc.time()[["elapsed"]] - start_time

      pam_assignment <-
        pam_model$clustering

      model_quality_results[[pam_model_id]] <- evaluate_assignment(
        cluster_assignment =
          pam_assignment,
        silhouette_distance =
          distance_object,
        scaled_data =
          scaled_data,
        feature_set =
          feature_set_name,
        feature_count =
          feature_count,
        method = "PAM",
        distance_method =
          distance_method,
        k = k,
        model_id = pam_model_id,
        elapsed_seconds =
          pam_elapsed
      )

      cluster_models[[feature_set_name]][[distance_method]][[paste0("pam_k", k)]] <- pam_model

      cluster_assignments[[feature_set_name]][[distance_method]][[paste0("pam_k", k)]] <- tibble::tibble(
        respondent_id =
          respondent_ids,
        cluster =
          as.integer(
            pam_assignment
          )
      )

      # Hierarchical clustering
      hierarchical_model_id <- paste(
        feature_set_name,
        "hierarchical",
        distance_method,
        paste0("k", k),
        sep = "__"
      )

      start_time <- proc.time()[["elapsed"]]

      hierarchical_model <-
        fit_hierarchical_model(
          distance_object =
            distance_object,
          k = k,
          linkage =
            hierarchical_linkage
        )

      hierarchical_elapsed <-
        proc.time()[["elapsed"]] -
        start_time

      model_quality_results[[hierarchical_model_id]] <- evaluate_assignment(
        cluster_assignment =
          hierarchical_model$
            assignment,
        silhouette_distance =
          distance_object,
        scaled_data =
          scaled_data,
        feature_set =
          feature_set_name,
        feature_count =
          feature_count,
        method =
          "Hierarchical",
        distance_method =
          distance_method,
        k = k,
        model_id =
          hierarchical_model_id,
        elapsed_seconds =
          hierarchical_elapsed
      )

      cluster_models[[feature_set_name]][[distance_method]][[paste0(
          "hierarchical_k",
          k)]] <- hierarchical_model$tree

      cluster_assignments[[feature_set_name]][[distance_method]][[paste0(
          "hierarchical_k",
          k)]] <- tibble::tibble(
        respondent_id =
          respondent_ids,
        cluster =
          as.integer(
            hierarchical_model$
              assignment
          )
      )
    }
  }

  # ---------------------------------------------------------------------------
  # K-means numerical baseline
  # ---------------------------------------------------------------------------

  cluster_models[[feature_set_name]][["kmeans"]] <- list()

  cluster_assignments[[feature_set_name]][["kmeans"]] <- list()

  for (k in k_values) {

    kmeans_model_id <- paste(
      feature_set_name,
      "kmeans",
      "euclidean",
      paste0("k", k),
      sep = "__"
    )

    start_time <- proc.time()[["elapsed"]]

    kmeans_model <- fit_kmeans_model(
      scaled_data =
        scaled_data,
      k = k,
      seed =
        random_seed + k
    )

    kmeans_elapsed <- proc.time()[["elapsed"]] - start_time

    model_quality_results[[kmeans_model_id]] <- evaluate_assignment(
      cluster_assignment =
        kmeans_model$cluster,
      silhouette_distance =
        euclidean_distance,
      scaled_data =
        scaled_data,
      feature_set =
        feature_set_name,
      feature_count =
        feature_count,
      method = "K-means",
      distance_method =
        "euclidean",
      k = k,
      model_id =
        kmeans_model_id,
      elapsed_seconds =
        kmeans_elapsed
    )

    cluster_models[[feature_set_name]][["kmeans"]][[paste0("k", k)]] <- kmeans_model

    cluster_assignments[[feature_set_name]][["kmeans"]][[paste0("k", k)]] <- tibble::tibble(
      respondent_id =
        respondent_ids,
      cluster = as.integer(
        kmeans_model$cluster
      )
    )
  }
}


# =============================================================================
# 8. Build quality table
# =============================================================================

model_quality <- dplyr::bind_rows(
  model_quality_results
) %>%
  dplyr::mutate(
    acceptable_cluster_size =
      minimum_cluster_share >=
        minimum_acceptable_cluster_share,
    silhouette_score =
      normalise_01(
        average_silhouette
      ),
    balance_score_scaled =
      normalise_01(
        balance_score
      ),
    minimum_share_score =
      normalise_01(
        minimum_cluster_share
      ),
    separation_score =
      normalise_01(
        between_ss_ratio
      ),
    preliminary_composite_score =
      weight_silhouette *
        silhouette_score +
      weight_balance *
        balance_score_scaled +
      weight_minimum_cluster_share *
        minimum_share_score +
      weight_between_ss_ratio *
        separation_score
  ) %>%
  dplyr::arrange(
    dplyr::desc(
      acceptable_cluster_size
    ),
    dplyr::desc(
      preliminary_composite_score
    )
  )

readr::write_csv(
  model_quality,
  file.path(
    tables_dir,
    "08_model_quality.csv"
  )
)


# =============================================================================
# 9. Rank model candidates
# =============================================================================

ranked_model_candidates <- model_quality %>%
  dplyr::mutate(
    overall_rank = rank(
      -preliminary_composite_score,
      ties.method = "first"
    ),
    silhouette_rank = rank(
      -average_silhouette,
      ties.method = "min"
    ),
    balance_rank = rank(
      -balance_score,
      ties.method = "min"
    ),
    separation_rank = rank(
      -between_ss_ratio,
      ties.method = "min"
    )
  ) %>%
  dplyr::arrange(
    overall_rank
  )

readr::write_csv(
  ranked_model_candidates,
  file.path(
    tables_dir,
    "08_ranked_model_candidates.csv"
  )
)

best_model_by_method <- ranked_model_candidates %>%
  dplyr::group_by(
    method,
    distance_method
  ) %>%
  dplyr::slice_min(
    order_by = overall_rank,
    n = 1,
    with_ties = FALSE
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    overall_rank
  )

readr::write_csv(
  best_model_by_method,
  file.path(
    tables_dir,
    "08_best_model_by_method.csv"
  )
)

best_model_by_feature_set <-
  ranked_model_candidates %>%
  dplyr::group_by(
    feature_set,
    feature_count
  ) %>%
  dplyr::slice_min(
    order_by = overall_rank,
    n = 1,
    with_ties = FALSE
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    dplyr::desc(feature_count)
  )

readr::write_csv(
  best_model_by_feature_set,
  file.path(
    tables_dir,
    "08_best_model_by_feature_set.csv"
  )
)


# =============================================================================
# 10. Summary tables
# =============================================================================

model_comparison_summary <- model_quality %>%
  dplyr::group_by(
    method,
    distance_method
  ) %>%
  dplyr::summarise(
    models_evaluated =
      dplyr::n(),
    mean_silhouette = mean(
      average_silhouette
    ),
    maximum_silhouette = max(
      average_silhouette
    ),
    mean_balance_score = mean(
      balance_score
    ),
    mean_minimum_cluster_share =
      mean(
        minimum_cluster_share
      ),
    mean_between_ss_ratio = mean(
      between_ss_ratio
    ),
    mean_composite_score = mean(
      preliminary_composite_score
    ),
    acceptable_size_models = sum(
      acceptable_cluster_size
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    dplyr::desc(
      mean_composite_score
    )
  )

readr::write_csv(
  model_comparison_summary,
  file.path(
    tables_dir,
    "08_model_comparison_summary.csv"
  )
)


# =============================================================================
# 11. Save reusable model objects
# =============================================================================

saveRDS(
  cluster_models,
  file.path(
    models_dir,
    "08_cluster_models.rds"
  )
)

saveRDS(
  cluster_assignments,
  file.path(
    models_dir,
    "08_cluster_assignments.rds"
  )
)


# =============================================================================
# 12. Figures
# =============================================================================

silhouette_plot <- ggplot2::ggplot(
  model_quality,
  ggplot2::aes(
    x = k,
    y = average_silhouette,
    linetype = method,
    shape = method
  )
) +
  ggplot2::geom_line(
    linewidth = 0.65
  ) +
  ggplot2::geom_point(
    size = 1.9
  ) +
  ggplot2::facet_grid(
    feature_set ~ distance_method,
    scales = "free_y"
  ) +
  ggplot2::scale_x_continuous(
    breaks = k_values
  ) +
  ggplot2::labs(
    title = "Clustering-model silhouette comparison",
    subtitle = "PAM and hierarchical use Manhattan/Gower; k-means is shown as a Euclidean baseline",
    x = "Number of clusters",
    y = "Average silhouette width",
    linetype = "Method",
    shape = "Method"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  ) +
  ggplot2::theme(
    plot.title =
      ggplot2::element_text(
        face = "bold"
      ),
    legend.position = "bottom"
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "08_model_silhouette_comparison.png"
  ),
  plot = silhouette_plot,
  width = 12,
  height = 13,
  dpi = 300,
  bg = "white"
)

top_model_plot_data <-
  ranked_model_candidates %>%
  dplyr::slice_head(
    n = min(
      number_top_models_to_plot,
      nrow(
        ranked_model_candidates
      )
    )
  ) %>%
  dplyr::mutate(
    display_label = paste0(
      feature_set,
      " | ",
      method,
      " | ",
      distance_method,
      " | k=",
      k
    ),
    display_label = reorder(
      display_label,
      preliminary_composite_score
    )
  )

top_model_plot <- ggplot2::ggplot(
  top_model_plot_data,
  ggplot2::aes(
    x = display_label,
    y = preliminary_composite_score,
    shape = acceptable_cluster_size
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Top preliminary clustering candidates",
    subtitle = "Composite score excludes bootstrap stability and interpretability",
    x = NULL,
    y = "Preliminary composite score",
    shape = "Minimum cluster share ≥ 5%"
  ) +
  ggplot2::theme_minimal(
    base_size = 11
  ) +
  ggplot2::theme(
    plot.title =
      ggplot2::element_text(
        face = "bold"
      ),
    panel.grid.major.y =
      ggplot2::element_blank(),
    legend.position = "bottom"
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "08_top_model_candidates.png"
  ),
  plot = top_model_plot,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)

balance_plot <- ggplot2::ggplot(
  model_quality,
  ggplot2::aes(
    x = average_silhouette,
    y = minimum_cluster_share,
    shape = method
  )
) +
  ggplot2::geom_hline(
    yintercept =
      minimum_acceptable_cluster_share,
    linetype = "dashed"
  ) +
  ggplot2::geom_point(
    alpha = 0.7,
    size = 2.2
  ) +
  ggplot2::facet_wrap(
    ~ feature_set
  ) +
  ggplot2::labs(
    title = "Model quality versus cluster-size balance",
    subtitle = "Dashed line marks the 5% minimum cluster-share threshold",
    x = "Average silhouette width",
    y = "Minimum cluster share",
    shape = "Method"
  ) +
  ggplot2::theme_minimal(
    base_size = 11
  ) +
  ggplot2::theme(
    plot.title =
      ggplot2::element_text(
        face = "bold"
      ),
    legend.position = "bottom"
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "08_cluster_balance_comparison.png"
  ),
  plot = balance_plot,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)


# =============================================================================
# 13. Final report
# =============================================================================

best_candidate <- ranked_model_candidates %>%
  dplyr::slice_head(
    n = 1
  )

capture.output(
  sessionInfo(),
  file = file.path(
    logs_dir,
    "08_sessionInfo.txt"
  )
)

write_log(
  "Model comparison completed successfully.",
  log_file = log_file
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — MODEL COMPARISON COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Candidate feature sets:         ",
  length(candidate_feature_sets),
  "\n",
  "Values of k:                    ",
  paste(
    k_values,
    collapse = ", "
  ),
  "\n",
  "Models evaluated:               ",
  nrow(model_quality),
  "\n",
  "Top preliminary candidate:      ",
  best_candidate$model_id,
  "\n",
  "Average silhouette:             ",
  round(
    best_candidate$
      average_silhouette,
    3
  ),
  "\n",
  "Minimum cluster share:          ",
  round(
    100 *
      best_candidate$
        minimum_cluster_share,
    1
  ),
  "%\n",
  "Preliminary composite score:    ",
  round(
    best_candidate$
      preliminary_composite_score,
    3
  ),
  "\n",
  "\nMain ranked output:\n",
  file.path(
    tables_dir,
    "08_ranked_model_candidates.csv"
  ),
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)
