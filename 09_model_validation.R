# =============================================================================
# 09_model_validation.R
# Behavioural Segmentation Toolkit
#
# Purpose:
#   - Load ranked clustering candidates and saved assignments
#   - Restrict validation to practically useful values of k
#   - Evaluate bootstrap cluster stability using Jaccard similarity
#   - Calculate Gap statistic, Calinski-Harabasz and Davies-Bouldin indices
#   - Combine quality, stability and balance into a validation score
#   - Produce a shortlist for final interpretability assessment
#
# Important:
#   k = 2 is retained as a reference in the earlier model-comparison stage,
#   but is excluded from the primary practical shortlist by default.
#
# Expected inputs:
#   data/processed/06_candidate_feature_sets.rds
#   outputs/tables/08_ranked_model_candidates.csv
#   outputs/models/08_cluster_assignments.rds
#   outputs/models/07_distance_objects.rds
#
# Main outputs:
#   outputs/tables/09_bootstrap_stability.csv
#   outputs/tables/09_internal_validation_metrics.csv
#   outputs/tables/09_validated_model_candidates.csv
#   outputs/tables/09_model_validation_summary.csv
#   outputs/models/09_bootstrap_assignments.rds
#   outputs/figures/09_stability_by_model.png
#   outputs/figures/09_validation_score.png
#   outputs/figures/09_quality_stability_tradeoff.png
#   outputs/logs/09_model_validation.log
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
  "purrr",
  "tibble",
  "tidyr",
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

# Practical segmentation range.
validation_k_values <- 3:6

# Number of top preliminary models to validate.
maximum_models_to_validate <- 30

# Bootstrap settings.
bootstrap_iterations <- 100
bootstrap_sample_fraction <- 0.80

# Gap statistic settings.
gap_bootstrap_iterations <- 50

# Stability thresholds.
minimum_mean_jaccard <- 0.60
good_mean_jaccard <- 0.75

# Cluster-size threshold.
minimum_acceptable_cluster_share <- 0.05

# Validation score weights.
weight_preliminary_quality <- 0.30
weight_jaccard_stability <- 0.35
weight_gap <- 0.15
weight_calinski_harabasz <- 0.10
weight_davies_bouldin <- 0.10

random_seed <- 123


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


cluster_centroids <- function(
    scaled_data,
    cluster_assignment
) {

  cluster_ids <- sort(
    unique(cluster_assignment)
  )

  purrr::map_dfr(
    cluster_ids,
    function(cluster_id) {

      cluster_data <- scaled_data[
        cluster_assignment == cluster_id,
        ,
        drop = FALSE
      ]

      tibble::as_tibble_row(
        colMeans(cluster_data),
        .name_repair = "minimal"
      ) %>%
        dplyr::mutate(
          cluster = cluster_id,
          .before = 1
        )
    }
  )
}


assign_to_nearest_centroid <- function(
    scaled_data,
    centroid_table
) {

  centroid_matrix <- as.matrix(
    centroid_table %>%
      dplyr::select(
        -cluster
      )
  )

  cluster_ids <- centroid_table$cluster

  assignment <- apply(
    scaled_data,
    1,
    function(row_values) {

      distances <- sqrt(
        rowSums(
          (
            centroid_matrix -
              matrix(
                row_values,
                nrow = nrow(centroid_matrix),
                ncol = ncol(centroid_matrix),
                byrow = TRUE
              )
          )^2
        )
      )

      cluster_ids[
        which.min(distances)
      ]
    }
  )

  as.integer(assignment)
}


jaccard_similarity <- function(
    reference_indices,
    bootstrap_indices
) {

  union_size <- length(
    union(
      reference_indices,
      bootstrap_indices
    )
  )

  if (union_size == 0) {
    return(NA_real_)
  }

  length(
    intersect(
      reference_indices,
      bootstrap_indices
    )
  ) / union_size
}


match_bootstrap_clusters <- function(
    reference_assignment,
    bootstrap_assignment,
    sampled_indices,
    k
) {

  reference_sample <- reference_assignment[
    sampled_indices
  ]

  similarity_matrix <- matrix(
    0,
    nrow = k,
    ncol = k
  )

  for (reference_cluster in seq_len(k)) {

    reference_members <- which(
      reference_sample ==
        reference_cluster
    )

    for (bootstrap_cluster in seq_len(k)) {

      bootstrap_members <- which(
        bootstrap_assignment ==
          bootstrap_cluster
      )

      similarity_matrix[
        reference_cluster,
        bootstrap_cluster
      ] <- jaccard_similarity(
        reference_members,
        bootstrap_members
      )
    }
  }

  matched_scores <- numeric(k)
  used_bootstrap_clusters <- integer()

  for (reference_cluster in seq_len(k)) {

    available_clusters <- setdiff(
      seq_len(k),
      used_bootstrap_clusters
    )

    best_cluster <- available_clusters[
      which.max(
        similarity_matrix[
          reference_cluster,
          available_clusters
        ]
      )
    ]

    matched_scores[
      reference_cluster
    ] <- similarity_matrix[
      reference_cluster,
      best_cluster
    ]

    used_bootstrap_clusters <- c(
      used_bootstrap_clusters,
      best_cluster
    )
  }

  matched_scores
}


fit_model_assignment <- function(
    method,
    distance_method,
    k,
    model_data,
    distance_object = NULL,
    seed = random_seed
) {

  scaled_data <- standardise_numeric_data(
    model_data
  )

  if (method == "K-means") {

    set.seed(seed)

    model <- stats::kmeans(
      scaled_data,
      centers = k,
      nstart = 100,
      iter.max = 200
    )

    return(
      list(
        assignment = as.integer(
          model$cluster
        ),
        scaled_data = scaled_data
      )
    )
  }

  if (is.null(distance_object)) {
    stop(
      "A distance object is required for ",
      method,
      "."
    )
  }

  if (method == "PAM") {

    model <- cluster::pam(
      distance_object,
      k = k,
      diss = TRUE
    )

    return(
      list(
        assignment = as.integer(
          model$clustering
        ),
        scaled_data = scaled_data
      )
    )
  }

  if (method == "Hierarchical") {

    tree <- stats::hclust(
      distance_object,
      method = "average"
    )

    return(
      list(
        assignment = as.integer(
          stats::cutree(
            tree,
            k = k
          )
        ),
        scaled_data = scaled_data
      )
    )
  }

  stop(
    "Unsupported clustering method: ",
    method
  )
}


compute_subset_distance <- function(
    model_data,
    distance_method
) {

  if (distance_method == "euclidean") {

    return(
      stats::dist(
        standardise_numeric_data(
          model_data
        ),
        method = "euclidean"
      )
    )
  }

  if (distance_method == "manhattan") {

    return(
      stats::dist(
        standardise_numeric_data(
          model_data
        ),
        method = "manhattan"
      )
    )
  }

  if (distance_method == "gower") {

    return(
      cluster::daisy(
        model_data,
        metric = "gower"
      )
    )
  }

  stop(
    "Unsupported distance method: ",
    distance_method
  )
}


bootstrap_model_stability <- function(
    model_data,
    reference_assignment,
    method,
    distance_method,
    k,
    iterations,
    sample_fraction,
    seed
) {

  set.seed(seed)

  n <- nrow(model_data)

  bootstrap_scores <- matrix(
    NA_real_,
    nrow = iterations,
    ncol = k
  )

  bootstrap_assignments <- vector(
    "list",
    iterations
  )

  for (iteration in seq_len(iterations)) {

    sampled_indices <- sample(
      seq_len(n),
      size = floor(
        sample_fraction * n
      ),
      replace = TRUE
    )

    bootstrap_data <- model_data[
      sampled_indices,
      ,
      drop = FALSE
    ]

    if (method == "K-means") {

      bootstrap_fit <- fit_model_assignment(
        method = method,
        distance_method =
          distance_method,
        k = k,
        model_data =
          bootstrap_data,
        seed = seed +
          iteration
      )

    } else {

      bootstrap_distance <-
        compute_subset_distance(
          model_data =
            bootstrap_data,
          distance_method =
            distance_method
        )

      bootstrap_fit <- fit_model_assignment(
        method = method,
        distance_method =
          distance_method,
        k = k,
        model_data =
          bootstrap_data,
        distance_object =
          bootstrap_distance,
        seed = seed +
          iteration
      )
    }

    bootstrap_scores[
      iteration,
    ] <- match_bootstrap_clusters(
      reference_assignment =
        reference_assignment,
      bootstrap_assignment =
        bootstrap_fit$assignment,
      sampled_indices =
        sampled_indices,
      k = k
    )

    bootstrap_assignments[[iteration]] <- tibble::tibble(
      sampled_index =
        sampled_indices,
      cluster =
        bootstrap_fit$assignment
    )
  }

  list(
    summary = tibble::tibble(
      cluster = seq_len(k),
      mean_jaccard = colMeans(
        bootstrap_scores,
        na.rm = TRUE
      ),
      median_jaccard = apply(
        bootstrap_scores,
        2,
        stats::median,
        na.rm = TRUE
      ),
      sd_jaccard = apply(
        bootstrap_scores,
        2,
        stats::sd,
        na.rm = TRUE
      ),
      minimum_jaccard = apply(
        bootstrap_scores,
        2,
        min,
        na.rm = TRUE
      )
    ),
    assignments =
      bootstrap_assignments
  )
}


davies_bouldin_index <- function(
    scaled_data,
    cluster_assignment
) {

  cluster_ids <- sort(
    unique(cluster_assignment)
  )

  k <- length(cluster_ids)

  centroids <- cluster_centroids(
    scaled_data,
    cluster_assignment
  )

  centroid_matrix <- as.matrix(
    centroids %>%
      dplyr::select(
        -cluster
      )
  )

  within_scatter <- numeric(k)

  for (i in seq_along(cluster_ids)) {

    cluster_data <- scaled_data[
      cluster_assignment ==
        cluster_ids[i],
      ,
      drop = FALSE
    ]

    centroid <- centroid_matrix[
      i,
      ,
      drop = FALSE
    ]

    within_scatter[i] <- mean(
      sqrt(
        rowSums(
          (
            cluster_data -
              matrix(
                centroid,
                nrow = nrow(cluster_data),
                ncol = ncol(cluster_data),
                byrow = TRUE
              )
          )^2
        )
      )
    )
  }

  centroid_distances <- as.matrix(
    stats::dist(
      centroid_matrix
    )
  )

  db_values <- numeric(k)

  for (i in seq_len(k)) {

    ratios <- numeric()

    for (j in seq_len(k)) {

      if (i == j) {
        next
      }

      ratios <- c(
        ratios,
        (
          within_scatter[i] +
            within_scatter[j]
        ) /
          centroid_distances[i, j]
      )
    }

    db_values[i] <- max(
      ratios
    )
  }

  mean(db_values)
}


calinski_harabasz_index <- function(
    scaled_data,
    cluster_assignment
) {

  n <- nrow(scaled_data)
  k <- length(
    unique(cluster_assignment)
  )

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

  within_ss <- 0

  for (
    cluster_id in
    unique(cluster_assignment)
  ) {

    cluster_data <- scaled_data[
      cluster_assignment ==
        cluster_id,
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

  if (
    k <= 1 ||
    n <= k ||
    within_ss <= 0
  ) {
    return(NA_real_)
  }

  (
    between_ss /
      (k - 1)
  ) /
    (
      within_ss /
        (n - k)
    )
}


gap_statistic_for_k <- function(
    model_data,
    k,
    iterations,
    seed
) {

  set.seed(seed)

  scaled_data <- standardise_numeric_data(
    model_data
  )

  observed_model <- stats::kmeans(
    scaled_data,
    centers = k,
    nstart = 50,
    iter.max = 200
  )

  observed_log_within <- log(
    observed_model$tot.withinss
  )

  minimums <- apply(
    scaled_data,
    2,
    min
  )

  maximums <- apply(
    scaled_data,
    2,
    max
  )

  reference_log_within <- numeric(
    iterations
  )

  for (iteration in seq_len(iterations)) {

    reference_data <- sapply(
      seq_len(ncol(scaled_data)),
      function(column_index) {

        stats::runif(
          nrow(scaled_data),
          min = minimums[
            column_index
          ],
          max = maximums[
            column_index
          ]
        )
      }
    )

    reference_model <- stats::kmeans(
      reference_data,
      centers = k,
      nstart = 20,
      iter.max = 200
    )

    reference_log_within[
      iteration
    ] <- log(
      reference_model$tot.withinss
    )
  }

  tibble::tibble(
    gap_statistic = mean(
      reference_log_within
    ) - observed_log_within,
    gap_standard_error =
      sqrt(
        1 + 1 / iterations
      ) *
      stats::sd(
        reference_log_within
      )
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

ranked_models_file <- file.path(
  tables_dir,
  "08_ranked_model_candidates.csv"
)

assignments_file <- file.path(
  models_dir,
  "08_cluster_assignments.rds"
)

distance_objects_file <- file.path(
  models_dir,
  "07_distance_objects.rds"
)

log_file <- file.path(
  logs_dir,
  "09_model_validation.log"
)

if (file.exists(log_file)) {
  file.remove(log_file)
}


# =============================================================================
# 5. Load inputs
# =============================================================================

required_input_files <- c(
  feature_sets_file,
  ranked_models_file,
  assignments_file,
  distance_objects_file
)

missing_input_files <- required_input_files[
  !file.exists(
    required_input_files
  )
]

if (length(missing_input_files) > 0) {
  stop(
    "Required input files are missing:\n",
    paste(
      missing_input_files,
      collapse = "\n"
    )
  )
}

candidate_feature_sets <- readRDS(
  feature_sets_file
)

ranked_models <- readr::read_csv(
  ranked_models_file,
  show_col_types = FALSE
)

cluster_assignments <- readRDS(
  assignments_file
)

distance_objects <- readRDS(
  distance_objects_file
)

write_log(
  "Inputs loaded successfully.",
  log_file = log_file
)


# =============================================================================
# 6. Select models for validation
# =============================================================================

models_to_validate <- ranked_models %>%
  dplyr::filter(
    k %in% validation_k_values,
    acceptable_cluster_size
  ) %>%
  dplyr::arrange(
    overall_rank
  ) %>%
  dplyr::slice_head(
    n = maximum_models_to_validate
  )

if (nrow(models_to_validate) == 0) {
  stop(
    "No models satisfy the validation filters."
  )
}

write_log(
  "Models selected for validation: ",
  nrow(models_to_validate),
  ".",
  log_file = log_file
)


# =============================================================================
# 7. Validate each candidate model
# =============================================================================

bootstrap_stability_results <- list()
internal_validation_results <- list()
bootstrap_assignment_objects <- list()

for (row_index in seq_len(nrow(models_to_validate))) {

  candidate <- models_to_validate[
    row_index,
    ,
    drop = FALSE
  ]

  model_id <- candidate$model_id
  feature_set <- candidate$feature_set
  method <- candidate$method
  distance_method <-
    candidate$distance_method
  k <- candidate$k

  write_log(
    "Validating ",
    model_id,
    "...",
    log_file = log_file
  )

  feature_data <- candidate_feature_sets[[feature_set]]

  model_data <- feature_data %>%
    dplyr::select(
      -dplyr::all_of(
        identifier_variable
      )
    )

  scaled_data <- standardise_numeric_data(
    model_data
  )

  if (method == "K-means") {

    assignment_table <-
      cluster_assignments[[feature_set]][["kmeans"]][[paste0("k", k)]]

  } else {

    assignment_key <- ifelse(
      method == "PAM",
      paste0("pam_k", k),
      paste0(
        "hierarchical_k",
        k
      )
    )

    assignment_table <-
      cluster_assignments[[feature_set]][[distance_method]][[assignment_key]]
  }

  reference_assignment <-
    assignment_table$cluster

  stability_result <-
    bootstrap_model_stability(
      model_data = model_data,
      reference_assignment =
        reference_assignment,
      method = method,
      distance_method =
        distance_method,
      k = k,
      iterations =
        bootstrap_iterations,
      sample_fraction =
        bootstrap_sample_fraction,
      seed =
        random_seed +
        row_index * 1000
    )

  bootstrap_stability_results[[model_id]] <- stability_result$summary %>%
    dplyr::mutate(
      model_id = model_id,
      feature_set = feature_set,
      method = method,
      distance_method =
        distance_method,
      k = k,
      .before = 1
    )

  bootstrap_assignment_objects[[model_id]] <- stability_result$assignments

  gap_result <- gap_statistic_for_k(
    model_data = model_data,
    k = k,
    iterations =
      gap_bootstrap_iterations,
    seed =
      random_seed +
      row_index * 100
  )

  internal_validation_results[[model_id]] <- tibble::tibble(
    model_id = model_id,
    feature_set = feature_set,
    method = method,
    distance_method =
      distance_method,
    k = k,
    calinski_harabasz =
      calinski_harabasz_index(
        scaled_data,
        reference_assignment
      ),
    davies_bouldin =
      davies_bouldin_index(
        scaled_data,
        reference_assignment
      ),
    gap_statistic =
      gap_result$gap_statistic,
    gap_standard_error =
      gap_result$
        gap_standard_error
  )
}

bootstrap_stability <- dplyr::bind_rows(
  bootstrap_stability_results
)

internal_validation_metrics <-
  dplyr::bind_rows(
    internal_validation_results
  )

readr::write_csv(
  bootstrap_stability,
  file.path(
    tables_dir,
    "09_bootstrap_stability.csv"
  )
)

readr::write_csv(
  internal_validation_metrics,
  file.path(
    tables_dir,
    "09_internal_validation_metrics.csv"
  )
)

saveRDS(
  bootstrap_assignment_objects,
  file.path(
    models_dir,
    "09_bootstrap_assignments.rds"
  )
)


# =============================================================================
# 8. Aggregate stability by model
# =============================================================================

model_stability_summary <- bootstrap_stability %>%
  dplyr::rename(
    cluster_mean_jaccard = mean_jaccard,
    cluster_median_jaccard = median_jaccard,
    cluster_sd_jaccard = sd_jaccard,
    cluster_minimum_jaccard = minimum_jaccard
  ) %>%
  dplyr::group_by(
    model_id,
    feature_set,
    method,
    distance_method,
    k
  ) %>%
  dplyr::summarise(
    mean_jaccard = mean(
      cluster_mean_jaccard,
      na.rm = TRUE
    ),
    minimum_cluster_jaccard = min(
      cluster_mean_jaccard,
      na.rm = TRUE
    ),
    median_cluster_jaccard = stats::median(
      cluster_mean_jaccard,
      na.rm = TRUE
    ),
    mean_jaccard_sd = mean(
      cluster_sd_jaccard,
      na.rm = TRUE
    ),
    maximum_jaccard_sd = max(
      cluster_sd_jaccard,
      na.rm = TRUE
    ),
    stable_clusters = sum(
      cluster_mean_jaccard >=
        minimum_mean_jaccard
    ),
    total_clusters = dplyr::n(),
    stable_cluster_share =
      stable_clusters /
      total_clusters,
    stable_model =
      mean_jaccard >=
      minimum_mean_jaccard &
      minimum_cluster_jaccard >=
      minimum_mean_jaccard,
    good_stability =
      mean_jaccard >=
      good_mean_jaccard &
      minimum_cluster_jaccard >=
      minimum_mean_jaccard,
    .groups = "drop"
  )


# =============================================================================
# 9. Build validated candidate table
# =============================================================================

validated_model_candidates <- models_to_validate %>%
  dplyr::select(
    -dplyr::any_of(
      c(
        "calinski_harabasz",
        "davies_bouldin",
        "gap_statistic",
        "gap_standard_error"
      )
    )
  ) %>%
  dplyr::left_join(
    model_stability_summary,
    by = c(
      "model_id",
      "feature_set",
      "method",
      "distance_method",
      "k"
    )
  ) %>%
  dplyr::left_join(
    internal_validation_metrics,
    by = c(
      "model_id",
      "feature_set",
      "method",
      "distance_method",
      "k"
    )
  ) %>%
  dplyr::mutate(
    preliminary_quality_score =
      normalise_01(
        preliminary_composite_score
      ),
    
    jaccard_score =
      normalise_01(
        mean_jaccard
      ),
    
    gap_score =
      normalise_01(
        gap_statistic
      ),
    
    calinski_harabasz_score =
      normalise_01(
        calinski_harabasz
      ),
    
    davies_bouldin_score =
      1 -
      normalise_01(
        davies_bouldin
      ),
    
    validation_score =
      weight_preliminary_quality *
      preliminary_quality_score +
      weight_jaccard_stability *
      jaccard_score +
      weight_gap *
      gap_score +
      weight_calinski_harabasz *
      calinski_harabasz_score +
      weight_davies_bouldin *
      davies_bouldin_score,
    
    validation_rank = rank(
      -validation_score,
      ties.method = "first"
    ),
    
    shortlist_status =
      dplyr::case_when(
        stable_model &
          acceptable_cluster_size ~
          "Shortlist",
        
        !stable_model ~
          "Insufficient bootstrap stability",
        
        !acceptable_cluster_size ~
          "Cluster-size imbalance",
        
        TRUE ~
          "Review"
      )
  ) %>%
  dplyr::arrange(
    validation_rank
  )

readr::write_csv(
  validated_model_candidates,
  file.path(
    tables_dir,
    "09_validated_model_candidates.csv"
  )
)


# =============================================================================
# 10. Validation summary
# =============================================================================

model_validation_summary <- validated_model_candidates %>%
  dplyr::group_by(
    method,
    distance_method
  ) %>%
  dplyr::summarise(
    models_validated =
      dplyr::n(),
    mean_jaccard = mean(
      mean_jaccard
    ),
    maximum_jaccard = max(
      mean_jaccard
    ),
    stable_models = sum(
      stable_model
    ),
    mean_gap = mean(
      gap_statistic
    ),
    mean_validation_score = mean(
      validation_score
    ),
    shortlist_models = sum(
      shortlist_status ==
        "Shortlist"
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    dplyr::desc(
      mean_validation_score
    )
  )

readr::write_csv(
  model_validation_summary,
  file.path(
    tables_dir,
    "09_model_validation_summary.csv"
  )
)


# =============================================================================
# 11. Figures
# =============================================================================

stability_plot <- ggplot2::ggplot(
  model_stability_summary,
  ggplot2::aes(
    x = factor(k),
    y = mean_jaccard,
    shape = method
  )
) +
  ggplot2::geom_hline(
    yintercept =
      minimum_mean_jaccard,
    linetype = "dashed"
  ) +
  ggplot2::geom_hline(
    yintercept =
      good_mean_jaccard,
    linetype = "dotted"
  ) +
  ggplot2::geom_point(
    size = 2.7
  ) +
  ggplot2::facet_grid(
    feature_set ~
      distance_method
  ) +
  ggplot2::labs(
    title = "Bootstrap cluster stability",
    subtitle = "Dashed line = minimum acceptable Jaccard; dotted line = good stability",
    x = "Number of clusters",
    y = "Mean cluster Jaccard similarity",
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
    "09_stability_by_model.png"
  ),
  plot = stability_plot,
  width = 12,
  height = 12,
  dpi = 300,
  bg = "white"
)

validation_plot_data <-
  validated_model_candidates %>%
  dplyr::slice_head(
    n = min(
      20,
      nrow(
        validated_model_candidates
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
      validation_score
    )
  )

validation_score_plot <- ggplot2::ggplot(
  validation_plot_data,
  ggplot2::aes(
    x = display_label,
    y = validation_score,
    shape = shortlist_status
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Validated clustering candidates",
    subtitle = "Composite score combines preliminary quality, stability and internal validation",
    x = NULL,
    y = "Validation score",
    shape = "Status"
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
    "09_validation_score.png"
  ),
  plot = validation_score_plot,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)

quality_stability_plot <- ggplot2::ggplot(
  validated_model_candidates,
  ggplot2::aes(
    x = preliminary_composite_score,
    y = mean_jaccard,
    shape = method
  )
) +
  ggplot2::geom_hline(
    yintercept =
      minimum_mean_jaccard,
    linetype = "dashed"
  ) +
  ggplot2::geom_point(
    size = 2.8,
    alpha = 0.8
  ) +
  ggplot2::facet_wrap(
    ~ feature_set
  ) +
  ggplot2::labs(
    title = "Model quality–stability trade-off",
    subtitle = "Preferred candidates combine strong preliminary quality with stable bootstrap recovery",
    x = "Preliminary model quality score",
    y = "Mean Jaccard stability",
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
    "09_quality_stability_tradeoff.png"
  ),
  plot = quality_stability_plot,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)


# =============================================================================
# 12. Final report
# =============================================================================

best_validated_candidate <-
  validated_model_candidates %>%
  dplyr::filter(
    shortlist_status ==
      "Shortlist"
  ) %>%
  dplyr::slice_min(
    order_by = validation_rank,
    n = 1,
    with_ties = FALSE
  )

if (
  nrow(best_validated_candidate) == 0
) {
  best_validated_candidate <-
    validated_model_candidates %>%
    dplyr::slice_head(
      n = 1
    )
}

capture.output(
  sessionInfo(),
  file = file.path(
    logs_dir,
    "09_sessionInfo.txt"
  )
)

write_log(
  "Model validation completed successfully.",
  log_file = log_file
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — MODEL VALIDATION COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Models validated:               ",
  nrow(
    validated_model_candidates
  ),
  "\n",
  "Practical k range:              ",
  paste(
    validation_k_values,
    collapse = ", "
  ),
  "\n",
  "Bootstrap iterations:          ",
  bootstrap_iterations,
  "\n",
  "Models passing stability:      ",
  sum(
    validated_model_candidates$
      stable_model
  ),
  "\n",
  "Shortlisted models:            ",
  sum(
    validated_model_candidates$
      shortlist_status ==
      "Shortlist"
  ),
  "\n",
  "Top validated candidate:       ",
  best_validated_candidate$
    model_id,
  "\n",
  "Mean Jaccard stability:        ",
  round(
    best_validated_candidate$
      mean_jaccard,
    3
  ),
  "\n",
  "Validation score:              ",
  round(
    best_validated_candidate$
      validation_score,
    3
  ),
  "\n",
  "\nMain validated output:\n",
  file.path(
    tables_dir,
    "09_validated_model_candidates.csv"
  ),
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)
