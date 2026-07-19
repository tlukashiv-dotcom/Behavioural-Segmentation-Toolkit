# =============================================================================
# 10_final_model_selection.R
# Behavioural Segmentation Toolkit
#
# Purpose:
#   - Load validated shortlist models
#   - Extract their saved cluster assignments
#   - Compare candidate partitions using Adjusted Rand Index
#   - Build standardised segmentation profiles
#   - Evaluate profile distinctness and profiling-variable associations
#   - Select and save the final segmentation model
#
# Important:
#   This script selects the final statistical model, but does not assign
#   substantive segment names. Naming and narrative interpretation remain
#   part of the downstream profiling stage.
#
# Expected inputs:
#   outputs/tables/09_validated_model_candidates.csv
#   outputs/models/08_cluster_assignments.rds
#   data/processed/06_candidate_feature_sets.rds
#   data/processed/profiling_data.rds
#   data/processed/02_variable_metadata.rds
#
# Main outputs:
#   outputs/tables/10_candidate_models.csv
#   outputs/tables/10_pairwise_adjusted_rand.csv
#   outputs/tables/10_segmentation_profiles_long.csv
#   outputs/tables/10_profile_distinctness.csv
#   outputs/tables/10_profiling_associations.csv
#   outputs/tables/10_candidate_profile_scores.csv
#   outputs/tables/10_candidate_profile_summary.csv
#   outputs/models/10_candidate_assignments.rds
#   outputs/models/10_final_model_assignment.rds
#   data/processed/10_final_segmentation_data.rds
#   data/processed/10_final_model_metadata.rds
#   outputs/tables/10_final_model_summary.csv
#   outputs/figures/10_pairwise_ari_heatmap.png
#   outputs/figures/10_candidate_profile_heatmaps.png
#   outputs/figures/10_profile_distinctness.png
#   outputs/logs/10_candidate_profile_comparison.log
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
  "ggplot2"
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

# Number of validated shortlist models to compare in depth.
maximum_candidate_models <- 6

# Prefer candidates within this validation-score distance from the best model.
validation_score_tolerance <- 0.20

# Minimum cluster share required for practical interpretation.
minimum_acceptable_cluster_share <- 0.05

# Number of strongest profiling associations retained per model.
maximum_profiling_associations_per_model <- 20

# Composite profile-comparison weights.
weight_validation_score <- 0.45
weight_segmentation_distinctness <- 0.30
weight_profiling_distinctness <- 0.15
weight_cluster_balance <- 0.10


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


adjusted_rand_index <- function(
    assignment_1,
    assignment_2
) {

  contingency <- table(
    assignment_1,
    assignment_2
  )

  n <- sum(contingency)

  if (n < 2) {
    return(NA_real_)
  }

  choose_2 <- function(x) {
    x * (x - 1) / 2
  }

  sum_cells <- sum(
    choose_2(contingency)
  )

  row_pairs <- sum(
    choose_2(
      rowSums(contingency)
    )
  )

  column_pairs <- sum(
    choose_2(
      colSums(contingency)
    )
  )

  total_pairs <- choose_2(n)

  expected_index <- (
    row_pairs *
      column_pairs
  ) / total_pairs

  maximum_index <- 0.5 * (
    row_pairs +
      column_pairs
  )

  denominator <- maximum_index -
    expected_index

  if (denominator == 0) {
    return(1)
  }

  (
    sum_cells -
      expected_index
  ) / denominator
}


extract_assignment <- function(
    assignments_object,
    feature_set,
    method,
    distance_method,
    k
) {

  if (method == "K-means") {

    assignment_table <-
      assignments_object[[feature_set]][["kmeans"]][[paste0("k", k)]]

  } else {

    assignment_key <- ifelse(
      method == "PAM",
      paste0("pam_k", k),
      paste0("hierarchical_k", k)
    )

    assignment_table <-
      assignments_object[[feature_set]][[distance_method]][[assignment_key]]
  }

  if (is.null(assignment_table)) {
    stop(
      "Saved assignment not found for: ",
      feature_set,
      " | ",
      method,
      " | ",
      distance_method,
      " | k=",
      k
    )
  }

  assignment_table %>%
    dplyr::rename(
      !!identifier_variable :=
        respondent_id
    )
}


safe_eta_squared <- function(
    x,
    cluster
) {

  valid <- !is.na(x) &
    !is.na(cluster)

  x <- x[valid]
  cluster <- cluster[valid]

  if (
    length(x) < 3 ||
    length(unique(cluster)) < 2
  ) {
    return(NA_real_)
  }

  total_ss <- sum(
    (
      x - mean(x)
    )^2
  )

  if (total_ss == 0) {
    return(0)
  }

  group_means <- tapply(
    x,
    cluster,
    mean
  )

  group_sizes <- table(
    cluster
  )

  between_ss <- sum(
    as.numeric(group_sizes) *
      (
        group_means -
          mean(x)
      )^2
  )

  between_ss / total_ss
}


safe_cramers_v <- function(
    x,
    cluster
) {

  valid <- !is.na(x) &
    !is.na(cluster)

  x <- droplevels(
    factor(x[valid])
  )

  cluster <- droplevels(
    factor(cluster[valid])
  )

  if (
    length(x) == 0 ||
    nlevels(x) < 2 ||
    nlevels(cluster) < 2
  ) {
    return(NA_real_)
  }

  contingency <- table(
    x,
    cluster
  )

  test <- suppressWarnings(
    stats::chisq.test(
      contingency,
      correct = FALSE
    )
  )

  n <- sum(contingency)
  minimum_dimension <- min(
    nrow(contingency) - 1,
    ncol(contingency) - 1
  )

  if (
    n == 0 ||
    minimum_dimension <= 0
  ) {
    return(NA_real_)
  }

  sqrt(
    as.numeric(test$statistic) /
      (
        n *
          minimum_dimension
      )
  )
}


cluster_profile_long <- function(
    model_data,
    assignment_table,
    model_id,
    metadata
) {

  joined <- model_data %>%
    dplyr::left_join(
      assignment_table,
      by = identifier_variable
    )

  feature_variables <- setdiff(
    names(model_data),
    identifier_variable
  )

  numeric_data <- joined %>%
    dplyr::select(
      dplyr::all_of(
        feature_variables
      )
    )

  means <- vapply(
    numeric_data,
    mean,
    numeric(1),
    na.rm = TRUE
  )

  standard_deviations <- vapply(
    numeric_data,
    stats::sd,
    numeric(1),
    na.rm = TRUE
  )

  standard_deviations[
    !is.finite(standard_deviations) |
      standard_deviations == 0
  ] <- 1

  joined %>%
    dplyr::group_by(
      cluster
    ) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(
          feature_variables
        ),
        ~ mean(
          .x,
          na.rm = TRUE
        )
      ),
      cluster_size = dplyr::n(),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(
        feature_variables
      ),
      names_to = "variable",
      values_to = "cluster_mean"
    ) %>%
    dplyr::mutate(
      overall_mean = means[variable],
      overall_sd =
        standard_deviations[variable],
      standardised_mean =
        (
          cluster_mean -
            overall_mean
        ) / overall_sd,
      model_id = model_id,
      .before = 1
    ) %>%
    dplyr::left_join(
      metadata %>%
        dplyr::select(
          variable,
          label,
          group,
          data_type
        ) %>%
        dplyr::distinct(
          variable,
          .keep_all = TRUE
        ),
      by = "variable"
    )
}


profile_distinctness_summary <- function(
    profile_long
) {

  profile_long %>%
    dplyr::group_by(
      model_id,
      variable,
      label,
      group
    ) %>%
    dplyr::summarise(
      standardised_range =
        max(
          standardised_mean,
          na.rm = TRUE
        ) -
        min(
          standardised_mean,
          na.rm = TRUE
        ),
      maximum_absolute_deviation =
        max(
          abs(
            standardised_mean
          ),
          na.rm = TRUE
        ),
      .groups = "drop"
    )
}


profiling_associations_for_model <- function(
    profiling_data,
    assignment_table,
    model_id,
    metadata
) {

  joined <- profiling_data %>%
    dplyr::left_join(
      assignment_table,
      by = identifier_variable
    )

  profiling_variables <- setdiff(
    names(profiling_data),
    identifier_variable
  )

  purrr::map_dfr(
    profiling_variables,
    function(variable_name) {

      x <- joined[[variable_name]]
      cluster <- joined$cluster

      if (is.numeric(x)) {

        association_value <-
          safe_eta_squared(
            x,
            cluster
          )

        association_type <-
          "Eta squared"

      } else {

        association_value <-
          safe_cramers_v(
            x,
            cluster
          )

        association_type <-
          "Cramer's V"
      }

      metadata_row <- metadata %>%
        dplyr::filter(
          variable ==
            variable_name
        ) %>%
        dplyr::slice_head(
          n = 1
        )

      tibble::tibble(
        model_id = model_id,
        variable = variable_name,
        label = ifelse(
          nrow(metadata_row) == 0,
          variable_name,
          metadata_row$label[1]
        ),
        group = ifelse(
          nrow(metadata_row) == 0,
          "Unspecified",
          metadata_row$group[1]
        ),
        association_type =
          association_type,
        association_value =
          association_value
      )
    }
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

validated_models_file <- file.path(
  tables_dir,
  "09_validated_model_candidates.csv"
)

assignments_file <- file.path(
  models_dir,
  "08_cluster_assignments.rds"
)

feature_sets_file <- file.path(
  data_processed_dir,
  "06_candidate_feature_sets.rds"
)

profiling_data_file <- file.path(
  data_processed_dir,
  "profiling_data.rds"
)

metadata_file <- file.path(
  data_processed_dir,
  "02_variable_metadata.rds"
)

log_file <- file.path(
  logs_dir,
  "10_candidate_profile_comparison.log"
)

if (file.exists(log_file)) {
  file.remove(log_file)
}


# =============================================================================
# 5. Load inputs
# =============================================================================

required_input_files <- c(
  validated_models_file,
  assignments_file,
  feature_sets_file,
  profiling_data_file,
  metadata_file
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

validated_models <- readr::read_csv(
  validated_models_file,
  show_col_types = FALSE
)

cluster_assignments <- readRDS(
  assignments_file
)

candidate_feature_sets <- readRDS(
  feature_sets_file
)

profiling_data <- readRDS(
  profiling_data_file
) %>%
  tibble::as_tibble()

variable_metadata <- readRDS(
  metadata_file
) %>%
  tibble::as_tibble()

write_log(
  "Inputs loaded successfully.",
  log_file = log_file
)


# =============================================================================
# 6. Select candidate models
# =============================================================================

shortlisted_models <- validated_models %>%
  dplyr::filter(
    shortlist_status ==
      "Shortlist",
    acceptable_cluster_size,
    minimum_cluster_share >=
      minimum_acceptable_cluster_share
  ) %>%
  dplyr::arrange(
    validation_rank
  )

if (nrow(shortlisted_models) == 0) {
  stop(
    "No shortlisted models are available."
  )
}

best_validation_score <- max(
  shortlisted_models$validation_score,
  na.rm = TRUE
)

candidate_models <- shortlisted_models %>%
  dplyr::filter(
    validation_score >=
      best_validation_score -
      validation_score_tolerance
  ) %>%
  dplyr::slice_head(
    n = maximum_candidate_models
  )

if (nrow(candidate_models) == 0) {
  candidate_models <- shortlisted_models %>%
    dplyr::slice_head(
      n = min(
        maximum_candidate_models,
        nrow(shortlisted_models)
      )
    )
}

readr::write_csv(
  candidate_models,
  file.path(
    tables_dir,
    "10_candidate_models.csv"
  )
)

write_log(
  "Candidate models selected: ",
  nrow(candidate_models),
  ".",
  log_file = log_file
)


# =============================================================================
# 7. Extract assignments
# =============================================================================

candidate_assignments <- list()

for (row_index in seq_len(nrow(candidate_models))) {

  candidate <- candidate_models[
    row_index,
    ,
    drop = FALSE
  ]

  candidate_assignments[[candidate$model_id]] <- extract_assignment(
    assignments_object =
      cluster_assignments,
    feature_set =
      candidate$feature_set,
    method =
      candidate$method,
    distance_method =
      candidate$distance_method,
    k =
      candidate$k
  )
}

saveRDS(
  candidate_assignments,
  file.path(
    models_dir,
    "10_candidate_assignments.rds"
  )
)


# =============================================================================
# 8. Pairwise Adjusted Rand Index
# =============================================================================

candidate_model_ids <- names(
  candidate_assignments
)

pairwise_ari <- purrr::map_dfr(
  seq_along(candidate_model_ids),
  function(index_1) {

    purrr::map_dfr(
      seq_along(candidate_model_ids),
      function(index_2) {

        model_1 <- candidate_model_ids[
          index_1
        ]

        model_2 <- candidate_model_ids[
          index_2
        ]

        assignments_1 <-
          candidate_assignments[[model_1]]

        assignments_2 <-
          candidate_assignments[[model_2]]

        joined <- assignments_1 %>%
          dplyr::rename(
            cluster_1 = cluster
          ) %>%
          dplyr::inner_join(
            assignments_2 %>%
              dplyr::rename(
                cluster_2 = cluster
              ),
            by = identifier_variable
          )

        tibble::tibble(
          model_1 = model_1,
          model_2 = model_2,
          n_compared = nrow(joined),
          adjusted_rand_index =
            adjusted_rand_index(
              joined$cluster_1,
              joined$cluster_2
            )
        )
      }
    )
  }
)

readr::write_csv(
  pairwise_ari,
  file.path(
    tables_dir,
    "10_pairwise_adjusted_rand.csv"
  )
)


# =============================================================================
# 9. Segmentation profiles
# =============================================================================

segmentation_profiles <- list()

for (row_index in seq_len(nrow(candidate_models))) {

  candidate <- candidate_models[
    row_index,
    ,
    drop = FALSE
  ]

  model_data <-
    candidate_feature_sets[[candidate$feature_set]]

  segmentation_profiles[[candidate$model_id]] <- cluster_profile_long(
    model_data = model_data,
    assignment_table =
      candidate_assignments[[candidate$model_id]],
    model_id =
      candidate$model_id,
    metadata =
      variable_metadata
  )
}

segmentation_profiles_long <-
  dplyr::bind_rows(
    segmentation_profiles
  )

readr::write_csv(
  segmentation_profiles_long,
  file.path(
    tables_dir,
    "10_segmentation_profiles_long.csv"
  )
)

profile_distinctness <-
  profile_distinctness_summary(
    segmentation_profiles_long
  )

readr::write_csv(
  profile_distinctness,
  file.path(
    tables_dir,
    "10_profile_distinctness.csv"
  )
)


# =============================================================================
# 10. Profiling-variable associations
# =============================================================================

profiling_associations <- purrr::map_dfr(
  candidate_model_ids,
  function(model_id) {

    profiling_associations_for_model(
      profiling_data =profiling_data,
      assignment_table =
        candidate_assignments[[model_id]],
      model_id = model_id,
      metadata =
        variable_metadata
    )
  }
) %>%
  dplyr::group_by(
    model_id
  ) %>%
  dplyr::arrange(
    dplyr::desc(
      association_value
    ),
    .by_group = TRUE
  ) %>%
  dplyr::mutate(
    association_rank =
      dplyr::row_number()
  ) %>%
  dplyr::ungroup()

readr::write_csv(
  profiling_associations,
  file.path(
    tables_dir,
    "10_profiling_associations.csv"
  )
)


# =============================================================================
# 11. Candidate profile scores
# =============================================================================

segmentation_distinctness_summary <-
  profile_distinctness %>%
  dplyr::group_by(
    model_id
  ) %>%
  dplyr::summarise(
    mean_standardised_range =
      mean(
        standardised_range,
        na.rm = TRUE
      ),
    median_standardised_range =
      stats::median(
        standardised_range,
        na.rm = TRUE
      ),
    maximum_standardised_range =
      max(
        standardised_range,
        na.rm = TRUE
      ),
    mean_maximum_absolute_deviation =
      mean(
        maximum_absolute_deviation,
        na.rm = TRUE
      ),
    .groups = "drop"
  )

profiling_distinctness_summary <-
  profiling_associations %>%
  dplyr::filter(
    association_rank <=
      maximum_profiling_associations_per_model
  ) %>%
  dplyr::group_by(
    model_id
  ) %>%
  dplyr::summarise(
    mean_profiling_association =
      mean(
        association_value,
        na.rm = TRUE
      ),
    maximum_profiling_association =
      max(
        association_value,
        na.rm = TRUE
      ),
    .groups = "drop"
  )

candidate_profile_scores <- candidate_models %>%
  dplyr::left_join(
    segmentation_distinctness_summary,
    by = "model_id"
  ) %>%
  dplyr::left_join(
    profiling_distinctness_summary,
    by = "model_id"
  ) %>%
  dplyr::mutate(
    validation_component =
      normalise_01(
        validation_score
      ),
    segmentation_distinctness_component =
      normalise_01(
        mean_standardised_range
      ),
    profiling_distinctness_component =
      normalise_01(
        mean_profiling_association
      ),
    cluster_balance_component =
      normalise_01(
        balance_score
      ),
    profile_comparison_score =
      weight_validation_score *
        validation_component +
      weight_segmentation_distinctness *
        segmentation_distinctness_component +
      weight_profiling_distinctness *
        profiling_distinctness_component +
      weight_cluster_balance *
        cluster_balance_component,
    profile_comparison_rank = rank(
      -profile_comparison_score,
      ties.method = "first"
    )
  ) %>%
  dplyr::arrange(
    profile_comparison_rank
  )

readr::write_csv(
  candidate_profile_scores,
  file.path(
    tables_dir,
    "10_candidate_profile_scores.csv"
  )
)


# =============================================================================
# 12. Candidate summary and final model selection
# =============================================================================

mean_pairwise_ari <- pairwise_ari %>%
  dplyr::filter(
    model_1 != model_2
  ) %>%
  dplyr::group_by(
    model_1
  ) %>%
  dplyr::summarise(
    mean_ari_with_other_candidates =
      mean(
        adjusted_rand_index,
        na.rm = TRUE
      ),
    minimum_ari_with_other_candidates =
      min(
        adjusted_rand_index,
        na.rm = TRUE
      ),
    .groups = "drop"
  ) %>%
  dplyr::rename(
    model_id = model_1
  )

candidate_profile_summary <-
  candidate_profile_scores %>%
  dplyr::left_join(
    mean_pairwise_ari,
    by = "model_id"
  ) %>%
  dplyr::select(
    profile_comparison_rank,
    model_id,
    feature_set,
    feature_count,
    method,
    distance_method,
    k,
    validation_score,
    mean_jaccard,
    average_silhouette,
    minimum_cluster_share,
    balance_score,
    mean_standardised_range,
    mean_profiling_association,
    mean_ari_with_other_candidates,
    minimum_ari_with_other_candidates,
    profile_comparison_score
  )

readr::write_csv(
  candidate_profile_summary,
  file.path(
    tables_dir,
    "10_candidate_profile_summary.csv"
  )
)



# =============================================================================
# 13. Select and save final model
# =============================================================================

final_model <- candidate_profile_summary %>%
  dplyr::slice_min(
    order_by = profile_comparison_rank,
    n = 1,
    with_ties = FALSE
  )

final_model_id <- final_model$model_id[1]
final_feature_set <- final_model$feature_set[1]

final_assignment <- candidate_assignments[[final_model_id]]

final_feature_data <- candidate_feature_sets[[final_feature_set]]

final_segmentation_data <- final_feature_data %>%
  dplyr::left_join(
    final_assignment,
    by = identifier_variable
  ) %>%
  dplyr::rename(
    final_segment = cluster
  )

final_model_metadata <- final_model %>%
  dplyr::mutate(
    selected_as_final_model = TRUE,
    selection_basis = paste(
      "Highest profile comparison score after validation,",
      "bootstrap stability, profile distinctness, profiling association,",
      "cluster balance and cross-model agreement assessment."
    )
  )

final_model_summary <- tibble::tibble(
  item = c(
    "Final model ID",
    "Feature set",
    "Feature count",
    "Method",
    "Distance method",
    "Number of segments",
    "Validation score",
    "Mean Jaccard stability",
    "Average silhouette",
    "Minimum cluster share",
    "Balance score",
    "Mean profile distinctness",
    "Mean profiling association",
    "Mean ARI with other candidates",
    "Profile comparison score"
  ),
  value = c(
    final_model$model_id,
    final_model$feature_set,
    final_model$feature_count,
    final_model$method,
    final_model$distance_method,
    final_model$k,
    round(final_model$validation_score, 6),
    round(final_model$mean_jaccard, 6),
    round(final_model$average_silhouette, 6),
    round(final_model$minimum_cluster_share, 6),
    round(final_model$balance_score, 6),
    round(final_model$mean_standardised_range, 6),
    round(final_model$mean_profiling_association, 6),
    round(final_model$mean_ari_with_other_candidates, 6),
    round(final_model$profile_comparison_score, 6)
  )
)

saveRDS(
  final_assignment,
  file.path(
    models_dir,
    "10_final_model_assignment.rds"
  )
)

saveRDS(
  final_segmentation_data,
  file.path(
    data_processed_dir,
    "10_final_segmentation_data.rds"
  )
)

saveRDS(
  final_model_metadata,
  file.path(
    data_processed_dir,
    "10_final_model_metadata.rds"
  )
)

readr::write_csv(
  final_model_summary,
  file.path(
    tables_dir,
    "10_final_model_summary.csv"
  )
)


# =============================================================================
# 14. Figures
# =============================================================================


ari_plot <- ggplot2::ggplot(
  pairwise_ari,
  ggplot2::aes(
    x = model_1,
    y = model_2,
    fill = adjusted_rand_index
  )
) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(
    ggplot2::aes(
      label = round(
        adjusted_rand_index,
        2
      )
    ),
    size = 3
  ) +
  ggplot2::scale_fill_gradient(
    limits = c(-1, 1)
  ) +
  ggplot2::labs(
    title = "Agreement between candidate segmentations",
    subtitle = "Adjusted Rand Index",
    x = NULL,
    y = NULL,
    fill = "ARI"
  ) +
  ggplot2::theme_minimal(
    base_size = 9
  ) +
  ggplot2::theme(
    plot.title =
      ggplot2::element_text(
        face = "bold"
      ),
    axis.text.x =
      ggplot2::element_text(
        angle = 60,
        hjust = 1
      ),
    panel.grid =
      ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "10_pairwise_ari_heatmap.png"
  ),
  plot = ari_plot,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

profile_heatmap_data <-
  segmentation_profiles_long %>%
  dplyr::mutate(
    display_label = dplyr::coalesce(
      label,
      variable
    ),
    cluster_label = paste0(
      "Cluster ",
      cluster
    )
  )

profile_heatmap <- ggplot2::ggplot(
  profile_heatmap_data,
  ggplot2::aes(
    x = cluster_label,
    y = display_label,
    fill = standardised_mean
  )
) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient2(
    midpoint = 0
  ) +
  ggplot2::facet_wrap(
    ~ model_id,
    scales = "free_y",
    ncol = 2
  ) +
  ggplot2::labs(
    title = "Candidate segmentation profiles",
    subtitle = "Cluster means standardised relative to the full sample",
    x = NULL,
    y = NULL,
    fill = "Standardised mean"
  ) +
  ggplot2::theme_minimal(
    base_size = 9
  ) +
  ggplot2::theme(
    plot.title =
      ggplot2::element_text(
        face = "bold"
      ),
    axis.text.x =
      ggplot2::element_text(
        angle = 45,
        hjust = 1
      ),
    panel.grid =
      ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "10_candidate_profile_heatmaps.png"
  ),
  plot = profile_heatmap,
  width = 13,
  height = max(
    9,
    3.5 *
      ceiling(
        length(candidate_model_ids) /
          2
      )
  ),
  dpi = 300,
  bg = "white"
)

distinctness_plot_data <-
  candidate_profile_scores %>%
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
      mean_standardised_range
    )
  )

distinctness_plot <- ggplot2::ggplot(
  distinctness_plot_data,
  ggplot2::aes(
    x = display_label,
    y = mean_standardised_range
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Segmentation-profile distinctness",
    subtitle = "Mean range of standardised cluster means across segmentation variables",
    x = NULL,
    y = "Mean standardised range"
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
      ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "10_profile_distinctness.png"
  ),
  plot = distinctness_plot,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# =============================================================================
# 15. Final report
# =============================================================================

top_candidate <- final_model

capture.output(
  sessionInfo(),
  file = file.path(
    logs_dir,
    "10_sessionInfo.txt"
  )
)

write_log(
  "Candidate profile comparison completed successfully.",
  log_file = log_file
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — FINAL MODEL SELECTION COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Candidate models compared:      ",
  nrow(candidate_models),
  "\n",
  "Pairwise ARI comparisons:       ",
  sum(
    pairwise_ari$model_1 !=
      pairwise_ari$model_2
  ) / 2,
  "\n",
  "Final segmentation model:       ",
  top_candidate$model_id,
  "\n",
  "Validation score:               ",
  round(
    top_candidate$validation_score,
    3
  ),
  "\n",
  "Mean profile distinctness:      ",
  round(
    top_candidate$
      mean_standardised_range,
    3
  ),
  "\n",
  "Profile comparison score:       ",
  round(
    top_candidate$
      profile_comparison_score,
    3
  ),
  "\n",
  "\nFinal model summary:\n",
  file.path(
    tables_dir,
    "10_final_model_summary.csv"
  ),
  "\n",
  "Final segmentation data:\n",
  file.path(
    data_processed_dir,
    "10_final_segmentation_data.rds"
  ),
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)
