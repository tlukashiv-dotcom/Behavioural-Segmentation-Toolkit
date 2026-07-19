# =============================================================================
# 06_feature_selection.R
# Behavioural Segmentation Toolkit
#
# Purpose:
#   - Rank quality-filtered segmentation variables
#   - Generate multiple candidate feature subsets
#   - Preserve behavioural-group coverage
#   - Support optional force_keep and force_exclude metadata fields
#   - Save all subsets for downstream clustering model comparison
#
# Important:
#   This script does NOT select one final feature set.
#   Final selection must be based on downstream clustering quality, stability,
#   cluster-size balance and interpretability.
#
# Expected inputs:
#   data/processed/segmentation_selected_imputed.rds
#   data/processed/05_selected_variable_metadata.rds
#
# Main outputs:
#   data/processed/06_candidate_feature_sets.rds
#   data/processed/06_candidate_feature_metadata.rds
#   data/processed/06_feature_scores.rds
#   outputs/tables/06_feature_scores.csv
#   outputs/tables/06_candidate_feature_sets.csv
#   outputs/tables/06_candidate_feature_set_summary.csv
#   outputs/tables/06_group_coverage_by_feature_set.csv
#   outputs/tables/06_feature_selection_summary.csv
#   outputs/figures/06_feature_importance.png
#   outputs/figures/06_candidate_feature_set_sizes.png
#   outputs/logs/06_feature_selection.log
# =============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  warn = 1,
  scipen = 999
)

required_packages <- c(
  "dplyr",
  "readr",
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

identifier_variable <- "respondent_id"

candidate_feature_counts <- c(
  23,
  20,
  18,
  15,
  12
)

pca_cumulative_variance_target <- 0.80
bootstrap_iterations <- 100
minimum_features_per_group <- 1

weight_pca <- 0.50
weight_entropy <- 0.20
weight_stability <- 0.20
weight_uniqueness <- 0.10

random_seed <- 123


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


normalised_entropy <- function(x) {

  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  probabilities <- prop.table(
    table(x)
  )

  if (length(probabilities) <= 1) {
    return(0)
  }

  -sum(
    probabilities * log(probabilities)
  ) / log(length(probabilities))
}


parse_optional_logical <- function(
    metadata,
    column_name,
    default = FALSE
) {

  if (!column_name %in% names(metadata)) {
    return(rep(default, nrow(metadata)))
  }

  x <- metadata[[column_name]]

  if (is.logical(x)) {
    x[is.na(x)] <- default
    return(x)
  }

  cleaned <- tolower(
    trimws(
      as.character(x)
    )
  )

  result <- dplyr::case_when(
    cleaned %in% c("true", "t", "yes", "y", "1") ~ TRUE,
    cleaned %in% c("false", "f", "no", "n", "0") ~ FALSE,
    is.na(cleaned) | cleaned == "" ~ default,
    TRUE ~ NA
  )

  if (any(is.na(result))) {
    stop(
      "Invalid values in optional metadata column '",
      column_name,
      "': ",
      paste(
        unique(cleaned[is.na(result)]),
        collapse = ", "
      )
    )
  }

  result
}


calculate_pca_importance <- function(
    scaled_data,
    cumulative_target
) {

  model <- stats::prcomp(
    scaled_data,
    center = FALSE,
    scale. = FALSE
  )

  variance_explained <- (
    model$sdev^2
  ) / sum(
    model$sdev^2
  )

  cumulative_variance <- cumsum(
    variance_explained
  )

  number_components <- which(
    cumulative_variance >= cumulative_target
  )[1]

  if (is.na(number_components)) {
    number_components <- length(
      variance_explained
    )
  }

  retained_loadings <- abs(
    model$rotation[
      ,
      seq_len(number_components),
      drop = FALSE
    ]
  )

  retained_variance <- variance_explained[
    seq_len(number_components)
  ]

  weighted_importance <- retained_loadings %*%
    retained_variance

  list(
    scores = tibble::tibble(
      variable = rownames(
        model$rotation
      ),
      pca_importance = as.numeric(
        weighted_importance
      )
    ),
    model = model,
    number_components = number_components,
    cumulative_variance = cumulative_variance[
      number_components
    ]
  )
}


calculate_bootstrap_stability <- function(
    scaled_data,
    reference_scores,
    iterations,
    seed
) {

  set.seed(seed)

  variables <- colnames(
    scaled_data
  )

  bootstrap_scores <- matrix(
    NA_real_,
    nrow = iterations,
    ncol = length(variables),
    dimnames = list(
      NULL,
      variables
    )
  )

  for (iteration in seq_len(iterations)) {

    indices <- sample(
      seq_len(nrow(scaled_data)),
      size = nrow(scaled_data),
      replace = TRUE
    )

    sample_data <- scaled_data[
      indices,
      ,
      drop = FALSE
    ]

    sample_result <- calculate_pca_importance(
      sample_data,
      pca_cumulative_variance_target
    )$scores

    bootstrap_scores[
      iteration,
      sample_result$variable
    ] <- sample_result$pca_importance
  }

  reference_vector <- reference_scores$pca_importance[
    match(
      variables,
      reference_scores$variable
    )
  ]

  reference_rank <- rank(
    -reference_vector,
    ties.method = "average"
  )

  rank_matrix <- t(
    apply(
      bootstrap_scores,
      1,
      function(x) {
        rank(
          -x,
          ties.method = "average"
        )
      }
    )
  )

  mean_rank <- colMeans(
    rank_matrix,
    na.rm = TRUE
  )

  rank_deviation <- abs(
    mean_rank - reference_rank
  )

  tibble::tibble(
    variable = variables,
    bootstrap_mean_importance = colMeans(
      bootstrap_scores,
      na.rm = TRUE
    ),
    bootstrap_sd_importance = apply(
      bootstrap_scores,
      2,
      stats::sd,
      na.rm = TRUE
    ),
    bootstrap_mean_rank = mean_rank,
    stability_score = 1 -
      normalise_01(rank_deviation)
  )
}


build_feature_set <- function(
    target_count,
    feature_scores,
    feature_metadata,
    minimum_per_group
) {

  eligible_scores <- feature_scores %>%
    dplyr::filter(
      !force_exclude
    )

  forced_keep <- eligible_scores %>%
    dplyr::filter(
      force_keep
    ) %>%
    dplyr::pull(variable)

  target_count <- max(
    target_count,
    length(forced_keep)
  )

  target_count <- min(
    target_count,
    nrow(eligible_scores)
  )

  selected <- forced_keep

  groups <- eligible_scores %>%
    dplyr::distinct(
      group
    ) %>%
    dplyr::pull(group)

  for (group_name in groups) {

    current_group_count <- feature_metadata %>%
      dplyr::filter(
        variable %in% selected,
        group == group_name
      ) %>%
      nrow()

    number_needed <- max(
      0,
      minimum_per_group -
        current_group_count
    )

    if (number_needed > 0) {

      group_candidates <- eligible_scores %>%
        dplyr::filter(
          group == group_name,
          !variable %in% selected
        ) %>%
        dplyr::arrange(
          dplyr::desc(combined_score)
        ) %>%
        dplyr::slice_head(
          n = number_needed
        ) %>%
        dplyr::pull(variable)

      selected <- c(
        selected,
        group_candidates
      )
    }
  }

  selected <- unique(selected)

  if (length(selected) < target_count) {

    additional <- eligible_scores %>%
      dplyr::filter(
        !variable %in% selected
      ) %>%
      dplyr::arrange(
        dplyr::desc(combined_score)
      ) %>%
      dplyr::slice_head(
        n = target_count -
          length(selected)
      ) %>%
      dplyr::pull(variable)

    selected <- c(
      selected,
      additional
    )
  }

  while (length(selected) > target_count) {

    removable <- eligible_scores %>%
      dplyr::filter(
        variable %in% selected,
        !force_keep
      ) %>%
      dplyr::arrange(
        combined_score
      )

    removed <- FALSE

    for (candidate in removable$variable) {

      candidate_group <- feature_metadata$group[
        match(
          candidate,
          feature_metadata$variable
        )
      ]

      remaining_group_count <- feature_metadata %>%
        dplyr::filter(
          variable %in%
            setdiff(selected, candidate),
          group == candidate_group
        ) %>%
        nrow()

      if (
        remaining_group_count >=
          minimum_per_group
      ) {
        selected <- setdiff(
          selected,
          candidate
        )

        removed <- TRUE
        break
      }
    }

    if (!removed) {
      break
    }
  }

  unique(selected)
}


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
      data_processed_dir,
      tables_dir,
      figures_dir,
      models_dir,
      logs_dir
    ),
    create_directory
  )
)

input_file <- file.path(
  data_processed_dir,
  "segmentation_selected_imputed.rds"
)

metadata_file <- file.path(
  data_processed_dir,
  "05_selected_variable_metadata.rds"
)

log_file <- file.path(
  logs_dir,
  "06_feature_selection.log"
)

if (file.exists(log_file)) {
  file.remove(log_file)
}

if (!file.exists(input_file)) {
  stop(
    "Quality-filtered segmentation dataset not found: ",
    input_file,
    "\nRun 05_feature_selection.R first."
  )
}

if (!file.exists(metadata_file)) {
  stop(
    "Selected variable metadata not found: ",
    metadata_file,
    "\nRun 05_feature_selection.R first."
  )
}

segmentation_data <- readRDS(
  input_file
) %>%
  tibble::as_tibble()

variable_metadata <- readRDS(
  metadata_file
) %>%
  tibble::as_tibble()

write_log(
  "Input data loaded: ",
  nrow(segmentation_data),
  " rows and ",
  ncol(segmentation_data),
  " columns.",
  log_file = log_file
)

if (!identifier_variable %in%
    names(segmentation_data)) {
  stop(
    "Identifier variable not found: ",
    identifier_variable
  )
}

candidate_variables <- setdiff(
  names(segmentation_data),
  identifier_variable
)

non_numeric_variables <- candidate_variables[
  !vapply(
    segmentation_data[candidate_variables],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_variables) > 0) {
  stop(
    "Non-numeric candidate features found: ",
    paste(
      non_numeric_variables,
      collapse = ", "
    )
  )
}

missing_metadata <- setdiff(
  candidate_variables,
  variable_metadata$variable
)

if (length(missing_metadata) > 0) {
  stop(
    "Candidate variables missing from metadata: ",
    paste(
      missing_metadata,
      collapse = ", "
    )
  )
}

force_keep_values <- parse_optional_logical(
  variable_metadata,
  "force_keep",
  FALSE
)

force_exclude_values <- parse_optional_logical(
  variable_metadata,
  "force_exclude",
  FALSE
)

variable_metadata <- variable_metadata %>%
  dplyr::filter(
    variable %in% candidate_variables
  ) %>%
  dplyr::mutate(
    force_keep = force_keep_values[
      match(
        variable,
        variable_metadata$variable
      )
    ],
    force_exclude = force_exclude_values[
      match(
        variable,
        variable_metadata$variable
      )
    ]
  )

conflicts <- variable_metadata %>%
  dplyr::filter(
    force_keep &
      force_exclude
  )

if (nrow(conflicts) > 0) {
  stop(
    "Variables cannot be both force_keep and force_exclude: ",
    paste(
      conflicts$variable,
      collapse = ", "
    )
  )
}

model_data <- segmentation_data %>%
  dplyr::select(
    dplyr::all_of(candidate_variables)
  )

model_scaled <- scale(
  model_data
)

if (any(!is.finite(model_scaled))) {
  stop(
    "Non-finite values were produced during standardisation."
  )
}

pca_result <- calculate_pca_importance(
  scaled_data = model_scaled,
  cumulative_target =
    pca_cumulative_variance_target
)

bootstrap_stability <- calculate_bootstrap_stability(
  scaled_data = model_scaled,
  reference_scores = pca_result$scores,
  iterations = bootstrap_iterations,
  seed = random_seed
)

univariate_scores <- purrr::map_dfr(
  candidate_variables,
  function(variable_name) {

    x <- model_data[[variable_name]]

    tibble::tibble(
      variable = variable_name,
      entropy = normalised_entropy(x),
      n_unique = dplyr::n_distinct(
        x,
        na.rm = TRUE
      )
    )
  }
)

feature_scores <- variable_metadata %>%
  dplyr::select(
    variable,
    label,
    group,
    data_type,
    force_keep,
    force_exclude
  ) %>%
  dplyr::left_join(
    pca_result$scores,
    by = "variable"
  ) %>%
  dplyr::left_join(
    bootstrap_stability,
    by = "variable"
  ) %>%
  dplyr::left_join(
    univariate_scores,
    by = "variable"
  ) %>%
  dplyr::mutate(
    pca_score = normalise_01(
      pca_importance
    ),
    entropy_score = normalise_01(
      entropy
    ),
    uniqueness_score = normalise_01(
      log1p(n_unique)
    ),
    combined_score =
      weight_pca * pca_score +
      weight_entropy * entropy_score +
      weight_stability * stability_score +
      weight_uniqueness * uniqueness_score,
    overall_rank = rank(
      -combined_score,
      ties.method = "first"
    )
  ) %>%
  dplyr::arrange(
    overall_rank
  )

readr::write_csv(
  feature_scores,
  file.path(
    tables_dir,
    "06_feature_scores.csv"
  )
)

saveRDS(
  pca_result$model,
  file.path(
    models_dir,
    "06_feature_ranking_pca.rds"
  )
)

available_feature_count <- feature_scores %>%
  dplyr::filter(
    !force_exclude
  ) %>%
  nrow()

candidate_counts <- candidate_feature_counts[
  candidate_feature_counts <=
    available_feature_count
]

candidate_counts <- unique(
  c(
    available_feature_count,
    candidate_counts
  )
)

candidate_counts <- sort(
  candidate_counts,
  decreasing = TRUE
)

candidate_feature_sets <- list()

for (feature_count in candidate_counts) {

  feature_set_name <- paste0(
    "set_",
    feature_count
  )

  selected_variables <- build_feature_set(
    target_count = feature_count,
    feature_scores = feature_scores,
    feature_metadata = variable_metadata,
    minimum_per_group =
      minimum_features_per_group
  )

  selected_variables <- feature_scores %>%
    dplyr::filter(
      variable %in% selected_variables
    ) %>%
    dplyr::arrange(
      overall_rank
    ) %>%
    dplyr::pull(variable)

  candidate_feature_sets[[feature_set_name]] <-
    segmentation_data %>%
    dplyr::select(
      dplyr::all_of(identifier_variable),
      dplyr::all_of(selected_variables)
    )
}

candidate_feature_metadata <- purrr::imap_dfr(
  candidate_feature_sets,
  function(feature_data, feature_set_name) {

    selected_variables <- setdiff(
      names(feature_data),
      identifier_variable
    )

    feature_scores %>%
      dplyr::filter(
        variable %in%
          selected_variables
      ) %>%
      dplyr::mutate(
        feature_set = feature_set_name,
        feature_count =
          length(selected_variables),
        within_set_rank = rank(
          -combined_score,
          ties.method = "first"
        )
      )
  }
)

candidate_feature_sets_long <- candidate_feature_metadata %>%
  dplyr::select(
    feature_set,
    feature_count,
    variable,
    label,
    group,
    data_type,
    combined_score,
    overall_rank,
    within_set_rank,
    force_keep
  ) %>%
  dplyr::arrange(
    dplyr::desc(feature_count),
    within_set_rank
  )

readr::write_csv(
  candidate_feature_sets_long,
  file.path(
    tables_dir,
    "06_candidate_feature_sets.csv"
  )
)

group_coverage <- candidate_feature_metadata %>%
  dplyr::count(
    feature_set,
    feature_count,
    group,
    name = "selected_features"
  ) %>%
  dplyr::left_join(
    variable_metadata %>%
      dplyr::filter(
        !force_exclude
      ) %>%
      dplyr::count(
        group,
        name = "available_features"
      ),
    by = "group"
  ) %>%
  dplyr::mutate(
    retention_pct = round(
      100 * selected_features /
        available_features,
      1
    )
  ) %>%
  dplyr::arrange(
    dplyr::desc(feature_count),
    group
  )

readr::write_csv(
  group_coverage,
  file.path(
    tables_dir,
    "06_group_coverage_by_feature_set.csv"
  )
)

feature_set_summary <- purrr::imap_dfr(
  candidate_feature_sets,
  function(feature_data, feature_set_name) {

    selected_variables <- setdiff(
      names(feature_data),
      identifier_variable
    )

    selected_scores <- feature_scores %>%
      dplyr::filter(
        variable %in%
          selected_variables
      )

    tibble::tibble(
      feature_set = feature_set_name,
      feature_count =
        length(selected_variables),
      groups_represented =
        dplyr::n_distinct(
          selected_scores$group
        ),
      mean_combined_score = mean(
        selected_scores$combined_score
      ),
      minimum_combined_score = min(
        selected_scores$combined_score
      ),
      forced_keep_features = sum(
        selected_scores$force_keep
      )
    )
  }
) %>%
  dplyr::arrange(
    dplyr::desc(feature_count)
  )

readr::write_csv(
  feature_set_summary,
  file.path(
    tables_dir,
    "06_candidate_feature_set_summary.csv"
  )
)

saveRDS(
  candidate_feature_sets,
  file.path(
    data_processed_dir,
    "06_candidate_feature_sets.rds"
  )
)

saveRDS(
  candidate_feature_metadata,
  file.path(
    data_processed_dir,
    "06_candidate_feature_metadata.rds"
  )
)

saveRDS(
  feature_scores,
  file.path(
    data_processed_dir,
    "06_feature_scores.rds"
  )
)

importance_plot_data <- feature_scores %>%
  dplyr::slice_max(
    order_by = combined_score,
    n = min(
      25,
      nrow(feature_scores)
    ),
    with_ties = FALSE
  ) %>%
  dplyr::mutate(
    display_label = dplyr::coalesce(
      label,
      variable
    ),
    display_label = reorder(
      display_label,
      combined_score
    )
  )

feature_importance_plot <- ggplot2::ggplot(
  importance_plot_data,
  ggplot2::aes(
    x = display_label,
    y = combined_score
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Candidate feature ranking",
    subtitle = paste0(
      "Ranking is used to generate candidate subsets; ",
      "it does not define the final feature set"
    ),
    x = NULL,
    y = "Combined feature score"
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold"
    ),
    panel.grid.major.y =
      ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "06_feature_importance.png"
  ),
  plot = feature_importance_plot,
  width = 10,
  height = max(
    7,
    0.30 *
      nrow(importance_plot_data)
  ),
  dpi = 300,
  bg = "white"
)

feature_set_size_plot <- ggplot2::ggplot(
  feature_set_summary,
  ggplot2::aes(
    x = reorder(
      feature_set,
      feature_count
    ),
    y = feature_count
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::geom_text(
    ggplot2::aes(
      label = feature_count
    ),
    hjust = -0.15
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(
      mult = c(0, 0.15)
    )
  ) +
  ggplot2::labs(
    title = "Candidate feature sets",
    subtitle = "Each subset will be evaluated during clustering model comparison",
    x = NULL,
    y = "Number of features"
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold"
    ),
    panel.grid.major.y =
      ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "06_candidate_feature_set_sizes.png"
  ),
  plot = feature_set_size_plot,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

feature_selection_summary <- tibble::tibble(
  item = c(
    "Rows",
    "Candidate features",
    "Eligible features",
    "Candidate feature sets",
    "Largest feature set",
    "Smallest feature set",
    "Behavioural groups",
    "Bootstrap iterations",
    "PCA cumulative variance target",
    "Forced keep features",
    "Forced exclude features"
  ),
  value = c(
    nrow(segmentation_data),
    length(candidate_variables),
    available_feature_count,
    length(candidate_feature_sets),
    max(feature_set_summary$feature_count),
    min(feature_set_summary$feature_count),
    dplyr::n_distinct(
      variable_metadata$group
    ),
    bootstrap_iterations,
    pca_cumulative_variance_target,
    sum(variable_metadata$force_keep),
    sum(variable_metadata$force_exclude)
  )
)

readr::write_csv(
  feature_selection_summary,
  file.path(
    tables_dir,
    "06_feature_selection_summary.csv"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    logs_dir,
    "06_sessionInfo.txt"
  )
)

write_log(
  "Candidate feature sets generated successfully.",
  log_file = log_file
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — CANDIDATE FEATURE SETS COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Rows:                          ",
  nrow(segmentation_data),
  "\n",
  "Candidate features:            ",
  length(candidate_variables),
  "\n",
  "Eligible features:             ",
  available_feature_count,
  "\n",
  "Candidate feature sets:        ",
  length(candidate_feature_sets),
  "\n",
  "Feature-set sizes:             ",
  paste(
    feature_set_summary$feature_count,
    collapse = ", "
  ),
  "\n",
  "Behavioural groups represented:",
  dplyr::n_distinct(
    variable_metadata$group
  ),
  "\n",
  "\nMain output:\n",
  file.path(
    data_processed_dir,
    "06_candidate_feature_sets.rds"
  ),
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)
