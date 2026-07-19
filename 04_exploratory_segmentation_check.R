# =============================================================================
# 04_exploratory_segmentation_check.R
# Behavioural Segmentation Toolkit
#
# Purpose:
#   - Load the full segmentation dataset
#   - Inspect missingness and variable distributions
#   - Create a temporary exploratory imputation
#   - Identify highly correlated and low-variance variables
#   - Run PCA as a diagnostic
#   - Compare exploratory k-means solutions
#   - Compare exploratory PAM solutions using Gower distance
#   - Produce diagnostic tables and figures
#
# Important:
#   This script is exploratory only.
#   It does not select the final segmentation variables or final model.
#
# Expected inputs:
#   data/processed/segmentation_data_full.rds
#   data/processed/02_variable_metadata.rds
#
# Main outputs:
#   data/processed/04_segmentation_exploratory_imputed.rds
#   outputs/tables/04_missingness_summary.csv
#   outputs/tables/04_variable_distribution_summary.csv
#   outputs/tables/04_low_variance_variables.csv
#   outputs/tables/04_high_correlation_pairs.csv
#   outputs/tables/04_pca_variance_explained.csv
#   outputs/tables/04_pca_loadings.csv
#   outputs/tables/04_kmeans_quality.csv
#   outputs/tables/04_pam_gower_quality.csv
#   outputs/figures/04_missingness_by_variable.png
#   outputs/figures/04_correlation_heatmap.png
#   outputs/figures/04_pca_variance.png
#   outputs/figures/04_pca_projection.png
#   outputs/figures/04_kmeans_quality.png
#   outputs/figures/04_pam_gower_quality.png
#   outputs/logs/04_exploratory_segmentation_check.log
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
  "stringr",
  "purrr",
  "tibble",
  "ggplot2",
  "cluster",
  "scales"
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

random_seed <- 123

high_correlation_threshold <- 0.80

low_variance_unique_threshold <- 1

maximum_variables_in_missingness_plot <- 40

maximum_pca_components_to_plot <- 20

maximum_loading_rows <- 20

gower_sample_size <- 2000


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


safe_median <- function(x) {
  
  if (all(is.na(x))) {
    return(0)
  }
  
  stats::median(
    x,
    na.rm = TRUE
  )
}


impute_numeric_median <- function(x) {
  
  if (!is.numeric(x)) {
    return(x)
  }
  
  replacement <- safe_median(x)
  
  x[is.na(x)] <- replacement
  
  x
}


safe_skewness <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) < 3) {
    return(NA_real_)
  }
  
  standard_deviation <- stats::sd(x)
  
  if (
    is.na(standard_deviation) ||
    standard_deviation == 0
  ) {
    return(NA_real_)
  }
  
  mean(
    (
      (x - mean(x)) /
        standard_deviation
    )^3
  )
}


safe_kurtosis <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) < 4) {
    return(NA_real_)
  }
  
  standard_deviation <- stats::sd(x)
  
  if (
    is.na(standard_deviation) ||
    standard_deviation == 0
  ) {
    return(NA_real_)
  }
  
  mean(
    (
      (x - mean(x)) /
        standard_deviation
    )^4
  ) - 3
}


calculate_entropy <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  probabilities <- prop.table(
    table(x)
  )
  
  entropy <- -sum(
    probabilities *
      log(probabilities)
  )
  
  if (length(probabilities) <= 1) {
    return(0)
  }
  
  entropy /
    log(length(probabilities))
}


summarise_variable <- function(data, variable_name) {
  
  x <- data[[variable_name]]
  
  tibble::tibble(
    variable = variable_name,
    observed_type = class(x)[1],
    n = length(x),
    n_missing = sum(is.na(x)),
    pct_missing = round(
      100 * mean(is.na(x)),
      2
    ),
    n_unique = dplyr::n_distinct(
      x,
      na.rm = TRUE
    ),
    minimum = ifelse(
      is.numeric(x) && any(!is.na(x)),
      min(x, na.rm = TRUE),
      NA_real_
    ),
    maximum = ifelse(
      is.numeric(x) && any(!is.na(x)),
      max(x, na.rm = TRUE),
      NA_real_
    ),
    mean = ifelse(
      is.numeric(x) && any(!is.na(x)),
      mean(x, na.rm = TRUE),
      NA_real_
    ),
    median = ifelse(
      is.numeric(x) && any(!is.na(x)),
      stats::median(x, na.rm = TRUE),
      NA_real_
    ),
    standard_deviation = ifelse(
      is.numeric(x) &&
        sum(!is.na(x)) >= 2,
      stats::sd(x, na.rm = TRUE),
      NA_real_
    ),
    skewness = ifelse(
      is.numeric(x),
      safe_skewness(x),
      NA_real_
    ),
    excess_kurtosis = ifelse(
      is.numeric(x),
      safe_kurtosis(x),
      NA_real_
    ),
    normalised_entropy = calculate_entropy(x)
  )
}


calculate_kmeans_quality <- function(
    scaled_data,
    k,
    distance_matrix
) {
  
  set.seed(
    random_seed + k
  )
  
  model <- stats::kmeans(
    scaled_data,
    centers = k,
    nstart = 100,
    iter.max = 200
  )
  
  silhouette_result <- cluster::silhouette(
    model$cluster,
    distance_matrix
  )
  
  cluster_sizes <- table(
    model$cluster
  )
  
  tibble::tibble(
    method = "K-means",
    k = k,
    total_withinss = model$tot.withinss,
    between_ss_ratio = model$betweenss /
      model$totss,
    average_silhouette = mean(
      silhouette_result[, "sil_width"]
    ),
    minimum_cluster_size = min(
      cluster_sizes
    ),
    maximum_cluster_size = max(
      cluster_sizes
    ),
    minimum_cluster_share = min(
      cluster_sizes
    ) /
      sum(cluster_sizes),
    maximum_cluster_share = max(
      cluster_sizes
    ) /
      sum(cluster_sizes)
  )
}


calculate_pam_quality <- function(
    gower_distance,
    k
) {
  
  pam_model <- cluster::pam(
    gower_distance,
    k = k,
    diss = TRUE
  )
  
  cluster_sizes <- table(
    pam_model$clustering
  )
  
  tibble::tibble(
    method = "PAM Gower",
    k = k,
    objective_value = pam_model$objective[2],
    average_silhouette = pam_model$silinfo$avg.width,
    minimum_cluster_size = min(
      cluster_sizes
    ),
    maximum_cluster_size = max(
      cluster_sizes
    ),
    minimum_cluster_share = min(
      cluster_sizes
    ) /
      sum(cluster_sizes),
    maximum_cluster_share = max(
      cluster_sizes
    ) /
      sum(cluster_sizes)
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

directories <- c(
  data_processed_dir,
  tables_dir,
  figures_dir,
  models_dir,
  logs_dir
)

invisible(
  lapply(
    directories,
    create_directory
  )
)

input_file <- file.path(
  data_processed_dir,
  "segmentation_data_full.rds"
)

metadata_file <- file.path(
  data_processed_dir,
  "02_variable_metadata.rds"
)

log_file <- file.path(
  logs_dir,
  "04_exploratory_segmentation_check.log"
)

if (file.exists(log_file)) {
  file.remove(log_file)
}


# =============================================================================
# 5. Load inputs
# =============================================================================

if (!file.exists(input_file)) {
  stop(
    "Segmentation dataset not found: ",
    input_file,
    "\nRun 02_data_preprocessing.R first."
  )
}

segmentation_data <- readRDS(
  input_file
) %>%
  tibble::as_tibble()

if (file.exists(metadata_file)) {
  
  variable_metadata <- readRDS(
    metadata_file
  ) %>%
    tibble::as_tibble()
  
} else {
  
  variable_metadata <- tibble::tibble(
    variable = names(segmentation_data),
    label = names(segmentation_data),
    group = "Unspecified",
    data_type = "numeric"
  )
  
  warning(
    "02_variable_metadata.rds was not found. ",
    "Generic labels will be used."
  )
}

write_log(
  "Segmentation data loaded: ",
  nrow(segmentation_data),
  " rows and ",
  ncol(segmentation_data),
  " columns.",
  log_file = log_file
)


# =============================================================================
# 6. Validate identifier and modelling variables
# =============================================================================

if (!identifier_variable %in% names(segmentation_data)) {
  stop(
    "Identifier variable not found: ",
    identifier_variable
  )
}

if (
  anyDuplicated(
    segmentation_data[[identifier_variable]]
  ) > 0
) {
  stop(
    "Identifier variable contains duplicate values."
  )
}

model_data <- segmentation_data %>%
  dplyr::select(
    -dplyr::all_of(identifier_variable)
  )

non_numeric_variables <- names(model_data)[
  !vapply(
    model_data,
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_variables) > 0) {
  stop(
    "The segmentation dataset contains non-numeric variables: ",
    paste(non_numeric_variables, collapse = ", "),
    "\nReview config/variable_metadata.csv and rerun preprocessing."
  )
}

write_log(
  "Modelling variables: ",
  ncol(model_data),
  log_file = log_file
)


# =============================================================================
# 7. Missingness diagnostics
# =============================================================================

missingness_summary <- tibble::tibble(
  variable = names(model_data),
  n_missing = purrr::map_int(
    model_data,
    ~ sum(is.na(.x))
  ),
  pct_missing = round(
    100 * purrr::map_dbl(
      model_data,
      ~ mean(is.na(.x))
    ),
    2
  )
) %>%
  dplyr::left_join(
    variable_metadata %>%
      dplyr::select(
        variable,
        label,
        group,
        data_type
      ),
    by = "variable"
  ) %>%
  dplyr::arrange(
    dplyr::desc(pct_missing),
    variable
  )

readr::write_csv(
  missingness_summary,
  file.path(
    tables_dir,
    "04_missingness_summary.csv"
  )
)

missingness_plot_data <- missingness_summary %>%
  dplyr::filter(
    pct_missing > 0
  ) %>%
  dplyr::slice_max(
    order_by = pct_missing,
    n = maximum_variables_in_missingness_plot,
    with_ties = FALSE
  ) %>%
  dplyr::mutate(
    display_label = dplyr::coalesce(
      label,
      variable
    ),
    display_label = reorder(
      display_label,
      pct_missing
    )
  )

if (nrow(missingness_plot_data) > 0) {
  
  missingness_plot <- ggplot2::ggplot(
    missingness_plot_data,
    ggplot2::aes(
      x = display_label,
      y = pct_missing
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(
          pct_missing,
          "%"
        )
      ),
      hjust = -0.1,
      size = 3
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(
        mult = c(0, 0.15)
      )
    ) +
    ggplot2::labs(
      title = "Missingness in candidate segmentation variables",
      x = NULL,
      y = "Missing values (%)",
      caption = "Behavioural Segmentation Toolkit"
    ) +
    ggplot2::theme_minimal(
      base_size = 12
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold"
      ),
      panel.grid.major.y = ggplot2::element_blank()
    )
  
  ggplot2::ggsave(
    filename = file.path(
      figures_dir,
      "04_missingness_by_variable.png"
    ),
    plot = missingness_plot,
    width = 10,
    height = max(
      6,
      0.28 * nrow(missingness_plot_data)
    ),
    dpi = 300,
    bg = "white"
  )
}


# =============================================================================
# 8. Variable distribution summary
# =============================================================================

variable_distribution_summary <- purrr::map_dfr(
  names(model_data),
  function(variable_name) {
    
    summarise_variable(
      data = model_data,
      variable_name = variable_name
    )
  }
) %>%
  dplyr::left_join(
    variable_metadata %>%
      dplyr::select(
        variable,
        label,
        group,
        data_type
      ),
    by = "variable"
  ) %>%
  dplyr::select(
    variable,
    label,
    group,
    data_type,
    dplyr::everything()
  )

readr::write_csv(
  variable_distribution_summary,
  file.path(
    tables_dir,
    "04_variable_distribution_summary.csv"
  )
)


# =============================================================================
# 9. Low-variance diagnostics
# =============================================================================

low_variance_variables <- variable_distribution_summary %>%
  dplyr::filter(
    n_unique <=
      low_variance_unique_threshold |
      is.na(standard_deviation) |
      standard_deviation == 0
  ) %>%
  dplyr::select(
    variable,
    label,
    group,
    data_type,
    n_unique,
    standard_deviation,
    pct_missing
  )

readr::write_csv(
  low_variance_variables,
  file.path(
    tables_dir,
    "04_low_variance_variables.csv"
  )
)

analysis_variables <- setdiff(
  names(model_data),
  low_variance_variables$variable
)

if (length(analysis_variables) < 2) {
  stop(
    "Fewer than two usable segmentation variables remain ",
    "after low-variance filtering."
  )
}

model_data_reduced <- model_data %>%
  dplyr::select(
    dplyr::all_of(analysis_variables)
  )

write_log(
  "Low-variance variables identified: ",
  nrow(low_variance_variables),
  log_file = log_file
)


# =============================================================================
# 10. Exploratory median imputation
# =============================================================================

model_data_imputed <- model_data_reduced %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      impute_numeric_median
    )
  )

segmentation_exploratory_imputed <- dplyr::bind_cols(
  segmentation_data %>%
    dplyr::select(
      dplyr::all_of(identifier_variable)
    ),
  model_data_imputed
)

saveRDS(
  segmentation_exploratory_imputed,
  file.path(
    data_processed_dir,
    "04_segmentation_exploratory_imputed.rds"
  )
)

imputation_summary <- tibble::tibble(
  variable = names(model_data_reduced),
  n_imputed = purrr::map_int(
    model_data_reduced,
    ~ sum(is.na(.x))
  ),
  median_used = purrr::map_dbl(
    model_data_reduced,
    safe_median
  )
)

readr::write_csv(
  imputation_summary,
  file.path(
    tables_dir,
    "04_exploratory_imputation_summary.csv"
  )
)


# =============================================================================
# 11. Correlation diagnostics
# =============================================================================

correlation_matrix <- stats::cor(
  model_data_imputed,
  use = "pairwise.complete.obs",
  method = "pearson"
)

correlation_table <- as.data.frame(
  correlation_matrix
) %>%
  tibble::rownames_to_column(
    "variable_1"
  ) %>%
  tidyr::pivot_longer(
    cols = -variable_1,
    names_to = "variable_2",
    values_to = "correlation"
  )

high_correlation_pairs <- correlation_table %>%
  dplyr::filter(
    variable_1 < variable_2,
    abs(correlation) >=
      high_correlation_threshold
  ) %>%
  dplyr::mutate(
    absolute_correlation = abs(
      correlation
    )
  ) %>%
  dplyr::arrange(
    dplyr::desc(
      absolute_correlation
    )
  )

readr::write_csv(
  high_correlation_pairs,
  file.path(
    tables_dir,
    "04_high_correlation_pairs.csv"
  )
)

correlation_plot <- ggplot2::ggplot(
  correlation_table,
  ggplot2::aes(
    x = variable_1,
    y = variable_2,
    fill = correlation
  )
) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient2(
    limits = c(-1, 1),
    midpoint = 0
  ) +
  ggplot2::coord_fixed() +
  ggplot2::labs(
    title = "Correlation matrix of candidate segmentation variables",
    x = NULL,
    y = NULL,
    fill = "Correlation"
  ) +
  ggplot2::theme_minimal(
    base_size = 9
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold"
    ),
    axis.text.x = ggplot2::element_text(
      angle = 60,
      hjust = 1
    ),
    panel.grid = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "04_correlation_heatmap.png"
  ),
  plot = correlation_plot,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

write_log(
  "High-correlation pairs identified: ",
  nrow(high_correlation_pairs),
  log_file = log_file
)


# =============================================================================
# 12. Standardisation and PCA
# =============================================================================

model_scaled <- scale(
  model_data_imputed
)

pca_result <- stats::prcomp(
  model_scaled,
  center = FALSE,
  scale. = FALSE
)

pca_variance <- tibble::tibble(
  component_number = seq_along(
    pca_result$sdev
  ),
  component = paste0(
    "PC",
    component_number
  ),
  variance_explained = (
    pca_result$sdev^2
  ) /
    sum(
      pca_result$sdev^2
    ),
  cumulative_variance = cumsum(
    variance_explained
  )
)

readr::write_csv(
  pca_variance,
  file.path(
    tables_dir,
    "04_pca_variance_explained.csv"
  )
)

pca_loadings <- as.data.frame(
  pca_result$rotation
) %>%
  tibble::rownames_to_column(
    "variable"
  ) %>%
  tidyr::pivot_longer(
    cols = -variable,
    names_to = "component",
    values_to = "loading"
  ) %>%
  dplyr::mutate(
    absolute_loading = abs(
      loading
    )
  ) %>%
  dplyr::group_by(
    component
  ) %>%
  dplyr::slice_max(
    order_by = absolute_loading,
    n = maximum_loading_rows,
    with_ties = FALSE
  ) %>%
  dplyr::ungroup()

readr::write_csv(
  pca_loadings,
  file.path(
    tables_dir,
    "04_pca_loadings.csv"
  )
)

pca_variance_plot_data <- pca_variance %>%
  dplyr::slice_head(
    n = maximum_pca_components_to_plot
  )

pca_variance_plot <- ggplot2::ggplot(
  pca_variance_plot_data,
  ggplot2::aes(
    x = component_number
  )
) +
  ggplot2::geom_col(
    ggplot2::aes(
      y = variance_explained
    )
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      y = cumulative_variance,
      group = 1
    ),
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      y = cumulative_variance
    ),
    size = 2
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 1
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = pca_variance_plot_data$
      component_number
  ) +
  ggplot2::labs(
    title = "PCA variance diagnostic",
    subtitle = "Bars show individual variance; line shows cumulative variance",
    x = "Principal component",
    y = "Variance explained"
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold"
    )
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "04_pca_variance.png"
  ),
  plot = pca_variance_plot,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

pca_scores <- tibble::as_tibble(
  pca_result$x[, 1:2, drop = FALSE]
) %>%
  dplyr::bind_cols(
    segmentation_data %>%
      dplyr::select(
        dplyr::all_of(identifier_variable)
      )
  )

pca_projection_plot <- ggplot2::ggplot(
  pca_scores,
  ggplot2::aes(
    x = PC1,
    y = PC2
  )
) +
  ggplot2::geom_point(
    alpha = 0.35,
    size = 1.3
  ) +
  ggplot2::labs(
    title = "Exploratory PCA projection",
    subtitle = "Candidate segmentation variables after median imputation and standardisation",
    x = paste0(
      "PC1 (",
      scales::percent(
        pca_variance$variance_explained[1],
        accuracy = 0.1
      ),
      ")"
    ),
    y = paste0(
      "PC2 (",
      scales::percent(
        pca_variance$variance_explained[2],
        accuracy = 0.1
      ),
      ")"
    )
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold"
    )
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "04_pca_projection.png"
  ),
  plot = pca_projection_plot,
  width = 8,
  height = 7,
  dpi = 300,
  bg = "white"
)

saveRDS(
  pca_result,
  file.path(
    models_dir,
    "04_exploratory_pca.rds"
  )
)


# =============================================================================
# 13. Exploratory k-means comparison
# =============================================================================

euclidean_distance <- stats::dist(
  model_scaled
)

kmeans_quality <- purrr::map_dfr(
  k_values,
  function(k) {
    
    calculate_kmeans_quality(
      scaled_data = model_scaled,
      k = k,
      distance_matrix = euclidean_distance
    )
  }
)

readr::write_csv(
  kmeans_quality,
  file.path(
    tables_dir,
    "04_kmeans_quality.csv"
  )
)

kmeans_quality_plot <- ggplot2::ggplot(
  kmeans_quality,
  ggplot2::aes(
    x = k,
    y = average_silhouette
  )
) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    size = 2.5
  ) +
  ggplot2::scale_x_continuous(
    breaks = k_values
  ) +
  ggplot2::labs(
    title = "Exploratory k-means model quality",
    subtitle = "Average silhouette width across candidate values of k",
    x = "Number of clusters",
    y = "Average silhouette width"
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold"
    )
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "04_kmeans_quality.png"
  ),
  plot = kmeans_quality_plot,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)


# =============================================================================
# 14. Exploratory PAM with Gower distance
# =============================================================================

set.seed(random_seed)

if (
  nrow(model_data_imputed) >
  gower_sample_size
) {
  
  gower_indices <- sample(
    seq_len(
      nrow(model_data_imputed)
    ),
    gower_sample_size
  )
  
} else {
  
  gower_indices <- seq_len(
    nrow(model_data_imputed)
  )
}

gower_data <- model_data_imputed[
  gower_indices,
  ,
  drop = FALSE
]

binary_variable_names <- variable_metadata %>%
  dplyr::filter(
    data_type == "binary",
    variable %in% names(gower_data)
  ) %>%
  dplyr::pull(variable)

if (length(binary_variable_names) > 0) {
  
  gower_data <- gower_data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(
          binary_variable_names
        ),
        ~ factor(
          .x,
          levels = c(0, 1)
        )
      )
    )
}

gower_distance <- cluster::daisy(
  gower_data,
  metric = "gower"
)

pam_gower_quality <- purrr::map_dfr(
  k_values,
  function(k) {
    
    calculate_pam_quality(
      gower_distance = gower_distance,
      k = k
    )
  }
)

readr::write_csv(
  pam_gower_quality,
  file.path(
    tables_dir,
    "04_pam_gower_quality.csv"
  )
)

pam_gower_quality_plot <- ggplot2::ggplot(
  pam_gower_quality,
  ggplot2::aes(
    x = k,
    y = average_silhouette
  )
) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    size = 2.5
  ) +
  ggplot2::scale_x_continuous(
    breaks = k_values
  ) +
  ggplot2::labs(
    title = "Exploratory PAM-Gower model quality",
    subtitle = paste0(
      "Average silhouette width; sample size = ",
      length(gower_indices)
    ),
    x = "Number of clusters",
    y = "Average silhouette width"
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold"
    )
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "04_pam_gower_quality.png"
  ),
  plot = pam_gower_quality_plot,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

saveRDS(
  gower_distance,
  file.path(
    models_dir,
    "04_exploratory_gower_distance.rds"
  )
)


# =============================================================================
# 15. Combined model comparison
# =============================================================================

combined_model_quality <- dplyr::bind_rows(
  kmeans_quality %>%
    dplyr::select(
      method,
      k,
      average_silhouette,
      minimum_cluster_share,
      maximum_cluster_share
    ),
  pam_gower_quality %>%
    dplyr::select(
      method,
      k,
      average_silhouette,
      minimum_cluster_share,
      maximum_cluster_share
    )
)

readr::write_csv(
  combined_model_quality,
  file.path(
    tables_dir,
    "04_combined_model_quality.csv"
  )
)

combined_quality_plot <- ggplot2::ggplot(
  combined_model_quality,
  ggplot2::aes(
    x = k,
    y = average_silhouette,
    linetype = method,
    shape = method
  )
) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    size = 2.5
  ) +
  ggplot2::scale_x_continuous(
    breaks = k_values
  ) +
  ggplot2::labs(
    title = "Exploratory clustering comparison",
    subtitle = "K-means versus PAM with Gower distance",
    x = "Number of clusters",
    y = "Average silhouette width",
    linetype = "Method",
    shape = "Method"
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold"
    ),
    legend.position = "bottom"
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "04_combined_model_quality.png"
  ),
  plot = combined_quality_plot,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)


# =============================================================================
# 16. Exploratory summary
# =============================================================================

best_kmeans <- kmeans_quality %>%
  dplyr::slice_max(
    order_by = average_silhouette,
    n = 1,
    with_ties = FALSE
  )

best_pam <- pam_gower_quality %>%
  dplyr::slice_max(
    order_by = average_silhouette,
    n = 1,
    with_ties = FALSE
  )

exploratory_summary <- tibble::tibble(
  item = c(
    "Rows",
    "Candidate variables",
    "Variables retained after low-variance check",
    "Variables with missing values",
    "Low-variance variables",
    "High-correlation pairs",
    "PCA components for 50% cumulative variance",
    "PCA components for 70% cumulative variance",
    "PCA components for 80% cumulative variance",
    "Best exploratory k-means k",
    "Best exploratory k-means silhouette",
    "Best exploratory PAM-Gower k",
    "Best exploratory PAM-Gower silhouette",
    "Gower sample size"
  ),
  value = c(
    nrow(model_data),
    ncol(model_data),
    ncol(model_data_imputed),
    sum(
      missingness_summary$n_missing > 0
    ),
    nrow(low_variance_variables),
    nrow(high_correlation_pairs),
    min(
      pca_variance$component_number[
        pca_variance$cumulative_variance >= 0.50
      ]
    ),
    min(
      pca_variance$component_number[
        pca_variance$cumulative_variance >= 0.70
      ]
    ),
    min(
      pca_variance$component_number[
        pca_variance$cumulative_variance >= 0.80
      ]
    ),
    best_kmeans$k,
    round(
      best_kmeans$average_silhouette,
      4
    ),
    best_pam$k,
    round(
      best_pam$average_silhouette,
      4
    ),
    length(gower_indices)
  )
)

readr::write_csv(
  exploratory_summary,
  file.path(
    tables_dir,
    "04_exploratory_summary.csv"
  )
)


# =============================================================================
# 17. Reproducibility
# =============================================================================

capture.output(
  sessionInfo(),
  file = file.path(
    logs_dir,
    "04_sessionInfo.txt"
  )
)


# =============================================================================
# 18. Final report
# =============================================================================

write_log(
  "Exploratory segmentation check completed successfully.",
  log_file = log_file
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — EXPLORATORY CHECK COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Rows:                              ",
  nrow(model_data),
  "\n",
  "Candidate variables:               ",
  ncol(model_data),
  "\n",
  "Variables retained:                ",
  ncol(model_data_imputed),
  "\n",
  "Low-variance variables:            ",
  nrow(low_variance_variables),
  "\n",
  "High-correlation pairs:            ",
  nrow(high_correlation_pairs),
  "\n",
  "Best exploratory k-means solution: k = ",
  best_kmeans$k,
  " (silhouette = ",
  round(
    best_kmeans$average_silhouette,
    3
  ),
  ")\n",
  "Best exploratory PAM solution:     k = ",
  best_pam$k,
  " (silhouette = ",
  round(
    best_pam$average_silhouette,
    3
  ),
  ")\n",
  "\nMain summary output:\n",
  file.path(
    tables_dir,
    "04_exploratory_summary.csv"
  ),
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)