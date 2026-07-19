# =============================================================================
# 07_distance_comparison.R
# Behavioural Segmentation Toolkit
# =============================================================================

rm(list = ls())
gc()
options(stringsAsFactors = FALSE, warn = 1, scipen = 999)

required_packages <- c("dplyr", "readr", "purrr", "tibble", "ggplot2", "cluster")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages, dependencies = TRUE)
invisible(lapply(required_packages, library, character.only = TRUE))

identifier_variable <- "respondent_id"
distance_methods <- c("euclidean", "manhattan", "gower")
k_values <- 2:8
random_seed <- 123
maximum_distance_sample_size <- 2000
maximum_distance_pairs_for_correlation <- 100000

find_project_root <- function(start_dir = getwd()) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  for (i in seq_len(10)) {
    if (dir.exists(file.path(current, "data")) && dir.exists(file.path(current, "outputs"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  stop("Project root not found. The project must contain data/ and outputs/ folders.")
}

create_directory <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

write_log <- function(..., log_file) {
  text <- paste0(...)
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", text, "\n", file = log_file, append = TRUE, sep = "")
  message(text)
}

standardise_numeric_data <- function(data) {
  scaled <- scale(data)
  if (any(!is.finite(scaled))) stop("Non-finite values were produced during standardisation.")
  as.data.frame(scaled)
}

prepare_gower_data <- function(data, feature_metadata) {
  gower_data <- data
  binary_variables <- feature_metadata %>%
    dplyr::filter(data_type == "binary", variable %in% names(gower_data)) %>%
    dplyr::pull(variable)
  if (length(binary_variables) > 0) {
    gower_data <- gower_data %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(binary_variables), ~ factor(.x, levels = c(0, 1))))
  }
  gower_data
}

compute_distance <- function(data, method, feature_metadata) {
  start_time <- proc.time()[["elapsed"]]
  if (method == "euclidean") {
    distance_object <- stats::dist(standardise_numeric_data(data), method = "euclidean")
  } else if (method == "manhattan") {
    distance_object <- stats::dist(standardise_numeric_data(data), method = "manhattan")
  } else if (method == "gower") {
    distance_object <- cluster::daisy(prepare_gower_data(data, feature_metadata), metric = "gower")
  } else {
    stop("Unsupported distance method: ", method)
  }
  list(distance = distance_object, elapsed_seconds = proc.time()[["elapsed"]] - start_time)
}

summarise_distance_distribution <- function(distance_object, feature_set, feature_count, distance_method, elapsed_seconds) {
  values <- as.numeric(distance_object)
  mean_distance <- mean(values)
  standard_deviation <- stats::sd(values)
  tibble::tibble(
    feature_set = feature_set,
    feature_count = feature_count,
    distance_method = distance_method,
    n_pairwise_distances = length(values),
    minimum_distance = min(values),
    first_quartile = as.numeric(stats::quantile(values, 0.25)),
    median_distance = stats::median(values),
    mean_distance = mean_distance,
    third_quartile = as.numeric(stats::quantile(values, 0.75)),
    maximum_distance = max(values),
    standard_deviation = standard_deviation,
    coefficient_of_variation = ifelse(mean_distance == 0, NA_real_, standard_deviation / mean_distance),
    relative_contrast = ifelse(max(values) == 0, NA_real_, (max(values) - min(values)) / max(values)),
    elapsed_seconds = elapsed_seconds
  )
}

evaluate_pam_solution <- function(distance_object, feature_set, feature_count, distance_method, k) {
  model <- cluster::pam(distance_object, k = k, diss = TRUE)
  cluster_sizes <- table(model$clustering)
  minimum_share <- min(cluster_sizes) / sum(cluster_sizes)
  maximum_share <- max(cluster_sizes) / sum(cluster_sizes)
  tibble::tibble(
    feature_set = feature_set,
    feature_count = feature_count,
    distance_method = distance_method,
    k = k,
    average_silhouette = model$silinfo$avg.width,
    minimum_cluster_size = min(cluster_sizes),
    maximum_cluster_size = max(cluster_sizes),
    minimum_cluster_share = minimum_share,
    maximum_cluster_share = maximum_share,
    cluster_size_ratio = max(cluster_sizes) / min(cluster_sizes),
    balance_score = 1 - (maximum_share - minimum_share)
  )
}

sample_distance_values <- function(distance_object, maximum_pairs, seed) {
  values <- as.numeric(distance_object)
  if (length(values) <= maximum_pairs) return(values)
  set.seed(seed)
  values[sample(seq_along(values), size = maximum_pairs, replace = FALSE)]
}

project_dir <- find_project_root()
data_processed_dir <- file.path(project_dir, "data", "processed")
tables_dir <- file.path(project_dir, "outputs", "tables")
figures_dir <- file.path(project_dir, "outputs", "figures")
models_dir <- file.path(project_dir, "outputs", "models")
logs_dir <- file.path(project_dir, "outputs", "logs")
invisible(lapply(c(tables_dir, figures_dir, models_dir, logs_dir), create_directory))

feature_sets_file <- file.path(data_processed_dir, "06_candidate_feature_sets.rds")
feature_metadata_file <- file.path(data_processed_dir, "06_candidate_feature_metadata.rds")
log_file <- file.path(logs_dir, "07_distance_comparison.log")
if (file.exists(log_file)) file.remove(log_file)

if (!file.exists(feature_sets_file)) stop("Candidate feature sets not found: ", feature_sets_file, "\nRun 06_feature_selection.R first.")
if (!file.exists(feature_metadata_file)) stop("Candidate feature metadata not found: ", feature_metadata_file, "\nRun 06_feature_selection.R first.")

candidate_feature_sets <- readRDS(feature_sets_file)
candidate_feature_metadata <- readRDS(feature_metadata_file) %>% tibble::as_tibble()
if (!is.list(candidate_feature_sets) || is.null(names(candidate_feature_sets))) stop("Candidate feature sets must be a named list.")
write_log("Candidate feature sets loaded: ", length(candidate_feature_sets), ".", log_file = log_file)

reference_ids <- NULL
for (feature_set_name in names(candidate_feature_sets)) {
  feature_data <- candidate_feature_sets[[feature_set_name]]
  if (!identifier_variable %in% names(feature_data)) stop("Identifier variable missing from ", feature_set_name)
  if (anyDuplicated(feature_data[[identifier_variable]]) > 0) stop("Duplicate identifiers found in ", feature_set_name)
  if (is.null(reference_ids)) reference_ids <- feature_data[[identifier_variable]]
  else if (!identical(reference_ids, feature_data[[identifier_variable]])) stop("Respondent order differs across feature sets.")
}

set.seed(random_seed)
if (length(reference_ids) > maximum_distance_sample_size) {
  sampled_indices <- sort(sample(seq_along(reference_ids), maximum_distance_sample_size))
} else {
  sampled_indices <- seq_along(reference_ids)
}
write_log("Rows used for distance comparison: ", length(sampled_indices), ".", log_file = log_file)

distance_objects <- list()
distance_distribution_results <- list()
pam_quality_results <- list()

for (feature_set_name in names(candidate_feature_sets)) {
  feature_data <- candidate_feature_sets[[feature_set_name]][sampled_indices, , drop = FALSE]
  model_data <- feature_data %>% dplyr::select(-dplyr::all_of(identifier_variable))
  feature_count <- ncol(model_data)
  non_numeric_variables <- names(model_data)[!vapply(model_data, is.numeric, logical(1))]
  if (length(non_numeric_variables) > 0) stop("Non-numeric features in ", feature_set_name, ": ", paste(non_numeric_variables, collapse = ", "))

  feature_metadata <- candidate_feature_metadata %>%
    dplyr::filter(feature_set == feature_set_name, variable %in% names(model_data)) %>%
    dplyr::distinct(variable, .keep_all = TRUE)
  if (nrow(feature_metadata) != feature_count) stop("Feature metadata mismatch for ", feature_set_name)

  distance_objects[[feature_set_name]] <- list()

  for (distance_method in distance_methods) {
    write_log("Computing ", distance_method, " distance for ", feature_set_name, "...", log_file = log_file)
    result <- compute_distance(model_data, distance_method, feature_metadata)
    distance_objects[[feature_set_name]][[distance_method]] <- result$distance
    key <- paste(feature_set_name, distance_method, sep = "__")
    distance_distribution_results[[key]] <- summarise_distance_distribution(
      result$distance, feature_set_name, feature_count, distance_method, result$elapsed_seconds
    )
    pam_quality_results[[key]] <- purrr::map_dfr(
      k_values,
      ~ evaluate_pam_solution(result$distance, feature_set_name, feature_count, distance_method, .x)
    )
  }
}

distance_distribution_summary <- dplyr::bind_rows(distance_distribution_results)
pam_distance_quality <- dplyr::bind_rows(pam_quality_results)
readr::write_csv(distance_distribution_summary, file.path(tables_dir, "07_distance_distribution_summary.csv"))
readr::write_csv(pam_distance_quality, file.path(tables_dir, "07_pam_distance_quality.csv"))
saveRDS(distance_objects, file.path(models_dir, "07_distance_objects.rds"))

best_distance_by_feature_set <- pam_distance_quality %>%
  dplyr::group_by(feature_set, feature_count, distance_method) %>%
  dplyr::slice_max(average_silhouette, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(feature_set, feature_count) %>%
  dplyr::mutate(silhouette_rank = rank(-average_silhouette, ties.method = "min")) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(feature_count), silhouette_rank)
readr::write_csv(best_distance_by_feature_set, file.path(tables_dir, "07_best_distance_by_feature_set.csv"))

distance_matrix_correlations <- purrr::imap_dfr(distance_objects, function(method_list, feature_set_name) {
  sampled_vectors <- purrr::map(method_list, sample_distance_values, maximum_pairs = maximum_distance_pairs_for_correlation, seed = random_seed)
  pairs <- utils::combn(names(sampled_vectors), 2, simplify = FALSE)
  purrr::map_dfr(pairs, function(pair) {
    v1 <- sampled_vectors[[pair[1]]]
    v2 <- sampled_vectors[[pair[2]]]
    n <- min(length(v1), length(v2))
    tibble::tibble(
      feature_set = feature_set_name,
      method_1 = pair[1],
      method_2 = pair[2],
      sampled_pairs = n,
      pearson_correlation = stats::cor(v1[seq_len(n)], v2[seq_len(n)], method = "pearson"),
      spearman_correlation = stats::cor(v1[seq_len(n)], v2[seq_len(n)], method = "spearman")
    )
  })
})
readr::write_csv(distance_matrix_correlations, file.path(tables_dir, "07_distance_matrix_correlations.csv"))

distance_comparison_summary <- best_distance_by_feature_set %>%
  dplyr::left_join(
    distance_distribution_summary %>%
      dplyr::select(feature_set, feature_count, distance_method, coefficient_of_variation, relative_contrast, elapsed_seconds),
    by = c("feature_set", "feature_count", "distance_method")
  ) %>%
  dplyr::group_by(distance_method) %>%
  dplyr::summarise(
    feature_sets_evaluated = dplyr::n(),
    mean_best_silhouette = mean(average_silhouette),
    median_best_silhouette = stats::median(average_silhouette),
    mean_balance_score = mean(balance_score),
    mean_minimum_cluster_share = mean(minimum_cluster_share),
    mean_distance_cv = mean(coefficient_of_variation),
    mean_relative_contrast = mean(relative_contrast),
    mean_elapsed_seconds = mean(elapsed_seconds),
    wins_by_silhouette = sum(silhouette_rank == 1),
    .groups = "drop"
  ) %>%
  dplyr::mutate(overall_rank = rank(-mean_best_silhouette, ties.method = "min")) %>%
  dplyr::arrange(overall_rank)
readr::write_csv(distance_comparison_summary, file.path(tables_dir, "07_distance_comparison_summary.csv"))

p1 <- ggplot2::ggplot(
  pam_distance_quality,
  ggplot2::aes(x = k, y = average_silhouette, linetype = distance_method, shape = distance_method)
) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ feature_set, ncol = 2) +
  ggplot2::scale_x_continuous(breaks = k_values) +
  ggplot2::labs(
    title = "Distance-method comparison across candidate feature sets",
    subtitle = "Exploratory PAM average silhouette width",
    x = "Number of clusters",
    y = "Average silhouette width",
    linetype = "Distance",
    shape = "Distance"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "bottom")
ggplot2::ggsave(file.path(figures_dir, "07_distance_quality_comparison.png"), p1, width = 11, height = 10, dpi = 300, bg = "white")

p2 <- ggplot2::ggplot(
  best_distance_by_feature_set,
  ggplot2::aes(x = reorder(feature_set, feature_count), y = average_silhouette, shape = distance_method, group = distance_method)
) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::geom_point(size = 2.8) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Best PAM silhouette achieved by each distance method",
    subtitle = "Best value across candidate k for every feature set",
    x = NULL,
    y = "Best average silhouette",
    shape = "Distance"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "bottom")
ggplot2::ggsave(file.path(figures_dir, "07_best_silhouette_by_distance.png"), p2, width = 9, height = 6, dpi = 300, bg = "white")

p3 <- ggplot2::ggplot(
  distance_distribution_summary,
  ggplot2::aes(x = feature_count, y = coefficient_of_variation, linetype = distance_method, shape = distance_method)
) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::scale_x_reverse(breaks = sort(unique(distance_distribution_summary$feature_count), decreasing = TRUE)) +
  ggplot2::labs(
    title = "Distance concentration across feature-set sizes",
    subtitle = "Higher coefficient of variation indicates greater distance contrast",
    x = "Number of features",
    y = "Distance coefficient of variation",
    linetype = "Distance",
    shape = "Distance"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "bottom")
ggplot2::ggsave(file.path(figures_dir, "07_distance_concentration.png"), p3, width = 9, height = 6, dpi = 300, bg = "white")

best_overall_method <- distance_comparison_summary %>%
  dplyr::slice_min(overall_rank, n = 1, with_ties = FALSE)

capture.output(sessionInfo(), file = file.path(logs_dir, "07_sessionInfo.txt"))
write_log("Distance comparison completed successfully.", log_file = log_file)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\nBEHAVIOURAL SEGMENTATION TOOLKIT — DISTANCE COMPARISON COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\nRows evaluated:                 ", length(sampled_indices),
  "\nCandidate feature sets:         ", length(candidate_feature_sets),
  "\nDistance methods:               ", paste(distance_methods, collapse = ", "),
  "\nPAM solutions evaluated:        ", nrow(pam_distance_quality),
  "\nBest mean silhouette method:    ", best_overall_method$distance_method,
  "\nMean best silhouette:           ", round(best_overall_method$mean_best_silhouette, 3),
  "\n\nMain summary output:\n",
  file.path(tables_dir, "07_distance_comparison_summary.csv"),
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)
