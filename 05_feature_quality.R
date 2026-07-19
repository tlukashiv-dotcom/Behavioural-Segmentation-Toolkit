# =============================================================================
# 05_feature_selection.R
# Behavioural Segmentation Toolkit
# =============================================================================

rm(list = ls())
gc()

options(stringsAsFactors = FALSE, warn = 1, scipen = 999)

required_packages <- c(
  "dplyr", "readr", "tidyr", "purrr", "tibble", "ggplot2"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# -----------------------------------------------------------------------------
# Settings
# -----------------------------------------------------------------------------

identifier_variable <- "respondent_id"
maximum_missingness_pct <- 40
minimum_unique_values <- 2
minimum_standard_deviation <- 1e-8
high_correlation_threshold <- 0.80
minimum_variables_per_group <- 1

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

find_project_root <- function(start_dir = getwd()) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)

  for (i in seq_len(10)) {
    if (
      dir.exists(file.path(current, "data")) &&
      dir.exists(file.path(current, "outputs"))
    ) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }

  stop("Project root not found. The project must contain data/ and outputs/ folders.")
}

create_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

write_log <- function(..., log_file) {
  text <- paste0(...)
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " | ", text, "\n",
    file = log_file,
    append = TRUE,
    sep = ""
  )
  message(text)
}

safe_median <- function(x) {
  if (all(is.na(x))) return(0)
  stats::median(x, na.rm = TRUE)
}

median_impute <- function(x) {
  replacement <- safe_median(x)
  x[is.na(x)] <- replacement
  x
}

normalised_entropy <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)

  probabilities <- prop.table(table(x))
  if (length(probabilities) <= 1) return(0)

  -sum(probabilities * log(probabilities)) / log(length(probabilities))
}

variable_quality <- function(data, metadata, variable_name) {
  x <- data[[variable_name]]
  metadata_row <- metadata %>% dplyr::filter(variable == variable_name)

  tibble::tibble(
    variable = variable_name,
    label = metadata_row$label[1],
    group = metadata_row$group[1],
    data_type = metadata_row$data_type[1],
    n_missing = sum(is.na(x)),
    pct_missing = round(100 * mean(is.na(x)), 2),
    n_unique = dplyr::n_distinct(x, na.rm = TRUE),
    standard_deviation = ifelse(
      sum(!is.na(x)) >= 2,
      stats::sd(x, na.rm = TRUE),
      NA_real_
    ),
    entropy = normalised_entropy(x)
  )
}

choose_variable_to_remove <- function(
    variable_1,
    variable_2,
    quality_table,
    selected_group_counts
) {
  q1 <- quality_table %>% dplyr::filter(variable == variable_1)
  q2 <- quality_table %>% dplyr::filter(variable == variable_2)

  group_1_count <- selected_group_counts[[q1$group[1]]]
  group_2_count <- selected_group_counts[[q2$group[1]]]

  if (
    !is.null(group_1_count) &&
    group_1_count <= minimum_variables_per_group &&
    (is.null(group_2_count) || group_2_count > minimum_variables_per_group)
  ) {
    return(variable_2)
  }

  if (
    !is.null(group_2_count) &&
    group_2_count <= minimum_variables_per_group &&
    (is.null(group_1_count) || group_1_count > minimum_variables_per_group)
  ) {
    return(variable_1)
  }

  if (q1$pct_missing[1] != q2$pct_missing[1]) {
    return(ifelse(q1$pct_missing[1] > q2$pct_missing[1], variable_1, variable_2))
  }

  entropy_1 <- dplyr::coalesce(q1$entropy[1], -Inf)
  entropy_2 <- dplyr::coalesce(q2$entropy[1], -Inf)

  if (entropy_1 != entropy_2) {
    return(ifelse(entropy_1 < entropy_2, variable_1, variable_2))
  }

  sd_1 <- dplyr::coalesce(q1$standard_deviation[1], -Inf)
  sd_2 <- dplyr::coalesce(q2$standard_deviation[1], -Inf)

  ifelse(sd_1 < sd_2, variable_1, variable_2)
}

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------

project_dir <- find_project_root()

data_processed_dir <- file.path(project_dir, "data", "processed")
tables_dir <- file.path(project_dir, "outputs", "tables")
figures_dir <- file.path(project_dir, "outputs", "figures")
logs_dir <- file.path(project_dir, "outputs", "logs")

invisible(lapply(
  c(data_processed_dir, tables_dir, figures_dir, logs_dir),
  create_directory
))

input_file <- file.path(data_processed_dir, "segmentation_data_full.rds")
metadata_file <- file.path(data_processed_dir, "02_variable_metadata.rds")
log_file <- file.path(logs_dir, "05_feature_selection.log")

if (file.exists(log_file)) file.remove(log_file)

# -----------------------------------------------------------------------------
# Load inputs
# -----------------------------------------------------------------------------

if (!file.exists(input_file)) {
  stop("Segmentation dataset not found: ", input_file)
}

if (!file.exists(metadata_file)) {
  stop("Variable metadata not found: ", metadata_file)
}

segmentation_data <- readRDS(input_file) %>% tibble::as_tibble()
variable_metadata <- readRDS(metadata_file) %>% tibble::as_tibble()

write_log(
  "Segmentation data loaded: ",
  nrow(segmentation_data), " rows and ",
  ncol(segmentation_data), " columns.",
  log_file = log_file
)

# -----------------------------------------------------------------------------
# Validate input
# -----------------------------------------------------------------------------

if (!identifier_variable %in% names(segmentation_data)) {
  stop("Identifier variable not found: ", identifier_variable)
}

candidate_variables <- setdiff(names(segmentation_data), identifier_variable)

non_numeric_variables <- candidate_variables[
  !vapply(segmentation_data[candidate_variables], is.numeric, logical(1))
]

if (length(non_numeric_variables) > 0) {
  stop(
    "Non-numeric variables found in segmentation dataset: ",
    paste(non_numeric_variables, collapse = ", ")
  )
}

candidate_metadata <- variable_metadata %>%
  dplyr::filter(variable %in% candidate_variables)

missing_metadata <- setdiff(candidate_variables, candidate_metadata$variable)
if (length(missing_metadata) > 0) {
  stop(
    "Candidate variables missing from metadata: ",
    paste(missing_metadata, collapse = ", ")
  )
}

# -----------------------------------------------------------------------------
# Variable quality metrics
# -----------------------------------------------------------------------------

quality_metrics <- purrr::map_dfr(
  candidate_variables,
  ~ variable_quality(segmentation_data, candidate_metadata, .x)
) %>%
  dplyr::mutate(
    missingness_pass = pct_missing <= maximum_missingness_pct,
    uniqueness_pass = n_unique >= minimum_unique_values,
    variance_pass = !is.na(standard_deviation) &
      standard_deviation > minimum_standard_deviation,
    initial_pass = missingness_pass & uniqueness_pass & variance_pass
  )

readr::write_csv(
  quality_metrics,
  file.path(tables_dir, "05_variable_quality_metrics.csv")
)

initial_exclusions <- quality_metrics %>%
  dplyr::filter(!initial_pass) %>%
  dplyr::mutate(
    exclusion_reason = dplyr::case_when(
      !missingness_pass ~ "Missingness above threshold",
      !uniqueness_pass ~ "Insufficient unique values",
      !variance_pass ~ "Zero or near-zero variance",
      TRUE ~ "Failed initial quality check"
    )
  )

retained_variables <- quality_metrics %>%
  dplyr::filter(initial_pass) %>%
  dplyr::pull(variable)

write_log(
  "Variables passing initial checks: ",
  length(retained_variables), " of ", length(candidate_variables), ".",
  log_file = log_file
)

# -----------------------------------------------------------------------------
# Correlation assessment
# -----------------------------------------------------------------------------

correlation_data <- segmentation_data %>%
  dplyr::select(dplyr::all_of(retained_variables)) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), median_impute))

correlation_matrix <- stats::cor(correlation_data, method = "pearson")

correlation_pairs <- as.data.frame(correlation_matrix) %>%
  tibble::rownames_to_column("variable_1") %>%
  tidyr::pivot_longer(
    cols = -variable_1,
    names_to = "variable_2",
    values_to = "correlation"
  ) %>%
  dplyr::filter(variable_1 < variable_2) %>%
  dplyr::mutate(absolute_correlation = abs(correlation)) %>%
  dplyr::arrange(dplyr::desc(absolute_correlation))

high_correlation_pairs <- correlation_pairs %>%
  dplyr::filter(absolute_correlation >= high_correlation_threshold)

readr::write_csv(
  high_correlation_pairs,
  file.path(tables_dir, "05_high_correlation_pairs.csv")
)

# -----------------------------------------------------------------------------
# Correlation-based redundancy reduction
# -----------------------------------------------------------------------------

selected_variables <- retained_variables

redundancy_decisions <- tibble::tibble(
  variable_1 = character(),
  variable_2 = character(),
  correlation = numeric(),
  removed_variable = character(),
  retained_variable = character(),
  reason = character()
)

if (nrow(high_correlation_pairs) > 0) {
  for (row_index in seq_len(nrow(high_correlation_pairs))) {
    variable_1 <- high_correlation_pairs$variable_1[row_index]
    variable_2 <- high_correlation_pairs$variable_2[row_index]

    if (
      !variable_1 %in% selected_variables ||
      !variable_2 %in% selected_variables
    ) {
      next
    }

    group_counts <- candidate_metadata %>%
      dplyr::filter(variable %in% selected_variables) %>%
      dplyr::count(group)

    selected_group_counts <- stats::setNames(group_counts$n, group_counts$group)

    removed_variable <- choose_variable_to_remove(
      variable_1,
      variable_2,
      quality_metrics,
      selected_group_counts
    )

    retained_variable <- setdiff(
      c(variable_1, variable_2),
      removed_variable
    )

    selected_variables <- setdiff(selected_variables, removed_variable)

    redundancy_decisions <- dplyr::bind_rows(
      redundancy_decisions,
      tibble::tibble(
        variable_1 = variable_1,
        variable_2 = variable_2,
        correlation = high_correlation_pairs$correlation[row_index],
        removed_variable = removed_variable,
        retained_variable = retained_variable,
        reason = paste0(
          "Absolute correlation >= ", high_correlation_threshold,
          "; decision based on group coverage, missingness, entropy and variability"
        )
      )
    )
  }
}

readr::write_csv(
  redundancy_decisions,
  file.path(tables_dir, "05_redundancy_decisions.csv")
)

# -----------------------------------------------------------------------------
# Preserve behavioural group representation
# -----------------------------------------------------------------------------

all_groups <- candidate_metadata %>%
  dplyr::filter(variable %in% retained_variables) %>%
  dplyr::distinct(group) %>%
  dplyr::pull(group)

selected_groups <- candidate_metadata %>%
  dplyr::filter(variable %in% selected_variables) %>%
  dplyr::distinct(group) %>%
  dplyr::pull(group)

groups_without_representation <- setdiff(all_groups, selected_groups)

if (length(groups_without_representation) > 0) {
  for (group_name in groups_without_representation) {
    replacement_candidate <- quality_metrics %>%
      dplyr::filter(
        group == group_name,
        initial_pass,
        !variable %in% selected_variables
      ) %>%
      dplyr::arrange(
        pct_missing,
        dplyr::desc(entropy),
        dplyr::desc(standard_deviation)
      ) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(variable)

    if (length(replacement_candidate) == 1) {
      selected_variables <- c(selected_variables, replacement_candidate)
    }
  }
}

selected_variables <- unique(selected_variables)

# -----------------------------------------------------------------------------
# Build selected datasets
# -----------------------------------------------------------------------------

segmentation_selected <- segmentation_data %>%
  dplyr::select(
    dplyr::all_of(identifier_variable),
    dplyr::all_of(selected_variables)
  )

segmentation_selected_imputed <- segmentation_selected %>%
  dplyr::mutate(
    dplyr::across(
      -dplyr::all_of(identifier_variable),
      median_impute
    )
  )

selected_variable_metadata <- candidate_metadata %>%
  dplyr::filter(variable %in% selected_variables) %>%
  dplyr::mutate(selection_order = match(variable, selected_variables)) %>%
  dplyr::arrange(selection_order)

selected_variables_table <- quality_metrics %>%
  dplyr::filter(variable %in% selected_variables) %>%
  dplyr::mutate(selection_status = "Selected") %>%
  dplyr::arrange(group, variable)

correlation_exclusions <- redundancy_decisions %>%
  dplyr::transmute(
    variable = removed_variable,
    exclusion_reason = "Removed as highly correlated redundant feature"
  ) %>%
  dplyr::left_join(
    quality_metrics %>%
      dplyr::select(variable, label, group, data_type),
    by = "variable"
  ) %>%
  dplyr::select(variable, label, group, data_type, exclusion_reason)

excluded_variables <- dplyr::bind_rows(
  initial_exclusions %>%
    dplyr::select(variable, label, group, data_type, exclusion_reason),
  correlation_exclusions
) %>%
  dplyr::distinct(variable, .keep_all = TRUE) %>%
  dplyr::arrange(group, variable)

# -----------------------------------------------------------------------------
# Save outputs
# -----------------------------------------------------------------------------

saveRDS(
  segmentation_selected,
  file.path(data_processed_dir, "segmentation_selected.rds")
)

saveRDS(
  segmentation_selected_imputed,
  file.path(data_processed_dir, "segmentation_selected_imputed.rds")
)

saveRDS(
  selected_variables,
  file.path(data_processed_dir, "05_selected_variable_names.rds")
)

saveRDS(
  selected_variable_metadata,
  file.path(data_processed_dir, "05_selected_variable_metadata.rds")
)

readr::write_csv(
  selected_variables_table,
  file.path(tables_dir, "05_selected_variables.csv")
)

readr::write_csv(
  excluded_variables,
  file.path(tables_dir, "05_excluded_variables.csv")
)

final_group_summary <- selected_variable_metadata %>%
  dplyr::count(group, name = "n_selected") %>%
  dplyr::arrange(dplyr::desc(n_selected), group)

readr::write_csv(
  final_group_summary,
  file.path(tables_dir, "05_selected_variables_by_group.csv")
)

# -----------------------------------------------------------------------------
# Figures
# -----------------------------------------------------------------------------

selected_group_plot <- ggplot2::ggplot(
  final_group_summary,
  ggplot2::aes(
    x = reorder(group, n_selected),
    y = n_selected
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::geom_text(
    ggplot2::aes(label = n_selected),
    hjust = -0.15
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.15))
  ) +
  ggplot2::labs(
    title = "Selected segmentation variables by behavioural group",
    x = NULL,
    y = "Number of selected variables"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.major.y = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(figures_dir, "05_selected_variables_by_group.png"),
  plot = selected_group_plot,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)

quality_plot_data <- quality_metrics %>%
  dplyr::mutate(selected = variable %in% selected_variables)

quality_overview_plot <- ggplot2::ggplot(
  quality_plot_data,
  ggplot2::aes(
    x = pct_missing,
    y = entropy,
    shape = selected
  )
) +
  ggplot2::geom_point(size = 3, alpha = 0.8) +
  ggplot2::labs(
    title = "Variable quality overview",
    subtitle = "Missingness versus normalised entropy",
    x = "Missing values (%)",
    y = "Normalised entropy",
    shape = "Selected"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "bottom"
  )

ggplot2::ggsave(
  filename = file.path(figures_dir, "05_variable_quality_overview.png"),
  plot = quality_overview_plot,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

feature_selection_summary <- tibble::tibble(
  item = c(
    "Rows",
    "Candidate variables",
    "Variables passing initial checks",
    "High-correlation pairs",
    "Variables removed for redundancy",
    "Final selected variables",
    "Behavioural groups represented",
    "Maximum missingness threshold",
    "High-correlation threshold"
  ),
  value = c(
    nrow(segmentation_data),
    length(candidate_variables),
    length(retained_variables),
    nrow(high_correlation_pairs),
    dplyr::n_distinct(redundancy_decisions$removed_variable),
    length(selected_variables),
    dplyr::n_distinct(selected_variable_metadata$group),
    maximum_missingness_pct,
    high_correlation_threshold
  )
)

readr::write_csv(
  feature_selection_summary,
  file.path(tables_dir, "05_feature_selection_summary.csv")
)

capture.output(
  sessionInfo(),
  file = file.path(logs_dir, "05_sessionInfo.txt")
)

write_log(
  "Feature selection completed successfully.",
  log_file = log_file
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — FEATURE SELECTION COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Rows:                          ", nrow(segmentation_data), "\n",
  "Candidate variables:           ", length(candidate_variables), "\n",
  "Passed initial checks:         ", length(retained_variables), "\n",
  "High-correlation pairs:        ", nrow(high_correlation_pairs), "\n",
  "Redundant variables removed:   ",
  dplyr::n_distinct(redundancy_decisions$removed_variable), "\n",
  "Final selected variables:      ", length(selected_variables), "\n",
  "Behavioural groups represented:",
  dplyr::n_distinct(selected_variable_metadata$group), "\n",
  "\nMain modelling output:\n",
  file.path(data_processed_dir, "segmentation_selected_imputed.rds"),
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)
