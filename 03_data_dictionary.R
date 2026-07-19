# =============================================================================
# 03_data_dictionary.R
# Behavioural Segmentation Toolkit
#
# Purpose:
#   - Load the preprocessed survey dataset
#   - Load variable metadata configuration
#   - Validate consistency between data and metadata
#   - Generate a complete data dictionary
#   - Generate role-specific dictionaries
#   - Generate variable-level descriptive summaries
#   - Export documentation-ready CSV files
#
# Expected inputs:
#   data/processed/survey_preprocessed.rds
#   config/variable_metadata.csv
#
# Main outputs:
#   outputs/documentation/03_data_dictionary.csv
#   outputs/documentation/03_segmentation_dictionary.csv
#   outputs/documentation/03_profiling_dictionary.csv
#   outputs/documentation/03_metadata_dictionary.csv
#   outputs/documentation/03_variable_levels.csv
#   outputs/documentation/03_numeric_summary.csv
#   outputs/documentation/03_dictionary_validation.csv
#   outputs/logs/03_data_dictionary.log
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
  "stringr",
  "purrr",
  "tibble",
  "janitor"
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

maximum_example_values <- 8

maximum_levels_to_export <- 100


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
      dir.exists(file.path(current, "config"))
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
    "The project must contain data/ and config/ folders."
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


clean_text <- function(x) {
  
  x %>%
    as.character() %>%
    stringr::str_squish() %>%
    dplyr::na_if("")
}


parse_logical <- function(x, column_name) {
  
  value <- stringr::str_to_lower(
    clean_text(x)
  )
  
  result <- dplyr::case_when(
    value %in% c("true", "t", "yes", "y", "1") ~ TRUE,
    value %in% c("false", "f", "no", "n", "0") ~ FALSE,
    is.na(value) ~ NA,
    TRUE ~ NA
  )
  
  invalid <- unique(
    value[
      !is.na(value) &
        is.na(result)
    ]
  )
  
  if (length(invalid) > 0) {
    stop(
      "Invalid values in metadata column '",
      column_name,
      "': ",
      paste(invalid, collapse = ", ")
    )
  }
  
  result
}


observed_type <- function(x) {
  
  if (is.factor(x)) {
    return("factor")
  }
  
  if (is.logical(x)) {
    return("logical")
  }
  
  if (is.integer(x)) {
    return("integer")
  }
  
  if (is.numeric(x)) {
    return("numeric")
  }
  
  if (is.character(x)) {
    return("character")
  }
  
  class(x)[1]
}


example_values <- function(
    x,
    maximum_values = maximum_example_values
) {
  
  values <- x %>%
    as.character() %>%
    stats::na.omit() %>%
    unique()
  
  if (length(values) == 0) {
    return(NA_character_)
  }
  
  paste(
    head(values, maximum_values),
    collapse = " | "
  )
}


safe_minimum <- function(x) {
  
  if (
    !is.numeric(x) ||
    all(is.na(x))
  ) {
    return(NA_real_)
  }
  
  min(x, na.rm = TRUE)
}


safe_maximum <- function(x) {
  
  if (
    !is.numeric(x) ||
    all(is.na(x))
  ) {
    return(NA_real_)
  }
  
  max(x, na.rm = TRUE)
}


safe_mean <- function(x) {
  
  if (
    !is.numeric(x) ||
    all(is.na(x))
  ) {
    return(NA_real_)
  }
  
  mean(x, na.rm = TRUE)
}


safe_median <- function(x) {
  
  if (
    !is.numeric(x) ||
    all(is.na(x))
  ) {
    return(NA_real_)
  }
  
  median(x, na.rm = TRUE)
}


safe_standard_deviation <- function(x) {
  
  if (
    !is.numeric(x) ||
    sum(!is.na(x)) < 2
  ) {
    return(NA_real_)
  }
  
  stats::sd(x, na.rm = TRUE)
}


# =============================================================================
# 4. Paths
# =============================================================================

project_dir <- find_project_root()

input_file <- file.path(
  project_dir,
  "data",
  "processed",
  "survey_preprocessed.rds"
)

metadata_file <- file.path(
  project_dir,
  "config",
  "variable_metadata.csv"
)

documentation_dir <- file.path(
  project_dir,
  "outputs",
  "documentation"
)

logs_dir <- file.path(
  project_dir,
  "outputs",
  "logs"
)

create_directory(documentation_dir)
create_directory(logs_dir)

log_file <- file.path(
  logs_dir,
  "03_data_dictionary.log"
)

if (file.exists(log_file)) {
  file.remove(log_file)
}


# =============================================================================
# 5. Load inputs
# =============================================================================

if (!file.exists(input_file)) {
  stop(
    "Preprocessed dataset not found: ",
    input_file,
    "\nRun 02_data_preprocessing.R first."
  )
}

if (!file.exists(metadata_file)) {
  stop(
    "Variable metadata file not found: ",
    metadata_file
  )
}

survey <- readRDS(
  input_file
) %>%
  tibble::as_tibble()

metadata <- readr::read_csv(
  metadata_file,
  show_col_types = FALSE,
  na = c("", "NA", "N/A")
) %>%
  janitor::clean_names()

write_log(
  "Survey loaded: ",
  nrow(survey),
  " rows and ",
  ncol(survey),
  " columns.",
  log_file = log_file
)

write_log(
  "Metadata loaded: ",
  nrow(metadata),
  " rows.",
  log_file = log_file
)


# =============================================================================
# 6. Validate metadata structure
# =============================================================================

required_columns <- c(
  "variable",
  "label",
  "role",
  "group",
  "data_type",
  "scale",
  "include_segmentation",
  "include_profiling",
  "allow_missing",
  "description"
)

missing_columns <- setdiff(
  required_columns,
  names(metadata)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing metadata columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

metadata <- metadata %>%
  dplyr::mutate(
    variable = clean_text(variable),
    label = clean_text(label),
    role = stringr::str_to_lower(
      clean_text(role)
    ),
    group = clean_text(group),
    data_type = stringr::str_to_lower(
      clean_text(data_type)
    ),
    scale = clean_text(scale),
    description = clean_text(description),
    include_segmentation = parse_logical(
      include_segmentation,
      "include_segmentation"
    ),
    include_profiling = parse_logical(
      include_profiling,
      "include_profiling"
    ),
    allow_missing = parse_logical(
      allow_missing,
      "allow_missing"
    )
  )

duplicate_variables <- metadata %>%
  dplyr::count(
    variable,
    name = "n"
  ) %>%
  dplyr::filter(
    n > 1
  )

if (nrow(duplicate_variables) > 0) {
  stop(
    "Duplicate variables in variable_metadata.csv: ",
    paste(
      duplicate_variables$variable,
      collapse = ", "
    )
  )
}


# =============================================================================
# 7. Validate consistency between dataset and metadata
# =============================================================================

dataset_variables <- names(survey)
metadata_variables <- metadata$variable

missing_from_metadata <- setdiff(
  dataset_variables,
  metadata_variables
)

missing_from_dataset <- setdiff(
  metadata_variables,
  dataset_variables
)

dictionary_validation <- dplyr::bind_rows(
  tibble::tibble(
    variable = intersect(
      dataset_variables,
      metadata_variables
    ),
    validation_status = "Matched"
  ),
  tibble::tibble(
    variable = missing_from_metadata,
    validation_status = "Dataset variable missing from metadata"
  ),
  tibble::tibble(
    variable = missing_from_dataset,
    validation_status = "Metadata variable missing from dataset"
  )
) %>%
  dplyr::arrange(
    validation_status,
    variable
  )

readr::write_csv(
  dictionary_validation,
  file.path(
    documentation_dir,
    "03_dictionary_validation.csv"
  )
)

if (length(missing_from_metadata) > 0) {
  stop(
    "Dataset variables missing from metadata: ",
    paste(missing_from_metadata, collapse = ", ")
  )
}

if (length(missing_from_dataset) > 0) {
  warning(
    "Configured variables missing from dataset: ",
    paste(missing_from_dataset, collapse = ", ")
  )
}


# =============================================================================
# 8. Build complete data dictionary
# =============================================================================

available_metadata <- metadata %>%
  dplyr::filter(
    variable %in% dataset_variables
  )

observed_summary <- purrr::map_dfr(
  available_metadata$variable,
  function(variable_name) {
    
    x <- survey[[variable_name]]
    
    tibble::tibble(
      variable = variable_name,
      observed_type = observed_type(x),
      n_rows = length(x),
      n_missing = sum(is.na(x)),
      pct_missing = round(
        100 * mean(is.na(x)),
        2
      ),
      n_non_missing = sum(!is.na(x)),
      n_unique = dplyr::n_distinct(
        x,
        na.rm = TRUE
      ),
      minimum = safe_minimum(x),
      maximum = safe_maximum(x),
      mean = safe_mean(x),
      median = safe_median(x),
      standard_deviation = safe_standard_deviation(x),
      example_values = example_values(x)
    )
  }
)

data_dictionary <- available_metadata %>%
  dplyr::left_join(
    observed_summary,
    by = "variable"
  ) %>%
  dplyr::mutate(
    missingness_status = dplyr::case_when(
      !allow_missing & n_missing > 0 ~
        "Unexpected missing values",
      pct_missing >= 50 ~
        "High missingness",
      pct_missing > 0 ~
        "Missing values present",
      TRUE ~
        "Complete"
    ),
    inclusion_status = dplyr::case_when(
      include_segmentation & include_profiling ~
        "Segmentation and profiling",
      include_segmentation ~
        "Segmentation",
      include_profiling ~
        "Profiling",
      TRUE ~
        "Not included in analysis"
    )
  ) %>%
  dplyr::select(
    variable,
    label,
    role,
    group,
    data_type,
    observed_type,
    scale,
    include_segmentation,
    include_profiling,
    inclusion_status,
    allow_missing,
    n_rows,
    n_missing,
    pct_missing,
    missingness_status,
    n_non_missing,
    n_unique,
    minimum,
    maximum,
    mean,
    median,
    standard_deviation,
    example_values,
    description
  ) %>%
  dplyr::arrange(
    factor(
      role,
      levels = c(
        "metadata",
        "segmentation",
        "profiling"
      )
    ),
    group,
    variable
  )

readr::write_csv(
  data_dictionary,
  file.path(
    documentation_dir,
    "03_data_dictionary.csv"
  )
)


# =============================================================================
# 9. Export role-specific dictionaries
# =============================================================================

segmentation_dictionary <- data_dictionary %>%
  dplyr::filter(
    include_segmentation
  )

profiling_dictionary <- data_dictionary %>%
  dplyr::filter(
    include_profiling
  )

metadata_dictionary <- data_dictionary %>%
  dplyr::filter(
    role == "metadata"
  )

readr::write_csv(
  segmentation_dictionary,
  file.path(
    documentation_dir,
    "03_segmentation_dictionary.csv"
  )
)

readr::write_csv(
  profiling_dictionary,
  file.path(
    documentation_dir,
    "03_profiling_dictionary.csv"
  )
)

readr::write_csv(
  metadata_dictionary,
  file.path(
    documentation_dir,
    "03_metadata_dictionary.csv"
  )
)


# =============================================================================
# 10. Export categorical and ordinal levels
# =============================================================================

level_variables <- available_metadata %>%
  dplyr::filter(
    data_type %in% c(
      "categorical",
      "ordinal",
      "binary"
    )
  ) %>%
  dplyr::pull(variable)

variable_levels <- purrr::map_dfr(
  level_variables,
  function(variable_name) {
    
    x <- survey[[variable_name]]
    
    level_table <- tibble::tibble(
      value = as.character(x)
    ) %>%
      dplyr::count(
        value,
        name = "n",
        .drop = FALSE
      ) %>%
      dplyr::mutate(
        pct = round(
          100 * n / sum(n),
          2
        ),
        variable = variable_name,
        missing = is.na(value)
      ) %>%
      dplyr::select(
        variable,
        value,
        missing,
        n,
        pct
      )
    
    if (nrow(level_table) > maximum_levels_to_export) {
      level_table <- level_table %>%
        dplyr::slice_max(
          order_by = n,
          n = maximum_levels_to_export,
          with_ties = FALSE
        )
    }
    
    level_table
  }
)

readr::write_csv(
  variable_levels,
  file.path(
    documentation_dir,
    "03_variable_levels.csv"
  )
)


# =============================================================================
# 11. Export numeric summary
# =============================================================================

numeric_summary <- data_dictionary %>%
  dplyr::filter(
    observed_type %in% c(
      "numeric",
      "integer"
    )
  ) %>%
  dplyr::select(
    variable,
    label,
    role,
    group,
    data_type,
    scale,
    n_missing,
    pct_missing,
    n_unique,
    minimum,
    maximum,
    mean,
    median,
    standard_deviation
  )

readr::write_csv(
  numeric_summary,
  file.path(
    documentation_dir,
    "03_numeric_summary.csv"
  )
)


# =============================================================================
# 12. Export variable-group summary
# =============================================================================

variable_group_summary <- data_dictionary %>%
  dplyr::group_by(
    role,
    group
  ) %>%
  dplyr::summarise(
    n_variables = dplyr::n(),
    n_segmentation_variables = sum(
      include_segmentation
    ),
    n_profiling_variables = sum(
      include_profiling
    ),
    mean_missingness_pct = round(
      mean(
        pct_missing,
        na.rm = TRUE
      ),
      2
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    role,
    group
  )

readr::write_csv(
  variable_group_summary,
  file.path(
    documentation_dir,
    "03_variable_group_summary.csv"
  )
)


# =============================================================================
# 13. Export dictionary summary
# =============================================================================

dictionary_summary <- tibble::tibble(
  item = c(
    "Rows in dataset",
    "Variables in dataset",
    "Variables in metadata",
    "Matched variables",
    "Metadata variables",
    "Segmentation variables",
    "Profiling variables",
    "Categorical variables",
    "Ordinal variables",
    "Binary variables",
    "Numeric variables",
    "Text variables",
    "Variables with missing values",
    "Variables with unexpected missing values"
  ),
  value = c(
    nrow(survey),
    ncol(survey),
    nrow(metadata),
    nrow(data_dictionary),
    sum(data_dictionary$role == "metadata"),
    sum(data_dictionary$include_segmentation),
    sum(data_dictionary$include_profiling),
    sum(data_dictionary$data_type == "categorical"),
    sum(data_dictionary$data_type == "ordinal"),
    sum(data_dictionary$data_type == "binary"),
    sum(data_dictionary$data_type == "numeric"),
    sum(data_dictionary$data_type == "text"),
    sum(data_dictionary$n_missing > 0),
    sum(
      data_dictionary$missingness_status ==
        "Unexpected missing values"
    )
  )
)

readr::write_csv(
  dictionary_summary,
  file.path(
    documentation_dir,
    "03_dictionary_summary.csv"
  )
)


# =============================================================================
# 14. Save reusable object
# =============================================================================

saveRDS(
  data_dictionary,
  file.path(
    project_dir,
    "data",
    "processed",
    "03_data_dictionary.rds"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    logs_dir,
    "03_sessionInfo.txt"
  )
)


# =============================================================================
# 15. Final report
# =============================================================================

write_log(
  "Data dictionary completed successfully.",
  log_file = log_file
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — DATA DICTIONARY COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Rows:                          ",
  nrow(survey),
  "\n",
  "Documented variables:          ",
  nrow(data_dictionary),
  "\n",
  "Metadata variables:            ",
  sum(data_dictionary$role == "metadata"),
  "\n",
  "Segmentation variables:        ",
  sum(data_dictionary$include_segmentation),
  "\n",
  "Profiling variables:           ",
  sum(data_dictionary$include_profiling),
  "\n",
  "Variables with missing values: ",
  sum(data_dictionary$n_missing > 0),
  "\n",
  "\nMain dictionary output:\n",
  file.path(
    documentation_dir,
    "03_data_dictionary.csv"
  ),
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)