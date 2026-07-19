# =============================================================================
# 02_data_preprocessing.R
# Behavioural Segmentation Toolkit
#
# Inputs:
#   data/processed/survey_audited.rds
#   config/variable_metadata.csv
#
# Outputs:
#   data/processed/survey_preprocessed.rds
#   data/processed/segmentation_data_full.rds
#   data/processed/profiling_data.rds
#   data/processed/metadata_data.rds
#   data/processed/02_segmentation_variable_names.rds
#   outputs/tables/02_configuration_validation.csv
#   outputs/tables/02_variable_summary.csv
#   outputs/tables/02_binary_conversion_summary.csv
#   outputs/tables/02_ordinal_validation.csv
#   outputs/tables/02_preprocessing_summary.csv
#   outputs/logs/02_data_preprocessing.log
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

maximum_segmentation_missingness_pct <- 40

binary_yes_values <- c(
  "yes",
  "y",
  "true",
  "1",
  "selected"
)

binary_no_values <- c(
  "no",
  "n",
  "false",
  "0",
  "not selected"
)


# =============================================================================
# 3. Helpers
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


convert_binary <- function(x) {
  
  value <- stringr::str_to_lower(
    clean_text(x)
  )
  
  dplyr::case_when(
    value %in% binary_yes_values ~ 1L,
    value %in% binary_no_values ~ 0L,
    is.na(value) ~ NA_integer_,
    TRUE ~ NA_integer_
  )
}


convert_numeric <- function(x) {
  
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  
  suppressWarnings(
    as.numeric(
      clean_text(x)
    )
  )
}


parse_scale <- function(scale_text) {
  
  values <- stringr::str_extract_all(
    scale_text,
    "-?\\d+(?:\\.\\d+)?"
  )[[1]]
  
  values <- suppressWarnings(
    as.numeric(values)
  )
  
  if (length(values) < 2) {
    return(
      c(NA_real_, NA_real_)
    )
  }
  
  c(
    min(values),
    max(values)
  )
}


summarise_variable <- function(
    data,
    metadata,
    variable_name
) {
  
  x <- data[[variable_name]]
  
  metadata_row <- metadata %>%
    dplyr::filter(
      variable == variable_name
    )
  
  tibble::tibble(
    variable = variable_name,
    label = metadata_row$label[1],
    role = metadata_row$role[1],
    group = metadata_row$group[1],
    configured_type = metadata_row$data_type[1],
    observed_type = class(x)[1],
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
    )
  )
}


# =============================================================================
# 4. Paths
# =============================================================================

project_dir <- find_project_root()

config_file <- file.path(
  project_dir,
  "config",
  "variable_metadata.csv"
)

input_file <- file.path(
  project_dir,
  "data",
  "processed",
  "survey_audited.rds"
)

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

logs_dir <- file.path(
  project_dir,
  "outputs",
  "logs"
)

create_directory(data_processed_dir)
create_directory(tables_dir)
create_directory(logs_dir)

log_file <- file.path(
  logs_dir,
  "02_data_preprocessing.log"
)

if (file.exists(log_file)) {
  file.remove(log_file)
}


# =============================================================================
# 5. Load inputs
# =============================================================================

if (!file.exists(input_file)) {
  stop(
    "Input file not found: ",
    input_file,
    "\nRun 01_load_and_audit.R first."
  )
}

if (!file.exists(config_file)) {
  stop(
    "Configuration file not found: ",
    config_file
  )
}

survey <- readRDS(
  input_file
) %>%
  tibble::as_tibble()

metadata <- readr::read_csv(
  config_file,
  show_col_types = FALSE,
  na = c("", "NA", "N/A")
) %>%
  janitor::clean_names()


# =============================================================================
# 6. Validate metadata
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
    "Duplicate variables in metadata: ",
    paste(
      duplicate_variables$variable,
      collapse = ", "
    )
  )
}

allowed_roles <- c(
  "metadata",
  "segmentation",
  "profiling"
)

allowed_types <- c(
  "identifier",
  "numeric",
  "ordinal",
  "binary",
  "categorical",
  "text"
)

invalid_roles <- setdiff(
  unique(metadata$role),
  allowed_roles
)

invalid_types <- setdiff(
  unique(metadata$data_type),
  allowed_types
)

if (length(invalid_roles) > 0) {
  stop(
    "Invalid metadata roles: ",
    paste(invalid_roles, collapse = ", ")
  )
}

if (length(invalid_types) > 0) {
  stop(
    "Invalid metadata data types: ",
    paste(invalid_types, collapse = ", ")
  )
}

missing_from_metadata <- setdiff(
  names(survey),
  metadata$variable
)

missing_from_dataset <- setdiff(
  metadata$variable,
  names(survey)
)

configuration_validation <- dplyr::bind_rows(
  tibble::tibble(
    variable = intersect(
      names(survey),
      metadata$variable
    ),
    status = "Matched"
  ),
  tibble::tibble(
    variable = missing_from_metadata,
    status = "Dataset variable missing from metadata"
  ),
  tibble::tibble(
    variable = missing_from_dataset,
    status = "Metadata variable missing from dataset"
  )
)

readr::write_csv(
  configuration_validation,
  file.path(
    tables_dir,
    "02_configuration_validation.csv"
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
# 7. Preprocess variables
# =============================================================================

survey_preprocessed <- survey %>%
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      clean_text
    )
  )

available_metadata <- metadata %>%
  dplyr::filter(
    variable %in% names(survey_preprocessed)
  )

binary_variables <- available_metadata %>%
  dplyr::filter(
    data_type == "binary"
  ) %>%
  dplyr::pull(variable)

numeric_variables <- available_metadata %>%
  dplyr::filter(
    data_type %in% c(
      "numeric",
      "ordinal"
    )
  ) %>%
  dplyr::pull(variable)

categorical_variables <- available_metadata %>%
  dplyr::filter(
    data_type == "categorical"
  ) %>%
  dplyr::pull(variable)

binary_conversion_summary <- purrr::map_dfr(
  binary_variables,
  function(variable_name) {
    
    original <- survey_preprocessed[[variable_name]]
    converted <- convert_binary(original)
    
    original_clean <- clean_text(original)
    
    unrecognised <- unique(
      original_clean[
        !is.na(original_clean) &
          is.na(converted)
      ]
    )
    
    survey_preprocessed[[variable_name]] <<-
      converted
    
    tibble::tibble(
      variable = variable_name,
      n_zero = sum(converted == 0, na.rm = TRUE),
      n_one = sum(converted == 1, na.rm = TRUE),
      n_missing = sum(is.na(converted)),
      n_unrecognised = length(unrecognised),
      unrecognised_values = ifelse(
        length(unrecognised) == 0,
        NA_character_,
        paste(unrecognised, collapse = " | ")
      )
    )
  }
)

for (variable_name in numeric_variables) {
  survey_preprocessed[[variable_name]] <-
    convert_numeric(
      survey_preprocessed[[variable_name]]
    )
}

if (length(categorical_variables) > 0) {
  survey_preprocessed <- survey_preprocessed %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(categorical_variables),
        as.factor
      )
    )
}

readr::write_csv(
  binary_conversion_summary,
  file.path(
    tables_dir,
    "02_binary_conversion_summary.csv"
  )
)


# =============================================================================
# 8. Validate ordinal variables
# =============================================================================

ordinal_metadata <- available_metadata %>%
  dplyr::filter(
    data_type == "ordinal"
  )

ordinal_validation <- purrr::map_dfr(
  seq_len(nrow(ordinal_metadata)),
  function(index) {
    
    variable_name <-
      ordinal_metadata$variable[index]
    
    scale_bounds <- parse_scale(
      ordinal_metadata$scale[index]
    )
    
    x <- survey_preprocessed[[variable_name]]
    
    invalid_indicator <-
      !is.na(x) &
      !is.na(scale_bounds[1]) &
      !is.na(scale_bounds[2]) &
      (
        x < scale_bounds[1] |
          x > scale_bounds[2]
      )
    
    invalid_values <- unique(
      x[invalid_indicator]
    )
    
    tibble::tibble(
      variable = variable_name,
      scale = ordinal_metadata$scale[index],
      minimum_expected = scale_bounds[1],
      maximum_expected = scale_bounds[2],
      minimum_observed = ifelse(
        all(is.na(x)),
        NA_real_,
        min(x, na.rm = TRUE)
      ),
      maximum_observed = ifelse(
        all(is.na(x)),
        NA_real_,
        max(x, na.rm = TRUE)
      ),
      n_invalid = sum(invalid_indicator),
      invalid_values = ifelse(
        length(invalid_values) == 0,
        NA_character_,
        paste(invalid_values, collapse = " | ")
      )
    )
  }
)

readr::write_csv(
  ordinal_validation,
  file.path(
    tables_dir,
    "02_ordinal_validation.csv"
  )
)

if (
  nrow(ordinal_validation) > 0 &&
  any(ordinal_validation$n_invalid > 0)
) {
  warning(
    "Ordinal values outside configured scales were detected."
  )
}


# =============================================================================
# 9. Define variable sets
# =============================================================================

identifier_variables <- available_metadata %>%
  dplyr::filter(
    data_type == "identifier"
  ) %>%
  dplyr::pull(variable)

if ("respondent_id" %in% names(survey_preprocessed)) {
  
  primary_identifier <- "respondent_id"
  
} else if (length(identifier_variables) > 0) {
  
  primary_identifier <- identifier_variables[1]
  
} else {
  
  stop(
    "No identifier variable is available."
  )
}

if (
  anyDuplicated(
    survey_preprocessed[[primary_identifier]]
  ) > 0
) {
  stop(
    "Identifier contains duplicate values: ",
    primary_identifier
  )
}

if (
  any(
    is.na(
      survey_preprocessed[[primary_identifier]]
    )
  )
) {
  stop(
    "Identifier contains missing values: ",
    primary_identifier
  )
}

segmentation_candidates <- available_metadata %>%
  dplyr::filter(
    include_segmentation
  ) %>%
  dplyr::pull(variable)

segmentation_missingness <- tibble::tibble(
  variable = segmentation_candidates,
  pct_missing = purrr::map_dbl(
    segmentation_candidates,
    function(variable_name) {
      round(
        100 * mean(
          is.na(
            survey_preprocessed[[variable_name]]
          )
        ),
        2
      )
    }
  )
)

excluded_high_missingness <- segmentation_missingness %>%
  dplyr::filter(
    pct_missing >
      maximum_segmentation_missingness_pct
  )

segmentation_variables <- segmentation_missingness %>%
  dplyr::filter(
    pct_missing <=
      maximum_segmentation_missingness_pct
  ) %>%
  dplyr::pull(variable)

zero_variance_variables <-
  segmentation_variables[
    purrr::map_lgl(
      segmentation_variables,
      function(variable_name) {
        dplyr::n_distinct(
          survey_preprocessed[[variable_name]],
          na.rm = TRUE
        ) <= 1
      }
    )
  ]

segmentation_variables <- setdiff(
  segmentation_variables,
  zero_variance_variables
)

profiling_variables <- available_metadata %>%
  dplyr::filter(
    include_profiling
  ) %>%
  dplyr::pull(variable)

metadata_variables <- available_metadata %>%
  dplyr::filter(
    role == "metadata"
  ) %>%
  dplyr::pull(variable)


# =============================================================================
# 10. Create output datasets
# =============================================================================

segmentation_data_full <- survey_preprocessed %>%
  dplyr::select(
    dplyr::all_of(primary_identifier),
    dplyr::all_of(segmentation_variables)
  )

profiling_data <- survey_preprocessed %>%
  dplyr::select(
    dplyr::all_of(primary_identifier),
    dplyr::all_of(
      setdiff(
        profiling_variables,
        primary_identifier
      )
    )
  )

metadata_data <- survey_preprocessed %>%
  dplyr::select(
    dplyr::all_of(
      unique(
        c(
          primary_identifier,
          metadata_variables
        )
      )
    )
  )


# =============================================================================
# 11. Diagnostics
# =============================================================================

variable_summary <- purrr::map_dfr(
  available_metadata$variable,
  function(variable_name) {
    
    summarise_variable(
      data = survey_preprocessed,
      metadata = available_metadata,
      variable_name = variable_name
    )
  }
)

readr::write_csv(
  variable_summary,
  file.path(
    tables_dir,
    "02_variable_summary.csv"
  )
)

readr::write_csv(
  excluded_high_missingness,
  file.path(
    tables_dir,
    "02_excluded_high_missingness_variables.csv"
  )
)

readr::write_csv(
  tibble::tibble(
    variable = zero_variance_variables
  ),
  file.path(
    tables_dir,
    "02_zero_variance_segmentation_variables.csv"
  )
)

preprocessing_summary <- tibble::tibble(
  item = c(
    "Rows",
    "Columns",
    "Configured variables",
    "Segmentation candidates",
    "Segmentation variables retained",
    "Profiling variables",
    "Metadata variables",
    "Binary variables",
    "Ordinal variables",
    "High-missingness exclusions",
    "Zero-variance exclusions"
  ),
  value = c(
    nrow(survey_preprocessed),
    ncol(survey_preprocessed),
    nrow(available_metadata),
    length(segmentation_candidates),
    length(segmentation_variables),
    length(profiling_variables),
    length(metadata_variables),
    length(binary_variables),
    nrow(ordinal_metadata),
    nrow(excluded_high_missingness),
    length(zero_variance_variables)
  )
)

readr::write_csv(
  preprocessing_summary,
  file.path(
    tables_dir,
    "02_preprocessing_summary.csv"
  )
)


# =============================================================================
# 12. Save outputs
# =============================================================================

saveRDS(
  survey_preprocessed,
  file.path(
    data_processed_dir,
    "survey_preprocessed.rds"
  )
)

saveRDS(
  segmentation_data_full,
  file.path(
    data_processed_dir,
    "segmentation_data_full.rds"
  )
)

saveRDS(
  profiling_data,
  file.path(
    data_processed_dir,
    "profiling_data.rds"
  )
)

saveRDS(
  metadata_data,
  file.path(
    data_processed_dir,
    "metadata_data.rds"
  )
)

saveRDS(
  segmentation_variables,
  file.path(
    data_processed_dir,
    "02_segmentation_variable_names.rds"
  )
)

saveRDS(
  available_metadata,
  file.path(
    data_processed_dir,
    "02_variable_metadata.rds"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    logs_dir,
    "02_sessionInfo.txt"
  )
)


# =============================================================================
# 13. Final report
# =============================================================================

write_log(
  "Preprocessing completed successfully.",
  log_file = log_file
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — PREPROCESSING COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Rows:                          ",
  nrow(survey_preprocessed),
  "\n",
  "Total columns:                 ",
  ncol(survey_preprocessed),
  "\n",
  "Configured variables:          ",
  nrow(available_metadata),
  "\n",
  "Segmentation variables:        ",
  length(segmentation_variables),
  "\n",
  "Profiling variables:           ",
  length(profiling_variables),
  "\n",
  "Binary variables converted:    ",
  length(binary_variables),
  "\n",
  "Ordinal variables processed:   ",
  nrow(ordinal_metadata),
  "\n",
  "High-missingness exclusions:   ",
  nrow(excluded_high_missingness),
  "\n",
  "Zero-variance exclusions:      ",
  length(zero_variance_variables),
  "\n",
  "\nMain modelling output:\n",
  file.path(
    data_processed_dir,
    "segmentation_data_full.rds"
  ),
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)