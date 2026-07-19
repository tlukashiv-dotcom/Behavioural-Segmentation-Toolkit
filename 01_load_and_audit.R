# =============================================================================
# 01_load_and_audit.R
# Behavioural Segmentation Toolkit
#
# Purpose:
#   - Identify the project root
#   - Create the required directory structure
#   - Load a behavioural survey dataset
#   - Standardise variable names
#   - Perform structural and data-quality checks
#   - Produce reusable audit tables and figures
#   - Save the imported dataset for subsequent pipeline stages
#
# Expected input:
#   data/raw/synthetic_behavioural_survey.csv
#
# Main outputs:
#   data/processed/survey_raw.rds
#   data/processed/survey_audited.rds
#   outputs/tables/01_dataset_summary.csv
#   outputs/tables/01_variable_audit.csv
#   outputs/tables/01_missingness_summary.csv
#   outputs/tables/01_duplicate_rows.csv
#   outputs/tables/01_potential_id_variables.csv
#   outputs/figures/01_missingness_by_variable.png
#   outputs/logs/01_load_and_audit.log
# =============================================================================


# =============================================================================
# 0. Clean environment
# =============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  warn = 1,
  scipen = 999
)


# =============================================================================
# 1. Required packages
# =============================================================================

required_packages <- c(
  "readr",
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "janitor",
  "ggplot2",
  "tibble"
)

installed_packages <- rownames(installed.packages())

missing_packages <- setdiff(
  required_packages,
  installed_packages
)

if (length(missing_packages) > 0) {
  message(
    "Installing missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
  
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

# Avoid common namespace conflicts
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
summarise <- dplyr::summarise
arrange <- dplyr::arrange
rename <- dplyr::rename
count <- dplyr::count
left_join <- dplyr::left_join


# =============================================================================
# 2. User configuration
# =============================================================================

# The toolkit supports CSV, XLSX, XLS, RDS and SAV input files.
#
# Change this value when using a different dataset.
input_filename <- "synthetic_behavioural_survey.csv"

# Optional Excel sheet name or index.
# Ignored for non-Excel files.
excel_sheet <- 1

# Input text encoding.
# Common alternatives:
#   "UTF-8"
#   "Windows-1252"
#   "ISO-8859-1"
input_encoding <- "UTF-8"

# Delimiter used only when importing delimited text files.
# Use NULL for automatic detection.
input_delimiter <- NULL

# Should imported variable names be converted to snake_case?
clean_variable_names <- TRUE

# Should completely empty rows be removed?
remove_empty_rows <- TRUE

# Should completely empty columns be removed?
remove_empty_columns <- TRUE

# Missingness threshold used to flag variables.
high_missingness_threshold <- 50

# Uniqueness threshold for identifying possible ID variables.
# A variable is flagged when the percentage of unique non-missing values
# is equal to or greater than this threshold.
potential_id_threshold <- 95

# Maximum number of example values stored per variable.
maximum_example_values <- 8

# Number of variables shown in the missingness figure.
maximum_variables_in_missingness_plot <- 40


# =============================================================================
# 3. Helper functions
# =============================================================================

find_project_root <- function(start_dir = getwd()) {
  
  current_dir <- normalizePath(
    start_dir,
    winslash = "/",
    mustWork = TRUE
  )
  
  project_markers <- c(
    ".git",
    "data",
    "R",
    "scripts",
    "README.md"
  )
  
  for (iteration in seq_len(10)) {
    
    markers_found <- vapply(
      project_markers,
      function(marker) {
        file.exists(file.path(current_dir, marker)) ||
          dir.exists(file.path(current_dir, marker))
      },
      logical(1)
    )
    
    if (
      dir.exists(file.path(current_dir, "data")) ||
      sum(markers_found) >= 2
    ) {
      return(current_dir)
    }
    
    parent_dir <- dirname(current_dir)
    
    if (identical(parent_dir, current_dir)) {
      break
    }
    
    current_dir <- parent_dir
  }
  
  message(
    "Project root could not be identified automatically. ",
    "Using the current working directory."
  )
  
  normalizePath(
    start_dir,
    winslash = "/",
    mustWork = TRUE
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
  
  message_text <- paste0(...)
  
  timestamped_message <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " | ",
    message_text
  )
  
  cat(
    timestamped_message,
    "\n",
    file = log_file,
    append = TRUE
  )
  
  message(message_text)
}


clean_character_missing_values <- function(data) {
  
  data %>%
    mutate(
      across(
        where(is.character),
        ~ {
          cleaned <- stringr::str_squish(.x)
          
          cleaned[
            cleaned %in% c(
              "",
              "NA",
              "N/A",
              "n/a",
              "NULL",
              "null",
              "Missing",
              "missing",
              "Not stated",
              "Prefer not to say"
            )
          ] <- NA_character_
          
          cleaned
        }
      )
    )
}


detect_variable_type <- function(x) {
  
  if (inherits(x, "Date")) {
    return("date")
  }
  
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return("datetime")
  }
  
  if (is.logical(x)) {
    return("logical")
  }
  
  if (is.factor(x)) {
    return("factor")
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


classify_variable_role <- function(
    variable_name,
    variable_type,
    n_unique,
    pct_unique,
    pct_missing
) {
  
  variable_name_lower <- tolower(variable_name)
  
  case_when(
    stringr::str_detect(
      variable_name_lower,
      "(^id$|_id$|^id_|respondent|participant|transaction|case_number)"
    ) ~ "Potential identifier",
    
    pct_unique >= potential_id_threshold &&
      n_unique > 20 ~ "Potential identifier",
    
    pct_missing >= high_missingness_threshold ~
      "High-missingness variable",
    
    variable_type %in% c("numeric", "integer") &&
      n_unique == 2 ~ "Binary numeric",
    
    variable_type %in% c("numeric", "integer") &&
      n_unique <= 10 ~ "Ordinal or categorical numeric",
    
    variable_type %in% c("numeric", "integer") ~
      "Continuous numeric",
    
    variable_type %in% c("factor", "character") &&
      n_unique == 2 ~ "Binary categorical",
    
    variable_type %in% c("factor", "character") &&
      n_unique <= 20 ~ "Categorical",
    
    variable_type %in% c("factor", "character") ~
      "High-cardinality categorical",
    
    variable_type %in% c("date", "datetime") ~
      "Date or time",
    
    TRUE ~ "Review"
  )
}


extract_example_values <- function(
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
  
  values <- head(
    values,
    maximum_values
  )
  
  paste(
    values,
    collapse = " | "
  )
}


read_input_dataset <- function(
    filepath,
    excel_sheet = 1,
    encoding = "UTF-8",
    delimiter = NULL
) {
  
  extension <- tolower(
    tools::file_ext(filepath)
  )
  
  if (extension == "csv") {
    
    if (is.null(delimiter)) {
      return(
        readr::read_csv(
          filepath,
          locale = readr::locale(
            encoding = encoding
          ),
          show_col_types = FALSE,
          progress = FALSE
        )
      )
    }
    
    return(
      readr::read_delim(
        filepath,
        delim = delimiter,
        locale = readr::locale(
          encoding = encoding
        ),
        show_col_types = FALSE,
        progress = FALSE
      )
    )
  }
  
  if (extension %in% c("txt", "tsv")) {
    
    detected_delimiter <- ifelse(
      is.null(delimiter),
      "\t",
      delimiter
    )
    
    return(
      readr::read_delim(
        filepath,
        delim = detected_delimiter,
        locale = readr::locale(
          encoding = encoding
        ),
        show_col_types = FALSE,
        progress = FALSE
      )
    )
  }
  
  if (extension %in% c("xlsx", "xls")) {
    
    return(
      readxl::read_excel(
        filepath,
        sheet = excel_sheet
      )
    )
  }
  
  if (extension == "rds") {
    return(
      readRDS(filepath)
    )
  }
  
  if (extension == "sav") {
    
    if (!requireNamespace("haven", quietly = TRUE)) {
      install.packages("haven")
    }
    
    return(
      haven::read_sav(filepath)
    )
  }
  
  stop(
    "Unsupported input file format: .",
    extension,
    "\nSupported formats are CSV, TXT, TSV, XLSX, XLS, RDS and SAV."
  )
}


safe_numeric_summary <- function(x) {
  
  if (!is.numeric(x)) {
    return(
      tibble(
        minimum = NA_real_,
        first_quartile = NA_real_,
        median = NA_real_,
        mean = NA_real_,
        third_quartile = NA_real_,
        maximum = NA_real_,
        standard_deviation = NA_real_
      )
    )
  }
  
  non_missing_x <- x[!is.na(x)]
  
  if (length(non_missing_x) == 0) {
    return(
      tibble(
        minimum = NA_real_,
        first_quartile = NA_real_,
        median = NA_real_,
        mean = NA_real_,
        third_quartile = NA_real_,
        maximum = NA_real_,
        standard_deviation = NA_real_
      )
    )
  }
  
  tibble(
    minimum = min(non_missing_x),
    first_quartile = as.numeric(
      stats::quantile(non_missing_x, 0.25)
    ),
    median = stats::median(non_missing_x),
    mean = mean(non_missing_x),
    third_quartile = as.numeric(
      stats::quantile(non_missing_x, 0.75)
    ),
    maximum = max(non_missing_x),
    standard_deviation = stats::sd(non_missing_x)
  )
}


# =============================================================================
# 4. Project paths
# =============================================================================

project_dir <- find_project_root()

data_dir <- file.path(
  project_dir,
  "data"
)

data_raw_dir <- file.path(
  data_dir,
  "raw"
)

data_processed_dir <- file.path(
  data_dir,
  "processed"
)

outputs_dir <- file.path(
  project_dir,
  "outputs"
)

tables_dir <- file.path(
  outputs_dir,
  "tables"
)

figures_dir <- file.path(
  outputs_dir,
  "figures"
)

logs_dir <- file.path(
  outputs_dir,
  "logs"
)

models_dir <- file.path(
  outputs_dir,
  "models"
)

reports_dir <- file.path(
  outputs_dir,
  "reports"
)

directories <- c(
  data_dir,
  data_raw_dir,
  data_processed_dir,
  outputs_dir,
  tables_dir,
  figures_dir,
  logs_dir,
  models_dir,
  reports_dir
)

invisible(
  lapply(
    directories,
    create_directory
  )
)

input_file <- file.path(
  data_raw_dir,
  input_filename
)

log_file <- file.path(
  logs_dir,
  "01_load_and_audit.log"
)

if (file.exists(log_file)) {
  file.remove(log_file)
}

write_log(
  "Behavioural Segmentation Toolkit: data audit started.",
  log_file = log_file
)

write_log(
  "Project directory: ",
  project_dir,
  log_file = log_file
)

write_log(
  "Input file: ",
  input_file,
  log_file = log_file
)


# =============================================================================
# 5. Validate input file
# =============================================================================

if (!file.exists(input_file)) {
  
  available_files <- list.files(
    data_raw_dir,
    full.names = FALSE
  )
  
  available_text <- if (
    length(available_files) == 0
  ) {
    "No files were found in data/raw."
  } else {
    paste(
      available_files,
      collapse = ", "
    )
  }
  
  stop(
    "Input file not found:\n",
    input_file,
    "\n\nAvailable files in data/raw:\n",
    available_text,
    "\n\nUpdate 'input_filename' near the beginning of this script."
  )
}


# =============================================================================
# 6. Load dataset
# =============================================================================

survey_imported <- read_input_dataset(
  filepath = input_file,
  excel_sheet = excel_sheet,
  encoding = input_encoding,
  delimiter = input_delimiter
)

survey_imported <- tibble::as_tibble(
  survey_imported
)

original_variable_names <- names(
  survey_imported
)

write_log(
  "Dataset imported successfully.",
  log_file = log_file
)

write_log(
  "Imported rows: ",
  format(nrow(survey_imported), big.mark = ","),
  log_file = log_file
)

write_log(
  "Imported columns: ",
  format(ncol(survey_imported), big.mark = ","),
  log_file = log_file
)


# =============================================================================
# 7. Clean variable names
# =============================================================================

if (clean_variable_names) {
  
  survey_raw <- survey_imported %>%
    janitor::clean_names()
  
} else {
  
  survey_raw <- survey_imported
}

variable_name_mapping <- tibble(
  original_variable = original_variable_names,
  cleaned_variable = names(survey_raw)
)

readr::write_csv(
  variable_name_mapping,
  file.path(
    tables_dir,
    "01_variable_name_mapping.csv"
  )
)


# =============================================================================
# 8. Remove empty rows and columns
# =============================================================================

rows_before_cleaning <- nrow(
  survey_raw
)

columns_before_cleaning <- ncol(
  survey_raw
)

if (remove_empty_rows) {
  survey_raw <- survey_raw %>%
    janitor::remove_empty(
      which = "rows"
    )
}

if (remove_empty_columns) {
  survey_raw <- survey_raw %>%
    janitor::remove_empty(
      which = "cols"
    )
}

empty_rows_removed <- rows_before_cleaning -
  nrow(survey_raw)

empty_columns_removed <- columns_before_cleaning -
  ncol(survey_raw)

write_log(
  "Completely empty rows removed: ",
  empty_rows_removed,
  log_file = log_file
)

write_log(
  "Completely empty columns removed: ",
  empty_columns_removed,
  log_file = log_file
)


# =============================================================================
# 9. Standardise character missing values
# =============================================================================

survey_audited <- clean_character_missing_values(
  survey_raw
)


# =============================================================================
# 10. Add toolkit respondent identifier
# =============================================================================

if (!"respondent_id" %in% names(survey_audited)) {
  
  survey_audited <- survey_audited %>%
    mutate(
      respondent_id = dplyr::row_number(),
      .before = 1
    )
  
  respondent_id_created <- TRUE
  
} else {
  
  respondent_id_created <- FALSE
}

write_log(
  "Toolkit respondent ID created: ",
  respondent_id_created,
  log_file = log_file
)


# =============================================================================
# 11. Dataset-level audit
# =============================================================================

number_of_rows <- nrow(
  survey_audited
)

number_of_columns <- ncol(
  survey_audited
)

total_cells <- number_of_rows *
  number_of_columns

total_missing_values <- sum(
  is.na(survey_audited)
)

overall_missingness_pct <- if (
  total_cells == 0
) {
  NA_real_
} else {
  round(
    100 * total_missing_values / total_cells,
    2
  )
}

duplicated_row_indicator <- duplicated(
  survey_audited
)

number_of_duplicate_rows <- sum(
  duplicated_row_indicator
)

dataset_summary <- tibble(
  item = c(
    "Input file",
    "Rows imported",
    "Columns imported",
    "Rows after cleaning",
    "Columns after cleaning",
    "Empty rows removed",
    "Empty columns removed",
    "Toolkit respondent ID created",
    "Total missing values",
    "Overall missingness percentage",
    "Duplicate rows",
    "Audit timestamp"
  ),
  value = c(
    input_filename,
    rows_before_cleaning,
    columns_before_cleaning,
    number_of_rows,
    number_of_columns,
    empty_rows_removed,
    empty_columns_removed,
    respondent_id_created,
    total_missing_values,
    overall_missingness_pct,
    number_of_duplicate_rows,
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    )
  )
)

readr::write_csv(
  dataset_summary,
  file.path(
    tables_dir,
    "01_dataset_summary.csv"
  )
)


# =============================================================================
# 12. Variable-level audit
# =============================================================================

variable_audit <- purrr::map_dfr(
  names(survey_audited),
  function(variable_name) {
    
    x <- survey_audited[[variable_name]]
    
    variable_type <- detect_variable_type(x)
    
    n_missing <- sum(
      is.na(x)
    )
    
    n_non_missing <- sum(
      !is.na(x)
    )
    
    n_unique <- dplyr::n_distinct(
      x,
      na.rm = TRUE
    )
    
    pct_missing <- if (
      number_of_rows == 0
    ) {
      NA_real_
    } else {
      round(
        100 * n_missing / number_of_rows,
        2
      )
    }
    
    pct_unique <- if (
      n_non_missing == 0
    ) {
      NA_real_
    } else {
      round(
        100 * n_unique / n_non_missing,
        2
      )
    }
    
    numeric_summary <- safe_numeric_summary(
      x
    )
    
    tibble(
      variable = variable_name,
      variable_type = variable_type,
      n_missing = n_missing,
      pct_missing = pct_missing,
      n_non_missing = n_non_missing,
      n_unique = n_unique,
      pct_unique_non_missing = pct_unique,
      constant_variable = n_unique <= 1,
      example_values = extract_example_values(x),
      suggested_role = classify_variable_role(
        variable_name = variable_name,
        variable_type = variable_type,
        n_unique = n_unique,
        pct_unique = pct_unique,
        pct_missing = pct_missing
      ),
      minimum = numeric_summary$minimum,
      first_quartile = numeric_summary$first_quartile,
      median = numeric_summary$median,
      mean = numeric_summary$mean,
      third_quartile = numeric_summary$third_quartile,
      maximum = numeric_summary$maximum,
      standard_deviation = numeric_summary$standard_deviation
    )
  }
) %>%
  arrange(
    desc(pct_missing),
    variable
  )

readr::write_csv(
  variable_audit,
  file.path(
    tables_dir,
    "01_variable_audit.csv"
  )
)


# =============================================================================
# 13. Missingness summary
# =============================================================================

missingness_summary <- variable_audit %>%
  select(
    variable,
    variable_type,
    n_missing,
    pct_missing,
    n_non_missing
  ) %>%
  arrange(
    desc(pct_missing),
    variable
  )

readr::write_csv(
  missingness_summary,
  file.path(
    tables_dir,
    "01_missingness_summary.csv"
  )
)

high_missingness_variables <- variable_audit %>%
  filter(
    pct_missing >= high_missingness_threshold
  ) %>%
  select(
    variable,
    variable_type,
    n_missing,
    pct_missing,
    suggested_role
  )

readr::write_csv(
  high_missingness_variables,
  file.path(
    tables_dir,
    "01_high_missingness_variables.csv"
  )
)


# =============================================================================
# 14. Constant and zero-variance variables
# =============================================================================

constant_variables <- variable_audit %>%
  filter(
    constant_variable
  ) %>%
  select(
    variable,
    variable_type,
    n_missing,
    n_unique,
    example_values
  )

readr::write_csv(
  constant_variables,
  file.path(
    tables_dir,
    "01_constant_variables.csv"
  )
)


# =============================================================================
# 15. Potential identifier variables
# =============================================================================

potential_id_variables <- variable_audit %>%
  filter(
    suggested_role == "Potential identifier"
  ) %>%
  select(
    variable,
    variable_type,
    n_non_missing,
    n_unique,
    pct_unique_non_missing,
    example_values
  )

readr::write_csv(
  potential_id_variables,
  file.path(
    tables_dir,
    "01_potential_id_variables.csv"
  )
)


# =============================================================================
# 16. Duplicate-row audit
# =============================================================================

duplicate_rows <- survey_audited[
  duplicated_row_indicator |
    duplicated(
      survey_audited,
      fromLast = TRUE
    ),
  ,
  drop = FALSE
] %>%
  mutate(
    audit_row_number = dplyr::row_number(),
    .before = 1
  )

readr::write_csv(
  duplicate_rows,
  file.path(
    tables_dir,
    "01_duplicate_rows.csv"
  )
)


# =============================================================================
# 17. Categorical-level audit
# =============================================================================

categorical_variables <- variable_audit %>%
  filter(
    variable_type %in% c(
      "character",
      "factor",
      "logical"
    ),
    n_unique <= 50
  ) %>%
  pull(variable)

if (length(categorical_variables) > 0) {
  
  categorical_level_audit <- purrr::map_dfr(
    categorical_variables,
    function(variable_name) {
      
      survey_audited %>%
        count(
          value = .data[[variable_name]],
          name = "n",
          .drop = FALSE
        ) %>%
        mutate(
          variable = variable_name,
          pct = round(
            100 * n / sum(n),
            2
          ),
          value = as.character(value)
        ) %>%
        select(
          variable,
          value,
          n,
          pct
        )
    }
  )
  
} else {
  
  categorical_level_audit <- tibble(
    variable = character(),
    value = character(),
    n = integer(),
    pct = numeric()
  )
}

readr::write_csv(
  categorical_level_audit,
  file.path(
    tables_dir,
    "01_categorical_level_audit.csv"
  )
)


# =============================================================================
# 18. Numeric-variable audit
# =============================================================================

numeric_variable_audit <- variable_audit %>%
  filter(
    variable_type %in% c(
      "numeric",
      "integer"
    )
  ) %>%
  select(
    variable,
    n_missing,
    pct_missing,
    n_unique,
    minimum,
    first_quartile,
    median,
    mean,
    third_quartile,
    maximum,
    standard_deviation
  )

readr::write_csv(
  numeric_variable_audit,
  file.path(
    tables_dir,
    "01_numeric_variable_audit.csv"
  )
)


# =============================================================================
# 19. Missingness figure
# =============================================================================

missingness_plot_data <- missingness_summary %>%
  filter(
    pct_missing > 0
  ) %>%
  slice_max(
    order_by = pct_missing,
    n = maximum_variables_in_missingness_plot,
    with_ties = FALSE
  ) %>%
  mutate(
    variable = reorder(
      variable,
      pct_missing
    )
  )

if (nrow(missingness_plot_data) > 0) {
  
  missingness_plot <- ggplot(
    missingness_plot_data,
    aes(
      x = variable,
      y = pct_missing
    )
  ) +
    geom_col() +
    coord_flip() +
    geom_text(
      aes(
        label = paste0(
          round(pct_missing, 1),
          "%"
        )
      ),
      hjust = -0.1,
      size = 3
    ) +
    scale_y_continuous(
      limits = c(
        0,
        min(
          105,
          max(
            missingness_plot_data$pct_missing *
              1.15,
            10
          )
        )
      )
    ) +
    labs(
      title = "Variables with missing values",
      subtitle = paste0(
        "Top ",
        min(
          maximum_variables_in_missingness_plot,
          nrow(missingness_plot_data)
        ),
        " variables ranked by missingness"
      ),
      x = NULL,
      y = "Missing values (%)",
      caption = "Behavioural Segmentation Toolkit"
    ) +
    theme_minimal(
      base_size = 12
    ) +
    theme(
      plot.title = element_text(
        face = "bold"
      ),
      panel.grid.major.y = element_blank()
    )
  
  ggsave(
    filename = file.path(
      figures_dir,
      "01_missingness_by_variable.png"
    ),
    plot = missingness_plot,
    width = 10,
    height = max(
      6,
      0.25 * nrow(missingness_plot_data)
    ),
    dpi = 300,
    bg = "white"
  )
}


# =============================================================================
# 20. Save processed objects
# =============================================================================

saveRDS(
  survey_raw,
  file.path(
    data_processed_dir,
    "survey_raw.rds"
  )
)

saveRDS(
  survey_audited,
  file.path(
    data_processed_dir,
    "survey_audited.rds"
  )
)

saveRDS(
  variable_audit,
  file.path(
    data_processed_dir,
    "01_variable_audit.rds"
  )
)


# =============================================================================
# 21. Save reproducibility information
# =============================================================================

session_info_file <- file.path(
  logs_dir,
  "01_sessionInfo.txt"
)

capture.output(
  sessionInfo(),
  file = session_info_file
)


# =============================================================================
# 22. Final audit messages
# =============================================================================

write_log(
  "Final rows: ",
  format(number_of_rows, big.mark = ","),
  log_file = log_file
)

write_log(
  "Final columns: ",
  format(number_of_columns, big.mark = ","),
  log_file = log_file
)

write_log(
  "Overall missingness: ",
  overall_missingness_pct,
  "%",
  log_file = log_file
)

write_log(
  "Duplicate rows identified: ",
  number_of_duplicate_rows,
  log_file = log_file
)

write_log(
  "Variables with at least ",
  high_missingness_threshold,
  "% missingness: ",
  nrow(high_missingness_variables),
  log_file = log_file
)

write_log(
  "Constant variables identified: ",
  nrow(constant_variables),
  log_file = log_file
)

write_log(
  "Potential identifier variables identified: ",
  nrow(potential_id_variables),
  log_file = log_file
)

write_log(
  "Processed dataset saved to: ",
  file.path(
    data_processed_dir,
    "survey_audited.rds"
  ),
  log_file = log_file
)

write_log(
  "Data audit completed successfully.",
  log_file = log_file
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — DATA AUDIT COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Rows:                  ", number_of_rows, "\n",
  "Columns:               ", number_of_columns, "\n",
  "Overall missingness:   ", overall_missingness_pct, "%\n",
  "Duplicate rows:        ", number_of_duplicate_rows, "\n",
  "Constant variables:    ", nrow(constant_variables), "\n",
  "Potential identifiers:", nrow(potential_id_variables), "\n",
  "\nMain output:\n",
  file.path(
    data_processed_dir,
    "survey_audited.rds"
  ),
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)