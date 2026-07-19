# =============================================================================
# 13_final_segment_tables.R
# Behavioural Segmentation Toolkit
#
# Consolidates final segment outputs and creates a reporting workbook.
# =============================================================================

rm(list = ls())
gc()

options(stringsAsFactors = FALSE, warn = 1, scipen = 999)

required_packages <- c(
  "dplyr", "readr", "tidyr", "tibble", "stringr", "openxlsx"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

invisible(lapply(required_packages, library, character.only = TRUE))

maximum_key_variables <- 20
excel_filename <- "13_segment_summary.xlsx"


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

  stop("Project root not found.")
}


create_directory <- function(path) {

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(path)
}


safe_read_csv <- function(path) {

  if (!file.exists(path)) {
    return(tibble::tibble())
  }

  readr::read_csv(path, show_col_types = FALSE)
}


clean_column_names <- function(data) {

  names(data) <- names(data) |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title()

  data
}


format_percentage_columns <- function(data) {

  percentage_columns <- names(data)[
    stringr::str_detect(
      names(data),
      "share|percent|pct"
    )
  ]

  for (column_name in percentage_columns) {

    if (!is.numeric(data[[column_name]])) {
      next
    }

    non_missing <- data[[column_name]][
      !is.na(data[[column_name]])
    ]

    if (length(non_missing) == 0) {
      next
    }

    if (max(non_missing) <= 1) {
      data[[column_name]] <- round(
        100 * data[[column_name]],
        1
      )
    } else {
      data[[column_name]] <- round(
        data[[column_name]],
        1
      )
    }
  }

  data
}


add_excel_sheet <- function(
    workbook,
    sheet_name,
    data,
    header_style,
    first_column_style
) {

  if (nrow(data) == 0) {
    return(invisible(NULL))
  }

  openxlsx::addWorksheet(workbook, sheet_name)

  openxlsx::writeData(
    workbook,
    sheet = sheet_name,
    x = data,
    headerStyle = header_style,
    withFilter = TRUE
  )

  openxlsx::freezePane(
    workbook,
    sheet = sheet_name,
    firstRow = TRUE
  )

  openxlsx::setColWidths(
    workbook,
    sheet = sheet_name,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )

  openxlsx::addStyle(
    workbook,
    sheet = sheet_name,
    style = first_column_style,
    rows = 2:(nrow(data) + 1),
    cols = 1,
    gridExpand = TRUE,
    stack = TRUE
  )

  invisible(NULL)
}


project_dir <- find_project_root()

tables_dir <- file.path(project_dir, "outputs", "tables")
final_dir <- file.path(project_dir, "outputs", "final")
logs_dir <- file.path(project_dir, "outputs", "logs")

invisible(lapply(c(final_dir, logs_dir), create_directory))

segment_sizes <- safe_read_csv(
  file.path(tables_dir, "11_segment_sizes.csv")
)

segmentation_profiles <- safe_read_csv(
  file.path(tables_dir, "11_segmentation_profiles.csv")
)

numeric_profiles <- safe_read_csv(
  file.path(tables_dir, "11_numeric_profiling_summary.csv")
)

categorical_profiles <- safe_read_csv(
  file.path(tables_dir, "11_categorical_profiling_summary.csv")
)

difference_tests <- safe_read_csv(
  file.path(tables_dir, "11_segment_difference_tests.csv")
)

distinguishing_variables <- safe_read_csv(
  file.path(tables_dir, "11_segment_distinguishing_variables.csv")
)

final_profile_summary <- safe_read_csv(
  file.path(tables_dir, "11_final_segment_profile_summary.csv")
)

segment_names <- safe_read_csv(
  file.path(tables_dir, "12_segment_names.csv")
)

segment_narratives <- safe_read_csv(
  file.path(tables_dir, "12_segment_narratives.csv")
)

intervention_implications <- safe_read_csv(
  file.path(tables_dir, "12_segment_intervention_implications.csv")
)

message_recommendations <- safe_read_csv(
  file.path(tables_dir, "12_segment_message_recommendations.csv")
)


required_nonempty <- list(
  segment_sizes = segment_sizes,
  segmentation_profiles = segmentation_profiles,
  final_profile_summary = final_profile_summary,
  segment_names = segment_names,
  segment_narratives = segment_narratives
)

empty_required <- names(required_nonempty)[
  vapply(required_nonempty, nrow, integer(1)) == 0
]

if (length(empty_required) > 0) {
  stop(
    "Required Stage 11/12 tables are missing or empty: ",
    paste(empty_required, collapse = ", ")
  )
}


final_segment_sizes <- segment_sizes |>
  dplyr::left_join(
    segment_names |>
      dplyr::select(segment, working_name),
    by = "segment"
  ) |>
  dplyr::select(
    segment,
    working_name,
    n,
    share,
    share_pct
  ) |>
  dplyr::arrange(segment)

readr::write_csv(
  final_segment_sizes,
  file.path(final_dir, "13_segment_sizes.csv")
)


final_numeric_profiles <- numeric_profiles |>
  dplyr::select(
    dplyr::any_of(
      c(
        "segment", "variable", "label", "group",
        "mean", "sd", "median", "iqr",
        "overall_mean", "overall_sd",
        "standardised_mean", "segment_n"
      )
    )
  ) |>
  dplyr::left_join(
    segment_names |>
      dplyr::select(segment, working_name),
    by = "segment"
  ) |>
  dplyr::relocate(working_name, .after = segment) |>
  dplyr::arrange(variable, segment)

readr::write_csv(
  final_numeric_profiles,
  file.path(final_dir, "13_segment_numeric_profiles.csv")
)


final_categorical_profiles <- categorical_profiles |>
  dplyr::select(
    dplyr::any_of(
      c(
        "segment", "variable", "label", "group",
        "level", "n", "segment_total",
        "within_segment_share", "within_segment_pct"
      )
    )
  ) |>
  dplyr::left_join(
    segment_names |>
      dplyr::select(segment, working_name),
    by = "segment"
  ) |>
  dplyr::relocate(working_name, .after = segment) |>
  dplyr::arrange(
    variable,
    segment,
    dplyr::desc(within_segment_share)
  )

readr::write_csv(
  final_categorical_profiles,
  file.path(final_dir, "13_segment_categorical_profiles.csv")
)


final_key_variables <- distinguishing_variables |>
  dplyr::filter(!is.na(effect_size)) |>
  dplyr::slice_head(
    n = min(
      maximum_key_variables,
      nrow(distinguishing_variables)
    )
  ) |>
  dplyr::select(
    dplyr::any_of(
      c(
        "effect_rank", "variable", "label", "group",
        "variable_role", "test", "effect_size",
        "effect_size_name", "p_value",
        "adjusted_p_value", "statistically_significant"
      )
    )
  )

readr::write_csv(
  final_key_variables,
  file.path(final_dir, "13_segment_key_variables.csv")
)


final_segment_narratives <- segment_narratives |>
  dplyr::select(
    dplyr::any_of(
      c(
        "segment", "working_name", "n", "share_pct",
        "short_narrative", "readiness_position",
        "confidence_position", "barrier_position",
        "engagement_position", "trust_position",
        "strongest_positive_features",
        "strongest_negative_features"
      )
    )
  ) |>
  dplyr::arrange(segment)

readr::write_csv(
  final_segment_narratives,
  file.path(final_dir, "13_segment_narratives.csv")
)


final_segment_recommendations <- intervention_implications |>
  dplyr::full_join(
    message_recommendations,
    by = c("segment", "working_name")
  ) |>
  dplyr::select(
    dplyr::any_of(
      c(
        "segment", "working_name", "primary_objective",
        "suggested_delivery_style",
        "suggested_support_intensity",
        "recommended_call_to_action",
        "recommended_message", "tone", "avoid"
      )
    )
  ) |>
  dplyr::arrange(segment)

readr::write_csv(
  final_segment_recommendations,
  file.path(final_dir, "13_segment_recommendations.csv")
)


segmentation_profile_wide <- segmentation_profiles |>
  dplyr::mutate(
    display_label = dplyr::coalesce(label, variable)
  ) |>
  dplyr::select(
    group,
    display_label,
    segment,
    standardised_mean
  ) |>
  tidyr::pivot_wider(
    names_from = segment,
    values_from = standardised_mean
  ) |>
  dplyr::arrange(group, display_label)


final_segment_overview <- final_profile_summary |>
  dplyr::left_join(
    segment_names |>
      dplyr::select(segment, working_name),
    by = "segment"
  ) |>
  dplyr::left_join(
    segment_narratives |>
      dplyr::select(
        segment,
        short_narrative,
        intervention_priority,
        recommended_message
      ),
    by = "segment"
  ) |>
  dplyr::select(
    dplyr::any_of(
      c(
        "segment", "working_name", "n", "share_pct",
        "short_narrative", "strongest_characteristics",
        "intervention_priority", "recommended_message",
        "final_model_id", "feature_set",
        "clustering_method", "number_of_segments"
      )
    )
  ) |>
  dplyr::arrange(segment)


workbook <- openxlsx::createWorkbook(
  creator = "Behavioural Segmentation Toolkit"
)

header_style <- openxlsx::createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#404040",
  halign = "center",
  valign = "center",
  textDecoration = "bold",
  border = "Bottom"
)

first_column_style <- openxlsx::createStyle(
  textDecoration = "bold"
)

add_excel_sheet(
  workbook,
  "Overview",
  clean_column_names(
    format_percentage_columns(
      final_segment_overview
    )
  ),
  header_style,
  first_column_style
)

add_excel_sheet(
  workbook,
  "Segment Sizes",
  clean_column_names(
    format_percentage_columns(
      final_segment_sizes
    )
  ),
  header_style,
  first_column_style
)

add_excel_sheet(
  workbook,
  "Segmentation Profiles",
  clean_column_names(
    segmentation_profile_wide
  ),
  header_style,
  first_column_style
)

add_excel_sheet(
  workbook,
  "Numeric Profiles",
  clean_column_names(
    final_numeric_profiles
  ),
  header_style,
  first_column_style
)

add_excel_sheet(
  workbook,
  "Categorical Profiles",
  clean_column_names(
    format_percentage_columns(
      final_categorical_profiles
    )
  ),
  header_style,
  first_column_style
)

add_excel_sheet(
  workbook,
  "Key Variables",
  clean_column_names(
    final_key_variables
  ),
  header_style,
  first_column_style
)

add_excel_sheet(
  workbook,
  "Narratives",
  clean_column_names(
    final_segment_narratives
  ),
  header_style,
  first_column_style
)

add_excel_sheet(
  workbook,
  "Recommendations",
  clean_column_names(
    final_segment_recommendations
  ),
  header_style,
  first_column_style
)

add_excel_sheet(
  workbook,
  "Statistical Tests",
  clean_column_names(
    difference_tests
  ),
  header_style,
  first_column_style
)

excel_output_file <- file.path(
  final_dir,
  excel_filename
)

openxlsx::saveWorkbook(
  workbook,
  excel_output_file,
  overwrite = TRUE
)

capture.output(
  sessionInfo(),
  file = file.path(
    logs_dir,
    "13_sessionInfo.txt"
  )
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — FINAL SEGMENT TABLES COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Segments included:              ",
  nrow(final_segment_sizes),
  "\n",
  "Numeric profile rows:           ",
  nrow(final_numeric_profiles),
  "\n",
  "Categorical profile rows:       ",
  nrow(final_categorical_profiles),
  "\n",
  "Key variables retained:         ",
  nrow(final_key_variables),
  "\n",
  "Excel workbook:\n",
  excel_output_file,
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)
