# =============================================================================
# 12_segment_naming_and_narratives.R
# Behavioural Segmentation Toolkit
#
# Purpose:
#   - Load final segment profiles from Stage 11
#   - Identify the strongest positive and negative characteristics per segment
#   - Generate structured working names and concise segment narratives
#   - Summarise barriers, motivations, readiness, engagement and support needs
#   - Produce intervention implications and message recommendations
#
# Important:
#   Automatically generated names are working labels.
#   They should be reviewed by subject-matter experts before publication.
#
# Expected inputs:
#   outputs/tables/11_final_segment_profile_summary.csv
#   outputs/tables/11_segmentation_profiles.csv
#   outputs/tables/11_numeric_profiling_summary.csv
#   outputs/tables/11_categorical_profiling_summary.csv
#   outputs/tables/11_segment_distinguishing_variables.csv
#   data/processed/11_final_segment_profile_data.rds
#
# Main outputs:
#   outputs/tables/12_segment_names.csv
#   outputs/tables/12_segment_narratives.csv
#   outputs/tables/12_segment_intervention_implications.csv
#   outputs/tables/12_segment_message_recommendations.csv
#   outputs/tables/12_segment_naming_summary.csv
#   data/processed/12_segment_narratives.rds
#   outputs/figures/12_segment_narrative_cards.png
#   outputs/logs/12_segment_naming_and_narratives.log
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
  "ggplot2",
  "stringr"
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

segment_variable <- "segment"

top_positive_features <- 4
top_negative_features <- 4

strong_deviation_threshold <- 0.50
moderate_deviation_threshold <- 0.25


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


safe_first <- function(x, default = NA_character_) {

  x <- x[
    !is.na(x) &
      x != ""
  ]

  if (length(x) == 0) {
    return(default)
  }

  x[1]
}


collapse_features <- function(
    labels,
    values,
    digits = 2
) {

  if (length(labels) == 0) {
    return(NA_character_)
  }

  paste0(
    labels,
    " (",
    ifelse(
      values >= 0,
      "+",
      ""
    ),
    round(
      values,
      digits
    ),
    " SD)",
    collapse = "; "
  )
}


get_segment_value <- function(
    profiles,
    segment_name,
    label_pattern
) {

  result <- profiles %>%
    dplyr::filter(
      segment == segment_name,
      stringr::str_detect(
        stringr::str_to_lower(
          dplyr::coalesce(
            label,
            variable
          )
        ),
        label_pattern
      )
    ) %>%
    dplyr::arrange(
      dplyr::desc(
        abs(
          standardised_mean
        )
      )
    ) %>%
    dplyr::slice_head(
      n = 1
    ) %>%
    dplyr::pull(
      standardised_mean
    )

  if (length(result) == 0) {
    return(NA_real_)
  }

  result[1]
}


classify_level <- function(
    value,
    positive_label,
    neutral_label,
    negative_label
) {

  if (is.na(value)) {
    return(neutral_label)
  }

  if (value >= strong_deviation_threshold) {
    return(positive_label)
  }

  if (value <= -strong_deviation_threshold) {
    return(negative_label)
  }

  neutral_label
}


generate_working_name <- function(
    readiness,
    confidence,
    barriers,
    engagement
) {

  if (
    !is.na(readiness) &&
    readiness >= strong_deviation_threshold &&
    !is.na(confidence) &&
    confidence >= strong_deviation_threshold
  ) {
    return("Confident and Ready")
  }

  if (
    !is.na(readiness) &&
    readiness <= -strong_deviation_threshold &&
    !is.na(barriers) &&
    barriers >= strong_deviation_threshold
  ) {
    return("Barrier-Burdened Habituals")
  }

  if (
    !is.na(confidence) &&
    confidence <= -moderate_deviation_threshold
  ) {
    return("Low-Confidence Considerers")
  }

  if (
    !is.na(engagement) &&
    engagement >= strong_deviation_threshold &&
    !is.na(readiness) &&
    readiness < moderate_deviation_threshold
  ) {
    return("Engaged but Uncommitted")
  }

  "Mixed-Readiness Mainstream"
}


generate_short_narrative <- function(
    name,
    readiness,
    confidence,
    barriers,
    engagement,
    trust
) {

  readiness_text <- classify_level(
    readiness,
    "high readiness to change",
    "moderate readiness to change",
    "low readiness to change"
  )

  confidence_text <- classify_level(
    confidence,
    "strong confidence",
    "mixed confidence",
    "limited confidence"
  )

  barrier_text <- classify_level(
    barriers,
    "substantial barriers",
    "moderate barriers",
    "relatively few barriers"
  )

  engagement_text <- classify_level(
    engagement,
    "high behavioural engagement",
    "average behavioural engagement",
    "low behavioural engagement"
  )

  trust_text <- classify_level(
    trust,
    "high trust",
    "mixed trust",
    "low trust"
  )

  paste0(
    name,
    " are characterised by ",
    readiness_text,
    ", ",
    confidence_text,
    ", ",
    barrier_text,
    ", ",
    engagement_text,
    " and ",
    trust_text,
    "."
  )
}


generate_intervention_priority <- function(
    readiness,
    confidence,
    barriers
) {

  if (
    !is.na(readiness) &&
    readiness >= strong_deviation_threshold
  ) {
    return(
      "Convert intention into immediate action with simple pathways, rapid access and clear calls to action."
    )
  }

  if (
    !is.na(confidence) &&
    confidence <= -moderate_deviation_threshold
  ) {
    return(
      "Build self-efficacy through guided support, small achievable steps, reassurance and visible progress."
    )
  }

  if (
    !is.na(barriers) &&
    barriers >= strong_deviation_threshold
  ) {
    return(
      "Reduce practical and perceived barriers before asking for commitment; emphasise affordability, convenience and trust."
    )
  }

  "Maintain engagement and provide relevant, low-friction opportunities to progress."
}


generate_message_recommendation <- function(
    readiness,
    confidence,
    barriers,
    trust
) {

  if (
    !is.na(readiness) &&
    readiness >= strong_deviation_threshold
  ) {
    return(
      "You are ready to take the next step. Start now with a clear, simple action and support available when needed."
    )
  }

  if (
    !is.na(confidence) &&
    confidence <= -moderate_deviation_threshold
  ) {
    return(
      "You do not have to do everything at once. Small supported steps can make change feel manageable."
    )
  }

  if (
    !is.na(barriers) &&
    barriers >= strong_deviation_threshold
  ) {
    return(
      "Change can be practical and affordable. Focus on options that fit your routine and reduce unnecessary effort."
    )
  }

  if (
    !is.na(trust) &&
    trust <= -moderate_deviation_threshold
  ) {
    return(
      "Use clear, credible and transparent information, supported by trusted people and real-world evidence."
    )
  }

  "Choose the support option that best fits your needs and preferred way of engaging."
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
      logs_dir
    ),
    create_directory
  )
)

summary_file <- file.path(
  tables_dir,
  "11_final_segment_profile_summary.csv"
)

segmentation_profiles_file <- file.path(
  tables_dir,
  "11_segmentation_profiles.csv"
)

numeric_profiles_file <- file.path(
  tables_dir,
  "11_numeric_profiling_summary.csv"
)

categorical_profiles_file <- file.path(
  tables_dir,
  "11_categorical_profiling_summary.csv"
)

distinguishing_variables_file <- file.path(
  tables_dir,
  "11_segment_distinguishing_variables.csv"
)

full_profile_data_file <- file.path(
  data_processed_dir,
  "11_final_segment_profile_data.rds"
)

log_file <- file.path(
  logs_dir,
  "12_segment_naming_and_narratives.log"
)

if (file.exists(log_file)) {
  file.remove(log_file)
}


# =============================================================================
# 5. Load inputs
# =============================================================================

required_files <- c(
  summary_file,
  segmentation_profiles_file,
  distinguishing_variables_file,
  full_profile_data_file
)

missing_files <- required_files[
  !file.exists(
    required_files
  )
]

if (length(missing_files) > 0) {
  stop(
    "Required inputs are missing:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}

segment_summary <- readr::read_csv(
  summary_file,
  show_col_types = FALSE
)

segmentation_profiles <- readr::read_csv(
  segmentation_profiles_file,
  show_col_types = FALSE
)

distinguishing_variables <- readr::read_csv(
  distinguishing_variables_file,
  show_col_types = FALSE
)

full_profile_data <- readRDS(
  full_profile_data_file
) %>%
  tibble::as_tibble()

numeric_profiles <- if (
  file.exists(
    numeric_profiles_file
  )
) {
  readr::read_csv(
    numeric_profiles_file,
    show_col_types = FALSE
  )
} else {
  tibble::tibble()
}

categorical_profiles <- if (
  file.exists(
    categorical_profiles_file
  )
) {
  readr::read_csv(
    categorical_profiles_file,
    show_col_types = FALSE
  )
} else {
  tibble::tibble()
}


# =============================================================================
# 6. Generate segment names and narratives
# =============================================================================

segments <- unique(
  segmentation_profiles$segment
)

segment_narratives <- purrr::map_dfr(
  segments,
  function(segment_name) {

    segment_profiles <- segmentation_profiles %>%
      dplyr::filter(
        segment == segment_name
      ) %>%
      dplyr::mutate(
        display_label =
          dplyr::coalesce(
            label,
            variable
          )
      )

    readiness <- get_segment_value(
      segmentation_profiles,
      segment_name,
      "change intention|readiness"
    )

    confidence <- get_segment_value(
      segmentation_profiles,
      segment_name,
      "confidence score"
    )

    low_confidence_barrier <- get_segment_value(
      segmentation_profiles,
      segment_name,
      "low confidence barrier"
    )

    cost_barrier <- get_segment_value(
      segmentation_profiles,
      segment_name,
      "cost barrier"
    )

    barriers <- max(
      c(
        low_confidence_barrier,
        cost_barrier
      ),
      na.rm = TRUE
    )

    if (!is.finite(barriers)) {
      barriers <- NA_real_
    }

    engagement <- get_segment_value(
      segmentation_profiles,
      segment_name,
      "weekly behaviour frequency|engagement"
    )

    trust <- get_segment_value(
      segmentation_profiles,
      segment_name,
      "trust score"
    )

    working_name <- generate_working_name(
      readiness = readiness,
      confidence = confidence,
      barriers = barriers,
      engagement = engagement
    )

    positive_features <- segment_profiles %>%
      dplyr::filter(
        standardised_mean > 0
      ) %>%
      dplyr::slice_max(
        order_by =
          standardised_mean,
        n = top_positive_features,
        with_ties = FALSE
      )

    negative_features <- segment_profiles %>%
      dplyr::filter(
        standardised_mean < 0
      ) %>%
      dplyr::slice_min(
        order_by =
          standardised_mean,
        n = top_negative_features,
        with_ties = FALSE
      )

    tibble::tibble(
      segment = segment_name,
      working_name = working_name,
      short_narrative =
        generate_short_narrative(
          name = working_name,
          readiness = readiness,
          confidence = confidence,
          barriers = barriers,
          engagement = engagement,
          trust = trust
        ),
      readiness_position =
        classify_level(
          readiness,
          "High",
          "Moderate",
          "Low"
        ),
      confidence_position =
        classify_level(
          confidence,
          "High",
          "Moderate",
          "Low"
        ),
      barrier_position =
        classify_level(
          barriers,
          "High",
          "Moderate",
          "Low"
        ),
      engagement_position =
        classify_level(
          engagement,
          "High",
          "Moderate",
          "Low"
        ),
      trust_position =
        classify_level(
          trust,
          "High",
          "Moderate",
          "Low"
        ),
      strongest_positive_features =
        collapse_features(
          positive_features$
            display_label,
          positive_features$
            standardised_mean
        ),
      strongest_negative_features =
        collapse_features(
          negative_features$
            display_label,
          negative_features$
            standardised_mean
        ),
      intervention_priority =
        generate_intervention_priority(
          readiness = readiness,
          confidence = confidence,
          barriers = barriers
        ),
      recommended_message =
        generate_message_recommendation(
          readiness = readiness,
          confidence = confidence,
          barriers = barriers,
          trust = trust
        )
    )
  }
)


# =============================================================================
# 7. Add segment size and model context
# =============================================================================

segment_narratives <- segment_narratives %>%
  dplyr::left_join(
    segment_summary %>%
      dplyr::select(
        segment,
        n,
        share,
        share_pct,
        final_model_id,
        feature_set,
        clustering_method,
        number_of_segments
      ),
    by = "segment"
  ) %>%
  dplyr::arrange(
    segment
  )


# =============================================================================
# 8. Intervention implications
# =============================================================================

segment_intervention_implications <- segment_narratives %>%
  dplyr::transmute(
    segment,
    working_name,
    primary_objective =
      intervention_priority,
    suggested_delivery_style =
      dplyr::case_when(
        readiness_position == "High" ~
          "Direct, action-oriented and low-friction",
        confidence_position == "Low" ~
          "Supportive, guided and confidence-building",
        barrier_position == "High" ~
          "Practical, reassuring and barrier-reducing",
        TRUE ~
          "Relevant, flexible and personalised"
      ),
    suggested_support_intensity =
      dplyr::case_when(
        confidence_position == "Low" &
          barrier_position == "High" ~
          "High",
        readiness_position == "High" ~
          "Low to moderate",
        TRUE ~
          "Moderate"
      ),
    recommended_call_to_action =
      dplyr::case_when(
        readiness_position == "High" ~
          "Start now",
        confidence_position == "Low" ~
          "Take one supported step",
        barrier_position == "High" ~
          "Explore an easier option",
        TRUE ~
          "Learn more and choose support"
      )
  )


# =============================================================================
# 9. Message recommendations
# =============================================================================

segment_message_recommendations <- segment_narratives %>%
  dplyr::transmute(
    segment,
    working_name,
    recommended_message,
    tone =
      dplyr::case_when(
        trust_position == "Low" ~
          "Transparent and credibility-led",
        confidence_position == "Low" ~
          "Encouraging and non-judgemental",
        readiness_position == "High" ~
          "Decisive and action-focused",
        TRUE ~
          "Balanced and informative"
      ),
    avoid =
      dplyr::case_when(
        readiness_position == "Low" ~
          "Overly forceful calls to action",
        confidence_position == "Low" ~
          "Messages implying personal failure",
        barrier_position == "High" ~
          "Ignoring cost, time or convenience concerns",
        trust_position == "Low" ~
          "Unsupported claims or institutional jargon",
        TRUE ~
          "Generic one-size-fits-all messaging"
      )
  )


# =============================================================================
# 10. Save outputs
# =============================================================================

segment_names <- segment_narratives %>%
  dplyr::select(
    segment,
    working_name,
    n,
    share_pct
  )

readr::write_csv(
  segment_names,
  file.path(
    tables_dir,
    "12_segment_names.csv"
  )
)

readr::write_csv(
  segment_narratives,
  file.path(
    tables_dir,
    "12_segment_narratives.csv"
  )
)

readr::write_csv(
  segment_intervention_implications,
  file.path(
    tables_dir,
    "12_segment_intervention_implications.csv"
  )
)

readr::write_csv(
  segment_message_recommendations,
  file.path(
    tables_dir,
    "12_segment_message_recommendations.csv"
  )
)

segment_naming_summary <- segment_narratives %>%
  dplyr::select(
    segment,
    working_name,
    share_pct,
    readiness_position,
    confidence_position,
    barrier_position,
    engagement_position,
    trust_position,
    short_narrative,
    intervention_priority,
    recommended_message
  )

readr::write_csv(
  segment_naming_summary,
  file.path(
    tables_dir,
    "12_segment_naming_summary.csv"
  )
)

saveRDS(
  list(
    segment_narratives =
      segment_narratives,
    intervention_implications =
      segment_intervention_implications,
    message_recommendations =
      segment_message_recommendations
  ),
  file.path(
    data_processed_dir,
    "12_segment_narratives.rds"
  )
)


# =============================================================================
# 11. Narrative cards figure
# =============================================================================

card_data <- segment_narratives %>%
  dplyr::mutate(
    card_text = paste0(
      working_name,
      "\n",
      n,
      " respondents (",
      share_pct,
      "%)\n\n",
      short_narrative,
      "\n\nPriority: ",
      intervention_priority
    ),
    y_position =
      rev(
        seq_len(
          dplyr::n()
        )
      )
  )

card_plot <- ggplot2::ggplot(
  card_data
) +
  ggplot2::geom_label(
    ggplot2::aes(
      x = 1,
      y = y_position,
      label = card_text
    ),
    hjust = 0,
    vjust = 0.5,
    size = 4,
    label.size = 0.4
  ) +
  ggplot2::xlim(
    0.9,
    2.6
  ) +
  ggplot2::ylim(
    0.5,
    max(
      card_data$y_position
    ) + 0.5
  ) +
  ggplot2::labs(
    title = "Working segment names and narratives",
    subtitle = "Automatically generated labels for expert review"
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title =
      ggplot2::element_text(
        face = "bold",
        size = 16
      ),
    plot.subtitle =
      ggplot2::element_text(
        size = 11
      )
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "12_segment_narrative_cards.png"
  ),
  plot = card_plot,
  width = 12,
  height = max(
    7,
    2.5 *
      nrow(card_data)
  ),
  dpi = 300,
  bg = "white"
)


# =============================================================================
# 12. Final report
# =============================================================================

capture.output(
  sessionInfo(),
  file = file.path(
    logs_dir,
    "12_sessionInfo.txt"
  )
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — SEGMENT NAMING COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Segments named:                 ",
  nrow(segment_names),
  "\n",
  "Final model:                    ",
  safe_first(
    segment_narratives$
      final_model_id
  ),
  "\n",
  "\nWorking names:\n",
  paste0(
    segment_names$segment,
    ": ",
    segment_names$working_name,
    collapse = "\n"
  ),
  "\n",
  "\nMain naming summary:\n",
  file.path(
    tables_dir,
    "12_segment_naming_summary.csv"
  ),
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)
