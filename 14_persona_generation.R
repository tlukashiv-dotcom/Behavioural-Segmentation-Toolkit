# =============================================================================
# 14_persona_generation.R
# Behavioural Segmentation Toolkit
#
# Purpose:
#   - Load final segment narratives, recommendations and profile tables
#   - Generate structured personas for each final segment
#   - Create concise persona summaries for reports and presentations
#   - Export persona tables and one visual card per segment
#
# Important:
#   Persona content is generated from the observed segment profiles.
#   Working names and narratives should still be reviewed by subject experts.
#
# Expected inputs:
#   outputs/final/13_segment_sizes.csv
#   outputs/final/13_segment_numeric_profiles.csv
#   outputs/final/13_segment_categorical_profiles.csv
#   outputs/final/13_segment_narratives.csv
#   outputs/final/13_segment_recommendations.csv
#   outputs/final/13_segment_key_variables.csv
#
# Main outputs:
#   outputs/final/14_persona_summary.csv
#   outputs/final/14_persona_details.csv
#   outputs/final/14_persona_cards.pdf
#   outputs/final/personas/14_persona_<segment>.png
#   outputs/final/personas/14_persona_<segment>.pdf
#   data/processed/14_personas.rds
#   outputs/logs/14_persona_generation.log
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
  "stringr",
  "ggplot2",
  "grid",
  "gridExtra"
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

top_numeric_characteristics <- 4
top_categorical_characteristics <- 4

persona_quote_prefix <- "“"
persona_quote_suffix <- "”"


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

  stop("Project root not found.")
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


safe_read_csv <- function(path) {

  if (!file.exists(path)) {
    return(tibble::tibble())
  }

  readr::read_csv(
    path,
    show_col_types = FALSE
  )
}


safe_text <- function(x, default = "Not available") {

  x <- x[
    !is.na(x) &
      x != ""
  ]

  if (length(x) == 0) {
    return(default)
  }

  as.character(x[1])
}


safe_filename <- function(x) {

  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}


collapse_numeric_characteristics <- function(
    numeric_profiles,
    segment_name,
    n_features
) {

  if (nrow(numeric_profiles) == 0) {
    return("No numeric profiling variables available.")
  }

  numeric_profiles |>
    dplyr::filter(
      segment == segment_name,
      !is.na(standardised_mean)
    ) |>
    dplyr::mutate(
      display_label =
        dplyr::coalesce(
          label,
          variable
        )
    ) |>
    dplyr::slice_max(
      order_by =
        abs(
          standardised_mean
        ),
      n = n_features,
      with_ties = FALSE
    ) |>
    dplyr::mutate(
      text = paste0(
        display_label,
        " (",
        ifelse(
          standardised_mean >= 0,
          "+",
          ""
        ),
        round(
          standardised_mean,
          2
        ),
        " SD)"
      )
    ) |>
    dplyr::pull(text) |>
    paste(
      collapse = "; "
    )
}


collapse_categorical_characteristics <- function(
    categorical_profiles,
    segment_name,
    n_features
) {

  if (nrow(categorical_profiles) == 0) {
    return("No categorical profiling variables available.")
  }

  categorical_profiles |>
    dplyr::filter(
      segment == segment_name,
      !is.na(within_segment_share)
    ) |>
    dplyr::mutate(
      display_label =
        dplyr::coalesce(
          label,
          variable
        ),
      display_text = paste0(
        display_label,
        ": ",
        level,
        " (",
        round(
          100 *
            within_segment_share,
          1
        ),
        "%)"
      )
    ) |>
    dplyr::group_by(variable) |>
    dplyr::slice_max(
      order_by =
        within_segment_share,
      n = 1,
      with_ties = FALSE
    ) |>
    dplyr::ungroup() |>
    dplyr::slice_max(
      order_by =
        within_segment_share,
      n = n_features,
      with_ties = FALSE
    ) |>
    dplyr::pull(display_text) |>
    paste(
      collapse = "; "
    )
}


generate_persona_quote <- function(
    readiness,
    confidence,
    barriers
) {

  if (
    readiness == "High" &&
    confidence == "High"
  ) {
    return(
      paste0(
        persona_quote_prefix,
        "I am ready to act, as long as the next step is clear and easy.",
        persona_quote_suffix
      )
    )
  }

  if (
    confidence == "Low"
  ) {
    return(
      paste0(
        persona_quote_prefix,
        "I might change, but I need support and reassurance that I can succeed.",
        persona_quote_suffix
      )
    )
  }

  if (
    barriers == "High"
  ) {
    return(
      paste0(
        persona_quote_prefix,
        "Change feels difficult, expensive or inconvenient for me right now.",
        persona_quote_suffix
      )
    )
  }

  paste0(
    persona_quote_prefix,
    "I am open to change, but the support must fit my circumstances.",
    persona_quote_suffix
  )
}


wrap_text <- function(x, width = 70) {

  stringr::str_wrap(
    x,
    width = width
  )
}


make_persona_card <- function(persona_row) {

  title_grob <- grid::textGrob(
    persona_row$working_name,
    x = 0,
    hjust = 0,
    gp = grid::gpar(
      fontsize = 22,
      fontface = "bold"
    )
  )

  size_grob <- grid::textGrob(
    paste0(
      persona_row$segment,
      " | ",
      persona_row$n,
      " respondents (",
      persona_row$share_pct,
      "%)"
    ),
    x = 0,
    hjust = 0,
    gp = grid::gpar(
      fontsize = 12
    )
  )

  quote_grob <- grid::textGrob(
    wrap_text(
      persona_row$persona_quote,
      75
    ),
    x = 0,
    hjust = 0,
    gp = grid::gpar(
      fontsize = 13,
      fontface = "italic"
    )
  )

  overview_grob <- grid::textGrob(
    wrap_text(
      persona_row$short_narrative,
      85
    ),
    x = 0,
    hjust = 0,
    gp = grid::gpar(
      fontsize = 11
    )
  )

  characteristics_grob <- grid::textGrob(
    wrap_text(
      paste0(
        "Key characteristics\n",
        persona_row$key_characteristics
      ),
      85
    ),
    x = 0,
    hjust = 0,
    gp = grid::gpar(
      fontsize = 11
    )
  )

  profiling_grob <- grid::textGrob(
    wrap_text(
      paste0(
        "Profiling signals\n",
        persona_row$profiling_signals
      ),
      85
    ),
    x = 0,
    hjust = 0,
    gp = grid::gpar(
      fontsize = 11
    )
  )

  intervention_grob <- grid::textGrob(
    wrap_text(
      paste0(
        "Recommended approach\n",
        persona_row$primary_objective,
        "\n\nDelivery style: ",
        persona_row$suggested_delivery_style,
        "\nSupport intensity: ",
        persona_row$suggested_support_intensity,
        "\nCall to action: ",
        persona_row$recommended_call_to_action
      ),
      85
    ),
    x = 0,
    hjust = 0,
    gp = grid::gpar(
      fontsize = 11
    )
  )

  message_grob <- grid::textGrob(
    wrap_text(
      paste0(
        "Message\n",
        persona_row$recommended_message,
        "\n\nTone: ",
        persona_row$tone,
        "\nAvoid: ",
        persona_row$avoid
      ),
      85
    ),
    x = 0,
    hjust = 0,
    gp = grid::gpar(
      fontsize = 11
    )
  )

  gridExtra::arrangeGrob(
    title_grob,
    size_grob,
    quote_grob,
    overview_grob,
    characteristics_grob,
    profiling_grob,
    intervention_grob,
    message_grob,
    ncol = 1,
    heights = c(
      0.7,
      0.5,
      0.8,
      1.1,
      1.3,
      1.2,
      1.8,
      1.8
    ),
    padding = grid::unit(
      0.8,
      "line"
    )
  )
}


# =============================================================================
# 4. Paths
# =============================================================================

project_dir <- find_project_root()

final_dir <- file.path(
  project_dir,
  "outputs",
  "final"
)

personas_dir <- file.path(
  final_dir,
  "personas"
)

processed_dir <- file.path(
  project_dir,
  "data",
  "processed"
)

logs_dir <- file.path(
  project_dir,
  "outputs",
  "logs"
)

invisible(
  lapply(
    c(
      final_dir,
      personas_dir,
      processed_dir,
      logs_dir
    ),
    create_directory
  )
)

log_file <- file.path(
  logs_dir,
  "14_persona_generation.log"
)

if (file.exists(log_file)) {
  file.remove(log_file)
}


# =============================================================================
# 5. Load inputs
# =============================================================================

segment_sizes <- safe_read_csv(
  file.path(
    final_dir,
    "13_segment_sizes.csv"
  )
)

numeric_profiles <- safe_read_csv(
  file.path(
    final_dir,
    "13_segment_numeric_profiles.csv"
  )
)

categorical_profiles <- safe_read_csv(
  file.path(
    final_dir,
    "13_segment_categorical_profiles.csv"
  )
)

segment_narratives <- safe_read_csv(
  file.path(
    final_dir,
    "13_segment_narratives.csv"
  )
)

segment_recommendations <- safe_read_csv(
  file.path(
    final_dir,
    "13_segment_recommendations.csv"
  )
)

key_variables <- safe_read_csv(
  file.path(
    final_dir,
    "13_segment_key_variables.csv"
  )
)

required_tables <- list(
  segment_sizes = segment_sizes,
  segment_narratives = segment_narratives,
  segment_recommendations =
    segment_recommendations
)

empty_required <- names(required_tables)[
  vapply(
    required_tables,
    nrow,
    integer(1)
  ) == 0
]

if (length(empty_required) > 0) {
  stop(
    "Required Stage 13 tables are missing or empty: ",
    paste(
      empty_required,
      collapse = ", "
    )
  )
}


# =============================================================================
# 6. Build persona dataset
# =============================================================================

persona_details <- segment_sizes |>
  dplyr::left_join(
    segment_narratives,
    by = c(
      "segment",
      "working_name",
      "n",
      "share_pct"
    )
  ) |>
  dplyr::left_join(
    segment_recommendations,
    by = c(
      "segment",
      "working_name"
    )
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    key_characteristics =
      collapse_numeric_characteristics(
        numeric_profiles,
        segment,
        top_numeric_characteristics
      ),
    profiling_signals =
      collapse_categorical_characteristics(
        categorical_profiles,
        segment,
        top_categorical_characteristics
      ),
    persona_quote =
      generate_persona_quote(
        readiness_position,
        confidence_position,
        barrier_position
      )
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(segment)


persona_summary <- persona_details |>
  dplyr::select(
    segment,
    working_name,
    n,
    share_pct,
    persona_quote,
    short_narrative,
    key_characteristics,
    profiling_signals,
    primary_objective,
    recommended_message
  )


# =============================================================================
# 7. Save persona tables
# =============================================================================

readr::write_csv(
  persona_summary,
  file.path(
    final_dir,
    "14_persona_summary.csv"
  )
)

readr::write_csv(
  persona_details,
  file.path(
    final_dir,
    "14_persona_details.csv"
  )
)

saveRDS(
  persona_details,
  file.path(
    processed_dir,
    "14_personas.rds"
  )
)


# =============================================================================
# 8. Create persona cards
# =============================================================================

persona_cards <- purrr::map(
  seq_len(
    nrow(persona_details)
  ),
  function(index) {

    persona_row <- persona_details[
      index,
      ,
      drop = FALSE
    ]

    make_persona_card(
      persona_row
    )
  }
)

for (index in seq_along(persona_cards)) {

  persona_name <- safe_filename(
    persona_details$
      working_name[index]
  )

  png_file <- file.path(
    personas_dir,
    paste0(
      "14_persona_",
      persona_name,
      ".png"
    )
  )

  pdf_file <- file.path(
    personas_dir,
    paste0(
      "14_persona_",
      persona_name,
      ".pdf"
    )
  )

  grDevices::png(
    png_file,
    width = 1600,
    height = 1200,
    res = 160
  )

  grid::grid.newpage()
  grid::grid.draw(
    persona_cards[[index]]
  )

  grDevices::dev.off()

  grDevices::pdf(
    pdf_file,
    width = 11,
    height = 8.5
  )

  grid::grid.newpage()
  grid::grid.draw(
    persona_cards[[index]]
  )

  grDevices::dev.off()
}


# =============================================================================
# 9. Combined persona PDF
# =============================================================================

combined_pdf <- file.path(
  final_dir,
  "14_persona_cards.pdf"
)

grDevices::pdf(
  combined_pdf,
  width = 11,
  height = 8.5,
  onefile = TRUE
)

for (persona_card in persona_cards) {

  grid::grid.newpage()
  grid::grid.draw(
    persona_card
  )
}

grDevices::dev.off()


# =============================================================================
# 10. Final report
# =============================================================================

capture.output(
  sessionInfo(),
  file = file.path(
    logs_dir,
    "14_sessionInfo.txt"
  )
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — PERSONA GENERATION COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Personas generated:             ",
  nrow(persona_details),
  "\n",
  "Persona summary:\n",
  file.path(
    final_dir,
    "14_persona_summary.csv"
  ),
  "\n",
  "Combined persona PDF:\n",
  combined_pdf,
  "\n",
  "Individual persona files:\n",
  personas_dir,
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)
