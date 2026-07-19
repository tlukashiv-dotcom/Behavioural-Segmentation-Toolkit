# =============================================================================
# 17_presentation_generator.R
# Behavioural Segmentation Toolkit
#
# Purpose:
#   - Generate a client-ready PowerPoint presentation
#   - Summarise final segmentation, personas and intervention recommendations
#   - Use final tables and persona outputs from Stages 13–16
#
# Expected inputs:
#   outputs/final/13_segment_sizes.csv
#   outputs/final/14_persona_details.csv
#   outputs/final/15_intervention_strategy.csv
#   outputs/final/15_intervention_actions.csv
#   outputs/final/15_message_framework.csv
#   outputs/final/15_kpi_framework.csv
#   outputs/report/figures/16_segment_sizes.png
#   outputs/report/figures/16_implementation_priorities.png
#
# Main output:
#   outputs/presentation/17_behavioural_segmentation_presentation.pptx
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
  "tibble",
  "stringr",
  "officer",
  "flextable"
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

presentation_title <- "Behavioural Segmentation Toolkit"
presentation_subtitle <- "Final segments, personas and intervention recommendations"

output_filename <- "17_behavioural_segmentation_presentation.pptx"

brand_colour <- "#1F4E79"
accent_colour <- "#70AD47"
light_grey <- "#F2F2F2"
dark_grey <- "#404040"

max_actions_per_segment <- 4
max_kpis_per_segment <- 4


# =============================================================================
# 3. Helper functions
# =============================================================================

find_project_root <- function(start_dir = getwd()) {

  current <- normalizePath(
    start_dir,
    winslash = "/",
    mustWork = TRUE
  )

  for (i in seq_len(12)) {

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

  x <- as.character(x)

  x <- x[
    !is.na(x) &
      stringr::str_trim(x) != ""
  ]

  if (length(x) == 0) {
    return(default)
  }

  x[1]
}


safe_number <- function(x, digits = 1) {

  x <- suppressWarnings(
    as.numeric(x)
  )

  if (
    length(x) == 0 ||
    is.na(x[1])
  ) {
    return("NA")
  }

  formatC(
    x[1],
    format = "f",
    digits = digits
  )
}


normalise_filename <- function(x) {

  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}


add_title_box <- function(
    ppt,
    title,
    subtitle = NULL
) {

  ppt <- officer::ph_with(
    ppt,
    value = officer::fpar(
      officer::ftext(
        title,
        prop = officer::fp_text(
          font.size = 30,
          bold = TRUE,
          color = brand_colour
        )
      )
    ),
    location = officer::ph_location(
      left = 0.55,
      top = 0.35,
      width = 12.2,
      height = 0.55
    )
  )

  if (!is.null(subtitle)) {

    ppt <- officer::ph_with(
      ppt,
      value = officer::fpar(
        officer::ftext(
          subtitle,
          prop = officer::fp_text(
            font.size = 14,
            color = dark_grey
          )
        )
      ),
      location = officer::ph_location(
        left = 0.6,
        top = 0.95,
        width = 12.0,
        height = 0.35
      )
    )
  }

  ppt
}


add_footer <- function(
    ppt,
    slide_label = "Behavioural Segmentation Toolkit"
) {

  officer::ph_with(
    ppt,
    value = officer::fpar(
      officer::ftext(
        slide_label,
        prop = officer::fp_text(
          font.size = 8,
          color = "#808080"
        )
      )
    ),
    location = officer::ph_location(
      left = 0.55,
      top = 7.15,
      width = 12.2,
      height = 0.25
    )
  )
}


add_text_block <- function(
    ppt,
    text,
    left,
    top,
    width,
    height,
    font_size = 14,
    bold = FALSE,
    color = "#000000"
) {

  officer::ph_with(
    ppt,
    value = officer::fpar(
      officer::ftext(
        text,
        prop = officer::fp_text(
          font.size = font_size,
          bold = bold,
          color = color
        )
      )
    ),
    location = officer::ph_location(
      left = left,
      top = top,
      width = width,
      height = height
    )
  )
}


add_bullets <- function(
    ppt,
    bullets,
    left,
    top,
    width,
    height,
    font_size = 13
) {

  bullets <- bullets[
    !is.na(bullets) &
      bullets != ""
  ]

  text <- paste0(
    "\u2022 ",
    bullets,
    collapse = "\n"
  )

  add_text_block(
    ppt = ppt,
    text = text,
    left = left,
    top = top,
    width = width,
    height = height,
    font_size = font_size
  )
}


add_image_if_exists <- function(
    ppt,
    image_path,
    left,
    top,
    width,
    height
) {

  if (
    !is.na(image_path) &&
    file.exists(image_path)
  ) {

    ppt <- officer::ph_with(
      ppt,
      value = officer::external_img(
        image_path,
        width = width,
        height = height
      ),
      location = officer::ph_location(
        left = left,
        top = top,
        width = width,
        height = height
      )
    )

  } else {

    ppt <- add_text_block(
      ppt,
      "Figure not available",
      left,
      top,
      width,
      height,
      font_size = 12,
      color = "#808080"
    )
  }

  ppt
}


make_ft <- function(data, font_size = 9) {

  ft <- flextable::flextable(data)
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::fontsize(
    ft,
    size = font_size,
    part = "all"
  )
  ft <- flextable::bold(
    ft,
    part = "header"
  )
  ft <- flextable::bg(
    ft,
    part = "header",
    bg = brand_colour
  )
  ft <- flextable::color(
    ft,
    part = "header",
    color = "white"
  )
  ft <- flextable::autofit(ft)

  ft
}


clean_display_names <- function(data) {

  names(data) <- names(data) |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title()

  data
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

report_figures_dir <- file.path(
  project_dir,
  "outputs",
  "report",
  "figures"
)

presentation_dir <- file.path(
  project_dir,
  "outputs",
  "presentation"
)

logs_dir <- file.path(
  project_dir,
  "outputs",
  "logs"
)

personas_dir <- file.path(
  final_dir,
  "personas"
)

invisible(
  lapply(
    c(
      presentation_dir,
      logs_dir
    ),
    create_directory
  )
)

output_file <- file.path(
  presentation_dir,
  output_filename
)


# =============================================================================
# 5. Load data
# =============================================================================

segment_sizes <- safe_read_csv(
  file.path(
    final_dir,
    "13_segment_sizes.csv"
  )
)

persona_details <- safe_read_csv(
  file.path(
    final_dir,
    "14_persona_details.csv"
  )
)

intervention_strategy <- safe_read_csv(
  file.path(
    final_dir,
    "15_intervention_strategy.csv"
  )
)

intervention_actions <- safe_read_csv(
  file.path(
    final_dir,
    "15_intervention_actions.csv"
  )
)

message_framework <- safe_read_csv(
  file.path(
    final_dir,
    "15_message_framework.csv"
  )
)

kpi_framework <- safe_read_csv(
  file.path(
    final_dir,
    "15_kpi_framework.csv"
  )
)

implementation_priorities <- safe_read_csv(
  file.path(
    final_dir,
    "15_implementation_priorities.csv"
  )
)

if (
  nrow(segment_sizes) == 0 ||
  nrow(persona_details) == 0 ||
  nrow(intervention_strategy) == 0
) {
  stop(
    "Required final outputs are missing. ",
    "Run Stages 13, 14 and 15 before Stage 17."
  )
}

segment_size_figure <- file.path(
  report_figures_dir,
  "16_segment_sizes.png"
)

priority_figure <- file.path(
  report_figures_dir,
  "16_implementation_priorities.png"
)


# =============================================================================
# 6. Create presentation
# =============================================================================

ppt <- officer::read_pptx()

ppt <- officer::layout_summary(ppt) |>
  dplyr::slice(1) |>
  dplyr::pull(layout) |>
  (\(layout_name) officer::read_pptx())()

# -------------------------------------------------------------------------
# Slide 1: Title
# -------------------------------------------------------------------------

ppt <- officer::add_slide(
  ppt,
  layout = "Title Slide",
  master = "Office Theme"
)

ppt <- officer::ph_with(
  ppt,
  value = presentation_title,
  location = officer::ph_location_type(
    type = "ctrTitle"
  )
)

ppt <- officer::ph_with(
  ppt,
  value = paste0(
    presentation_subtitle,
    "\nGenerated on ",
    format(Sys.Date(), "%d %B %Y")
  ),
  location = officer::ph_location_type(
    type = "subTitle"
  )
)


# -------------------------------------------------------------------------
# Slide 2: Executive summary
# -------------------------------------------------------------------------

ppt <- officer::add_slide(
  ppt,
  layout = "Blank",
  master = "Office Theme"
)

ppt <- add_title_box(
  ppt,
  "Executive summary",
  "Validated behavioural segmentation and action plan"
)

summary_bullets <- c(
  paste0(
    "Final solution contains ",
    nrow(segment_sizes),
    " actionable segments."
  ),
  paste0(
    "Largest segment: ",
    segment_sizes |>
      dplyr::slice_max(
        order_by = share_pct,
        n = 1
      ) |>
      dplyr::pull(working_name),
    " (",
    segment_sizes |>
      dplyr::slice_max(
        order_by = share_pct,
        n = 1
      ) |>
      dplyr::pull(share_pct),
    "%)."
  ),
  "The toolkit generates segment profiles, personas, recommendations, channels and KPIs.",
  "The outputs are intended for campaign planning, service design and intervention prioritisation."
)

ppt <- add_bullets(
  ppt,
  summary_bullets,
  left = 0.7,
  top = 1.55,
  width = 5.7,
  height = 4.7,
  font_size = 16
)

ppt <- add_image_if_exists(
  ppt,
  segment_size_figure,
  left = 6.6,
  top = 1.45,
  width = 6.0,
  height = 4.6
)

ppt <- add_footer(ppt)


# -------------------------------------------------------------------------
# Slide 3: Methodology
# -------------------------------------------------------------------------

ppt <- officer::add_slide(
  ppt,
  layout = "Blank",
  master = "Office Theme"
)

ppt <- add_title_box(
  ppt,
  "Methodology",
  "From survey data to validated behavioural segments"
)

workflow_steps <- c(
  "Data audit and preprocessing",
  "Feature screening and candidate feature sets",
  "Distance and model comparison",
  "Bootstrap stability validation",
  "Final model selection and segment profiling",
  "Personas, intervention recommendations and report generation"
)

ppt <- add_bullets(
  ppt,
  workflow_steps,
  left = 0.8,
  top = 1.4,
  width = 11.6,
  height = 5.1,
  font_size = 18
)

ppt <- add_footer(ppt)


# -------------------------------------------------------------------------
# Slide 4: Final segment sizes
# -------------------------------------------------------------------------

ppt <- officer::add_slide(
  ppt,
  layout = "Blank",
  master = "Office Theme"
)

ppt <- add_title_box(
  ppt,
  "Final segment sizes",
  "Three segments with practical scale for targeting"
)

ppt <- add_image_if_exists(
  ppt,
  segment_size_figure,
  left = 0.8,
  top = 1.35,
  width = 6.4,
  height = 4.9
)

segment_size_table <- segment_sizes |>
  dplyr::select(
    segment,
    working_name,
    n,
    share_pct
  ) |>
  clean_display_names()

ft_sizes <- make_ft(
  segment_size_table,
  font_size = 10
)

ppt <- officer::ph_with(
  ppt,
  ft_sizes,
  location = officer::ph_location(
    left = 7.35,
    top = 1.55,
    width = 5.25,
    height = 3.8
  )
)

ppt <- add_footer(ppt)


# -------------------------------------------------------------------------
# Slide 5+: One slide per segment
# -------------------------------------------------------------------------

for (index in seq_len(nrow(persona_details))) {

  segment_row <- persona_details[
    index,
    ,
    drop = FALSE
  ]

  segment_name <- safe_text(
    segment_row$working_name
  )

  persona_file_key <- normalise_filename(
    segment_name
  )

  persona_png <- file.path(
    personas_dir,
    paste0(
      "14_persona_",
      persona_file_key,
      ".png"
    )
  )

  ppt <- officer::add_slide(
    ppt,
    layout = "Blank",
    master = "Office Theme"
  )

  ppt <- add_title_box(
    ppt,
    segment_name,
    paste0(
      safe_text(segment_row$segment),
      " | ",
      safe_text(segment_row$n),
      " respondents | ",
      safe_number(segment_row$share_pct, 1),
      "%"
    )
  )

  ppt <- add_image_if_exists(
    ppt,
    persona_png,
    left = 0.55,
    top = 1.35,
    width = 5.7,
    height = 4.55
  )

  narrative_bullets <- c(
    safe_text(
      segment_row$short_narrative
    ),
    paste0(
      "Key characteristics: ",
      safe_text(
        segment_row$key_characteristics
      )
    ),
    paste0(
      "Recommended message: ",
      safe_text(
        segment_row$recommended_message
      )
    )
  )

  ppt <- add_bullets(
    ppt,
    narrative_bullets,
    left = 6.45,
    top = 1.35,
    width = 6.05,
    height = 4.6,
    font_size = 13
  )

  ppt <- add_footer(ppt)
}


# -------------------------------------------------------------------------
# Slide: Implementation priorities
# -------------------------------------------------------------------------

ppt <- officer::add_slide(
  ppt,
  layout = "Blank",
  master = "Office Theme"
)

ppt <- add_title_box(
  ppt,
  "Implementation priorities",
  "Recommended sequence for applying the segmentation"
)

ppt <- add_image_if_exists(
  ppt,
  priority_figure,
  left = 0.8,
  top = 1.35,
  width = 6.0,
  height = 4.7
)

priority_table <- implementation_priorities |>
  dplyr::select(
    overall_rank,
    working_name,
    implementation_priority,
    recommended_sequence
  ) |>
  clean_display_names()

ft_priority <- make_ft(
  priority_table,
  font_size = 8
)

ppt <- officer::ph_with(
  ppt,
  ft_priority,
  location = officer::ph_location(
    left = 6.9,
    top = 1.35,
    width = 5.75,
    height = 4.85
  )
)

ppt <- add_footer(ppt)


# -------------------------------------------------------------------------
# Slide: Intervention strategy
# -------------------------------------------------------------------------

ppt <- officer::add_slide(
  ppt,
  layout = "Blank",
  master = "Office Theme"
)

ppt <- add_title_box(
  ppt,
  "Intervention strategy by segment",
  "Core behavioural problem and strategic objective"
)

strategy_table <- intervention_strategy |>
  dplyr::select(
    working_name,
    implementation_priority,
    strategic_objective,
    suggested_support_intensity
  ) |>
  clean_display_names()

ft_strategy <- make_ft(
  strategy_table,
  font_size = 8
)

ppt <- officer::ph_with(
  ppt,
  ft_strategy,
  location = officer::ph_location(
    left = 0.65,
    top = 1.35,
    width = 12.05,
    height = 5.3
  )
)

ppt <- add_footer(ppt)


# -------------------------------------------------------------------------
# Slide: Recommended actions
# -------------------------------------------------------------------------

ppt <- officer::add_slide(
  ppt,
  layout = "Blank",
  master = "Office Theme"
)

ppt <- add_title_box(
  ppt,
  "Recommended actions",
  paste0(
    "Top ",
    max_actions_per_segment,
    " actions per segment"
  )
)

actions_table <- intervention_actions |>
  dplyr::group_by(
    segment
  ) |>
  dplyr::slice_head(
    n = max_actions_per_segment
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    working_name,
    action_rank,
    action_category,
    recommended_action,
    delivery_mode
  ) |>
  clean_display_names()

ft_actions <- make_ft(
  actions_table,
  font_size = 7
)

ppt <- officer::ph_with(
  ppt,
  ft_actions,
  location = officer::ph_location(
    left = 0.55,
    top = 1.25,
    width = 12.3,
    height = 5.85
  )
)

ppt <- add_footer(ppt)


# -------------------------------------------------------------------------
# Slide: Messaging framework
# -------------------------------------------------------------------------

ppt <- officer::add_slide(
  ppt,
  layout = "Blank",
  master = "Office Theme"
)

ppt <- add_title_box(
  ppt,
  "Messaging framework",
  "Primary message, proof point and call to action"
)

message_table <- message_framework |>
  dplyr::select(
    working_name,
    primary_message,
    call_to_action,
    tone,
    avoid
  ) |>
  clean_display_names()

ft_messages <- make_ft(
  message_table,
  font_size = 7.5
)

ppt <- officer::ph_with(
  ppt,
  ft_messages,
  location = officer::ph_location(
    left = 0.55,
    top = 1.35,
    width = 12.25,
    height = 5.4
  )
)

ppt <- add_footer(ppt)


# -------------------------------------------------------------------------
# Slide: KPI framework
# -------------------------------------------------------------------------

ppt <- officer::add_slide(
  ppt,
  layout = "Blank",
  master = "Office Theme"
)

ppt <- add_title_box(
  ppt,
  "KPI framework",
  paste0(
    "Top ",
    max_kpis_per_segment,
    " KPIs per segment"
  )
)

kpi_table <- kpi_framework |>
  dplyr::group_by(
    segment
  ) |>
  dplyr::slice_head(
    n = max_kpis_per_segment
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    working_name,
    kpi_rank,
    kpi_category,
    kpi_name,
    expected_direction
  ) |>
  clean_display_names()

ft_kpis <- make_ft(
  kpi_table,
  font_size = 8
)

ppt <- officer::ph_with(
  ppt,
  ft_kpis,
  location = officer::ph_location(
    left = 0.65,
    top = 1.35,
    width = 12.0,
    height = 5.4
  )
)

ppt <- add_footer(ppt)


# -------------------------------------------------------------------------
# Slide: Next steps
# -------------------------------------------------------------------------

ppt <- officer::add_slide(
  ppt,
  layout = "Blank",
  master = "Office Theme"
)

ppt <- add_title_box(
  ppt,
  "Recommended next steps",
  "Using the segmentation in practice"
)

next_steps <- c(
  "Review segment names and personas with subject-matter experts.",
  "Validate the recommendations with stakeholders and frontline teams.",
  "Pilot priority interventions with clear KPIs and comparison groups.",
  "Develop a short segment-assignment questionnaire for future use.",
  "Integrate the segmentation into campaign planning, service design and reporting."
)

ppt <- add_bullets(
  ppt,
  next_steps,
  left = 0.9,
  top = 1.55,
  width = 11.3,
  height = 5.2,
  font_size = 17
)

ppt <- add_footer(ppt)


# =============================================================================
# 7. Save presentation
# =============================================================================

print(
  ppt,
  target = output_file
)

capture.output(
  sessionInfo(),
  file = file.path(
    logs_dir,
    "17_sessionInfo.txt"
  )
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — PRESENTATION GENERATION COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Slides generated:               ",
  length(ppt),
  "\n",
  "Presentation file:\n",
  output_file,
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)
