# =============================================================================
# 15_intervention_recommendations.R
# Behavioural Segmentation Toolkit
#
# Purpose:
#   - Convert final segment profiles and personas into actionable strategies
#   - Generate segment-specific intervention recommendations
#   - Define behavioural objectives, channels, messages, service actions and KPIs
#   - Export implementation-ready tables for reports and presentations
#
# Expected inputs:
#   outputs/final/14_persona_details.csv
#   outputs/final/14_persona_summary.csv
#   outputs/final/13_segment_key_variables.csv
#   outputs/final/13_segment_numeric_profiles.csv
#   outputs/final/13_segment_categorical_profiles.csv
#
# Main outputs:
#   outputs/final/15_intervention_strategy.csv
#   outputs/final/15_intervention_actions.csv
#   outputs/final/15_message_framework.csv
#   outputs/final/15_channel_strategy.csv
#   outputs/final/15_kpi_framework.csv
#   outputs/final/15_implementation_priorities.csv
#   outputs/final/15_intervention_recommendations.xlsx
#   data/processed/15_intervention_recommendations.rds
#   outputs/logs/15_intervention_recommendations.log
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
  "openxlsx"
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

maximum_actions_per_segment <- 6

default_review_note <- paste(
  "Recommendations are generated from observed segment profiles.",
  "They should be reviewed with subject-matter experts before implementation."
)


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


normalise_position <- function(x) {

  x <- stringr::str_to_lower(
    safe_text(x, "moderate")
  )

  dplyr::case_when(
    stringr::str_detect(x, "high|strong") ~ "High",
    stringr::str_detect(x, "low|weak|limited") ~ "Low",
    TRUE ~ "Moderate"
  )
}


priority_from_profile <- function(
    readiness,
    confidence,
    barriers,
    segment_share
) {

  readiness <- normalise_position(readiness)
  confidence <- normalise_position(confidence)
  barriers <- normalise_position(barriers)

  score <- 0

  if (readiness == "High") score <- score + 3
  if (readiness == "Moderate") score <- score + 2
  if (readiness == "Low") score <- score + 1

  if (confidence == "Low") score <- score + 2
  if (confidence == "Moderate") score <- score + 1

  if (barriers == "High") score <- score + 3
  if (barriers == "Moderate") score <- score + 1

  if (!is.na(segment_share) && segment_share >= 30) {
    score <- score + 2
  } else if (!is.na(segment_share) && segment_share >= 20) {
    score <- score + 1
  }

  dplyr::case_when(
    score >= 8 ~ "Very high",
    score >= 6 ~ "High",
    score >= 4 ~ "Medium",
    TRUE ~ "Lower"
  )
}


generate_core_problem <- function(
    readiness,
    confidence,
    barriers
) {

  readiness <- normalise_position(readiness)
  confidence <- normalise_position(confidence)
  barriers <- normalise_position(barriers)

  if (
    readiness == "Low" &
    barriers == "High"
  ) {
    return(
      paste(
        "Low readiness is reinforced by substantial practical or psychological barriers,",
        "making change feel difficult and unlikely to succeed."
      )
    )
  }

  if (
    readiness == "Moderate" &
    confidence == "Low"
  ) {
    return(
      paste(
        "There is some openness to change, but limited confidence prevents intention",
        "from becoming sustained action."
      )
    )
  }

  if (
    readiness == "High" &
    confidence == "High"
  ) {
    return(
      paste(
        "Motivation and confidence are already present, but friction, delay or unclear",
        "next steps may prevent immediate action."
      )
    )
  }

  if (barriers == "High") {
    return(
      paste(
        "The segment is interested in change, but practical barriers and perceived",
        "effort reduce the likelihood of action."
      )
    )
  }

  paste(
    "The segment requires a more relevant and personally meaningful pathway",
    "from awareness to action."
  )
}


generate_strategic_objective <- function(
    readiness,
    confidence,
    barriers
) {

  readiness <- normalise_position(readiness)
  confidence <- normalise_position(confidence)
  barriers <- normalise_position(barriers)

  if (
    readiness == "Low" &
    barriers == "High"
  ) {
    return(
      paste(
        "Reduce perceived difficulty, rebuild trust and create small, supported",
        "opportunities for progress."
      )
    )
  }

  if (
    confidence == "Low"
  ) {
    return(
      paste(
        "Increase self-efficacy by demonstrating achievable steps, visible progress",
        "and accessible support."
      )
    )
  }

  if (
    readiness == "High" &
    confidence == "High"
  ) {
    return(
      paste(
        "Convert readiness into immediate action through a simple, low-friction",
        "pathway and rapid access to support."
      )
    )
  }

  paste(
    "Strengthen personal relevance and provide flexible options that make",
    "the next step easy to choose."
  )
}


generate_behavioural_strategy <- function(
    readiness,
    confidence,
    barriers
) {

  readiness <- normalise_position(readiness)
  confidence <- normalise_position(confidence)
  barriers <- normalise_position(barriers)

  if (
    readiness == "Low" &
    barriers == "High"
  ) {
    return(
      paste(
        "Use barrier reduction, motivational interviewing, graded tasks,",
        "social proof and frequent reassurance."
      )
    )
  }

  if (confidence == "Low") {
    return(
      paste(
        "Use guided planning, implementation intentions, progress feedback,",
        "peer examples and confidence-building prompts."
      )
    )
  }

  if (
    readiness == "High" &
    confidence == "High"
  ) {
    return(
      paste(
        "Use direct calls to action, defaults, reminders, rapid onboarding",
        "and immediate access to the desired behaviour or service."
      )
    )
  }

  paste(
    "Use personalised information, choice architecture, timely prompts",
    "and flexible support options."
  )
}


generate_service_strategy <- function(
    readiness,
    confidence,
    barriers
) {

  readiness <- normalise_position(readiness)
  confidence <- normalise_position(confidence)
  barriers <- normalise_position(barriers)

  if (barriers == "High") {
    return(
      paste(
        "Provide assisted navigation, human support, flexible scheduling,",
        "cost reduction where possible and proactive follow-up."
      )
    )
  }

  if (confidence == "Low") {
    return(
      paste(
        "Provide structured onboarding, check-ins, practical coaching",
        "and clear indicators of progress."
      )
    )
  }

  if (
    readiness == "High" &
    confidence == "High"
  ) {
    return(
      paste(
        "Provide self-service access, short forms, immediate booking,",
        "fast confirmation and optional light-touch follow-up."
      )
    )
  }

  paste(
    "Provide a combination of self-service and human support,",
    "allowing people to choose the level of help they need."
  )
}


generate_channel_strategy <- function(
    readiness,
    confidence,
    barriers,
    digital_tool_text = ""
) {

  readiness <- normalise_position(readiness)
  confidence <- normalise_position(confidence)
  barriers <- normalise_position(barriers)

  digital_signal <- stringr::str_detect(
    stringr::str_to_lower(
      safe_text(digital_tool_text, "")
    ),
    "used digital tool.*\\+|digital.*high|online.*high"
  )

  if (
    barriers == "High" ||
    confidence == "Low"
  ) {
    return(
      paste(
        "Primary: trusted professionals, telephone support, community settings and",
        "targeted direct contact. Secondary: simple digital follow-up and reminders."
      )
    )
  }

  if (
    readiness == "High" &
    confidence == "High"
  ) {
    return(
      paste(
        "Primary: mobile, email, website and in-service prompts.",
        "Secondary: retargeting and brief reminder messages."
      )
    )
  }

  if (digital_signal) {
    return(
      paste(
        "Primary: personalised email, mobile and web journeys.",
        "Secondary: optional adviser or community support."
      )
    )
  }

  paste(
    "Use a blended approach combining digital communication, direct outreach",
    "and trusted offline touchpoints."
  )
}


generate_message_hierarchy <- function(
    readiness,
    confidence,
    barriers,
    recommended_message
) {

  readiness <- normalise_position(readiness)
  confidence <- normalise_position(confidence)
  barriers <- normalise_position(barriers)

  primary <- safe_text(
    recommended_message,
    "A clear and relevant invitation to take the next step."
  )

  secondary <- dplyr::case_when(
    barriers == "High" ~
      "Acknowledge the difficulty and explain how practical barriers will be reduced.",
    confidence == "Low" ~
      "Reassure people that support is available and progress can begin with one small step.",
    readiness == "High" ~
      "Emphasise immediacy, simplicity and the benefit of starting now.",
    TRUE ~
      "Explain the available choices and how support can be tailored."
  )

  proof <- dplyr::case_when(
    barriers == "High" ~
      "Use credible testimonials, visible support options and evidence that the service is accessible.",
    confidence == "Low" ~
      "Use achievable success stories, progress examples and supportive professional endorsement.",
    readiness == "High" ~
      "Use clear outcome benefits, fast access information and confirmation of what happens next.",
    TRUE ~
      "Use relevant examples and concise evidence of effectiveness."
  )

  call_to_action <- dplyr::case_when(
    readiness == "High" ~
      "Start now",
    confidence == "Low" ~
      "Take one supported step",
    barriers == "High" ~
      "Talk to someone about your options",
    TRUE ~
      "Explore the support that fits you"
  )

  tibble::tibble(
    primary_message = primary,
    secondary_message = secondary,
    proof_point = proof,
    call_to_action = call_to_action
  )
}


generate_actions <- function(
    segment,
    working_name,
    readiness,
    confidence,
    barriers
) {

  readiness <- normalise_position(readiness)
  confidence <- normalise_position(confidence)
  barriers <- normalise_position(barriers)

  if (
    readiness == "Low" &
    barriers == "High"
  ) {

    actions <- tibble::tribble(
      ~action_category, ~recommended_action, ~delivery_mode, ~implementation_horizon,
      "Barrier reduction", "Offer a short barrier-assessment and personalised support plan.", "Human-assisted", "Immediate",
      "Confidence building", "Break the desired behaviour into small, achievable steps.", "Human and digital", "Immediate",
      "Trust", "Use recognised professionals and transparent explanations of the service.", "Offline and digital", "Short term",
      "Access", "Provide telephone or assisted booking and flexible appointment options.", "Human-assisted", "Immediate",
      "Follow-up", "Use proactive check-ins after the first contact.", "Telephone or message", "Short term",
      "Social proof", "Show realistic examples of people overcoming similar barriers.", "Campaign content", "Medium term"
    )

  } else if (confidence == "Low") {

    actions <- tibble::tribble(
      ~action_category, ~recommended_action, ~delivery_mode, ~implementation_horizon,
      "Planning", "Provide a guided action plan with one clearly defined first step.", "Human and digital", "Immediate",
      "Self-efficacy", "Use progress tracking and positive feedback after each action.", "Digital and service", "Short term",
      "Support", "Offer optional coaching, adviser contact or peer support.", "Human-assisted", "Short term",
      "Reminders", "Send timely prompts linked to the individual's chosen goal.", "Digital", "Immediate",
      "Personalisation", "Allow users to select the type and intensity of support.", "Digital and service", "Medium term",
      "Reassurance", "Use relatable success stories and clear explanations of what to expect.", "Campaign content", "Immediate"
    )

  } else if (
    readiness == "High" &
    confidence == "High"
  ) {

    actions <- tibble::tribble(
      ~action_category, ~recommended_action, ~delivery_mode, ~implementation_horizon,
      "Conversion", "Use a direct call to action linked to immediate booking or registration.", "Digital and service", "Immediate",
      "Friction reduction", "Minimise form length, steps and waiting time.", "Digital and operational", "Immediate",
      "Defaults", "Pre-select the most appropriate next step where ethically suitable.", "Digital and service", "Short term",
      "Reminders", "Use rapid follow-up when someone begins but does not complete the process.", "Digital", "Immediate",
      "Access", "Provide same-day or near-term support options.", "Operational", "Short term",
      "Retention", "Use light-touch progress reminders after initial action.", "Digital", "Medium term"
    )

  } else {

    actions <- tibble::tribble(
      ~action_category, ~recommended_action, ~delivery_mode, ~implementation_horizon,
      "Personalisation", "Tailor messages to the person's stated priorities and circumstances.", "Digital and human", "Immediate",
      "Choice", "Offer a small set of clearly differentiated support options.", "Digital and service", "Immediate",
      "Relevance", "Connect the behaviour to immediate personal benefits.", "Campaign content", "Short term",
      "Prompting", "Use timely reminders at moments when action is most relevant.", "Digital", "Short term",
      "Support", "Make adviser help available without making it mandatory.", "Blended", "Medium term",
      "Feedback", "Test which message and support combination produces the strongest response.", "Research and optimisation", "Medium term"
    )
  }

  actions |>
    dplyr::mutate(
      segment = segment,
      working_name = working_name,
      action_rank = dplyr::row_number()
    ) |>
    dplyr::relocate(
      segment,
      working_name,
      action_rank
    ) |>
    dplyr::slice_head(
      n = maximum_actions_per_segment
    )
}


generate_kpis <- function(
    segment,
    working_name,
    readiness,
    confidence,
    barriers
) {

  readiness <- normalise_position(readiness)
  confidence <- normalise_position(confidence)
  barriers <- normalise_position(barriers)

  if (
    readiness == "Low" &
    barriers == "High"
  ) {

    kpis <- tibble::tribble(
      ~kpi_category, ~kpi_name, ~definition, ~expected_direction,
      "Engagement", "Supported first contacts", "Number or rate of people completing an initial supported conversation.", "Increase",
      "Barrier reduction", "Reported barrier score", "Average perceived barrier score after intervention exposure.", "Decrease",
      "Confidence", "Self-efficacy score", "Change in confidence to complete the target behaviour.", "Increase",
      "Access", "Assisted booking completion", "Share of assisted journeys resulting in a completed booking.", "Increase",
      "Retention", "Follow-up engagement", "Share responding to or attending a follow-up contact.", "Increase"
    )

  } else if (confidence == "Low") {

    kpis <- tibble::tribble(
      ~kpi_category, ~kpi_name, ~definition, ~expected_direction,
      "Confidence", "Self-efficacy score", "Change in confidence to complete the target behaviour.", "Increase",
      "Planning", "Action-plan completion", "Share completing a personalised action plan.", "Increase",
      "Engagement", "Support uptake", "Share choosing coaching, adviser or peer support.", "Increase",
      "Progress", "First milestone completion", "Share completing the first agreed behavioural milestone.", "Increase",
      "Retention", "Continuation rate", "Share remaining engaged after the first intervention step.", "Increase"
    )

  } else if (
    readiness == "High" &
    confidence == "High"
  ) {

    kpis <- tibble::tribble(
      ~kpi_category, ~kpi_name, ~definition, ~expected_direction,
      "Conversion", "Immediate action rate", "Share completing the target action after exposure.", "Increase",
      "Journey", "Completion rate", "Share completing the full registration, booking or onboarding journey.", "Increase",
      "Friction", "Time to action", "Median time between first prompt and completed action.", "Decrease",
      "Drop-off", "Abandonment rate", "Share starting but not completing the journey.", "Decrease",
      "Retention", "Sustained participation", "Share still participating after the initial action period.", "Increase"
    )

  } else {

    kpis <- tibble::tribble(
      ~kpi_category, ~kpi_name, ~definition, ~expected_direction,
      "Engagement", "Message response rate", "Share responding to a personalised message or prompt.", "Increase",
      "Choice", "Support-option selection", "Share selecting one of the available support pathways.", "Increase",
      "Relevance", "Perceived relevance score", "Reported relevance of the communication or offer.", "Increase",
      "Conversion", "Next-step completion", "Share completing the recommended next action.", "Increase",
      "Learning", "Variant performance difference", "Difference in outcomes between tested messages or pathways.", "Optimise"
    )
  }

  kpis |>
    dplyr::mutate(
      segment = segment,
      working_name = working_name,
      kpi_rank = dplyr::row_number()
    ) |>
    dplyr::relocate(
      segment,
      working_name,
      kpi_rank
    )
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

  openxlsx::addWorksheet(
    workbook,
    sheet_name
  )

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


clean_column_names <- function(data) {

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
      processed_dir,
      logs_dir
    ),
    create_directory
  )
)

log_file <- file.path(
  logs_dir,
  "15_intervention_recommendations.log"
)

if (file.exists(log_file)) {
  file.remove(log_file)
}


# =============================================================================
# 5. Load inputs
# =============================================================================

persona_details <- safe_read_csv(
  file.path(
    final_dir,
    "14_persona_details.csv"
  )
)

persona_summary <- safe_read_csv(
  file.path(
    final_dir,
    "14_persona_summary.csv"
  )
)

key_variables <- safe_read_csv(
  file.path(
    final_dir,
    "13_segment_key_variables.csv"
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

if (nrow(persona_details) == 0) {
  stop(
    "14_persona_details.csv is missing or empty. ",
    "Run 14_persona_generation.R first."
  )
}


# =============================================================================
# 6. Build intervention strategy
# =============================================================================

intervention_strategy <- persona_details |>
  dplyr::rowwise() |>
  dplyr::mutate(
    readiness_level =
      normalise_position(
        readiness_position
      ),
    confidence_level =
      normalise_position(
        confidence_position
      ),
    barrier_level =
      normalise_position(
        barrier_position
      ),
    core_behavioural_problem =
      generate_core_problem(
        readiness_position,
        confidence_position,
        barrier_position
      ),
    strategic_objective =
      generate_strategic_objective(
        readiness_position,
        confidence_position,
        barrier_position
      ),
    behavioural_strategy =
      generate_behavioural_strategy(
        readiness_position,
        confidence_position,
        barrier_position
      ),
    service_strategy =
      generate_service_strategy(
        readiness_position,
        confidence_position,
        barrier_position
      ),
    channel_strategy =
      generate_channel_strategy(
        readiness_position,
        confidence_position,
        barrier_position,
        key_characteristics
      ),
    implementation_priority =
      priority_from_profile(
        readiness_position,
        confidence_position,
        barrier_position,
        share_pct
      ),
    review_note = default_review_note
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    segment,
    working_name,
    n,
    share_pct,
    readiness_level,
    confidence_level,
    barrier_level,
    core_behavioural_problem,
    strategic_objective,
    behavioural_strategy,
    service_strategy,
    channel_strategy,
    suggested_support_intensity,
    suggested_delivery_style,
    implementation_priority,
    review_note
  ) |>
  dplyr::arrange(segment)


# =============================================================================
# 7. Build message framework
# =============================================================================

message_framework <- purrr::map_dfr(
  seq_len(
    nrow(persona_details)
  ),
  function(index) {

    row <- persona_details[
      index,
      ,
      drop = FALSE
    ]

    messages <- generate_message_hierarchy(
      row$readiness_position,
      row$confidence_position,
      row$barrier_position,
      row$recommended_message
    )

    messages |>
      dplyr::mutate(
        segment = row$segment,
        working_name =
          row$working_name,
        tone = safe_text(row$tone),
        avoid = safe_text(row$avoid)
      ) |>
      dplyr::relocate(
        segment,
        working_name
      )
  }
)


# =============================================================================
# 8. Build intervention actions
# =============================================================================

intervention_actions <- purrr::map_dfr(
  seq_len(
    nrow(persona_details)
  ),
  function(index) {

    row <- persona_details[
      index,
      ,
      drop = FALSE
    ]

    generate_actions(
      segment = row$segment,
      working_name =
        row$working_name,
      readiness =
        row$readiness_position,
      confidence =
        row$confidence_position,
      barriers =
        row$barrier_position
    )
  }
)


# =============================================================================
# 9. Build channel strategy
# =============================================================================

channel_strategy <- intervention_strategy |>
  dplyr::transmute(
    segment,
    working_name,
    implementation_priority,
    primary_channel_strategy =
      channel_strategy,
    service_delivery =
      service_strategy,
    communication_style =
      suggested_delivery_style,
    support_intensity =
      suggested_support_intensity
  )


# =============================================================================
# 10. Build KPI framework
# =============================================================================

kpi_framework <- purrr::map_dfr(
  seq_len(
    nrow(persona_details)
  ),
  function(index) {

    row <- persona_details[
      index,
      ,
      drop = FALSE
    ]

    generate_kpis(
      segment = row$segment,
      working_name =
        row$working_name,
      readiness =
        row$readiness_position,
      confidence =
        row$confidence_position,
      barriers =
        row$barrier_position
    )
  }
)


# =============================================================================
# 11. Build implementation priorities
# =============================================================================

implementation_priorities <- intervention_strategy |>
  dplyr::mutate(
    priority_score =
      dplyr::case_when(
        implementation_priority == "Very high" ~ 4,
        implementation_priority == "High" ~ 3,
        implementation_priority == "Medium" ~ 2,
        TRUE ~ 1
      ),
    
    recommended_sequence =
      dplyr::case_when(
        readiness_level == "High" &
          confidence_level == "High" ~
          "Launch conversion-focused actions first.",
        
        barrier_level == "High" ~
          "Begin with barrier reduction and assisted support.",
        
        confidence_level == "Low" ~
          "Begin with confidence-building and guided planning.",
        
        TRUE ~
          "Begin with personalised communication and pathway testing."
      )
  ) |>
  dplyr::select(
    segment,
    working_name,
    n,
    share_pct,
    implementation_priority,
    priority_score,
    recommended_sequence,
    strategic_objective
  ) |>
  dplyr::arrange(
    dplyr::desc(priority_score),
    dplyr::desc(share_pct)
  ) |>
  dplyr::mutate(
    overall_rank = dplyr::row_number()
  ) |>
  dplyr::relocate(
    overall_rank
  )

# =============================================================================
# 12. Export CSV files
# =============================================================================

readr::write_csv(
  intervention_strategy,
  file.path(
    final_dir,
    "15_intervention_strategy.csv"
  )
)

readr::write_csv(
  intervention_actions,
  file.path(
    final_dir,
    "15_intervention_actions.csv"
  )
)

readr::write_csv(
  message_framework,
  file.path(
    final_dir,
    "15_message_framework.csv"
  )
)

readr::write_csv(
  channel_strategy,
  file.path(
    final_dir,
    "15_channel_strategy.csv"
  )
)

readr::write_csv(
  kpi_framework,
  file.path(
    final_dir,
    "15_kpi_framework.csv"
  )
)

readr::write_csv(
  implementation_priorities,
  file.path(
    final_dir,
    "15_implementation_priorities.csv"
  )
)


# =============================================================================
# 13. Save consolidated R object
# =============================================================================

intervention_outputs <- list(
  intervention_strategy =
    intervention_strategy,
  intervention_actions =
    intervention_actions,
  message_framework =
    message_framework,
  channel_strategy =
    channel_strategy,
  kpi_framework =
    kpi_framework,
  implementation_priorities =
    implementation_priorities
)

saveRDS(
  intervention_outputs,
  file.path(
    processed_dir,
    "15_intervention_recommendations.rds"
  )
)


# =============================================================================
# 14. Create Excel workbook
# =============================================================================

workbook <- openxlsx::createWorkbook(
  creator =
    "Behavioural Segmentation Toolkit"
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
  "Strategy",
  clean_column_names(
    intervention_strategy
  ),
  header_style,
  first_column_style
)

add_excel_sheet(
  workbook,
  "Actions",
  clean_column_names(
    intervention_actions
  ),
  header_style,
  first_column_style
)

add_excel_sheet(
  workbook,
  "Messages",
  clean_column_names(
    message_framework
  ),
  header_style,
  first_column_style
)

add_excel_sheet(
  workbook,
  "Channels",
  clean_column_names(
    channel_strategy
  ),
  header_style,
  first_column_style
)

add_excel_sheet(
  workbook,
  "KPIs",
  clean_column_names(
    kpi_framework
  ),
  header_style,
  first_column_style
)

add_excel_sheet(
  workbook,
  "Priorities",
  clean_column_names(
    implementation_priorities
  ),
  header_style,
  first_column_style
)

if (nrow(key_variables) > 0) {

  add_excel_sheet(
    workbook,
    "Key Variables",
    clean_column_names(
      key_variables
    ),
    header_style,
    first_column_style
  )
}

excel_output_file <- file.path(
  final_dir,
  "15_intervention_recommendations.xlsx"
)

openxlsx::saveWorkbook(
  workbook,
  excel_output_file,
  overwrite = TRUE
)


# =============================================================================
# 15. Final report
# =============================================================================

capture.output(
  sessionInfo(),
  file = file.path(
    logs_dir,
    "15_sessionInfo.txt"
  )
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — INTERVENTION RECOMMENDATIONS COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Segments included:              ",
  nrow(intervention_strategy),
  "\n",
  "Intervention actions:           ",
  nrow(intervention_actions),
  "\n",
  "Message frameworks:             ",
  nrow(message_framework),
  "\n",
  "KPIs defined:                   ",
  nrow(kpi_framework),
  "\n",
  "Excel workbook:\n",
  excel_output_file,
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)
