# =============================================================================
# 11_final_segment_profiles.R
# Behavioural Segmentation Toolkit
#
# Inputs:
#   data/processed/10_final_segmentation_data.rds
#   data/processed/10_final_model_metadata.rds
#   data/processed/profiling_data.rds
#   data/processed/02_variable_metadata.rds
#
# Outputs:
#   outputs/tables/11_segment_sizes.csv
#   outputs/tables/11_segmentation_profiles.csv
#   outputs/tables/11_numeric_profiling_summary.csv
#   outputs/tables/11_categorical_profiling_summary.csv
#   outputs/tables/11_segment_difference_tests.csv
#   outputs/tables/11_segment_distinguishing_variables.csv
#   outputs/tables/11_final_segment_profile_summary.csv
#   data/processed/11_final_segment_profile_data.rds
#   outputs/figures/11_segment_sizes.png
#   outputs/figures/11_segmentation_profile_heatmap.png
#   outputs/figures/11_numeric_profile_heatmap.png
#   outputs/figures/11_top_distinguishing_variables.png
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
  "scales"
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

identifier_variable <- "respondent_id"
segment_variable <- "final_segment"

p_adjustment_method <- "BH"
significance_level <- 0.05

maximum_top_variables <- 20


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


safe_mean <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(x, na.rm = TRUE)
}


safe_sd <- function(x) {
  
  if (sum(!is.na(x)) < 2) {
    return(NA_real_)
  }
  
  stats::sd(x, na.rm = TRUE)
}


safe_median <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  stats::median(x, na.rm = TRUE)
}


safe_iqr <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  stats::IQR(x, na.rm = TRUE)
}


eta_squared <- function(x, segment) {
  
  valid <- !is.na(x) &
    !is.na(segment)
  
  x <- x[valid]
  segment <- factor(segment[valid])
  
  if (
    length(x) < 3 ||
    nlevels(segment) < 2
  ) {
    return(NA_real_)
  }
  
  total_ss <- sum(
    (x - mean(x))^2
  )
  
  if (total_ss == 0) {
    return(0)
  }
  
  segment_means <- tapply(
    x,
    segment,
    mean
  )
  
  segment_sizes <- table(segment)
  
  between_ss <- sum(
    as.numeric(segment_sizes) *
      (
        segment_means -
          mean(x)
      )^2
  )
  
  between_ss / total_ss
}


cramers_v <- function(x, segment) {
  
  valid <- !is.na(x) &
    !is.na(segment)
  
  x <- droplevels(
    factor(x[valid])
  )
  
  segment <- droplevels(
    factor(segment[valid])
  )
  
  if (
    nlevels(x) < 2 ||
    nlevels(segment) < 2
  ) {
    return(NA_real_)
  }
  
  contingency <- table(
    x,
    segment
  )
  
  test <- suppressWarnings(
    stats::chisq.test(
      contingency,
      correct = FALSE
    )
  )
  
  denominator <- sum(contingency) *
    min(
      nrow(contingency) - 1,
      ncol(contingency) - 1
    )
  
  if (denominator <= 0) {
    return(NA_real_)
  }
  
  sqrt(
    as.numeric(test$statistic) /
      denominator
  )
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

final_segmentation_file <- file.path(
  data_processed_dir,
  "10_final_segmentation_data.rds"
)

final_model_metadata_file <- file.path(
  data_processed_dir,
  "10_final_model_metadata.rds"
)

profiling_data_file <- file.path(
  data_processed_dir,
  "profiling_data.rds"
)

variable_metadata_file <- file.path(
  data_processed_dir,
  "02_variable_metadata.rds"
)


# =============================================================================
# 5. Load inputs
# =============================================================================

required_input_files <- c(
  final_segmentation_file,
  final_model_metadata_file,
  profiling_data_file,
  variable_metadata_file
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0) {
  stop(
    "Required input files are missing:\n",
    paste(
      missing_input_files,
      collapse = "\n"
    )
  )
}

final_segmentation_data <- readRDS(
  final_segmentation_file
) %>%
  tibble::as_tibble()

final_model_metadata <- readRDS(
  final_model_metadata_file
) %>%
  tibble::as_tibble()

profiling_data <- readRDS(
  profiling_data_file
) %>%
  tibble::as_tibble()

variable_metadata <- readRDS(
  variable_metadata_file
) %>%
  tibble::as_tibble()


# =============================================================================
# 6. Validate and combine data
# =============================================================================

if (
  !all(
    c(
      identifier_variable,
      segment_variable
    ) %in%
    names(final_segmentation_data)
  )
) {
  stop(
    "Final segmentation data must contain ",
    identifier_variable,
    " and ",
    segment_variable,
    "."
  )
}

if (!identifier_variable %in%
    names(profiling_data)) {
  stop(
    "Profiling data do not contain ",
    identifier_variable,
    "."
  )
}

final_profile_data <- final_segmentation_data %>%
  dplyr::left_join(
    profiling_data,
    by = identifier_variable,
    suffix = c(
      "",
      "_profiling"
    )
  ) %>%
  dplyr::mutate(
    final_segment = factor(
      final_segment,
      levels = sort(
        unique(final_segment)
      ),
      labels = paste0(
        "Segment ",
        sort(
          unique(final_segment)
        )
      )
    )
  )

saveRDS(
  final_profile_data,
  file.path(
    data_processed_dir,
    "11_final_segment_profile_data.rds"
  )
)


# =============================================================================
# 7. Variable sets
# =============================================================================

segmentation_variables <- setdiff(
  names(final_segmentation_data),
  c(
    identifier_variable,
    segment_variable
  )
)

profiling_variables <- intersect(
  setdiff(
    names(profiling_data),
    identifier_variable
  ),
  names(final_profile_data)
)

numeric_profiling_variables <- profiling_variables[
  vapply(
    final_profile_data[profiling_variables],
    is.numeric,
    logical(1)
  )
]

categorical_profiling_variables <- setdiff(
  profiling_variables,
  numeric_profiling_variables
)


# =============================================================================
# 8. Segment sizes
# =============================================================================

segment_sizes <- final_profile_data %>%
  dplyr::count(
    final_segment,
    name = "n"
  ) %>%
  dplyr::mutate(
    share = n / sum(n),
    share_pct = round(
      100 * share,
      1
    )
  ) %>%
  dplyr::rename(
    segment = final_segment
  )

readr::write_csv(
  segment_sizes,
  file.path(
    tables_dir,
    "11_segment_sizes.csv"
  )
)


# =============================================================================
# 9. Segmentation profiles
# =============================================================================

overall_segmentation <- final_profile_data %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(
        segmentation_variables
      ),
      list(
        overall_mean = safe_mean,
        overall_sd = safe_sd
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c(
      "variable",
      ".value"
    ),
    names_sep = "__"
  )

segmentation_profiles <- final_profile_data %>%
  dplyr::group_by(
    final_segment
  ) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(
        segmentation_variables
      ),
      list(
        mean = safe_mean,
        sd = safe_sd,
        median = safe_median,
        iqr = safe_iqr
      ),
      .names = "{.col}__{.fn}"
    ),
    segment_n = dplyr::n(),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = -c(
      final_segment,
      segment_n
    ),
    names_to = c(
      "variable",
      ".value"
    ),
    names_sep = "__"
  ) %>%
  dplyr::left_join(
    overall_segmentation,
    by = "variable"
  ) %>%
  dplyr::mutate(
    standardised_mean =
      dplyr::if_else(
        is.na(overall_sd) |
          overall_sd == 0,
        0,
        (
          mean -
            overall_mean
        ) / overall_sd
      )
  ) %>%
  dplyr::left_join(
    variable_metadata %>%
      dplyr::select(
        variable,
        label,
        group,
        data_type,
        description
      ) %>%
      dplyr::distinct(
        variable,
        .keep_all = TRUE
      ),
    by = "variable"
  ) %>%
  dplyr::rename(
    segment = final_segment
  )

readr::write_csv(
  segmentation_profiles,
  file.path(
    tables_dir,
    "11_segmentation_profiles.csv"
  )
)


# =============================================================================
# 10. Numeric profiling summary
# =============================================================================

numeric_profiling_summary <- tibble::tibble()

if (length(numeric_profiling_variables) > 0) {
  
  overall_numeric <- final_profile_data %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(
          numeric_profiling_variables
        ),
        list(
          overall_mean = safe_mean,
          overall_sd = safe_sd
        ),
        .names = "{.col}__{.fn}"
      )
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = c(
        "variable",
        ".value"
      ),
      names_sep = "__"
    )
  
  numeric_profiling_summary <- final_profile_data %>%
    dplyr::group_by(
      final_segment
    ) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(
          numeric_profiling_variables
        ),
        list(
          mean = safe_mean,
          sd = safe_sd,
          median = safe_median,
          iqr = safe_iqr
        ),
        .names = "{.col}__{.fn}"
      ),
      segment_n = dplyr::n(),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = -c(
        final_segment,
        segment_n
      ),
      names_to = c(
        "variable",
        ".value"
      ),
      names_sep = "__"
    ) %>%
    dplyr::left_join(
      overall_numeric,
      by = "variable"
    ) %>%
    dplyr::mutate(
      standardised_mean =
        dplyr::if_else(
          is.na(overall_sd) |
            overall_sd == 0,
          0,
          (
            mean -
              overall_mean
          ) / overall_sd
        )
    ) %>%
    dplyr::left_join(
      variable_metadata %>%
        dplyr::select(
          variable,
          label,
          group,
          data_type
        ) %>%
        dplyr::distinct(
          variable,
          .keep_all = TRUE
        ),
      by = "variable"
    ) %>%
    dplyr::rename(
      segment = final_segment
    )
}

readr::write_csv(
  numeric_profiling_summary,
  file.path(
    tables_dir,
    "11_numeric_profiling_summary.csv"
  )
)


# =============================================================================
# 11. Categorical profiling summary
# =============================================================================

categorical_profiling_summary <- tibble::tibble()

if (length(categorical_profiling_variables) > 0) {
  
  categorical_profiling_summary <- purrr::map_dfr(
    categorical_profiling_variables,
    function(variable_name) {
      
      final_profile_data %>%
        dplyr::transmute(
          segment = final_segment,
          level = as.character(
            .data[[variable_name]]
          )
        ) %>%
        dplyr::mutate(
          level = dplyr::if_else(
            is.na(level),
            "Missing",
            level
          )
        ) %>%
        dplyr::count(
          segment,
          level,
          name = "n"
        ) %>%
        dplyr::group_by(
          segment
        ) %>%
        dplyr::mutate(
          segment_total = sum(n),
          within_segment_share =
            n / segment_total,
          within_segment_pct =
            round(
              100 *
                within_segment_share,
              1
            ),
          variable =
            variable_name
        ) %>%
        dplyr::ungroup()
    }
  ) %>%
    dplyr::left_join(
      variable_metadata %>%
        dplyr::select(
          variable,
          label,
          group,
          data_type
        ) %>%
        dplyr::distinct(
          variable,
          .keep_all = TRUE
        ),
      by = "variable"
    )
}

readr::write_csv(
  categorical_profiling_summary,
  file.path(
    tables_dir,
    "11_categorical_profiling_summary.csv"
  )
)


# =============================================================================
# 12. Difference tests and effect sizes
# =============================================================================

all_profile_variables <- unique(
  c(
    segmentation_variables,
    profiling_variables
  )
)

difference_tests <- purrr::map_dfr(
  all_profile_variables,
  function(variable_name) {
    
    x <- final_profile_data[[variable_name]]
    
    segment <- final_profile_data[[segment_variable]]
    
    metadata_row <- variable_metadata %>%
      dplyr::filter(
        variable ==
          variable_name
      ) %>%
      dplyr::slice_head(
        n = 1
      )
    
    if (is.numeric(x)) {
      
      valid <- !is.na(x) &
        !is.na(segment)
      
      fit <- if (
        sum(valid) >= 3
      ) {
        stats::aov(
          x[valid] ~
            factor(
              segment[valid]
            )
        )
      } else {
        NULL
      }
      
      model_summary <- if (
        is.null(fit)
      ) {
        NULL
      } else {
        summary(fit)[[1]]
      }
      
      result <- tibble::tibble(
        test = "ANOVA",
        statistic = if (
          is.null(model_summary)
        ) {
          NA_real_
        } else {
          model_summary[["F value"]][1]
        },
        p_value = if (
          is.null(model_summary)
        ) {
          NA_real_
        } else {
          model_summary[["Pr(>F)"]][1]
        },
        effect_size =
          eta_squared(
            x,
            segment
          ),
        effect_size_name =
          "Eta squared"
      )
      
    } else {
      
      valid <- !is.na(x) &
        !is.na(segment)
      
      contingency <- table(
        factor(x[valid]),
        factor(segment[valid])
      )
      
      chi_test <- if (
        nrow(contingency) > 1 &&
        ncol(contingency) > 1
      ) {
        suppressWarnings(
          stats::chisq.test(
            contingency,
            correct = FALSE
          )
        )
      } else {
        NULL
      }
      
      result <- tibble::tibble(
        test = "Chi-square",
        statistic = if (
          is.null(chi_test)
        ) {
          NA_real_
        } else {
          as.numeric(
            chi_test$statistic
          )
        },
        p_value = if (
          is.null(chi_test)
        ) {
          NA_real_
        } else {
          chi_test$p.value
        },
        effect_size =
          cramers_v(
            x,
            segment
          ),
        effect_size_name =
          "Cramer's V"
      )
    }
    
    result %>%
      dplyr::mutate(
        variable =
          variable_name,
        label = ifelse(
          nrow(metadata_row) == 0,
          variable_name,
          metadata_row$label[1]
        ),
        group = ifelse(
          nrow(metadata_row) == 0,
          "Unspecified",
          metadata_row$group[1]
        ),
        variable_role =
          ifelse(
            variable_name %in%
              segmentation_variables,
            "Segmentation",
            "Profiling"
          ),
        .before = 1
      )
  }
) %>%
  dplyr::mutate(
    adjusted_p_value =
      stats::p.adjust(
        p_value,
        method =
          p_adjustment_method
      ),
    statistically_significant =
      adjusted_p_value <
      significance_level,
    effect_rank = rank(
      -effect_size,
      ties.method = "first"
    )
  ) %>%
  dplyr::arrange(
    effect_rank
  )

readr::write_csv(
  difference_tests,
  file.path(
    tables_dir,
    "11_segment_difference_tests.csv"
  )
)

readr::write_csv(
  difference_tests %>%
    dplyr::filter(
      !is.na(effect_size)
    ),
  file.path(
    tables_dir,
    "11_segment_distinguishing_variables.csv"
  )
)


# =============================================================================
# 13. Final summary
# =============================================================================

top_characteristics <- segmentation_profiles %>%
  dplyr::group_by(
    segment
  ) %>%
  dplyr::slice_max(
    order_by =
      abs(
        standardised_mean
      ),
    n = 5,
    with_ties = FALSE
  ) %>%
  dplyr::summarise(
    strongest_characteristics =
      paste0(
        dplyr::coalesce(
          label,
          variable
        ),
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
        " SD)",
        collapse = "; "
      ),
    .groups = "drop"
  )

final_segment_profile_summary <- segment_sizes %>%
  dplyr::left_join(
    top_characteristics,
    by = "segment"
  ) %>%
  dplyr::mutate(
    final_model_id =
      final_model_metadata$
      model_id[1],
    feature_set =
      final_model_metadata$
      feature_set[1],
    clustering_method =
      final_model_metadata$
      method[1],
    number_of_segments =
      final_model_metadata$
      k[1]
  ) %>%
  dplyr::select(
    final_model_id,
    feature_set,
    clustering_method,
    number_of_segments,
    segment,
    n,
    share,
    share_pct,
    strongest_characteristics
  )

readr::write_csv(
  final_segment_profile_summary,
  file.path(
    tables_dir,
    "11_final_segment_profile_summary.csv"
  )
)


# =============================================================================
# 14. Figures
# =============================================================================

segment_size_plot <- ggplot2::ggplot(
  segment_sizes,
  ggplot2::aes(
    x = segment,
    y = share
  )
) +
  ggplot2::geom_col() +
  ggplot2::geom_text(
    ggplot2::aes(
      label = paste0(
        n,
        " (",
        share_pct,
        "%)"
      )
    ),
    vjust = -0.3
  ) +
  ggplot2::scale_y_continuous(
    labels =
      scales::percent_format(
        accuracy = 1
      ),
    expand =
      ggplot2::expansion(
        mult = c(
          0,
          0.12
        )
      )
  ) +
  ggplot2::labs(
    title = "Final segment sizes",
    x = NULL,
    y = "Share of respondents"
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "11_segment_sizes.png"
  ),
  plot = segment_size_plot,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

segmentation_heatmap <- segmentation_profiles %>%
  dplyr::mutate(
    display_label =
      dplyr::coalesce(
        label,
        variable
      )
  ) %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = segment,
      y = display_label,
      fill = standardised_mean
    )
  ) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(
    ggplot2::aes(
      label = round(
        standardised_mean,
        2
      )
    ),
    size = 3
  ) +
  ggplot2::scale_fill_gradient2(
    midpoint = 0
  ) +
  ggplot2::labs(
    title = "Final segmentation-variable profiles",
    subtitle = "Segment means standardised relative to the full sample",
    x = NULL,
    y = NULL,
    fill = "Standardised mean"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "11_segmentation_profile_heatmap.png"
  ),
  plot = segmentation_heatmap,
  width = 9,
  height = max(
    7,
    0.42 *
      length(
        segmentation_variables
      )
  ),
  dpi = 300,
  bg = "white"
)

top_difference_plot_data <- difference_tests %>%
  dplyr::filter(
    !is.na(effect_size)
  ) %>%
  dplyr::slice_head(
    n = min(
      maximum_top_variables,
      nrow(
        difference_tests
      )
    )
  ) %>%
  dplyr::mutate(
    display_label = reorder(
      dplyr::coalesce(
        label,
        variable
      ),
      effect_size
    )
  )

top_difference_plot <- ggplot2::ggplot(
  top_difference_plot_data,
  ggplot2::aes(
    x = display_label,
    y = effect_size,
    shape = variable_role
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Variables that most strongly distinguish the final segments",
    x = NULL,
    y = "Effect size",
    shape = "Variable role"
  ) +
  ggplot2::theme_minimal(
    base_size = 11
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "11_top_distinguishing_variables.png"
  ),
  plot = top_difference_plot,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

if (nrow(numeric_profiling_summary) > 0) {
  
  numeric_profile_heatmap <- numeric_profiling_summary %>%
    dplyr::mutate(
      display_label =
        dplyr::coalesce(
          label,
          variable
        )
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = segment,
        y = display_label,
        fill = standardised_mean
      )
    ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      ggplot2::aes(
        label = round(
          standardised_mean,
          2
        )
      ),
      size = 3
    ) +
    ggplot2::scale_fill_gradient2(
      midpoint = 0
    ) +
    ggplot2::labs(
      title = "Numeric profiling characteristics of the final segments",
      x = NULL,
      y = NULL,
      fill = "Standardised mean"
    ) +
    ggplot2::theme_minimal(
      base_size = 10
    )
  
  ggplot2::ggsave(
    filename = file.path(
      figures_dir,
      "11_numeric_profile_heatmap.png"
    ),
    plot = numeric_profile_heatmap,
    width = 9,
    height = max(
      6,
      0.42 *
        length(
          numeric_profiling_variables
        )
    ),
    dpi = 300,
    bg = "white"
  )
}


# =============================================================================
# 15. Final report
# =============================================================================

capture.output(
  sessionInfo(),
  file = file.path(
    logs_dir,
    "11_sessionInfo.txt"
  )
)

cat(
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "BEHAVIOURAL SEGMENTATION TOOLKIT — FINAL SEGMENT PROFILES COMPLETE\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  "Final model:                    ",
  final_model_metadata$model_id[1],
  "\n",
  "Segments profiled:              ",
  nrow(segment_sizes),
  "\n",
  "Segmentation variables:         ",
  length(segmentation_variables),
  "\n",
  "Numeric profiling variables:    ",
  length(numeric_profiling_variables),
  "\n",
  "Categorical profiling variables:",
  length(categorical_profiling_variables),
  "\n",
  "Significant differences:        ",
  sum(
    difference_tests$
      statistically_significant,
    na.rm = TRUE
  ),
  "\n",
  "\nMain segment summary:\n",
  file.path(
    tables_dir,
    "11_final_segment_profile_summary.csv"
  ),
  "\n",
  "Full profile data:\n",
  file.path(
    data_processed_dir,
    "11_final_segment_profile_data.rds"
  ),
  "\n",
  paste(rep("=", 78), collapse = ""),
  "\n",
  sep = ""
)