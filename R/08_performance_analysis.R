################################################################################
#
# FILE: R/08_performance_analysis.R
#
# DESCRIPTION: Contains functions for analyzing CAT performance across
#              different demographic or clinical groups.
#
################################################################################

#' Analyze CAT Performance by Group
#'
#' @description
#' This function takes the result of a single CAT simulation and calculates
#' key performance metrics (Bias, RMSE, Average Test Length) for each subgroup
#' defined by a grouping variable (e.g., clinical group).
#'
#' @param sim_result The output object from `catR::simulateRespondents`.
#' @param patient_data A data frame or tibble containing subject-level data,
#'   including the `id_col_name`, `group_col_name`, and true theta values.
#' @param id_col_name The name of the subject identifier column.
#' @param group_col_name The name of the column to use for grouping.
#'
#' @return A tibble summarizing the performance metrics for each group.
#'
#' @importFrom dplyr %>% group_by summarise
#' @importFrom tibble tibble
#' @export
analyze_performance_by_group <- function(sim_result, scenario, patient_data, id_col_name, group_col_name) {

  analysis_df <- tibble::tibble(
    # Use the grouping variable directly from the patient data
    !!group_col_name := patient_data[[group_col_name]],
    # Use the true theta from the patient data (named theta_anchor)
    true_theta = patient_data[["theta_anchor"]],
    # Get the estimated theta from the simulation results
    estimated_theta = sim_result$estimatedThetas,
    # Get the test length from the simulation results
    test_length = sim_result$testLength
  )

  # Calculate metrics per group
  summary_by_group <- analysis_df %>%
    dplyr::group_by(.data[[group_col_name]]) %>%
    dplyr::summarise(
      Bias = mean(estimated_theta - true_theta, na.rm = TRUE),
      RMSE = sqrt(mean((estimated_theta - true_theta)^2, na.rm = TRUE)),
      Avg_Length = mean(test_length, na.rm = TRUE),
      .groups = 'drop'
    )

  # Add the scenario information to the summary report for context
  summary_with_scenario <- summary_by_group %>%
    dplyr::mutate(
      se_threshold = scenario$se_threshold,
      max_items = scenario$max_items,
      .before = 1 # Place new columns at the beginning
    )

  return(summary_with_scenario)

  return(summary_by_group)
}
