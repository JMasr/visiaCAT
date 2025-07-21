################################################################################
#
# FILE: R/06_simulation_core.R
#
# DESCRIPTION: Contains the core functions for running and evaluating CAT
# simulation scenarios.
#
################################################################################

#' Run a Single CAT Simulation Scenario
#'
#' @description
#' This function executes a complete CAT simulation for a population of examinees
#' (`thetas`) against a given item bank, based on a single, defined scenario
#' of CAT rules (item selection, stopping criteria, etc.).
#'
#' @param scenario A single-row data frame or list containing the parameters
#'   for the simulation, typically including `item_select_rule`, `se_threshold`,
#'   and `max_items`.
#' @param thetas A numeric vector of true ability (theta) values for the
#'   simulated population.
#' @param item_bank A numeric matrix of item parameters, formatted for `catR`.
#'
#' @return An object of class `catR` containing the detailed results of the
#'   simulation for all respondents.
#'
#' @importFrom catR simulateRespondents
#' @export
run_cat_scenario <- function(scenario, thetas, item_bank) {

  message(sprintf(
    "Running scenario: Item Selection = %s, Stop Rule = SE < %.2f OR Max Items = %d",
    scenario$item_select_rule, scenario$se_threshold, scenario$max_items
  ))

  # Define the CAT components based on the scenario
  start_list <- list(nrItems = 5, startSelect = "MFI")
  test_list <- list(method = "EAP", itemSelect = scenario$item_select_rule)
  stop_list <- list(rule = c("precision", "length"), thr = c(scenario$se_threshold, scenario$max_items))
  final_list <- list(method = "EAP", alpha = 0.05)

  # Execute the simulation for all thetas
  simulation_result <- catR::simulateRespondents(
    thetas = thetas,
    itemBank = item_bank,
    model = "GRM", # Graded Response Model
    start = start_list,
    test = test_list,
    stop = stop_list,
    final = final_list
  )

  return(simulation_result)
}

#' Evaluate CAT Simulation Performance
#'
#' @description
#' Calculates key performance metrics for a completed CAT simulation, including
#' overall bias, Root Mean Squared Error (RMSE), and the average test length.
#'
#' @param simulation_result The output object from `catR::simulateRespondents`.
#' @param true_thetas The original vector of true theta values used in the simulation.
#'
#' @return A single-row tibble with the columns `Bias`, `RMSE`, and `Avg_Length`.
#'
#' @importFrom tibble tibble
#' @export
evaluate_cat_performance <- function(simulation_result, true_thetas) {
  estimated_thetas <- simulation_result$estimatedThetas
  test_lengths <- simulation_result$testLength

  bias <- mean(estimated_thetas - true_thetas, na.rm = TRUE)
  rmse <- sqrt(mean((estimated_thetas - true_thetas)^2, na.rm = TRUE))
  avg_length <- mean(test_lengths, na.rm = TRUE)

  tibble::tibble(Bias = bias, RMSE = rmse, Avg_Length = avg_length)
}
