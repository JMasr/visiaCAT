################################################################################
#
# FILE: R/10_exposure_analysis.R
#
# DESCRIPTION: Contains functions for analyzing item exposure rates from CAT
#              simulation results.
#
################################################################################

#' Calculate Item Exposure Rates from a CAT Simulation
#'
#' @description
#' This function processes the results of a single CAT simulation to calculate
#' the exposure rate for each item in the bank. It provides rates both
#' overall (using catR's pre-calculated values) and broken down by a specified
#' grouping variable (calculated manually by reshaping the response data).
#'
#' @param sim_result The output object from `catR::simulateRespondents`.
#' @param patient_data A data frame or tibble containing subject-level data,
#'   including the `group_col_name`. The number of rows must match the
#'   number of rows in the simulation results.
#' @param item_bank A data frame of the calibrated item bank, which must contain
#'   an 'item' column with the item names.
#' @param group_col_name The name of the column in `patient_data` to use for
#'   grouping (e.g., "clinical_group").
#'
#' @return A tibble with the columns `item_name`, `group`, `n_administrations`,
#'   `n_in_group`, and `exposure_rate`. Includes an "Overall" group.
#'
#' @importFrom dplyr %>% count group_by summarise mutate n left_join bind_rows select starts_with row_number filter
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#' @importFrom rlang !! sym
#' @export
calculate_exposure_rates <- function(sim_result, patient_data, item_bank, group_col_name) {

  message("--- Calculating Item Exposure Rates ---")

  # 1. Create a mapping from item index (used by catR) to item name
  item_map <- tibble::tibble(
    item_index = 1:nrow(item_bank),
    item_name = item_bank$item
  )

  # --- Group-Specific Calculation (Manual) ---

  # 2. Reshape the response data from wide to long format
  # Add a person_id to track individuals across data frames
  person_level_items <- sim_result$responses.df %>%
    dplyr::select(dplyr::starts_with("items.administrated")) %>%
    dplyr::mutate(person_id = dplyr::row_number()) %>%
    tidyr::pivot_longer(
      cols = -person_id,
      names_to = "administration_step",
      values_to = "item_index"
    ) %>%
    # Remove NA rows, which represent steps after a person's test ended
    dplyr::filter(!is.na(item_index))

  # 3. Prepare patient group data with the same person_id
  patient_groups <- patient_data %>%
    dplyr::mutate(person_id = dplyr::row_number()) %>%
    # Defensive step: Ensure we only consider people who are in the simulation results
    dplyr::filter(person_id %in% person_level_items$person_id) %>%
    dplyr::select(person_id, group = !!rlang::sym(group_col_name))

  # 4. Join to get the group for each administered item
  analysis_data <- dplyr::left_join(person_level_items, patient_groups, by = "person_id")

  # 5. Calculate exposure counts per group
  exposure_by_group <- analysis_data %>%
    dplyr::count(group, item_index, name = "n_administrations")

  # 6. Calculate total number of people in each group
  group_totals <- patient_groups %>%
    dplyr::count(group, name = "n_in_group")

  # 7. Calculate group-specific exposure rates
  rates_by_group <- exposure_by_group %>%
    dplyr::left_join(group_totals, by = "group") %>%
    # Defensive step: Ensure we only use valid item indices
    dplyr::filter(item_index %in% item_map$item_index) %>%
    dplyr::mutate(exposure_rate = n_administrations / n_in_group) %>%
    dplyr::left_join(item_map, by = "item_index") %>%
    dplyr::select(item_name, group, n_administrations, n_in_group, exposure_rate)

  # --- Overall Calculation (using pre-calculated values from catR) ---

  # 8. Use the pre-calculated overall exposure rates from catR
  overall_rates <- tibble::tibble(
    item_index = 1:length(sim_result$exposureRates),
    exposure_rate = sim_result$exposureRates
  ) %>%
    # Defensive step: Ensure we only use valid item indices
    dplyr::filter(item_index %in% item_map$item_index) %>%
    dplyr::left_join(item_map, by = "item_index") %>%
    dplyr::mutate(
      group = "Overall",
      n_in_group = nrow(patient_data),
      # Back-calculate the number of administrations for a complete report
      n_administrations = round(exposure_rate * n_in_group)
    ) %>%
    dplyr::select(item_name, group, n_administrations, n_in_group, exposure_rate)

  # 9. Combine group-specific and overall results
  final_rates <- dplyr::bind_rows(rates_by_group, overall_rates)

  message("Exposure rate calculation complete.")
  return(final_rates)
}
