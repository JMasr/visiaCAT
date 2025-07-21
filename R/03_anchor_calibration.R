################################################################################
#
# FILE: R/03_anchor_calibration.R
#
# DESCRIPTION: Contains functions to perform IRT calibration on the anchor
# test and estimate person ability (theta) scores.
#
################################################################################

#' Calculate Theta Scores from Anchor Items
#'
#' @description
#' This function fits a unidimensional graded response model (GRM) to the anchor
#' item data and calculates ability estimates (theta scores) for each person
#' using the Expected a Posteriori (EAP) method.
#'
#' @param data_list A list object produced by `preprocess_items`.
#'
#' @return A tibble containing the processed data from the input, with two
#'   new columns appended: `theta_anchor` and `se_theta_anchor`.
#'
#' @importFrom dplyr %>% select all_of rename bind_cols as_tibble
#' @importFrom mirt mirt fscores
#' @export
calculate_anchor_theta <- function(data_list) {
  processed_data <- data_list$processed_data
  anchor_item_cols <- data_list$anchor_item_cols
  anchor_responses <- processed_data %>% dplyr::select(dplyr::all_of(anchor_item_cols))

  message("\nFitting IRT model to the anchor test...")
  # Fit a Graded Response Model (GRM)
  anchor_model <- mirt::mirt(
    data = anchor_responses,
    model = 1,
    itemtype = 'graded',
    verbose = FALSE
  )

  message("Calculating EAP scores based on the anchor model...")
  # Calculate ability scores (theta) and their standard errors
  anchor_thetas <- mirt::fscores(
    anchor_model,
    method = 'EAP',
    full.scores = TRUE,
    full.scores.SE = TRUE
  ) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(theta_anchor = F1, se_theta_anchor = SE_F1)

  final_data <- dplyr::bind_cols(processed_data, anchor_thetas)
  message("--- Anchor Theta Calculation Complete ---")
  return(final_data)
}
