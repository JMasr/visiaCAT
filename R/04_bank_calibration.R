################################################################################
#
# FILE: R/04_bank_calibration.R
#
# DESCRIPTION: Contains the function for co-calibrating the item bank using a
# fixed-anchor approach.
#
################################################################################

#' Calibrate Item Bank using a Fixed Anchor Test
#'
#' @description
#' This function performs a co-calibration of a full item bank against a set of
#' anchor items with fixed parameters. This method, known as fixed-anchor
#' calibration, places all item parameters onto the same scale defined by the
#' anchor test.
#'
#' The process involves:
#' 1. Calibrating the anchor items alone to establish the reference scale.
#' 2. Creating a parameter template for the full item set (anchor + bank).
#' 3. Fixing the anchor item parameters in the template to their calibrated values.
#' 4. Calibrating the full item set, where only the new bank item parameters are estimated.
#'
#' @param data_phase_1 A tibble containing the full response matrix, typically
#'   the output from the preprocessing phase.
#' @param anchor_item_cols A character vector of column names for the anchor items.
#' @param bank_item_cols A character vector of column names for all items (anchor + bank).
#'
#' @return A data.frame (tibble) containing the calibrated IRT parameters for all items,
#'   with anchor item parameters fixed and bank item parameters estimated.
#'
#' @importFrom dplyr %>% select filter rename
#' @importFrom tibble rownames_to_column
#' @importFrom mirt mirt mirt.model mod2values coef
#'
#' @export
calibrate_bank <- function(data_phase_1, anchor_item_cols, bank_item_cols) {
  all_responses <- data_phase_1 %>% dplyr::select(dplyr::all_of(bank_item_cols))

  # Step 2.1: Calibrate the anchor test alone to establish the reference scale.
  message("Step 2.1: Calibrating anchor to establish reference scale...")
  anchor_responses <- data_phase_1 %>% dplyr::select(dplyr::all_of(anchor_item_cols))
  anchor_model <- mirt::mirt(
    data = anchor_responses,
    model = 1,
    itemtype = 'graded',
    verbose = FALSE
  )

  # Step 2.2: Create a parameter template for the entire item bank.
  message("Step 2.2: Creating full parameter template...")
  model_definition_full <- mirt::mirt.model(paste0('F = 1-', length(bank_item_cols)))
  full_pars_template <- mirt::mirt(
    all_responses,
    model_definition_full,
    itemtype = 'graded',
    pars = 'values'
  )

  # Step 2.3: Transfer anchor parameters to the full template and fix them.
  message("Step 2.3: Transferring and fixing anchor parameters in template...")
  anchor_pars <- mirt::mod2values(anchor_model) %>%
    dplyr::filter(item %in% anchor_item_cols)

  # Find the rows in the full template that correspond to the anchor parameters
  anchor_rows_in_template <- which(full_pars_template$item %in% anchor_item_cols)

  # Replace the values and set the estimation flag to FALSE
  full_pars_template[anchor_rows_in_template, "value"] <- anchor_pars$value
  full_pars_template[anchor_rows_in_template, "est"] <- FALSE

  # Step 2.4: Calibrate the full model using the prepared parameter template.
  message("Step 2.4: Calibrating full bank with fixed anchor parameters...")
  co_calibrated_model <- mirt::mirt(
    data = all_responses,
    model = model_definition_full,
    pars = full_pars_template,
    verbose = FALSE,
    technical = list(NCYCLES = 2000)
  )
  message("Anchored model successfully fitted.")

  # Step 2.5: Extract and format parameters for all items.
  item_parameters <- mirt::coef(co_calibrated_model, IRTpars = TRUE, simplify = TRUE)
  calibrated_bank_df <- as.data.frame(item_parameters$items) %>%
    tibble::rownames_to_column(var = "item")

  return(calibrated_bank_df)
}
