################################################################################
#
# FILE: R/05_simulation_helpers.R
#
# DESCRIPTION: Contains helper functions for preparing data for the CAT
# simulation phase.
#
################################################################################

#' Prepare Calibrated Item Bank for catR
#'
#' @description
#' This function converts a data frame of calibrated item parameters into the
#' specific matrix format required by the `catR` package. It selects the
#' relevant parameter columns, validates that no infinite values are present
#' (removing items if necessary), and structures the data as a numeric matrix
#' with item names as rownames.
#'
#' @param item_bank_df A data frame containing the calibrated item bank, with
#'   at least the columns 'item', 'a', and threshold parameters (e.g., 'b1', 'b2', ...).
#'
#' @return A numeric matrix formatted for use with `catR`.
#'
#' @importFrom dplyr %>% select starts_with mutate across if_any filter pull
#' @export
prepare_catr_bank <- function(item_bank_df) {
  message("\n--- Preparing item bank for catR format ---")

  # 1. Select parameter columns and ensure they are numeric
  params_df <- item_bank_df %>%
    dplyr::select(item, a, dplyr::starts_with("b")) %>%
    dplyr::mutate(dplyr::across(-item, as.numeric))

  # 2. Validate and clean items with infinite parameter values
  invalid_items <- params_df %>%
    dplyr::filter(dplyr::if_any(-item, ~is.infinite(.))) %>%
    dplyr::pull(item)

  if (length(invalid_items) > 0) {
    warning(sprintf("Found %d items with infinite parameters. These will be removed for the simulation.", length(invalid_items)))
    message("Items removed: ", paste(invalid_items, collapse = ", "))

    params_df <- params_df %>%
      dplyr::filter(!item %in% invalid_items)
  } else {
    message("Validation complete: No items with infinite parameters found.")
  }

  # 3. Create the final numeric matrix with item names as rownames
  item_names <- params_df$item
  params_matrix <- as.matrix(params_df[, -1])
  rownames(params_matrix) <- item_names

  message("Item bank successfully converted to catR matrix format.")
  return(params_matrix)
}
