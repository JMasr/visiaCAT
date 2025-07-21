################################################################################
#
# FILE: R/02_data_preprocessing.R
#
# DESCRIPTION: Contains functions for cleaning and preparing the merged
# response data for IRT analysis.
#
################################################################################

#' Preprocess Item Response Data
#'
#' @description
#' This function performs critical preprocessing steps on the merged item
#' response data. It homogenizes item scales to start at 0, recodes reverse-scored
#' items, handles missing values (NAs) by row deletion, and removes items
#' with zero variance.
#'
#' @param data_list A list object produced by `load_and_merge_data`.
#' @param id_col_name A string for the subject identifier column.
#' @param group_col_name A string for the grouping variable column (e.g., "clinical_group").
#' @param inverse_item_names A character vector of names for the reverse-scored items.
#'
#' @return A list containing:
#'   \item{processed_data}{A tibble with the cleaned data.}
#'   \item{anchor_item_cols}{A character vector of the final anchor item column names.}
#'   \item{all_item_cols}{A character vector of all final item column names.}
#'
#' @importFrom dplyr %>% mutate across select setdiff all_of
#' @importFrom tidyr drop_na
#' @export
preprocess_items <- function(data_list, id_col_name, group_col_name, inverse_item_names) {
  master_data <- data_list$master_data
  anchor_item_cols <- dplyr::setdiff(data_list$anchor_data_names, c(id_col_name, group_col_name))
  bank_item_cols <- dplyr::setdiff(data_list$item_bank_data_names, id_col_name)
  all_item_cols <- c(anchor_item_cols, bank_item_cols)

  message(sprintf(
    "\nIdentified %d anchor items and %d bank items.",
    length(anchor_item_cols), length(bank_item_cols)
  ))

  # --- 1. Homogenize Scales to start at 0 ---
  # This is a CRITICAL step for mirt's 'graded' response model.
  message("Homogenizing item scales to start from 0...")
  master_data <- master_data %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(all_item_cols),
      ~ .x - min(.x, na.rm = TRUE)
    ))

  # --- 2. Recode Inverse Items ---
  if (length(inverse_item_names) > 0) {
    # Max value is calculated AFTER homogenizing scales to 0.
    max_scale_value <- master_data %>%
      dplyr::select(dplyr::all_of(inverse_item_names)) %>%
      max(na.rm = TRUE)

    master_data <- master_data %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(inverse_item_names), ~ max_scale_value - .x))
    message(sprintf("Recoded %d inverse items.", length(inverse_item_names)))
  }

  # --- 3. Analyze and Handle Missing Data (NA) ---
  message("\n--- Analyzing Missing Data ---")
  if (any(is.na(master_data %>% dplyr::select(dplyr::all_of(all_item_cols))))) {
    message("ALERT: Missing data (NA) found in item responses.")
    rows_before <- nrow(master_data)
    master_data <- master_data %>% tidyr::drop_na(dplyr::all_of(all_item_cols))
    rows_after <- nrow(master_data)
    message(sprintf(
      "ACTION: Removed %d rows containing NAs in item columns.",
      rows_before - rows_after
    ))
  } else {
    message("No missing data found in item columns.")
  }

  # --- 4. Check for Zero-Variance Items ---
  message("\n--- Checking for Zero-Variance Items ---")
  items_to_check <- master_data %>% dplyr::select(dplyr::all_of(all_item_cols))
  n_unique_responses <- sapply(items_to_check, function(x) length(unique(x)))
  zero_variance_items <- names(n_unique_responses[n_unique_responses <= 1])

  if (length(zero_variance_items) > 0) {
    warning(sprintf("Found %d zero-variance items. These items will be removed.", length(zero_variance_items)))
    message("Items to be removed: ", paste(zero_variance_items, collapse = ", "))

    master_data <- master_data %>% dplyr::select(-dplyr::all_of(zero_variance_items))
    all_item_cols <- dplyr::setdiff(all_item_cols, zero_variance_items)
    anchor_item_cols <- dplyr::setdiff(anchor_item_cols, zero_variance_items)
    bank_item_cols <- dplyr::setdiff(bank_item_cols, zero_variance_items)
    message("Zero-variance items removed.")
  } else {
    message("No zero-variance items found.")
  }

  return(list(
    processed_data = master_data,
    anchor_item_cols = anchor_item_cols,
    bank_item_cols = bank_item_cols,
    all_item_cols = all_item_cols
  ))
}
