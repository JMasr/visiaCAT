################################################################################
#
# FILE: R/01_data_loading.R
#
# DESCRIPTION: Contains functions related to loading and merging data sources
# for the visiaCat project.
#
################################################################################

#' (Internal) Recode Character Item Responses to a Numeric Scale
#'
#' @description
#' This helper function iterates through specified item columns of a dataframe.
#' If a column is of character type, it attempts to automatically convert it to
#' a numeric scale based on a set of predefined rules.
#'
#' @param master_data The dataframe containing the item response data.
#' @param all_item_cols A character vector of column names to process.
#'
#' @return The dataframe with character item columns recoded to numeric.
#'
#' @importFrom dplyr %>% mutate across case_when
#' @importFrom stringr str_to_lower str_starts str_extract
#'
#' @noRd
recode_responses_to_numeric <- function(master_data, all_item_cols) {

  message("\n--- Checking and Recoding Item Response Data ---")

  # Define the Coding logic as a single function to be applied to each column
  recode_column <- function(col_vector) {
    # 1. If column is already numeric, do nothing.
    if (is.numeric(col_vector)) {
      return(col_vector)
    }

    # 2. If column is not numeric, it's likely character. Get unique values.
    unique_vals <- unique(stats::na.omit(col_vector))
    unique_vals_lower <- stringr::str_to_lower(unique_vals)

    # --- Rule A: Binary Coding (Yes/No) ---
    if (all(unique_vals_lower %in% c("yes", "si", "sí", "no"))) {
      message(sprintf("  - Recoding binary column (Yes/No)..."))
      return(dplyr::case_when(
        stringr::str_to_lower(col_vector) %in% c("yes", "si", "sí") ~ 1,
        stringr::str_to_lower(col_vector) == "no" ~ 0,
        TRUE ~ NA_real_
      ))
    }

    # --- Rule B: Numeric Prefix Coding ---
    if (all(stringr::str_starts(unique_vals, "\\d"))) {
      message(sprintf("  - Recoding column from numeric prefix..."))
      return(as.numeric(stringr::str_extract(col_vector, "^\\d+")))
    }

    # --- Rule C: Fallback Ordinal Factor Recoding ---
    # This handles cases like "Never", "Sometimes", "Always"
    message(sprintf("  - Recoding ordinal column using factor conversion..."))
    # The factor levels are created in order of appearance.
    # We subtract 1 to ensure the scale starts at 0 (crucial for IRT models).
    return(as.numeric(as.factor(col_vector)) - 1)
  }

  # Apply the coding function across all specified item columns
  master_data_recoded <- master_data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(all_item_cols), recode_column))

  message("--- Item Recoding Complete ---")
  return(master_data_recoded)
}



#' Load and Merge Anchor and Item Bank Data
#'
#' @description
#' This function discovers and reads all CSV files from specified anchor and
#' item bank directories. It then performs a full join to merge them into a
#' single master dataframe based on a shared ID column. It also validates that
#' the anchor data file does not appear in the item bank directory and that
#' required columns are present.
#'
#' @param path_anchor A string path to the directory containing anchor item data.
#'   The directory should contain at least one CSV file.
#' @param path_item_bank A string path to the directory containing item bank data.
#'   Can contain multiple CSV files.
#' @param id_col_name A string representing the column name used as the unique
#'   identifier for joining datasets (e.g., "subject_id").
#' @param group_col_name A string for the grouping variable column, expected to
#'   be in the anchor data (e.g., "clinical_group").
#'
#' @return A list containing three elements:
#'   \item{master_data}{A tibble with the fully merged data.}
#'   \item{anchor_data_names}{A character vector of column names for the anchor *items*, excluding ID and group columns.}
#'   \item{item_bank_data_names}{A character vector of column names for the bank *items*, excluding the ID column.}
#'
#' @importFrom readr read_csv
#' @importFrom dplyr full_join %>% setdiff
#' @importFrom purrr map reduce
#' @importFrom janitor clean_names
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create dummy directories and files for example
#' dir.create("temp_anchor")
#' dir.create("temp_bank")
#' write.csv(
#'   data.frame(crd = 1, clinical_group = "A", item_a1 = 1),
#'   "temp_anchor/anchor.csv",
#'   row.names = FALSE
#' )
#' write.csv(data.frame(crd = 1, item_b1 = 2), "temp_bank/bank.csv", row.names = FALSE)
#'
#' loaded_data <- load_and_merge_data(
#'   path_anchor = "temp_anchor",
#'   path_item_bank = "temp_bank",
#'   id_col_name = "crd",
#'   group_col_name = "clinical_group"
#' )
#'
#' # Clean up dummy files
#' unlink("temp_anchor", recursive = TRUE)
#' unlink("temp_bank", recursive = TRUE)
#' }
load_and_merge_data <- function(path_anchor, path_item_bank, id_col_name, group_col_name) {

  # --- Discover Anchor and Bank Files ---
  anchor_files <- list.files(path_anchor, pattern = "\\.csv$", full.names = TRUE)
  if (length(anchor_files) == 0) {
    stop("No CSV file found in the anchor data directory: '", path_anchor, "'")
  }

  item_bank_files <- list.files(path_item_bank, pattern = "\\.csv$", full.names = TRUE)

  # --- VALIDATION 1: Check for anchor/bank overlap ---
  anchor_basename <- basename(anchor_files[1])
  bank_basenames <- basename(item_bank_files)

  if (anchor_basename %in% bank_basenames) {
    stop(
      "Validation Error: The anchor data file '", anchor_basename,
      "' was also found in the item bank directory. Please ensure anchor and bank data are in separate, exclusive directories."
    )
  }

  # --- Load Anchor Data ---
  anchor_data <- readr::read_csv(anchor_files[1], show_col_types = FALSE) %>%
    janitor::clean_names()

  # --- VALIDATION 2: Check for required columns in anchor data ---
  if (!id_col_name %in% names(anchor_data)) {
    stop("Validation Error: The specified id column '", id_col_name, "' was not found in the anchor data file.")
  }
  if (!group_col_name %in% names(anchor_data)) {
    stop("Validation Error: The specified group column '", group_col_name, "' was not found in the anchor data file.")
  }

  # Identify and recode anchor item columns
  final_anchor_names <- dplyr::setdiff(names(anchor_data), c(id_col_name, group_col_name))
  anchor_data <- recode_responses_to_numeric(anchor_data, final_anchor_names)

  # --- Load Item Bank Data ---
  if (length(item_bank_files) > 0) {
    item_bank_data <- item_bank_files %>%
      purrr::map(~readr::read_csv(.x, show_col_types = FALSE) %>% janitor::clean_names()) %>%
      purrr::reduce(~dplyr::full_join(.x, .y, by = id_col_name))

    master_data <- dplyr::full_join(anchor_data, item_bank_data, by = id_col_name)
  } else {
    warning("No CSV files found in the item bank directory: '", path_item_bank, "'. Proceeding with anchor data only.")
    master_data <- anchor_data
    # Create a shell tibble for consistency in the return object
    item_bank_data <- tibble::tibble(!!id_col_name := master_data[[id_col_name]])
  }

  message("--- Data Merging Complete ---")
  # Prepare final lists of item names, excluding metadata columns
  final_anchor_names <- dplyr::setdiff(names(anchor_data), c(id_col_name, group_col_name))
  final_bank_names <- dplyr::setdiff(names(item_bank_data), id_col_name)
  all_item_cols <- c(final_anchor_names, final_bank_names)

  # Coding Master Data
  master_data <- recode_responses_to_numeric(master_data, all_item_cols)

  message(sprintf("Total subjects: %d", nrow(master_data)))
  message(sprintf("Total columns (items + IDs): %d", ncol(master_data)))

  return(list(
    master_data = master_data,
    anchor_data_names = final_anchor_names,
    item_bank_data_names = final_bank_names
  ))
}
