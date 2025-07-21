################################################################################
#
# FILE: R/09_dif_analysis.R
#
# DESCRIPTION: Contains functions for performing Differential Item Functioning
#              (DIF) analysis.
#
################################################################################

#' Run Differential Item Functioning (DIF) Analysis
#'
#' @description
#' This function performs a DIF analysis using the `mirt` package to identify
#' items that function differently across specified groups. It fits a
#' multiple-group IRT model and uses Lord's Chi-squared test to compare a
#' constrained model (no DIF) with models where each item's parameters are
#' freely estimated.
#'
#' @param patient_data A data frame containing the full response matrix and
#'   the grouping variable.
#' @param item_bank_cols A character vector of the column names corresponding to
#'   the items to be included in the analysis.
#' @param dif_variable The column name of the grouping variable (e.g., "gender",
#'   "clinical_group").
#'
#' @return A data frame containing the DIF statistics (Chi-squared, df, p-value)
#'   for each item.
#'
#' @importFrom dplyr %>% select all_of
#' @importFrom mirt mirt.model multipleGroup DIF
#' @export
run_dif_analysis <- function(patient_data, item_bank_cols, dif_variable) {

  message(sprintf("\n--- Running DIF Analysis for variable: '%s' ---", dif_variable))

  if (!dif_variable %in% names(patient_data)) {
    warning(sprintf("The DIF grouping variable '%s' was not found in the data. Skipping DIF analysis.", dif_variable))
    return(NULL)
  }

  # Prepare data: response matrix and grouping vector
  response_data <- patient_data %>% dplyr::select(dplyr::all_of(item_bank_cols))
  grouping_vector <- patient_data[[dif_variable]]

  # Define a unidimensional model for all items
  model_spec <- mirt::mirt.model(paste0('F = 1-', ncol(response_data)))

  # Step 1: Fit the constrained multiple-group model (baseline for comparison)
  message("Step 1: Fitting the constrained (no DIF) multiple-group model...")
  constrained_model <- mirt::multipleGroup(
    data = response_data,
    model = model_spec,
    group = grouping_vector,
    invariance = c('slopes', 'intercepts', 'free_means', 'free_var'),
    itemtype = 'graded',
    verbose = FALSE
  )

  # Step 2: Run the DIF test for each item against the constrained model.
  message("Step 2: Performing Lord's Chi-squared test for each item...")
  pars_to_test <- constrained_model@ParObjects[["pars"]][[1]]@ParObjects[["pars"]][[1]]@parnames
  dif_results <- mirt::DIF(
    constrained_model,
    which.par = pars_to_test,
    test = 'lord',
    plotdif = FALSE # Set to FALSE for non-interactive script
  )

  message("DIF analysis complete.")
  return(dif_results)
}
