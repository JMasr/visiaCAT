################################################################################
#
# FILE: analysis/run_dif_analysis.R
#
# TITLE: Differential Item Functioning (DIF) Analysis
#
# DESCRIPTION: This script performs a computationally intensive DIF analysis on
#              the full item bank. It should be run separately after the main
#              simulation pipeline is complete.
#
# AUTHOR: Jose M. Ramirez
# DATE: 2025-07-21
#
################################################################################

# --- 1. SETUP ---
# ------------------------------------------------------------------------------
library(tidyverse)
library(here)
devtools::load_all()


# --- 2. CONFIGURATION ---
# ------------------------------------------------------------------------------
# Define the path to your results directory
RESULTS_DIR <- here::here("output", "results")

# ACTION REQUIRED: Define the column name of the grouping variable for DIF analysis.
# This variable must exist in the "data_with_anchor_theta.csv" file.
DIF_GROUPING_VARIABLE <- "grupo_clinico"


# --- 3. LOAD REQUIRED DATA ---
# ------------------------------------------------------------------------------
message("--- Loading data for DIF analysis ---")

# Load patient data (which includes the response matrix and grouping variable)
patient_data_path <- file.path(RESULTS_DIR, "data_with_anchor_theta.csv")
if (!file.exists(patient_data_path)) {
  stop("Patient data file not found. Please run the main 'run_analysis.R' script first to generate it.")
}
patient_data <- readr::read_csv(patient_data_path, show_col_types = FALSE)

# Load the calibrated item bank to get the final list of all item names
item_bank_path <- file.path(RESULTS_DIR, "calibrated_item_bank.csv")
if (!file.exists(item_bank_path)) {
  stop("Calibrated item bank file not found. Please run 'run_analysis.R' first.")
}
item_bank <- readr::read_csv(item_bank_path, show_col_types = FALSE)

# Get the list of all item columns that were included in the calibration
final_all_item_cols <- item_bank$item


# --- 4. RUN DIFFERENTIAL ITEM FUNCTIONING (DIF) ANALYSIS ---
# ------------------------------------------------------------------------------
# This section can be very time-consuming.
# ==============================================================================
dif_report <- run_dif_analysis(
  patient_data = patient_data,
  item_bank_cols = final_all_item_cols,
  dif_variable = DIF_GROUPING_VARIABLE
)

# --- 5. PROCESS AND SAVE RESULTS ---
# ------------------------------------------------------------------------------
if (!is.null(dif_report)) {
  message("\n--- DIF ANALYSIS REPORT ---")
  # The raw output from mirt::DIF can be printed directly
  print(dif_report)

  # For a cleaner summary, let's show items with significant p-values
  significant_dif <- as.data.frame(dif_report) %>%
    filter(p < 0.05)

  if (nrow(significant_dif) > 0) {
    message("\nItems with significant DIF (p < 0.05):")
    print(significant_dif)
  } else {
    message("\nNo items with significant DIF were found.")
  }

  # Save the full DIF report to a CSV file
  report_filename <- file.path(RESULTS_DIR, "report_dif_analysis.csv")
  write_csv(
    as.data.frame(dif_report),
    report_filename
  )
  message(sprintf("\nFull DIF report saved to: %s", report_filename))

} else {
  message("DIF analysis was skipped or returned no results.")
}

message("\n--- DIF Analysis Script Complete ---")
