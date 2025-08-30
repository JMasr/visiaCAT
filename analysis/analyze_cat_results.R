################################################################################
#
# FILE: analysis/analyze_cat_results.R
#
# TITLE: Explicability and Content Analysis of a Selected CAT Scenario
#
# DESCRIPTION: This script loads a specific, pre-computed CAT simulation result
#              and performs an in-depth analysis of its behavior, focusing on
#              item exposure rates to provide clinical context.
#
# AUTHOR: Jose M. Ramirez
# DATE: 2025-07-21
#
################################################################################

# --- 1. SETUP ---
# ------------------------------------------------------------------------------
library(tidyverse)
library(here)
devtools::load_all() # Load visiaCat package functions

# --- 2. CONFIGURATION: CHOOSE THE SCENARIO TO ANALYZE ---
# ------------------------------------------------------------------------------
# ACTION REQUIRED: Set these variables to match the .rds file you want to analyze.
GROUP_COL_NAME <- "clinical_group"
TARGET_ITEM_SELECT_RULE <- "MFI"
TARGET_STOP_CONDITION <- "High Precision" # Must match exactly: "High Precision", "Standard Precision", or "Short Test"
TOP_N_ITEMS <- 100 # How many top items to show in the report

# --- 3. LOAD REQUIRED DATA ---
# ------------------------------------------------------------------------------
message("--- Loading data for analysis ---")

# Define paths
RESULTS_DIR <- here("output", "results")

# Load patient data (which includes the grouping variable)
# This assumes the main run_analysis.R script has been run and created this file.
patient_data_path <- file.path(RESULTS_DIR, "data_with_anchor_theta.csv")
if (!file.exists(patient_data_path)) {
  stop("Patient data file not found. Please run the main 'run_analysis.R' script first.")
}
patient_data <- readr::read_csv(patient_data_path, show_col_types = FALSE)

# Load the calibrated item bank to get item names
item_bank_path <- file.path(RESULTS_DIR, "calibrated_item_bank.csv")
if (!file.exists(item_bank_path)) {
  stop("Calibrated item bank file not found. Please run 'run_analysis.R' first.")
}
item_bank <- readr::read_csv(item_bank_path, show_col_types = FALSE)

# Construct the filename for the target simulation result
target_filename <- sprintf(
  "simulation_result_%s_%s.rds",
  TARGET_ITEM_SELECT_RULE,
  gsub(" ", "_", TARGET_STOP_CONDITION)
)
target_rds_path <- file.path(RESULTS_DIR, target_filename)

if (!file.exists(target_rds_path)) {
  stop("Target simulation file not found: ", target_rds_path)
}
message("Loading target simulation: ", target_filename)
target_sim_result <- readRDS(target_rds_path)


# --- 4. RUN ITEM EXPOSURE ANALYSIS ---
# ------------------------------------------------------------------------------
exposure_rates <- calculate_exposure_rates(
  sim_result = target_sim_result,
  patient_data = patient_data,
  item_bank = item_bank,
  group_col_name = GROUP_COL_NAME
)

# --- 5. GENERATE AND DISPLAY THE TOP N REPORT ---
# ------------------------------------------------------------------------------
message("\n=========================================================")
message(sprintf("--- TOP %d MOST USED ITEMS REPORT ---", TOP_N_ITEMS))
message("=========================================================")
message(sprintf("Scenario: %s with %s",
                TARGET_ITEM_SELECT_RULE,
                TARGET_STOP_CONDITION))

top_n_report <- exposure_rates %>%
  group_by(group) %>%
  slice_max(order_by = exposure_rate, n = TOP_N_ITEMS) %>%
  arrange(group, desc(exposure_rate)) %>%
  select(group, item_name, exposure_rate)

print(top_n_report, n = Inf)

# Save the report to a CSV file
report_filename <- file.path(
  RESULTS_DIR,
  sprintf("report_top_%d_items_%s_%s.csv",
          TOP_N_ITEMS, TARGET_ITEM_SELECT_RULE,
          gsub(" ", "_", TARGET_STOP_CONDITION))
)
write_csv(top_n_report, report_filename)
message(sprintf("\nTop N report saved to: %s\n", report_filename))

# --- 6. SAVE FINAL THETA ESTIMATES ---
# ------------------------------------------------------------------------------
message("\n--- Saving final theta estimates for the selected scenario ---")

# Create a data frame with the patient identifier, group, and final theta score
final_thetas_df <- tibble(
  !!ID_COL_NAME := patient_data[[ID_COL_NAME]],
  !!GROUP_COL_NAME := patient_data[[GROUP_COL_NAME]],
  "true_theta" = target_sim_result$thetas,
  estimated_theta = target_sim_result$estimatedThetas
)

# Construct the output filename
theta_filename <- file.path(
  RESULTS_DIR,
  sprintf("final_thetas_%s_%s.csv",
          TARGET_ITEM_SELECT_RULE,
          gsub(" ", "_", TARGET_STOP_CONDITION))
)

# Save the data frame to a CSV file
write_csv(final_thetas_df, theta_filename)
message(sprintf("Final theta estimates saved to: %s\n", theta_filename))
