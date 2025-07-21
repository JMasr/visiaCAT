################################################################################
#
# FILE: analysis/run_analysis.R
#
# TITLE: Main Execution Script for visiaCat Project
#
# DESCRIPTION: This script serves as the main entry point to run the entire
#              visiaCat analysis pipeline, from data loading to CAT simulation
#              and evaluation.
#
# AUTHOR: Jose M. Ramirez
# DATE: 2025-07-18
#
################################################################################

# --- SETUP ---
# Load required libraries and the custom package functions
# ------------------------------------------------------------------------------
# renv::restore() # Uncomment if using renv for dependency management
library(tidyverse)
library(here)
library(parallel)
library(pbapply) # Added for the progress bar in parallel operations
devtools::load_all() # Loads all functions from the R/ directory

# --- CONFIGURATION ---
# Define all paths and parameters in one place for easy modification.
# ------------------------------------------------------------------------------
# Input paths
PATH_ANCHOR <- here("data", "raw", "q_anchor")
PATH_I_BANK <- here("data", "raw", "q_bank")

# Output paths
OUTPUT_DIR <- here("output")
PLOT_DIR <- here("output", "plots")
RESULTS_DIR <- here("output", "results")

# Create output directories if they don't exist
dir.create(OUTPUT_DIR, showWarnings = FALSE)
dir.create(PLOT_DIR, showWarnings = FALSE)
dir.create(RESULTS_DIR, showWarnings = FALSE)

# Column names
ID_COL_NAME <- "crd"
GROUP_COL_NAME <- "grupo_clinico"
DIF_GROUPING_VARIABLE <- "grupo_clinico"

# Item parameters
# ACTION REQUIRED: Define the names of any reverse-scored items here.
INVERSE_ITEM_NAMES <- c()

# ==============================================================================
# --- PHASE 1: PRE-PROCESSING DATA ---
# ==============================================================================
message("--- RUNNING PHASE 1: DATA PREPARATION ---")
loaded_data <- load_and_merge_data(
  path_anchor = PATH_ANCHOR,
  path_item_bank = PATH_I_BANK,
  id_col_name = ID_COL_NAME,
  group_col_name = GROUP_COL_NAME
)

preprocessed_data_list <- preprocess_items(
  data_list = loaded_data,
  id_col_name = ID_COL_NAME,
  group_col_name = GROUP_COL_NAME,
  inverse_item_names = INVERSE_ITEM_NAMES
)

data_with_anchor_theta <- calculate_anchor_theta(preprocessed_data_list)

# Get the updated, final list of item columns after preprocessing
final_anchor_cols <- preprocessed_data_list$anchor_item_cols
final_bank_i_cols <- preprocessed_data_list$bank_item_cols

# Save Phase 1 results
readr::write_csv(
  data_with_anchor_theta,
  file.path(RESULTS_DIR, "data_with_anchor_theta.csv")
)
message("--- Phase 1 Complete! ---")

# ==============================================================================
# --- PHASE 2: Co-calibration Bank  ---
# ==============================================================================
message("\n--- RUNNING PHASE 2: ITEM BANK CALIBRATION ---")
calibrated_item_bank <- calibrate_bank(
  data_phase_1 = data_with_anchor_theta,
  anchor_item_cols = final_anchor_cols,
  bank_item_cols = final_bank_i_cols
)

# Save the calibrated bank for future use or inspection
readr::write_csv(
  calibrated_item_bank,
  file.path(RESULTS_DIR, "calibrated_item_bank.csv")
)
message("\n--- Phase 2 Complete! ---")

# ==============================================================================
# --- PHASE 3: CAT Simulation & Optimization ---
# ==============================================================================
message("\n--- SETTING UP PHASE 3 ---")
# Prepare the item bank for catR
catr_bank <- prepare_catr_bank(calibrated_item_bank)

# Use the anchor thetas as the true ability levels for the simulation population
true_thetas <- data_with_anchor_theta$theta_anchor

# Define the simulation scenarios to be tested
scenarios <- expand_grid(
  item_select_rule = c("MFI", "MEI"),
  stop_condition = c("High Precision", "Standard Precision", "Short Test")
) %>%
  mutate(
    se_threshold = case_when(
      stop_condition == "High Precision" ~ 0.25,
      stop_condition == "Standard Precision" ~ 0.30,
      stop_condition == "Short Test" ~ 0.30
    ),
    max_items = case_when(
      stop_condition == "High Precision" ~ 30,
      stop_condition == "Standard Precision" ~ 25,
      stop_condition == "Short Test" ~ 15
    )
  )
message("Defined ", nrow(scenarios), " simulation scenarios to run.")
print(scenarios)

# --- PHASE 3: PARALLEL SIMULATION EXECUTION ---
# ------------------------------------------------------------------------------
message("\n--- RUNNING PHASE 3: PARALLEL SIMULATION ---")

# Detect the number of available CPU cores, leaving one free for system processes
n_cores <- detectCores() - 1
message("Setting up a parallel cluster with ", n_cores, " cores...")

# Create the cluster
cl <- makeCluster(n_cores)

# Get a list of all functions defined in our package environment
package_functions <- ls("package:visiaCat")

# Export necessary variables AND all our custom package functions to each worker node
clusterExport(cl,
              varlist = c("true_thetas",
                          "catr_bank",
                          "PLOT_DIR",
                          "RESULTS_DIR",
                          package_functions))

# Convert scenarios data.frame to a list of single-row data.frames for lapply
scenarios_list <- split(scenarios, seq(nrow(scenarios)))

# Run the simulations in parallel using pblapply for a progress bar
parallel_results <- pblapply(cl = cl, X = scenarios_list, FUN = function(current_scenario) {
  # This code runs on each worker node for one scenario

  # 1. Run the core CAT simulation
  sim_result <- run_cat_scenario(
    scenario = current_scenario,
    thetas = true_thetas,
    item_bank = catr_bank
  )

  # 2. Evaluate the performance of the simulation
  performance_metrics <- evaluate_cat_performance(
    simulation_result = sim_result,
    true_thetas = true_thetas
  )

  # 3. Generate and save diagnostic plots
  save_cat_diagnostic_plots(
    simulation_result = sim_result,
    scenario = current_scenario,
    output_path = PLOT_DIR
  )

  # 4. Save the full simulation object (.rds file)
  result_filename <- sprintf(
    "simulation_result_%s_%s.rds",
    current_scenario$item_select_rule,
    gsub(" ", "_", current_scenario$stop_condition)
  )
  saveRDS(sim_result, file = file.path(RESULTS_DIR, result_filename))

  # 4. Return a list containing the scenario definition and its performance
  return(
    list(
      scenario = current_scenario,
      performance = performance_metrics,
      simulation_object = sim_result
    )
  )
})

# Stop the cluster once the work is done
stopCluster(cl)
message("Parallel processing complete. Cluster stopped.")

# --- AGGREGATE RESULTS AND CREATE FINAL REPORT ---
# ------------------------------------------------------------------------------
message("\n--- AGGREGATING RESULTS AND CREATING FINAL REPORT ---")

# Use purrr::map_df to elegantly combine the results into a single data frame
final_report <- map_df(parallel_results, ~{
  bind_cols(.x$scenario, .x$performance)
}) %>%
  select(item_select_rule, stop_condition, se_threshold, max_items, Bias, RMSE, Avg_Length) %>%
  arrange(RMSE)

# Print the final report to the console
message("\n=========================================================")
message("--- CAT SIMULATION PERFORMANCE REPORT ---")
message("=========================================================")
print(final_report)

# Save the final report to a CSV file
report_filename <- file.path(RESULTS_DIR, "final_simulation_report.csv")
write_csv(final_report, report_filename)
message(sprintf("\nFinal report saved to: %s\n", report_filename))

# ==============================================================================
# --- PHASE 4: VALIDATION ANALYSIS ---
# ==============================================================================
message("\n--- RUNNING PHASE 4: VALIDATION ANALYSIS ---")

# Performance Analysis by Group
# ----------------------------------
message("\n--- Analyzing performance by group for each scenario ---")

performance_by_group <- imap_dfr(parallel_results, ~{
  # .x is the result list, .y is the index/name of the list element
  scenario_name <- .x$scenario %>% unite("name", item_select_rule, stop_condition, sep = "_") %>% pull(name)

  analyze_performance_by_group(
    sim_result = .x$simulation_object,
    scenario = .x$scenario,
    patient_data = data_with_anchor_theta,
    id_col_name = ID_COL_NAME,
    group_col_name = GROUP_COL_NAME
  ) %>%
    mutate(scenario = scenario_name, .before = 1)
})

message("\n--- PERFORMANCE BY GROUP REPORT ---")
print(performance_by_group)
write_csv(
  performance_by_group,
  file.path(RESULTS_DIR, "report_performance_by_group.csv")
)


# --- FINAL GUIDANCE ---
# ------------------------------------------------------------------------------
message("\n=========================================================")
message("--- FINAL GUIDANCE FOR CAT SELECTION ---")
message("=========================================================")
message("To select the best scenario, consider the following points:")
message("1. Review the 'Overall Performance' report. Prioritize scenarios with low RMSE and Bias.")
message("2. Review the 'Performance by Group' report. Ensure the chosen scenario performs well across ALL groups, not just on average.")
message("3. Consider 'Average Length'. A scenario with a slightly higher RMSE but a much shorter test might be practically superior.")
message("4. Review the 'DIF Report'. If critical items show significant DIF, consider removing them and re-running the calibration and simulation (Phases 2-4).")
message("5. The OPTIMAL CAT is a balance between PRECISION (low RMSE), EFFICIENCY (short length), and FAIRNESS (no major DIF).")


message("\n--- visiaCat ANALYSIS COMPLETE ---")
