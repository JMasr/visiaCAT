# Title: R Script for K-Fold Stratified Cross-Validation

# ---
# Description:
# This script performs a K-fold stratified split on a dataset.
# Stratification is based on the 'TARGET_COLUMN' to ensure that
# the relative proportions of the clinical groups are maintained in each fold.
#
# The script will:
# 1. Install and load the necessary 'caret' library.
# 2. Create a sample dataset (you can easily replace this with your own data).
# 3. Generate 5 stratified folds.
# 4. Demonstrate how to loop through each fold to either run your analysis
#    or save each fold as a separate CSV file.
# ---

# --- SETUP ---
# Load required libraries and the custom package functions
# ------------------------------------------------------------------------------
# The 'caret' package is a comprehensive framework for classification and
# regression training, and it includes excellent functions for data splitting.
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

library(caret)
library(here)
devtools::load_all(here::here())

# --- CONFIGURATION ---
# Define all paths and parameters in one place for easy modification.
# ------------------------------------------------------------------------------
# Input paths
PATH_ANCHOR <- here("data", "raw", "q_anchor", "paykel.csv")

# Output paths
OUTPUT_DIR <- here("data", "processed", "folds_output")

# Variables
K_FOLDS <- 5
TARGET_COLUMN = 'grupo_clinico'

# --- 1. Load Your Data -------
my_data <- read.csv(PATH_ANCHOR)

message("--- Original Dataset Summary ---")
message("Total rows:", nrow(my_data))

# --- 2. Create K Stratified Folds ---
# We will use the createFolds function from the caret package.
# - 'y' is the vector of outcomes to stratify by "TARGET_COLUMN".
# - 'k' is the number of folds to create.
# - 'list = TRUE' returns the folds as a list of row indices.
# - 'returnTrain = FALSE' get the indices for the test/hold-out part each fold.

fold_test_indices <- createFolds(
  my_data$grupo_clinico,
  k = K_FOLDS,
  list = TRUE,
  returnTrain = FALSE)

# --- 4. Loop Through Folds for Analysis or Saving ---
# This loop iterates from 1 to 5. In each iteration, it:
# 1. Subsets the main dataframe to get the data for the current fold.
# 2. Prints a summary to verify the stratification.
# 3. Provides a placeholder for you to insert your analysis code.
# 4. Saves the current fold's data to a new CSV file.

# Create a directory to store the output files
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR)
}


for (i in 1:K_FOLDS) {
  fold_name <- paste0("Fold", i)

  current_test_indices <- fold_test_indices[[fold_name]]

  # Define the training indices as all row numbers NOT in the test set.
  current_train_indices <- setdiff(1:nrow(my_data), current_test_indices)

  # --- Verification Step: Check for Overlap ---
  # We confirm that the intersection of train and test indices is empty.
  overlap <- intersect(current_train_indices, current_test_indices)
  if (length(overlap) == 0) {
    message("--- Processing ", fold_name, ": ",
            "OK - Train/Test sets are mutually exclusive.---")
  } else {
    message("\n--- WARNING: ", fold_name,
            " has overlapping train/test indices! ---")
  }

  # Subset the data to create the dataframes for the current fold
  train_data <- my_data[current_train_indices, ]
  test_data <- my_data[current_test_indices, ]

  # --- Verification and Analysis ---
  message("Train set rows:", nrow(train_data))
  message("Test set rows:", nrow(test_data))

  message("Distribution of", TARGET_COLUMN, "in", fold_name, "TEST set:")
  print(table(test_data$grupo_clinico))
  message("Distribution of", TARGET_COLUMN, "in", fold_name, "TRAIN set:")
  print(table(train_data$grupo_clinico))

  # --- Save the Fold to a CSV file ---
  # --- Save the Folds to CSV files ---
  train_output_filename <- file.path(
    OUTPUT_DIR,
    paste0("fold_", i, "_train_data.csv")
  )
  write.csv(train_data, train_output_filename, row.names = FALSE)
  message("Saved Train set to:", train_output_filename)

  test_output_filename <- file.path(
    OUTPUT_DIR,
    paste0("fold_", i, "_test_data.csv")
  )
  write.csv(test_data, test_output_filename, row.names = FALSE)
  cat(paste("Saved Test set to:", test_output_filename, "\n"))
}

message("--- Pipeline Step Complete ---")
message("All stratified folds have been generated and saved.")
