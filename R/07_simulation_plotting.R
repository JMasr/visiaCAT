################################################################################
#
# FILE: R/07_simulation_plotting.R
#
# DESCRIPTION: Contains functions for visualizing CAT simulation results.
#
################################################################################

#' Generate and Save Basic Diagnostic Plots for a CAT Simulation
#'
#' @description
#' Uses the default plotting method from the `catR` package to generate
#' standard diagnostic plots (e.g., Bias, RMSE across ability levels) and saves
#' them to a single PDF file.
#'
#' @param simulation_result The output object from `catR::simulateRespondents`.
#' @param scenario The single-row data frame describing the scenario being plotted.
#' @param output_path A string path to the directory where the plot PDF will be saved.
#'
#' @return Invisibly returns the path to the generated PDF file.
#'
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics plot
#' @export
save_cat_diagnostic_plots <- function(simulation_result, scenario, output_path) {
  # Create a unique filename for the PDF based on the scenario
  filename <- sprintf("diagnostic_plots_%s_se_%.2f_max_%d.pdf",
                      scenario$item_select_rule,
                      scenario$se_threshold,
                      scenario$max_items)
  pdf_path <- file.path(output_path, filename)

  # Open the PDF device to save all plots to a single file
  grDevices::pdf(pdf_path, width = 11, height = 8.5)

  # --- Generate catR's default plots ---
  # Wrapped in try() to prevent an error in one plot from stopping the script.
  try(graphics::plot(simulation_result, ci = TRUE), silent = TRUE)
  try(graphics::plot(simulation_result, deciles = "deciles"), silent = TRUE)

  # Close the PDF device
  grDevices::dev.off()

  message(sprintf("-> Diagnostic plots saved to: %s", pdf_path))

  # Return the path for potential further use
  invisible(pdf_path)
}
