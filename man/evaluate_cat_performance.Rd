% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/06_simulation_core.R
\name{evaluate_cat_performance}
\alias{evaluate_cat_performance}
\title{Evaluate CAT Simulation Performance}
\usage{
evaluate_cat_performance(simulation_result, true_thetas)
}
\arguments{
\item{simulation_result}{The output object from `catR::simulateRespondents`.}

\item{true_thetas}{The original vector of true theta values used in the simulation.}
}
\value{
A single-row tibble with the columns `Bias`, `RMSE`, and `Avg_Length`.
}
\description{
Calculates key performance metrics for a completed CAT simulation, including
overall bias, Root Mean Squared Error (RMSE), and the average test length.
}
