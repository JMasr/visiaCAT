% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_anchor_calibration.R
\name{calculate_anchor_theta}
\alias{calculate_anchor_theta}
\title{Calculate Theta Scores from Anchor Items}
\usage{
calculate_anchor_theta(data_list)
}
\arguments{
\item{data_list}{A list object produced by `preprocess_items`.}
}
\value{
A tibble containing the processed data from the input, with two
  new columns appended: `theta_anchor` and `se_theta_anchor`.
}
\description{
This function fits a unidimensional graded response model (GRM) to the anchor
item data and calculates ability estimates (theta scores) for each person
using the Expected a Posteriori (EAP) method.
}
