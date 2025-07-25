% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_data_preprocessing.R
\name{preprocess_items}
\alias{preprocess_items}
\title{Preprocess Item Response Data}
\usage{
preprocess_items(data_list, id_col_name, group_col_name, inverse_item_names)
}
\arguments{
\item{data_list}{A list object produced by `load_and_merge_data`.}

\item{id_col_name}{A string for the subject identifier column.}

\item{group_col_name}{A string for the grouping variable column (e.g., "clinical_group").}

\item{inverse_item_names}{A character vector of names for the reverse-scored items.}
}
\value{
A list containing:
  \item{processed_data}{A tibble with the cleaned data.}
  \item{anchor_item_cols}{A character vector of the final anchor item column names.}
  \item{all_item_cols}{A character vector of all final item column names.}
}
\description{
This function performs critical preprocessing steps on the merged item
response data. It homogenizes item scales to start at 0, recodes reverse-scored
items, handles missing values (NAs) by row deletion, and removes items
with zero variance.
}
