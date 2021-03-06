% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{add_fraction}
\alias{add_fraction}
\title{add_fraction}
\usage{
add_fraction(df, filename_colname = filename)
}
\arguments{
\item{df}{A data frame containing a column corresponding to MS data file names.}

\item{filename_colname}{The name of the column containing the data file names.}
}
\value{
A data frame containing a new column corresponding to fraction names.
}
\description{
Parses filenames in a dataframe to get the fraction number.
}
