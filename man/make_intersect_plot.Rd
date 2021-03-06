% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_intersect_plot.R
\name{make_intersect_plot}
\alias{make_intersect_plot}
\title{Make an intersection degree plot for a set of unique identifiers}
\usage{
make_intersect_plot(
  IDlist,
  shortNames,
  mediumUsed,
  fracMethod,
  Yrange = c(0, 100),
  plotType = "Protein"
)
}
\arguments{
\item{IDlist}{A list of vectors. Each list item should be a vector of unique
identifiers corresponding to protein or proteoform IDs, e.g. UniProt accession
numbers or proteoform record numbers from the Consortium for Top-Down Proteomics
proteoform atlas. This function is deprecated in favor of `make_intersection_degree_plot()`.}

\item{shortNames}{Vector of short names}

\item{mediumUsed}{Vector of media used}

\item{fracMethod}{Vector of frac methods}

\item{Yrange}{Percentage range to use for the Y-axis of the plot. Defaults to `c(0,100)`.}

\item{plotType}{Type of plot to create, typically "Protein" or "Proteoform".This only
affects the Y-axis title and can be any string.}
}
\value{

}
\description{
Make an intersection degree plot for a set of unique identifiers
}
