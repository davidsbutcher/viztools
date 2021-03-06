% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_heatmap_vertical.R
\name{make_heatmap_vertical}
\alias{make_heatmap_vertical}
\title{Make a heatmap of masses split by fraction}
\usage{
make_heatmap_vertical(
  df,
  heatmapType = "Protein",
  binSize = 1000,
  savePDF = FALSE,
  outputDir = getwd(),
  pdfPrefix = format(Sys.time(), "\%Y\%m\%d_\%H\%M\%S"),
  massColname = "ObservedPrecursorMass",
  fractionColname = "fraction",
  Yrange = NULL,
  countRange = NULL
)
}
\arguments{
\item{df}{A data frame containing columns for mass (in Daltons) and fraction.}

\item{heatmapType}{Type of heatmap to create, typically "Protein" or
"Proteoform".This only affects the legend title and output file name and
can be any string.}

\item{binSize}{Size of the mass bin in Daltons. Masses in the data frame
should be in Daltons.}

\item{savePDF}{Boolean value (TRUE or FALSE). Should a PDF be saved to the
output directory?}

\item{outputDir}{Directory to place output files in.}

\item{pdfPrefix}{String to use to prepend heatmap names. Defaults to date and
time.}

\item{massColname}{Name of column containing masses. Defaults to
"ObservedPrecursorMass".}

\item{fractionColname}{Name of column containing fractions. Defaults to
"fraction".}

\item{Yrange}{Numeric vector of length 2 specifying range of heatmap in kDa.
Defaults to NULL, and range is determined by rounding highest mass bin up to
the nearest interval of 5.}

\item{countRange}{Numeric vector of length 2 specifying range of counts to
use for generating heatmap fill color range. Defaults to NULL, which
determines color range automatically using
ggplot2::scale_fill_viridis_c(option = "C", direction = -1)}
}
\value{

}
\description{
Uses a data frame containing mass and fraction columns to generate a heatmap
showing the distribution of masses per fraction with a vertical orientation.
DEPRECATED because make_heatmap() now supports vertical orientations.
}
\examples{

library(magrittr)

tibble::tibble(
   "fraction" =
      c(1,1,1,1,2,2,3,3,3,3),
   "ObservedPrecursorMass" =
      c(1500,3000,4200,4250,3500,4500,10500,12050,12075,14050)
) \%>\%
   make_heatmap(
      heatmapType = "Proteoform",
      savePDF = F,
      binSize = 500
   )
}
