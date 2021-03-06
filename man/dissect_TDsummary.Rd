% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dissect_TDsummary.R
\name{dissect_TDsummary}
\alias{dissect_TDsummary}
\title{Dissect a TDSummary file}
\usage{
dissect_TDsummary(
  TDsummarypath,
  shortName = NULL,
  UpSetType = "protein",
  proteinSheetName = "fractions_protein",
  pformSheetName = "fractions_proteoform",
  unfracSheetName = "runs_protein"
)
}
\arguments{
\item{TDsummarypath}{Full path to TDsummary file. Should be .xlsx format.}

\item{shortName}{Analysis shortnames to use to make UpSets.}

\item{UpSetType}{Type of UpSet plot to make - "protein", "proteoform", or
"protein_unfrac"}
}
\value{
A list of UniProt accessions or proteoform record numbers properly formatted
for use with the UpSet_maker() function.
}
\description{
Extracts data from sheets in a top-down summary file and formats it for use
in making an UpSet plot with UpSet_maker() or intersection degree plot with
make_intersect_plot() . Searches for a sheet in the TDsummary named according
to the arguments shortName and proteinSheetName, pformSheetName, or
unfracSheetName in the form "{shortName}_{sheetName}", e.g.
"peppi04d_fractions_protein". This function is included for use in the package
author's workflow and may not be useful for others.
}
\examples{
\dontrun{
dissect_TDsummary(
   "C:/Users/Bakunin/Documents/TDdatasummary.xlsx",
   c("peppi04d"),
   UpSetType = "protein"
)
}

}
