
#' Dissect a TDSummary file
#'
#' @description
#' Extracts data from sheets in a top-down summary file and formats it for use
#' in making an UpSet plot with UpSet_maker() or intersection degree plot with
#' make_intersect_plot() . Searches for a sheet in the TDsummary named according
#' to the arguments shortName and proteinSheetName, pformSheetName, or
#' unfracSheetName in the form "{shortName}_{sheetName}", e.g.
#' "peppi04d_fractions_protein".
#'
#' @param TDsummarypath Full path to TDsummary file. Should be .xlsx format.
#' @param shortName Analysis shortnames to use to make UpSets.
#' @param UpSetType Type of UpSet plot to make - "protein", "proteoform", or
#' "protein_unfrac"
#'
#' @return
#' A list of UniProt accessions or proteoform record numbers properly formatted
#' for use with the UpSet_maker() function.
#'
#' @examples
#' \dontrun{
#' dissect_TDsummary(
#'    "C:/Users/Bakunin/Documents/TDdatasummary.xlsx",
#'    c("peppi04d"),
#'    UpSetType = "protein"
#' )
#' }
#'
#' @importFrom magrittr %>%
#'
#' @export


dissect_TDsummary <-
   function(
      TDsummarypath,
      shortName = NULL,
      UpSetType = "protein",
      proteinSheetName = "fractions_protein",
      pformSheetName = "fractions_proteoform",
      unfracSheetName = "runs_protein"
   ) {

      if (UpSetType == "protein") {

         upset_data <-
            shortName %>%
            as.list %>%
            purrr::map(function(x) glue::glue("{x}_{proteinSheetName}")) %>%
            purrr::map(function(x) readxl::read_xlsx(TDsummarypath, sheet = x)) %>%
            purrr::map(as.list) %>%
            purrr::map(killNA)

         names(upset_data) <- shortName

      }

      if (UpSetType == "proteoform") {

         upset_data <-
            shortName %>%
            as.list %>%
            purrr::map(function(x) glue::glue("{x}_{pformSheetName}")) %>%
            purrr::map(function(x) readxl::read_xlsx(TDsummarypath, sheet = x)) %>%
            purrr::map(as.list) %>%
            purrr::map(killNA)

         names(upset_data) <- shortName

      }

      if (UpSetType == "protein_unfrac") {

         upset_data <-
            shortName %>%
            as.list %>%
            purrr::map(function(x) glue::glue("{x}_{unfracSheetName}")) %>%
            purrr::map(function(x) readxl::read_xlsx(TDsummarypath, sheet = x)) %>%
            purrr::map(as.list) %>%
            purrr::map(killNA)

         names(upset_data) <- shortName

      }
      return(upset_data)

   }
