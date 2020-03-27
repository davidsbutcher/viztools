
#' dissect_TDsummary
#'
#' @param TDsummarypath
#' Full path to TDsummary file
#' @param shortName
#' Analysis shortnames to make UpSets from
#' @param UpSetType
#' Type of UpSet plot to make - "protein", "proteoform", "protein_unfrac"
#'
#' @return
#'
#' @examples

dissect_TDsummary <-
   function(
      TDsummarypath = NULL,
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
