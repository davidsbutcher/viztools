#' modify_hit_report
#'
#' @param hitReport A data frame containing a hit report produced by TDViewer.
#' @param isoformReport A data frame containing an isoform report produced by TDViewer.
#' @param proteoformReport A data frame containing a proteoform report produced by TDViewer.
#' @param IDtype Type of ID in the hit report to filter by, either "protein" or "proteoform".
#'
#' @return
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

modify_hit_report <-
   function(
      hitReport,
      isoformReport = NULL,
      proteoformReport = NULL,
      IDtype = "protein"
   ) {

      if (IDtype == "protein") {

         isoforms <-
            isoformReport %>%
            dplyr::pull(`Entry Accession`)

         heatmap_data <-
            hitReport %>%
            dplyr::mutate(filename = `File Name`) %>%
            add_fraction() %>%
            dplyr::select(fraction, everything()) %>%
            dplyr::group_by(fraction, PFR) %>%
            dplyr::filter(`Global Q-value` == min(`Global Q-value`)) %>%
            dplyr::filter(Accession %in% isoforms) %>%
            dplyr::filter(`P-score` == min(`P-score`)) %>%
            dplyr::filter(`E-value` == min(`E-value`)) %>%
            dplyr::filter(`C-score` == max(`C-score`)) %>%
            dplyr::ungroup() %>%
            dplyr::rename("ObservedPrecursorMass" = `Observed Precursor Mass`)

      } else if (IDtype == "proteoform") {

         pforms <-
            proteoformReport %>%
            dplyr::pull(Accession)

         heatmap_data <-
            hitReport %>%
            dplyr::mutate(filename = `File Name`) %>%
            add_fraction() %>%
            dplyr::select(fraction, everything()) %>%
            dplyr::group_by(fraction, Accession) %>%
            dplyr::filter(`Global Q-value` == min(`Global Q-value`)) %>%
            dplyr::filter(Accession %in% pforms) %>%
            dplyr::filter(`P-score` == min(`P-score`)) %>%
            dplyr::filter(`E-value` == min(`E-value`)) %>%
            dplyr::filter(`C-score` == max(`C-score`)) %>%
            dplyr::ungroup() %>%
            dplyr::rename("ObservedPrecursorMass" = `Observed Precursor Mass`)

      }

      return(heatmap_data)

   }
