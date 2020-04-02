
#' Make a heatmap of masses split by fraction
#'
#' @description
#' Uses a data frame containing mass and fraction columns to generate a heatmap
#' showing the distribution of masses per fraction.
#'
#' @param df A data frame containing columns for mass (in Daltons) and fraction.
#' @param outputDir Directory to place output files in.
#' @param binSize Size of the mass bin in Daltons. Masses in the data frame
#' should be in Daltons.
#' @param savePDF Boolean value (TRUE or FALSE). Should a PDF be saved to the
#' output directory?
#' @param pdfPrefix String to use to prepend heatmap names. Defaults to date and
#' time.
#' @param massColname Name of column containing masses. Defaults to
#' "ObservedPrecursorMass".
#' @param fractionColname Name of column containing fractions. Defaults to
#' "fraction".
#' @param heatmapType Type of heatmap to create, typically "Protein" or
#' "Proteoform".This only affects the legend title and output file name and
#' can be any string.
#'
#' @return
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#'
#' library(magrittr)
#'
#' tibble::tibble(
#'    "fraction" =
#'       c(1,1,1,1,2,2,3,3,3,3),
#'    "ObservedPrecursorMass" =
#'       c(1500,3000,4200,4250,3500,4500,10500,12050,12075,14050)
#' ) %>%
#'    make_heatmap(
#'       heatmapType = "Proteoform",
#'       savePDF = F,
#'       binSize = 500
#'    )

make_heatmap <-
   function(
      df,
      heatmapType = "Protein",
      binSize = 1000,   # SPECIFY SIZE for heatmap mass bins
      savePDF = FALSE,
      outputDir = getwd(),
      pdfPrefix = format(Sys.time(), "%Y%m%d_%H%M%S"),
      massColname = "ObservedPrecursorMass",
      fractionColname = "fraction"
   ) {

      # Assertions --------------------------------------------------------------

      assertthat::assert_that(
         is.data.frame(df),
         msg = "df is not a recognized dataframe"
      )

      assertthat::assert_that(
         assertthat::has_name(df, c(massColname, fractionColname)),
         msg = "df missing a needed column"
      )

      assertthat::assert_that(
         assertthat::is.dir(outputDir),
         msg = "outputDir is not a recognized path"
      )

      assertthat::assert_that(
         assertthat::is.flag(savePDF),
         msg = "savePDF should be TRUE or FALSE"
      )

      assertthat::assert_that(
         assertthat::is.string(pdfPrefix),
         msg = "pdfPrefix is not a string"
      )

      assertthat::assert_that(
         assertthat::is.string(heatmapType),
         msg = "heatmapType is not a string"
      )

      # Analyze data ------------------------------------------------------------

      # Make bins for binning the data

      bins <-  seq.int(0, 500000, by = binSize)

      # Make enquos and syms for tidy evaluation

      massColname_sym <- rlang::sym(massColname)

      fractionColname_sym <- rlang::sym(fractionColname)

      massColname <- rlang::enquo(massColname)

      fractionColname <- rlang::enquo(fractionColname)

      fillname_sym <- rlang::sym(glue::glue("{heatmapType}\nCount"))

      # Reshape dataframe

         heatmap_data <-
            df %>%
            dplyr::select(!!massColname, !!fractionColname) %>%
            dplyr::mutate(mass_bin = cut(!!massColname_sym, breaks = bins, labels = FALSE)) %>%
            dplyr::group_by(!!fractionColname_sym, mass_bin) %>%
            dplyr::summarize(dplyr::n()) %>%
            dplyr::mutate(mass_bin = (mass_bin * binSize) - binSize) %>%
            dplyr::rename(!!fillname_sym := `dplyr::n()`) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(!!fractionColname := forcats::as_factor(!!fractionColname_sym))

         heatmap <-
            heatmap_data %>%
            ggplot2::ggplot(
               ggplot2::aes(
                  mass_bin/1000, !!fractionColname_sym,
                  fill = !!fillname_sym
               )
            ) +
            ggplot2::geom_tile() +
            ggplot2::scale_fill_viridis_c(option = "C", direction = -1) +
            ggplot2::xlim(
               0,
               plyr::round_any(
                  max(
                     heatmap_data$mass_bin/1000),
                  5,
                  f = ceiling
               )
            ) +
            ggplot2::scale_y_discrete(
               limits = rev(
                  levels(heatmap_data %>% dplyr::pull(!!fractionColname))
                  )
               ) +
            ggplot2::labs(
               x = "Mass Bin (kDa)",
               y = "Fraction"
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(text = ggplot2::element_text(size=18))


      # Save heatmaps ---------------------------------------------------

      if (savePDF == TRUE) {

         pdf(
            file = glue::glue("{outputDir}/{pdfPrefix}_{heatmapType}_heatmap.pdf"),
            width = 8,
            height = 5,
            bg = "transparent"
         )
         print(heatmap)
         dev.off()

      }

      return(heatmap)
   }
