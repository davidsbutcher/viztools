
#' Make a heatmap of masses split by fraction
#'
#' @description
#' Uses a data frame containing mass and fraction columns to generate a heatmap
#' showing the distribution of masses per fraction with a vertical orientation.
#' DEPRECATED because make_heatmap() now supports vertical orientations.
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
#' @param Yrange Numeric vector of length 2 specifying range of heatmap in kDa.
#' Defaults to NULL, and range is determined by rounding highest mass bin up to
#' the nearest interval of 5.
#' @param countRange Numeric vector of length 2 specifying range of counts to
#' use for generating heatmap fill color range. Defaults to NULL, which
#' determines color range automatically using
#' ggplot2::scale_fill_viridis_c(option = "C", direction = -1)
#'
#' @return
#'
#' @importFrom magrittr %>%
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

make_heatmap_vertical <-
   function(
      df,
      heatmapType = "Protein",
      binSize = 1000,   # SPECIFY SIZE for heatmap mass bins
      savePDF = FALSE,
      outputDir = getwd(),
      pdfPrefix = format(Sys.time(), "%Y%m%d_%H%M%S"),
      massColname = "ObservedPrecursorMass",
      fractionColname = "fraction",
      Yrange = NULL,
      countRange = NULL
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
         assertthat::is.dir(dirname(outputDir)),
         msg = "outputDir parent directory is not a recognized path"
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

      if (is.null(Yrange) == FALSE) {

         assertthat::assert_that(
            is.numeric(Yrange),
            msg = "Yrange is not numeric"
         )

      }

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

      # Make plot

      heatmap <-
         heatmap_data %>%
         ggplot2::ggplot(
            ggplot2::aes(
               !!fractionColname_sym,
               mass_bin/1000,
               fill = !!fillname_sym
            )
         ) +
         ggplot2::geom_tile(
            position = ggplot2::position_nudge(y = (binSize/2)/1000)
            ) +
         ggplot2::scale_x_discrete(
            limits =
               levels(heatmap_data %>% dplyr::pull(!!fractionColname))
         ) +
         ggplot2::scale_y_continuous(
            breaks = scales::pretty_breaks()
         ) +
         ggplot2::labs(
            y = "Mass Bin (kDa)",
            x = "Fraction"
         ) +
         ggplot2::theme_minimal() +
         ggplot2::theme(
            text = ggplot2::element_text(size=18),
            panel.grid.major = ggplot2::element_line(color = "gray"),
            panel.grid.minor = ggplot2::element_line(color = "lightgray")
         )

      # Add Yrange to plot

      if (is.null(Yrange) == FALSE) {

         heatmap <-
            heatmap +
            ggplot2::scale_y_continuous(
               breaks = scales::pretty_breaks(),
               limits = Yrange,
               expand = c(0,0)
            )
      }

      # Specify count range for heatmap fill

      if (is.null(countRange) == TRUE) {

         heatmap <-
            heatmap +
            ggplot2::scale_fill_viridis_c(option = "C", direction = -1)

      } else {

         heatmap <-
            heatmap +
            ggplot2::scale_fill_gradient2(
               low = "#F0F921FF",
               mid = "#CC4678FF",
               high = "#0D0887FF",
               midpoint = mean(countRange),
               limits = c(floor(countRange[[1]]), ceiling(countRange[[2]]))
            )

      }

      # Save heatmaps ---------------------------------------------------

      if (savePDF == TRUE) {

         if (dir.exists(outputDir) == FALSE) {
            dir.create(outputDir)
         }

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
