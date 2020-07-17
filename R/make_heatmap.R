
#' Make a heatmap of masses split by fraction
#'
#' @description
#' Uses a data frame containing mass and fraction columns to generate a heatmap
#' showing the distribution of masses per fraction.
#'
#' @param df A data frame containing columns for mass (in Daltons) and fraction.
#' @param plotType Type of heatmap to create, typically "Protein" or
#' "Proteoform".This only affects the legend title and output file name and
#' can be any string.
#' @param orientation Controls orientation of heatmap. Set to "h" for horizontal
#' (mass bins on X-axis) or "v" for vertical.
#' @param binSize Size of the mass bin in Daltons. Masses in the data frame
#' should be in Daltons.
#' @param savePDF Boolean value (TRUE or FALSE). Should a PDF be saved to the
#' output directory?
#' @param outputDir Directory to place output files into.
#' @param outputPrefix String to use to prepend heatmap output filenames.
#' Defaults to date and time.
#' @param massColname Name of column containing masses. Defaults to
#' "mass".
#' @param fractionColname Name of column containing fractions. Defaults to
#' "fraction".
#' @param axisRange Numeric vector of length 2 specifying range of heatmap in kDa.
#' Defaults to NULL, and range is determined by rounding highest mass bin up to
#' the nearest interval of 5.
#' @param countRange Numeric vector of length 2 specifying range of counts to
#' use for generating heatmap fill color range. Defaults to NULL, which
#' determines color range automatically using
#' ggplot2::scale_fill_viridis_c(option = "C", direction = -1). Useful for
#' creating multiple heatmaps with matching fill color ranges.
#' @param fontFamily Font family to use for plot. Defaults to "sans".
#'
#' @return
#' A heatmap (a ggplot object)
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' df <-
#'    data.frame(
#'       "fraction" =
#'          c(1,1,1,1,2,2,3,3,3,3),
#'       "mass" =
#'          c(1500,3000,4200,4250,3500,4500,10500,12050,12075,14050)
#'    )
#'
#'    make_heatmap(
#'       df,
#'       plotType = "Protein",
#'       savePDF = F,
#'       binSize = 1000
#'    )

make_heatmap <-
   function(
      df,
      plotType = "Protein",
      orientation = "h",
      binSize = 1000,   # SPECIFY SIZE for heatmap mass bins
      savePDF = FALSE,
      outputDir = getwd(),
      outputPrefix = format(Sys.time(), "%Y%m%d_%H%M%S"),
      massColname = "mass",
      fractionColname = "fraction",
      axisRange = NULL,
      countRange = NULL,
      fontFamily = "sans"
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
         assertthat::is.string(outputPrefix),
         msg = "outputPrefix is not a string"
      )

      assertthat::assert_that(
         assertthat::is.string(plotType),
         msg = "plotType is not a string"
      )

      assertthat::assert_that(
         orientation == "h" | orientation == "v",
         msg = "orientation not set to 'h' or 'v'"
      )

      if (is.null(axisRange) == FALSE) {

         assertthat::assert_that(
            is.numeric(axisRange),
            msg = "axisRange is not numeric"
         )

         assertthat::assert_that(
            length(axisRange) == 2,
            msg = "axisRange should have a length of 2"
         )

      }

      if (is.null(countRange) == FALSE) {

         assertthat::assert_that(
            is.numeric(countRange),
            msg = "countRange is not numeric"
         )

         assertthat::assert_that(
            length(countRange) == 2,
            msg = "countRange should have a length of 2"
         )

      }

      assertthat::assert_that(
         fontFamily %in% c("sans", "serif", "mono"),
         msg = "fontFamily should be sans, serif, or mono"
      )

      # Analyze data ------------------------------------------------------------

      # Make bins for binning the data

      bins <-  seq.int(0, 500000, by = binSize)

      # Make enquos and syms for tidy evaluation

      massColname_sym <- rlang::sym(massColname)

      fractionColname_sym <- rlang::sym(fractionColname)

      massColname <- rlang::enquo(massColname)

      fractionColname <- rlang::enquo(fractionColname)

      fillname_sym <- rlang::sym(glue::glue("{plotType}\nCount"))

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

      # Make horizontal plot

      if (orientation == "h") {

      heatmap <-
         heatmap_data %>%
         ggplot2::ggplot(
            ggplot2::aes(
               mass_bin/1000, !!fractionColname_sym,
               fill = !!fillname_sym
            )
         ) +
         ggplot2::geom_tile(
            position = ggplot2::position_nudge(x = (binSize/2)/1000)
            ) +
         ggplot2::scale_y_discrete(
            limits = rev(
               levels(heatmap_data %>% dplyr::pull(!!fractionColname))
            )
         ) +
         ggplot2::scale_x_continuous(
            breaks = scales::pretty_breaks()
         ) +
         ggplot2::labs(
            x = "Mass Bin (kDa)",
            y = "Fraction"
         ) +
         ggplot2::theme_minimal() +
         ggplot2::theme(
            text = ggplot2::element_text(size=18, family = fontFamily),
            panel.grid.major = ggplot2::element_line(color = "gray"),
            panel.grid.minor = ggplot2::element_line(color = "lightgray")
         )

      # Add axisRange to plot

      if (is.null(axisRange) == FALSE) {

         heatmap <-
            heatmap +
            ggplot2::scale_x_continuous(
               breaks = scales::pretty_breaks(),
               limits = axisRange,
               expand = c(0,0)
            )
      }

      }

      # Make vertical plot

      if (orientation == "v") {

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
               text = ggplot2::element_text(size=18, family = fontFamily),
               panel.grid.major = ggplot2::element_line(color = "gray"),
               panel.grid.minor = ggplot2::element_line(color = "lightgray")
            )

         # Add axisRange to plot

         if (is.null(axisRange) == FALSE) {

            heatmap <-
               heatmap +
               ggplot2::scale_y_continuous(
                  breaks = scales::pretty_breaks(),
                  limits = axisRange,
                  expand = c(0,0)
               )
         }

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
            file = glue::glue("{outputDir}/{outputPrefix}_{plotType}_heatmap.pdf"),
            width = 8,
            height = 5,
            bg = "transparent"
         )
         print(heatmap)
         dev.off()

      }

      return(heatmap)
   }
