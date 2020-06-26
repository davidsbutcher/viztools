#' Make a waffle plot
#'
#' @description
#' Make a waffle plot from a data frame containing columns for short name,
#' fraction, and subcellular localizations.
#'
#' @param df A data frame containing columns for shortname, fraction, and localization counts columns.
#' @param shortName Shortname to use for making waffle plot.
#' @param savePDF Boolean value, controls whether to save PDF output to outputDir. Defaults to FALSE.
#' @param outputDir Directory to save PDF output. Defaults to R working directory.
#' @param shortname_colname Name of data frame column containing shortnames. Defaults to 'Short name'.
#' @param fraction_colname Name of data frame column containing fractions. Defaults to 'fraction'.
#' @param loc_colnames Vector of names of data frame columns containing counts of
#' cytosolic, membrane, periplasmic, and none-of-the-above/unannotated proteins. Defaults to
#' c("Cytosolic Proteins", "Membrane Proteins", "Periplasmic Proteins", "NOTA Proteins").
#' @param waffleType Type of Waffle plot to make. This only affects the axis
#' titles and filename. Typical values are "Protein" or "Proteoform". Defaults to "Protein".
#'
#' @return
#' @export
#'
#' @examples

waffle_iron <-
   function(
      df,
      shortName = NULL,
      savePDF = FALSE,
      outputDir = getwd(),
      shortname_colname = "Short name",
      fraction_colname = "fraction",
      loc_colnames = c(
         "Cytosolic Proteins",
         "Membrane Proteins",
         "Periplasmic Proteins",
         "NOTA Proteins"
      ),
      waffleType = "Protein"
   ) {

      # Assertions --------------------------------------------------------------

      assertthat::assert_that(
         is.data.frame(df),
         msg = "df is not a recognized dataframe"
      )

      assertthat::assert_that(
         assertthat::has_name(
            df, c(shortname_colname, fraction_colname, loc_colnames)
         ),
         msg = "df is missing a needed column"
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
         assertthat::is.string(waffleType),
         msg = "waffleType is not a string"
      )

      # Break out localization column names -------------------------------------

      cyt_colname <- loc_colnames[[1]]

      mem_colname <- loc_colnames[[2]]

      peri_colname <- loc_colnames[[3]]

      nota_colname <- loc_colnames[[4]]

      # Create quosures for tidy evaluation -------------------------------------

      shortname_colname_enq <- rlang::enquo(shortname_colname)
      shortname_colname_sym <- rlang::sym(shortname_colname)

      fraction_colname_enq <- rlang::enquo(fraction_colname)

      cyt_colname_enq <- rlang::enquo(cyt_colname)
      mem_colname_enq <- rlang::enquo(mem_colname)
      peri_colname_enq <- rlang::enquo(peri_colname)
      nota_colname_enq <- rlang::enquo(nota_colname)


      # Create waffle input -----------------------------------------------------

      df <-
         df %>%
         dplyr::filter(!!shortname_colname_sym == shortName)

      fractionlist <-
         df %>%
         dplyr::pull(!!fraction_colname_enq)

      waffledata <-
         tibble::tibble(
            Localization = factor(
               c(
                  rep("Cytosolic", length(fractionlist)),
                  rep("Membrane", length(fractionlist)),
                  rep("Periplasmic", length(fractionlist)),
                  rep("Unannotated", length(fractionlist))
               )
            ),
            Count = c(
               dplyr::pull(df, !!cyt_colname_enq),
               dplyr::pull(df, !!mem_colname_enq),
               dplyr::pull(df, !!peri_colname_enq),
               dplyr::pull(df, !!nota_colname_enq)
            ),
            Fraction = rep(fractionlist, 4)
         ) %>%
         dplyr::group_by(Fraction, Localization) %>%
         dplyr::summarize(Count)


      # Make waffle plot --------------------------------------------------------

      output_waffle <-
         waffledata %>%
         ggplot2::ggplot(ggplot2::aes(fill = Localization, values = Count)) +
         waffle::geom_waffle(color = "white", size = 0.35, n_rows = 10, flip = TRUE) +
         ggplot2::facet_wrap(~Fraction, nrow = 1, strip.position = "bottom") +
         ggplot2::scale_x_discrete(expand=c(0,0)) +
         ggplot2::scale_y_continuous(
            labels = function(x) x * 10, # this multiplier must be equal to n_rows above
            expand = c(0,0)
         ) +
         ggplot2::scale_fill_viridis_d(option = "plasma", begin = 0, end = 0.85) +
         ggplot2::coord_equal() +
         ggplot2::labs(
            x = "Fraction Number",
            y = glue::glue("{waffleType} Count")
         ) +
         ggplot2::theme_minimal() +
         ggplot2::theme(
            panel.grid =
               ggplot2::element_blank(), axis.ticks.y = ggplot2::element_line()
         ) +
         ggplot2::guides(
            fill = ggplot2::guide_legend("Localization", reverse = TRUE)
         )

      # Save waffle plot --------------------------------------------------------

      if (savePDF == TRUE) {

         if (dir.exists(outputDir) == FALSE) {
            dir.create(outputDir)
         }

         pdf(
            file = glue::glue("{outputDir}/{shortName}_{waffleType}_waffle_plot.pdf"),
            width = 8,
            height = 5,
            bg = "transparent"
         )
         print(output_waffle)
         dev.off()

      }

      return(output_waffle)

   }
