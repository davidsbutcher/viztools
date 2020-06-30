#' Make a waffle plot
#'
#' @description
#' Make a waffle plot from a data frame containing columns for
#' fraction and subcellular localization.
#'
#' @param df A data frame containing columns for fraction and subcellular localization counts.
#' @param fraction_colname Name of data frame column containing fractions. Defaults to 'fraction'.
#' @param waffleType Type of Waffle plot to make. This only affects the axis
#' titles and filename. Typical values are "Protein" or "Proteoform". Defaults to "Protein".
#'
#' @return
#' A waffle plot (ggplot object)
#'
#' @export
#'
#' @examples
#'
#' df <-
#'    data.frame(
#'       "fraction" =
#'          c(1,2,3,4,5,6),
#'       "Cytosol" =
#'          c(20,25,30,30,20,10),
#'       "Membrane" =
#'          c(15,10,20,25,10,5)
#'    )
#'
#'    waffle_iron(
#'       df
#'    )

waffle_iron <-
   function(
      df,
      fraction_colname = "fraction",
      waffleType = "Protein"
   ) {

      # Assertions --------------------------------------------------------------

      assertthat::assert_that(
         is.data.frame(df),
         msg = "df is not a recognized dataframe"
      )

      assertthat::assert_that(
         assertthat::has_name(
            df, c(fraction_colname)
         ),
         msg = "df is missing fraction column"
      )

      assertthat::assert_that(
         assertthat::is.string(waffleType),
         msg = "waffleType is not a string"
      )


      # Create quosures for tidy evaluation -------------------------------------

      fraction_colname_enq <- rlang::enquo(fraction_colname)
      fraction_colname_sym <- rlang::sym(fraction_colname)

      # Create waffle input -----------------------------------------------------


      waffledata <-
         df %>%
         tidyr::pivot_longer(
            cols = -!!fraction_colname_enq,
            names_to = "localization",
            values_to = "count"
         ) %>%
         dplyr::mutate(
            !!fraction_colname_enq := forcats::as_factor(!!fraction_colname_sym),
         ) %>%
         dplyr::filter(
            is.na(count) == FALSE
         )



      # Make waffle plot --------------------------------------------------------

      output_waffle <-
         waffledata %>%
         ggplot2::ggplot(
            ggplot2::aes(fill = localization, values = count)
         ) +
         waffle::geom_waffle(
            color = "white", size = 0.35, n_rows = 10, flip = TRUE
         ) +
         ggplot2::facet_wrap(~fraction, nrow = 1, strip.position = "bottom") +
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

      # Return waffle plot -----------------------------------------------------

      return(output_waffle)

   }
