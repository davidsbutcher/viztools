#' Make an intersection degree plot from a set of unique identifiers
#'
#' @description
#'
#' @param IDlist A list of vectors. Each list item should be a vector of unique
#' identifiers corresponding to protein or proteoform IDs, e.g. UniProt accession
#' numbers or proteoform record numbers from the Consortium for Top-Down Proteomics
#' proteoform atlas.
#' @param Yrange
#' Percentage range to use for the Y-axis of the plot. Defaults to `c(0,100)`.
#' @param plotType
#' Type of plot to create, typically "Protein" or "Proteoform".This only
#' affects the Y-axis title and can be any string.
#' @param fillColor
#' Fill color to use for bars on plot. Should be specified as a Hex code.
#' Defaults to MagLab violet (#4C4184).
#'
#' @return
#' An intersection degree plot (a ggplot object).
#'
#' @export
#'
#' @examples
#'

make_intersection_degree_plot <-
   function(
      IDlist,
      Yrange = c(0,100),
      plotType = "Protein",
      fillColor = "#4C4184"
   ) {


      # Assertions --------------------------------------------------------------

      assertthat::assert_that(
         is.list(IDlist),
         msg = "IDlist is not a list object"
      )

      if (is.null(Yrange) == FALSE) {

         assertthat::assert_that(
            is.numeric(Yrange),
            msg = "Yrange is not numeric"
         )

         assertthat::assert_that(
            length(Yrange) == 2,
            msg = "Yrange should have a length of 2"
         )

      }

      assertthat::assert_that(
         assertthat::is.string(plotType),
         msg = "plotType is not a string"
      )

      assertthat::assert_that(
         assertthat::is.string(fillColor),
         msg = "fillColor is not a string"
      )


      # ggplot Themes -----------------------------------------------------------

      column_plot_01 <-
         list(
            ggplot2::theme_minimal(),
            ggplot2::theme(
               panel.background = ggplot2::element_blank(),
               panel.grid = ggplot2::element_blank(),
               axis.line = ggplot2::element_line(color = "black"),
               axis.ticks = ggplot2::element_line(color = "black"),
               axis.text = ggplot2::element_text(
                  color = "black"
               ),
               text = ggplot2::element_text(
                  size = 16,
                  face = "bold",
                  color = "black"
               )
            )
         )

      # Reshape data ------------------------------------------------------------

      counts <-
         IDlist %>%
         unlist() %>%
         unname() %>%
         table() %>%
         tibble::enframe(name = "accession", value = "int_degree") %>%
         dplyr::group_by(int_degree) %>%
         dplyr::summarize(count = dplyr::n()) %>%
         dplyr::mutate(count_frac = count/sum(count))

      ## Column plots - average of fractionation methods

      int_deg_plot <-
         counts %>%
         ggplot2::ggplot(
            ggplot2::aes(int_degree, count_frac*100, fill = fillColor)
         ) +
         ggplot2::geom_col(position = "dodge") +
         ggplot2::geom_text(
            ggplot2::aes(
               int_degree,
               count_frac*100,
               label = format(count_frac*100, digits = 1, nsmall = 1)
            ),
            nudge_y = max(counts$count_frac*5)
         ) +
         ggplot2::scale_x_continuous(
            breaks =
               scales::breaks_pretty(n = length(unique(counts$int_degree)))
         ) +
         ggplot2::scale_y_continuous(
            breaks = scales::breaks_pretty(),
            expand = c(0,0),
            limits = Yrange
         ) +
         ggplot2::labs(
            x = "Intersection Degree",
            y = glue::glue("Percentage of {plotType} IDs")
         ) +
         ggplot2::guides(
            color = "none",
            fill = "none"
         ) +
         column_plot_01


      return(int_deg_plot)

   }
