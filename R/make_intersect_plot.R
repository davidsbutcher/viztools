#' Make an intersection degree plot for a set of unique identifiers
#'
#'
#' @param IDlist A list of vectors. Each list item should be a vector of unique
#' identifiers corresponding to protein or proteoform IDs, e.g. UniProt accession
#' numbers or proteoform record numbers from the Consortium for Top-Down Proteomics
#' proteoform atlas.
#' @param shortNames Vector of short names
#' @param mediumUsed Vector of media used
#' @param fracMethod Vector of frac methods
#' @param Yrange
#' Percentage range to use for the Y-axis of the plot. Defaults to `c(0,100)`.
#' @param plotType
#' Type of plot to create, typically "Protein" or "Proteoform".This only
#' affects the Y-axis title and can be any string.
#'
#' @return
#'

make_intersect_plot <-
   function(
      IDlist,
      shortNames,
      mediumUsed,
      fracMethod,
      Yrange = c(0,100),
      plotType = "Protein"
   ) {


      # Assertions --------------------------------------------------------------





      # ggplot Themes -----------------------------------------------------------

      column_plot_01 <-
         list(
            ggplot2::scale_fill_viridis_d(
               option = "plasma",
               begin = 0.15,
               end = 0.85),
            ggplot2::scale_color_viridis_d(
               option = "plasma",
               begin = 0.15,
               end = 0.85),
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
         purrr::pmap(
            list(
               IDlist,
               shortNames %>% as.list(),
               mediumUsed %>% as.list(),
               fracMethod %>% as.list()
            ),
            ~{
               unlist(..1) %>%
                  unname() %>%
                  table() %>%
                  tibble::enframe(name = "accession", value = "int_degree") %>%
                  dplyr::group_by(int_degree) %>%
                  dplyr::summarize(count = dplyr::n()) %>%
                  dplyr::mutate(shortname = ..2) %>%
                  dplyr::mutate(medium = ..3) %>%
                  dplyr::mutate(frac_method = ..4) %>%
                  dplyr::select(shortname, dplyr::everything()) %>%
                  dplyr::mutate(count_frac = count/sum(count))
            }
         ) %>%
         purrr::reduce(dplyr::union_all) %>%
         dplyr::ungroup()


      counts_sum <-
         counts %>%
         dplyr::group_by(int_degree, frac_method, medium) %>%
         dplyr::summarize(
            ave_frac = mean(count_frac),
            stdev_frac = sd(count_frac)
         )

      # Make plot ---------------------------------------------------------------

      ## Column plots - average of fractionation methods

      int_deg_plot <-
         counts_sum %>%
         ggplot2::ggplot(
            ggplot2::aes(int_degree, ave_frac*100, fill = frac_method)
         ) +
         ggplot2::geom_col(position = "dodge") +
         ggplot2::geom_text(
            ggplot2::aes(
               int_degree,
               ave_frac*100,
               label = format(ave_frac*100, digits = 1, nsmall = 1),
               color = frac_method
            ),
            nudge_y = max(counts_sum$ave_frac*3)
         ) +
         ggplot2::geom_errorbar(
            ggplot2::aes(
               ymin = (ave_frac*100 - (stdev_frac/2)*100),
               ymax = (ave_frac*100 + (stdev_frac/2)*100)
            ),
            width = 0.3, position = ggplot2::position_dodge(width = 0.9)
         ) +
         ggplot2::scale_x_continuous(
            breaks =
               scales::breaks_pretty(n = length(unique(counts_sum$int_degree)))
         ) +
         ggplot2::scale_y_continuous(
            breaks = scales::breaks_pretty(),
            expand = c(0,0),
            limits = Yrange
         ) +
         ggplot2::labs(
            x = "Intersection Degree",
            y = glue::glue("Percentage of {plotType} IDs"),
            fill = "Frac.\n Method"
         ) +
         ggplot2::guides(
            color = "none"
         ) +
         # ggplot2::annotate(
         #   "text", size = 4,
         #   x = max(counts$int_degree - 0.5),
         #   y = max(counts$count_frac*100*0.90),
         #   label = glue::glue(
         #     "GELFrEE: n = {gf_count}\nPEPPI: n = {peppi_count}"
         #   )
         # ) +
         column_plot_01

      if (length(unique(counts_sum$frac_method)) == 1) {

         int_deg_plot <-
            int_deg_plot +
            ggplot2::guides(fill = "none")

      }

      return(int_deg_plot)

   }
