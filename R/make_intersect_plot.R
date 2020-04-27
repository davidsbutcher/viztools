#' Make an intersect plot for a set of protein/proteoform IDs
#'
#' @param IDlist
#' @param shortNames
#' @param mediumUsed
#' @param fracMethod
#' @param Yrange
#' @param plotType
#'
#' @return
#' @export
#'
#' @examples

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
