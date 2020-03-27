
#' make_protein_heatmap
#'
#' @param df
#' @param outputDir
#' @param binSize
#' @param savePDF
#' @param pdfName
#'
#' @return
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples\

make_protein_heatmap <-
   function(
      df = NULL,
      outputDir = getwd(),
      binSize = 1000,   # SPECIFY SIZE for heatmap mass bins
      savePDF = FALSE,
      pdfName = format(Sys.time(), "%Y%m%d_%H%M%S")
   ) {

      # Assertions --------------------------------------------------------------

      assertthat::assert_that(
         is.data.frame(df),
         msg = "df is not a recognized dataframe"
      )

      assertthat::assert_that(
         assertthat::has_name(df, c("ObservedPrecursorMass", "fraction")),
         msg = "df missing a needed column (ObservedPrecursorMass or fraction)"
      )

      assertthat::assert_that(
         assertthat::is.dir(outputdir),
         msg = "outputdir is not a recognized path"
      )

      assertthat::assert_that(
         assertthat::is.flag(savePDF),
         msg = "savePDF should be TRUE or FALSE"
      )

      assertthat::assert_that(
         assertthat::is.string(pdfName),
         msg = "pdfName is not a string"
      )

      # Analyze data ------------------------------------------------------------

      bins <-  seq.int(0, 500000, by = binSize)

      protein_heatmap_data <-
         df %>%
         dplyr::select(c("ObservedPrecursorMass", "fraction")) %>%
         dplyr::mutate(mass_bin = cut(ObservedPrecursorMass, breaks = bins, labels = FALSE)) %>%
         dplyr::group_by(fraction, mass_bin) %>%
         dplyr::summarize(dplyr::n()) %>%
         dplyr::mutate(mass_bin =  (mass_bin * binSize) - binSize) %>%
         dplyr::rename("Protein\nCount" = `dplyr::n()`) %>%
         dplyr::ungroup() %>%
         dplyr::mutate(fraction = forcats::as_factor(fraction))

      protein_heatmap <-
         protein_heatmap_data %>%
         ggplot2::ggplot(
            ggplot2::aes(
               mass_bin/1000, fraction,
               fill = `Protein\nCount`
            )
         ) +
         ggplot2::geom_tile() +
         ggplot2::scale_fill_viridis_c(option = "C", direction = -1) +
         ggplot2::xlim(
            0,
            plyr::round_any(
               max(
                  protein_heatmap_data$mass_bin/1000),
               10,
               f = ceiling
            )
         ) +
         ggplot2::scale_y_discrete(limits = rev(levels(protein_heatmap_data$fraction))) +
         ggplot2::labs(
            x = "Mass Bin (kDa)",
            y = "Fraction"
         ) +
         ggplot2::theme_minimal() +
         ggplot2::theme(text = ggplot2::element_text(size=18))


      # Save protein heatmaps ---------------------------------------------------

      # heatmapname <-
      #    glue::glue("{outputdir}/png/{basename(names(proteinlist)[i])}_protein_heatmap.png")

      # ggplot2::ggsave(
      #    filename = heatmapname,
      #    plot = protein_heatmap,
      #    device = "png",
      #    height = 5.4,
      #    width = 9.6,
      #    dpi = 600
      # )

      if (savePDF == TRUE) {

         pdf(
            file = glue::glue("{outputDir}/{pdfName}_protein_heatmap.pdf"),
            width = 8,
            height = 5,
            bg = "transparent"
         )
         print(protein_heatmap)
         dev.off()

      }

      return(protein_heatmap)
   }
