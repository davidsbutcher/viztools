#' Make an UpSet plot from a list of unique identifiers
#'
#' @description
#' make_UpSet_plot() creates an UpSet plot from a list of unique identifiers such as
#' UniProt accession numbers or proteoform record numbers.
#'
#' @param UpSetlist A list of lists of identifiers properly formatted for use by the
#' UpSetR::upset() function. Alternatively, a data frame with columns named by
#' fraction number and containing unique identifiers for proteins/proteoforms.
#' @param plotType Type of UpSet plot to make. This only affects the axis
#' titles and filename. Typical values are "Protein" or "Proteoform". Defaults to "Protein".
#' @param savePDF Boolean value, controls whether to save PDF output to outputDir. Defaults to FALSE.
#' @param outputDir Directory to save PDF output. Defaults to R working directory.
#'
#' @return
#' An UpSet plot created by UpSetR::upset(). If savePDF is TRUE, a PDF is saved
#' to the output directory.
#'
#' @examples
#' make_UpSet_plot(
#'    list(
#'       list(
#'          "Fraction1" = c("A", "B", "C", "D", "E"),
#'          "Fraction2" = c("C", "D", "E", "F", "G")
#'       )
#'    )
#' )
#'
#' @importFrom magrittr %>%
#'
#' @export

make_UpSet_plot <-
   function(
      UpSetlist,
      plotType = "Protein",
      savePDF = FALSE,
      outputDir = getwd()
   ) {


      # Assertions --------------------------------------------------------------

      assertthat::assert_that(
         is.list(UpSetlist) | is.data.frame(UpSetlist),
         msg = "UpSetlist is not a list or data frame object"
      )

      assertthat::assert_that(
         assertthat::is.string(plotType),
         msg = "plotType is not a string"
      )

      assertthat::assert_that(
         assertthat::is.flag(savePDF),
         msg = "savePDF should be TRUE or FALSE"
      )

      assertthat::assert_that(
         assertthat::is.dir(dirname(outputDir)),
         msg = "outputDir parent directory is not a recognized path"
      )


      # Reshape dataframe -------------------------------------------------------

      # If UpSetlist is supplied as a data.frame, reshape it to a list

      if (is.data.frame(UpSetlist) == TRUE) {

         UpSetlist <-
            list(
               purrr::map(
                  as.list(UpSetlist),
                  ~.x[!is.na(.x)]
               )
            ) %>%
            rlang::set_names(1:length(.))

      }

      # Make plots --------------------------------------------------------------

      UpSet <-
         UpSetlist %>%
         purrr::map(
            function(list)
            {
               UpSetR::upset(
                  UpSetR::fromList(list),
                  sets = rev(names(list)),
                  nintersects = NA,
                  sets.x.label = glue::glue("Total {plotType} IDs"),
                  keep.order = T,
                  mainbar.y.label =
                     glue::glue("Unique {plotType} IDs in Intersection"),
                  text.scale =  c(1.5, 1.2, 1.5, 1.5, 1.2, 0.9),
                  point.size = 2,
                  line.size = 0.75,
                  group.by = "degree"
               )
            }
         )

      if (savePDF == TRUE) {

         if (dir.exists(outputDir) == FALSE) {
            dir.create(outputDir)
         }

         message(glue::glue("\nSaving {plotType} UpSet plots to outputDir"))
         purrr::map2(
            names(UpSetlist),
            UpSet,
            ~{
               pdf(
                  file = glue::glue("{outputDir}/{.x}_UpSet_{plotType}.pdf"),
                  width = 8,
                  height = 5,
                  bg = "transparent",
                  useDingbats = FALSE
               )
               print(.y)
               dev.off()
            }
         )

      }

      return(UpSet)
   }


