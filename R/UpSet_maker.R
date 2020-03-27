#' UpSet_maker
#'
#' @param UpSetlist
#' @param UpSetType
#'
#' @return
#' @export
#'
#' @examples

UpSet_maker <-
   function(
      UpSetlist,
      UpSetType = "protein",
      savePDF = FALSE,
      outputDir = getwd()
   ) {


      if (UpSetType == "protein") {
         UpSet <-
            UpSetlist %>%
            purrr::map(
               function(proteinlist)
               {
                  UpSetR::upset(
                     UpSetR::fromList(proteinlist),
                     sets = rev(names(proteinlist)),
                     nintersects = NA,
                     sets.x.label = "Total Protein IDs",
                     keep.order = T,
                     mainbar.y.label = "Unique Protein IDs in Intersection",
                     text.scale =  c(1.5, 1.2, 1.5, 1.5, 1.2, 0.9),
                     point.size = 2,
                     line.size = 0.75,
                     group.by = "degree"
                  )
               }
            )
      }

      # UpSet plot, proteoforms by fraction ---------------------------------------------------------

      if (UpSetType == "proteoform") {

         UpSet <-
            UpSetlist %>%
            purrr::map(

               function(proteoformlist)
               {

                  UpSetR::upset(
                     UpSetR::fromList(proteoformlist),
                     sets = rev(names(proteoformlist)),
                     nintersects = NA,
                     sets.x.label = "Total Proteoform IDs",
                     keep.order = T,
                     mainbar.y.label = "Unique Proteoform IDs in Intersection",
                     text.scale =  c(1.5, 1.2, 1.5, 1.5, 1.2, 0.9),
                     point.size = 2,
                     line.size = 0.75,
                     group.by = "degree"
                  )

               }

            )

      }


      # UpSet plots, proteins by run - unfractionated ---------------------------

      if (UpSetType == "protein_unfrac") {

         UpSet <-
            UpSetlist %>%
            purrr::map(

               function(proteinlist)
               {
                  UpSetR::upset(
                     UpSetR::fromList(proteinlist),
                     sets = rev(names(proteinlist)),
                     nintersects = NA,
                     sets.x.label = "Total Protein IDs",
                     keep.order = T,
                     mainbar.y.label = "Unique Protein IDs in Intersection",
                     text.scale =  c(1.5, 1.2, 1.5, 1.5, 1.2, 0.9),
                     point.size = 2,
                     line.size = 0.75,
                     group.by = "degree"
                  )
               }
            )
      }

      if (savePDF == TRUE) {

         message(glue::glue("\nSaving {UpSetType} UpSet plots to outputDir"))
         purrr::map2(
            names(UpSetlist),
            UpSet,
            ~{
               pdf(
                  file = glue::glue("{outputDir}/{.x}_UpSet_{UpSetType}.pdf"),
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


