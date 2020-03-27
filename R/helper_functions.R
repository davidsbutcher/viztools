
#' KillNA
#'
#' @param list_in Any list object
#'
#' @return
#' Input list with NA elements removed
#'
#' @examples

killNA <- function(list_in) {

   list_out <- list()

   for (i in seq_along(list_in)) {

      list_out[[i]] <- list_in[[i]][!is.na(list_in[[i]])]

   }

   names(list_out) <- names(list_in)

   return(list_out)
}
