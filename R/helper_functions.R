
#' KillNA
#'
#' @param list_in Any list object
#'
#' @return
#' Input list with NA elements removed


killNA <- function(list_in) {

   list_out <- list()

   for (i in seq_along(list_in)) {

      list_out[[i]] <- list_in[[i]][!is.na(list_in[[i]])]

   }

   names(list_out) <- names(list_in)

   return(list_out)
}

#' add_fraction
#'
#' @description
#' Parses filenames in a dataframe to get the fraction number.
#'
#' @param df A data frame containing a column corresponding to MS data file names.
#' @param filename_colname The name of the column containing the data file names.
#'
#' @importFrom magrittr %>%
#'
#' @return
#' A data frame containing a new column corresponding to fraction names.


add_fraction <- function(
   df,
   filename_colname = filename
   ) {

   filename_colname <- rlang::enquo(filename_colname)

   # This function attempts to parse the filenames to extract information
   # about the fraction that a raw file corresponds to. This is only useful
   # for GELFrEE/PEPPI/other fractionated data

   message("\nAdding fraction numbers by parsing filenames")

   df %>%
      dplyr::mutate(
         fraction = dplyr::case_when(
            stringr::str_detect(!!filename_colname,
                                "(?i)(?<=gf|gf_|peppi|peppi_|frac|fraction|f|f_)[0-9]{1,2}") == TRUE ~
               stringr::str_extract(!!filename_colname,
                                    "(?i)(?<=gf|gf_|peppi|peppi_|frac|fraction|f|f_)[0-9]{1,2}"),
            TRUE ~ "NA"
         )
      )

}
