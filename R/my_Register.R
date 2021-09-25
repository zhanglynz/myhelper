#' Get all R files under a folder
#'
#' This returns a dataframe, which has all the R-file information under the specified folder
#' @param folder_name name of the folder
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @importFrom magrittr %>%
#' @examples
#' df <- my_Register()


my_Register <- function(folder_name = "R")
{the_folder <- paste0(getwd(), "/", folder_name)
 file_names <- list.files(path = the_folder, pattern = "R$",
                          full.names = FALSE)
 a_df <-
   data.frame(file_name = file_names) %>%
   dplyr::mutate(prefix = sub("_.*", "", file_name)) %>%
   dplyr::mutate(group_letter = substring(prefix, 1, 1)) %>%
   dplyr::arrange(group_letter, prefix)

 return(a_df)
}
file_name <- prefix <- group_letter <- NULL
