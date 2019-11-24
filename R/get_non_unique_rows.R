#' Get non-unique rows in a dataframe
#'
#' Return a dataframe where the the rows are non-unique in terms of a varialbe
#' @param a_df a dataframe
#' @param a_var a variable, for defining row uniqueness
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}

#' @importFrom magrittr %>%

#' @examples
#' fk_data <-
#' data.frame(x = c(rep("a", 2), rep("b", 3), rep("c", 5), letters[11:20]),
#' y = rnorm(20))
#' test_df <- get_non_unique_rows(a_df = fk_data, a_var = "x")
#'

get_non_unique_rows <- function(a_df, a_var)
{a_var <- rlang::sym(a_var)
 b_df <-
   a_df %>%
   dplyr::count(!!a_var) %>%
   dplyr::filter(n == 1) %>%
   dplyr::select(-n)

 re_df <-
   a_df %>%
   dplyr::anti_join(b_df)

 return(re_df)
}

