#' Controlled rounding off numbers
#'
#' Return controlled rounding off numbers
#' @param a_vector a vector of non-negative numbers
#' @keywords rounding off control
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' x <- rnorm(10, mean = 10, sd = 1)
#' y <- controlled_rounding_off(x)
#' (c(sum(x), sum(y)))
#' (z <- cbind(x, y))

controlled_rounding_off <- function(a_vector)
{total <- round(sum(a_vector), 0)
 a_df <-
   data.frame(x = a_vector) %>%
   dplyr::mutate(ID = row_number()) %>%
   dplyr::mutate(y = trunc(x)) %>%
   dplyr::mutate(z = x - y) %>%
   dplyr::arrange(desc(z)) %>%
   dplyr::mutate(new_ID = row_number())

 the_diff <- total - sum(a_df$y)

 b_df <-
   a_df %>%
   dplyr::mutate(one_or_zero = ifelse(new_ID <= the_diff, 1, 0)) %>%
   dplyr::mutate(result = y + one_or_zero) %>%
   dplyr::arrange(ID)

 return(b_df$result)
}
