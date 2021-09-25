#' Controlled rounding off numbers
#'
#' Return controlled rounding off numbers
#' @param n the total
#' @param a_vector a vector of non-negative numbers
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' x <- rnorm(10, mean = 10, sd = 1)
#' y <- controlled_rounding_off(a_vector = x)
#' (c(sum(x), sum(y)))
#' (z <- cbind(x, y))

controlled_rounding_off <- function(n = NULL, a_vector)
{if(is.null(n)) {
  total <- round(sum(a_vector), 0) } else {
  total <- n }

 a_df <-
   data.frame(x = a_vector) %>%
   dplyr::mutate(ID = dplyr::row_number()) %>%
   dplyr::mutate(y = trunc(x)) %>%
   dplyr::mutate(z = x - y) %>%
   dplyr::arrange(dplyr::desc(z)) %>%
   dplyr::mutate(new_ID = dplyr::row_number())

 the_diff <- total - sum(a_df$y)

 b_df <-
   a_df %>%
   dplyr::mutate(one_or_zero = ifelse(new_ID <= the_diff, 1, 0)) %>%
   dplyr::mutate(result = y + one_or_zero) %>%
   dplyr::arrange(ID)

 return(b_df$result)
}
