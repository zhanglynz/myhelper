#' Controlled rounding off numbers
#'
#' Return controlled rounding off numbers
#' @param a_vector a vector of non-negative numbers
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' x <- rnorm(10, mean = 10, sd = 1)
#' y <- controlled_rounding_off_3(a_vector = x)
#' (c(sum(x), sum(y)))
#' (z <- cbind(x, y))

controlled_rounding_off_3 <- function(a_vector)
{T <- round(sum(a_vector), 0)

 y <- trunc(a_vector)
 z <- a_vector - y
 the_order <- order(-z)

 the_diff <- T - sum(y)
 if(the_diff >= 1) {
  the_subscript <- the_order[1:the_diff]
  y[the_subscript] <- y[the_subscript] + 1}

 return(y)
}
