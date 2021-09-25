#' Controlled rounding off numbers
#'
#' Return controlled rounding off numbers
#' @param a_vector a vector of non-negative numbers
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @importFrom data.table :=
#' @examples
#' x <- rnorm(10, mean = 10, sd = 1)
#' y <- controlled_rounding_off(a_vector = x)
#' (c(sum(x), sum(y)))
#' (z <- cbind(x, y))

controlled_rounding_off_2 <- function(a_vector)
{T <- round(sum(a_vector), 0)
 n <- length(a_vector)
 a_dt <- data.table::data.table(id = 1:n,
                                x = a_vector)
 a_dt <- a_dt[, `:=`(y = trunc(x))][, `:=`(z = x - y)]
 a_dt <- a_dt[order(-z)]
 the_diff <- T - sum(a_dt$y)
 con_vec <- rep(0, n)
 con_vec[1:the_diff] <- 1
 a_dt <- a_dt[, `:=`(the_re = y + con_vec)]
 a_dt <- a_dt[order(id)]

 return(a_dt$the_re)
}
x <- y <- z <- id <- NULL
