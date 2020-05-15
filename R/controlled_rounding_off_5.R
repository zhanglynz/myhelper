#' Controlled rounding off numbers
#'
#' Return controlled rounding off numbers
#' @param a_vector a vector of positive/negative numbers
#' @keywords rounding off control
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' x <- rnorm(10, mean = 10, sd = 10)
#' y <- controlled_rounding_off_5(a_vector = x)
#' (c(sum(x), sum(y)))
#' (z <- cbind(x, y))


controlled_rounding_off_5 <- function(a_vector)
{n <- length(a_vector)
 pos_position <- which(a_vector >= 0)

 L <- length(pos_position)

 # case 1
 if(L == 0) return(-controlled_rounding_off_3(abs(a_vector)))
 # case 2
 if(L == n) return(controlled_rounding_off_3(a_vector))

 # case 3
 neg_position <- setdiff(1:n, pos_position)

 pos_vec <- a_vector[pos_position]
 pos_rounding <- controlled_rounding_off_3(pos_vec)

 neg_vec <- a_vector[neg_position]
 neg_rounding <- -controlled_rounding_off_3(abs(neg_vec))

 re <- rep(0, n)
 re[pos_position] <- pos_rounding
 re[neg_position] <- neg_rounding

 return(re)
}
