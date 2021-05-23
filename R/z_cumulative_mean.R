#' cumulative mean
#'
#' return cumulative mean of a vector
#' @param a_vector a vector of numbers
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' x <- 1:10
#' (y <- z_cumu_mean(a_vector = x))
#'

z_cumu_mean <- function(a_vector)
{n <- length(a_vector)
 if(n == 0) return(NA)
 cumsum(a_vector) / (1:n)
}
