#' cumulative standard deviation
#'
#' return cumulative standard deviation of a vector
#' @param a_vector
#' @keywords standard deviation
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' x <- 1:10
#' (y <- z_cumu_sd(a_vector = x, divisor = "n_minus_1"))
#'

z_cumu_sd <- function(a_vector, divisor = "n_minus_1")
{n <- length(a_vector)
 if(n == 1) return(0)

first_moment <- cumsum(a_vector) / (1:n)
second_moment <- cumsum(a_vector^2) / (1:n)

cum_var <- second_moment - first_moment^2

if(divisor == "n_minus_1") cum_var[2:n] <- (2:n) / ((2:n) - 1) * cum_var[2:n]

return(sqrt(cum_var))

}
