#' Wright allocation
#'
#' Return Wright allocation
#' @param n the total sample size
#' @param Nh size of stratum h
#' @param Sh standard deviation of y in stratum h
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' n <- 10
#' Nh <- c(47, 61, 41)
#' Sh <- c(10, 6, 4)
#' (the_results <- wright_allocation(n, Nh, Sh))
#'
#' @references Wright, T. (2014). A simple method of exact optimal sample allocation under stratification with any mixed contraint patterns

wright_allocation <- function(n, Nh, Sh)
{N_S <- Nh * Sh

 row_nbr <- length(Nh)
 col_nbr <- n - row_nbr

 a_matrix <- matrix(0, row_nbr, col_nbr)
 for(i in 1:col_nbr) a_matrix[, i] <- 1 / sqrt(i * (i + 1))
 a_matrix <- N_S * a_matrix

 b <- sort(as.vector(a_matrix))
 m <- row_nbr * col_nbr
 the_top <- b[-(1:(m-col_nbr))]

 the_re <- rep(0, row_nbr)
 for(i in 1:row_nbr)
 {the_re[i] <- sum(a_matrix[i, ] %in% the_top)
 }

 the_re <- the_re + 1
 return(the_re)
}
