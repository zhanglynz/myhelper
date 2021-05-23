#' pps action
#'
#' Return probabilities proportional to size
#' @param size_vec a vector of size
#' @param the_n sample size n -- the sum of the resulted probabilities
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' a_vec <- c(6, 8, 1, 2, 3, 1, 1, 1)
#' (y <- pps_action(size_vec = a_vec, the_n = 4))
#' (sum(y))

pps_action <- function(size_vec, the_n)
{the_re <- the_n * (size_vec / sum(size_vec))
 the_n_fixed <- the_n
 while(1) {
   bad_ones_index <- which(the_re > 1)
   good_ones_index <- which(the_re < 1)
   if(!length(bad_ones_index)) return(the_re)
   the_re[bad_ones_index] <- 1
   m <- sum(the_re == 1)
   the_n <- the_n_fixed - m
   the_re[good_ones_index] <-
     the_n * (size_vec[good_ones_index] / sum(size_vec[good_ones_index]))
 }
}
