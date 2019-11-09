#' Sampling with unequal selection probabilities
#'
#' This is a wraper of sampling:UPbrewer
#' @param a_pop a population
#' @param relative_prob_wgt relative probability weights
#' @param ssize sample size
#' @keywords unequal selection probabilities
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' popu <- letters[1:3]
#' relative_prob <- c(0.5, 0.3, 0.1)
#' sample_uneq_prob(a_pop = popu, relative_prob_wgt = relative_prob, ssize = 2)

sample_uneq_prob <- function(a_pop, relative_prob_wgt, ssize) {
# a helper function
  selection_prob <- function(x = relative_prob_wgt, n = ssize) {
    x <- n * (x / sum(x))
    if (any(x > 1.0)) {
      x_ <- ifelse(x > 1, 1, x)
      selection_prob(n * x_ / sum(x_), n)
    } else x
  }

  pi <- selection_prob(x = relative_prob_wgt, n = ssize)
  a_pop[sampling::UPbrewer(pik = pi) == 1]
}
