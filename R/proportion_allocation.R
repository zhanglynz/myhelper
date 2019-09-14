#' Proportional allocation
#'
#' This is to return proportionally allocated sample sizes
#' @param n sample size
#' @param a_vec a vector, which contains sizes of strata
#' @keywords proportional allocation
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' a <- c(100, 200, 300, 200, 400, 500)
#' (b <- proportional_allocation(200, a))
#' (sum(b))
#'
proportional_allocation <- function(n, a_vec)
{prop <- a_vec / sum(a_vec)
 allocation <- n * prop
 the_result <- controlled_rounding_off(allocation)
 return(the_result)
}


