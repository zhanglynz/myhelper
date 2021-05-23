#' convert a vector of counts into proportions
#'
#' @param x a vector (of counts)
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @export
#' @examples
#' x <- 1:5
#' p <- convert_2_prop(x)

convert_2_prop <- function(x)
{x / sum(x)
}
