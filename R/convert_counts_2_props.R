#' convert a vector of counts into proportions
#'
#' @param x a vector (of counts)
#' @keywords counts; proportions
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
#' @examples
#' x <- 1:5
#' p <- convert_2_prop(x)

convert_2_prop <- function(x)
{x / sum(x)
}
