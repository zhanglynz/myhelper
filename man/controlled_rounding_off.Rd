% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/controlled_rounding_off.R
\name{controlled_rounding_off}
\alias{controlled_rounding_off}
\title{Controlled rounding off numbers}
\usage{
controlled_rounding_off(n = NULL, a_vector)
}
\arguments{
\item{n}{the total}

\item{a_vector}{a vector of non-negative numbers}
}
\description{
Return controlled rounding off numbers
}
\examples{
x <- rnorm(10, mean = 10, sd = 1)
y <- controlled_rounding_off(a_vector = x)
(c(sum(x), sum(y)))
(z <- cbind(x, y))
}
\author{
Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
}
