% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pps_action.R
\name{pps_action}
\alias{pps_action}
\title{pps action}
\usage{
pps_action(size_vec, the_n)
}
\arguments{
\item{size_vec}{a vector of size}

\item{the_n}{sample size n -- the sum of the resulted probabilities}
}
\description{
Return probabilities proportional to size
}
\examples{
a_vec <- c(6, 8, 1, 2, 3, 1, 1, 1)
(y <- pps_action(size_vec = a_vec, the_n = 4))
(sum(y))
}
\author{
Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
}
