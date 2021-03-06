% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jk_variance_4_continuous.R
\name{jk_variance_4_continuous}
\alias{jk_variance_4_continuous}
\title{calculate Jackknife variance for a continuous variable}
\usage{
jk_variance_4_continuous(df, total = TRUE)
}
\arguments{
\item{df}{a dataframe, which contains a continuous variable, final_wgt and replicate weights--must be in this order}

\item{total}{a logical variable}
}
\description{
It returns estimated total and corresponding variance if total is TRUE; otherwise returns estimated mean and corresponding variance
}
\author{
Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
}
