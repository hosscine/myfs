% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculation.R
\name{rowTimes}
\alias{rowTimes}
\title{Calculates \code{X \* a} with row first.}
\usage{
rowTimes(X, a)
}
\arguments{
\item{X}{timed matrix.}

\item{a}{times vector.}
}
\value{
\code{X \* a} with row first.
}
\description{
R's default calculation of \code{X \* a} is with col first likes \code{X[,1] \* a}, \code{X[,2] \* a}, ....
This function implements calclation with row first likes \code{X[1,] \* a}, \code{X[1,] \* a}, ....
}
