% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system.R
\name{eval_timer}
\alias{eval_timer}
\title{Measure the elapsed time of \code{eval} and returns the result.}
\usage{
eval_timer(eval, index.msg = NULL, keep = F, return = T)
}
\arguments{
\item{eval}{evalated code.}

\item{index.msg}{integer or character to be used as message.}

\item{keep}{if \code{T}, returns evalated object with elapsed time.}

\item{return}{if \code{T}, returns evalated object.}
}
\value{
evalated object.
}
\description{
Measure the elapsed time of \code{eval} and returns the result.
}
