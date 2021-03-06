% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{fmt_dcimals}
\alias{fmt_dcimals}
\title{A function to control decimal precision for ggplot axis labels}
\usage{
fmt_dcimals(decimals = 0)
}
\arguments{
\item{decimals}{Desired number of decimals on plot axis.}
}
\value{
A vector  of correctly formated labels when used as an argument for the \code{labels}-argument
of a ggplot scale function.
}
\description{
This function was originally posted here \link{https://stackoverflow.com/questions/10035786/ggplot2-y-axis-label-decimal-precision#10036038}
}
