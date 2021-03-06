% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mode.R
\name{mode}
\alias{mode}
\title{A mode Function}
\usage{
mode(x, method = "one", na.rm = FALSE)
}
\arguments{
\item{x}{data in vector}

\item{method}{method use to calculate mode. "one" and "mode" returns only one mode, return NA if there are more than
one modes. "n", "nmodes" returns the number of modes. "all", "modes" return NA if no modes exist, else return all modes.}

\item{na.rm}{FALSE if missing data is to be used in analysis}
}
\value{
Return mode of the data
}
\description{
This function identify mode of the given data set.
}
\examples{
data(iris)
mode(iris[, 5])
}
\author{
Saisakul Chernbumroong, Henry  Nanji
}
\keyword{mode}
