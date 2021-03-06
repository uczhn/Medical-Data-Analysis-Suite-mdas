% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutualInfo.R
\name{mutualInfo}
\alias{mutualInfo}
\title{A mutualInfo Function}
\usage{
mutualInfo(x, y)
}
\arguments{
\item{x}{numerical or factor data.}

\item{y}{numerical or factor data.}
}
\value{
Return mutual information
}
\description{
This function calculates mutual information of the two variables.
}
\examples{
data(iris)
mutualInfo(iris[, 1], iris[, 5])
}
\author{
Saisakul Chernbumroong, Henry  Nanji
}
\keyword{information}
\keyword{mutual}
