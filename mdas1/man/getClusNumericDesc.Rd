% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getClusNumericDesc.R
\name{getClusNumericDesc}
\alias{getClusNumericDesc}
\title{A getClusNumericDesc Function}
\usage{
getClusNumericDesc(x, clusters)
}
\arguments{
\item{x}{a data in data.frame}

\item{clusters}{clusters in factor}
}
\value{
Return a data.table containing distinct characteristic of each cluster.
}
\description{
This function performs statistical tests and generate description of a given clusters for numerical variables.
}
\examples{
data(iris)
h = cluster::agnes(iris[, -5], method = "ward")
getClusNumericDesc(iris[, -5], cutree(h, k=3))
}
\author{
Saisakul Chernbumroong, Henry  Nanji
}
\keyword{analysis}
\keyword{cluster}
