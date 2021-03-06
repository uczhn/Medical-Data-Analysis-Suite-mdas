% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correctType.R
\name{correctType}
\alias{correctType}
\title{A correctType Function}
\usage{
correctType(raw, datatype, cols = NULL)
}
\arguments{
\item{raw}{data in data.frame format}

\item{datatype}{the data type for each raw data column. Accepted data types are factor, numeric, integer,
date, ordered, and character.}

\item{cols}{if not all columns need correction, specify the column number that need correction.}
}
\value{
Return the data set with the corrected data type.
}
\description{
Often the data read from .csv file are not in correct data type.
This function processes the raw data into the correct data type.
}
\examples{
library(FactoMineR)
data(wine)
class(wine$Class)
#Change data type of wine column 1 into factor
new.wine = correctType(wine, "factor", 1)
class(wine$Class)
}
\author{
Saisakul Chernbumroong, Henry  Nanji
}
\keyword{data}
\keyword{type}
