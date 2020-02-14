#' A checkVariableNames Function
#'
#' This function checks if the given parameters' names and the columns' names of the data are the same.
#' @param raw Data in data.frame format.
#' @param parameterName Factor or characters vector of parameters' names.
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @keywords check variable name
#' @export
#' @examples
#' data(iris)
#' checkVariableNames(iris,c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species"))
checkVariableNames = function(raw, parameterName)
{
  if(all(colnames(raw) == make.names(parameterName)))
  {
    message("The columns' names match the given parameters.")
  }
  else
  {
    message("The following parameters do not match the columns' name:",
        paste(colnames(raw)[!colnames(raw) == make.names(parameterName)]))
  }
}
