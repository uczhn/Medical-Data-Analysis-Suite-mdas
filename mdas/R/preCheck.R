#' A preCheck Function
#'
#' This function performs an initial checking for the given data set and data type. The checking process includes data format of dataset
#' and data type. Columns' names for the data type object and variables' names of the data set.
#' @param dataset data in data.frame.
#' @param datatype data.frame object containing information about variable names,	type,	whether the variable
#' should be excluded,	the source of the variable,	and whether the variable is a response/target.
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @keywords data checking
#' @export
#' @examples
#' data(iris)
#' datatype = data.frame("Parameter" = c("Sepal.Length","Sepal.Width","Petal.Length",
#'                       "Petal.Width","Species"),
#'                       "Type" = c("numeric","numeric","numeric","numeric","factor"),
#'                       "Excluded" = c(0,0,0,0,0),
#'                       "Source" = c(NA,NA,NA,NA,NA),
#'                       "Response" = c(0,0,0,0,1))
#' preCheck(iris, datatype)
preCheck = function(dataset, datatype)
{
  if(!all(is.data.frame(dataset), is.data.frame(datatype)))
  {
    stop("The data is not in data.frame format.")
  }

  #check that data type contain parameters' names and types
  #Parameter	Type	Excluded	Source	Response
  if(!all(colnames(datatype[,1:5]) == c("Parameter","Type","Excluded","Source","Response")))
  {
    stop("The column names for data type do not match the requirement. Please
         ensure the data type object has the following columns' names:
         Parameter,	Type,	Excluded,	Source,	Response.")
  }


  #Check variables' names
  checkVariableNames(dataset, parameterName = datatype$Parameter)

  #check data set validity
  checkValidity(dataset, datatype$Type, showOnlyInvalid = F, saveToFile = F)

}
