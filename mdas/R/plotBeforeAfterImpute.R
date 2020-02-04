#' A plotBeforeAfterImpute Function
#'
#' This function plots graphs to compare before and after data imputation. This function is used to
#' assess the quality of the imputed data.
#' @param before data before imputation in data.frame format
#' @param after data fater imputation in data.frame format
#' @param col.names Name of column
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @keywords imputation analysis
#' @export
#' @examples
#' data(iris)
#' #create missing data
#' before = iris
#' before[sample(1:nrow(iris), round(0.1*nrow(iris))), 1] = NA
#' before[sample(1:nrow(iris), round(0.1*nrow(iris))), 3] = NA
#' after = missForest::missForest(before)$ximp
#' plotBeforeAfterImpute(before, after)
plotBeforeAfterImpute = function(before, after, col.names = NULL)
{

   if(requireNamespace("ggplot2", quietly = TRUE))

  if(requireNamespace("gridExtra", quietly = TRUE))

  ## Basic function checking
  #if data is not in data frame format change it
  if(!is.data.frame(before) | !is.data.frame(after))
  {
    before = data.frame("Variable" = before)
    after = data.frame("Variable" = after)
  }

  if(ncol(before) != ncol(after))
  {
    stop("The column of before and after data sets are not equal.")
  }

  if(!all(unlist(lapply(before, class)) == unlist(lapply(after, class))))
  {
    stop("The data type of before and after data sets are not the same.")
  }

  no.col = ncol(before)
  types = lapply(before, class)

  if(is.null(col.names))
  {
    col.names = colnames(before)
  }
  else
  {
    colnames(before) = col.names
    colnames(after) = col.names
  }


  #Only plot variables with missing data
  for(i in 1:no.col)
  {
    if(types[[i]][1] %in% c("factor", "ordered", "integer"))
    {
      p1 = ggplot2::ggplot(before, ggplot2::aes_string(col.names[i], fill = col.names[i])) + ggplot2::geom_bar() + ggplot2::ggtitle("Before")
      p2 = ggplot2::ggplot(after, ggplot2::aes_string(col.names[i], fill = col.names[i])) + ggplot2::geom_bar() + ggplot2::ggtitle("After")
    }
    else
    {
      xmin = min(before[, col.names[i]], after[, col.names[i]], na.rm = T)
      xmax = max(before[, col.names[i]], after[, col.names[i]], na.rm = T)
      p1 = ggplot2::ggplot(before, ggplot2::aes_string(x = col.names[i])) + ggplot2::geom_density(alpha=.2, fill="red") + ggplot2::xlim(c(xmin, xmax)) + ggplot2::ggtitle("Before")
      p2 = ggplot2::ggplot(after, ggplot2::aes_string(x = col.names[i])) + ggplot2::geom_density(alpha=.2, fill="blue") + ggplot2::xlim(c(xmin, xmax)) + ggplot2::ggtitle("After")
    }

    gridExtra::grid.arrange(p1,p2, top = paste(col.names[i], " Missing ", format(mean(is.na(before[, i]))*100, digits = 4), "%", sep = ""))
  }


}
