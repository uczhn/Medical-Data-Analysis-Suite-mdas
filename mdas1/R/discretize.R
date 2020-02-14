#' A discretize Function
#'
#' This function performs a basic discretization using binning.
#' @param x data.
#' @param no.bin number of bin or
#' 'FD' to use the Freedman-Diaconis rule for identifying number of bins.
#' The number of bin is max-min/2*IQR*n^-(1/3).
#' 'ThreeStage' to discretize data into three stages.
#' 1 if data is more than mu+sigma/2; 0=-1 if data is less than mu - sigma/2; otherwise 0.
#' @return Return the discretized data
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @export
#' @examples
#' data(iris)
#' iris.bin = discretize(iris, no.bin = 5)
discretize = function(x, no.bin=5)
{


  #discretize if continuous variable
  var.type = sapply(x, class)
  n = nrow(x)

  for(i in which(var.type == "numeric"))
  {
    if(no.bin == "FD")
    {

      x[, i] = cut(x[, i], round(max(x[, i]) - min(x[, i]))/(2*stats::IQR(x[, i])*n^(-1/3)), labels = FALSE)
    }
    else if(no.bin == "ThreeStage")
    {

    x[, i] = ifelse(x[, i] > (mean(x[, i]) + (stats::sd(x[, i])/2)), 1, ifelse(x[, i] < (mean(x[, i]) - (stats::sd(x[, i])/2)), -1, 0))
    }
    else
    {
      x[, i] = cut(x[, i], no.bin, labels = FALSE)
    }


  }

  return(x)
}







