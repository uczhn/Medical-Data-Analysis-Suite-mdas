#' A outlierDetection Function
#'
#' This function return a list of outliers detected.
#' @param dat a data frame for data values
#' @param grubb Grubb method performs grubbs' test to detect outliers.
#' @param basic  Basic method detects data that lies outside the specified range, given by alpha
#' @param beforePlot plot data distribution of the data
#' @param outlierPlot pot data distribution when outliers are removed
#' @param ... other arguments
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @references \url{https://towardsdatascience.com/a-brief-overview-of-outlier-detection-techniques-1e0b2c19e561}
#' @keywords outlier
#' @return Return a list of outliers containing the data id and their values.
#' @export
#' @examples
#' data(iris)
#' outlierDetection(iris, grubb = TRUE)
# outlierDetection(iris, basic = TRUE, alpha = 1.5)
outlierDetection = function(dat, grubb= FALSE, basic = FALSE, beforePlot = F, outlierPlot = F, ...)
{



  if(beforePlot)
  {

    if(requireNamespace("ggplot2", quietly = TRUE))

    if(requireNamespace("gridExtra", quietly = TRUE))
    p = list()
    #max number of plots to group
    max.group = 4
    counter = 0
    col.names = colnames(dat)
    fac = which(sapply(dat, class) %in% c("numeric", "integer"))
    for(i in fac)
    {
      counter = counter +1

      p[[counter]] = ggplot2::ggplot(dat, ggplot2::aes_string(x = 1, y = col.names[i])) +
        ggplot2::geom_boxplot(notch = TRUE, fill = "light blue") + ggplot2::ggtitle(col.names[i])


      if(counter %% max.group == 0)
      {
        n <- length(p)
        #nCol <- floor(sqrt(n))
        do.call("grid.arrange", p)
        p = list()
        counter = 0
      }
    }

    if(length(p)>0)
    {
      do.call("grid.arrange", p)
    }
  }

  if(grubb == TRUE)
  {
    outlierDetection(dat, ...=...)

  }
  else if(basic == TRUE)
  {
    outlierDetection(dat, plot = outlierPlot, ... = ...)

  }

}
