#' A plotMissing Function
#'
#' This function produces a plot showing percentage of data missing in each variable.
#' @param dat data in data frame format.
#' @param plot TRUE to plot percentage of missing data
#' @keywords missing data
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @return Return percentage of data missing in each variable
#' @export
#' @examples
#' library( FactoMineR)
#' data(wine)
#' set.seed(100)
#' for(i in 2:ncol(wine))
#' {
#' #Generate missing data
#' missing = sample.int(nrow(wine), round(0.2*nrow(wine)))
#' wine[missing, i] = NA
#' }
#' plotMissing(wine)
plotMissing = function(dat, plot = T)
{

  P.Missing <- Var  <-NULL

  if(requireNamespace("ggplot2", quietly = TRUE))


  #calculate missing data
  mis = data.frame("Var" = colnames(dat),
                   "Missing" = sapply(dat, function(x){sum(is.na(x))}),
                   "P.Missing" = sapply(dat, function(x){mean(is.na(x))*100}))

  if(plot)
  {
    p = ggplot2::ggplot(mis, ggplot2::aes(y = P.Missing, x = Var)) + ggplot2::geom_bar(stat = "identity", fill = "lightblue") + ggplot2::coord_flip() +
      ggplot2::ylim(0, 100) +
      ggplot2::ggtitle("Percentage of missing data")
    print(p)
  }

  return(mis)
}
