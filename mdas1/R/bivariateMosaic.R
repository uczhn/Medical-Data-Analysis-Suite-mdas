#' A bivariateMosaic Function
#'
#' This function produces a heatmap for the bivariate analysis result.
#' @param dat data results from BivariateAnalysis function
#' @param title graph title
#' @param color characters of color name for low, mid, and high values respectively.
#' @param limit scale of the heat plot. Two number to represent minimum, and maximum range to plot.
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @references \url{https://www.tandfonline.com/doi/full/10.1080/10618600.2018.1473780}
#' @keywords bivariate plot
#' @export
#' @examples
#' data(iris)
#' result = bivariateAnalysis(iris)
#' #Plot only significant correlation/association
#' bivariateMosaic(result[result$pval <0.05,])
bivariateMosaic = function(dat, title = "Bivariate analysis", color = c("blue", "yellow", "red"), limit = c(0,0.05))
{

  if(requireNamespace("ggplot2", quietly = TRUE))

  X <-Y <-p.val <-shortName <-NULL

  a = dat[, c(1,2,5)]
  colnames(a) = c("X", "Y", "p.val")
  b = dat[, c(2,1,5)]
  colnames(b) = c("X", "Y", "p.val")
  g = rbind.data.frame(a,b)


  if(length(color) != 3)
  {
    color = c("blue", "yellow", "red")
  }

  ggplot2::ggplot(g,  ggplot2::aes(X, Y, fill = p.val)) +  ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = color[1],  high = color[3], mid = color[2], limit = limit) +  ggplot2::ggtitle(title) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
}
