#' A clusterPlot Function
#'
#' This function produces a cluster plot using the first two columns as axes. Otherwise sepcify use.PCA
#' to TRUE to use PCA to tranform data for plotting.
#' @param x data in a data.frame format
#' @param y cluster results
#' @param title title for the cluster plot
#' @param use.PCA TRUE to use PCA to tranform data for plotting
#' @param color.blind TRUE if colors for the colour blind palette to be used
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @keywords cluster analysis
#' @export
#' @examples
#' data(iris)
#' cm = kmeans(iris[, -5], centers = 3)
#' clusterPlot(iris[, -5], y = factor(cm$cluster))
clusterPlot = function(x, y, title = "Cluster plot", use.PCA = FALSE, color.blind = T)
{
  #require(ggsci)
  if(requireNamespace("ggsci", quietly = TRUE))
  #require(ggplot2)
  if(requireNamespace("ggplot2", quietly = TRUE))
    Cluster <- NULL
  #Use PCA to tranform data
  if(use.PCA)
  {
    #require(FactoMineR)
    if(requireNamespace("FactoMineR", quietly = TRUE))

    if(any(sapply(x, is.factor)))
    {
      x.pca = FactoMineR::FAMD(x, ncp = Inf, graph = F)
    }
    else
    {
      x.pca = FactoMineR::PCA(x, ncp = Inf, graph = F)
    }

    x = x.pca$ind$coord
  }

  mydata <- cbind.data.frame(x,"Cluster" = y)
  colnames(mydata) = make.names(colnames(mydata))
  p1 = ggplot2::ggplot(data = mydata, ggplot2::aes_string(x = colnames(mydata)[1], y = colnames(mydata)[2])) +
    ggplot2::geom_point(ggplot2::aes(shape = Cluster, color = Cluster)) +
    ggplot2::stat_ellipse(geom = "polygon", alpha = 0.2, ggplot2::aes(fill = Cluster, color = Cluster)) +
    ggplot2::ggtitle(title)

  if(color.blind)
  {
    p1 = p1 + ggsci::scale_color_jco() + ggsci::scale_fill_jco()
  }

  return(p1)
}
