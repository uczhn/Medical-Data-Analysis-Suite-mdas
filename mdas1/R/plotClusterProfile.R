#' A plotClusterProfile Function
#'
#' This function plots clusters profile including a heatmap for numerical variables and
#' bar chart for categorical variables.
#' @param x outcome observations in data.frame format
#' @param clusters clusters in factor format
#' @param plot.type Sepcify plot type for numerical data. Use "both" will produce both heatmap and boxplot, otherwise
#' specify either "boxplot" or "heatmap".
#' @param max.group number of graphs per plot
#' @param col characters specifying a set of colors used to represent low, mid, and high values, respectively in heatmap.
#' @param color.blind logical operator to indicate if color blind pallete should be used.
#' @param stat "mean" to use mean or "median" to use meidan when calculating average values
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @keywords cluster analysis
#' @export
# @examples
#' data(iris)
#' cm = kmeans(iris[, -5], centers = cm$cluster)
#' plotClusterProfile(as.data.frame(iris[, 5]), clusters = cm$cluster)
plotClusterProfile = function(x, clusters, plot.type = "both", max.group = 4, col = c("green", "white", "red"), color.blind = F, stat = "median")
{
  if(color.blind)
  #
    plotClusterOutcomes <- plotHeatmapGroup <- NULL
  {
    col = c("blue", "white", "yellow")
  }

  if(!is.data.frame(x))
  {
    stop("x is not a data frame")
  }

  #convert ordinal to integer
  if(any(sapply(x, is.ordered)))
  {
    for(i in which(sapply(x, is.ordered)))
    {
      x[, i] = as.integer(x[, i])
    }
  }


  type = unlist(lapply(x, class))
  fac = which(!lapply(x, class) %in%  c("numeric", "integer"))
  non.fac = which(type %in% c("numeric", "integer"))
  col.names = colnames(x)

  if(length(fac)>0)
  {
    xx = data.frame(x[, fac])
    colnames(xx) = col.names[fac]
    plotClusterOutcomes(xx, clusters, max.group = max.group,
                        alpha = 0.5, color.blind = color.blind, col.names = col.names[fac])

  }

  if(length(non.fac)>0)
  {
    xx = data.frame(x[, non.fac])
    colnames(xx) = col.names[non.fac]
    if(plot.type == "both" | plot.type == "heatmap")
    {
      print(plotHeatmapGroup(xx, clusters, scale = T, col = col, stat = stat))
    }

    if(plot.type == "both" | plot.type == "boxplot")
    {
      plotBoxplotGroup(xx, groups = clusters, scale = T)
    }

  }


}
