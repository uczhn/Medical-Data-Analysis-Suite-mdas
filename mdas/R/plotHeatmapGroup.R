#' A plotHeatmapGroup Function
#'
#' This function plots a heatmap of a data frame
#'
#' @param dat data in  data.frame format.
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @keywords heatmap
#' @export
#' @examples
#' data(iris)
#'plotHeatmapGroup(iris[,1:4])
plotHeatmapGroup = function(dat)



  {

  if(requireNamespace("ggplot2", quietly = TRUE))

    if(requireNamespace("reshape2", quietly = TRUE))

      {


        dat <- dat[,1:4]
        cor <- reshape2::melt(cor(dat, use="p"))

        heat <- ggplot2::ggplot(data=cor, ggplot2::aes(x=cor[,1], y=cor[,2], fill=cor[,3]))
        heat + ggplot2::geom_tile() + ggplot2::labs(x = "", y = "") + ggplot2::scale_fill_gradient2(limits=c(-1, 1))


      }

}
