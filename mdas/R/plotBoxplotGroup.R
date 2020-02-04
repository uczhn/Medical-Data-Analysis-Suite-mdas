#' A plotBoxplotGroup Function
#'
#' This function plots box plots by groups.
#' @param x numerical observations in data.frame format.
#' @param groups groups in factor.
#' @param scale a logical value to indicate if data centering and scaling should be used. It is recommend to use
#' this when producing a box plot.
#' @param max.plot maximium plot size
#' @param color.blind TRUE to use color pallett suitable for color blind
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @keywords box plot
#' @export
#' @examples
#' data(iris)
#' cl = kmeans(iris[, -5], 3)
#' plotBoxplotGroup(iris[, -5], cl$cluster)
plotBoxplotGroup = function(x, groups, scale = T, max.plot = 20, color.blind = TRUE)
{

  #only plot numerical data

  if(requireNamespace("ggplot2", quietly = TRUE))

  if(requireNamespace("reshape2", quietly = TRUE))

  if(requireNamespace("ggsci", quietly = TRUE))


    variable <-value <-NULL

  if(!is.data.frame(x))
  {
    stop("x is not a data.frame")
  }

  if(!all(sapply(x, is.numeric)))
  {
    stop("x contains non-numerical values.")
  }

  #scale the data.
  if(scale)
  {
    x = scale(x)
  }

  if(!is.factor(groups))
  {
    groups = factor(groups)
  }

  dat = cbind.data.frame(x, "Group" = groups)

  counter = 1
  from = 1
  step = 20
  finished = FALSE
  while(!finished)
  {
    to = min(counter*step, ncol(x))
    id = seq(from,to)
    m = reshape2::melt(dat[, c(seq(from,to), ncol(dat))], id.vars = "Group")
    p1 = ggplot2::ggplot(m, ggplot2::aes(x=variable, y=value, fill = factor("Group"))) +
      ggplot2::geom_boxplot() + ggplot2::facet_wrap( ~ variable, scales="free")

    if(color.blind)
    {
      p1 = p1 + ggsci::scale_color_jco() + ggsci::scale_fill_jco()
    }

    print(p1)

    #update which variables to plot
    from = from + step
    to = min(counter*step, ncol(x))
    counter = counter+1

    #check if we are done
    if(to >= ncol(x))
    {
      finished = TRUE
    }
  }

}
