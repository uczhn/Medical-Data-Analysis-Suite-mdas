#' A getClusNumericDesc Function
#'
#' This function performs statistical tests and generate description of a given clusters for numerical variables.
#' @param x a data in data.frame
#' @param clusters clusters in factor
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @return Return a data.table containing distinct characteristic of each cluster.
#' @keywords cluster analysis
#' @export
#' @examples
#' data(iris)
#' h = cluster::agnes(iris[, -5], method = "ward")
#' getClusNumericDesc(iris[, -5], cutree(h, k=3))
getClusNumericDesc = function(x, clusters)
{


  if(requireNamespace("data.table", quietly = TRUE))
  desc = list()
  clusters = factor(clusters)


  if(nrow(x) != length(clusters))
  {
    stop("The number of individual and clusters are not equal!")
  }


  #identify which columns are factor
  fac = sapply(x, is.factor)
  non.fac = x[, !fac]

  if(ncol(non.fac)>0)
  {

    for(k in levels(clusters))
    {
      vars = stats = clus.avg = glo.avg = p.val = signs = c()
      for(i in names(non.fac))
      {
        t = stats::wilcox.test(x[clusters == k, i], x[, i])
        if(t$p.value < 0.05)
        {
          vars = c(vars, i)
          stats = c(stats, t$statistic)
          signs = c(signs, ifelse(stats::median(x[clusters == k, i]) > stats::median(x[, i]), "+", "-"))
          clus.avg = c(clus.avg, stats::median(x[clusters == k, i]))
          glo.avg = c(glo.avg, stats::median(x[, i]))
          p.val = c(p.val, format(t$p.value, digits = 4))
        }
      }

      #if there is any var
      if(!is.null(vars))
      {
        desc[[paste("cluster", k, sep = ".")]] = data.table::data.table(Variable = vars,
                                                            Statistics = stats,
                                                            Direction = signs,
                                                            Cluster.Avg = clus.avg,
                                                            Global.Avg = glo.avg,
                                                            P.value = p.val)
      }

    }

    return(desc)
  }



}
