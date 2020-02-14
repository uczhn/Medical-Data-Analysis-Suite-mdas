#' A getClusterDesc Function
#'
#' This function performs statistical tests and generate description of a given clusters.
#' @param x a data in data.frame
#' @param clusters clusters in factor
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @return Return a data.table containing distinct characteristic of each cluster
#' @keywords cluster analysis
#' @export
#' @examples
#' data(iris)
#' h = cluster::agnes(iris[, -5], method = "ward")
#' getClusterDesc(iris, cutree(h, k=3))
getClusterDesc = function(x, clusters)
{
  re = list()
  #numerical variables
  re$numerical = getClusNumericDesc(x, clusters)

  #categorical variables
  re$categorical = getClusCategoDesc(x, clusters)

  #combine
  vars = direction = clu = glo = c()
  re$desc = vector()

  #combine description of both numerical and categorical data
  combi = function(tab, clusters, isNumeric)
  {
    for(k in 1:length(tab))
    {
      vars = c(vars, tab[[k]]$Variable)
    }

    vars = unique(vars)

    com = vector()

    #identify directions
    for(v in vars)
    {
      direction = c()
      for(k in 1:length(levels(factor(clusters))))
      {
        if(k <= length(tab))
        {
          if(isNumeric)
          {
            d = tab[[k]]$Direction[tab[[k]]$Variable == v]
          }
          else
          {
            d = tab[[k]]$Cluster.Avg[tab[[k]]$Variable == v]
          }
        }
        else
        {
          d = ""
        }


        direction = c(direction, ifelse(length(d)!=0, d, ""))
      }


      #If there are more than one plus sign
      if(sum(direction == "+")>1)
      {
        #find which one is highest
        re$numerical
      }

      #count number of -
      sum(direction == "-")

      com = rbind(com, c(v, direction))


    }


    return(com)
  }


  re$desc = combi(re$numerical, clusters, isNumeric = T)
  re$desc = rbind(re$desc, combi(re$categorical, clusters, isNumeric = F))
  colnames(re$desc) = c("Variables", paste("Cluster", 1:length(levels(factor(clusters))), sep = "."))

  return(re)
}


