#' A clusterAnalysis Function
#'
#' This function perform a hierarchical clustering, with an option of using other clustering methods e.g. k-mean,
#' fuzzy c-mean, or Partitioning Around Medoids to consolidate clustering results.
#
#' @param x a data frame or matrix of data
#' @param k integer number of clusters
#' @param diss If FALSE, then x is treated as a matrix of observations by variables. Otherwise, distant matrix should be given.
#' @param scale TRUE to scale and center data before clustering
#' @param metric character string specifying the metric to be used for calculating dissimilarities between observations.
#' The currently available options are "euclidean" and "manhattan". Euclidean distances are root sum-of-squares of
#' differences, and manhattan distances are the sum of absolute differences. If x is already a dissimilarity matrix,
#' then this argument will be ignored
#' @param method character string defining the hierarchical clustering method. See agnes for more details.
#' @param hierConsol TRUE to perform cluster consolidation.
#' @param consolMet clustering methods to use. The function implements 4 clustering algorithms: "kmean" (k-mean),
#' "fuzzy" (Fuzzy c-mean), "pam" (Partitioning Around Medoids), and "hierarchy" (Hierarchical Clustering).
#' @param ... arguments pass on to hierarhical clustering algorithm functions.
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @references \url{https://www.sciencedirect.com/topics/computer-science/clustering-technique}
#' @return Return a list containing
#' \item{t}{Hierarchical tree object}
#' \item{cluster}{Cluster outcomes}
#' @keywords clustering analysis
#' @export
#' @examples
#' data(iris)
#' hClustCon(iris[, -5])
hClustCon = function(x, k=1, diss = F, scale = T, metric = "euclidean", method = "ward",  hierConsol = T,
                     consolMet = "kmean", ...)
{

  if(requireNamespace("cluster", quietly = TRUE))

  if(requireNamespace("e1071", quietly = TRUE))

  if(requireNamespace("RANN", quietly = TRUE))


  re = list()

  #to lowercase
  consolMet = tolower(consolMet)

  #if standardized data
  if(scale)
  {
    x = scale(x)
  }

  #if use distant matrix
  if(!diss)
  {
    re$t = do.call(cluster::agnes, c(list(x, diss = F, metric = metric, method = method), ... = ...))
  }
  else
  {
    re$t = do.call(cluster::agnes, c(list(x, diss = T, metric = metric, method = method), ... = ...))
  }

  #consolidate
  if(hierConsol)
  {
    clu = stats::cutree(re$t, k = k)
    ctr = vector()
    for(clus in 1:k)
    {
      #if only one data point is in that cluster
      if(class(x[clu == clus, ]) == "numeric")
      {
        ctr = rbind(ctr, x[clu == clus, ])
      }
      else
      {
        ctr = rbind(ctr, apply(x[clu == clus, ], 2, mean))
      }
    }

    if(consolMet == "kmean")
    {
      km = stats::kmeans(x, centers = ctr)
    }
    else if(consolMet == "fuzzy")
    {
      km = e1071::cmeans(x, centers = ctr)
    }
    else if(consolMet == "pam")
    {
      #find the closest points using nearest neighbour
      closest <- RANN::nn2(data = x, query = ctr, k = 1)

      #check if there is any duplication
      dup = anyDuplicated(closest$nn.idx)
      while(dup>0)
      {
        #find new point
        closest$nn.idx[dup] = RANN::nn2(data = x[-closest$nn.idx[dup], ],
                                        query = matrix(ctr[dup, ], nrow = 1), k = 1)$nn.idx
        #recheck
        dup = anyDuplicated(closest$nn.idx)
      }

      km = cluster::pam(x, k = k, medoids = closest$nn.idx)
    }

    re$cluster = km$cluster
  }
  else
  {
    re$cluster = stats::cutree(re$t, k = k)
  }

  return(re)
}
