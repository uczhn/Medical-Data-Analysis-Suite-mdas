#' A getClusCategoDesc Function
#'
#' This function performs statistical tests and generate description of a given clusters for categorical variables.
#' @param x a data in data.frame
#' @param clusters clusters in factor
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @return Return a data.table containing distinct characteristic of each cluster
#' @keywords cluster analysis
#' @export
#' @examples
#' data(iris)
#' h = cluster::agnes(iris[, -5], method = "ward")
#' getClusCategoDesc(iris, cutree(h, k=3))
getClusCategoDesc = function(x, clusters)
{
  #require(data.table)
  if(requireNamespace("data.table", quietly = TRUE))

  #This function is used for identifying distinct features
  findDistinct = function(this.x, groups)
  {

    #Calculate the percentage of individual for each characteristic and belonging to each cluster

    if(!is.factor(this.x) | !is.factor(groups))
    {
      stop("Only works with categorical variables")
    }

    # #Remove any NA

    lev = levels(this.x)
    group = levels(groups)

    per = matrix(NA, nrow = length(lev), ncol = length(group))

    for(l in 1:length(lev))
    {
      for(g in 1:length(group))
      {
        per[l,g] = sum(this.x[groups == group[g]] == lev[l]) / sum(this.x == lev[l])
      }
    }

    #For each group, distinct feature is the one with higher percentage
    re = vector()
    for(g in 1:length(group))
    {
      re = rbind.data.frame(re, cbind.data.frame("Group" = group[g],
                                                 "Dist.Fea" = lev[which.max(per[, g])]))
    }

    return(re)

  }


  #######Function main body starts here

  #Remove any NA
  if(any(is.na(x)))
  {
    x = cbind.data.frame(x, clusters)
    x = x[stats::complete.cases(x),]
    clusters = x[, ncol(x)]
    x = x[, -ncol(x)]
  }


  desc = list()
  clusters = factor(clusters)

  fac.id = sapply(x, is.factor)
  fac = as.data.frame(x[, fac.id])

  #if there is only 1 categorical data
  if(sum(fac.id) == 1)
  {
    colnames(fac) = colnames(x)[fac.id]
  }



  #Find distinct feature in each group
  for(k in levels(clusters))
  {
    vars = stats = clus.avg = glo.avg = p.val = per.mode.glo = per.mode.clus = c()
    for(i in names(fac))
    {
      m = matrix(c(summary(x[clusters == k, i]),  summary(x[clusters != k, i])),
                 dimnames = list(levels(x[, i]), cluster = c(k, "Others")), nrow = length(levels(x[, i])), ncol = 2)
      if(any(m < 5))
      {
        ch = stats::fisher.test(m, simulate.p.value = T)
        ch$statistic = NA
      }
      else
      {
        ch = stats::chisq.test(m, simulate.p.value = T)
      }

      #If there is a strong association between variable and groups
      if(ch$p.value < 0.05)
      {
        vars = c(vars, i)
        #clus.avg = c(clus.avg, as.character(mode(x[clusters == k, i])))
        di = findDistinct(x[, i], clusters)
        clus.avg = c(clus.avg, as.character(di$Dist.Fea[di$Group == k]))
        stats = c(stats, ch$statistic)
        #percentage of individual having this mode in this cluster
        per.mode.clus = c(per.mode.clus, mean(x[clusters == k, i] == mode(x[clusters == k, i]))*100)
        #percentage of individual having this mode and belong to this cluster
        per.mode.glo = c(per.mode.glo, sum(x[clusters == k, i] == mode(x[clusters == k, i]))/sum(x[, i] == mode(x[clusters == k, i]))*100)
        glo.avg = c(glo.avg, as.character(mode(x[, i])))
        p.val = c(p.val, format(ch$p.value, digits = 4))
      }
    }

    #if there is any var
    if(!is.null(vars))
    {
      desc[[paste("cluster", k, sep = ".")]] = data.table::data.table(Variable = vars,
                                                          Cluster.Avg = clus.avg,
                                                          Statistics = stats,
                                                          Per.mode.cluster = per.mode.clus,
                                                          Per.mode.global = per.mode.glo,
                                                          Global.Avg = glo.avg,
                                                          P.value = p.val)
    }



  }

  return(desc)
}
