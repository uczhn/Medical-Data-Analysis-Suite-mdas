#' A dataImputation Function
#'
#' This function performs impuation using 3 techniques i.e. Random Forest (RF), Multiple Imputation by
#' Chained Equations (MICE), and MICE with RF. The imputed data are check against the original data to see if
#' the imputed data distribution differs from original data distribution.
#' @param dat data to be imputed in data.frame format
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @references \url{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-019-3110-0}
#' @references \url{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.169.5745&rep=rep1&type=pdf}
#' @return A list contains imputation models with imputed data, and a summary of imputation results.
#' @keywords imputation
#' @export
#' @examples
#' library(FactoMineR)
#' data(wine)
#' dataImputation = function(wine)
dataImputation = function(dat)
{

  if (requireNamespace("mice", quietly = TRUE))
  if (requireNamespace("missForest", quietly = TRUE))

  result = list()

  imp = list()

  imp$missForest = missForest::missForest(dat)

  imp$mice = mice::mice(dat, visitSequence = "monotone")

  imp$mi.rf = mice::mice(dat, visitSequence = "monotone", defaultMethod = c("rf", "rf", "rf", "rf"))

  #function to interpret imputation results. Return the number of variables that have distribution different from the original data
  interpert = function(before, after)
  {
    ch = checkImpute(before, after)

    if(is.character(ch))
    {
      return(0)
    }
    else
    {
      return(ncol(ch))
    }
  }


  dif.dist = c(interpert(dat, imp$missForest$ximp),
               #interpert(dat, complete(imp$mice)),
               interpert(dat, tidyr::complete(imp$mice)),
               #interpert(dat, complete(imp$mi.rf)))
               interpert(dat, tidyr::complete(imp$mi.rf)))


  #Put the results together
  result$imp = imp
  result$summary = cbind.data.frame("Method" = c("RF", "MICE", "MICE+RF"),
                                    "No. of variables with diff dist" = dif.dist)
  return(result)
}
