#' A checkImpute Function
#'
#' This function performs statistical tests to check if two data distributions generated from before and after imputation
#' are the same.
#' @param before data frame before perform imputation.
#' @param after data frame after perform imputation.
#' @param sig significant value threshold.
#' @param plot TRUE to plot before and after imputation data with significant different distribution.
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @return Return a list of variables that have different distribution statistically after imputation.
#' @keywords imputation analysis
#' @export
#' @examples
#'library(FactoMineR)
#' data(wine)
#' set.seed(100)
#' for(i in 2:ncol(wine))
#' {
#' #Generate missing data
#' missing = sample.int(nrow(wine), round(0.2*nrow(wine)))
#' wine[missing, i] = NA
#' }
#'
#' #Impute using mean
#' impute.wine = wine
#' for(i in 2:ncol(wine))
#' {
#'   impute.wine[which(is.na(wine[, i])), i] = mean(wine[, i], na.rm = TRUE)
#' }
#'
#' checkImpute(wine, impute.wine, sig = 0.05)
checkImpute = function(before, after, sig = 0.05, plot = F)
{
  if(!all(is.data.frame(before), is.data.frame(after)))
  {
    stop("The data is not in data frame format.")
  }

  if(class(before) != class(after))
  {
    stop("Data type of before and after do not match.")
  }

  #Perform statistical tests to compare two data distributions.
  statsTest = function(x, y, data.type)
  {
    #use Kolmogorov-Smirnov Tests
    if(data.type %in% c("numeric", "integer"))
    {
      #return(ks.test(x, y, alternative = "two.sided"))
      return(stats::ks.test(x, y, alternative = "two.sided"))
    }
    else
    {

      ks = stats::chisq.test(c(rep("G1", times = length(x)), rep("G2", times = length(y))), c(x, y), simulate.p.value = T)
      return(ks)
    }
  }

  resultFormat = function(vars, pvars, before, after, plot)
  {
    if(is.null(vars))
    {
      return("The data distributions before and after imputation are not statistically different.")
    }
    else
    {
      if(plot)
      {
        #plotBeforeAfterImpute(before, after)
        plot(before, after)

      }

      #return(data.table(Variable = vars,
                        #P.value = format(pvars, digits = 4)))
      return(data.table::data.table(Variable = vars,
                        P.value = format(pvars, digits = 4)))
    }
  }

  #declaration
  vars = pvars = id = c()

  #if a data frame is given
  if(is.data.frame(before))
  {
    #require(data.table)
    if (requireNamespace("data.table", quietly = TRUE)){
    }

    if(!all(unlist(sapply(before, class)) == unlist(sapply(before, class))))
    {
      stop("Data type of variables in before and after do not match.")
    }


    for(i in 1:ncol(before))
    {
      st = statsTest(before[, i], after[, i], class(before[, i])[1])

      #Only report significant distribution
      if(st$p.value < sig)
      {
        vars = c(vars, colnames(before)[i])
        pvars = c(pvars, st$p.value)

        #keep index for plots
        id = c(id, i)
      }
    }

    return(resultFormat(vars, pvars, before = before[, id], after = after[, id], plot = plot))
  }
  else
  {
    st = statsTest(before, after, class(before)[1])

    #Only report significant distribution
    if(st$p.value < sig)
    {
      vars = c(vars, colnames(before)[i])
      pvars = c(pvars, st$p.value)
    }

    return(resultFormat(vars, pvars, before, after, plot))
  }

}
