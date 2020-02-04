#' A missingAnalysis Function
#'
#' This function performs missing data pattern Analysis. The function calculates correlation between missing data
#' between variables.
#' @param dat data set in data frame format.
#' @param lower.limit the correlation limit that specifies significant pattern between variables.
#' @param plot TRUE to produce plots.
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @return Return a list containing missing data proportion and missingness correlation
#' @keywords missing data
#' @export
#'  @examples
#'library(FactoMineR)
#' data(wine)
#' set.seed(100)
#' for(i in 2:ncol(wine))
#' {
#' #Generate missing data
#' missing = sample.int(nrow(wine), round(0.2*nrow(wine)))
#' wine[missing, i] = NA
#' }
#' #"The correlation of missing data between variables are below the specified lower limit."
#' missingAnalysis(wine)
#' missingAnalysis(wine, lower.limit = 0.1)
missingAnalysis = function(dat, lower.limit = 0.5, plot = TRUE)
{

  if (requireNamespace("ggplot2", quietly = TRUE))
  result = list()

  plotMissing <- shortName <- NULL

  #missing percentages
  #result$missingTab = plotMissing(dat, plot = plot)
  result$missingTab = plotMissing(dat, plot = plot)

  #Analyse missing pattern
  x <- as.data.frame(abs(is.na(dat)))

  #Extracting variables that have some missing values.

  y <- x[which(sapply(x, stats::sd) > 0)]
  #Calculate correlation
  #c = cor(y)
  c = stats::cor(y)

  #Obtain variables that are missing-correlated
  cc = which( abs(c) >= abs(lower.limit) & abs(c) < 1, arr.ind=T )
  if(nrow(cc)==0)
  {
    print("The correlation of missing data between variables are below the specified lower limit.")
    return(result)
  }



  rname = shortName(rownames(cc))

  cname = shortName(colnames(y)[as.numeric(cc[, 2])])
  vname = vector(mode = "numeric")
  for(i in 1:nrow(cc))
  {
    vname[i] = c[cc[i,1], cc[i,2]]
  }

  result$missingCor = cbind.data.frame(rname, cname, vname)
  colnames(result$missingCor) = c("Var1", "Var2", "Correlation")

  #plot missing information, correlation, and frequency
  if(plot)
  {
    #missing data correlation

    plot(ggplot2::ggplot(result$missingCor, ggplot2::aes("Var1", "Var2", fill = "Correlation")) + ggplot2::geom_tile() +
           ggplot2::scale_fill_gradient2(low = "blue",  high = "red", mid = "yellow") +
           ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
           ggplot2::ggtitle("Correlation between missing data"))

    #missing frequency
    br = seq(0,100,by=5)
    #hist(result$missingTab$P.Missing[result$missingTab$P.Missing>0], br, plot = TRUE, col = "blue", xlab = "Percentage missing",
  graphics::hist(result$missingTab$P.Missing[result$missingTab$P.Missing>0], br, plot = TRUE, col = "blue", xlab = "Percentage missing",
         ylab = "Number of variables", main = "Histogram of missing variables")
  }


  return(result)

}

