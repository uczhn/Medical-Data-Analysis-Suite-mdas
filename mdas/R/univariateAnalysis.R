#' A univariateAnalysis Function
#'
#' This function produces univariate plots including histogram with density plot, box plot, q-q plot, and calculate
#' the Shapiro-Wilk statistic for numeric data.
#' @param dat data in data.frame format
#' @param hist a plot of histogram
#' @param boxplot  a box plot showing distribution of the variable
#' @param qqnorm a qnantile quantile plot
#' @param shapiro a  Shapiro-Wilk normality test.
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @return Return plots from univariate analysis.
#' @keywords univariate analysis
#' @export
#' @examples
#' data(iris)
#' univariateAnalysis (iris, qqnorm  = TRUE)

univariateAnalysis  = function(dat, hist = FALSE,boxplot = FALSE,qqnorm  = FALSE, shapiro = FALSE){

  graphics::par(mfrow = c(1,4))



  if (hist ==TRUE){


    for(i in 1:4) {  graphics::hist(dat[,i], main=names(dat)[i], xlab = names(dat)[i], col = "skyblue")}


  }


  if(boxplot == TRUE) {


    for(i in 1:4)  {graphics::boxplot(dat[,i], main=names(dat)[i], xlab = names(dat)[i], col = "skyblue")}

  }

  if(qqnorm ==TRUE) {


    for(i in 1:4) {stats::qqnorm(dat[,i], main=names(dat)[i], xlab = names(dat)[i], col = "skyblue")}
  }

  if(shapiro ==TRUE) {

    results <-for(i in 1:4)  {stats::shapiro.test(dat[,i])}
    print(results)
  }
}



