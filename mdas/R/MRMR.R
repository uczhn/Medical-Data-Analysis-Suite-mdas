#' A MRMR Function
#'
#' This function performs feature selection based on Mutual Information using the maximum relevance
#' minimum redundancy (MRMR).
#' @param x independent variable in data.frame.
#' @param y dependent variable.
#' @param no.bin number of bin used if variables are continuous.
#' @param criteria feature selection scheme to use. 'MID' for Mutual Information Difference
#' criterion, or 'MIQ' for Mutual Information Quotient criterion.
#' @param no.of.variable Number of variable to be selected.
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @references \url{https://link.springer.com/article/10.1007/s10489-018-1305-0}
#' @return Return the selected features.
#' @keywords feature selection
#' @export
#' @examples
#' data(iris)
#' selected.feature = MRMR(iris[, -5], iris[, 5], no.bin = 3, criteria = "MID")
MRMR = function(x, y, no.bin = 5, criteria = "MID", no.of.variable = 5)
{
  x.bin = discretize(x, no.bin)

#:
  mutualInfo <-NULL
  no.fea = ncol(x.bin)
  fea = colnames(x.bin)

  VI = rep(NA, no.fea)
  sel.fea = c()

  #calculate maximum relevance
  S = fea
  for(i in 1:length(S))
  {
    VI[i] = 1/(length(S))*mutualInfo(x.bin[, i], y)
  }

  #first feature is the one with maximum relevancy
  if(length(sel.fea) == 0)
  {
    sel = fea[which.max(VI)]
    #add to selected fea
    sel.fea = c(sel.fea, sel)
    #remove feature
    VI = VI[-which(fea == sel)]
    fea = fea[-which(fea == sel)]
  }

  I.start = 0
  repeat{
    print(length(sel.fea))
    #calculate minimum redundancy
    WI = rep(NA, length(fea))
    I = rep(I.start, length(fea))
    for(i in 1:length(fea))
    {
      S = c(sel.fea, fea[i])

      for(ii in sel.fea)
      {
        I[i] = I[i] + mutualInfo(x.bin[, fea[i]], x.bin[, ii])
      }

      WI[i] = (1/(length(S)^2))*I[i]
    }



    #MID: Mutual Information Difference criterion
    if(criteria == "MID")
    {
      sel = fea[which.max(VI-WI)]
    }
    else if(criteria == "MIQ") #MIQ: Mutual Information Quotient criterion
    {
      sel = fea[which.max(VI/WI)]
    }

    #add to selected fea
    sel.fea = c(sel.fea, sel)
    #Keep MI
    I.start = I[which(fea == sel)]
    #remove feature
    VI = VI[-which(fea == sel)]
    fea = fea[-which(fea == sel)]


    if(length(fea) == 1 | length(sel.fea) == no.of.variable)
    {
      if(length(fea) == 1)
      {
        #add the last feature to selected fea
        sel.fea = c(sel.fea, fea)
        #remove feature
        VI = VI[-which(fea == fea)]
        fea = fea[-which(fea == fea)]
      }

      break
    }
  }
  return(sel.fea)
}



