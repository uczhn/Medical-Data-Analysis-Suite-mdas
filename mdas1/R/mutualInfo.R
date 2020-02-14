#' A mutualInfo Function
#'
#' This function calculates mutual information of the two variables.
#' @param x numerical or factor data.
#' @param y numerical or factor data.
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @return Return mutual information
#' @keywords mutual information
#' @export
#' @examples
#' data(iris)
#' mutualInfo(iris[, 1], iris[, 5])
mutualInfo = function(x,y)
{
  lev.x = levels(factor(x))
  lev.y = levels(factor(y))
  mi = 0

  for(i in 1:length(lev.x))
  {
    for(j in 1:length(lev.y))
    {
      prob.joint = jointprob(x,lev.x[i],y,lev.y[j])

      mi = mi + (prob.joint * mylog2(prob.joint/(prob(x,lev.x[i]) * prob(y,lev.y[j]))))
    }
  }

  return(mi)
}

mylog2 = function(x)
{
  return(ifelse(x != 0, log2(x), 0))
}

#Calculate probability
prob = function(x, event)
{
  prob = sum(x == event)/length(x)
  return(prob)
}

#Calculate joint probability
jointprob = function(x, xEvent, y, yEvent)
{
  prob = sum(x == xEvent & y == yEvent) / length(x)
  return(prob)
}

