#' A mode Function
#'
#' This function identify mode of the given data set.
#' @param x data in vector
#' @param na.rm FALSE if missing data is to be used in analysis
#' @param method method use to calculate mode. "one" and "mode" returns only one mode, return NA if there are more than
#' one modes. "n", "nmodes" returns the number of modes. "all", "modes" return NA if no modes exist, else return all modes.
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @return Return mode of the data
#' @keywords mode
#' @export
#' @examples
#' data(iris)
#' mode(iris[, 5])
mode <- function(x, method = "one", na.rm = FALSE) {
  x <- unlist(x)
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  # Get unique values
  ux <- unique(x)
  n <- length(ux)

  if(n==1)
  {
    return(ux)
  }

  # Get frequencies of all unique values
  frequencies <- tabulate(match(x, ux))
  modes <- frequencies == max(frequencies)

  # Determine number of modes
  nmodes <- sum(modes)
  nmodes <- ifelse(nmodes==n, 0L, nmodes)

  if (method %in% c("one", "mode", "") | is.na(method)) {
    # Return NA if not exactly one mode, else return the mode
    if (nmodes != 1) {
      return(NA)
    } else {
      return(ux[which(modes)])
    }
  } else if (method %in% c("n", "nmodes")) {
    # Return the number of modes
    return(nmodes)
  } else if (method %in% c("all", "modes")) {
    # Return NA if no modes exist, else return all modes
    if (nmodes > 0) {
      return(ux[which(modes)])
    } else {
      return(NA)
    }
  }
  warning("Warning: method not recognised.  Valid methods are 'one'/'mode' [default], 'n'/'nmodes' and 'all'/'modes'")
}
