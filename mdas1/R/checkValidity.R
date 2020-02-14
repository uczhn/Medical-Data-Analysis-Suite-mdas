#' A checkValidity Function
#'
#' This function checks that the designated data are valid given its data type. It also produces data summary
#' including data type and data range.
#' @param dat data in data frame format.
#' @param datatype characters of data types.
#' @param showOnlyInvalid TRUE if you want the function to return only invalid data.
#' @param saveToFile TRUE if you wish to save the data summary to a file.
#' @param fileName a file name used to save the data summary.
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @keywords data checking
#' @export
#' @examples
#' library(FactoMineR)
#' data(wine)
#' datatype = c("factor", "numeric", "numeric", "numeric", "numeric", "integer", "numeric", "numeric",
#' "numeric", "numeric", "numeric", "numeric", "numeric", "integer")
#' checkValidity(wine, datatype)
checkValidity = function(dat, datatype, showOnlyInvalid = F, saveToFile = F, fileName = "datasummary")
{
  dat = as.data.frame(dat)


  data.type = sapply(dat, class)

  re = ""

  for(i in 1:ncol(dat))
  {

    x = stats::na.omit(dat[, i])
    type = datatype[i]
    colname = colnames(dat)[i]

    p = paste(colname, ". Type: ", type, ".", sep = "")

    if(length(x) > 0)
    {

      if(!any(data.type[[i]] != TRUE))
      {
        p = paste(p, "Your data is in", data.type[1], ", which is different from what was specified.", collapse = " ")

      }

      if(type %in% c("numeric", "integer", "real"))
      {
        if(type == "real")
        {
          r = which(grepl("^-?[[:digit:]]", x) == F)
        }
        else
        {
          r = which(grepl("^[[:digit:]]", x) == F)
        }



        #if there is a mis-match
        if(length(r) != 0)
        {
          #report this value
          rep = paste(p, "Your data is invalid. It has the following invalid data values",
                      paste(unique(x[r]), collapse = ", "), collapse = " ")
          re = c(re, rep, "\n")
          #return(colname)
        }
        else if(!showOnlyInvalid)
        {
          rep = paste(p, "Your data is valid. The data range is",
                      paste(range(as.numeric(gsub(",", "", x)), na.rm = T), collapse = " - "), collapse = " ")
          re = c(re, rep, "\n")
        }
      }
      else if(type %in% c("factor", "ordered") & !showOnlyInvalid)
      {
        rep = paste(p, "Your data is valid. The data range is",
                    paste(levels(as.factor(x)), collapse = ", "), collapse = " ")
        re = c(re, rep, "\n")
      }
    }
  }

  if(is.null(re))
  {
    print("Checking complete")
  }
  else
  {
    cat(re)
  }


  if(saveToFile)
  {
    cat(re, file = paste(fileName,".txt"))
  }

}
