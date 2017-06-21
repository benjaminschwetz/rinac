#' Finding extremes in a data set
#'
#' @param x A data frame or a vector of values
#' @param column If x is a data frame, the name of the column needs to be given as a character string.
#' @param pits Boolean: Should the function filter pits or peaks?
#' @return A vector of Boolean values with the length of x, returning true if the respective point is an extreme
#' @examples
#' find_extremes(InsectSprays,"count") #finds minima
#' # or without column argument
#' find_extremes(InsectSprays$count,pits=FALSE) #finds maxima
#' # wrappers
#' peaks(InsectSprays$count)
#' pits(InsectSprays$count)
#' # example using functions from dplyr package
#' dplyr::filter(InsectSprays,peaks(count))
find_extremes <- function(x,column=NULL,pits=TRUE){
  if(is.null(column)) {
    x.tb <- pastecs::turnpoints(x)
  } else {
  x.tb <- pastecs::turnpoints(x[,paste(column)])
  }
  if(pits) {
    pos.p <- x.tb$pos[x.tb$pits]
  } else {
    pos.p <- x.tb$pos[x.tb$peaks]
  }
  vec.p <- 1:x.tb$n %in% pos.p
  return(vec.p)
}
#wrappers for easy access to be used i.e. with dplyr::filter
#' @describeIn find_extremes Wrapper for finding peaks
peaks <- function(x) find_extremes(x, pits=FALSE)
#' @describeIn find_extremes Wrapper for finding pits
pits <- function(x) find_extremes(x, pits=TRUE)

