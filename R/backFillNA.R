
#' A function to back fill NAs in a vector with the last non-NA value
#'
#' The function will throw errors when the input is not an atomic vector.
#'
#' @param x A vector with some NAs
#'
#' @return A vector with NAs back filled
#'
#' @details see https://gist.github.com/garyfeng/27e7f8e406192a8cb33a
#' where we compared 3 algorithms: a simple loop, a recursive, and this non-recursive algorithms.
#' A simple performance comparison is also there. The conclusion is clear, this is about 100X faster
#' than the loop version, and ~70X faster than the recursive on a 10K list. 
#'
#' @export

backFillNA<- function (x) {
  # input checking
  if(!is.atomic(x)) stop("backFillNA only takes a atomic vector as input.")
  if(!is.null(dim(x))) stop("backFillNA cannot take a matrix or array or data.frame as input.")

  # return if there is no NA
  if(!any(is.na(x))) return(x)

  # now let's work
  nas<- which(is.na(x))
  # trick from http://stackoverflow.com/questions/24837401/find-consecutive-values-in-vector-in-r
  naList<-split(nas, cumsum(c(1, diff(nas) != 1)))

  # get the backfill values
  valueList<-lapply(naList, function(nalist) {
    prev<- nalist[1]-1
    #print(nalist)
    if (prev<=0) prev <- 1
    x[nalist]<-x[prev]
    return(x[nalist])
  })
  # now back fill
  # can't use unlist(), as it converts everything to a common class.
  # see http://stackoverflow.com/questions/13859905/returning-a-vector-of-class-posixct-with-vapply
  #x[unlist(naList)] <- unlist(valueList)
  x[unlist(naList)] <- do.call(c, valueList)

  return (x)
}
