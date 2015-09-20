#' A function to compute time differences between t0 and ts, and returns a vector of duration.
#' !!TO-DO!! Wanted to use my backFillNA function, but it throws error messages working with POSIX timestamps.
#' !!TO-DO!! So we do a stupid for loop here
#'
#' @param t0 A vector of time marks (integer, real, or Date), with some elements (at least the first) with non-NA values
#' @param ts A vector of timestamps of the same length and type of t0.
#' @return A vector of real values that is ts-t0. When t0 is NA, it is back filled with the last nonNA value.
#' @details Note that the return value is of type "difftime";
#'   ggplot and dplyr seem to have trouble with them. You can savely convert them to seconds using "as.numeric()".
#' @export

timeDiff <- function(t0, ts) {
  # error checks; ignored for now
  prevVal<-t0[1]
  if(is.na(prevVal)) stop("First value of t0 cannot be NA")
  # back fill NA with a for loop
  # skip if no NA in t0 for speed
  if (! any(is.na(t0))) return ((ts-t0))
  # now do the for loop
  for (i in 1:length(t0)){
    if (is.na(t0[i])) {
      t0[i]<- prevVal
    } else {
      prevVal<-t0[i]
    }
  }
  return(ts-t0)
}
