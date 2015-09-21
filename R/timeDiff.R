#' A function to compute time differences between t0 and ts, and returns a vector of duration.
#'
#' In process data analysis we frequently need to calculate the duration between two events. since
#'    in most cases each event is a row, we can set a particular event as t0, with NA filled for all
#'    other rows except for the event of interest. Then we calcualte the lag time for the vector
#'    ts.
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
  # if there are NAs, backfill; the current backFillNA() can handle POSIXct
  t0 <- backFillNA(t0)

  return(ts-t0)
}
