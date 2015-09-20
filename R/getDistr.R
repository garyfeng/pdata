
#' To compute the empirical density function and hazard rate of response time
#'
#' Human response time, often measured in milliseconds, follow lawful distributions.
#' Given a sample of response time (as a vector or a variable in a data frame),
#' calculate the pdf and hazard rate along with other statistics using the
#' library "muhaz" and other methods.
#'
#' @param df The observed response time, either as a vector or a data frame
#' @param timeVar [optional] The name of the RT variable if df is a data frame
#'    Useful when using the %>% pipeline.
#' @param binwidth [optional] The width of the bin used to computer statistics; in msec.
#' @param maxtime [unused]
#' @param mintime [unused]
#' @return a data frame of distributional statistics such as the pdf, hazard rate, counts, etc.
#'
#' @export
#'
#'

getDistr <-function (df, timeVar="t", binwidth=50, maxtime=10000, mintime=0) {
  require(muhaz)

  if(is.data.frame(df)) {
    fixdur<-unlist(df[,timeVar])
  } else if (is.atomic(df)) {
    fixdur <- as.vector(as.numeric(df))
  } else {
    stop("Data should be either a numeric vector or a data frame")
  }
  # drop na
  fixdur <- fixdur[!is.na(fixdur)]
  if (length(fixdur)<10) stop("Vector length <10")
  #print(str(fixdur))
  fixdur <- as.vector(fixdur)
  fixdur<-round(fixdur/binwidth)*binwidth
  status<-rep(1,length(fixdur))

  result<-kphaz.fit(fixdur,status)
  #if (nrow(result)==0) stop("Output is empty")

  # get the density fun
  fixhist <-hist(fixdur, c(0, result$time, max(fixdur)), plot=F)
  # the lengths don't match.
  n<-length(fixhist$density);
  #result$time2 <-fixhist$breaks[2:n]
  result$pdf <-fixhist$density[2:n]
  result$counts <-fixhist$counts[2:n]

  return (as.data.frame(result))
}
