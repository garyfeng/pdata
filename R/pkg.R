
#' load a R package, download if necessary
#'
#' @param x Name of the library to load, in string
#'
#' @export
pkg <- function(x){
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop(paste("Can not load library", x))
  }
}
