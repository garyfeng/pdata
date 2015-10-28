#' Function that checks whether a set of columns given as a vector of strings is in a data frame and, if not, gives feedback on
#'   which are missing before stopping execution of the program.
#'
#' @param  requiredCols A character vector containg the names of columns the presence of which in a data frame is to be verfied
#' @param  dfName Name of the data frame in which the columns in requiredCols are to be found
#' @param  specifiedIn An optional argument (default) that allows the output to detail where the varaible names can be changed by the user
#' @return TRUE, unless it stopped


#Florian Lorenz, 2014


checkDfCols <- function(requiredCols, dfName, specifiedIn="which was specified in preferences"){

  if( !all( requiredCols %in% colnames(dfName) ) ){
    for( rc in length(requiredCols) ){
      if ( !( requiredCols[rc] %in% colnames(dfName) ) ){
        stop(paste("Required variable ", requiredCols[rc], specifiedIn, " is not in input data.",sep=""))
      }
    }
  }
  return(TRUE)
}
