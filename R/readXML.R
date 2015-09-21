# set global function for reading  millisecons from the timestamp in the data
options(digits.secs=3)


#' Read a list of XML files and return a data frame containing all non eNAEP API events
#'
#' @param xmlFiles A vector of eNAEP XML file names with full path;
#'   typically from \code{Sys.glob("path/*")}
#' @param subjIdVar String of the name of the variable in the returned data frame for the
#'   subjectId, which is the filename of the XML file; default to rewriting "bookletId".
#'   If that's not appropriate, use another meaningful variable name.
#' @examples test <- readXML(Sys.glob("data/*"), subjIdVar="subj")
#' @export
#'
readXML <- function (xmlFiles, subjIdVar = "bookletId") {

  # construct the result data frame
  res <- NULL
  for (f in xmlFiles) {
    # parse the XML file
    doc <- xmlParse(f)
    if (is.null(doc)) {
      # parsing failed
      warning(paste("readXML failed to parse the file ", f, ". Moving on"))
      next
    }
    # conert to data.frame.
    #!!! note that the bookletId is always "TextBooklet", so we don't have studentId
    #!!! have to calc from the filename
    d <-events2df(doc, xpathAllEvents, filterOutEvents=filterOutEvents,
                  dropEvents="api.initializeSettingRequest")
    if (nrow(d)==0) {
      # conversion failed, no useful data extracted
      warning(paste("events2df returned no valid data for the file ", f, ". Moving on"))
      next
    }
    # extract the filename==studentID and drop the path and extension number
    d[, subjIdVar] <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", f)
    # now we get all the events, calc the durations, but drop the initial api event
    res <- rbind(res, d)
  }
  return(res)
}
