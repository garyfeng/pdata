# set global function for reading  millisecons from the timestamp in the data
options(digits.secs=3)
require(XML)

#' Read a list of XML files and return a data frame containing all non eNAEP API events
#'
#' @param xmlFiles A vector of eNAEP XML file names with full path;
#'   typically from \code{Sys.glob("path/*")}. Note that the XMLs can be zipped.
#' @param saveXMLFileNameAs String of variable name to contain the XML file name .
#' @param validate Whether the XML parser should perform validation, either using DTD or schemma;
#'   Default is False; @@ schemma is not yet implemented
#' @param funEvents2df The function to call to turn XML events into rows in a data frame.
#'   Default is set to events2df()
#' @param xpathAllEvents The XPath expression to pass to the funEvents2df()
#' @param dropEvents A vector of event names to be dropped after funEvents2df() calculates
#'   the event times.
#' @param filterOutEvents A vector of event names to be excluded completely in funEvents2df()
#'
#' @examples test <- readXML(Sys.glob("data/*"), saveXMLFileNameAs="subj")
#' @export
#'
readXML <- function (xmlFiles, saveXMLFileNameAs = "xmlFileName", validate=F,
                     funEvents2df = events2df,
                     xpathAllEvents = '//observableData/observableDatum',
                     dropEvents="api.initializeSettingRequest",
                     filterOutEvents=NULL) {

  # construct the result data frame
  res <- NULL
  for (f in xmlFiles) {
    # print out a sign that files are being processed
    cat("."); flush.console()

    # parse the XML file with validation, if it's specified and DTD is specified in the XML
    # The current XML output does not have the schemma specified.

    doc <- tryCatch({
      xmlParse(f, validate= validate)
    }, warning = function(w) {
      warning(paste("readXML failed to parse the file ", f, ".\nWarning = ",w,"\nMoving on"))
      next
    }, error = function(e) {
      warning(paste("readXML failed to parse the file ", f, ".\nError = ",e,"\nMoving on"))
      next
    })

    if (is.null(doc)) {
      # parsing failed
      warning(paste("readXML failed to parse the file ", f, ". Moving on"))
      next
    }
    # convert to data.frame.
    d <-funEvents2df(doc, xpathAllEvents, filterOutEvents=filterOutEvents,
                  dropEvents=dropEvents)
    if (nrow(d)==0) {
      # conversion failed, no useful data extracted
      warning(paste("events2df returned no valid data for the file ", f, ". Moving on"))
      next
    }
    # extract the filename==studentID and drop the path and extension number
    d[, saveXMLFileNameAs] <- basename(f)
    # now we get all the events, calc the durations, but drop the initial api event
    res <- rbind(res, d)
  }
  return(res)
}
