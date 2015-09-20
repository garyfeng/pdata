######################

#' A function to map the extendedInfo into a list of lists
#' @param nodes A set of XML nodes from getNodeSet() that point to '//observableData/observableDatum'
#' @return a list of lists, with names of the list elements set to the "key" and value to "value"
#' @details This assumes a certain structure of the XML, i.e.,
#'
#'   \code{//observableData/observableDatum/content/pair/key}
#'
#'   \code{//observableData/observableDatum/content/pair/value}
#'
#'   where \code{key} and \code{value} are automic class, i.e., numeric or string. The code then
#'   makes a list with named elements, i.e., 'key' as the key, and 'value' as the value in the list.
#'
#' @export

getExtInfo <- function(nodes) {
  lapply(nodes, function(x){
    # first turn things
    keys<- xpathApply(x, "content/pair/key", xmlValue)
    vals<- xpathApply(x, "content/pair/value", xmlValue)
    names(vals)<-keys
    # need to return the vals, not names(vals)
    invisible(vals)
  })
}
