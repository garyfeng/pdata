# require(XML)

#' A funtion that takes a parsed xml doc, find observableDatum nodes according to the xpath, and turn it into a dataframe
#' It assumes the node is a '//observableData/observableDatum', with extended Info (i.e., "content").
#'
#' @param xmldoc The parsed xml output, e.g., doc <- xmlParse("SampleLog.xml")
#' @param xpath The xpath expression that defines the observableEvents; can be 'xpath1|xpath2'
#' @param filterOutEvents The optional filterOutEvents is a list of events (as.character) that should be taken out before calculating the duration of other events. Examples are the system events (e.g., eventType =="api.*")
#' @param dropEvents The optional dropEvents is a list of event names that are used in calculating event durations, but are taken out at the output stage because they are not of interest.
#' @return A data frame, with one observableEvent (defined by the xpath) as a row
#' @details Turning the observableEvents into a dataframe is easy. The function does a number of things that facilitate further analysis.
#'  -- It adds the test-taker-level info
#'  -- It turns extendedInfo in the "content" subnode into a named list, and replaces the '$content' var
#'  -- It corrects the "sceneId" for "nav.scene" events
#'  -- It converts the timestamp into POSIXct and calculates various duration measures, where "dur" is the time in between each events (i.e., row), "durSinceTestStart" when 0 is the timestamp of the very first event, and "durSinceSceneStart" when 0 is for each "nav.scene".
#'  -- It filters out unwanted events using the optional 'filterOutEvents' param; it also drops dropEvents at the end.
#' @export
events2df <-function(xmldoc, xpath, filterOutEvents=NULL, dropEvents=NULL) {
  # first get all the nodes
  ns <- getNodeSet(xmldoc, xpath)
  # we can first get everything in a df
  df <- xmlToDataFrame(ns)
  # we can refer to them as nav.scene.df$extInfo[[i]]$from or $to
  df$bookletId <-xmlValue(getNodeSet(xmldoc, "//bookletId")[[1]])
  df$taskId <-xmlValue(getNodeSet(xmldoc, "//taskId")[[1]])
  df$blockId <-xmlValue(getNodeSet(xmldoc, "//blockId")[[1]])
  df$content <- getExtInfo(ns)

  # filter out events that we don't want
  # This is done before we calculate the time differences, and therefore these intervening events
  # do not affect the calculation of duration
  if (!is.null(filterOutEvents)) {
    df<-subset(df, ! eventType %in% filterOutEvents)
  }

  # correct the "sceneID" for "nav.scene" events:
  # the semantics is a bit confusing with "nav.scene". Its "sceneId" refers to the "from" page, but it is
  #  the only mark of the beginning of the "to" page. So we are changing the "sceneId" to that of the "to" scene.
  # Note that the preceeding "nav.next" page is only a few msec earlier, and can be used as the ending mark of the
  #  "from" page.
  # Because there is not a direct way to access the $from and $to fields of a list of lists, we use sapply
  df$sceneId[df$eventType=="nav.scene"]<- sapply(df$content[df$eventType=="nav.scene"], function(t) t$to)

  # we now calculate the time duration, as POSIXct
  df$ts <-as.POSIXct(strptime(df$timestamp, "%Y-%m-%dT%H:%M:%OS"))

  # we calculate the diff of time; the first row is NA because that's the init
  # the semantics is that dur = the time stayed in this row
  # dur from the previous event
  df$dur <-c(NA, diff(df$ts))
  # duration since the test starts; we set t0 to have only the very initial ts
  t0<- df$ts; t0[-1]<-NA
  df$durSinceTestStart <- timeDiff(t0, df$ts)
  # duration since the last scene
  t0<- df$ts; t0[df$eventType!="nav.scene"]<-NA; t0[1]<-df$ts[1]
  df$durSinceSceneStart <- timeDiff(t0, df$ts)

  # drop rows in the dropEvents
  if (!is.null(dropEvents)) {
    df<-subset(df, ! eventType %in% dropEvents)
  }
  return(df)
}
