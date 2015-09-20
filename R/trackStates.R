#' compute states from events
#'
#' @param df Data frame from which to compute the state variables
#' @param stateVarName The name of the state variable to be exported. Not used in the current version
#'    as we return just the states as a list of lists
#' @return A list of lists that represent the state variables.
#' @details The current code is specifically for the Mango reading SBT task. We will find ways to
#'    abstract this and make it more general.
#' @export

trackStates <- function(df, stateVarName = "states") {
  # check to make sure vars are available
  require(dfat)
  requiredVars <-c("sceneId", "eventType", "content")
  stopifnot(is.data.frame(df))
  stopifnot(nrow(df)>10)
  stopifnot(all(requiredVars %in% names(df)))

  # init
  res <-NULL
  df[, stateVarName] <-NA
  states <- list(
    onPage ="",     # <-paste(onChapter, ifelse(onChapter="First Chapter), firstChapterPage, lastChapterPage)
    onChapter="",   # "" before any page is shown; then the implicit state of the tab.
    firstChapterPage=NA,   # keeping track of the implicit/explicit page numer for ch1
    lastChapterPage=NA,    # tracking the page number of ch2
    dualTabState=NA,       # Express 1 is wierd, as it remembers its own tab state.
    #  If you do "Express1" w/ tab on "First Chapter", nav.back to "2Interpret3" (which
    #  is always "Last Chapter", and then come back to "Express1", it goes back to "First Chapter"
    sceneHistory=NULL      # this is a vector of sceneId history
  )
  scenesToInitAll = c("Intro01")
  scenesWithNoPages = c("Intro01", "Intro02", "Task2Title", "Task3Title", "Conclusion" )
  scenesWithTabs = c("Express1","ExpressReset","Express2","Task3Response" )
  scenesResetToFirstChapter =
    c("Chapter1Read", "InterpretIntro", "Interpret1", "Interpret2", "Interpret3", "YouInterpret1","YouInterpret2")
  scenesResetToLastChapter = c("Chapter2Read","2Interpret1", "2Interpret1Close","2Interpret2","2Interpret3")
  #     c("Chapter2Read","2Interpret1", "2Interpret1Close","2Interpret2","2Interpret3",
  #       "Express1","ExpressReset","Express2","Task3Title","Task3Response")
  sceneDualTab = c("Express1","ExpressReset","Express2","Task3Response")

  # looping
  for (i in 1:nrow(df)) {
    # get local var of this row
    iSceneID <-df$sceneId[i]; iEventType <-df$eventType[i];
    iContent <-df$content[i]; # this gives a named list
    iTo <-iContent@"to"; iFrom <-iContent@"from"; iMethod <-iContent@"method"; #NA if not present

    # update states
    ###############
    # New scenes
    # generally speaking, scenes set the onChapter, and initialize the page number if it's not already
    #       if (iSceneID %in% scenesToInitAll) {
    #         # very first scene, let's init everything
    #         states$onPage <-"" ; states$onChapter="";
    #         # states$firstChapterPage=NA; states$lastChapterPage=NA
    #       }
    if (iSceneID %in% scenesResetToFirstChapter) {
      states$onChapter <-"First Chapter"   # new scene forces to ch1
      if(is.na(states$firstChapterPage)) states$firstChapterPage=1; # only init page if NA
    }
    if (iSceneID %in% scenesResetToLastChapter) {
      states$onChapter <-"Last Chapter"    # new scene forces to ch2
      if(is.na(states$lastChapterPage)) states$lastChapterPage=1 # only init page if NA
    }
    if (iSceneID %in% sceneDualTab) {
      # first time entering these scenes, set tab to the current chapter, and remeber its state
      # then we remember that everytime we switch tabs ever since we update this.
      if(is.na(states$dualTabState)) states$dualTabState = states$onChapter
      # set the states to previously remembered
      states$onChapter=states$dualTabState;
    }
    #lastly, add the current scene to the history
    states$sceneHistory <- c(states$sceneHistory, as.character(iSceneID)) # adding the currentscene
    ###########
    # Events
    if (iEventType == "nav.tab") {
      states$onChapter <- iTo
      if (iSceneID %in% sceneDualTab) states$dualTabState = states$onChapter
    }
    if (iEventType == "nav.page") {
      # explicit page turn events; value depending on the chapter, tab, etc.
      # LLBs can potentially change the tab, so let's deal with them first
      if(iMethod =="auto") {
        # LBB, which forces page turns and sometimes Chapter turns
        if(iSceneID=="YouInterpret1") {
          # LBB in "YouInterpret1" set it to page = 2
          states$onChapter="First Chapter"; states$firstChapterPage=2;
        } else if(iSceneID=="YouInterpret2") {
          # LBB in "YouInterpret2" set it to page = 3
          states$onChapter="First Chapter"; states$firstChapterPage=3;
        } else if(iSceneID=="2Interpret1Close") {
          # LBB in "2Interpret1Close" set it to page = 1
          states$onChapter="Last Chapter"; states$firstChapterPage=1;
        } else if(iSceneID=="2Interpret2") {
          # LBB in "2Interpret2" set it to page = 1
          states$onChapter="Last Chapter"; states$firstChapterPage=1;
        } else if(iSceneID=="Express1") {
          # LBB in "Express1" set it to tab=FirstChapter
          # @@@@ BUG: the current version of the taskplayer recorded this as going from page 1 to 1 or 0 to 1.
          # This is incorrect.
          states$onChapter="First Chapter";
        }
      } else {
        # Now we know which chapter is on, we can update pags.
        # user actions; update the corresponding chapter pages depedning on which chapter we are on.
        # We leave the other chapter page untouched, i.e., persistent
        # Note: in the XML page number doesn't tell you which chapter this comes from. Duh!
        if(states$onChapter=="First Chapter") {
          states$firstChapterPage = as.numeric(iTo)
        } else if(states$onChapter=="Last Chapter") {
          states$lastChapterPage  = as.numeric(iTo)
        }
      }
      # now remember the dualTab state if it's one of those dualtab scenes
      if (iSceneID %in% sceneDualTab) {
        states$dualTabState = states$onChapter
      }

    }  # end if nav.page

    if (iEventType=="nav.back") {
      # we need to take care of cases where you go back from "Express1" to "2Interpret3",
      # because you may be on 1stCh in Express1, and be forced to lastCh when you go back,
      # but you will return to Express1 with 1stCh, rather than the persistent value of onChapter.
      # So we track the stat of the dualTab using states$dualTabState. When you go back from Express1,
      # we need to track the value.
      if (iSceneID %in% sceneDualTab) states$dualTabState = states$onChapter
    } # end if nav.back

    ############
    # calculate the onPage var
    if(states$onChapter=="First Chapter") {
      states$onPage = paste(states$onChapter, states$firstChapterPage)
    } else if(states$onChapter=="Last Chapter") {
      states$onPage  = paste(states$onChapter, states$lastChapterPage)
    }
    # wipe the scenes without text
    if (iSceneID %in% scenesWithNoPages) {
      states$onPage <-""  # no visible page for these scenes
    }

    # error checking:
    if (states$onPage =="Last Chapter 3")
      warning(paste("Error: ", df$bookletId[1], i, iSceneID, iEventType, " LastChapter3"))
    if (states$onChapter =="First Chapter" & iSceneID %in% c("2Interpret1Close", "2Interpret2", "2Interpret3"))
      warning(paste("Error: ", df$bookletId[1], i, iSceneID, iEventType, " FirstChapter for Task2"))

    # save states
    #df[i, stateVarName] <- as.list(states)
    res <-append(res, list(states))
  }

  return(res)
}
