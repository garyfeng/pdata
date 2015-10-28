require("plyr")
require("ggplot2")

#' Main function of GFEye package. Gets user preferences, check user input, calls other programs to create output.
#'
#' The first 12 columns must be in correct order, but additional (currently unused) columns are allowed
#'    includes code written by Gary Feng and Nigshan Zhang
#'
#' @param datafilename String with valid path and location for input data. Input data must conform to this scheme:
#'    1 header line, at least 12 columns of data:
#'     subjectID, vtime, gazetime, x, y, page name, AOI_ID, AOI_content, x1, y1, x2, y2
#' @param picturelocation Valid path of folder containing picture files over which the heat maps are to be placed. These
#'    must be .png files, and the file names must correspond to the page name in datafilenames
#' @param outfilename Optional if no preferences, name of output file. If this does not contain a path, the file with
#'     be saved in the default directory.
#' @param preferences Optional, user preferences for heat map. Must be either an R list or a file (with header line)
#'                 containing
#'                 1) string indicating a valid R color for the lowest value of the heat map
#'                 2) string indicating a valid R c color of the highest value of the heat map
#'                 3) vector of screen size to be used (in file: two entries, not a vector)
#'                 4) numeric value between 0 and 1 transparancy value for heat map
#'                 5) numberic value indicating the desired resolution for heat map
#'                 6) string with name of the variable containing the subject ID
#'                 7) string with name of the variable containing gaze time
#'                 8) string with name of the variable containing AOI ID
#'                 9) string with name of the variable containing x Coordinates
#'                 10) string with name of the variable containing y Coordinates
#'                 11) string with label for the Subject ID variable
#'                 12) string with label for the AOI ID variable
#'                 Any argument that is not followed by other arguments is optional.
#'                 If read from a file, the file must be comma-sepearated with no headers.
#' @param coverpage Optional, an argument that allows the user to turn off the cover page of the report. Default="On".
#'                 Any user-supplied value other than the exact string "On" will turn cover page genration off.
#'
#' @return PDF file named according to outputfilename containg a summary of gaze time by AOI and subject as well as heat maps
#'
#' @export

#
#Florian Lorenz, March/April 2014, revised December 2014


heatmapanalysis <- function ( datafilename, picturelocation, outfilename, outputTitle, preferences, coverPage="On"){


  #number of columns, starting at first, from input file that are used in the program below
  #others are truncated. This can be changed in case of future program changes
  ncolsused <- 12
  #column names: number of entries must be equal to scalar ncolsused
  #columnnames <- c("subj", "vtime", "gazetime", "x", "y", "page", "AOI_ID", "AOI_content", "x1", "y1", "x2", "y2")

  #default perferences: heat map minimum value color, maximum value color, screen size, heat map transparancy, resolution
  defaultprefs <- list("white", "red4", c(1680,1050), 0.40, 200,
                       "subj", "gazetime", "AOI_ID", "x", "y", "SubjectID", "Task")



  #################################BEGIN: CHECK USER INPUT################################################

  #Check user input: data file name
  if( missing(datafilename) ){
    stop("Required data file name and location is missing. Terminating program.")
  } else if( !is.character(datafilename)){
    stop("Required data file name and location is not a string. Terminating program.")
  } else if (!file.exists(datafilename)) {
    stop("Required data file name and location is missing. Terminating Program.")
  }

  #Check user input: picture location
  if( missing(picturelocation) ){
    stop("Required location of .png files is missing. Terminating program.")
  } else if( !is.character(picturelocation)){
    stop("Required location of .png files is not a string. Terminating program.")
  } else if(!file.exists(picturelocation)){
    stop("Required location of .png files does not exist. Terminating program.")
  }

  #Check user input: output title
  if( missing(outputTitle) | !is.character(outputTitle)){
    print("Optional output tilte is missing or not a string. Using default: input data file name.")
    #get title from datafile name
    title <- substr(datafilename, (rev(gregexpr("\\/",datafilename)[[1]])[1]+1),(nchar(datafilename)-4))
  }

  #check user input: output file name
  if( missing(outfilename) ){
    print("Optional output file name and location is missing. Using defaults.")
    outfilename <- paste(title,".pdf",sep="")
  } else if( !is.character(outfilename)){
    print("Optional output file name and location is not a string. Using defaults.")
    outfilename <- paste(title,".pdf",sep="")
  }
  slashindex <- rev(gregexpr("\\/",outfilename)[[1]])
  if (slashindex < 1){
    print("Output will be saved in default location.")
  } else if (!file.exists(substr(outfilename,1,(slashindex)-1)) ) {
    print("Path for output file is invalid. Output will be saved in default location.")
  }


  #check user input: determine whether preferences is a file or R list
  if (missing(preferences)){
    print("Optional list of user preferences was not provided. Using defaults.")
    preferences <- defaultprefs
  } else {
    if( is.character(preferences)){
      if (file.exists(preferences)){
        print("Loading user preferences from specified file.")
        print(preferences)
        preftemp <- read.table(preferences, sep=",", header=TRUE)
        rm(preferences)
        #13 elementsfrom file, to become 12 list entries: two elements are placed into a vector
        preferences <- list(as.character(preftemp[1,1]),
                            as.character(preftemp[1,2]),
                            c(preftemp[1,3],preftemp[1,4]),
                            preftemp[1,5],
                            preftemp[1,6],
                            preftemp[1,7],
                            preftemp[1,8],
                            preftemp[1,9],
                            preftemp[1,10],
                            preftemp[1,11],
                            preftemp[1,12],
                            preftemp[1,13])
        #check each item in preferences
        preferences <- checkPrefs(preferences, defaultprefs)
      } else {
        print("Optional list of user preferences is a character variable but not an existing file. Using defaults.")
        preferences <- defaultprefs
      }
    } else {
      if (is.list(preferences)){
        #ensure that the list contains all items
        if(length(preferences)==12)
          #check each item in preferences
          preferences <- checkPrefs(preferences, defaultprefs)
      } else{
        print("Optional user preferences is not an R list object or a string containing a file location. Using defaults.")
        preferences <- defaultprefs
      }
    }

  }

  #read in data
  dframe<-read.table(datafilename, sep=",", header=TRUE)

  #data check: sufficient numer of columns
  if (dim(dframe)[2] < ncolsused){
    stop(paste("Data file does not include all ", ncolsused, " necessary columns. Terminating Program.", sep=""))
  }
  #data check: do columns named by user in preferences exist?
  checkDfCols(c(preferences[[6]], preferences[[7]], preferences[[8]], preferences[[9]], preferences[[10]]), dframe)

  #load libraries if necessary
  lapply(c("ggplot2","grid", "png", "grid", "gridExtra"), require, character.only=T)


  #since the minimum necessary number of columns is present, ensure correct column names
  #colnames(dframe)[1:ncolsused]<- columnnames

  #faster conversion of factors into numeric variables, if necessary
  if (!is.factor(dframe$subj)){
    dframe$subj <- as.factor(dframe$subj)
  }
  if (is.factor(dframe$vtime)){
    dframe$vtime <- as.numeric(levels(dframe$vtime))[dframe$vtime]
  }
  if (is.factor(dframe$x)){
    dframe$vtime <- as.numeric(levels(dframe$x))[dframe$x]
  }
  if (is.factor(dframe$y)){
    dframe$vtime <- as.numeric(levels(dframe$y))[dframe$y]
  }

  #remove data that are extraneous to this analysis
  dframe <- dframe[1:ncolsused]

  # remove duplicates by taking advantage of the fact that repeated samples are consecutive.
  nondup <- ! (dframe$vtime == c(dframe$vtime[-1], dframe$vtime[1]))
  pagesonly <- subset(dframe, nondup)

  # create a task var; replacing leading and trailing junk
  pagesonly$task <-gsub("Assessment/items/", "", pagesonly$page)
  pagesonly$task <-gsub("^([^/]+)/.*$", "\\1", pagesonly$task)

  #cross-tabulation of data
  xtabs(~ task + subj, data=pagesonly)

  #load gazeHeatmap only if it has not yet been loaded, to save time
  if(!exists("gazeHeatmap", mode="function")) source("gazeHeatmap.r")


  # initialize output pdf (standard US paper)
  pdf(outfilename, onefile=TRUE, paper='USr', width=10, height=7.5)

  #check whether cover page has been turned off by user
  if (overPage =="On"){
    #title page with gaze distribution using gazeTimeHist.R
    coverpage <- gazeTimeHist(dframe, preferences[[6]], preferences[[7]], preferences[[8]],
                              MainTitle=outputTitle, preferences[[11]], preferences[[12]])
    print(coverpage)
  }
  # create heatmaps using gazeHeatmap.R
  tasklist <- unique(pagesonly$task)
  for (thistask in tasklist) {
    tmp <- subset(pagesonly, task == thistask)
    taskpage <- gazeHeatmap(tmp, gsub(" ","",thistask), picturelocation, title,
                            preferences[[1]], preferences[[2]], preferences[[3]], preferences[[4]], preferences[[5]])
    print(taskpage)
  }

  #close output pdf
  dev.off()

}

