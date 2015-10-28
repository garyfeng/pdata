#' Function that checks user preferences for heatmapanalysis.R, and, where necessary, overwrites with default preferences and displays
#' a message to the user detailing the problematic preference specification and the default value used.
#' Problems with incorrectly specified variable names lead to the program being terminated rather than the use of defaults.
#'
#' @param prefs A list of preferences for use in heatmapanalysis.R
#' @param dprefs A list of defaults for the preferences for use in heatmapanalysis.R
#' @return prefs A list of preferences for use in heatmapanalysis.R in which the entries have been check and, where neede, replaced
#'           with default values
#
#Florian Lorenz, 2014

checkPrefs <- function(prefs, dprefs){

  #check user input
  if (!is.list(prefs)){
    stop("checkPrefs: Input argument prefs is not, but must be a list. Terminating program.")
  }
  if (!is.list(dprefs)){
    stop("checkPrefs: Input argument dprefs is not, but must be a list. Terminating program.")
  }
  if (length(prefs) != 12){
    stop("checkPrefs: Input argument prefs is not, but must contain twelve items. Terminating program.")
  }
  if (length(dprefs) != 12){
    stop("checkPrefs: Input argument dprefs is not, but must contain twelve items. Terminating program.")
  }


  if( "try-error"%in%class(try(col2rgb(prefs[[1]]),silent=TRUE)) ){
    print("First item in preference list, heat map MinColor, is not a string containing a valid color. Using default.")
    prefs[[1]] <- as.character(dprefs[1])
  }
  if( "try-error"%in%class(try(col2rgb(prefs[[2]]),silent=TRUE)) ){
    print("Second item in preference list, heat map MaxColor, is not a string containing a valid color. Using default.")
    prefs[[2]] <- as.character(dprefs[2])
  }
  if( length(prefs[[3]]) != 2 | !is.numeric(prefs[[3]][1]) | !is.numeric(prefs[[3]][2]) ){
    print("Third item in preference list c(HscreenDim,VscreenDim): screen size, is not vector containing a numeric values. Using default.")
    prefs[[3]] <- as.character(dprefs[3])
  }
  if( !is.double(prefs[[4]]) | prefs[[4]] > 1 | prefs[[4]] < 0 | length(as.vector(prefs[[4]])) != 1 ){
    print("Fourth item in preference list, heat map Transparancy, is not numeric or outside the valid range [0;1]. Using default.")
    prefs[[4]] <- as.character(dprefs[4])
  }
  if( !is.numeric(prefs[[5]]) | prefs[[5]] > 1000 | prefs[[5]] < 10 | length(as.vector(prefs[[5]])) != 1 | prefs[[5]]%%1 > 0){
    print("Fifth item in preference list, heat map Resolution, is not an integer or outside the valid range [10:1000]. Using default.")
    prefs[[5]] <- as.character(dprefs[5])
  }
  varnameprefs <- c("SubjectVariableName", "TimeVariableName", "TaskVariableName","XcoordVariableName","YcoordVariableName")
  for(k in 6:10){
    if( prefs[[k]] != make.names(prefs[[k]]) ){
      errmess <- paste("Preference item ", as.character(k), ", the variable containing ",
                       varnameprefs[(k-5)] , " is not a valid variable name.", sep="")
      stop(errmess)
    }
  }
  if( !is.character(prefs[[11]]) ){
    print("prefs item 11, SubjectVariableLable, is not a string. Using default: Subject ID")
    prefs[[11]] <- as.character(dprefs[11])
  }
  if( !is.character(prefs[[12]]) ){
    print("prefs item 12, TaskVariableLabel, is not a string. Using default: Task")
    prefs[[12]] <- as.character(dprefs[12])
  }

  return(prefs)
}
