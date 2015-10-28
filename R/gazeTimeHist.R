#' plot distribution of gaze time, in seconds (assuming input in ms), by subject and by task
#'    user can specify input variables subject, time variable, task variable as well as title, subject variable lable and task variable label
#'
#' @param df1 Input data frame that contains the variables specified by the arguments below, so at the very least three variables: a
#'    variable with a subject ID, a variable with the gaze time to be plotted, and variable with a Task ID
#' @param SubVar The name of the variable in df1 that contains the subject ID
#' @param TimeVar The name of the variable in df1 that contains the gaze time
#' @param TaskVar The name of the varaible in df1 that containts the task variable
#' @param MainTitle Optional argument, allows the user to set the title printed above the histogram
#' @param SubVarLabel Optional argument, allows the user to set the label of the subject variable (visible in plot legend)
#' @param TaskLabel Optinoal argument, allows the user to set the label of the Task variable (X-axis label)
#' @return ggplot object: histogram of gaze time, by subject and task
#'
#' @examples
#'   gazeTimeHist (df1, SubVar, TimeVar, TaskVar,
#'     MainTitle="User Output", SubVarLabel="Subject ID", TaskLabel="Task")
#'
#' @export

#
#Florian Lorenz, 2014

gazeTimeHist <- function (df1, SubVar, TimeVar, TaskVar, MainTitle="User Output", SubVarLabel="Subject ID", TaskLabel="Task"){


  #Check user input: data set
  if( missing(df1) ){
    stop("gazeTimeHist: Required argument data frame is missing. Terminating program.")
  } else if(!is.data.frame(df1)){
    stop("gazeTimeHist: Required argument data frame points to object that is not a data frame. Terminating program.")
  }
  #Check user input: subject variable name
  if( missing(SubVar) | !is.character(SubVar)){
    stop("gazeTimeHist: Required argument subject variable name is missing or is not a string. Terminating program.")
  }
  #Check user input: time variable name
  if( missing(TimeVar) | !is.character(TimeVar)){
    stop("gazeTimeHist: Required argument time variable name is missing or is not a string. Terminating program.")
  }
  #Check user input: task variable name, e.g. AOI_ID
  if( missing(TaskVar) | !is.character(TaskVar)){
    stop("gazeTimeHist: Required argument task varaible name is missing or is not a string. Terminating program.")
  }
  #Check user input: main title
  if( !is.character(MainTitle)){
    print("gazeTimeHist: Optional argument main title is not a string. Using default.")
    MainTitle <-"User Output"
  }
  #Check user input: subject variable label
  if( !is.character(SubVarLabel)){
    print("gazeTimeHist: Optional argument subject variable label is not a string. Using default.")
    SubVarLabel <- "Subject ID"
  }
  #Check user input: main title
  if( !is.character(TaskLabel)){
    print("gazeTimeHist: Optional argument task variable label is not a string. Using default.")
    TaskLabel <- "Task"
  }


  #create a new data frame with only the variables needed
  eval(parse(text=paste("df2 <- as.data.frame(md$", SubVar,")",sep="")))
  colnames(df2)[1] <- "subjectID"
  eval(parse(text=paste("df2$gazeTime <- md$", TimeVar ,sep="")))
  eval(parse(text=paste("df2$taskName <- md$", TaskVar ,sep="")))

  #load ggplot, if needed
  require(ggplot2)

  p <- ggplot( df2, aes( taskName, (gazeTime/1000), fill=subjectID ) )+
    geom_bar(stat="identity", position="dodge")+
    ggtitle(paste(MainTitle,"\n Time Spent by Task",sep=""))+
    scale_y_continuous(name="Gaze Time in s")+
    scale_x_discrete(name=TaskLabel)+
    scale_fill_discrete(name=SubVarLabel)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  return(p)
}
