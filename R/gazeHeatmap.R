require("plyr")
require("png")
require ("ggplot2")


#' This function reads in a picture file specified by the user, then overlays it with a heat map computed according to user
#'   specifications.
#' @param dframe Data frame containing the data to be used, with at least $x, $y, and $dur
#' @param pict String giving the base name of the PNG file to be used, e.g., "background.png"
#' @param path String giving the path of the PNG file to be used, e.g., path=getwd()
#' @param outputtitle Optional argument, allowing the user to specify the output title
#' @param colormin Optional argument, allowing the user to specify the color corresponding to the lowest value of the heat map
#' @param colormax Optional argument, allowing the user to specify the color corresponding to the highest value of the heat map
#' @param screendim Optional argument, allowing the user to specify the dimensions of the screen that was viewed
#' @param transparancyLevel Optional argument, allowing the user to set the transparancy level of the heat map
#' @param ngridpoints Optional argument, allowing the user to set the resolution of the heat map
#' @param showFixations Optional argument, to show individual fixations if TRUE; default to FALSE for heat map
#'
#' @return ggplot object: heat map or fixation map of gaze time overlayed onto a picture read from file
#'
#' @examples
#' gazeHeatmap(tmp, paste(storyName, " Summary", sep=""), path=getwd(),
#'     screendim = screenSize, transparancyLevel = alphaLevel, ngridpoints = 200,
#'     showFixations = F)
#' @export

#
#Input Arugments:
#----------------
#dframe:            data frame containing the data to be used
#pict:              string giving the name of the picture file to be used
#path:          string giving the path of the picture file to be used
#outputtitle:       optional argument, allowing the user to specify the output title
#colormin:          optional argument, allowing the user to specify the color corresponding to the lowest value of the heat map
#colormax:          optional argument, allowing the user to specify the color corresponding to the highest value of the heat map
#screendim:         optional argument, allowing the user to specify the dimensions of the screen that was viewed
#transparancyLevel: optional argument, allowing the user to set the transparancy level of the heat map
#ngridpoints:       optional argument, allowing the user to set the resolution of the heat map
#
#Ouput:
#------
#ggplot object: heat map of gaze time overlayed onto a picture read from file
#
#Florian Lorenz, 2014 (with code by Gary Feng and Nigshan Zhang)



gazeHeatmap <- function (dframe, pict, path, outputtitle="User Output", colormin="white", colormax="red4",
                         screendim=c(1680, 1050), transparancyLevel=0.40, ngridpoints=100,
                         showFixations = F)
{

  #may be needed due to ggplot2 bugs
  #colormin <<- colormin
  #colormax <<- colormax

  # note: heatmap code adapted from Ningshan Zhang's code (2012)
  #load libraries if necessary
  lapply(c("ggplot2","grid", "png", "gridExtra"), require, character.only=T)


  #check user input
  if( missing(dframe) ){
    stop("Required data frame is missing. Terminating program.")
  }
  if( missing(pict) | !is.character(pict) ){
    stop("Required picture file name is missing or not a string. Terminating program.")
  }
  if( missing(path) | !is.character(path) ){
    stop("Required picture file path is missing or not a string. Terminating program.")
  }
  if( !is.character(outputtitle) ){
    print("Optional output title is not a string. Using default: UserOutput.")
    outputtitle <- "UserOutput"
  }
  if( !is.character(colormin) ){
    print("Optional minimum value color for heat map is not a string. Using default: white.")
    colormin <- "white"
  }
  if( !is.character(colormax) ){
    print("Optional maximum value color for heat map is not a string. Using default: dark red.")
    colormin <- "red4"
  }
  if ( !is.numeric(screendim) ){
    print("Optional screen size vector not numeric. Using default: c(1680, 1050).")
    screendim <- c(1680, 1050)
  }
  if ( length(screendim) != 2 ){
    print("Optional screen size vector does not have two dimensions. Using default: c(1680, 1050).")
    screendim <- c(1680, 1050)
  }
  if( !is.double(transparancyLevel) |
      as.logical(ncol(as.matrix(transparancyLevel)) > 1) | as.logical(nrow(as.matrix(transparancyLevel)) > 1) |
      transparancyLevel > 1         | transparancyLevel < 0 ){
    print("Optional transparancy level scalar is not a scalar, not numeric or out of bounds [0.0,1.0]. Using default: 0.40.")
    transparancyLevel <- 0.40
  }
  if ( length(ngridpoints) != 1 ){
    print("Optional screen size vector does not have one dimension. Using default: 100.")
    ngridpoints <- 100
  }
  if ( !is.numeric(ngridpoints) ){
    print("Optional output resultion argument number of grid points is not numeric. Using default: 100.")
    ngridpoints <- 100
  }

  #create complete file name
  imageFileName <- paste(path, "/", pict, ".png", sep="")
  # garyfeng: changed to read from the 'img' directory of the package, see below
  imageFileName <- paste( pict, ".png", sep="")
  # garyfeng: now get the full path to the img file under the library
  #imageFileName <-system.file("img", imageFileName, package="GFEye")
  print (imageFileName)

  # skip if no image file exists
  if (!file.exists(imageFileName)) {
    print("Image file does not exist. Skipping...")
    return (-1)
  }

  # read in the image
  m <- readPNG(imageFileName,native = F)
  imgsize <- dim(m)


  #check image dimensions against user-suplied dimensions
  if (imgsize[1] != screendim[2]  | imgsize[2] != screendim[1]){
    print("WARNING: Size of image differs from user-specified size (or default, if that was used). ")
    screendim <- c(imgsize[2], imgsize[1])
    #print("WARNING: Size of image differs from user-specified size (or default, if that was used). Skipping...")
    #return(-1)

  }

  # native=TRUE to allow for grayscale images; but data returned is not [0,1]
  # so disabled for now.
  if (length(dim(m))==2) {
    # grayscale
    g <- matrix(rgb(m, m, m, transparancyLevel), nrow=dim(m)[1])
  } else if (dim(m)[3]==3){
    g <- matrix(rgb(m[,,1],m[,,2],m[,,3], transparancyLevel), nrow=dim(m)[1])
  } else if(dim(m)[3]==4){
    g <- matrix(rgb(m[,,1],m[,,2],m[,,3],m[,,4]*transparancyLevel), nrow=dim(m)[1])
  }

  #ineligant workaround to deal with ggplot bug: aes (only) cannot find global or program-scope screendim
  dframe$screendim1 <- screendim[1]
  dframe$screendim2 <- screendim[2]

  nsample<-nrow(dframe)
  # print(nrow(dframe))

  # color pallette
  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  # use n=200 for better effect but slow
  if (showFixations) {
    p <- ggplot(dframe, aes(x, screendim2[2]-y)) +
      xlim(c(0, screendim[1]))+ylim(c(0,screendim[2]))+
      geom_point(aes(colour = ROI, alpha=0.1, size=sqrt(sqrt(dur)))) +
      scale_size(range = c(2, 6))+
      ggtitle(paste(outputtitle,"   Task = '", pict,"'   #Sample = ", nsample, sep="")) +
      coord_fixed()

  } else {
    p <- ggplot(dframe, aes(x, screendim2[2]-y)) +
      xlim(c(0, screendim[1]))+ylim(c(0,screendim[2]))+
      stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE, n=ngridpoints, h = c(50,30)) +
      scale_fill_gradientn(colours = jet.colors(10), trans="sqrt") +
      ggtitle(paste(outputtitle,"   Task = '", pict,"'   #Sample = ", nsample, sep="")) +
      coord_fixed()
  }

  q <- p +  annotation_raster(g, xmin=0, xmax=screendim[1], ymin=0, ymax=screendim[2]+0, interpolate = TRUE)+
    theme(plot.title=element_text(size=rel(1)),
          axis.line=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())

  #print(q)

  return(q)
}
