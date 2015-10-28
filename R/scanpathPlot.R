# heatmap code adapted from Ningshan Zhang's code 2012
library(ggplot2)
library(png)
library(grid)
library(gridExtra)
library(plyr)
library(fMultivar)


#' This function reads in a picture file specified by the user, then overlays it with a heat map computed according to user
#'   specifications.
#' @param dframe Data frame containing the data to be used, with at least $x, $y, and $dur
#' @param pict String giving the base name of the PNG file to be used, e.g., "background.png"
#' @param path String giving the path of the PNG file to be used, e.g., path=getwd()
#' @param outputtitle Optional argument, allowing the user to specify the output title
#' @param arrows Optional argument, whether to draw arrows or just fixations; default to T
#' @param alphaLevel Optional argument, transparency level, default to 0.4
#' @param pathAlpha Optional argument, transparency level for paths, default to 0.8
#' @param pathSize Optional argument, path size default to 0.7
#' @param sColor Optional argument, starting color, default to "red"
#' @param mColor Optional argument, median color, default to "green"
#' @param eColor Optional argument, end color, default to "blue"
#' @param dx Optional argument, adjustment to x dimension, default to 0px
#' @param dy Optional argument, adjustment to y dimension, default to 60px
#'
#' @return ggplot object: heat map or fixation map of gaze time overlayed onto a picture read from file
#'
#' @examples
#' scanpathPlot(tmp, paste(storyName, ".png", sep=""), path=getwd(),
#'     alphaLevel = alphaLevel)
#' @export

# see http://stackoverflow.com/questions/5106782/use-of-ggplot-within-another-function-in-r
# for why I can't use a subset of dframe for ggplot in a function
scanpathPlot <- function (dframe, pict, path='.', outputtitle, dx=0, dy=60, alphaLevel = 0.4,
                          arrows=T, pathAlpha=0.8, pathSize=0.7,
                          sColor="red", mColor="green", eColor="blue",
                          x0=-1, y0=-1, x1=-1, y1=-1)
{
  if (missing(outputtitle)) {
    # if no title,
    outputtitle = pict
  }
  # find out whether pict has extension
  pictlen = nchar(pict)
  if (tolower(substr(pict, pictlen-3, pictlen))==".png") {
    # pict is
  } else {
    pict <- paste(pict, ".png", sep="")
  }
  # now we need to add path
  if (missing(path)) {
    # if path is not specified
    imageFileName <- pict
  } else {
    imageFileName <- paste(path, "/", pict, sep="")
  }

  trackdata <- data.frame(dframe$x+dx, dframe$y+dy, dframe$t)
  #print(trackdata)
  #original dimensions saved b/c they're used often below
  nrows <-nrow(trackdata)
  if (nrows<1) {
    print("No data in the dataframe; check the input")
    #return (-1)
    return (NULL)
  }
  #create time spent variable
  #timespent <-c(min(trackdata[,3]),trackdata[2:nrows,3]-trackdata[1:(nrows-1),3] )
  timespent <-c(0,trackdata[2:nrows,3]-trackdata[1:(nrows-1),3] )
  trackdata[,4] <- timespent
  #add propotion of time spent (plus one to make sure all points are represented)
  #trackdata[,5] <- round(timespent/max(timespent)*10000)+1
  trackdata[,5] <- round(timespent/100)+1

  colnames(trackdata) <-c("xcoords", "ycoords", "Time", "Time_Spent", "Tspent_scaled")
  #trackdata$Tspent_scaled[trackdata$Tspent_scaled<4] <-1
  trackdata$Tspent_scaled[trackdata$Tspent_scaled>5] <-5

  #original dimensions saved b/c they're used often below (updated because of timespent addtion)
  nrows <-nrow(trackdata)
  ncols <-ncol(trackdata)

  #order by x-axis (here variable 1)
  trackdata2 <-trackdata[ order(trackdata[,3], trackdata[,3]), ]
  #replace row numbers (original row numbers remain after sorting)
  rownames(trackdata2)<-1:nrows

  #create additional variable with color
  #trackdata2[,(ncols+1)] <-colorRampPalette(c(sColor,mColor, eColor))(nrows)
  trackdata2[,(ncols+1)] <-colorRampPalette(rainbow(10))(nrows)
  #trackdata2[,(ncols+1)] <-colorRampPalette(c("#222222", "#AAAAAA"))(nrows)
  colnames(trackdata2)[(ncols+1)] <- "colorcode"

  # print (imageFileName)

  #########################
  # Read the image and convert if necessary
  #########################
  # skip if no image file exists
  if (!file.exists(imageFileName)) {
    print(paste("Image file ", imageFileName, "does not exist. Skipping..."))
    #return (-1)
    return (NULL)
  }
  # read in the image
  m <- readPNG(imageFileName, native = F)
  # native=TRUE to allow for grayscale images; but data returned is not [0,1]
  # so disabled for now.
  if (length(dim(m))==2) {
    # grayscale
    g <- matrix(rgb(m, m, m, alphaLevel), nrow=dim(m)[1])
  } else if(dim(m)[3]==4){
    g <- matrix(rgb(m[,,1],m[,,2],m[,,3],m[,,4]*alphaLevel), nrow=dim(m)[1])
  } else if (dim(m)[3]==3){
    g <- matrix(rgb(m[,,1],m[,,2],m[,,3], alphaLevel), nrow=dim(m)[1])
  } else if (dim(m)[3]==2){
    g <- matrix(rgb(m[,,1],m[,,1],m[,,1], alphaLevel), nrow=dim(m)[1])
  }

  # let's read the image size
  screenSize = c(dim(m)[2], dim(m)[1])
  x0<-ifelse(x0<0, 0, ifelse(x0>screenSize[1], screenSize[1], x0))
  y0<-ifelse(y0<0, 0, ifelse(y0>screenSize[2], screenSize[2], y0))
  x1<-ifelse(x1<0, screenSize[1], ifelse(x1>screenSize[1], screenSize[1], x1))
  y1<-ifelse(y1<0, screenSize[2], ifelse(y1>screenSize[2], screenSize[2], y1))
  # sub image
  g<-g[y0:y1, x0:x1]

  ################
  # make scanpath plot
  ################

  myplot <- ggplot(trackdata2, aes(x=xcoords, y=screenSize[2]-ycoords)) +
    xlim(c(x0, x1))+ylim(c(y0, y1))
  #   myplot <- myplot + geom_point(aes(size=Tspent_scaled, color=Time)) +
  #     scale_colour_gradient(low=sColor, high=eColor)
  if (!arrows) {myplot <- myplot + geom_point(aes(size=Tspent_scaled, colour=colorcode))}
  myplot <- myplot + geom_path(colour=trackdata2$colorcode, alpha=pathAlpha, size=pathSize, arrow = arrow(type="closed",angle = 15,length = unit(trackdata2$Tspent_scaled/10, "inches")))

  q <- myplot +  annotation_raster(g, xmin=x0, xmax=x1, ymin=y0, ymax=y1, interpolate = TRUE)+
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

  return (q)
}
