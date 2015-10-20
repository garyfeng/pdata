require(knitr)

#' Render a list of htmlWidgets using various tricks
#'
#' @param widgetList A list of htmlWidget objects to be rendered
#' @param renderFunction The function to render individual widgets. It can be either a name
#'   of the rendering function, e.g., "render_graph" in DiagrammeR, or the actual function to
#'   be passed to this call.
#' @return The knitted string. This is to be included in the output by using `r renderHtmlWidgetList(...)`;
#' @details This is a collection of various tricks. See the URL citations in the code.
#'   Note that this code does alliterate global variables starting with "renderHtmlWidgetList_".
#'   You may want to delete them using rm(list = ls(pattern="renderHtmlWidgetList_*")).
#' @examples Inlcude the following in the Rmd directly
#'   `r require(DiagrammeR); renderHtmlWidgetList(grfDf$graph, render_graph)`
#'
#' @export

renderHtmlWidgetList <- function(widgetList, renderFunction){
  # error checking
  stopifnot(is.list(widgetList))
  # handles if the renderFunction is actually a function
  # http://stackoverflow.com/questions/10520772/in-r-how-to-get-an-objects-name-after-it-is-sent-to-a-function
  if(is.function(renderFunction)) {
    # convert back to string, because we need to knit it later
    renderFunction <- deparse(substitute(renderFunction))
  }
  stopifnot(is.character(renderFunction) & length(renderFunction)==1)
  stopifnot(exists(renderFunction, mode = "function"))
  # inject global vars; make sure we have a unique global var name
  gVarName<- paste0("renderHtmlWidgetList_", sample(1:10000, 1))
  while (exists(gVarName)) {
    gVarName<- paste0("renderHtmlWidgetList_", sample(1:10000, 1))
  }
  # assigning widgetList to a global temp var
  # http://stackoverflow.com/questions/5510966/create-a-variable-name-with-paste-in-r
  assign(gVarName, widgetList, envir = .GlobalEnv)
  # solution from https://gist.github.com/ReportMort/9ccb544a337fd1778179
  out <- NULL
  knitPrefix1 <- "\n```{r "
  knitPrefix2 <- "results='asis', cache=FALSE, echo=FALSE}\n\n"
  knitSuffix <- "\n\n```"
  for (i in 1:length(widgetList)) {
    # adding unique chunk name
    knit_expanded <- paste0(knitPrefix1, gVarName, knitPrefix2, renderFunction, "(", gVarName, "[[", i, "]])")
    out = c(out, knit_expanded)
  }
  #invisible(out)
  paste(knitr::knit(text = out), collapse = '\n')
}
