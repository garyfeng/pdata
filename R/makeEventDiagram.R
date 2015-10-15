#' Generating a graph based on an input vector of states
#'
#' @return a DiagrammeR graph object
#'
#' @param evt A vector of events, which can be either numeric, char, or factor. The unique
#'    values of the vector will become the nodes (unless excluded). Edges will be draw from
#'    the current event to the next event (except those that are excluded).
#' @param excludeEdges A logical vector the same longth as the events. If True, the edge
#'    associated with corresponding event will not be drawn. NA by default.
#' @param excludedNodes A vector of names of the nodes to be excluded.
#' @param subGraphName The Name of the graph. This string will be printed at the top of the
#'    output graph.
#' @param ignoreRuns FALSE by default. If TRUE, run-length-encoding will be done on the evt
#'    vector, so that there are no self-referencial edges.
#' @param minimalEdgeCount The minimal number of transition occurances before an edge is drawn.
#'
#' @export
makeEventDiagram <- function(evt, excludeEdges=NA,
                             subGraphName="",
                             ignoreRuns = F,
                             excludeNodes = c(NA, "NA"),
                             minimalEdgeCount = 0) {
  # need to convert factors to char
  evt<-as.character(evt)
  # make sure the vectors are the same size
  if (length(excludeEdges)==1) stopifnot(length(excludeEdges) == length(evt))
  # reduce the runs to avoid too much self-referencials.
  if (ignoreRuns) {
    evt <- rle(evt)$values
    evtRunCounts <-rle(evt)$lengths
  }

  # nodes; exclude NAs
  nodes <- setdiff(unique(evt), excludeNodes)
  nodes <- create_nodes(nodes = nodes, type = "event", shape = "rectangle")
  stopifnot(nrow(nodes)>0)

  # to create edges, we need to aggregate and count unique edges
  dfEvt <-data.frame(from=evt, to=lead(evt))
  # delete edges crossing run boundaries, if the if (is.na(excludeEdges))
  if (length(excludeEdges)>1) {
    dfEvt <-data.frame(from=dfEvt$from[!excludeEdges], to=dfEvt$to[!excludeEdges])
  }
  # aggregate and create
  dfEvt <- dfEvt %>% group_by(from, to) %>% summarise(n=n())
  # filter by minimalEdgeCount
  dfEvt <- dfEvt %>% filter(n>minimalEdgeCount)
  # filter out any NA cases
  dfEvt <- dfEvt[complete.cases(dfEvt),]
  # create edges
  edges <- create_edges(from = dfEvt$from,
                        to = dfEvt$to,
                        relationship = "goto",
                        data=dfEvt$n
  )
  # Use the 'scale_edges' function to create an attribute
  # column that's scaled to data in another column
  edges <- scale_edges(edges_df = edges,
                       to_scale = edges$data,
                       edge_attr = "penwidth",
                       range = c(1, 10))

  #If no edges, then don't plot
  if (nrow(edges)>0) {
    gr <- create_graph(nodes_df = nodes,
                       edges_df = edges,
                       #graph_attrs = "layout = neato",
                       graph_name = subGraphName,
                       node_attrs = c("fontname = Helvetica",
                                      "style = filled",
                                      "fixedsize = true",
                                      "width = 3"),
                       edge_attrs = c("color = gray20",
                                      "arrowsize = 0.5"))
  } else {
    gr <- create_graph(nodes_df = nodes,
                       #edges_df = edges,
                       #graph_attrs = "layout = neato",
                       graph_name = subGraphName,
                       node_attrs = c("fontname = Helvetica",
                                      "style = filled",
                                      "fixedsize = true",
                                      "width = 3"),
                       edge_attrs = c("color = gray20",
                                      "arrowsize = 0.5"))

  }

  # adding graph title; not a function; must do manually
  gr$dot_code <- gsub("digraph [{]",
                      paste("digraph {\ngraph [label='", subGraphName, "', labelloc=t, fontsize=30];\n", sep=" "),
                      gr$dot_code)

  # return as a subgraph
  # gf_createSubgraph(gr, subGraphName)
  # return the graph object
  invisible(gr)
}
