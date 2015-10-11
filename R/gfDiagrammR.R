# a hack from https://github.com/rich-iannone/DiagrammeR/issues/127

#' Turn a GraphViz DOT graph into a subgraph
#'
#' @param graph A DiagrammeR graph object created using the create_graph() method. We need only
#'   the $dot_code portion
#' @param subGraphName Optional string for the name of the subgraph; No spaces
#' @return DOT of the graph but packaged as a subgraph
#'
#' @export
gf_createSubgraph <- function (graph, subGraphName="") {
  gsub("digraph", paste("subgraph", subGraphName, sep=" "), graph$dot_code)
}

#' Great a DOT graph from subgraphs
#'
#' @param ... objects that are subgraphs created using gf_createSubgraph(). This can be individually listed
#'   subgraphs, or a list with subgraphs
#' @return DOT of the graph with all the subgraphs
#'
#' @export

gf_makeGraphFromSubgraphs <- function(...) {
  # deals with both variable arguments and a list of subgraphs
  subs <- unlist(list(...))
  # need to check that they are valid subgraphs
  paste("digraph{", subs, "}", sep = "\n")
}
