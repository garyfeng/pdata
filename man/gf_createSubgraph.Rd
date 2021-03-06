% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/gfDiagrammR.R
\name{gf_createSubgraph}
\alias{gf_createSubgraph}
\title{Turn a GraphViz DOT graph into a subgraph}
\usage{
gf_createSubgraph(graph, subGraphName = "")
}
\arguments{
\item{graph}{A DiagrammeR graph object created using the create_graph() method. We need only
the $dot_code portion}

\item{subGraphName}{Optional string for the name of the subgraph; No spaces}
}
\value{
DOT of the graph but packaged as a subgraph
}
\description{
Turn a GraphViz DOT graph into a subgraph
}

