# devtools::install_github('garyfeng/DiagrammeR')
# require(DiagrammeR)

#' Generating a graph based on an input vector of states
#'
#' @return a DiagrammeR graph object
#'
#' @param evt A vector of events, which can be either numeric, char, or factor. The unique
#'    values of the vector will become the nodes (unless excluded). Edges will be draw from
#'    the current event to the next event (except those that are excluded).
#' @param nodeShapeBy A vector of distinct values, of the same length as evt, such that
#'    event nodes with the same value of nodeShapeBy will have the same shape.
#' @param shapes A character vector of graphviz node shapes. The list will be repeated
#'    if the number of nodes is larger than the length of the shapes vector.
#' @param excludeEdges A logical vector the same longth as the events. If True, the edge
#'    associated with corresponding event will not be drawn. NA by default.
#' @param excludedNodes A vector of names of the nodes to be excluded.
#' @param graphName The Name of the graph. This string will be printed at the top of the
#'    output graph.
#' @param ignoreRuns FALSE by default. If TRUE, run-length-encoding will be done on the evt
#'    vector, so that there are no self-referencial edges.
#' @param minimalEdgeCount The minimal number of transition occurances before an edge is drawn.
#' @param deleteIsolatedNodes Do not show isolated nodes (default = T)
#'
#' @examples
#' df <- data.frame(evt=c("this", "2", "hi", "that", "2", "3"), by=c(1,2, 1, 1, 2, 2))
#' makeEventDiagram(df$evt)
#' g<-makeEventDiagram(df$evt, nodeShapeBy=df$by); print(g)
#'
#' @export

makeEventDiagram <- function(evt,
                             nodeShapeBy = NA,
                             graphName="",
                             ignoreRuns = F,
                             excludeEdges=NA,
                             excludeNodes = c(NA, "NA"),
                             minimalEdgeCount = 0,
                             shapes = c( "rectangle", "oval", "diamond", "egg", "triangle",
                                         "parallelogram", "house", "pentagon", "hexagon", "septagon",
                                         "octagon", "doubleoctagon", "cds"),
                             deleteIsolatedNodes = T
                            ) {
  # need to convert factors to char
  evt<-as.character(evt)
  # make sure the vectors are the same size
  if (length(excludeEdges)>1) stopifnot(length(excludeEdges) == length(evt))
  # reduce the runs to avoid too much self-referencials.
  if (ignoreRuns) {
    evt <- rle(evt)$values
    evtRunCounts <-rle(evt)$lengths
    # this will break the excludeEndges, so let's get the corresponding
    # elements in the excludeEdges assuming we don't exclude edges in the runs
    if(length(excludeEdges)>1) excludeEdges <- excludeEdges[cumsum(evtRunCounts)]
    if(length(nodeShapeBy)>1) {
      nodeShapeBy<-nodeShapeBy[cumsum(evtRunCounts)]
    }
  }

  # nodes; exclude NAs
  nodeNames <- setdiff(unique(evt), excludeNodes)
  # nodeShapeValue is either a vector of shapes or a char as the default
  nodeShapeValue <- "rectangle"  # default
  nodeTooltip <- nodeNames
  # change shape if necessary
  if(length(nodeShapeBy)>1 ) {
    # remove NAs
    nodeShapeBy[is.na(nodeShapeBy)] <-"NA"
    # repeat the shapes if not enough
    nShapesNeeded <- length(unique(nodeShapeBy))
    if(length(shapes) < nShapesNeeded) {
      shapes <- rep_len(shapes, nShapesNeeded)
    }
    # get shapes; result should be the same length as nodeNames
    # so we find the unique
    df <- unique(data.frame(evt=evt, by=nodeShapeBy)) %>%
      filter(evt %in% nodeNames)
    # test if a event falls under multiple "by" categories. Stop if true.
    if (length(df$evt) > length(nodeNames))
      stop("Some events occur under different values of the nodeShapeBy variable.\n
           Do a crosstab to debug the problem.")
    nodeShapeValue <- shapes[as.numeric(as.factor(as.character(df$by)))]
    nodeTooltip <- as.character(df$by)
  }
  nodes <- create_nodes(nodes = nodeNames, type = "event",
                        tooltip = nodeTooltip,
                        shape = nodeShapeValue)
  stopifnot(nrow(nodes)>0)

#   filter <- which(nodes$nodes %in% names(nodeShapeBy))
#   nodes$shape[filter] <- nodeShapeBy[[nodes$nodes[filter]]]

  # to create edges, we need to aggregate and count unique edges
  dfEvt <-data.frame(from=evt, to=c(evt[-1], NA))
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
                       graph_name = graphName,
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
                       graph_name = graphName,
                       node_attrs = c("fontname = Helvetica",
                                      "style = filled",
                                      "fixedsize = true",
                                      "width = 3"),
                       edge_attrs = c("color = gray20",
                                      "arrowsize = 0.5"))

  }

  # delete isolated nodes if deleteIsolatedNodes=T
  if (deleteIsolatedNodes) {
    isoNodes <- node_info(gr)[which(node_info(gr)["degree"] == 0), ][, 1]
    for (node in isoNodes) {
      gr <- delete_node(gr, node = node)
    }
  }

  # adding graph title; not a function; must do manually
  gr$dot_code <- gsub("digraph [{]",
                      paste("digraph {\ngraph [label='", graphName, "', labelloc=t, fontsize=30];\n", sep=" "),
                      gr$dot_code)

  # return as a subgraph
  # gf_createSubgraph(gr, graphName)
  # return the graph object
  invisible(gr)
}


# for some reason the scale_edges function is missing. Here it is
# from https://codecov.io/github/rich-iannone/DiagrammeR/R/scale_edges.R

#' Create numerical and color scales for edge attributes
#' @description Generates either numeric or color scales for specified edge attributes and applies those scales to edge data frames.
#' @param edges_df a data frame containing, at minimum, a column (called \code{edge_op}) with edge operations as character strings (in the form of \code{[node_id] -> [node_id]}). Alternatively, there may be two columns where node IDs specifying edges are provided.
#' @param to_scale a vector of numerical values to be scaled; these currently need to be of the same length and order as the edge operations in the supplied edge data frame, so, it's recommended to reference a column of values available in \code{edges_df}.
#' @param edge_attr the name of the edge attribute for which scaled values are to be created.
#' @param range a vector of 2 elements providing either lower and upper numerical or X11 color values.
#' @param scale_type the type of scaling to perform. Currently, \code{linear} is the only option available.
#' @return an edge data frame.
#' @examples
#' \dontrun{
#' # Add an edge attribute which has values scaled to
#' # numeric data in another column
#' edges <- create_edges(from = c("a", "b", "c"),
#'                       to = c("d", "d", "a"),
#'                       label = '',
#'                       relationship = "given_to",
#'                       data = sample(seq(1:50), 10))
#'
#' edges <- scale_edges(edges_df = edges,
#'                      to_scale = edges$data,
#'                      edge_attr = "penwidth",
#'                      range = c(1, 5))
#' }
#' @export scale_edges

scale_edges <- function(edges_df,
                        to_scale,
                        edge_attr,
                        range,
                        scale_type = "linear"){

  # Create vector of edge attributes that take numeric values
  numeric_edge_attributes <-
    c("arrowsize", "fontsize", "labelangle", "labeldistance", "labelfontsize",
      "minlen", "penwidth", "weight")

  # Create vector of edge attributes that take color values
  color_edge_attributes <- c("color", "fontcolor", "labelfontcolor")

  # Determine whether there is a valid numeric edge attribute
  # in the statement
  is_num_edge_attribute <- edge_attr %in% numeric_edge_attributes

  # Determine whether there is a valid color edge attribute
  # in the statement
  is_col_edge_attribute <- edge_attr %in% color_edge_attributes

  # Determine whether the attribute is an alpha attribute
  is_alpha_edge_attribute <- ifelse(grepl("alpha.*", edge_attr), TRUE, FALSE)

  if (is_num_edge_attribute){

    # Stop function if the length of the numeric vector is not 2
    stopifnot(length(range) == 2)

    # Obtain the min and max values for the data to normalize
    num_range_min_max <- c(min(to_scale), max(to_scale))

    # Get normalized values for attribute if "linear" option chosen
    if (scale_type == "linear"){

      normalized <- range[1] +
        ((to_scale - num_range_min_max[1]) *
           (range[2] - range[1])) /
        (num_range_min_max[2] - num_range_min_max[1])
    }

    # Create data frame for merging
    normalized_df <- data.frame(normalized)

    # Column bind both data frames together
    edges_df <- cbind(edges_df, normalized_df)

    # Change temporary name of column to that of attribute chosen
    colnames(edges_df)[which(names(edges_df) %in% "normalized")] <- edge_attr

    return(edges_df)
  }

  if (is_col_edge_attribute){

    # Stop function if the length of the character vector is not 2
    stopifnot(length(range) == 2)

    # If the colors are named colors, then transform to hex
    if (all(range %in% x11_hex()[,1])){

      for (i in 1:length(range)){

        if (i == 1) hex_color_values <- vector(mode = 'character', length = 0)

        a_hex_color <- x11_hex()[which(x11_hex()[,1] %in% range[i]),2]

        hex_color_values <- c(hex_color_values, a_hex_color)
      }
    }

    # Obtain the min and max values for the data to normalize
    num_range_min_max <- c(min(to_scale), max(to_scale))

    # Obtain 100 colors within the color range provided
    number_of_stops <- 100

    for (j in 1:number_of_stops){

      if (j == 1) hex_colors <- vector(mode = "character", length = 0)

      js_call <- paste0("chromato.interpolate('", hex_color_values[1], "', '",
                        hex_color_values[2], "', ",
                        j/number_of_stops, ", 'hsl');")

      ct <- V8::new_context("window")
      invisible(ct$source(system.file("htmlwidgets/lib/chromatography/chromatography.js",
                                      package = "DiagrammeR")))

      hex_colors <- c(hex_colors, unlist(strsplit(ct$eval(js_call), ",")))

      if (j == number_of_stops){
        fractional_hex_colors <-
          rbind(data.frame(fraction = 0.0, hex_colors = hex_colors[1],
                           stringsAsFactors = FALSE),
                data.frame(fraction = seq(from = 1, to = 100, by = 1)/100,
                           hex_colors = hex_colors, stringsAsFactors = FALSE))
      }
    }

    # Get normalized values for attribute
    for (k in 1:length(to_scale)){
      if (k == 1) normalized <- vector(mode = 'character', length = 0)

      a_hex_color <-
        fractional_hex_colors[which(fractional_hex_colors[,1] ==
                                      round((to_scale - min(to_scale))/
                                              (max(to_scale) - min(to_scale)),
                                            digits = 2)[k]), 2]

      normalized <- c(normalized, a_hex_color)
    }

    # Create data frame for merging
    normalized_df <- data.frame(normalized)

    # Column bind both data frames together
    edges_df <- cbind(edges_df, normalized_df)

    # Change temporary name of column to that of attribute chosen
    colnames(edges_df)[which(names(edges_df) %in% "normalized")] <- edge_attr

    return(edges_df)
  }

  if (is_alpha_edge_attribute){

    # Parse statement and determine if the alpha values should be applied to
    # a color attribute, or, whether a new color attribute should be created
    # before applying that alpha

    if (grepl(":", edge_attr)){
      additional_stmt <- unlist(strsplit(edge_attr, ":"))[-1]
    }

    if (exists("additional_stmt") & grepl("=", additional_stmt)){
      attr_assign <- gsub(" ", "", unlist(strsplit(additional_stmt, "=")))
    }

    if (exists("additional_stmt") & grepl("=", additional_stmt) == FALSE){
      attr_assign <- gsub(" ", "", additional_stmt)
    }

    # Add column for color-type attr in edges_df
    if (exists("attr_assign")){

      if (length(attr_assign) == 2){

        if (attr_assign[1] %in% color_edge_attributes){

          attr_in_df <- attr_assign[1] %in% colnames(edges_df)

          if (attr_in_df == FALSE){

            color_attr <- rep(attr_assign[2], nrow(edges_df))

            edges_df <- cbind(edges_df, color_attr)

            colnames(edges_df)[length(edges_df)] <- attr_assign[1]

            apply_to_column_no <- which(colnames(edges_df) %in% attr_assign[1])

          }
        }
      }

      if (length(attr_assign) == 1){

        if (attr_assign[1] %in% color_edge_attributes){

          attr_in_df <- attr_assign[1] %in% colnames(edges_df)

          if (attr_in_df){

            apply_to_column_no <- which(colnames(edges_df) %in% attr_assign[1])

          }
        }
      }
    }

    # Obtain the min and max values for the data to normalize
    num_range_min_max <- c(min(to_scale), max(to_scale))

    # Get normalized values for attribute if "linear" option chosen
    if (scale_type == "linear"){

      normalized <- range[1] +
        ((to_scale - num_range_min_max[1]) *
           (range[2] - range[1])) /
        (num_range_min_max[2] - num_range_min_max[1])
    }

    # Create data frame for merging
    normalized_df <- data.frame(normalized)

    # Column bind both data frames together
    edges_df <- cbind(edges_df, normalized_df)

    # Change temporary name of column to that of attribute chosen
    colnames(edges_df)[which(names(edges_df) %in% "normalized")] <-
      paste0("alpha_", attr_assign[1])

    return(edges_df)
  }

}
