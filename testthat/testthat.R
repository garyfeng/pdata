### Unit testing ###
require(testthat)
devtools::install_github('garyfeng/DiagrammeR')
require(DiagrammeR)
require(dplyr)
# we use the github site
require(devtools)
devtools::install_github('garyfeng/pdata')

############
nodes <-
  create_nodes(nodes = c("a", "b", "c", "d"),
               label = FALSE,
               type = "lower",
               style = "filled",
               color = "aqua",
               shape = c("circle", "circle",
                         "rectangle", "rectangle"),
               data = c(3.5, 2.6, 9.4, 2.7))
edges <-
  create_edges(from = c("a", "c", "c"),
               to = c("d", "c", "a"),
               relationship = "leading_to")


graph <-
  create_graph(nodes_df = nodes,
               edges_df = edges,
               node_attrs = "fontname = Helvetica",
               edge_attrs = c("color = blue",
                              "arrowsize = 2"))

test_that("Deleting isolated nodes makes no errors", {
  # making a graph using all default parameters
  expect_that(delete_node(graph, node="b"), not(throws_error()))
})
#
# # original graph
# node_info(graph)
# render_graph(graph)
# # delete "b" node
# gr1 <- delete_node(graph, node="b")
# node_info(gr1)
# render_graph(gr1)



############
# define a diagram
df <- data.frame(
  evt=c("this", "2", "hi", "that", "2", "3", "that", "2", "hi", "that", "2", "3", "not this"),
  by=c(1, 2, 1, 1, 2, 2, 1, 2, 1, 1, 2, 2, 1)
)

# # making a graph using all default parameters
# g <- makeEventDiagram(df$evt)
# render_graph(g)
#
# # using options nodeShapeBy
# g<-makeEventDiagram(df$evt, nodeShapeBy=df$by);
# render_graph(g)
#
# # using options ignoreRuns
# g<-makeEventDiagram(df$evt, nodeShapeBy=df$by, ignoreRuns = T);
# render_graph(g)
#
# # using options minimalEdgeCount
# g<-makeEventDiagram(df$evt, nodeShapeBy=df$by, minimalEdgeCount = 1);
# render_graph(g)
#
# # using options minimalEdgeCount
# g<-makeEventDiagram(df$evt, nodeShapeBy=df$by, minimalEdgeCount = 1, deleteIsolatedNodes=F);
# render_graph(g)

test_that("makeEventDiagram.R makes no errors", {
  # making a graph using all default parameters
  expect_that(makeEventDiagram(df$evt), not(throws_error()))
  expect_that(makeEventDiagram(df$evt, nodeShapeBy=df$by), not(throws_error()))
  expect_that(makeEventDiagram(df$evt, nodeShapeBy=df$by, minimalEdgeCount = 1), not(throws_error()))
  expect_that(makeEventDiagram(df$evt, nodeShapeBy=df$by, minimalEdgeCount = 1, deleteIsolatedNodes=F), not(throws_error()))
})


