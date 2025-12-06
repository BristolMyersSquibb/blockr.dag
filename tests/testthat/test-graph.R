library(testthat)

test_that("new_graph creates valid graph objects", {
  # Test with default empty lists
  g <- new_graph()

  expect_s3_class(g, "graph")
  expect_type(g, "list")
  expect_named(g, c("nodes", "edges", "combos"))
  expect_equal(length(g$nodes), 0)
  expect_equal(length(g$edges), 0)
  expect_equal(length(g$combos), 0)

  # Test with provided data
  nodes <- list(list(id = "1"), list(id = "2"))
  edges <- list(list(source = "1", target = "2"))
  combos <- list(list(id = "combo1"))

  g <- new_graph(nodes, edges, combos)

  expect_s3_class(g, "graph")
  expect_identical(g$nodes, nodes)
  expect_identical(g$edges, edges)
  expect_identical(g$combos, combos)
})

test_that("is_graph correctly identifies graph objects", {
  g <- new_graph()

  expect_true(is_graph(g))
  expect_false(is_graph(list()))
  expect_false(is_graph("not a graph"))
  expect_false(is_graph(NULL))
  expect_false(is_graph(123))
  expect_false(is_graph(data.frame()))
})

test_that("graph_nodes extracts nodes correctly", {
  nodes <- list(list(id = "1"), list(id = "2"))
  g <- new_graph(nodes = nodes)

  expect_identical(graph_nodes(g), nodes)

  # Test with empty graph
  empty_g <- new_graph()
  expect_equal(length(graph_nodes(empty_g)), 0)
  expect_type(graph_nodes(empty_g), "list")
})

test_that("graph_edges extracts edges correctly", {
  edges <- list(
    list(source = "1", target = "2"),
    list(source = "2", target = "3")
  )
  g <- new_graph(edges = edges)

  expect_identical(graph_edges(g), edges)

  # Test with empty graph
  empty_g <- new_graph()
  expect_equal(length(graph_edges(empty_g)), 0)
  expect_type(graph_edges(empty_g), "list")
})

test_that("graph_combos extracts combos correctly", {
  combos <- list(list(id = "combo1"), list(id = "combo2"))
  g <- new_graph(combos = combos)

  expect_identical(graph_combos(g), combos)

  # Test with empty graph
  empty_g <- new_graph()
  expect_equal(length(graph_combos(empty_g)), 0)
  expect_type(graph_combos(empty_g), "list")
})

test_that("as_graph.graph returns identical object", {
  g1 <- new_graph(
    nodes = list(list(id = "1")),
    edges = list(list(source = "1", target = "2"))
  )

  g2 <- as_graph(g1)

  expect_identical(g1, g2)
  expect_s3_class(g2, "graph")
})

test_that("as_graph.list converts list to graph", {
  g_data <- list(
    nodes = list(list(id = "1"), list(id = "2")),
    edges = list(list(source = "1", target = "2")),
    combos = list(list(id = "combo1"))
  )

  g <- as_graph(g_data)

  expect_s3_class(g, "graph")
  expect_identical(graph_nodes(g), g_data$nodes)
  expect_identical(graph_edges(g), g_data$edges)
  expect_identical(graph_combos(g), g_data$combos)
})

test_that("as_graph.list works with partial lists", {
  # Only nodes
  g_data <- list(nodes = list(list(id = "test")))
  g <- as_graph(g_data)

  expect_s3_class(g, "graph")
  expect_equal(length(graph_nodes(g)), 1)
  expect_equal(length(graph_edges(g)), 0)
  expect_equal(length(graph_combos(g)), 0)

  # Only edges
  g_data <- list(edges = list(list(source = "1", target = "2")))
  g <- as_graph(g_data)

  expect_s3_class(g, "graph")
  expect_equal(length(graph_nodes(g)), 0)
  expect_equal(length(graph_edges(g)), 1)
  expect_equal(length(graph_combos(g)), 0)

  # Empty list
  g_data <- list()
  g <- as_graph(g_data)

  expect_s3_class(g, "graph")
  expect_equal(length(graph_nodes(g)), 0)
  expect_equal(length(graph_edges(g)), 0)
  expect_equal(length(graph_combos(g)), 0)
})

test_that("as_graph generic dispatches correctly", {
  # Test that UseMethod works correctly
  g <- new_graph()
  expect_identical(as_graph(g), g)

  g_data <- list(nodes = list(), edges = list(), combos = list())
  g <- as_graph(g_data)
  expect_s3_class(g, "graph")
})

test_that("accessor functions handle missing components gracefully", {
  # Create graph-like object missing some components
  incomplete <- structure(list(nodes = list()), class = "graph")

  expect_equal(graph_nodes(incomplete), list())
  expect_null(graph_edges(incomplete))
  expect_null(graph_combos(incomplete))
})
