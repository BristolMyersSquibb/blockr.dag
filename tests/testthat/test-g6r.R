library(testthat)

test_that("to_g6_node_id works correctly", {
  expect_equal(to_g6_node_id("test1"), "node-test1")
  expect_equal(
    to_g6_node_id(c("test1", "test2")),
    c("node-test1", "node-test2")
  )
  expect_equal(to_g6_node_id(character(0)), character(0))
  expect_equal(to_g6_node_id(NULL), NULL)
  expect_equal(to_g6_node_id(""), "node-")
  expect_equal(to_g6_node_id(123), "node-123")
})

test_that("from_g6_node_id works correctly", {
  expect_equal(from_g6_node_id("node-test1"), "test1")
  expect_equal(
    from_g6_node_id(c("node-test1", "node-test2")),
    c("test1", "test2")
  )
  expect_equal(from_g6_node_id("test1"), "test1")
  expect_equal(from_g6_node_id(""), "")
  expect_equal(from_g6_node_id("node-"), "")
  expect_equal(from_g6_node_id("node-node-test"), "node-test")
})

test_that("to_g6_edge_id works correctly", {
  expect_equal(to_g6_edge_id("edge1"), "edge-edge1")
  expect_equal(
    to_g6_edge_id(c("edge1", "edge2")),
    c("edge-edge1", "edge-edge2")
  )
  expect_equal(to_g6_edge_id(character(0)), character(0))
  expect_equal(to_g6_edge_id(NULL), NULL)
  expect_equal(to_g6_edge_id(""), "edge-")
  expect_equal(to_g6_edge_id(456), "edge-456")
})

test_that("from_g6_edge_id works correctly", {
  expect_equal(from_g6_edge_id("edge-edge1"), "edge1")
  expect_equal(
    from_g6_edge_id(c("edge-edge1", "edge-edge2")),
    c("edge1", "edge2")
  )
  expect_equal(from_g6_edge_id("edge1"), "edge1")
  expect_equal(from_g6_edge_id(""), "")
  expect_equal(from_g6_edge_id("edge-"), "")
  expect_equal(from_g6_edge_id("edge-edge-test"), "edge-test")
})

test_that("to_g6_combo_id works correctly", {
  expect_equal(to_g6_combo_id("combo1"), "combo-combo1")
  expect_equal(
    to_g6_combo_id(c("combo1", "combo2")),
    c("combo-combo1", "combo-combo2")
  )
  expect_equal(to_g6_combo_id(character(0)), character(0))
  expect_equal(to_g6_combo_id(NULL), NULL)
  expect_equal(to_g6_combo_id(""), "combo-")
  expect_equal(to_g6_combo_id(789), "combo-789")
})

test_that("from_g6_combo_id works correctly", {
  expect_equal(from_g6_combo_id("combo-combo1"), "combo1")

  expect_equal(
    from_g6_combo_id(c("combo-combo1", "combo-combo2")),
    c("combo1", "combo2")
  )

  expect_equal(from_g6_combo_id("combo1"), "combo1")
  expect_equal(from_g6_combo_id(""), "")
  expect_equal(from_g6_combo_id("combo-"), "")
  expect_equal(from_g6_combo_id("combo-combo-test"), "combo-test")
})

test_that("to_g6_port_id works correctly", {
  expect_equal(to_g6_port_id("in1", "node1"), "node1-in1")
  expect_equal(
    to_g6_port_id(c("in1", "out1"), "node1"),
    c("node1-in1", "node1-out1")
  )
  expect_equal(
    to_g6_port_id("in1", c("node1", "node2")),
    c("node1-in1", "node2-in1")
  )
  expect_equal(to_g6_port_id(character(0), "node1"), character(0))
  expect_equal(to_g6_port_id(NULL, "node1"), NULL)
  expect_equal(to_g6_port_id("in1", ""), "-in1")
})

test_that("from_g6_port_id works correctly", {
  expect_equal(from_g6_port_id("node1-in1", "node1"), "in1")
  expect_equal(
    from_g6_port_id(c("node1-in1", "node1-out1"), "node1"),
    c("in1", "out1")
  )
  expect_equal(from_g6_port_id("node2-in2", "node2"), "in2")
  expect_equal(from_g6_port_id("in1", "node1"), "in1")
  expect_equal(from_g6_port_id("", "node1"), "")
  expect_equal(from_g6_port_id("node1-", "node1"), "")
})

test_that("roundtrip conversions work correctly", {
  original_nodes <- c("node1", "node2", "node3")
  expect_equal(from_g6_node_id(to_g6_node_id(original_nodes)), original_nodes)

  original_edges <- c("edge1", "edge2", "edge3")
  expect_equal(from_g6_edge_id(to_g6_edge_id(original_edges)), original_edges)

  original_combos <- c("combo1", "combo2", "combo3")
  expect_equal(
    from_g6_combo_id(to_g6_combo_id(original_combos)),
    original_combos
  )
})

test_that("g6_from_graph works", {
  g <- new_graph(
    nodes = list(
      list(id = 1, style = list(labelText = "Node 1")),
      list(id = 2, style = list(labelText = "Node 2"))
    ),
    edges = list(
      list(
        source = 1,
        target = 2,
        style = list(
          labelText = "Edge from 1 to 2"
        )
      )
    )
  )
  res <- g6_from_graph(g)
  expect_s3_class(res, c("g6", "htmlwidget"))
})
