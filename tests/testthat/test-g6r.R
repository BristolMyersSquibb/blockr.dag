library(testthat)

# Test to_g6_node_id function
test_that("to_g6_node_id works correctly", {
  # Single ID
  expect_equal(to_g6_node_id("test1"), "node-test1")

  # Multiple IDs
  expect_equal(
    to_g6_node_id(c("test1", "test2")),
    c("node-test1", "node-test2")
  )

  # Empty vector
  expect_equal(to_g6_node_id(character(0)), character(0))

  # NULL input
  expect_equal(to_g6_node_id(NULL), NULL)

  # Empty string
  expect_equal(to_g6_node_id(""), "node-")

  # Numeric input (converted to character)
  expect_equal(to_g6_node_id(123), "node-123")
})

# Test from_g6_node_id function
test_that("from_g6_node_id works correctly", {
  # Single ID with prefix
  expect_equal(from_g6_node_id("node-test1"), "test1")

  # Multiple IDs with prefix
  expect_equal(
    from_g6_node_id(c("node-test1", "node-test2")),
    c("test1", "test2")
  )

  # ID without prefix (should remain unchanged)
  expect_equal(from_g6_node_id("test1"), "test1")

  # Empty string
  expect_equal(from_g6_node_id(""), "")

  # Just the prefix
  expect_equal(from_g6_node_id("node-"), "")

  # Multiple prefixes (only first is removed)
  expect_equal(from_g6_node_id("node-node-test"), "node-test")
})

# Test to_g6_edge_id function
test_that("to_g6_edge_id works correctly", {
  # Single ID
  expect_equal(to_g6_edge_id("edge1"), "edge-edge1")

  # Multiple IDs
  expect_equal(
    to_g6_edge_id(c("edge1", "edge2")),
    c("edge-edge1", "edge-edge2")
  )

  # Empty vector
  expect_equal(to_g6_edge_id(character(0)), character(0))

  # NULL input
  expect_equal(to_g6_edge_id(NULL), NULL)

  # Empty string
  expect_equal(to_g6_edge_id(""), "edge-")

  # Numeric input
  expect_equal(to_g6_edge_id(456), "edge-456")
})

# Test from_g6_edge_id function
test_that("from_g6_edge_id works correctly", {
  # Single ID with prefix
  expect_equal(from_g6_edge_id("edge-edge1"), "edge1")

  # Multiple IDs with prefix
  expect_equal(
    from_g6_edge_id(c("edge-edge1", "edge-edge2")),
    c("edge1", "edge2")
  )

  # ID without prefix
  expect_equal(from_g6_edge_id("edge1"), "edge1")

  # Empty string
  expect_equal(from_g6_edge_id(""), "")

  # Just the prefix
  expect_equal(from_g6_edge_id("edge-"), "")

  # Multiple prefixes
  expect_equal(from_g6_edge_id("edge-edge-test"), "edge-test")
})

# Test to_g6_combo_id function
test_that("to_g6_combo_id works correctly", {
  # Single ID
  expect_equal(to_g6_combo_id("combo1"), "combo-combo1")

  # Multiple IDs
  expect_equal(
    to_g6_combo_id(c("combo1", "combo2")),
    c("combo-combo1", "combo-combo2")
  )

  # Empty vector
  expect_equal(to_g6_combo_id(character(0)), character(0))

  # NULL input
  expect_equal(to_g6_combo_id(NULL), NULL)

  # Empty string
  expect_equal(to_g6_combo_id(""), "combo-")

  # Numeric input
  expect_equal(to_g6_combo_id(789), "combo-789")
})

# Test from_g6_combo_id function
test_that("from_g6_combo_id works correctly", {
  # Single ID with prefix
  expect_equal(from_g6_combo_id("combo-combo1"), "combo1")

  # Multiple IDs with prefix
  expect_equal(
    from_g6_combo_id(c("combo-combo1", "combo-combo2")),
    c("combo1", "combo2")
  )

  # ID without prefix
  expect_equal(from_g6_combo_id("combo1"), "combo1")

  # Empty string
  expect_equal(from_g6_combo_id(""), "")

  # Just the prefix
  expect_equal(from_g6_combo_id("combo-"), "")

  # Multiple prefixes
  expect_equal(from_g6_combo_id("combo-combo-test"), "combo-test")
})

# Test roundtrip functionality
test_that("roundtrip conversions work correctly", {
  # Node IDs
  original_nodes <- c("node1", "node2", "node3")
  expect_equal(from_g6_node_id(to_g6_node_id(original_nodes)), original_nodes)

  # Edge IDs
  original_edges <- c("edge1", "edge2", "edge3")
  expect_equal(from_g6_edge_id(to_g6_edge_id(original_edges)), original_edges)

  # Combo IDs
  original_combos <- c("combo1", "combo2", "combo3")
  expect_equal(
    from_g6_combo_id(to_g6_combo_id(original_combos)),
    original_combos
  )
})
