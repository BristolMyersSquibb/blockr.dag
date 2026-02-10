library(testthat)

test_that("get_children_from_links works correctly", {
  # Create sample links
  links <- list(
    from = c("a", "a", "b"),
    to = c("b", "c", "d"),
    input = c("data", "data", "data")
  )
  class(links) <- c("links", "list")

  children <- get_children_from_links(links) # Check that node-a has two children
  expect_true("node-a" %in% names(children))
  expect_length(children[["node-a"]], 2)
  expect_true("node-b" %in% children[["node-a"]])
  expect_true("node-c" %in% children[["node-a"]])

  # Check that node-b has one child
  expect_true("node-b" %in% names(children))
  expect_length(children[["node-b"]], 1)
  expect_equal(children[["node-b"]], list("node-d"))

  # Check that node-c and node-d have no children
  expect_false("node-c" %in% names(children))
  expect_false("node-d" %in% names(children))
})

test_that("get_children_from_links handles empty links", {
  children <- get_children_from_links(list())
  expect_length(children, 0)
})

test_that("get_children_from_links handles duplicate targets", {
  # A node pointing to same target multiple times should only list it once
  links <- list(
    from = c("a", "a", "a"),
    to = c("b", "b", "c"),
    input = c("data1", "data2", "data3")
  )
  class(links) <- c("links", "list")

  children <- get_children_from_links(links)
  expect_length(children[["node-a"]], 2)
  expect_true("node-b" %in% children[["node-a"]])
  expect_true("node-c" %in% children[["node-a"]])
})

test_that("g6_nodes_from_blocks includes children when provided", {
  # Create simple blocks
  blocks <- list(
    a = new_dataset_block(),
    b = new_head_block()
  )
  class(blocks) <- c("blocks", "list")

  stacks <- list()
  class(stacks) <- c("stacks", "list")

  # Create children mapping (as list like get_children_from_links returns)
  children <- list(
    "node-a" = list("node-b")
  )

  nodes <- g6_nodes_from_blocks(blocks, stacks, children)

  # Check that node-a has children
  expect_length(nodes, 2)
  expect_equal(nodes[[1]]$id, "node-a")
  expect_equal(nodes[[1]]$children, list("node-b"))

  # Check that node-b has no children
  expect_equal(nodes[[2]]$id, "node-b")
  expect_null(nodes[[2]]$children)
})

test_that("g6_data_from_board computes and includes children", {
  # Create a simple board with blocks and links
  blocks <- as_blocks(c(
    a = new_dataset_block(),
    b = new_head_block(),
    c = new_head_block()
  ))

  links <- as_links(c(
    new_link("a", "b", input = "data"),
    new_link("b", "c", input = "data")
  ))

  stacks <- list()
  class(stacks) <- c("stacks", "list")

  board <- structure(
    list(
      blocks = blocks,
      links = links,
      stacks = stacks
    ),
    class = c("board", "list")
  )

  graph <- g6_data_from_board(board)

  # Check that nodes have children
  expect_length(graph$nodes, 3)

  # node-a should have node-b as child
  node_a <- graph$nodes[[1]]
  expect_equal(node_a$id, "node-a")
  expect_equal(node_a$children, list("node-b"))

  # node-b should have node-c as child
  node_b <- graph$nodes[[2]]
  expect_equal(node_b$id, "node-b")
  expect_equal(node_b$children, list("node-c"))

  # node-c should have no children
  node_c <- graph$nodes[[3]]
  expect_equal(node_c$id, "node-c")
  expect_null(node_c$children)
})
