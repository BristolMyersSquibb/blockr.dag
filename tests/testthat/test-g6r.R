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

test_that("g6_from_board builds a widget from the board", {
  board <- new_board(
    blocks = c(a = new_dataset_block("iris"), b = new_head_block()),
    links = list(ab = new_link("a", "b", input = "data"))
  )
  res <- g6_from_board(board)
  expect_s3_class(res, c("g6", "htmlwidget"))
})

test_that("default_dag_layout is a serializable top-down antv-dagre", {
  lay <- default_dag_layout()
  expect_identical(lay$type, "antv-dagre")
  expect_identical(lay$rankdir, "TB")
  expect_identical(lay$ranker, "network-simplex")
  # No JS callbacks, so it survives a JSON round trip (save / restore)
  rt <- jsonlite::fromJSON(jsonlite::toJSON(lay, auto_unbox = TRUE))
  expect_identical(rt$type, "antv-dagre")
  expect_identical(rt$rankdir, "TB")
})

test_that("set_g6_layout falls back to the default when layout is NULL", {
  board <- new_board(blocks = c(a = new_dataset_block("iris")))
  g <- set_g6_layout(g6_from_board(board))
  expect_identical(g$x$layout$type, "antv-dagre")
  expect_identical(g$x$layout$rankdir, "TB")
})

test_that("set_g6_layout honors a supplied layout", {
  board <- new_board(blocks = c(a = new_dataset_block("iris")))
  g <- set_g6_layout(
    g6_from_board(board),
    antv_dagre_layout(rankdir = "LR", nodesep = 99)
  )
  expect_identical(g$x$layout$rankdir, "LR")
  expect_identical(g$x$layout$nodesep, 99)
})

test_that("a JSON round-tripped layout is accepted (restore path)", {
  board <- new_board(blocks = c(a = new_dataset_block("iris")))
  # On restore the layout comes back as a plain list (class dropped by JSON)
  restored <- jsonlite::fromJSON(
    jsonlite::toJSON(antv_dagre_layout(rankdir = "LR"), auto_unbox = TRUE)
  )
  g <- set_g6_layout(g6_from_board(board), restored)
  expect_identical(g$x$layout$type, "antv-dagre")
  expect_identical(g$x$layout$rankdir, "LR")
})

test_that("merge_node_positions overlays positions onto matching nodes", {
  nodes <- list(
    structure(list(id = "node-a", style = list(src = "x")), class = "g6_node"),
    structure(list(id = "node-b", style = list(src = "y")), class = "g6_node")
  )

  # No positions -> nodes unchanged
  expect_identical(merge_node_positions(nodes, NULL), nodes)
  expect_identical(merge_node_positions(nodes, list()), nodes)

  res <- merge_node_positions(
    nodes,
    list(a = list(x = 10, y = 20), zzz = list(x = 1, y = 2))
  )

  # Matching block id is pinned; unknown / stale ids are ignored
  expect_equal(res[[1]]$style$x, 10)
  expect_equal(res[[1]]$style$y, 20)
  # board-derived styling is preserved
  expect_equal(res[[1]]$style$src, "x")
  # node without a supplied position keeps no preset
  expect_null(res[[2]]$style$x)
  expect_null(res[[2]]$style$y)
})

test_that("project_positions re-keys live state to a block-id spec", {
  expect_identical(project_positions(NULL), list())
  expect_identical(project_positions(list(nodes = NULL)), list())

  state <- list(
    nodes = list(
      "node-a" = list(id = "node-a", style = list(x = 10, y = 20, src = "x")),
      # missing coordinates -> dropped
      "node-b" = list(id = "node-b", style = list(src = "y"))
    )
  )

  res <- project_positions(state)

  expect_named(res, "a")
  expect_identical(res$a, list(x = 10, y = 20))
})

test_that("positions round-trip through project then merge", {
  board <- new_board(blocks = c(a = new_dataset_block("iris")))

  # Live state as emitted by g6 (keyed by g6 id, positions in style$x/y)
  state <- list(
    nodes = list(
      "node-a" = list(id = "node-a", style = list(x = 42, y = 84))
    )
  )

  positions <- project_positions(state)

  nodes <- merge_node_positions(
    graph_nodes(g6_data_from_board(board)),
    positions
  )

  expect_equal(nodes[[1]]$style$x, 42)
  expect_equal(nodes[[1]]$style$y, 84)
})

test_that("positions_equal compares up to whole-pixel rounding", {
  expect_true(positions_equal(list(), list()))
  expect_true(positions_equal(
    list(a = list(x = 10, y = 20)),
    list(a = list(x = 10.4, y = 19.6))
  ))
  expect_false(positions_equal(
    list(a = list(x = 10, y = 20)),
    list(a = list(x = 13, y = 20))
  ))
  # Different number of entries
  expect_false(positions_equal(
    list(a = list(x = 10, y = 20)),
    list(a = list(x = 10, y = 20), b = list(x = 1, y = 1))
  ))
})

test_that("positions_diff returns only changed entries (echo guard)", {
  live <- list(a = list(x = 10, y = 20), b = list(x = 5, y = 5))

  # Identical -> nothing to push (this is what breaks the echo loop)
  expect_length(positions_diff(live, live), 0L)

  # Only b moved -> only b is pushed
  target <- list(a = list(x = 10, y = 20), b = list(x = 9, y = 9))
  expect_named(positions_diff(target, live), "b")

  # Empty target -> nothing
  expect_identical(positions_diff(list(), live), list())
})

test_that("apply_node_positions builds a g6 node-update payload", {
  captured <- NULL
  local_mocked_bindings(
    g6_update_nodes = function(proxy, nodes) {
      captured <<- nodes
      invisible()
    }
  )

  apply_node_positions(list(a = list(x = 100, y = 200)), proxy = NULL)

  expect_equal(captured[[1]]$id, to_g6_node_id("a"))
  expect_equal(captured[[1]]$style$x, 100)
  expect_equal(captured[[1]]$style$y, 200)

  # No-op on empty input (no client message)
  captured <- NULL
  apply_node_positions(list(), proxy = NULL)
  expect_null(captured)
})

test_that("is_variadic_block works", {
  expect_false(is_variadic_block(new_dataset_block()))
  expect_true(is_variadic_block(new_rbind_block()))
})

test_that("create_block ports works", {
  blk <- new_scatter_block()
  ports <- create_block_ports(blk, "test")
  expect_s3_class(ports, "g6_ports")
  expect_length(ports, 2)
  expect_identical(ports[[1]]$type, "input")
  expect_identical(ports[[2]]$type, "output")
})

test_that("create_block_ports places ports by flow direction", {
  blk <- new_head_block() # mono input

  tb <- create_block_ports(blk, "x", rankdir = "TB")
  expect_identical(tb[[1]]$placement, "top")
  expect_identical(tb[[2]]$placement, "label-bottom")

  lr <- create_block_ports(blk, "x", rankdir = "LR")
  expect_identical(lr[[1]]$placement, "left")
  expect_identical(lr[[2]]$placement, "right")
})

test_that("resolve_target_ports works", {
  blocks <- as_blocks(c(
    a = new_dataset_block(),
    b = new_head_block(),
    c = new_rbind_block()
  ))
  links <- as_links(
    c(
      new_link("a", "b", input = "data"),
      new_link("a", "c"),
      new_link("b", "c")
    )
  )
  res <- resolve_target_ports(links, blocks)
  expect_identical(res[[1]], "node-b-data")
  expect_identical(res[[2]], "node-c-in")
  expect_identical(res[[3]], "node-c-in")
})
