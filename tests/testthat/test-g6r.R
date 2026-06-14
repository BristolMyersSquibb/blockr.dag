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

test_that("new_nodes_layout places a single node at the anchor", {
  expect_equal(
    new_nodes_layout("a", base_x = 100, base_y = 200),
    list(x = 100, y = 200)
  )
})

test_that("new_nodes_layout stacks unstructured batches vertically", {
  res <- new_nodes_layout(
    c("a", "b", "c"),
    links = NULL,
    base_x = 100,
    base_y = 50,
    ranksep = 130
  )
  expect_equal(res$x, c(100, 100, 100))
  expect_equal(res$y, c(50, 180, 310))
})

test_that("new_nodes_layout layers a connected chain top-to-bottom", {
  res <- new_nodes_layout(
    c("a", "b", "c"),
    links = list(from = c("a", "b"), to = c("b", "c")),
    base_x = 0,
    base_y = 0,
    ranksep = 100
  )
  # one node per layer => centred on base_x
  expect_equal(res$x, c(0, 0, 0))
  expect_equal(res$y, c(0, 100, 200))
})

test_that("new_nodes_layout spreads siblings and centres them", {
  res <- new_nodes_layout(
    c("a", "b", "c", "d"),
    links = list(from = c("a", "a", "b", "c"), to = c("b", "c", "d", "d")),
    base_x = 200,
    base_y = 50,
    ranksep = 130,
    nodesep = 150
  )
  # a top, b/c middle (centred around 200), d bottom
  expect_equal(res$y, c(50, 180, 180, 310))
  expect_equal(res$x, c(200, 125, 275, 200))
})

test_that("new_nodes_layout ignores links to existing nodes", {
  res <- new_nodes_layout(
    c("a", "b"),
    links = list(from = c("X", "a"), to = c("a", "b")),
    base_x = 0,
    base_y = 0,
    ranksep = 130
  )
  # X is not in the new set, so a stays at layer 0, b at layer 1
  expect_equal(res$y, c(0, 130))
})

test_that("existing_node_positions reads coordinates from graph state", {
  proxy <- list(
    session = list(
      ns = identity,
      input = list(
        `graph-state` = list(
          nodes = list(
            list(id = "node-a", style = list(x = 10, y = 20)),
            list(id = "node-b", style = list(x = 30, y = 40)),
            list(id = "node-c", style = list()) # no coords -> dropped
          )
        )
      )
    )
  )

  pos <- existing_node_positions(proxy)
  expect_named(pos, c("node-a", "node-b"))
  expect_equal(pos[["node-b"]], list(x = 30, y = 40))
})

test_that("new_nodes_anchor anchors below existing parents", {
  proxy <- list(
    session = list(
      input = list(
        `graph-mouse_position` = list(x = 999, y = 999),
        `graph-state` = list(
          nodes = list(
            list(id = "node-p1", style = list(x = 100, y = 100)),
            list(id = "node-p2", style = list(x = 200, y = 140))
          )
        )
      )
    )
  )

  # new node "n" connects from existing parents p1 and p2
  anchor <- new_nodes_anchor(
    ids = "n",
    links = list(from = c("p1", "p2"), to = c("n", "n")),
    proxy = proxy,
    ranksep = 130
  )
  expect_equal(anchor$x, 150) # mean(100, 200)
  expect_equal(anchor$y, 270) # max(100, 140) + 130
})

test_that("new_nodes_anchor falls back to cursor without existing parents", {
  proxy <- list(
    session = list(
      input = list(
        `graph-mouse_position` = list(x = 42, y = 84),
        `graph-state` = list(nodes = list())
      )
    )
  )

  # purely internal links, no link from an existing node
  anchor <- new_nodes_anchor(
    ids = c("a", "b"),
    links = list(from = "a", to = "b"),
    proxy = proxy
  )
  expect_equal(anchor, list(x = 42, y = 84))
})
