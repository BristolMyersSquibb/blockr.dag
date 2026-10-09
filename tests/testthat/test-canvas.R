test_that("card nodes leave their ports' size to g6R", {
  blocks <- as_blocks(c(a = new_dataset_block(), b = new_head_block()))
  stacks <- list()
  class(stacks) <- c("stacks", "list")

  icons <- g6_nodes_from_blocks(blocks, stacks)
  cards <- as_card_nodes(
    icons,
    blocks,
    function(blks) lapply(blks, function(blk) htmltools::div())
  )

  port_field <- function(nodes, field) {
    unlist(lapply(nodes, function(n) lapply(n$style$ports, `[[`, field)))
  }

  expect_true(all(port_field(icons, "r") == 3))
  # g6R sizes the ports of an HTML node to the node unless they set a radius
  expect_false(any(port_field(cards, "r") == 3))
  expect_true(all(port_field(cards, "rAuto")))
  # a card has no label to place its output under
  expect_false("label-bottom" %in% port_field(cards, "placement"))
  expect_identical(
    port_field(cards, "key"),
    port_field(icons, "key")
  )
})

test_that("the board makes room for cards that grow", {
  dep <- dag_board_dep()
  expect_true("make-room.js" %in% basename(unlist(dep$script)))
  expect_true(
    file.exists(system.file("assets", "js", "make-room.js", package = "blockr.dag"))
  )
})

test_that("a card node is an HTML node holding its block's card", {
  blocks <- as_blocks(c(a = new_dataset_block(), b = new_head_block()))
  stacks <- list()
  class(stacks) <- c("stacks", "list")
  icons <- g6_nodes_from_blocks(blocks, stacks)

  seen <- NULL
  cards <- as_card_nodes(
    icons,
    blocks,
    function(blks) {
      seen <<- names(blks)
      lapply(names(blks), function(id) htmltools::div(id = paste0("card-", id)))
    }
  )

  # one card per node, in the nodes' order
  expect_identical(seen, c("a", "b"))
  expect_identical(
    vapply(cards, `[[`, character(1L), "id"),
    vapply(icons, `[[`, character(1L), "id")
  )

  card <- cards[[1L]]
  expect_identical(card$type, "custom-html-node")
  expect_identical(card$ui$attribs$id, "card-a")
  expect_identical(card$style$size, card_size())
  expect_true(card$style$autoHeight)
  # the card shows the name; the label stays for search and the outline
  expect_false(card$style$label)
  expect_identical(card$style$labelText, icons[[1L]]$style$labelText)
  expect_null(card$style$src)
})

test_that("without cards, the nodes stay icon nodes", {
  blocks <- as_blocks(c(a = new_dataset_block()))
  stacks <- list()
  class(stacks) <- c("stacks", "list")
  icons <- g6_nodes_from_blocks(blocks, stacks)

  expect_identical(as_card_nodes(icons, blocks, NULL), icons)
})
