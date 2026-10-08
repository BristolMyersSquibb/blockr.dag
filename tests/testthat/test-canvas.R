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
