# Card nodes: the DAG board's blocks.
#
# On a DAG board every node carries its block's card, the same card
# blockr.dock shows in a panel, as g6R HTML node content. The node builders
# produce icon nodes for the DAG extension; as_card_nodes() turns them into
# card nodes, so both share the node ids, stacks, ports and positions.

# Width of a card, and the height it starts at: its node then takes the
# card's height (g6R's autoHeight), so a card never scrolls as a whole.
card_size <- function() c(380, 200)

# Room between cards, across and down: the layout's spacing, and what a card
# that grows keeps above the cards below it (make-room.js).
card_gap <- function() 80

# `cards` is a function of a `blocks` object returning one card per block,
# in order (see block_cards()); NULL leaves the nodes as they are.
as_card_nodes <- function(nodes, blocks, cards) {

  if (is.null(cards) || !length(nodes)) {
    return(nodes)
  }

  ids <- from_g6_node_id(chr_ply(nodes, `[[`, "id"))
  blocks <- blocks[ids]
  uis <- cards(blocks)

  for (i in seq_along(nodes)) {
    nodes[[i]] <- as_card_node(nodes[[i]], uis[[i]], blocks[[i]])
  }

  nodes
}

as_card_node <- function(node, ui, block) {

  node$type <- "custom-html-node"
  # The label is not drawn (the card shows the block's name) but kept, since
  # search and the outline find and list nodes by it, and renames update it.
  node$style <- c(
    node$style[setdiff(names(node$style), "src")],
    list(size = card_size(), autoHeight = TRUE, label = FALSE)
  )

  # An icon node's ports are small, and its output sits under its label. A
  # card's ports are sized to the card by g6R, and a card has no label.
  ports <- create_block_ports(block, node$id, r = NULL)
  ports[] <- lapply(
    ports,
    function(port) {
      if (identical(port$placement, "label-bottom")) {
        port$placement <- "bottom"
      }
      port
    }
  )
  node$style$ports <- ports

  node$ui <- ui
  node
}

# The card builder for a DAG board's nodes: blockr.dock's block card, with the
# board's edit and control plugins, namespaced under the board so the block
# servers find their inputs and outputs. The card header is the node's drag
# handle.
block_cards <- function(board, plugins) {

  # plugins are a strict list: extracting one that is not served errors
  pick <- function(name) if (name %in% names(plugins)) plugins[[name]]

  edit_ui <- pick("edit_block")
  ctrl_ui <- pick("ctrl_block")

  # called while building the graph and from the update observer: the cards
  # need the board as it is, not a dependency on it
  function(blocks) {
    cards <- blockr.core::block_ui(
      isolate(board$board_id),
      isolate(board$board),
      edit_ui = edit_ui,
      blocks = blocks,
      ctrl_ui = ctrl_ui
    )
    lapply(cards, mark_drag_handle)
  }
}

mark_drag_handle <- function(card) {
  htmltools::tagQuery(card)$
    find(".blockr-block-header")$
    addAttrs(`data-g6-drag-handle` = NA)$
    allTags()
}

# The board is lazy (see dag_board_callback()): core evaluates the blocks the
# canvas holds eager, and renders a block once it is reported painted. The
# canvas holds the cards on screen, as on-screen.js reports them, and nothing
# before its first report. A report can name a block that a removal has just
# taken off the board, so it is read against the blocks core still has.
hold_on_screen <- function(input, update, visibility, owner) {

  held <- new.env(parent = emptyenv())
  held$ids <- character()

  observeEvent(
    input$on_screen,
    {
      # an empty report (nothing on screen) arrives as an empty list
      on_screen <- as.character(unlist(input$on_screen))
      ids <- sort(intersect(on_screen, ls(visibility$visible)))

      for (id in setdiff(ls(visibility$visible), ids)) {
        if (isTRUE(visibility$visible[[id]]())) {
          visibility$visible[[id]](FALSE)
        }
      }

      for (id in ids) {
        if (!isTRUE(visibility$visible[[id]]())) {
          visibility$visible[[id]](TRUE)
        }
      }

      if (!identical(held$ids, ids)) {
        held$ids <- ids
        hold_eager(update, owner, ids)
      }
    },
    ignoreNULL = FALSE
  )

  invisible()
}

# Core takes one board update per flush, so the eager set is folded into what
# is already pending rather than replacing it.
hold_eager <- function(update, owner, ids) {
  update(
    utils::modifyList(
      coal(isolate(update()), list(), fail_all = FALSE),
      list(eager = set_names(list(list(set = ids)), owner))
    )
  )
}
