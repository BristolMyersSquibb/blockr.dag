retarget_harness <- function(board_id = "board") {

  log <- new.env(parent = emptyenv())
  log$fired <- character()

  record <- function(name) {
    function(id) log$fired <- c(log$fired, paste0(name, ":", id))
  }

  ext <- new_dag_extension()

  actions <- list(
    edit_stack_action = record("edit_stack"),
    add_stack_action = record("add_stack"),
    add_link_action = record("add_link"),
    edit_link_action = record("edit_link"),
    append_block_action = record("append_block"),
    add_block_action = record("add_block"),
    prepend_block_action = record("prepend_block")
  )

  server <- function(input, output, session) {
    setup_sidebar_retarget(
      c(context_menu_items(ext), toolbar_items(ext)),
      actions,
      reactiveValues(board = NULL, board_id = board_id),
      list(session = session)
    )
  }

  list(log = log, server = server)
}

# A panel input as the client reports it once an action has written the body:
# `show_sidebar()` stamps the writing module, which for a board action is
# `NS(<board id>, <action id>)`.
owned_panel <- function(action, board_id = "board", pinned = TRUE) {
  list(open = TRUE, pinned = pinned, owner = NS(board_id, action))
}

test_that("a pinned panel re-targets on a matching selection", {

  h <- retarget_harness()

  testServer(h$server, {

    session$setInputs(`board-actions_sidebar` = owned_panel("edit_stack_action"))

    session$setInputs(`graph-selected_combo` = "combo-s2")
    expect_identical(h$log$fired, "edit_stack:s2")

    session$setInputs(`graph-selected_node` = "node-a")
    expect_identical(h$log$fired, "edit_stack:s2")
  })
})

test_that("the link editor follows edge selections", {

  h <- retarget_harness()

  testServer(h$server, {

    session$setInputs(`board-actions_sidebar` = owned_panel("edit_link_action"))

    session$setInputs(`graph-selected_edge` = "edge-a_b")
    expect_identical(h$log$fired, "edit_link:a_b")

    session$setInputs(`graph-selected_node` = "node-a")
    expect_identical(h$log$fired, "edit_link:a_b")
  })
})

test_that("another entry filling the panel stops the re-target", {

  h <- retarget_harness()

  testServer(h$server, {

    session$setInputs(`board-actions_sidebar` = owned_panel("add_stack_action"))
    session$setInputs(`graph-selected_combo` = "combo-s2")

    expect_identical(h$log$fired, character())
  })
})

test_that("an entry whose action does not hold the panel does nothing", {

  h <- retarget_harness()

  testServer(h$server, {

    session$setInputs(
      `board-append_block_sidebar` = owned_panel("append_block_action")
    )

    session$setInputs(`graph-selected_node` = "node-b")
    expect_identical(h$log$fired, "append_block:b")

    session$setInputs(`graph-selected_combo` = "combo-s2")
    expect_identical(h$log$fired, "append_block:b")
  })
})

test_that("an unpinned panel never re-targets", {

  h <- retarget_harness()

  testServer(h$server, {

    session$setInputs(
      `board-actions_sidebar` = owned_panel("edit_stack_action", pinned = FALSE)
    )

    session$setInputs(`graph-selected_combo` = "combo-s2")
    expect_identical(h$log$fired, character())
  })
})
