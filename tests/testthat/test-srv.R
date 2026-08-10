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

test_that("a pinned panel re-targets on a matching selection", {

  h <- retarget_harness()

  testServer(h$server, {

    session$setInputs(`board-actions_sidebar` = list(open = TRUE, pinned = TRUE))
    session$setInputs(ctx_edit_stack = "s1")

    session$setInputs(`graph-selected_combo` = "combo-s2")
    expect_identical(h$log$fired, "edit_stack:s2")

    session$setInputs(`graph-selected_node` = "node-a")
    expect_identical(h$log$fired, "edit_stack:s2")
  })
})

test_that("another entry filling the panel stops the re-target", {

  h <- retarget_harness()

  testServer(h$server, {

    session$setInputs(`board-actions_sidebar` = list(open = TRUE, pinned = TRUE))
    session$setInputs(ctx_edit_stack = "s1")

    session$setInputs(ctx_create_stack = TRUE)
    session$setInputs(`graph-selected_combo` = "combo-s2")

    expect_identical(h$log$fired, character())
  })
})

test_that("the toolbar and a canvas prepend also release the panel", {

  h <- retarget_harness()

  testServer(h$server, {

    session$setInputs(`board-actions_sidebar` = list(open = TRUE, pinned = TRUE))

    session$setInputs(ctx_edit_stack = "s1")
    session$setInputs(tool_add_stack = TRUE)
    session$setInputs(`graph-selected_combo` = "combo-s2")

    expect_identical(h$log$fired, character())

    session$setInputs(ctx_edit_stack = "s3")
    session$setInputs(
      added_edge = list(targetType = "canvas", portType = "input", source = "a")
    )
    session$setInputs(`graph-selected_combo` = "combo-s4")

    expect_identical(h$log$fired, character())
  })
})

test_that("closing the panel drops its owner", {

  h <- retarget_harness()

  testServer(h$server, {

    session$setInputs(`board-actions_sidebar` = list(open = TRUE, pinned = TRUE))
    session$setInputs(ctx_edit_stack = "s1")

    session$setInputs(`board-actions_sidebar` = list(open = FALSE, pinned = TRUE))
    session$setInputs(`graph-selected_combo` = "combo-s2")

    expect_identical(h$log$fired, character())
  })
})

test_that("each editor is gated on the panel it fills", {

  h <- retarget_harness()

  testServer(h$server, {

    session$setInputs(
      `board-append_block_sidebar` = list(open = TRUE, pinned = TRUE)
    )
    session$setInputs(`board-actions_sidebar` = list(open = FALSE, pinned = FALSE))

    session$setInputs(ctx_append_block = "a")
    session$setInputs(`graph-selected_node` = "node-b")

    expect_identical(h$log$fired, "append_block:b")
  })
})

test_that("an unpinned panel never re-targets", {

  h <- retarget_harness()

  testServer(h$server, {

    session$setInputs(`board-actions_sidebar` = list(open = TRUE, pinned = FALSE))
    session$setInputs(ctx_edit_stack = "s1")

    session$setInputs(`graph-selected_combo` = "combo-s2")
    expect_identical(h$log$fired, character())
  })
})
