test_that("a dag board is a dock board with a DAG extension", {
  board <- new_dag_board(blocks = c(a = new_dataset_block()))

  expect_s3_class(board, c("dag_board", "dock_board", "board"))
  expect_true(is_dag_board(board))
  expect_false(is_dag_board(blockr.dock::new_dock_board()))

  ext <- blockr.dock::dock_extensions(board)
  expect_length(ext, 1L)
  expect_s3_class(ext[[1L]], "dag_extension")
  expect_identical(dag_extension_id(board), names(ext))
})

test_that("a dag board keeps the DAG extension it is given", {
  board <- new_dag_board(extensions = new_dag_extension())

  expect_length(blockr.dock::dock_extensions(board), 1L)
})

test_that("a dag board round-trips through JSON as a dag board", {
  board <- new_dag_board(
    blocks = c(a = new_dataset_block("iris"), b = new_head_block(n = 7L)),
    links = c(ab = new_link("a", "b")),
    extensions = new_dag_extension(
      positions = list(
        a = list(x = 10, y = 20, height = 300),
        b = list(x = 10, y = 400, height = 250)
      )
    )
  )

  ser <- blockr_ser(board)
  expect_identical(ser[["constructor"]][["constructor"]], "new_dag_board")
  expect_identical(ser[["constructor"]][["package"]], "blockr.dag")

  path <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(ser, path, auto_unbox = TRUE, null = "null")
  res <- blockr_deser(blockr.core:::read_json(path))

  expect_s3_class(res, "dag_board")
  expect_named(board_blocks(res), c("a", "b"))
  expect_equal(blockr_ser(board_blocks(res)[["b"]])[["payload"]][["n"]], 7L)
  expect_length(board_links(res), 1L)
  # the extension's positions, heights included, come back as saved
  expect_identical(
    blockr_ser(res)[["payload"]][["extensions"]],
    ser[["payload"]][["extensions"]]
  )
})

test_that("the board UI is the canvas, its cards' dependencies and gap", {
  board <- new_dag_board(blocks = c(a = new_dataset_block()))

  ui <- board_ui("board", board)
  html <- htmltools::renderTags(ui)

  expect_match(html$html, "class=\"blockr-dag-board\"", fixed = TRUE)
  expect_match(
    html$html,
    sprintf("data-card-gap=\"%s\"", card_gap()),
    fixed = TRUE
  )

  deps <- vapply(html$dependencies, `[[`, character(1L), "name")
  # the card's own (blockr_dock_dep() is named "blockr-fab") and the board's
  expect_contains(
    deps,
    c("blockr-fab", "show-block", "blockr-block-rename", "dag-board")
  )
  expect_false(any(c("bootstrap", "bslib-component-css") %in% deps))
})

test_that("the board callback serves the DAG extension and the actions", {
  board <- new_dag_board(blocks = c(a = new_dataset_block()))
  plugins <- board_plugins(board)
  captured <- new.env(parent = emptyenv())

  callback <- function(...) {
    captured$res <- dag_board_callback(..., plugins = plugins)
    captured$res
  }

  testServer(
    board_server,
    {
      res <- captured$res

      expect_named(res, c("dock", "actions", "view_data", "extensions"))
      expect_null(res[["dock"]])
      expect_null(res[["view_data"]])
      expect_named(res[["extensions"]], dag_extension_id(board))
      expect_contains(
        names(res[["actions"]]),
        c("add_block_action", "remove_block_action", "add_link_action")
      )
      expect_named(res[["extensions"]][[1L]][["state"]], "positions")
    },
    args = list(
      x = board,
      plugins = plugins,
      options = blockr_app_options(board),
      callbacks = callback,
      callback_location = "start"
    )
  )
})
