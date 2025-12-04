library(testthat)

empty_board <- new_board()
non_empty_board <- new_board(
  blocks = list(
    new_dataset_block()
  )
)

test_that("dag extension ctor", {
  ext <- new_dag_extension()
  expect_s3_class(ext, "dag_extension")
})

test_that("ext_ui works", {
  ui <- dag_ext_ui("test", empty_board)
  expect_s3_class(ui, "shiny.tag.list")

  empty_state_container <- htmltools::tagQuery(ui)$find(
    ".dag-empty-state"
  )$selectedTags()[[1]]

  expect_null(empty_state_container$attribs$style)

  # Non empty board
  ui <- dag_ext_ui("test", non_empty_board)
  empty_state_container <- htmltools::tagQuery(ui)$find(
    ".dag-empty-state"
  )$selectedTags()[[1]]

  expect_identical(empty_state_container$attribs$style, "display: none;")

  # Custom dependencies
  deps <- htmltools::findDependencies(ui)
  dep_names <- chr_ply(deps, `[[`, "name")
  expect_contains(dep_names, c("rm-selection", "dag-empty-state"))
})
