library(testthat)

test_that("dag_layout defaults to a top-down antv-dagre", {
  l <- dag_layout()
  expect_identical(l$type, "antv-dagre")
  expect_identical(l$rankdir, "TB")
  expect_identical(l$ranker, "network-simplex")
  expect_true(l$sortByCombo)
  # No nodeSize for top-down flow -> dagre's own spacing (familiar look)
  expect_null(l$nodeSize)
})

test_that("dag_layout forwards and validates its knobs", {
  l <- dag_layout(
    rankdir = "TB",
    nodesep = 30,
    ranksep = 80,
    node_size = c(100, 60),
    sort_by_combo = FALSE
  )
  expect_identical(l$rankdir, "TB")
  expect_identical(l$nodesep, 30)
  expect_identical(l$ranksep, 80)
  expect_identical(l$nodeSize, c(100, 60))
  expect_false(l$sortByCombo)

  expect_error(dag_layout(rankdir = "diagonal"))
  expect_error(dag_layout(ranker = "spline"))
  expect_error(dag_layout(nodesep = "wide"))
})

test_that("node_size default is orientation-aware", {
  # Horizontal flow reserves label width along the rank axis
  expect_identical(dag_layout(rankdir = "LR")$nodeSize, c(140, 40))
  # Top-down keeps dagre's own spacing (no nodeSize)
  expect_null(dag_layout(rankdir = "TB")$nodeSize)
})

test_that("dag_layout round-trips through JSON (save / restore)", {
  rt <- jsonlite::fromJSON(
    jsonlite::toJSON(dag_layout(rankdir = "LR"), auto_unbox = TRUE)
  )
  expect_identical(rt$type, "antv-dagre")
  expect_identical(rt$rankdir, "LR")
})

test_that("dag_orientation maps flow direction to ports and edge curve", {
  expect_identical(dag_orientation("TB")$edge, "cubic-vertical")
  expect_identical(dag_orientation("TB")$input, "top")
  expect_identical(dag_orientation("TB")$output, "label-bottom")

  expect_identical(dag_orientation("LR")$edge, "cubic-horizontal")
  expect_identical(dag_orientation("LR")$input, "left")
  expect_identical(dag_orientation("LR")$output, "right")
})

test_that("layout_rankdir reads rankdir and defaults to TB", {
  expect_identical(layout_rankdir(dag_layout(rankdir = "LR")), "LR")
  expect_identical(layout_rankdir(list()), "TB")
})
