test_that("dag_layout() defaults to top to bottom, normal spacing", {
  x <- dag_layout()
  expect_s3_class(x, "dag_layout")
  expect_identical(unclass(x), list(direction = "TB", spacing = "normal"))
  expect_error(dag_layout(direction = "RL"))
  expect_error(dag_layout(spacing = "huge"))
})

test_that("a layout survives a JSON round trip", {
  for (x in list(dag_layout(), dag_layout("LR", "compact"), dag_layout("TB", "loose"))) {
    back <- jsonlite::fromJSON(jsonlite::toJSON(unclass(x), auto_unbox = TRUE))
    expect_identical(as_dag_layout(back), x)
  }
  expect_identical(as_dag_layout(NULL), dag_layout())
  expect_identical(as_dag_layout(list(direction = "LR")), dag_layout("LR"))
  expect_error(as_dag_layout(list(type = "indented")), class = "dag_layout_invalid")
  expect_error(as_dag_layout(list(direction = "RL")), class = "dag_layout_invalid")
})

test_that("a layout maps to the layered g6R layout", {
  cfg <- dag_layout_config(dag_layout("LR", "compact"))
  expect_identical(cfg$type, "antv-dagre")
  expect_identical(cfg$rankdir, "LR")
  expect_identical(cfg$nodesep, 20)
  expect_identical(cfg$ranksep, 30)
  expect_true(cfg$sortByCombo)
})

test_that("the client's catalog builds what R builds", {
  catalog <- dag_layout_catalog()
  expect_named(catalog, c("TB", "LR"))
  for (dir in names(catalog)) {
    for (sp in c("compact", "normal", "loose")) {
      expect_identical(catalog[[dir]][[sp]], dag_layout_config(dag_layout(dir, sp)))
    }
  }
})

test_that("the layout state follows the menu and pushes external sets", {
  pushed <- list()
  local_mocked_bindings(
    push_layout = function(layout, session) {
      pushed[[length(pushed) + 1L]] <<- layout
      invisible()
    }
  )

  testServer(
    function(id) {
      moduleServer(id, function(input, output, session) {
        rv <- setup_layout_ctrl(dag_layout())
        session$userData$rv <- rv
      })
    },
    {
      rv <- session$userData$rv

      # A change from the menu: taken, not sent back.
      session$setInputs(layout = list(direction = "LR", spacing = "loose"))
      expect_identical(rv(), dag_layout("LR", "loose"))
      expect_length(pushed, 0L)

      # Set from outside, as a plain list: validated and sent to the client.
      rv(list(direction = "TB", spacing = "compact"))
      session$flushReact()
      expect_identical(rv(), dag_layout("TB", "compact"))
      expect_length(pushed, 1L)
      expect_identical(pushed[[1L]], dag_layout("TB", "compact"))

      # An invalid set is dropped for the last layout the client showed.
      expect_warning(
        {
          rv(list(direction = "RL"))
          session$flushReact()
        },
        class = "dag_layout_ignored"
      )
      expect_identical(rv(), dag_layout("LR", "loose"))
    }
  )
})
