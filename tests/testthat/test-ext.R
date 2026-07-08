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

test_that("positions is an externally controllable handle", {
  expect_identical(attr(new_dag_extension(), "external_ctrl"), "positions")
})

test_that("external position set pushes the moved node to the client", {
  pushed <- NULL
  local_mocked_bindings(
    g6_update_nodes = function(proxy, nodes) {
      pushed <<- nodes
      invisible()
    }
  )

  testServer(
    function(id) {
      moduleServer(id, function(input, output, session) {
        setup_positions_ctrl(NULL, proxy = NULL, session = session)
      })
    },
    {
      rv <- session$returned
      expect_s3_class(rv, "reactiveVal")

      session$setInputs("graph-initialized" = TRUE)

      # External write (as the board update lifecycle would do) with no
      # client positions yet -> the node is pushed to the client.
      rv(list(a = list(x = 100, y = 200)))
      session$flushReact()

      expect_equal(pushed[[1]]$id, to_g6_node_id("a"))
      expect_equal(pushed[[1]]$style$x, 100)
      expect_equal(pushed[[1]]$style$y, 200)
    }
  )
})

test_that("client drags sync into the positions reactiveVal (debounced)", {
  local_mocked_bindings(g6_update_nodes = function(proxy, nodes) invisible())

  testServer(
    function(id) {
      moduleServer(id, function(input, output, session) {
        setup_positions_ctrl(NULL, proxy = NULL, session = session)
      })
    },
    {
      rv <- session$returned
      session$setInputs("graph-initialized" = TRUE)
      session$setInputs(
        "graph-state" = list(
          nodes = list(
            `node-a` = list(id = "node-a", style = list(x = 12, y = 34))
          )
        )
      )
      session$elapse(300) # let the debounce fire
      expect_identical(rv(), list(a = list(x = 12, y = 34)))
    }
  )
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


# Can we make the scope of blockr.dock before?
dock_view_proxy <- function(
  id,
  data = NULL,
  session = getDefaultReactiveDomain()
) {
  if (is.null(session)) {
    stop(
      "dock_view_proxy must be called from the server function of a Shiny app."
    )
  }
  structure(list(id = id, session = session), class = "dock_view_proxy")
}

test_board <- blockr.dock::new_dock_board(
  blocks = c(
    a = new_dataset_block("iris"),
    b = new_scatter_block(x = "Sepal.Length", y = "Sepal.Width"),
    c = new_head_block(),
    d = new_subset_block()
  ),
  links = list(from = "a", to = "b", input = "data"),
  stacks = c(
    stack_1 = blockr.dock::new_dock_stack(c("a", "b"), color = "#0000FF"),
    stack_2 = blockr.dock::new_dock_stack()
  ),
  extensions = new_dag_extension()
)

testServer(
  # Path to app.R is easier than trying to mock all board and dock stuff.
  #system.file("examples/stacks/app.R", package = "blockr.dag"),
  dag_ext_srv(NULL),
  args = list(
    board = reactiveValues(board = test_board),
    update = reactiveVal(NULL),
    # Mock dock returned value
    dock = list(
      layout = reactive(NULL), #blockr.dock::new_dock_layout()
      proxy = dock_view_proxy(
        "dock",
        session = MockShinySession$new()
      ),
      prev_active_group = reactiveVal(NULL)
    ),
    actions = blockr.dock::action_triggers(
      unlst(
        c(
          list(board_actions(test_board)),
          board_actions(new_dag_extension())
        )
      )
    )
  ),
  {
    # Test graph output
    session$output[[graph_id()]]
    expect_null(update())
    expect_s3_class(proxy, "g6_proxy")
    session$setInputs("graph-initialized" = TRUE)

    # Trigger draw link
    session$setInputs(
      added_edge = list(
        source = "a",
        target = "b",
        targetType = "node",
        sourcePort = "node-a-out",
        targetPort = "node-b-data"
      )
    )
    session$setInputs(
      added_edge = list(
        source = "a",
        target = "c",
        targetType = "node",
        sourcePort = "node-a-out",
        targetPort = "node-c-data"
      )
    )

    # Trigger append block
    session$setInputs(
      added_edge = list(
        source = "a",
        target = NULL,
        targetType = "canvas",
        sourcePort = "node-a-out",
        portType = "output"
      )
    )

    # Trigger prepend block
    session$setInputs(
      added_edge = list(
        source = "d",
        target = NULL,
        targetType = "canvas",
        sourcePort = "node-d-data",
        portType = "input"
      )
    )

    # Trigger brush_select
    session$setInputs(
      "graph-selected_node" = structure(
        c("a", "b"),
        eventType = "brush_select"
      )
    )

    # Trigger batch delete
    session$setInputs(
      "graph-batch_delete" = TRUE
    )

    # blockr.core's mod slot now expects partial-args deltas keyed by
    # entry ID rather than full block / stack objects. stack_1 is being
    # removed in the same update so it cannot also appear in mod.

    mod_stacks <- list(
      stack_2 = list(blocks = "d")
    )

    # Block names: rename blocks a and b via the block_name delta key.
    mod_blocks <- list(
      a = list(block_name = "a_new"),
      b = list(block_name = "b_new")
    )

    update(
      list(
        blocks = list(
          add = as_blocks(new_dataset_block()),
          mod = mod_blocks
        ),
        stacks = list(
          add = as_stacks(blockr.dock::new_dock_stack("c", color = "#0000FF")),
          mod = mod_stacks,
          rm = c("stack_1")
        ),
        links = list(
          rm = "ab"
        )
      )
    )
    session$flushReact()
  }
)

test_that("a block-name mod delta relabels the node", {
  captured <- NULL
  local_mocked_bindings(
    g6_update_nodes = function(proxy, nodes) {
      captured <<- nodes
      invisible()
    }
  )

  rename_board <- blockr.dock::new_dock_board(
    blocks = c(a = new_dataset_block("iris"))
  )

  testServer(
    function(id, board, update) {
      moduleServer(
        id,
        function(input, output, session) {
          update_observer(update, board, blockr_g6_proxy(session))
        }
      )
    },
    args = list(
      board = reactiveValues(board = rename_board),
      update = reactiveVal(NULL)
    ),
    {
      update(
        list(blocks = list(mod = list(a = list(block_name = "renamed"))))
      )
      session$flushReact()

      expect_identical(captured[[1L]]$id, to_g6_node_id("a"))
      expect_identical(captured[[1L]]$style$labelText, "renamed")
    }
  )
})

test_that("extension_block_callback works", {
  ext_cb <- extension_block_callback(new_dag_extension())

  testServer(
    function(input, output, session) {
      conditions <- reactive({
        list(
          errors = input$errors
        )
      })
      res <- ext_cb(
        id = "test",
        board = test_board,
        update = reactiveVal(NULL),
        conditions = conditions,
        dag_extension = list(
          proxy = g6_proxy(
            "graph",
            session = session
          )
        ),
        session = session
      )
    },
    {
      expect_null(res)
      session$setInputs(
        `graph-initialized` = TRUE,
        errors = c("error1", "error2")
      )
      session$setInputs(errors = character(0))
    }
  )
})

test_that("dag_badge_status maps errors and eval status to a badge (#145)", {

  # An error condition always wins -> failed (covers a render-phase error on a
  # block that is otherwise `ready`).
  expect_identical(dag_badge_status(2L, "ready"), "failed")
  expect_identical(dag_badge_status(1L, NULL), "failed")

  # No errors: mirror the eval status for the indicator states.
  expect_identical(dag_badge_status(0L, "waiting"), "waiting")
  expect_identical(dag_badge_status(0L, "unset"), "unset")
  expect_identical(dag_badge_status(0L, "failed"), "failed")

  # `ready` and an absent status carry no badge.
  expect_null(dag_badge_status(0L, "ready"))
  expect_null(dag_badge_status(0L, NULL))

  # `dormant` means the status is not currently computed: signal "leave the
  # badge as-is" with `NA` rather than clearing it when a block drops out of
  # the eval set.
  expect_identical(dag_badge_status(0L, "dormant"), NA_character_)
})

test_that("a block going dormant keeps its badge (#146)", {
  pushed <- list()
  local_mocked_bindings(
    g6_update_nodes = function(proxy, nodes) {
      pushed[[length(pushed) + 1L]] <<- nodes[[1]]$style$badges
      invisible()
    }
  )

  status <- reactiveVal("dormant")

  testServer(
    function(id) {
      moduleServer(id, function(input, output, session) {
        cb <- extension_block_callback(new_dag_extension())
        cb(
          id = "a",
          board = list(eval = list(a = status)),
          update = reactiveVal(NULL),
          conditions = reactive(list(error = list())),
          dag_extension = list(proxy = list(session = session)),
          session = session
        )
      })
    },
    {
      session$setInputs("graph-initialized" = TRUE)
      session$flushReact()

      # Enters the eval set as `waiting` -> exactly one badge is drawn.
      status("waiting")
      session$flushReact()
      expect_length(pushed, 1L)
      expect_length(pushed[[1L]], 1L)

      # Drops out of the eval set (`dormant`): the badge must be kept, so no
      # clearing update is pushed. Under the pre-fix behaviour `dormant`
      # cleared the badge here, adding a second (empty-badges) push.
      status("dormant")
      session$flushReact()
      expect_length(pushed, 1L)
    }
  )
})
