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

empty_cnd <- function() {
  data.frame(
    block = character(),
    phase = character(),
    severity = character(),
    message = character(),
    id = character()
  )
}

err_cnd <- function(block) {
  data.frame(
    block = block,
    phase = "eval",
    severity = "error",
    message = "boom",
    id = "e1"
  )
}

status_badge_test_board <- function(statuses, conditions = reactive(empty_cnd())) {
  list(
    blocks = set_names(vector("list", length(statuses)), names(statuses)),
    eval = statuses,
    conditions = conditions
  )
}

test_that("status badges repaint changed nodes in one batched push", {
  pushed <- list()
  local_mocked_bindings(
    g6_update_nodes = function(proxy, nodes) {
      pushed[[length(pushed) + 1L]] <<- nodes
      invisible()
    }
  )

  a <- reactiveVal("ready")
  b <- reactiveVal("ready")
  d <- reactiveVal("ready")

  board <- status_badge_test_board(list(a = a, b = b, d = d))

  testServer(
    function(id) {
      moduleServer(id, function(input, output, session) {
        status_badge_observer(board, NULL, session)
      })
    },
    {
      session$setInputs("graph-initialized" = TRUE)
      session$flushReact()

      # `ready` carries no badge -> nothing is painted.
      expect_length(pushed, 0L)

      # Three blocks change status in a single flush -> ONE batched push
      # carrying all three node configs. The pre-fix per-block observers would
      # each fire and emit a separate `g6_update_nodes()` call.
      a("waiting")
      b("unset")
      d("failed")
      session$flushReact()

      expect_length(pushed, 1L)
      expect_length(pushed[[1L]], 3L)
      expect_setequal(
        vapply(pushed[[1L]], `[[`, character(1L), "id"),
        to_g6_node_id(c("a", "b", "d"))
      )

      # Only `a` changes next -> a single-node push; `b` / `d` are untouched.
      a("ready")
      session$flushReact()

      expect_length(pushed, 2L)
      expect_length(pushed[[2L]], 1L)
      expect_identical(pushed[[2L]][[1L]]$id, to_g6_node_id("a"))
      expect_length(pushed[[2L]][[1L]]$style$badges, 0L)

      # A flush that changes no status must not repaint anything.
      session$flushReact()
      expect_length(pushed, 2L)
    }
  )
})

test_that("a block going dormant keeps its badge (#146)", {
  pushed <- list()
  local_mocked_bindings(
    g6_update_nodes = function(proxy, nodes) {
      pushed[[length(pushed) + 1L]] <<- nodes
      invisible()
    }
  )

  status <- reactiveVal("dormant")

  board <- status_badge_test_board(list(a = status))

  testServer(
    function(id) {
      moduleServer(id, function(input, output, session) {
        status_badge_observer(board, NULL, session)
      })
    },
    {
      session$setInputs("graph-initialized" = TRUE)
      session$flushReact()

      # Dormant on entry: an `NA` spec paints nothing.
      expect_length(pushed, 0L)

      # Enters the eval set as `waiting` -> exactly one badge is drawn.
      status("waiting")
      session$flushReact()
      expect_length(pushed, 1L)
      expect_identical(pushed[[1L]][[1L]]$id, to_g6_node_id("a"))
      expect_length(pushed[[1L]][[1L]]$style$badges, 1L)

      # Drops out of the eval set (`dormant`): the badge must be kept, so no
      # clearing update is pushed. Under the pre-fix behaviour `dormant`
      # cleared the badge here, adding a second (empty-badges) push.
      status("dormant")
      session$flushReact()
      expect_length(pushed, 1L)
    }
  )
})

test_that("a render-phase error promotes the badge to failed", {
  pushed <- list()
  local_mocked_bindings(
    g6_update_nodes = function(proxy, nodes) {
      pushed[[length(pushed) + 1L]] <<- nodes
      invisible()
    }
  )

  conditions <- reactiveVal(empty_cnd())

  board <- status_badge_test_board(
    list(a = reactiveVal("ready")),
    conditions = conditions
  )

  testServer(
    function(id) {
      moduleServer(id, function(input, output, session) {
        status_badge_observer(board, NULL, session)
      })
    },
    {
      session$setInputs("graph-initialized" = TRUE)
      session$flushReact()

      # `ready` with no conditions carries no badge.
      expect_length(pushed, 0L)

      # An error condition promotes a `ready` block to a red `failed` badge.
      conditions(err_cnd("a"))
      session$flushReact()

      expect_length(pushed, 1L)
      expect_length(pushed[[1L]][[1L]]$style$badges, 1L)
      expect_identical(
        pushed[[1L]][[1L]]$style$badges[[1L]]$backgroundFill,
        "#dc2626"
      )
    }
  )
})

test_that("reveal_panel_delta builds a valid current-view delta (#308)", {

  # Default layout: every block is a member of the sole view, so revealing is
  # a plain select of the existing panel.
  paneled <- blockr.dock::new_dock_board(
    blocks = c(a = new_dataset_block("iris"), b = new_head_block())
  )
  view <- blockr.dock::active_view(blockr.dock::board_views(paneled))

  d_sel <- reveal_panel_delta(paneled, "a")
  expect_identical(d_sel$views$mod[[view]], list(select = "block_panel-a"))
  expect_no_error(blockr.core::augment_board_update(d_sel, paneled))

  # A block the current view does not hold: add it there, then select it.
  parked <- blockr.dock::new_dock_board(
    blocks = c(a = new_dataset_block("iris"), b = new_head_block()),
    views  = list(main = "a")
  )

  d_add <- reveal_panel_delta(parked, "b")
  expect_identical(names(d_add$views$mod$main), c("add", "select"))
  expect_identical(names(d_add$views$mod$main$add), "block_panel-b")
  expect_identical(d_add$views$mod$main$select, "block_panel-b")
  expect_no_error(blockr.core::augment_board_update(d_add, parked))

  # Neither delta carries a top-level `active`, so a reveal never jumps views.
  expect_null(d_sel$views$active)
  expect_null(d_add$views$active)
})

test_that("node click emits the reveal delta on update (#308)", {

  board <- blockr.dock::new_dock_board(
    blocks = c(a = new_dataset_block("iris"), b = new_head_block()),
    extensions = new_dag_extension()
  )

  testServer(
    dag_ext_srv(NULL),
    args = list(
      board = reactiveValues(board = board),
      update = reactiveVal(NULL),
      actions = blockr.dock::action_triggers(
        unlst(
          c(
            list(board_actions(board)),
            board_actions(new_dag_extension())
          )
        )
      )
    ),
    {
      session$setInputs(`graph-initialized` = TRUE)
      session$setInputs(`graph-selected_node` = "node-a")

      delta <- update()
      view <- names(delta$views$mod)

      expect_identical(delta$views$mod[[view]], list(select = "block_panel-a"))
    }
  )
})
