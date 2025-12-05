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
    c = new_head_block()
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
      added_edge = list(source = "a", target = "b", targetType = "node")
    )
    session$setInputs(
      added_edge = list(source = "a", target = "c", targetType = "node")
    )

    # Trigger append block
    session$setInputs(
      added_edge = list(source = "a", targetType = "canvas")
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
  }
)

test_that("extension_block_callback works", {
  ext_cb <- extension_block_callback(new_dag_extension())

  testServer(
    function(input, output, session) {
      conditions <- reactive({
        list(
          errors = input$errors
        )
      })
      ext_cb(
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
      session$setInputs(errors = c("error1", "error2"))
      session$setInputs(errors = character(0))
    }
  )
})
