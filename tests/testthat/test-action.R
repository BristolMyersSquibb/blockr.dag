my_board <- new_board(
  blocks = c(
    a = new_dataset_block(),
    b = new_head_block(),
    c = new_dataset_block(),
    d = new_scatter_block()
  ),
  links = list(
    from = "c",
    to = "d",
    input = "data"
  )
)

test_draw_link_action <- function(edge, expected_updates) {
  testServer(
    function(id, ...) {
      moduleServer(
        id,
        module = draw_link_action(
          trigger = reactive(edge),
          board = reactiveValues(board = my_board),
          update = reactiveVal(list()),
          dag_extension = list(
            proxy = g6_proxy(
              "graph",
              session = MockShinySession$new()
            )
          )
        )
      )
    },
    {
      expect_length(update(), 0L)
      session$flushReact()

      upd <- update()
      expect_length(upd, expected_updates)

      if (expected_updates > 0) {
        expect_named(upd, "links")
        expect_length(upd$links, 1L)
        expect_named(upd$links, "add")
        expect_length(upd$links$add, 1L)

        new_link <- as.list(upd$links$add[[1]])
        expect_identical(new_link$from, "a")
        expect_identical(new_link$to, "b")
        expect_identical(new_link$input, "data")
      }
    }
  )
}

test_that("draw link action with valid target", {
  test_draw_link_action(
    list(id = "a-b", source = "a", target = "b", targetPort = "node-b-data"),
    1L
  )
})

test_that("remove_selected_action works", {
  testServer(
    function(id, ...) {
      moduleServer(
        id,
        module = remove_selected_action(
          trigger = reactive(TRUE),
          board = reactiveValues(board = my_board),
          update = reactiveVal(list()),
          dag_extension = list(
            proxy = g6_proxy(
              "graph",
              session = MockShinySession$new()
            )
          )
        )
      )
    },
    {
      expect_length(update(), 0L)
      dag_extension$proxy$session$setInputs(
        "graph-selected_node" = c("a", "b"),
        "graph-selected_edge" = "c-d"
      )
      upd <- update()
      expect_length(upd, 3L)
      expect_named(upd, c("blocks", "links", "stacks"))
      expect_named(upd$blocks, "rm")
      expect_identical(upd$blocks$rm, input[["graph-selected_node"]])
      expect_named(upd$links, "rm")
      expect_identical(upd$links$rm, input[["graph-selected_edge"]])
      expect_named(upd$stacks, "rm")
      expect_length(upd$stacks$rm, 0L)
    }
  )
})
