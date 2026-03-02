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

# --- Copy / Cut / Paste fixtures and helpers ---

copy_board <- new_board(
  blocks = c(
    a = new_dataset_block(),
    b = new_head_block(),
    c = new_dataset_block(),
    d = new_scatter_block()
  ),
  links = list(from = "a", to = "b", input = "data"),
  stacks = list(s1 = new_stack(c("a", "b")))
)

make_clipboard_spy <- function() {
  spy <- new.env(parent = emptyenv())
  spy$msgs <- list()
  mock_session <- MockShinySession$new()
  mock_session$sendCustomMessage <- function(type, message) {
    spy$msgs[[length(spy$msgs) + 1L]] <- list(type = type, message = message)
  }
  list(session = mock_session, spy = spy)
}

# --- copy_selected_action ---

test_that("copy_selected_action sends clipboard message for selected nodes", {
  cs <- make_clipboard_spy()

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        module = copy_selected_action(
          trigger = reactive(TRUE),
          board = reactiveValues(board = copy_board, blocks = list()),
          update = reactiveVal(list()),
          dag_extension = list(
            proxy = g6_proxy("graph", session = cs$session)
          )
        )
      )
    },
    {
      expect_length(update(), 0L)
      dag_extension$proxy$session$setInputs(
        "graph-selected_node" = c("node-a", "node-b")
      )
      session$flushReact()

      expect_length(cs$spy$msgs, 1L)
      expect_identical(cs$spy$msgs[[1]]$type, "write-clipboard")

      json_data <- jsonlite::fromJSON(
        cs$spy$msgs[[1]]$message$json,
        simplifyDataFrame = FALSE
      )
      expect_identical(json_data$object, "subboard")

      upd <- update()
      expect_length(upd, 0L)
    }
  )
})

test_that("copy_selected_action does nothing when nothing is selected", {
  cs <- make_clipboard_spy()

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        module = copy_selected_action(
          trigger = reactive(TRUE),
          board = reactiveValues(board = copy_board, blocks = list()),
          update = reactiveVal(list()),
          dag_extension = list(
            proxy = g6_proxy("graph", session = cs$session)
          )
        )
      )
    },
    {
      expect_length(update(), 0L)
      session$flushReact()

      expect_length(cs$spy$msgs, 0L)
      expect_length(update(), 0L)
    }
  )
})

# --- cut_selected_action ---

test_that("cut_selected_action copies to clipboard and removes", {
  cs <- make_clipboard_spy()

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        module = cut_selected_action(
          trigger = reactive(TRUE),
          board = reactiveValues(board = copy_board, blocks = list()),
          update = reactiveVal(list()),
          dag_extension = list(
            proxy = g6_proxy("graph", session = cs$session)
          )
        )
      )
    },
    {
      expect_length(update(), 0L)
      dag_extension$proxy$session$setInputs(
        "graph-selected_node" = c("node-a", "node-b")
      )
      session$flushReact()

      expect_length(cs$spy$msgs, 1L)
      expect_identical(cs$spy$msgs[[1]]$type, "write-clipboard")

      upd <- update()
      expect_length(upd, 3L)
      expect_named(upd, c("blocks", "links", "stacks"))
      expect_named(upd$blocks, "rm")
      expect_true(all(c("a", "b") %in% upd$blocks$rm))
      expect_named(upd$links, "rm")
      expect_named(upd$stacks, "rm")
    }
  )
})

test_that("cut_selected_action does nothing when nothing is selected", {
  cs <- make_clipboard_spy()

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        module = cut_selected_action(
          trigger = reactive(TRUE),
          board = reactiveValues(board = copy_board, blocks = list()),
          update = reactiveVal(list()),
          dag_extension = list(
            proxy = g6_proxy("graph", session = cs$session)
          )
        )
      )
    },
    {
      expect_length(update(), 0L)
      session$flushReact()

      expect_length(cs$spy$msgs, 0L)
      expect_length(update(), 0L)
    }
  )
})

# --- paste_action ---

test_that("paste_action adds remapped blocks/links/stacks", {
  sub <- extract_subboard(copy_board, block_ids = c("a", "b"))
  json_str <- as.character(jsonlite::toJSON(
    blockr_ser(sub),
    auto_unbox = TRUE
  ))

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        module = paste_action(
          trigger = reactive(json_str),
          board = reactiveValues(board = copy_board),
          update = reactiveVal(list()),
          dag_extension = list(
            proxy = g6_proxy("graph", session = MockShinySession$new())
          )
        )
      )
    },
    {
      expect_length(update(), 0L)
      session$flushReact()

      upd <- update()
      expect_named(upd, c("blocks", "links", "stacks"))
      expect_named(upd$blocks, "add")
      expect_named(upd$links, "add")
      expect_named(upd$stacks, "add")

      expect_length(upd$blocks$add, 2L)
      new_block_ids <- names(upd$blocks$add)
      expect_true(!any(new_block_ids %in% board_block_ids(copy_board)))
    }
  )
})

test_that("paste_action ignores invalid JSON", {
  testServer(
    function(id, ...) {
      moduleServer(
        id,
        module = paste_action(
          trigger = reactive("not valid json {{{"),
          board = reactiveValues(board = copy_board),
          update = reactiveVal(list()),
          dag_extension = list(
            proxy = g6_proxy("graph", session = MockShinySession$new())
          )
        )
      )
    },
    {
      expect_length(update(), 0L)
      session$flushReact()
      expect_length(update(), 0L)
    }
  )
})

test_that("paste_action with empty subboard produces empty add lists", {
  empty_sub <- new_subboard(blocks(), links(), stacks())
  json_str <- as.character(jsonlite::toJSON(
    blockr_ser(empty_sub),
    auto_unbox = TRUE
  ))

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        module = paste_action(
          trigger = reactive(json_str),
          board = reactiveValues(board = copy_board),
          update = reactiveVal(list()),
          dag_extension = list(
            proxy = g6_proxy("graph", session = MockShinySession$new())
          )
        )
      )
    },
    {
      expect_length(update(), 0L)
      session$flushReact()

      upd <- update()
      expect_named(upd, c("blocks", "links", "stacks"))
      expect_length(upd$blocks$add, 0L)
      expect_length(upd$links$add, 0L)
      expect_length(upd$stacks$add, 0L)
    }
  )
})

# --- remove_subboard() helper ---

test_that("remove_subboard() produces correct rm lists", {
  sb <- extract_subboard(copy_board, block_ids = c("a", "b"))
  captured <- NULL
  mock_update <- function(val) captured <<- val

  remove_subboard(sb, mock_update)

  expect_named(captured, c("blocks", "links", "stacks"))
  expect_identical(captured$blocks$rm, names(sb$blocks))
  expect_identical(captured$links$rm, names(sb$links))
  expect_identical(captured$stacks$rm, names(sb$stacks))
})
