# A block spliced into a wire is placed by the DAG, not by the layout: the
# gesture that creates it is a right-click on the wire, so it arrives at the
# click position, sitting on the link it just split.

fake_proxy <- function(positions) {
  nodes <- lapply(
    positions,
    function(p) list(style = list(x = p[[1L]], y = p[[2L]]))
  )
  names(nodes) <- to_g6_node_id(names(positions))
  list(session = list(input = set_names(list(list(nodes = nodes)), "graph-state")))
}

splice_update <- function(block = "c", from = "a", to = "b", rm = "l1") {
  list(
    blocks = list(add = as_blocks(set_names(list(new_head_block()), block))),
    links = list(
      add = as_links(
        c(
          l2 = new_link(from, block, "data"),
          l3 = new_link(block, to, "data")
        )
      ),
      rm = rm
    )
  )
}

capture_push <- function(code) {
  pushed <- NULL
  local_mocked_bindings(
    apply_node_positions = function(positions, proxy) {
      pushed <<- positions
      invisible()
    }
  )
  force(code)
  pushed
}

test_that("a splice is told apart from the other ways a block arrives", {

  expect_identical(
    spliced_link(splice_update()),
    list(block = "c", from = "a", to = "b")
  )

  # An add: one block, no links.
  expect_null(
    spliced_link(
      list(blocks = list(add = as_blocks(c(c = new_head_block()))))
    )
  )

  # An append: one block and one link, nothing removed.
  expect_null(
    spliced_link(
      list(
        blocks = list(add = as_blocks(c(c = new_head_block()))),
        links = list(add = as_links(c(l = new_link("a", "c", "data"))))
      )
    )
  )

  # Two links but both pointing the same way is not a splice either.
  expect_null(
    spliced_link(
      list(
        blocks = list(add = as_blocks(c(c = new_head_block()))),
        links = list(
          add = as_links(
            c(
              l2 = new_link("a", "c", "data"),
              l3 = new_link("z", "c", "y")
            )
          ),
          rm = "l1"
        )
      )
    )
  )
})

test_that("the whole branch below the target moves, not just the target", {

  links <- data.frame(
    from = c("a", "b", "d", "e"),
    to = c("b", "d", "e", "f"),
    stringsAsFactors = FALSE
  )

  expect_identical(downstream_of("b", links), c("d", "e", "f"))
  expect_identical(downstream_of("f", links), character())

  # A diamond reaches the join once, not twice.
  diamond <- data.frame(
    from = c("b", "b", "c", "d"),
    to = c("c", "d", "e", "e"),
    stringsAsFactors = FALSE
  )

  expect_setequal(downstream_of("b", diamond), c("c", "d", "e"))
  expect_length(downstream_of("b", diamond), 3L)
})

test_that("a splice keeps the gap the wire already had", {

  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_head_block()),
    links = c(l1 = new_link("a", "b", "data"))
  )

  # The layout's own spacing: 132px between rank centres.
  proxy <- fake_proxy(list(a = c(150, 150), b = c(150, 282)))

  pushed <- capture_push(
    space_spliced_node(
      list(block = "c", from = "a", to = "b"),
      splice_update(), board, proxy
    )
  )

  expect_named(pushed, c("c", "b"))
  # One gap past the source, and the target moved down by the same gap, so
  # both new wires are as long as the one they replaced.
  expect_identical(pushed$c, list(x = 150, y = 282))
  expect_identical(pushed$b, list(x = 150, y = 414))
})

test_that("a loosely spaced wire stays loose, a tight one is opened up", {

  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_head_block()),
    links = c(l1 = new_link("a", "b", "data"))
  )

  loose <- capture_push(
    space_spliced_node(
      list(block = "c", from = "a", to = "b"),
      splice_update(), board,
      fake_proxy(list(a = c(0, 0), b = c(0, 400)))
    )
  )

  expect_identical(loose$c$y, 400)
  expect_identical(loose$b$y, 800)

  # Ends that nearly touch would otherwise splice into an unreadable gap, so
  # the minimum applies instead of the span.
  tight <- capture_push(
    space_spliced_node(
      list(block = "c", from = "a", to = "b"),
      splice_update(), board,
      fake_proxy(list(a = c(0, 0), b = c(0, 20)))
    )
  )

  expect_identical(tight$c$y, 130)
  expect_identical(tight$b$y, 260)
})

test_that("a wire running sideways is spaced along its own axis", {

  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_head_block()),
    links = c(l1 = new_link("a", "b", "data"))
  )

  pushed <- capture_push(
    space_spliced_node(
      list(block = "c", from = "a", to = "b"),
      splice_update(), board,
      fake_proxy(list(a = c(100, 50), b = c(400, 50)))
    )
  )

  expect_identical(pushed$c, list(x = 400, y = 50))
  expect_identical(pushed$b, list(x = 700, y = 50))
})

test_that("a splice upwards moves the branch the same way", {

  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_head_block()),
    links = c(l1 = new_link("a", "b", "data"))
  )

  # The target sits above the source, so the gap is negative and everything
  # below it moves further up rather than snapping across the source.
  pushed <- capture_push(
    space_spliced_node(
      list(block = "c", from = "a", to = "b"),
      splice_update(), board,
      fake_proxy(list(a = c(0, 300), b = c(0, 100)))
    )
  )

  expect_identical(pushed$c$y, 100)
  expect_identical(pushed$b$y, -100)
})

test_that("a splice with the branch below carries the branch along", {

  board <- new_board(
    c(
      a = new_dataset_block("iris"), b = new_head_block(),
      d = new_head_block()
    ),
    links = c(l1 = new_link("a", "b", "data"), l4 = new_link("b", "d", "data"))
  )

  pushed <- capture_push(
    space_spliced_node(
      list(block = "c", from = "a", to = "b"),
      splice_update(), board,
      fake_proxy(list(a = c(0, 0), b = c(0, 130), d = c(0, 260)))
    )
  )

  expect_setequal(names(pushed), c("c", "b", "d"))
  expect_identical(pushed$b$y, 260)
  expect_identical(pushed$d$y, 390)
})

test_that("an unplaced end leaves positions alone", {

  board <- new_board(
    c(a = new_dataset_block("iris"), b = new_head_block()),
    links = c(l1 = new_link("a", "b", "data"))
  )

  pushed <- capture_push(
    space_spliced_node(
      list(block = "c", from = "a", to = "b"),
      splice_update(), board, fake_proxy(list(a = c(0, 0)))
    )
  )

  expect_null(pushed)
})
