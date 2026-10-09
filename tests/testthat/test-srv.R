test_that("an edge dropped on the canvas passes on where it was dropped", {

  fired <- list()

  record <- function(name) {
    function(target, at = NULL) {
      fired[[length(fired) + 1L]] <<- list(name, target, at)
    }
  }

  actions <- list(
    append_block_action = record("append_block"),
    prepend_block_action = record("prepend_block")
  )

  testServer(
    function(input, output, session) {
      actions_observers(actions, list(session = session))
    },
    {
      session$setInputs(
        added_edge = list(
          source = "a",
          targetType = "canvas",
          portType = "output",
          at = list(x = 10, y = 20)
        )
      )

      # A drop whose point could not be read still appends or prepends.
      session$setInputs(
        added_edge = list(source = "b", targetType = "canvas", portType = "input")
      )
    }
  )

  expect_identical(
    fired,
    list(
      list("append_block", "a", list(x = 10, y = 20)),
      list("prepend_block", "b", NULL)
    )
  )
})
