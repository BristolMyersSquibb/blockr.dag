library(shinytest2)

expect_values <- function(app, ...) {
  vals <- app$get_values()
  filter_names <- function(x) {
    grep("board-dag_extension", names(x), value = TRUE)
  }
  app$expect_values(
    ...,
    export = vals$export,
    input = filter_names(vals$input),
    output = filter_names(vals$output),
    transform = function(lines) {
      gsub("nonce=[0-9a-f]+", "nonce=<scrubbed>", lines, perl = TRUE)
    }
  )
}

# In reference to: https://github.com/BristolMyersSquibb/blockr.dag/issues/90
test_that("variadic app works", {
  appdir <- system.file(package = "blockr.dag", "examples/variadic")

  # when shinytest2 0.5.0 lands ...
  #local_app_support(appdir)

  app <- AppDriver$new(
    appdir,
    name = "variadic-app",
    seed = 4323
  )

  expect_values(app)
  app$wait_for_idle()
  app$stop()
})
